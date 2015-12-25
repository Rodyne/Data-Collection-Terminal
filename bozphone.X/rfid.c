/* Public domain based on several different libraries (such as arduino) contributors to the code base are:
 * COOQROBOT, Dr.Leong, Miguel Balboa (circuitito.com), Søren Thing Andersen (access.thing.dk) and Tom Clement
 *
 * Changed from c++ to c and simplified by removing writing ability and support of cards other than mifare 1K classic (S50 chip)
 *
 * The PCD is short for Proximity Coupling Device ie the NXP MFRC522 Contactless Reader IC
 * The PICC is short for Proximity Integrated Circuit Card: A card or tag using the ISO 14443A interface, eg Mifare or NTAG203.
 *
 * The protocol is described in the MFRC522 datasheet: http://www.nxp.com/documents/data_sheet/MFRC522.pdf
 * The protocol is defined in ISO/IEC 14443-3 Identification cards -- Contactless integrated circuit cards -- Proximity cards -- Part 3: Initialization and anticollision".
 * A free version of the final draft can be found at http://wg8.de/wg8n1496_17n3613_Ballot_FCD14443-3.pdf
 * Details are found in chapter 6, Type A ? Initialization and anticollision.
 * To read and write from MIFARE PICCs, the MIFARE protocol is used after the PICC has been selected.
 * The MIFARE 1K Classic chips and protocol is described in the datasheet  http://www.nxp.com/documents/data_sheet/MF1S503x.pdf
 * 		Has 16 sectors * 4 blocks/sector * 16 uint8s/block = 1024 uint8s.
 * 		The blocks are numbered 0-63.
 * 		Block 3 in each sector is the Sector Trailer. See http://www.nxp.com/documents/data_sheet/MF1S503x.pdf sections 8.6 and 8.7:
 * 				uint8s 0-5:   Key A
 * 				uint8s 6-8:   Access Bits
 * 				uint8s 9:     User data
 * 				uint8s 10-15: Key B (KeyB not used in this app so can be used for user data)
 * 		Block 0 is read only manufacturer data.
 * 		To access a block, an authentication using a key from the block's sector must be performed first.
 * 		Example: To read from block 10, first authenticate using a key from sector 3 (blocks 8-11).
 * 		All keys are set to FFFFFFFFFFFFh at chip delivery.
 * 		Warning: Please read section 8.7 "Memory Access". It includes this text: if the PICC detects a format violation the whole sector is irreversibly blocked.
 */

#include "hardware.h"
#include "rfid.h"

enum PCD_Register
{
	CommandReg			= 0x01,	// starts and stops command execution
	ComIEnReg				= 0x02,	// enable and disable interrupt request control bits
	DivIEnReg				= 0x03,	// enable and disable interrupt request control bits
	ComIrqReg				= 0x04,	// interrupt request bits
	DivIrqReg				= 0x05,	// interrupt request bits
	ErrorReg				= 0x06,	// error bits showing the error status of the last command executed
	Status1Reg			= 0x07,	// communication status bits
	Status2Reg			= 0x08,	// receiver and transmitter status bits
	FIFODataReg			= 0x09,	// input and output of 64 uint8 FIFO buffer
	FIFOLevelReg		= 0x0A,	// number of uint8s stored in the FIFO buffer
	WaterLevelReg		= 0x0B,	// level for FIFO underflow and overflow warning
	ControlReg			= 0x0C,	// miscellaneous control registers
	BitFramingReg		= 0x0D,	// adjustments for bit-oriented frames
	CollReg					= 0x0E,	// bit position of the first bit-collision detected on the RF interface
	ModeReg					= 0x11,	// defines general modes for transmitting and receiving
	TxModeReg				= 0x12,	// defines transmission data rate and framing
	RxModeReg				= 0x13,	// defines reception data rate and framing
	TxControlReg		= 0x14,	// controls the logical behavior of the antenna driver pins TX1 and TX2
	TxASKReg				= 0x15,	// controls the setting of the transmission modulation
	CRCResultRegH		= 0x21,	// shows the MSB and LSB values of the CRC calculation
	CRCResultRegL		= 0x22,
	TModeReg				= 0x2A,	// defines settings for the internal timer
	TPrescalerReg		= 0x2B,	// the lower 8 bits of the TPrescaler value. The 4 high bits are in TModeReg.
	TReloadRegH			= 0x2C,	// defines the 16-bit timer reload value
	TReloadRegL			= 0x2D
};

enum PCD_Command	// MFRC522 commands. Described in chapter 10 of the datasheet.
{
	PCD_Idle				= 0x00,		// no action, cancels current command execution
	PCD_CalcCRC			= 0x03,		// activates the CRC coprocessor or performs a self test
	PCD_Sleep				= 0x37,		// no command change, + set power down bits
	PCD_Wake				= 0x37,		// no command change, + clr power down bits
	PCD_Transceive	= 0x0C,		// transmits data from FIFO buffer to antenna and automatically activates the receiver after transmission
	PCD_MFAuthent 	= 0x0E,		// performs the MIFARE standard authentication as a reader
	PCD_SoftReset		= 0x0F		// resets the MFRC522
};

enum PICC_Command // Commands sent to the PICC.
{
	// The commands used by the PCD to manage communication with several PICCs (ISO 14443-3, Type A, section 6.4)
	PICC_CMD_REQA					= 0x26,		// REQuest command, Type A. Invites PICCs in state IDLE to go to READY and prepare for anticollision or selection. 7 bit frame.
	PICC_CMD_WUPA					= 0x52,		// Wake-UP command, Type A. Invites PICCs in state IDLE and HALT to go to READY(*) and prepare for anticollision or selection. 7 bit frame.
	PICC_CMD_CT						= 0x88,		// Cascade Tag. Not really a command, but used during anti collision.
	PICC_CMD_SEL_CL1			= 0x93,		// Anti collision/Select, Cascade Level 1
	PICC_CMD_SEL_CL2			= 0x95,		// Anti collision/Select, Cascade Level 2
	PICC_CMD_SEL_CL3			= 0x97,		// Anti collision/Select, Cascade Level 3
	PICC_CMD_HLTA					= 0x50,		// HaLT command, Type A. Instructs an ACTIVE PICC to go to state HALT.
	// The commands used for MIFARE Classic (from http://www.nxp.com/documents/data_sheet/MF1S503x.pdf, Section 9)
	// Use PCD_MFAuthent to authenticate access to a sector, then use these commands to read/write/modify the blocks on the sector.
	PICC_CMD_MF_AUTH_KEY_A= 0x60,		// Perform authentication with Key A (not using Key B)
	PICC_CMD_MF_READ			= 0x30		// Reads one 16 uint8 block from the authenticated sector of the PICC. Also used for MIFARE Ultralight.
};

enum StatusCode		// Return codes from the functions in this class
{
	STATUS_OK						= 1,	// Success
	STATUS_ERROR				= 2,	// Error in communication
	STATUS_COLLISION		= 3,	// Collission detected
	STATUS_TIMEOUT			= 4,	// Timeout in communication.
	STATUS_INVALID			= 7,	// Invalid argument.
	STATUS_CRC_WRONG		= 8,	// The CRC_A does not match
	STATUS_MIFARE_NACK	= 9		// A MIFARE PICC responded with NAK.
};

uint8 key[6] = {0xff,0xff,0xff,0xff,0xff,0xff}; // default key used to decrypt the RFID, change if required
tuid  uid;
char  RFIDData[64];
uint8 RFIDBuf[64];


void PCD_WriteRegister(uint8 addr, uint8 data) // write uint8 to RFID over SPI Bus (see also figure 25 of manual)
{
	SPISelect(SelRFID);		// Select RC522 RFID Chip (Any other SPI devices must be deselected!)
	addr = (addr<<1) & 0x7E;	// address shifted left into bits 1-6, See datasheet 8.1.2.3 and MSB Must be cleared
	SPITransfer(addr);				// Write address to bus to begin transmission
  SPITransfer(data);				// write data to bus
	SPISelect(SelNONE);		// De-Select RC522 RFID Chip
}

void PCD_WriteRegisters(uint8 addr, uint8 length, uint8 *data)	// Write multiple data to the FIFO
{
	uint8 i;
	SPISelect(SelRFID);						// Select RC522 RFID Chip (Any other SPI devices must be deselected!)
	addr = (addr<<1) & 0x7E;	// address shifted left into bits 1-6, See datasheet 8.1.2.3 and MSB Must be cleared
	SPITransfer(addr);				// Write address to bus to begin transmission
	for(i=0; i<length; i++)
		SPITransfer(data[i]);		// write data to bus
	SPISelect(SelNONE);		// De-Select RC522 RFID Chip
}

uint8 PCD_ReadRegister(uint8 addr) // Read uint8 from RFID over SPI Bus (8.1.2.1 We read the SPI twice as data only available after second read see also figure 25 )
{
	uint8 i;
	SPISelect(SelRFID);						// Select RC522 RFID Chip (Any other SPI devices must be deselected!)
	addr = ((addr<<1) & 0x7e) | 0x80; // address shifted left as datasheet 8.1.2.3 and MSB must be set
	i = SPITransfer(addr);						// read once ignore result as it is not available until second read
  i = SPITransfer(addr);	  				// read again for correct value
	SPISelect(SelNONE);						// De-Select RC522 RFID Chip
	return i;													// return value
}

uint8 PCD_ReadRegisters(uint8 addr, uint8 length, uint8 *data, uint8 rxAlign) // Read uint8 from RFID over SPI Bus (8.1.2.1 We read the SPI twice as data only available after second read see also figure 25 )
{
	uint8 index,i,mask,value;
	SPISelect(SelRFID);						// Select RC522 RFID Chip (Any other SPI devices must be deselected!)
	addr = ((addr<<1) & 0x7e) | 0x80; // address shifted left as datasheet 8.1.2.3 and MSB must be set
	SPITransfer(addr);								// read once ignore result as data is not available until second read
	for(index=0; index<length; index++)
	{
		if(index==0 && rxAlign)	// Only update bit positions rxAlign..7 in values[0]
		{
			mask=0;
			for(i=rxAlign; i<=7; i++)
				mask = mask | (1<<i);
			value = SPITransfer(addr);
			data[0] = (data[index] & ~mask) | (value & mask);
		}
		else
	    data[index]=SPITransfer(addr);			// read length uint8s into result array
	}
  return 1;
	SPISelect(SelNONE);	// De-Select RC522 RFID Chip
}

void PCD_SetRegisterBitMask(uint8 addr, uint8 mask) // read RFID register and set bits if req
{
	uint8 tmp = PCD_ReadRegister(addr);
	if(!(tmp & mask)) PCD_WriteRegister(addr, tmp | mask );
}

void PCD_ClearRegisterBitMask(uint8 addr, uint8 mask) // read RFID register and reset bits if req
{
	uint8 tmp = PCD_ReadRegister(addr);
	if(tmp & mask) PCD_WriteRegister(addr, tmp&(~mask) );
}

void CloseRFID()
{
 	PCD_ClearRegisterBitMask(Status2Reg, 0x08);		// Status2Reg[7..0] bits are: TempSensClear I2CForceHS reserved reserved MFCrypto1On ModemState[2:0]
  PCD_ClearRegisterBitMask(TxControlReg, 0x03);	// Antenna Off
  PCD_WriteRegister(CommandReg, PCD_Sleep);			// Put reader into power saving (10uA)
}

uint8 PCD_CalculateCRC(	uint8 *data, uint8 length, uint8 *result)
{
	PCD_WriteRegister(CommandReg, PCD_Idle);			// Stop any active command.
	PCD_WriteRegister(DivIrqReg, 0x04);						// Clear the CRCIRq interrupt request bit
	PCD_SetRegisterBitMask(FIFOLevelReg, 0x80);		// FlushBuffer = 1, FIFO initialization
	PCD_WriteRegisters(FIFODataReg, length, data);// Write data to the FIFO
	PCD_WriteRegister(CommandReg, PCD_CalcCRC);		// Start the calculation
	
	// Wait for the CRC calculation to complete. Each iteration of the while-loop takes 17.73ms.
	uint16 i = 5000;
	uint8  n;
	while (1) 
	{
		n = PCD_ReadRegister(DivIrqReg);	// DivIrqReg[7..0] bits are: Set2 reserved reserved MfinActIRq reserved CRCIRq reserved reserved
		if (n & 0x04) break;				// CRCIRq bit set - calculation done
		if (--i == 0) return STATUS_TIMEOUT;// The emergency break. If chip down We will eventually terminate on this one after 89ms.
	}
	PCD_WriteRegister(CommandReg, PCD_Idle);		// Stop calculating CRC for new content in the FIFO.
	
	// Transfer the result from the registers to the result buffer
	result[0] = PCD_ReadRegister(CRCResultRegL);
	result[1] = PCD_ReadRegister(CRCResultRegH);
	return STATUS_OK;
}


// Transfers data to the MFRC522 FIFO, executes a command, waits for completion and transfers data back from the FIFO.
// CRC validation can only be done if backData and backLen are specified.
uint8 PCD_CommunicateWithPICC(	uint8 command,		// < The command to execute. One of the PCD_Command enums.
																uint8 waitIRq,		// < The bits in the ComIrqReg register that signals successful completion of the command.
																uint8 *sendData,	// < Pointer to the data to transfer to the FIFO.
																uint8 sendLen,		// < Number of uint8s to transfer to the FIFO.
																uint8 *backData,	// < NULL or pointer to buffer if data should be read back after executing the command.
																uint8 *backLen,		// < In: Max number of uint8s to write to *backData. Out: The number of uint8s returned.
																uint8 *validBits,	// < In/Out: The number of valid bits in the last uint8. 0 for 8 valid bits.
																uint8 rxAlign,		// < In: Defines the bit position in backData[0] for the first bit received. Default 0.
																uint8 checkCRC		// < In: True => The last two uint8s of the response is assumed to be a CRC_A that must be validated.
															)
{
	// Prepare values for BitFramingReg
	uint8 txLastBits = validBits ? *validBits : 0;
	uint8 bitFraming = (rxAlign << 4) + txLastBits;		// RxAlign = BitFramingReg[6..4]. TxLastBits = BitFramingReg[2..0]
	
	PCD_WriteRegister(CommandReg, PCD_Idle);			// Stop any active command.
	PCD_WriteRegister(ComIrqReg, 0x7F);					// Clear all seven interrupt request bits
	PCD_SetRegisterBitMask(FIFOLevelReg, 0x80);			// FlushBuffer = 1, FIFO initialization
	PCD_WriteRegisters(FIFODataReg, sendLen, sendData);	// Write sendData to the FIFO
	PCD_WriteRegister(BitFramingReg, bitFraming);		// Bit adjustments
	PCD_WriteRegister(CommandReg, command);				// Execute the command
	if (command == PCD_Transceive) 
	{
		PCD_SetRegisterBitMask(BitFramingReg, 0x80);	// StartSend=1, transmission of data starts
	}
	
	// Wait for the command to complete.
	// In PCD_Init() we set the TAuto flag in TModeReg. This means the timer automatically starts when the PCD stops transmitting.
	// Each iteration of the do-while-loop takes 17.86ms.
	uint8 n;
//	int16 i = 35;
	while (1) 
	{
		n = PCD_ReadRegister(ComIrqReg);	// ComIrqReg[7..0] bits are: Set1 TxIRq RxIRq IdleIRq HiAlertIRq LoAlertIRq ErrIRq TimerIRq
		if (n & waitIRq) break;				// One of the interrupts that signal success has been set.
		if (n & 0x01) return STATUS_TIMEOUT;
//		if (--i == 0) return STATUS_TIMEOUT;// The emergency break. If all other condions fail we will eventually terminate on this one after 35.7ms. Communication with the MFRC522 might be down.
		DelayMs(1);
	}
	
	// Stop now if any errors except collisions were detected.
	uint8 errorRegValue = PCD_ReadRegister(ErrorReg); // ErrorReg[7..0] bits are: WrErr TempErr reserved BufferOvfl CollErr CRCErr ParityErr ProtocolErr
	if (errorRegValue & 0x13) return STATUS_ERROR;
	if (errorRegValue & 0x08) return STATUS_COLLISION;	// Tell about collisions

	uint8 _validBits;
	
	if (backData && backLen) // If the caller wants data back, get it from the MFRC522 fifo.
	{
		n = PCD_ReadRegister(FIFOLevelReg);			// Number of uint8s in the FIFO
		if (n > *backLen) return STATUS_ERROR;
		*backLen = n;											// Number of uint8s returned
		PCD_ReadRegisters(FIFODataReg, n, backData, rxAlign);	// Get received data from FIFO
		_validBits = PCD_ReadRegister(ControlReg) & 0x07;		// RxLastBits[2:0] indicates the number of valid bits in the last received uint8. If this value is 000b, the whole uint8 is valid.
		if (validBits)	*validBits = _validBits;
	
		if (checkCRC)	// Perform CRC_A validation if requested.
		{
			// In this case a MIFARE Classic NAK is not OK.
			if (*backLen == 1 && _validBits == 4) return STATUS_MIFARE_NACK;
			// We need at least the CRC_A value and all 8 bits of the last uint8 must be received.
			if (*backLen < 2 || _validBits != 0) return STATUS_CRC_WRONG;
			// Verify CRC_A - do our own calculation and store the control in controlBuffer.
			uint8 controlBuffer[2];
			n = PCD_CalculateCRC(&backData[0], *backLen - 2, &controlBuffer[0]);
			if (n != STATUS_OK) return n;
			if ((backData[*backLen - 2] != controlBuffer[0]) || (backData[*backLen - 1] != controlBuffer[1])) return STATUS_CRC_WRONG;
		}
	}
	return STATUS_OK;
}

// Executes the Transceive command.
// CRC validation can only be done if backData and backLen are specified.
uint8 PCD_TransceiveData(	uint8 *sendData,	// < Pointer to the data to transfer to the FIFO.
													uint8 sendLen,		// < Number of uint8s to transfer to the FIFO.
													uint8 *backData,	// < NULL or pointer to buffer if data should be read back after executing the command.
													uint8 *backLen,		// < In: Max number of uint8s to write to *backData. Out: The number of uint8s returned.
													uint8 *validBits,	// < In/Out: The number of valid bits in the last uint8. 0 for 8 valid bits. Default NULL.
													uint8 rxAlign,		// < In: Defines the bit position in backData[0] for the first bit received. Default 0.
													uint8 checkCRC		// < In: True => The last two uint8s of the response is assumed to be a CRC_A that must be validated.
												 )
{
	uint8 waitIRq = 0x30;		// RxIRq and IdleIRq
	return PCD_CommunicateWithPICC(PCD_Transceive, waitIRq, sendData, sendLen, backData, backLen, validBits, rxAlign, checkCRC);
}

/*
 * Transmits SELECT/ANTICOLLISION commands to select a single PICC.
 * Before calling this function the PICCs must be placed in the READY(*) state by calling PICC_RequestA() or PICC_WakeupA().
 * On success:
 * 		- The chosen PICC is in state ACTIVE(*) and all other PICCs have returned to state IDLE/HALT. (Figure 7 of the ISO/IEC 14443-3 draft.)
 * 		- The UID size and value of the chosen PICC is returned in *uid along with the SAK.
 * 
 * A PICC UID consists of 4, 7 or 10 uint8s.
 * Only 4 uint8s can be specified in a SELECT command, so for the longer UIDs two or three iterations are used:
 * 		UID size	Number of UID uint8s		Cascade levels		Example of PICC
 * 		========	===================		==============		===============
 * 		single				 4						1				MIFARE Classic
 * 		double				 7						2				MIFARE Ultralight
 * 		triple				10						3				Not currently in use?
 * 
 * @return STATUS_OK on success, STATUS_??? otherwise.
 */
uint8 PICC_Select() //  Uses Uid struct. Normally output, but can also be used to supply a known UID. UIDvalidbits normally 0
{
	uint8 cascadeLevel	= 1;
	uint8 selectDone, useCascadeTag, result, count, index, responseLength;
	uint8 uidIndex;					// The first index in uid->uiduint8[] that is used in the current Cascade Level.
	char currentLevelKnownBits;		// The number of known UID bits in the current Cascade Level.
	uint8 buffer[9];					// The SELECT/ANTICOLLISION commands uses a 7 uint8 standard frame + 2 uint8s CRC_A
	uint8 bufferUsed;				// The number of uint8s used in the buffer, ie the number of uint8s to transfer to the FIFO.
	uint8 rxAlign;					// Used in BitFramingReg. Defines the bit position for the first bit received.
	uint8 txLastBits;				// Used in BitFramingReg. The number of valid bits in the last transmitted uint8.
	uint8 *responseBuffer;
	
	// Description of buffer structure:
	//		uint8 0: SEL 				Indicates the Cascade Level: PICC_CMD_SEL_CL1, PICC_CMD_SEL_CL2 or PICC_CMD_SEL_CL3
	//		uint8 1: NVB					Number of Valid Bits (in complete command, not just the UID): High nibble: complete uint8s, Low nibble: Extra bits.
	//		uint8 2: UID-data or CT		See explanation below. CT means Cascade Tag.
	//		uint8 3: UID-data
	//		uint8 4: UID-data
	//		uint8 5: UID-data
	//		uint8 6: BCC					Block Check Character - XOR of uint8s 2-5
	//		uint8 7: CRC_A
	//		uint8 8: CRC_A
	// The BCC and CRC_A is only transmitted if we know all the UID bits of the current Cascade Level.
	//
	// Description of uint8s 2-5: (Section 6.5.4 of the ISO/IEC 14443-3 draft: UID contents and cascade levels)
	//		UID size	Cascade level	uint82	uint83	uint84	uint85
	//		========	=============	=====	=====	=====	=====
	//		 4 uint8s		1			uid0	uid1	uid2	uid3
	//		 7 uint8s		1			CT		uid0	uid1	uid2
	//						2			uid3	uid4	uid5	uid6
	//		10 uint8s		1			CT		uid0	uid1	uid2
	//						2			CT		uid3	uid4	uid5
	//						3			uid6	uid7	uid8	uid9
	
	PCD_ClearRegisterBitMask(CollReg, 0x80);		// Prepare RC522 ValuesAfterColl=1 => Bits received after collision are cleared.

	// Repeat Cascade Level loop until we have a complete UID.
	uint8 uidComplete = 0;
	
	while ( ! uidComplete) 
	{		
		switch (cascadeLevel) // Set the Cascade Level in the SEL uint8, find out if we need to use the Cascade Tag in uint8 2.
		{
			case 1:
				buffer[0] = PICC_CMD_SEL_CL1;
				uidIndex = 0;
				useCascadeTag = uid.size > 4;	// When we know that the UID has more than 4 uint8s
				break;
			
			case 2:
				buffer[0] = PICC_CMD_SEL_CL2;
				uidIndex = 3;
				useCascadeTag = uid.size > 7;	// When we know that the UID has more than 7 uint8s
				break;
			
			case 3:
				buffer[0] = PICC_CMD_SEL_CL3;
				uidIndex = 6;
				useCascadeTag = 0;						// Never used in CL3.
				break;
			
			default:
				return STATUS_ERROR;
				break;
		}
		
		currentLevelKnownBits = 0;
		
		// Copy the known bits from uid->uiduint8[] to buffer[]
		index = 2; // destination index in buffer[]
		if (useCascadeTag) 
		{
			buffer[index++] = PICC_CMD_CT;
		}
		
		uint8 uint8sToCopy = (currentLevelKnownBits % 8 ? 1 : 0); // The number of uint8s needed to represent the known bits for this level.
		if (uint8sToCopy)
		{
			uint8 maxuint8s = useCascadeTag ? 3 : 4; // Max 4 uint8s in each Cascade Level. Only 3 left if we use the Cascade Tag
			if (uint8sToCopy > maxuint8s)
			{ 
				uint8sToCopy = maxuint8s;
			}
			for (count = 0; count < uint8sToCopy; count++)
			{
				buffer[index++] = uid.uiduint8[uidIndex + count];
			}
		}
		
		// Now that the data has been copied we need to include the 8 bits in CT in currentLevelKnownBits
		if (useCascadeTag) currentLevelKnownBits += 8;
		
		// Repeat anti collision loop until we can transmit all UID bits + BCC and receive a SAK - max 32 iterations.
		selectDone = 0;
		while ( ! selectDone) 
		{
			// Find out how many bits and uint8s to send and receive.
			if (currentLevelKnownBits >= 32) // All UID bits in this Cascade Level are known. This is a SELECT.
			{ 
				//Serial.print("SELECT: currentLevelKnownBits="); Serial.println(currentLevelKnownBits, DEC);
				buffer[1] = 0x70; // NVB - Number of Valid Bits: Seven whole uint8s
				// Calculate BCC - Block Check Character
				buffer[6] = buffer[2] ^ buffer[3] ^ buffer[4] ^ buffer[5];
				// Calculate CRC_A
				result = PCD_CalculateCRC(buffer, 7, &buffer[7]);
				if (result != STATUS_OK) 
				{
					return result;
				}
				txLastBits		= 0; // 0 => All 8 bits are valid.
				bufferUsed		= 9;
				// Store response in the last 3 uint8s of buffer (BCC and CRC_A - not needed after tx)
				responseBuffer	= &buffer[6];
				responseLength	= 3;
			}
			else 
			{ // This is an ANTICOLLISION.
				//Serial.print("ANTICOLLISION: currentLevelKnownBits="); Serial.println(currentLevelKnownBits, DEC);
				txLastBits		= currentLevelKnownBits % 8;
				count			= currentLevelKnownBits / 8;	// Number of whole uint8s in the UID part.
				index			= 2 + count;					// Number of whole uint8s: SEL + NVB + UIDs
				buffer[1]		= (index << 4) + txLastBits;	// NVB - Number of Valid Bits
				bufferUsed		= index + (txLastBits ? 1 : 0);
				// Store response in the unused part of buffer
				responseBuffer	= &buffer[index];
				responseLength	= sizeof(buffer) - index;
			}

			// Set bit adjustments
			rxAlign = txLastBits;											// Having a seperate variable is overkill. But it makes the next line easier to read.
			PCD_WriteRegister(BitFramingReg, (rxAlign << 4) + txLastBits);	// RxAlign = BitFramingReg[6..4]. TxLastBits = BitFramingReg[2..0]

			// Transmit the buffer and receive the response.
			result = PCD_TransceiveData(buffer, bufferUsed, responseBuffer, &responseLength, &txLastBits, rxAlign,0);
			if (result == STATUS_COLLISION) // More than one PICC in the field => collision.
			{
				result = PCD_ReadRegister(CollReg); // CollReg[7..0] bits are: ValuesAfterColl reserved CollPosNotValid CollPos[4:0]
				if (result & 0x20) // CollPosNotValid
				{
					return STATUS_COLLISION; // Without a valid collision position we cannot continue
				}
				uint8 collisionPos = result & 0x1F; // Values 0-31, 0 means bit 32.
				if (collisionPos == 0) 
				{
					collisionPos = 32;
				}
				if (collisionPos <= currentLevelKnownBits) // No progress - should not happen 
				{
					return STATUS_ERROR;
				}
				// Choose the PICC with the bit set.
				currentLevelKnownBits = collisionPos;
				count			= (currentLevelKnownBits - 1) % 8; // The bit to modify
				index			= 1 + (currentLevelKnownBits / 8) + (count ? 1 : 0); // First uint8 is index 0.
				buffer[index]	|= (1 << count); 
			}
			else if (result != STATUS_OK) 
			{
				return result;
			}
			else 
			{ // STATUS_OK
				if (currentLevelKnownBits >= 32)  // This was a SELECT.
					selectDone = 1; // No more anticollision
					// We continue below outside the while.
				else // This was an ANTICOLLISION.
				{
					// We now have all 32 bits of the UID in this Cascade Level
					currentLevelKnownBits = 32;
					// Run loop again to do the SELECT.
				}
			}
		} // End of while ( ! selectDone)

		// We do not check the CBB - it was constructed by us above.
		
		// Copy the found UID uint8s from buffer[] to uid->uiduint8[]
		index			= (buffer[2] == PICC_CMD_CT) ? 3 : 2; // source index in buffer[]
		uint8sToCopy		= (buffer[2] == PICC_CMD_CT) ? 3 : 4;
		for (count = 0; count < uint8sToCopy; count++)
		{
			uid.uiduint8[uidIndex + count] = buffer[index++];
		}
		
		// Check response SAK (Select Acknowledge)
		if (responseLength != 3 || txLastBits != 0) 		// SAK must be exactly 24 bits (1 uint8 + CRC_A).
		{
			return STATUS_ERROR;
		}
		// Verify CRC_A - do our own calculation and store the control in buffer[2..3] - those uint8s are not needed anymore.
		result = PCD_CalculateCRC(responseBuffer, 1, &buffer[2]);
		if (result != STATUS_OK) 
		{
			return result;
		}
		if ((buffer[2] != responseBuffer[1]) || (buffer[3] != responseBuffer[2])) 
		{
			return STATUS_CRC_WRONG;
		}
		if (responseBuffer[0] & 0x04) // Cascade bit set - UID not complete yes
		{
			cascadeLevel++;
		}
		else 
		{
			uidComplete = 1;
			uid.sak = responseBuffer[0];
		}
	} // End of while ( ! uidComplete)
	
	// Set correct uid->size
	uid.size = 3 * cascadeLevel + 1;

	return STATUS_OK;
} 

/**
 * Executes the MFRC522 MFAuthent command.
 * This command manages MIFARE authentication to enable a secure communication to any MIFARE Mini, MIFARE 1K and MIFARE 4K card.
 * The authentication is described in the MFRC522 datasheet section 10.3.1.9 and http://www.nxp.com/documents/data_sheet/MF1S503x.pdf section 10.1.
 * For use with MIFARE Classic PICCs.
 * The PICC must be selected - ie in state ACTIVE(*) - before calling this function.
 * Remember to call PCD_StopCrypto1() after communicating with the authenticated PICC - otherwise no new communications can start.
 * 
 * All keys are set to FFFFFFFFFFFFh at chip delivery.
 * 
 * @return STATUS_OK on success, STATUS_??? otherwise. Probably STATUS_TIMEOUT if you supply the wrong key.
 */
uint8 PCD_Authenticate(uint8 authCommand, uint8 blockAddr)
{
	// Build command buffer
	uint8 sendData[12];
	uint8 i;
	sendData[0] = authCommand;
	sendData[1] = blockAddr;
	for (i = 0; i < 6; i++) 	// 6 key uint8s
	{
		sendData[2+i] = key[i];
	}
	for (i = 0; i < 4; i++) 				// The first 4 uint8s of the UID
	{
		sendData[8+i] = uid.uiduint8[i];
	}
	
	// Start the authentication.
	i = PCD_CommunicateWithPICC(PCD_MFAuthent, 0x10, &sendData[0], sizeof(sendData), 0, 0, 0, 0, 0);
	return i;
}

uint8 PICC_ReadBlock(uint8 blockAddr)
{
	uint8 bufferATQA[2];
	uint8 buffSize = 2;
	uint8 trailerBlock = 7; // reading sector #1, covering block #4 up to and including block #7
	uint8 validBits = 7;	// For REQA and WUPA we need the short frame format - transmit only 7 bits of the last (and only) uint8. TxLastBits = BitFramingReg[2..0]
  uint8 i;
  
  // Init MFRC522 card
	PCD_WriteRegister(CommandReg, PCD_SoftReset);	// Issue the SoftReset command.
	DelayMs(50);// Wait for the PowerDown bit in CommandReg to be cleared
	
	// When communicating with a PICC we need a timeout if something goes wrong.
	// f_timer = 13.56 MHz / (2*TPreScaler+1) where TPreScaler = [TPrescaler_Hi:TPrescaler_Lo].
	// TPrescaler_Hi are the four low bits in TModeReg. TPrescaler_Lo is TPrescalerReg.
	PCD_WriteRegister(TModeReg, 0x80);			// TAuto=1; timer starts automatically at the end of the transmission in all communication modes at all speeds
	PCD_WriteRegister(TPrescalerReg, 0xA9);		// TPreScaler = TModeReg[3..0]:TPrescalerReg, ie 0x0A9 = 169 => f_timer=40kHz, ie a timer period of 25?s.
	PCD_WriteRegister(TReloadRegH, 0x03);		// Reload timer with 0x3E8 = 1000, ie 25ms before timeout.
	PCD_WriteRegister(TReloadRegL, 0xE8);
	PCD_WriteRegister(TxASKReg, 0x40);		// Default 0x00. Force a 100 % ASK modulation independent of the ModGsPReg register setting
	PCD_WriteRegister(ModeReg, 0x3D);		// Default 0x3F. Set the preset value for the CRC coprocessor for the CalcCRC command to 0x6363 (ISO 14443-3 part 6.2.4)
	PCD_SetRegisterBitMask(TxControlReg, 0x03); // turn antenna on.. reader now ready

  // Transmits REQA or WUPA commands to wake up select tag
	PCD_ClearRegisterBitMask(CollReg, 0x80);
	RFIDBuf[0] = PICC_CMD_REQA;
	if (PCD_TransceiveData(RFIDBuf, 1, bufferATQA, &buffSize, &validBits,0,0) != STATUS_OK) return 0;
	if (validBits != 0) return 0;
	if ( PICC_Select() != STATUS_OK) return 0;
	
	if (uid.sak != 0x08)	return 0; // incompatable card types, I'm only supportting the common MIFARE_1K (S50)
	if(blockAddr==0) return 1; // ok. SerialNo and card type is now available in struct uid
	
	buffSize = 18;					// 16 bytes + 2 CRC

	if ( PCD_Authenticate(PICC_CMD_MF_AUTH_KEY_A, blockAddr) != STATUS_OK) return 0; // Authenticate using key A, return if Auth FAILS!

	RFIDBuf[0] = PICC_CMD_MF_READ;
	RFIDBuf[1] = blockAddr;
	if (PCD_CalculateCRC(RFIDBuf, 2, &RFIDBuf[2]) != STATUS_OK) return 0; // Calculate CRC_A

  i = PCD_TransceiveData(RFIDBuf, 4, RFIDBuf, &buffSize, NULL, 0, 1);
	if (i != STATUS_OK ) return 0; // Transmit buffer and validate CRC_A on response

  for(i=0; i<buffSize; i++)
		RFIDData[i]=RFIDBuf[i];
//	RFIDData[i]='\0';
  return 1;
}

uint8 ReadRFIDUID() // public function encapsulates above to read just the uid
{
	uint16 loops		= 50; // num of loops to do before timeout, set to about 5 seconds
	uint32 serialno =	0;
	char  buf[20];
	while(--loops)
	{
		if(PICC_ReadBlock(0)) // func takes very approx 100mS
		{
			CloseRFID();
			serialno = uid.uiduint8[0] + (uid.uiduint8[1]<<8) + (uid.uiduint8[2]<<16) + (uid.uiduint8[3]<<24);
			strcpy(uid.SerialNo,utoa(buf,serialno,10));
			beep(50);
			DelayMs(100);
			beep(50);
			return 1;
		}
	}
	CloseRFID();
	beep(500);
	return 0;
}

uint8 ReadRFIDBlock(uint8 block) // public function encapsulates above to read selected block or timeout
{
	uint16 loops = 100; // num of loops to do before timeout
	while(--loops)
	{
		if(PICC_ReadBlock(block)) // func takes very approx 100mS
		{
			CloseRFID();
			beep(50);
			DelayMs(100);
			beep(50);
			return 1;
		}
	}
	CloseRFID();
	beep(500);
	return 0;
}