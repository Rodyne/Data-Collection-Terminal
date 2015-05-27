/*
 * procs and definitions specific to the data collection terminal hardware located on the motherboard
 * ie the CPU (PIC32MX250F128D), KEYBOARD, BEEPER + Low level drivers for Power saving, SPI and SERIAL
*/

#define _SUPPRESS_PLIB_WARNING	// Get rid of annoying XC32 plib depreciation warnings
#include <plib.h>								// microchip pic32 peripheral libraries
#include "hardware.h"
#include "lcd.h"

// USB Bootloader Should be possible with little or no change to use CHIPKIT and AVRDUDE USB bootloader (cct is similar to fabuino mini)

#pragma config		POSCMOD = XT			// Primary Oscillator Configuration (XT = 3-10Mhz XTAL)
#pragma config		IESO = OFF				// Internal/External Switch Over Disabled
#pragma config		FPBDIV = DIV_1		// Peripheral Clock Divisor (Pb_Clk is Sys_Clk/1)
#pragma config		OSCIOFNC = OFF		// CLKO Output Signal Active on the OSCO Pin Disabled
#pragma config		FSOSCEN = OFF			// Secondary Oscillator Disabled not required and osc2 pins are needed
#pragma config		FWDTEN  = OFF			// Watchdog Timer disabled (will enable in software when required)
#pragma config		WDTPS = PS1024		// watchdog timeout = 1024mS, use it to wake from sleep and do housekeeping while waiting for user
#pragma config		FCKSM = CSDCMD		// clock switching and checking disabled
#pragma config		ICESEL = ICS_PGx2	// required to DEBUG
#pragma config		FUSBIDIO = OFF		// USB ID Pin not used
#pragma config		FNOSC = PRIPLL		// Oscillator Selection = Primary Osc PLL
#pragma config		FPLLIDIV = DIV_2	// 8Mhz -> 4mhz
#pragma config		FPLLMUL = MUL_20	// 4Mhz x 20 = 80Mhz
#pragma config		FPLLODIV = DIV_4	// div4 = 20Mhz
#pragma config		FVBUSONIO = ON    // VBUS input from USB
#define SYS_FREQ	20000000ul				// 8MHZ XTAL PLL up to 20Mhz, (also if uart is 115200 baud)
#define US_DELAY	2									// hard coded timing variable for delay routines below

#define GetSystemClock()			(SYS_FREQ)       // Used instead of SYS_FREQ in some modules.
#define GetInstructionClock()	GetSystemClock() // GetSystemClock()/1 for PIC32.  Might need changing if using Doze modes.
#define GetPeripheralClock()	GetSystemClock() // GetSystemClock()/1 for PIC32.  Divisor may be different if using a PIC32 sinc


// local variables

volatile uint16  RxData1In, RxData1Out, RxData2In, RxData2Out; // indexes for FIFO for chars read in from uART and char read out by user
volatile uint8   RxBuffer1[32], RxBuffer2[900]; // interrupt receive buffers wi-fi data buffer should be much bigger than barcode as it will be used more
volatile uint8   KeyScan; // what keypad line was active in interrupt

// 4 variables below are also declared extern in header and available to all modules

uint8		USBPowered  = 0;
uint8		DEBUG = 0;
uint8		KEYBEEP = 1;
char		KeypadData[32];
uint32	*NVMPtr;

Tconfiguration	config; // declare system global vars, not really supposed to do in header but seems more logical here

void ReadConfig() 	// retrieve last saved values from NVM (We dont have eeprom so using flash, and reflashing the pic32 will wipe the config)
{
	uint8 i;

	// set defaults
	strcpy(config.field.SSID,"CYBERNET");
	strcpy(config.field.PASS,"");
	strcpy(config.field.HOST,"192.168.1.65");
	strcpy(config.field.UserName,"");
	config.field.PowerOffTimeout = 500; // 500 seconds

		// Look for where NVM record data ends and erased flash begins, saved data is the bytes just before this
	NVMPtr = NVM_START;
	while( *NVMPtr != 0xffffffff && (void*)NVMPtr < NVM_END)
		NVMPtr++;

	NVMPtr = NVMPtr - sizeof(config); // find the start of this data (ie go back 48 bytes)
	if( *(NVMPtr) != 0xffffffff)	 // yip, looks like real data at NVMPtr position so retrieve stored values and overwrite defaults)
	{
		for(i=0; i<sizeof(config); i++)
			config.NVM[i] = *NVMPtr++; // cfg.NVM[] and cfg.fields[] are a union so are effectively the same memory  one expressed as chars the other as longints
	}
}

void SaveConfig() // Save CFG (Note memory is 32 bit wide, we are saving bytes so we need to convert 4 x bytes to Longint before writing
{
	uint8 i;
	// NVMPtr is already set to next free Flash slot due to the NVMread above
	if( (void*)(NVMPtr+sizeof(config)) >= NVM_END)  // check if used all flash. If so have to erase whole page and start again (max 1000 times!)
	{
		NVMErasePage(NVM_START);
		NVMPtr = NVM_START;
	}
	for(i=0; i<sizeof(config); i++)
		NVMWriteWord( (void*) NVMPtr++ , config.NVM[i] ); // cfg.NVM[] and cfg.fields[] are a union so effectively the same memory
}


uint8 BrownOutReset()
{
	uint8 i = RCON;
	RCONbits.BOR = 0; // Reset brown out flag so we know above warning is real brown-out not just a power on reset
	return (i==3);
}

void DelayMs(uint32 time) // Blocking wait in milliseconds (approx)
{
	time = time * US_DELAY * 1000;
	while(time--)	asm("NOP");
}

void DelayUs(uint32 time) // Blocking wait in microseconds (approx)
{
	time = time * US_DELAY;
	while(time--)	asm("NOP");
}

void beep(uint16 BCMilliseconds) // set 2Khz buzzer active for this many mS - note this is blocking but interrupts still work
{
	while(BCMilliseconds--)
	{
		BUZZER(0); // on
		DelayUs(480);
		BUZZER(1); // off / repeat
		DelayUs(480);
	}
}

void  SPISelect(enum TSPISelect device) // Only 1 SPI device can be selected on SPI1 bus so do it here
{
	LCD_CS(device!=SelLCD);		// LCD device selected by pulling CS Low
	RFID_CS(device!=SelRFID);	// RFID device selected by pulling CS Low
}

uint8 SPITransfer(uint8 reg)		// Generic Write/read SPI1 (Before issuing, the correct device must be selected using function above!)
{
  SPI1BUF = reg;								// Write address to bus to begin transmission (MSB=0, address shifted left into bits 1-6, See datasheet 8.1.2.3)
  while(!SPI1STATbits.SPIRBF);  // wait for transfer to complete
	return SPI1BUF;								// must do read so we dont get framing error
}

void PeripheralReset() // hard External reset signal to allow peripheral devices to initialize in controlled manner after power up (LCD and RFID)
{
	EXT_RST(0);
	DelayMs(100);
	EXT_RST(1);
	DelayMs(50);
}

void SerialWrite(UART_MODULE id, uint8 data) // write to UART directly and wait to finish (blocking - hangs execution until finished)
{
  UARTSendDataByte(id, data);
  while(!UARTTransmissionHasCompleted(id));
}

void SerialWriteStr(UART_MODULE id, char *str) // use above to send a string
{
  while(*str!=0)
  {
    SerialWrite(id, *str);
    str++;
  }
}

uint8 SerialAvailable(UART_MODULE id)
{
  if(id==UART1) return RxData1In>RxData1Out; // return num of chars waiting in buffer pr zero if no chars waiting
	else
  if(id==UART2) return RxData2In>RxData2Out;
	else
		return 0;
}

uint8 SerialRead(UART_MODULE id) // Reads buffer. No checks here, you must use SerialDataAvailable before calling this or we return 0x00
{
  if(id==UART1)
  {
    if(RxData1Out<RxData1In) // check we actually have data first
      return RxBuffer1[RxData1Out++]; // no worries as RxDataOut can never be more than RxDataIn and this cannot be bigger than buffer
    else
      return 0; // no data WTF, should have used serialDataAvaiable first!
  }
  else if(id==UART2)
  {
    if(RxData2Out<RxData2In) // check we actually have data first (if rxDataIn=15 we have overrun but we ignore this)
      return RxBuffer2[RxData2Out++]; // no worries as RxDataOut can never be more than RxDataIn and this cannot be bigger than buffer
    else
      return 0; // no data WTF, should have used serialDataAvaiable first!
  }
	return 0; // default nothing happened!
}

void SerialPurge(UART_MODULE id) // discard remaining serial chars and clear errors and buffers
{
	register uint16 i;
  if(id==UART1)
  {
		i=0;
		while(i<sizeof(RxBuffer1)) RxBuffer1[i++]=0; // required only for debugging so I can see array more clearly
		U1STAbits.OERR = 0;
		U1STAbits.FERR = 0;
    RxData1Out=0;
    RxData1In=0;
    IFS1bits.U1RXIF=0; // clear any current interrupt flags
  }
	else if(id==UART2)
  {
		i=0;
		while(i<sizeof(RxBuffer2)) RxBuffer2[i++]=0; // required only for debugging so I can see array more clearly
		U2STAbits.OERR = 0;
		U2STAbits.FERR = 0;
    RxData2Out=0;
    RxData2In=0;
    IFS1bits.U2RXIF=0; // clear any current interrupt flags
  }
}

void SerialOpen(UART_MODULE id) // discard remaining serial chars and clear errors and buffers
{
  if(id==UART1)
  {
  	U1STAbits.UTXEN=1; // put uart tx pins back under uart control
  	SerialPurge(id);
    INTEnable(INT_SOURCE_UART_RX(UART1), INT_ENABLED);
  }
	else if(id==UART2)
  {
	  U2STAbits.UTXEN=1; // put uart tx pins back under uart control
  	SerialPurge(id);
    INTEnable(INT_SOURCE_UART_RX(UART2), INT_ENABLED);
  }
}

void SerialClose(UART_MODULE id)
{
	// turn off interrupts and put UART TX Pins back under normal control so they can be switched to logic 0 state (will draw pwr otherwise)
  if(id==UART1)
  {
    INTEnable(INT_SOURCE_UART_RX(UART1), INT_DISABLED);
  	U1STAbits.UTXEN=0;
	}
	else if(id==UART1)
  {
    INTEnable(INT_SOURCE_UART_RX(UART2), INT_DISABLED);
  	U2STAbits.UTXEN=0;
		UART2_TX(0);
	}
}

void InitPIC32(void) // Initialise PIC32 and terminal io pins
{
  DDPCONbits.JTAGEN = 0; // Turn off JTAG allows RA0, RA1, RA4, and RA5 to be used
  SYSTEMConfig(SYS_FREQ, SYS_CFG_WAIT_STATES | SYS_CFG_PCACHE);

  // Turn on USB and test status of VUSB, then power USB off as we use one of its pins for 
  // do before we set SPI (as enabling USB uses an SPI pin and dispay wont work - need to investigate)
	// I'm sure it will be possible to remedy this if/when I eventually do a USB enabled version
	// anyway if USB powered remove the shutdown option (which wont work anyway as USB power bypasses the switch logic) and add setup option
	
	U1PWRCbits.USBPWR=1;
	DelayMs(20);
	USBPowered=U1OTGSTATbits.SESVD;
 	U1PWRCbits.USBPWR=0;

	CVREFClose();//disables the CVREF module.

  // Init IO Pins and PPS using macros - why? - keep pin defs together in header for easier changing rather than going to top of program and then back here all the time!

	PPSUnLock;					// Unlock PPS (Peripheral Pin Select) to allow PIN Mapping
  SET_PPS_SDO;				// SDO (MOSI) - RPB13
  SET_PPS_SDI;				// SDI (MISO) - RPB1
  SET_PPS_WIFI_TX;		// UART 2 TX	- RPC1
  SET_PPS_WIFI_RX;		// UART 2 RX  - RPA8
  SET_PPS_BARCODE_RX; // UART 1 RX  - RPC3 (note that the uart transmit is not required for the barcode and pin is not connected)
	SET_PPS_INT3;				// External Interrupt 3 - RPB8 Keyboard sense line (KEYSTROBE1)
	SET_PPS_INT4;				// External Interrupt 4 - RPB7 Keyboard sense line (KEYSTROBE0)
	PPSLock;						// lock PPS

	CFG_PORT_A_IN
	CFG_PORT_A_OUT
	CFG_PORT_B_IN
	CFG_PORT_B_OUT
	CFG_PORT_C_IN
	CFG_PORT_C_OUT

	// Note that IO Pins, when not in use, must be set to the same idle state of the pin they control to ensure current is not
	// accidentally supplied to sub-system when not in use (this is a common problem with current being drawn in sleep mode).
	// Once the sub-system is powered up then we can reset to the correct state.

	PWR_HOLD(1);		// hold power on system after keypress (firmware must do this first thing to latch power on!)
	EXT_RST(0);			// LCD + RFID reset line active
  BUZZER(1);			// Turn off (line is pulled low to send current thro buzzer, so 1 is off)
	WIFI_PWR(0);    // turn off
  ESP_PGM(0);			// 0=firmware update mode, 1=Normal mode, however when wifi is innactive we Turn off I/O for power saving
	BARCODE_PWR(0); // Note 0 does not mean 0V it means off (in this case a logic 1 turns the P-Channel MOSFET OFF so the #define is inveted to make an active 1)
  BACKLIGHT(0);   // Turn off (line is pulled low to send current thro backlight, so 1 is off)
  BARCODE_TRIG(0);// 0=Trigger (take reading) 1=idle, however when barcode is innactive we Turn off I/O for power saving

	// turn off any PIC32 modules not used to save power (it all helps!)
	PMD1bits.CTMUMD = 1; // CTMU Off
	PMD1bits.CVRMD = 1;  // CVR Off
	PMD2bits.CMP1MD = 1; // Comparator 1 off
	PMD2bits.CMP2MD = 1; // Comparator 2 off
	PMD2bits.CMP3MD = 1; // Comparator 3 off
	PMD3bits.IC1MD = 1;  // Input compare 1 off
	PMD3bits.IC2MD = 1;  // Input compare 2 off
	PMD3bits.IC3MD = 1;  // Input compare 3 off
	PMD3bits.IC4MD = 1;  // Input compare 4 off
	PMD3bits.IC5MD = 1;  // Input compare 5 off
	PMD3bits.OC1MD = 1;  // output compare 1 off
	PMD3bits.OC2MD = 1;  // output compare 2 off
	PMD3bits.OC3MD = 1;  // output compare 3 off
	PMD3bits.OC4MD = 1;  // output compare 4 off
	PMD3bits.OC5MD = 1;  // output compare 5 off
	PMD4bits.T2MD = 1;   // timer2 off
	PMD4bits.T3MD = 1;   // timer3 off
	PMD4bits.T4MD = 1;   // timer4 off
	PMD4bits.T5MD = 1;   // timer5 off
	PMD5bits.I2C1MD = 1; // I2C Off
	PMD5bits.I2C2MD = 1; // I2C Off
	PMD5bits.SPI2MD = 1; // SPI 2 off
	PMD6bits.PMPMD = 1;  // Parallel Master Port off
	PMD6bits.REFOMD = 1; // ref clk out off

  // SPI
  SPI1CON  = 0;
	//SPI1CON2 = 0;				// CON2 is for SPI Interrupts and Audio so turn off
  SPI1BRG  = 0x0004;		// Baud =  pOSC/4 (Note SPI Data is MSB First if your analysing it)
	SPI1STATbits.SPIROV=0;// Clear SPI Overflow
	SPI1CONbits.MSTEN=1;	// PIC32 is SPI Master
	SPI1CONbits.CKP=1;		// SCK polarity: idle state high, active state low
	SPI1CONbits.CKE=0;		// Serial data output changes on CKP transition from idle to active ie the rising edge
	SPI1CONbits.SMP=1;		// Input data sampled at end of data output time
	SPI1CONbits.ON=1;			// Turn SPI ON
	//SPI1CON = 0x00008260;


	//T1CON = 0x0000A030; // Use TMR1 as a 16 bit counter that will continuously count up at 1 pulse every 12.8uS (20Mhz with div256 prescaler)

	// Configure interrupts but dont enable them until required

	// Use Ext Interrupts for KBD (PIC32 will be in sleep only WDT and ext int will wake)
	ConfigINT3(EXT_INT_DISABLE | FALLING_EDGE_INT | EXT_INT_PRI_2);	// set up external interrupts INT3 (RB8/KEYSTROBE1)
	ConfigINT4(EXT_INT_DISABLE | FALLING_EDGE_INT | EXT_INT_PRI_3); // set up external interrupts INT4 (RB7/KEYSTROBE0)

	// Configure UART1 (higher priority than keybrd)
  UARTConfigure(BARCODE_UART, UART_ENABLE_PINS_TX_RX_ONLY);
  UARTSetLineControl(BARCODE_UART, UART_DATA_SIZE_8_BITS | UART_PARITY_NONE | UART_STOP_BITS_1);
  UARTSetDataRate(BARCODE_UART, GetPeripheralClock(), BARCODE_BAUD);
  UARTEnable(BARCODE_UART, UART_ENABLE_FLAGS(UART_PERIPHERAL | UART_RX | UART_TX) );
	// configure UART1 Interrupt (initially off)
  INTSetVectorPriority(INT_VECTOR_UART(BARCODE_UART), INT_PRIORITY_LEVEL_4);
  INTEnable(INT_SOURCE_UART_RX(BARCODE_UART), INT_DISABLED); // disabled until I say

  // Configure UART2 (higher priority than keybrd)
  UARTConfigure(WIFI_UART, UART_ENABLE_PINS_TX_RX_ONLY);
  UARTSetLineControl(WIFI_UART, UART_DATA_SIZE_8_BITS | UART_PARITY_NONE | UART_STOP_BITS_1);
  UARTSetDataRate(WIFI_UART, GetPeripheralClock(), WIFI_BAUD);
  UARTEnable(WIFI_UART, UART_ENABLE_FLAGS(UART_PERIPHERAL | UART_RX | UART_TX) );
	// configure UART2 Interrupt (initially off)
  INTSetVectorPriority(INT_VECTOR_UART(WIFI_UART), INT_PRIORITY_LEVEL_5);
  INTEnable(INT_SOURCE_UART_RX(WIFI_UART),  INT_DISABLED); // disabled until I say
  
  INTConfigureSystem(INT_SYSTEM_CONFIG_MULT_VECTOR); // Configure for Multi-Vectored Interrupts

	SPISelect(SelNONE); // Select
	PeripheralReset();	// Issue hard reset to LCD and RFID to init

	INTEnableInterrupts(); // gloabally enable interrupts but all are currently configured off until required
}

// 99.99% of the time I expect the program to be here, where it will sit in low power mode until a key pressed or timeout indicated occurs
int8 WaitEvent(uint16 TimeoutSeconds)
{
  const  uint8 ScanCode[16] = { K_1, K_2, K_3, K_OK, K_4, K_5, K_6, K_CANCEL, K_7, K_8, K_9, K_UP, K_DECIMAL, K_0, K_DEL, K_DN }; // map scan 2 code

	uint16 BacklightTimeout = 20;		// 20 seconds
	uint32 PowerTimeout = config.field.PowerOffTimeout;
	Backlight(ON);

	// Peripherals must be off prior to calling this or sleep wont produce any power savings!!

	K0(0); K1(0); K2(0); K3(0); K4(0); K5(0); K6(0); K7(0); // all key lines low so when keypressed the strobe lines will activate
  mINT3IntEnable(1); // enable keystrobe1 line interrupt
  mINT4IntEnable(1); // enable keystrobe0 line interrupt
	KeyScan=0;

	while(KeyScan==0 && TimeoutSeconds--) // wait here for the above timeout or a key press (wakes up and does about 1 loop per second unless interrupted)
	{
		EnableWDT();
		ClearWDT();
	  PowerSaveSleep(); // wait for watchdog timeout (1024mS) or keypress. Only GLCD drawing power so terminal consumption about 500-700uA at this point
		ClearWDT();
		DisableWDT();
		// wakes up every second (or if keypressed) and check for backlight timeout
		if(BacklightTimeout>0) BacklightTimeout--;
		if(BacklightTimeout==0)	Backlight(OFF);
		// check for power off inactivity timeout
    if(PowerTimeout>0) PowerTimeout--;
	  if(PowerTimeout==0) PWR_HOLD(0);
	}
	mINT3IntEnable(0);
	mINT4IntEnable(0);

	uint8 KeyNo	= 0;

	if(KeyScan==1) // SCAN Key caused the interrupt (note disabled at momment)
	{
		beep(200);
		return K_SCAN;
	}
	else
	if(KeyScan == 2 || KeyScan==3) // Get which scan key caused the interrupt
	{
		while(KeyNo<8) // loop through scan lines to find which key asserted
		{
			// do round robin on keylines making each line low in turn
			K0(KeyNo!=0); // only goes low when keyno == 0
			K1(KeyNo!=1); // only goes low when keyno == 1
			K2(KeyNo!=2); // only goes low when keyno == 2
			K3(KeyNo!=3); // only goes low when keyno == 3
			K4(KeyNo!=4); // only goes low when keyno == 4
			K5(KeyNo!=5); // only goes low when keyno == 5
			K6(KeyNo!=6); // only goes low when keyno == 6
			K7(KeyNo!=7); // only goes low when keyno == 7
			if(KEYSTROBE0())
			{
      	Backlight(ON);
      	if(KEYBEEP) beep(50);
				return ScanCode[KeyNo];
			}
			if(KEYSTROBE1())
			{
      	Backlight(ON);
				if(KEYBEEP) beep(50);
				return ScanCode[KeyNo+8];
			}
			KeyNo++;
		}
		return 0;
	}
	else
	  return 0;
}

void __ISR(_EXTERNAL_3_VECTOR,IPL2AUTO) External_Interrupt_3(void) // PRB8 - KEYSTROBE1 goes low
{
	KeyScan = 2;
  mINT3ClearIntFlag();
}

void __ISR(_EXTERNAL_4_VECTOR,IPL3AUTO) External_Interrupt_4(void) // PRB7 - KEYSTROBE0 goes low
{
	KeyScan = 3;
	mINT4ClearIntFlag();
}

void __ISR(_UART1_VECTOR, IPL4AUTO) IntUart1Handler(void) // UART 1 RX Interrupt Handlers
{
  if(IFS1bits.U1RXIF)  // RX char ready to read from UART into Rxbuffer
  {
    RxBuffer1[RxData1In] = UARTGetDataByte(UART1); // Read data from Rx.
    if(RxData1In<(sizeof(RxBuffer1)-1)) // Make sure buffer is not full and move the RxBuffer Index to indicate a char available if we are at the buffer limit this indicates an error
      RxData1In++;
    IFS1bits.U1RXIF=0; // All done, so Clear Interrupt flag to allow new interrupt
  }
} // end UART interrupt handler

void __ISR(_UART_2_VECTOR, IPL5AUTO) IntUart2Handler(void) // UART 2 RX Interrupt Handlers
{
  if(IFS1bits.U2RXIF)  // RX char ready to read from UART into Rxbuffer
  {
    RxBuffer2[RxData2In] = UARTGetDataByte(UART2); // Read data from Rx.
    if(RxData2In<(sizeof(RxBuffer2)-1)) // Make sure buffer is not full and move the RxBuffer Index to indicate a char available if we are at the buffer limit this indicates an error
      RxData2In++;
    IFS1bits.U2RXIF=0; // All done, so Clear Interrupt flag to allow new interrupt
  }
} // end UART interrupt handler
