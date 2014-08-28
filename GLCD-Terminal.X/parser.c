/* Mobile data collection terminal basic byte code parser
 * 
 * Code interprets the users bytecode program into actions on the mobile hardware
 * The bytecode is defined in the mbasic documentation which must accompany this file
 * the default action on reset is start at line 
 */

#ifndef PARSER_C
#define PARSER_C

#include "hardware.c"
#include "common-defs.h"
#include "helper_funcs.c"

#define BYTE_RESET		0x01 /* Resets user program back to start */
#define BYTE_GOTO			0x02 /* Expect 2 bytes after this MSB then LSB of new app Program Counter value */
#define BYTE_BEEP  		0x03 /* Expect 1 Byte after this with the length of the beep in inits of 25mS (shorted beep possible)*/
#define BYTE_CLS  		0x04 /* no additional params, clears the screen */
#define BYTE_DELAY		0x05 /* Byte1 = delay in units of 100mS (upto 25 Seconds)*/
#define BYTE_INPUT		0x06 /* Byte1 following this says input from what where 0x01=Keypad, 0x02=Barcode, 0x03=RFID, (0x04=COM Future!)
														* Byte2 = MSB of string in app RAM to put data, Byte3=LSB of string in app RAM
														*	Byte1 = 0x01 Keypad - Byte4=Xcursor pos, Byte5=YCursor pos, Byte6=MaxLen, Byte7=Allow Dec
														* 0x02 Barcode (no other parameters req)
														* 0x03 RFID (No other parameters req)
														* ** All inputs use default timeouts for that input which can be set as system variables **
														*/
#define BYTE_IFTHEN   0x07 /* Byte1 following this is MSB of new PC is expression TRUE, byte2=LSB, bytes3 onward are an expression (see expression info) */
#define BYTE_WHILE		0x08 /* Executes loop until condition false, byte1 onward following this are an expression (see expression info) */
#define BYTE_WEND			0x09 /* terminates above loop if above condition false PC execution continues in byte code after */
#define BYTE_GOSUB		0x0a /* Expect 2 bytes after this MSB then LSB of new app Program Counter value, current PC addres will be popped on the stack */
#define BYTE_RETURN		0x0b
#define BYTE_LET			0x0c
#define BYTE_PRINT		0x0d
#define BYTE_BOX	  	0x0e
#define BYTE_LINE	  	0x0f
#define BYTE_GETKEY		0x10

void run()
{	
	if(!loaded) load(); // load byte code program from host if not loaded already

	ResetProgramVars(); // reset program variables to known condition

	while(1) // do until reset or crash or power off
	{
		// Process the easy byte commands first

		if(app[prg.PC]==BYTE_RESET) break; // reset user app by exiting this loop and going to outer loop which resets

		if(app[prg.PC]==BYTE_GOTO) // Set the program counter to the 2 bytes following the BYTE_GOTO statement
		{
			// it is the byte code compilers job to ensure this address is legal!
			prg.PC=(app[prg.PC]<<8)+app[prg.PC+1];
			continue;
		}

		if(app[prg.PC]==BYTE_BEEP) // beep the beeper
		{
			beep(app[prg.PC+1]*25);
			prg.PC+=2;
			continue;
		}

		if(app[prg.PC]==BYTE_CLS) // Clear screen
		{
			CLS();
			prg.PC+=1;
			continue;
		}

		if(app[prg.PC]==BYTE_BEEP) // Delay
		{
			DelayMs(app[prg.PC+1]*100);
			prg.PC+=2;
			continue;
		}

		if(app[prg.PC]==BYTE_INPUT) // Set the program counter to the 2 bytes following the BYTE_GOTO statement
		{

			prg.PC=(app[prg.PC]<<8)+app[prg.PC+1];
			continue;
		}

		// if we get to here it means we have encountered an unsupported byte code or something has gone wrong so crash/fail
		AppCrash("00 Bad Instr");

	}	// End of paser loop
}

#endif
