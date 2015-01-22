/*
 * procs and definitions specific to the data collection terminal hardware revision 2
 *
 * CPU     =  44pin PIC32MX150F128D or PIC32MX170F256D Clocked at 8Mhz using external XTAL (microchip.com)
 * GLCD    =  East Rising ER128128 GLCD (buydisplay.com)
 * Barcode =  Marson MT700 (marson.com.tw)
 * WIFI    =  USR WIFI232-S (en.usr.cn)
 * RFID    =  MLX9019 125Khz reader (www.melexis.com)
 *
*/

#ifndef HARDWARE_C
#define HARDWARE_C

#include "common-defs.h" // Font tables etc
#include "fonts.c" // Font table

#define _SUPPRESS_PLIB_WARNING // Get rid of MPLABX plib depreciation harmony annoyances (WTF Microchip!)
#include <plib.h>    // microchip pic32 peripheral libraries

#define DEBUG_WIFI // show chars from wifi serial
#define CLK_8 // Rev 3 uses 3.57Mhz Clock and slightly different pinIO

// PIC32 Config/Fuse settings

#ifdef CLK_7372  // uses 7.3728Mhz (No PLL) will give 115200 baud (0% error)

  #define SYS_FREQ              7372000ul        // 7.372MHZ
  #pragma config								FNOSC = PRI      // Oscillator Selection = Primary Osc (No PLL we just use the osc as is!)
  #define TIMER1DIVISOR					T1_PS_1_256			 // (pOsc/256/7) timer 1 divisor = 1997 interupts per second (for buzzer/backlight)
  #define TIMER1PERIOD					15							 // (pOSC/256/7) timer 1 period = 1997 interupts per second
  #define uSDELAYMULTIPLIER     2					  		 // Need to set this to ensure microsecond delay routine is correct

#endif

#ifdef CLK_8 // REV1 and 2 used 8Mhz XTAL and PLL to 20Mhz to allow baud rate generator to get to 115200 baud (not achievable without PLL)

  #define SYS_FREQ              20000000ul       // 20Mhz (using 8Mhz XTAL)
  #pragma config								FNOSC = PRIPLL   // Oscillator Selection = Primary Osc (tweek for lowest supporting 115200 baud)
  #pragma config								FPLLIDIV = DIV_2 // must be div 2 to get in pll range for input = 4
  #pragma config								FPLLMUL = MUL_20 // 
  #pragma config								FPLLODIV = DIV_4 //
  #define TIMER1DIVISOR					T1_PS_1_256			 // (20000/256/35) timer 1 divisor = 2000 interupts per second (for buzzer/backlight)
  #define TIMER1PERIOD					35							 // (20000/256/35) timer 1 period = 2000 interupts per second
  #define microsecond						10              // Use old fashioned NOP delay not coretimer (value is number times to call NOP in a loop)

#endif

#pragma config									POSCMOD = XT     // Primary Oscillator Configuration (XT = 3-10Mhz XTAL, HS >10Mhz XTAL)
#pragma config									IESO = OFF       // Internal/External Switch Over Disabled
#pragma config									FPBDIV = DIV_1   // Peripheral Clock Divisor (Pb_Clk is Sys_Clk/1)
#pragma config									OSCIOFNC = OFF   // CLKO Output Signal Active on the OSCO Pin Disabled
#pragma config									FSOSCEN = OFF    // Secondary Oscillator Disabled not required and osc2 pins are needed
#pragma config									FWDTEN  = OFF    // Watchdog Timer disabled initially (will be enabled in software if required)
#pragma config									FCKSM = CSECMD   // clock switching enabled / clock monitor disabled

#define GetSystemClock()				(SYS_FREQ)       // Used instead of SYS_FREQ in some modules.
#define GetInstructionClock()		GetSystemClock() // GetSystemClock()/1 for PIC32.  Might need changing if using Doze modes.
#define GetPeripheralClock()		GetSystemClock() // GetSystemClock()/1 for PIC32.  Divisor may be different if using a PIC32 sinc


// Macro Pin definitions for ALL PIC32 pins. The pin defs and pin macros MUST Match

// Hardware note for PIC32MX170 bit 2 and 3 of port A unavailable as used for XTAL OSC! 5 and 6 dont exist physically!
#define CFG_PORT_A_OUT	PORTSetPinsDigitalOut(IOPORT_A, BIT_4 | BIT_5 | BIT_6 | BIT_7 | BIT_9 | BIT_10 );
#define CFG_PORT_A_IN   PORTSetPinsDigitalIn(IOPORT_A, BIT_0 | BIT_1 | BIT_8 );
#define CFG_PORT_B_OUT	PORTSetPinsDigitalOut(IOPORT_B, BIT_0 | BIT_1 | BIT_2 | BIT_4 |BIT_5 | BIT_9 | BIT_10 | BIT_11 | BIT_12 | BIT_13 | BIT_14 | BIT_15 );
#define CFG_PORT_B_IN   PORTSetPinsDigitalIn(IOPORT_B, BIT_3 | BIT_6 | BIT_7 | BIT_8 );
#define CFG_PORT_C_OUT	PORTSetPinsDigitalOut(IOPORT_C, BIT_0 | BIT_1 | BIT_2 | BIT_4 | BIT_5 | BIT_6 |BIT_7 | BIT_8 | BIT_9  );
#define CFG_PORT_C_IN   PORTSetPinsDigitalIn(IOPORT_C, BIT_3 );

// PORT A
#define RFID_CLK()            PORTAbits.RA0           // IN  (O/C - REQUIRES MCU INTERNAL PU ACTIVE!)
#define RFID_DATA()           PORTAbits.RA1           // IN  (O/C - REQUIRES MCU INTERNAL PU ACTIVE!)
#define BARCODE_RESET(state)  LATAbits.LATA4 = state  // OUT (Barcode reset / active low)
#define UNUSED1(state)        LATAbits.LATA7 = state  // OUT (Set Low)
#define SET_PPS_WIFI_RX       PPSInput(2,U2RX,RPA8)   // IN  (UART 2 SERIAL RECEIVE)
#define BARCODE_TRIG(state)		LATAbits.LATA9 = state  // OUT (Active low, starts a barcode scan)
#define RFID_MOD(state) 			LATAbits.LATA10 = state // OUT (1=RFID POWERDN /  0 = ON, uses open drain to pull low and stop osc in pwr save)
// PORT B
#define BUZZER(state)					LATBbits.LATB0 = state  // OUT modulate to sound buzzer
#define BACKLIGHT(state)      LATBbits.LATB1 = state  // OUT PWM for backlight LED control brightness
#define UNUSED2(state)				LATBbits.LATB2 = state  // OUT (Set low)
#define WIFI_LINK()           !PORTBbits.RB3          // IN  (active low)
#define WIFI_PWR(state)				LATBbits.LATB4 = !state // OUT (inverted as 1=power off, 0=power on)
#define PWR_HOLD(state)				LATBbits.LATB5 = state  // OUT (set high to keep CPU powered on, drop to 'true' power off system fully)
#define KEY_SCAN              !PORTBbits.RB6          // IN  (active low)
#define KEYSTROBE0            !PORTBbits.RB7          // IN  (active low)
#define KEYSTROBE1            !PORTBbits.RB8          // IN  (active low)
#define K0(state)             LATBbits.LATB9  = state // OUT
#define K5(state)             LATBbits.LATB10 = state // OUT
#define K6(state)             LATBbits.LATB11 = state // OUT
#define K7(state)             LATBbits.LATB12 = state // OUT
#define SET_PPS_SDO           PPSOutput(3,RPB13,SDO1) // OUT SPI DATA OUT
#define LCD_CLK(state)        LATBbits.LATB14 = state // OUT
#define LCD_DC(state)         LATBbits.LATB15 = state // OUT
// PORT C
#define WIFI_RESET(state)     LATCbits.LATC0 = state  // OUT (Active low - reset for wifi)
#define LCD_RESET(state)      LATCbits.LATC1 = state  // OUT (Active Low - reset for LCD)
#define SET_PPS_WIFI_TX       PPSOutput(4,RPC2,U2TX)  // OUT (UART 2 TRANSMIT OUT)
#define SET_PPS_BARCODE_RX    PPSInput(3,U1RX,RPC3)   // IN  (UART 1 RECEIVE IN. Note If using SE-1223 *must* be 5V tollerant or use level converter)
#define BARCODE_PWR(state)		LATCbits.LATC4 = !state // OUT (inverted as 1=power off, 0=power on)
#define SET_PPS_BARCODE_TX    PPSOutput(1,RPC5,U1TX)  // OUT (Note we dont actually have a need to write to the barcode module yet maybe never!)
#define K1(state)             LATCbits.LATC6  = state // OUT
#define K2(state)             LATCbits.LATC7  = state // OUT
#define K3(state)             LATCbits.LATC8  = state // OUT
#define K4(state)             LATCbits.LATC9  = state // OUT

#define BARCODE_UART          UART1
#define BARCODE_BAUD          9600
#define WIFI_UART             UART2
#define WIFI_BAUD             115200

// defs only app to this specific hardware

#define KEYTHRESHOLD          2     // 15 times a second if a key pressed it increments its counter, if not it decrements it, if counter>threshold we say is pressed (no debounce)
#define KEYPRESSMAX           40    // this is the number the above counter ceilings at
#define LCD_PG                0xb0  // LCD Pg you can add offset to it
#define NORMAL_CONTRAST       0x14
#define SER1BUFLEN            32    // size of serial buffer COM1 (barcode)
#define SER2BUFLEN            1024  // size of serial buffer COM2 (wifi)
#define KEYBUFLEN							8     // size of keypad buffer
#define LCD_ON								0xAF  // LCD Command to turn on LCD
#define LCD_OFF								0xAE  // LCD Command to turn off LCD
#define LCD_NORMAL						0xA6  // LCD Command to normalise LCD Display (black on white)
#define LCD_REVERSE						0xA7  // LCD Command to invert LCD display (white on black)
#define BARCODE_TIMEOUT				5000  // mS to read barcode before giving up
#define WIFI_TIMEOUT					5000  // mS to wait for response from host before giving up

#define millisecond  					1000*microsecond  // milliseconds

volatile uint8   KeyState[17]; // low level key buffer hold no of pulses counted while key pressed and count reset on release (anti-bounce etc)
volatile uint8   BacklightTimeout, Brightness, triggered;
volatile uint8   KeyNo=0; // key scan processing ticker/counter
volatile uint16  BuzzerTimeout, UserActivityTimeout; // counts up 10 times a second resets if key pressed
volatile uint8   KeyBuf[KEYBUFLEN]; // high level buffer, holds processed keys read by readkey()
volatile uint16  RxData1In, RxData1Out, RxData2In, RxData2Out, KeyBufInPtr, KeyBufOutPtr; // indexes for char read in from uART and char read out by user
volatile uint8   RxBuffer1[SER1BUFLEN], RxBuffer2[SER2BUFLEN]; // interrupt receive buffers wi-fi data buffer should be bigger than barcode as it will be used more
tform            CurScr,SaveScr;
enum tdevicemode WIFIMode; // DATA or CMD

void delay(uint32 time) // Blocking wait of selected no of uS, only use for small delays as CPU is working 100% all this time
{
	while(time--)
		asm("NOP");
}

void WaitEvent() // turn off timer 1 and wait in idle here for another interrupt to occur
{
	// turn everything off
	asm("di");
	BACKLIGHT(0);   // backlight off
	WIFI_PWR(0);    // power off wifi module
	BARCODE_PWR(0); // power off barcode module
  IFS1bits.U1RXIF=0; // clear any current interrupt flags
  IFS1bits.U2RXIF=0; // clear any current interrupt flags
	IFS0bits.T1IF = 0; // clear any current interrupt flags
  IEC0bits.T1IE = 0; // disable timer 1 interrupt
	K0(0); K1(0); K2(0); K3(0); K4(0); K5(0); K6(0); K7(0); // all key lines low so when keypressed the strobe lines will activate
  mCNBIntEnable(TRUE); // turn on change notification for key strobe0 and strobe1
  asm("ei");
	PowerSaveIdle(); // turn off main clock (should be in low power how)
	asm("nop"); // wakes up here
}


/*
 *  GLCD ROUTINES
 *
 */

char LCDWrite(uint8 ntype, uint8 data) // write CMD or DATA byte to LCD over SPI Bus (LCD-CS is low permenantly)
{
  LCD_DC(ntype);                // Set control to CMD or DATA
  SPI1BUF = data;               // write to shift register to begin transmission
  while( !SPI1STATbits.SPIRBF); // wait for last transfer to complete
  return SPI1BUF;								// dummy read
}

void LCDMode(uint8 lcd_mode)
{
	LCDWrite(CMD,lcd_mode); // execute LCD CMD
}

void LCDWriteCharAt( uint8 x, uint8 y, uint8 ch) // write char ch at position x,y (note x,y are in multiples of 8 pixels same as the page size)
{
  uint8 i;
  int16 offset = (prg.FontHeight*(ch-prg.FontMin)); // seek offset into font bitmap

	if(x>prg.MaxCursorX || y>prg.MaxCursorY) return;

	CurScr[x][y]=ch; // keep a snapshot of the screen in case we need to write a window over it and restore it (array must be big enough for [x][y]!)

	LCDWrite(CMD,0x40); // set start line
	LCDWrite(CMD,0);    // 0x00 (we are working in 8 pixel rounding but if we set it >0 we can use pixels inbetween)

	LCDWrite(CMD,LCD_PG+y);
	LCDWrite(CMD,(x*8)&0x0f); // col lsb +stdot!
	LCDWrite(CMD,0x10+((x*8)>>4));		 // col msb
  for(i=0;i<8;i++)
  {
    if(prg.pen) // pen colour > 0 (normal)
    {
      LCDWrite(DATA,font1[offset+i]);
      LCDWrite(DATA,font1[offset+i]);
    }
    else // pen colour=0 ignore background lets invert colours
    {
      LCDWrite(DATA,~font1[offset+i]);
      LCDWrite(DATA,~font1[offset+i]);
    }
  }
  LCDWrite(CMD,LCD_PG+y+1);
	LCDWrite(CMD,(x*8)&0x0f); // col lsb +stdot!
	LCDWrite(CMD,0x10+((x*8)>>4));		 // col msb
  for(i=0;i<8;i++)
  {
    if(prg.pen)
    {
      LCDWrite(DATA,font1[offset+i+8]);
      LCDWrite(DATA,font1[offset+i+8]);
    }
    else
    {
      LCDWrite(DATA,~font1[offset+i+8]);
      LCDWrite(DATA,~font1[offset+i+8]);
    }
  }
}

void LCDWriteChar(uint8 ch) // write char ch at cursor position and inc cursor (go to new line if overruns screen!)
{
	if(ch=='\n')
	{
    prg.CursorY=prg.CursorY+2;
    if(prg.CursorY>prg.MaxCursorY) prg.CursorY=0;
		return;
	}
	if(ch=='\r')
	{
    prg.CursorX=0;
		return;
	}
  if(prg.CursorX<prg.MaxCursorX && prg.CursorY<prg.MaxCursorY)
  {
    LCDWriteCharAt(prg.CursorX,prg.CursorY,ch);
  }
  prg.CursorX++;
  if(prg.CursorX>prg.MaxCursorX)
  {
    prg.CursorX=0;
    prg.CursorY=prg.CursorY+2;
    if(prg.CursorY>prg.MaxCursorY) prg.CursorY=0;
  }
}

void LCDWriteStr(char *str) // call above to write a string
{
	while(*str!='\0')
	{
		LCDWriteChar(*str);
		str++;
	}
}

void LCDGotoXY( uint8 x, uint8 y) // set cursor
{
  prg.CursorX=x;
  prg.CursorY=y;
}

void LCDWriteStrAt( uint8 x, uint8 y, char *str) // call above to write a string
{
  LCDGotoXY(x,y);
	while(*str!='\0')
	{
		LCDWriteChar(*str);
		str++;
	}
}

void CLS() // clear screen with cur canvas
{
	uint8 j,k;
	LCDWrite(CMD,0x40); // set start line
	LCDWrite(CMD,0x00); // 0x00
	for(j=0;j<16;j++)
	{
		LCDWrite(CMD,0x00);
		LCDWrite(CMD,0x10);
		LCDWrite(CMD,0xb0+j);
		for(k=0;k<128;k++)
		{
			LCDWrite(DATA,prg.canvas);
			LCDWrite(DATA,prg.canvas);
		}
	}
  prg.CursorX=0;
  prg.CursorY=0;
}

void Pen(uint8 state)
{
  if(state==BLACK)
  {
    prg.pen=BLACK;
    prg.canvas=WHITE;
  }
  else
  {
    prg.pen=WHITE;
    prg.canvas=BLACK;
  }
}

void LCDSetContrast(uint8 clevel)
{
  if(clevel>0x30) clevel=0x30; // clip max
  if(clevel<0x8) clevel=0x8;   // clip min
 	LCDWrite(CMD,0x81);
 	LCDWrite(CMD,clevel);
}

void LCDSleep()
{
  LCDWrite(CMD,0x00);
}

void InitLCD() // as reset pin is common with wifi and barcode then we assume physical reset has been issued before calling this
{
	int i;
	char greyscale[]={0x00,0x00,0xdd,0xdd,0xaa,0xaa,0xff,0xff};

	// set hardware defs in prg struct to match actual hardware

  prg.MaxCursorX = 16;
  prg.MaxCursorY = 15;
  prg.ScrXMax = 128;
  prg.ScrYMax = 128;

	prg.CursorX=0;
  prg.CursorY=0;
	prg.pen=BLACK;
	prg.canvas=WHITE;

	prg.FontWidth=8;
	prg.FontHeight=16;
	prg.FontMin=0;
	prg.FontMax=96;
	prg.DefBrightness=5;         

  LCD_RESET(0);
	delay(10*millisecond); // while the error comes in
  LCD_RESET(1);
	delay(20*millisecond); // while the error comes in

  // copied verbotem from example on east riding site, have tried to understand but only makes sense if you know how the ST7541 is wired up internally to their LCD I guess!

	LCDWrite(CMD,0x38); // Mode set to 77 hz booster efficiency level 2 (this is the default why issue it?)
	LCDWrite(CMD,0x04); //
  LCDWrite(CMD,0xAB); // Intern OSC ON
	LCDWrite(CMD,0x57); // Set LCD Bias last 3 bits = 1/12 again the default - wtf!)
	LCDWrite(CMD,0x38); // Mode set to 73 hz booster efficiency level 1 why we changing it again?
  LCDWrite(CMD,0x80); //
	LCDWrite(CMD,0x27); // Regulator internal resistance ratio of internal voltage regulator (111) = ratio of 7.2 ?
	LCDWrite(CMD,0x64); // DC DC Step up Vout = 3 x boost
	delay(1*millisecond); // while the error comes in
	LCDWrite(CMD,0x2c); // PowerControll VC= ON VR=OFF VF=OFF
	delay(1*millisecond); // while the error comes in
	LCDWrite(CMD,0x67); // boost level 6
	LCDWrite(CMD,0x2e); // PowerControll VC= ON VR=ON VF=OFF
	delay(1*millisecond); // while the error comes in
	LCDWrite(CMD,0x2f); // PowerControll VC= ON VR=OFF VF=ON
	delay(1*millisecond); // while the error comes in
	LCDWrite(CMD,0x93); // FRC PWM Set 3=FRC, 15=PWM

	for(i=0;i<8;i++)
	{
		LCDWrite(CMD,0x88+i);
		LCDWrite(CMD,greyscale[i]);
	}

	LCDWrite(CMD,0xA1); // ADC Select reverse direction ? COM127 -> COM0
	LCDWrite(CMD,0xC8); // ComScanDirection

  LCDSetContrast(NORMAL_CONTRAST);
	LCDMode(LCD_NORMAL);
	LCDMode(LCD_ON);
}


/*
 *  SERIAL UART ROUTINES
 *
 */

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
  if(id==UART1) return RxData1In-RxData1Out; // return num of chars waiting in buffer pr zero if no chars waiting
  if(id==UART2) return RxData2In-RxData2Out;
}

uint8 SerialRead(UART_MODULE id) // Reads buffer. No checks here, you must use SerialDataAvailable before calling this or we return 0x00
{
  uint8 c;
  if(id==UART1)
  {
    if(RxData1Out<RxData1In) // check we actually have data first
    {
      c = RxBuffer1[RxData1Out++];
      if(RxData1Out==RxData1In)// once up to date reset the buffer indexes back to the begining
			{
        RxData1Out=0;
        RxData1In=0;
			}
      return c;
    }
    else
      return 0; // no data WTF, should have used serialDataAvaiable first!
  }
  else if(id==UART2)
  {
    if(RxData2Out<RxData2In) // check we actually have data first (if rxDataIn=15 we have overrun but we ignore this)
    {
      c = RxBuffer2[RxData2Out++];
      if(RxData2Out==RxData2In)// once up to date reset the buffer indexes back to the begining
			{
        RxData2Out=0;
        RxData2In=0;
			}
      return c;
    }
    else
      return 0; // no data WTF, should have used serialDataAvaiable first!
  }
}

void PurgeSerial(UART_MODULE id) // discard remaining serial chars and clear errors and buffers
{
	uint8 dummychar;
	delay(10*millisecond);
	while(SerialAvailable(id)) // wait for any current chars being sent by device to finish
	{
		dummychar=SerialRead(id); // read/discard them!
  	delay(5*millisecond);
	}
	// clear errors and buffers
  asm("di");
  if(id==UART1)
  {
		U1STAbits.OERR = 0;
		U1STAbits.FERR = 0;
    RxData1Out=0;
    RxData1In=0;
  }
  if(id==UART2)
  {
		U2STAbits.OERR = 0;
		U2STAbits.FERR = 0;
    RxData2Out=0;
    RxData2In=0;
  }
  asm("ei");
}

uint8 SerialDataMatches(uint8 UARTID, char *matching, uint16 TimeoutMs) // will fail if serial data does not match string in serial stream before timeout occurs
{
	char c;
	char *str = matching;
  while(TimeoutMs--) // keep in loop until timeout or match/fail
  {
    if(SerialAvailable(UARTID))
		{
			c=SerialRead(UARTID);
// 		  LCDWriteChar(c);
			if(c == *str) str++; else str=matching; // go to next char if match or reset to begining if match fails
			if(*str == '\0') return 1; // got to end of match string so all chars matched
    }
  	delay(1*millisecond);
  }
	return 0; // timeout before match
}

/*
 *  WIFI ROUTINES
 *
 */


uint8 WriteWIFICMD(char *param1, char *param2, uint8 IsReset) // return 1 if ok
{
	uint8  retry=3;
	while(WIFIMode!=CMD && retry--) // If not in command mode then enter command mode (try 3 times before failing just in case!)
	{
		// as per wifi232-s data sheet write out +++ wait for an 'a' and then send an 'a' in response
  	PurgeSerial(WIFI_UART); // remove any crud before we start
		SerialWrite(WIFI_UART,'+'); // write the first '+' to test we are not already in CMD mode
		if(SerialDataMatches(WIFI_UART,"+",100)) // wait 100mS for an echo, if we recv the + back we must already be in cmd mode (assuming the host doesnt echo!)
		{
			// already in cmd mode so terminate it (the '+' will give a syntax error but we know that and can safely ignore it)
	    SerialWrite(WIFI_UART,'\r');
			delay(50*millisecond); // while the error comes in
			PurgeSerial(WIFI_UART);
			WIFIMode=CMD;
		}
		else // not in cmd mode yet so write the other two +  - to make 3 in total (+++)
		{
		  SerialWriteStr(WIFI_UART,"++");
  		if(SerialDataMatches(WIFI_UART,"a",100)) // wait for the 'a' response
  		{
  	    SerialWrite(WIFI_UART,'a'); // write the 'a'
    		if(SerialDataMatches(WIFI_UART,"+ok",100)) // wait for the +ok message to confirm
    		  WIFIMode=CMD;
   		}
		}
	}
	if(WIFIMode==CMD) // assuming the above worked and we are now in CMD mode then execute the actual cmd
	{
		PurgeSerial(WIFI_UART); // just to be safe eliminate any additional chars that may have been left over from above (50mS delay)
		SerialWriteStr(WIFI_UART,param1); // write out first parameter
	  if(param2!="") SerialWriteStr(WIFI_UART,param2);  // if second parameter then write out second parameter
		SerialWriteStr(WIFI_UART,"\r"); // terminate cmd with CR
		if(IsReset) // doesnt echo but resets terminal and should go back to data mode on reset
		{
			WIFIMode=DATA;
			return 1;
		}
	  return SerialDataMatches(WIFI_UART,"+ok",1000); // remember it will also echo the cmd before
	}
	return 0; // if we diddnt get in cmd mode then fail by returning 0
}

uint8 WriteWIFIDATA(char *param1, char *param2) // return 1 
{
  if(WIFIMode!=DATA) // if we are in cmd mode then get back into transparant mode
		if(WriteWIFICMD("AT+ENTM","",0))
			WIFIMode=DATA;

	if(WIFIMode==DATA)
	{
	  SerialWriteStr(WIFI_UART,param1); // write first parameter
	  if(param2!="") SerialWriteStr(WIFI_UART,param2); // if there is a second parameter write this immediately following first (no space like on a cmd)
	  return 1; // return 1 to say has been sent (not sure if received tho!)
	}
	return 0; // error
}

uint8 InitWiFi() // Bring up wifi module and try and connect, return true if connected to host
{
	uint16 WifiConnectTimeout=200;
  INTEnable(INT_SOURCE_UART_RX(WIFI_UART),  INT_DISABLED);
  WIFI_RESET(0);
	WIFI_PWR(1);
	delay(5*millisecond);
  WIFI_RESET(1);
  PurgeSerial(WIFI_UART);
	WIFIMode=UNKNOWN;
  while(!WIFI_LINK() && WifiConnectTimeout) delay(1*millisecond); // wait for Wifi to connect
  INTEnable(INT_SOURCE_UART_RX(WIFI_UART),  INT_ENABLED);
	return WIFI_LINK(); // return if connected or not
}


/*
 *  MISC SYSTEM HARDWARE ROUTINES
 *
 */

void beep(uint16 milliseconds) // set buzzer active for this many mS
{
  if(BuzzerTimeout<100) BuzzerTimeout=milliseconds*2; // because interrupt is 2000 times a second and decs buzzer timeout each pass
}

void FatalError(char * reason) // print error reason and halt program
{
  CLS();
  LCDWriteStrAt(0,0,"FATAL ERROR!");
  LCDGotoXY(0,2);
  LCDWriteStr(reason);
  while(1); // die
}

/*
 *  BARCODE ROUTINES
 *
 */

uint8 ReadBarcode() // Read in barcode string from above scan (up to max input size)
{
  uint8  BarcodeLen=0;
  uint32 timeleft = 5000 * millisecond; // set our timout for this operation

	prg.BarcodeStr[0]=0x00; // set length to 0

  BARCODE_PWR(1);
	delay(50*millisecond);
  PurgeSerial(BARCODE_UART);
  INTEnable(INT_SOURCE_UART_RX(BARCODE_UART),  INT_ENABLED);
  BARCODE_TRIG(0); // initiate scan on MT700 (should wake up and led go on to read) as its a seperate CPU

  while(timeleft && !SerialAvailable(BARCODE_UART)); // while we have not timed out and not decoded the barcode hang around here
	while(BarcodeLen<STRMAX && timeleft)
  {
		beep(80);
    if(SerialAvailable(BARCODE_UART))
    {
      prg.BarcodeStr[BarcodeLen] = SerialRead(BARCODE_UART); // build up barcode string as received
      if(prg.BarcodeStr[BarcodeLen]=='\n') break;
      BarcodeLen++;
    }
  }
	INTEnable(INT_SOURCE_UART_RX(BARCODE_UART),  INT_DISABLED);
  BARCODE_TRIG(1); // end - put MT700 back in to idle mode (HOW?)
	BARCODE_PWR(0);
  prg.BarcodeStr[BarcodeLen]='\0'; // set end
  prg.BarcodeStr[STRLEN]=BarcodeLen; // set size
  return BarcodeLen;
}


/*
 *  KEYBOARD ROUTINES
 *
 */

void Sleep16x(uint8 x16)
{

}

void PurgeKeyBuf() // reset buffer (called automatically when all keys read can also call manually to purge key buffer if required)
{
	asm("di");
  KeyBufInPtr=0;
	KeyBufOutPtr=0;
	asm("ei");
}


void InitKeypad()
{
	uint8 i=17;
  K0(1); K1(1); K2(1); K3(1); K4(1); K5(1); K6(1); K7(1); // all scan lines high
	PurgeKeyBuf();
	while(i--)
		KeyState[i]=0;
}

uint8 WaitKey() // same as keypressed but powers down CPU until press occurs
{
	WaitInterrupt(0);
	return (KeyBufInPtr>KeyBufOutPtr) && (KeyBufInPtr>0);
}

uint8 KeyPressed() // return true if a key is waiting in buffer
{
	return (KeyBufInPtr>KeyBufOutPtr) && (KeyBufInPtr>0);
}

uint8 ReadKey() // return key scan code from keybuffer and inc ptr to next key if there is one
{
	uint8 k;
	if(!KeyPressed())	return 0; // should have checked!
	k = KeyBuf[KeyBufOutPtr++];
	if(KeyBufInPtr==KeyBufOutPtr) PurgeKeyBuf();
	return k;
}


/*
 *  INIT EVERYTHING (STARTUP / REBOOT)
 *
 */

void ResetDefaults()
{
  // Set up the serial receive buffer indexes
	PurgeSerial(WIFI_UART);
	PurgeSerial(BARCODE_UART);
	PurgeKeyBuf();
	triggered=0;
  Brightness=prg.DefBrightness;
	UserActivityTimeout=0;
	prg.InputTimeoutMs=30*2000;			// 30 seconds
	prg.DimTimeoutMs=10*2000;				// 10 seconds
	prg.OffTimeoutMs=20*60*2000;		// 20 minutes of no activity shuts off system
	prg.BarcodeTimeout=5*2000;			// 5 seconds
	prg.RFIDTimeout=5*2000;					// 5 seconds
	prg.BarcodePwrTimeout=30*2000;	// 30 seconds
	prg.WIFIPwrTimeout=30*2000;			// 60 seconds
}

void InitTerminal(void) // Initialise PIC32 and terminal io pins
{
  uint8 i;
  // init CPU
  DDPCONbits.JTAGEN = 0; // Turn off JTAG allows RA0, RA1, RA4, and RA5 to be used
  SYSTEMConfig(SYS_FREQ, SYS_CFG_WAIT_STATES | SYS_CFG_PCACHE);
  CVREFClose();//disables the CVREF module.

  // Init IO Pins using macros - why? - keep pin defs together in header for easier changing rather than going to top of program and then back here all the time!
	CFG_PORT_A_IN
	CFG_PORT_A_OUT
	CFG_PORT_B_IN
	CFG_PORT_B_OUT
	CFG_PORT_C_IN
	CFG_PORT_C_OUT

	// turn off PIC32 modules not used to save power (a few miliamps at most but it all helps)
  AD1CHS = 0; // ADC Off
	PMD1bits.AD1MD = 1;  // ADC Pwr off
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
	PMD6bits.REFOMD = 1;  // ref clk out off

	// set default pin states
  PWR_HOLD(1); // hold power on system after keypress (must do this first thing to keep power on!)
  BUZZER(0);
	WIFI_PWR(0);
	BARCODE_PWR(0);
  BACKLIGHT(0);
  BARCODE_TRIG(1);
  RFID_MOD(0);
	UNUSED1(0);
	UNUSED2(0);

  PPSUnLock; // Unlock PPS to allow UART AND SPI PIN Mapping
  SET_PPS_SDO;
  SET_PPS_WIFI_TX;
  SET_PPS_WIFI_RX;
  SET_PPS_BARCODE_TX;
  SET_PPS_BARCODE_RX;
	PPSLock; // lock PPS

	// set change notification for RBP7 and RBP8

  // SPI 8-bit, Master Mode, clk idle high, Baud =  pOSC/4
  SPI1CON = 0x00;
  i = SPI1BUF;
  SPI1BRG = 0x0004;
  SPI1STATbits.SPIROV = 0;
  SPI1CON = 0x00008260;

	ResetDefaults(); 	// set default timeouts and things that can be altered by user program

  // Configure UART1
  UARTConfigure(BARCODE_UART, UART_ENABLE_PINS_TX_RX_ONLY);
  UARTSetLineControl(BARCODE_UART, UART_DATA_SIZE_8_BITS | UART_PARITY_NONE | UART_STOP_BITS_1);
  UARTSetDataRate(BARCODE_UART, GetPeripheralClock(), BARCODE_BAUD);
  UARTEnable(BARCODE_UART, UART_ENABLE_FLAGS(UART_PERIPHERAL | UART_RX | UART_TX) );
  
  // Configure UART2 RX Interrupt (we dont use TX interrupts just fill the tx buff and wait for them to go)
  INTSetVectorPriority(INT_VECTOR_UART(BARCODE_UART), INT_PRIORITY_LEVEL_2);
  INTEnable(INT_SOURCE_UART_RX(BARCODE_UART), INT_DISABLED); // disabled until I say

  // Configure UART2
  UARTConfigure(WIFI_UART, UART_ENABLE_PINS_TX_RX_ONLY);
  UARTSetLineControl(WIFI_UART, UART_DATA_SIZE_8_BITS | UART_PARITY_NONE | UART_STOP_BITS_1);
  UARTSetDataRate(WIFI_UART, GetPeripheralClock(), WIFI_BAUD);
  UARTEnable(WIFI_UART, UART_ENABLE_FLAGS(UART_PERIPHERAL | UART_RX | UART_TX) );

  // Configure UART1 RX Interrupt
  INTSetVectorPriority(INT_VECTOR_UART(WIFI_UART), INT_PRIORITY_LEVEL_3);
  INTEnable(INT_SOURCE_UART_RX(WIFI_UART),  INT_DISABLED); // disabled until I say

  // Configure timer 1 backlight/buzzer
  OpenTimer1(T1_ON | T1_SOURCE_INT | TIMER1DIVISOR, TIMER1PERIOD); // We want timer interrupt approx 2000 times a second (for buzzer/backlight)
  ConfigIntTimer1(T1_INT_ON | T1_INT_PRIOR_4); // highest priority

	// need to configure the change notification interrupt on RPB7 and 8 so we can get keypresses when idle/sleep
	mCNBOpen(CNB_ON | CNB_IDLE_CON,  CNB7_ENABLE | CNB8_ENABLE, CNB7_PULLUP_ENABLE | CNB8_PULLUP_ENABLE);
	mCNSetIntPriority(CHANGE_INT_PRI_5);
	mCNBIntEnable(FALSE); // Off for now, only enable during waitforkeypress

  // Configure for Multi-Vectored Interrupts //
  INTConfigureSystem(INT_SYSTEM_CONFIG_MULT_VECTOR);

	// Enable Interrupts
  INTEnableInterrupts();

	InitKeypad();
	InitLCD();
  InitWiFi(0);

}

// INTERRUPT HANDLERS ....  (remember any interrupts will wake CPU from idle/sleep!)

void __ISR(_CHANGE_NOTICE_VECTOR, IPL5) CNInterrupt() // does nothing just wakes the CPU and ack the interrupt (timer1 should start again and read kbd
{
	mCNBIntEnable(FALSE); // turn further CN interrupts Off
  mCNBClearIntFlag(); // Clear CN interrupt flag
  asm ("nop"); // Suggested to clear pipeline
}

void __ISR(_UART1_VECTOR, IPL2) IntUart1Handler(void) // UART 1 RX/TX Interrupt Handlers
{
  if(IFS1bits.U1RXIF)  // RX char ready to read from UART into Rxbuffer
  {
    RxBuffer1[RxData1In] = UARTGetDataByte(UART1); // Read data from Rx.
    if(RxData1In<(sizeof(RxBuffer1)-1)) // Make sure buffer is not full and move the RxBuffer Index to indicate a char available if we are at the buffer limit this indicates an error
      RxData1In++;
    IFS1bits.U1RXIF=0; // All done, so Clear Interrupt flag to allow new interrupt
  }
} // end UART interrupt handler

void __ISR(_UART_2_VECTOR, IPL3) IntUart2Handler(void) // UART 2 RX/TX Interrupt Handlers
{
  if(IFS1bits.U2RXIF)  // RX char ready to read from UART into Rxbuffer
  {
    RxBuffer2[RxData2In] = UARTGetDataByte(UART2); // Read data from Rx.
    if(RxData2In<(sizeof(RxBuffer2)-1)) // Make sure buffer is not full and move the RxBuffer Index to indicate a char available if we are at the buffer limit this indicates an error
      RxData2In++;
    IFS1bits.U2RXIF=0; // All done, so Clear Interrupt flag to allow new interrupt
  }
} // end UART interrupt handler


void ProcessKeyStrobe(uint8 StrobeLine) // process key scan lines (function must be very small and fast, no loops/procs, as its in a time critical interrupt!)
{
	if(StrobeLine) // check strobe line if active then register keypress (actual key depends on which of the 8 scan lines (KeyNo val) is set)
	{
		// key pressed
		if(KeyState[KeyNo]<KEYPRESSMAX)	KeyState[KeyNo]++; // chk strobe line while key is pressed increment up to MAX
		if(KeyNo==14 && KeyState[KeyNo]==KEYPRESSMAX) PWR_HOLD(0); // If cancel key held for MAX time then power off unit
		return;
	}
	// if we get here key is not pressed
	if(KeyState[KeyNo]) // key was pressed but is now released - and we do the processing on the release of the key!
	{
		if(KeyState[KeyNo]>KEYTHRESHOLD) // if key press count goes above threshold we consider the key as "really pressed" (ie no debounce)
		{
			KeyState[KeyNo]=0;  // reset key counter on process
			UserActivityTimeout=0; // reset this to show user is still alive and pressing keys (we dont want to pwr off if they are using it!)
			Brightness=prg.DefBrightness; // light up backlight if pressed a key!
			if(KeyBufInPtr<KEYBUFLEN)
			{
				// keep adding keypresses to the key buffer until full
				KeyBuf[KeyBufInPtr++]=ScanCode[KeyNo];
				BuzzerTimeout=20; // key click (if thats possible!)
			}
			if(KeyNo==14 && KeyState[KeyNo]==KEYPRESSMAX) PWR_HOLD(0);  // If cancel key held for the KEYPRESSMAX time (at least 2 seconds) then power off unit
		}
		else // very important this else as we have already set keystate[keyno] to zero above so one more dec would set to 255!
  		KeyState[KeyNo]--; // if key is released before it gets to threshold then dec the counter back to 0 (eliminate noise/bounce)
	}
}

void __ISR( _TIMER_1_VECTOR, IPL4) _T1Interrupt( void) // interrput code for the timer 1 (2000 x a second - clock is 3.5mhz so dont hog CPU!)
{
	static uint8 prescaler; // need this to slow 2000 times a second to (2000/10) 200 times/sec (persistant/local var)

  if(BuzzerTimeout>0) BuzzerTimeout--;
  BUZZER(BuzzerTimeout && BuzzerTimeout%2); // sound @ 2Khz while active
  if(++BacklightTimeout>10) BacklightTimeout=0; // Set PWM on TFT Backlight from 0% to 100% duty in 10% steps depending upon brightness
  BACKLIGHT(BacklightTimeout<Brightness);

	if(prescaler++>8) // divide the 2000 times a second by 8 to a more manageble 250 a second (approx 1 every 5mS) for keypad processing
	{
		prescaler=0;

		// process keypad
		// Check for keypresses we have 17 keys each keypress increases the counter for that key in an array KeyState[] a release decreases it.
		// once we get past the threshold we call the key pressed you can check how long held (if count reaches threshold then about 1.5 seconds!)
		// once key considered pressed we add it to the end of the keybuf (up to 8 key presses max)
		// note KeyNo goess from 0 to 16 so we effectively sample keypress at (250/17 =) 15 times a second
		// use KeyNo as a ticker, processing two keys each tick/pass this way we dont do too much (certainly dont use any loops during the interrupt!)

		// Set scan lines low in sequence (active low as the strobe lines are pulled high)
		K0(KeyNo!=0); // only goes low when keyno == 0
		K1(KeyNo!=2); // only goes low when keyno == 2
		K2(KeyNo!=4); // only goes low when keyno == 4
		K3(KeyNo!=6); // only goes low when keyno == 6
		K4(KeyNo!=8); // only goes low when keyno == 8
		K5(KeyNo!=10); // only goes low when keyno == 10
		K6(KeyNo!=12); // only goes low when keyno == 12
		K7(KeyNo!=14); // only goes low when keyno == 14

		ProcessKeyStrobe(KEYSTROBE0); // processing even keys on keypad - dont worry function is very small and fast (as we're in an interrupt!)
		KeyNo++;
		ProcessKeyStrobe(KEYSTROBE1); // processing odd keys on keypad
		if(++KeyNo>16) KeyNo=0;

		// tick activity counters at
		UserActivityTimeout++;

		if(UserActivityTimeout>prg.DimTimeoutMs) Brightness=0; // no activity then power off backlight
		if(UserActivityTimeout>prg.OffTimeoutMs) PWR_HOLD(0); // no activity for a long time then power off terminal fully to save battery

		if(U2STAbits.OERR || U2STAbits.FERR || U2STAbits.PERR) // damn errors
		{
			U2STAbits.OERR = 0;
			U2STAbits.FERR = 0;
			U2STAbits.PERR = 0;
			BuzzerTimeout=2000; // long annoying beep (maybe the user will mention it then!)
		}
//		if(WifiActivityTimeout>prg.WIFIPwrTimeout) PWR_WIFI(0); // no wifi activity so power it down V3 on
//		if(BarcodeActivityTimeout>prg.BarcodePwrTimeout) PWR_BARCODE(0); // no barcode activity so power it down V3 on

	}

  IFS0bits.T1IF = 0; // mark interrupt processed and allow next interrupt
} // end timer interrupt handler

#endif