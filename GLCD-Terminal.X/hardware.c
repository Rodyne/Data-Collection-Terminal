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

#define _SUPPRESS_PLIB_WARNING // Get rid of annoyances
#include <plib.h>    // microchip pic32 peripheral libraries

// PIC32 Config/Fuse settings

// Using 8Mhz XTAL
#pragma config POSCMOD = XT     // Primary Oscillator Configuration (XT = 3-10Mhz XTAL, HS >10Mhz XTAL)
#pragma config FNOSC = PRIPLL   // Oscillator Selection = Primary Osc (tweek for lowest that supports 115200 baud 16Mhz about OK
#pragma config FPLLIDIV = DIV_2 // must be div 2 to get in pll range for input
#pragma config FPLLMUL = MUL_20
#pragma config FPLLODIV = DIV_4 // 20mhz!

#pragma config IESO = OFF      // Internal/External Switch Over Disabled
#pragma config FPBDIV = DIV_1  // Peripheral Clock Divisor (Pb_Clk is Sys_Clk/1)
#pragma config OSCIOFNC = OFF  // CLKO Output Signal Active on the OSCO Pin Disabled
#pragma config FCKSM = CSDCMD  // Clock Switching and Monitor Selection Disabled
#pragma config FSOSCEN = OFF   // Secondary Oscillator Disabled not required and osc2 pins are needed
#pragma config FWDTEN  = OFF   // Watchdog Timer off not required

#define SYS_FREQ              20000000ul       // 8Mhz NOT 80Mhz! plenty of horsepower for this application and reduces pwr req
#define GetSystemClock()      (SYS_FREQ)       // Used instead of SYS_FREQ in some modules.
#define GetInstructionClock() GetSystemClock() // GetSystemClock()/1 for PIC32.  Might need changing if using Doze modes.
#define GetPeripheralClock()  GetSystemClock() // GetSystemClock()/1 for PIC32.  Divisor may be different if using a PIC32 sinc
#define TIMER1PERIOD					8								 // Need to set this when changing clock to get approx 2000 interupts per second
#define uSDELAYMULTIPLIER     10							 // Need to set this to ensure microsecond delay routine is correct


// Macro Pin definitions for ALL PIC32 pins. The pin defs and pin macros MUST Match

// Hardware note for PIC32MX170 bit 2 and 3 of port A unavailable as used for XTAL OSC! 5 and 6 dont exist physically!
#define CFG_PORT_A_OUT	PORTSetPinsDigitalOut(IOPORT_A, BIT_4 | BIT_5 | BIT_6 | BIT_7 | BIT_9 | BIT_10 );
#define CFG_PORT_A_IN   PORTSetPinsDigitalIn(IOPORT_A, BIT_0 | BIT_1 | BIT_8 );
#define CFG_PORT_B_OUT	PORTSetPinsDigitalOut(IOPORT_B, BIT_0 | BIT_1 | BIT_2 | BIT_4 |BIT_5 | BIT_9 | BIT_10 | BIT_11 | BIT_12 | BIT_13 | BIT_14 | BIT_15 );
#define CFG_PORT_B_IN   PORTSetPinsDigitalIn(IOPORT_B, BIT_3 | BIT_6 | BIT_7 | BIT_8 );
#define CFG_PORT_C_OUT	PORTSetPinsDigitalOut(IOPORT_C, BIT_0 | BIT_1 | BIT_2 | BIT_5 | BIT_6 |BIT_7 | BIT_8 | BIT_9  );
#define CFG_PORT_C_IN   PORTSetPinsDigitalIn(IOPORT_C, BIT_3 | BIT_4 );

// PORT A
#define RFID_CLK()            PORTAbits.RA0           // IN  (O/C - REQUIRES MCU INTERNAL PU ACTIVE!)
#define RFID_DATA()           PORTAbits.RA1           // IN  (O/C - REQUIRES MCU INTERNAL PU ACTIVE!)
#define BARCODE_RESET(state)  LATAbits.LATA4 = state  // OUT (Barcode reset / active low)
#define LCD_CS(state)         LATAbits.LATA7 = state  // OUT (Active low to enable LCD)
#define SET_PPS_WIFI_RX       PPSInput(2,U2RX,RPA8)   // IN  (UART 2 SERIAL RECEIVE)
#define BARCODE_TRIG(state)		LATAbits.LATA9 = state  // OUT (Active low, starts a barcode scan)
#define RFID_MOD(state) 			LATAbits.LATA10 = state // OUT (1=RFID POWERDN /  0 = ON, uses open drain to pull low and stop osc in pwr save)
// PORT B
#define BUZZER(state)					LATBbits.LATB0 = state  // OUT modulate to sound buzzer
#define BACKLIGHT(state)      LATBbits.LATB1 = state  // OUT PWM for backlight LED control brightness
#define WIFI_RELOAD(state)    LATBbits.LATB2 = state  // OUT (Active low should not be required by program unless error in devlopment!)
#define WIFI_LINK()           !PORTBbits.RB3          // IN  (active low)
#define UNUSED_PIN(state)		  LATBbits.LATB4 = state  // OUT (unused)
#define PWR_HOLD(state)				LATBbits.LATB5 = state  // OUT (set high to keep CPU powered on, drop to 'true' power off system fully)
#define KEY_SCAN              !PORTBbits.RB6          // IN
#define KEYSTROBE0            !PORTBbits.RB7          // IN
#define KEYSTROBE1            !PORTBbits.RB8          // IN
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
#define BARCODE_DECODE()      PORTCbits.RC4           // IN  (Requires weak pull up)
#define SET_PPS_BARCODE_TX    PPSOutput(1,RPC5,U1TX)  // OUT
#define K1(state)             LATCbits.LATC6  = state // OUT
#define K2(state)             LATCbits.LATC7  = state // OUT
#define K3(state)             LATCbits.LATC8  = state // OUT
#define K4(state)             LATCbits.LATC9  = state // OUT

#define BARCODE_UART          UART1
#define BARCODE_BAUD          9600
#define WIFI_UART             UART2
#define WIFI_BAUD             115200

// defs only app to this specific hardware

#define KEYTHRESHOLD          10      // each few mS each key is checked, if pressed it increments its counter, if not it zeroises it, if counter>threshold we say is pressed (sort of a debounce)
#define LCD_PG                0xb0  // LCD Pg you can add offset to it
#define NORMAL_CONTRAST       0x14
#define SER1BUFLEN            32    // size of serial buffer COM1 (barcode)
#define SER2BUFLEN            1024  // size of serial buffer COM2 (wifi)
#define KEYBUFLEN							8     // size of keypad buffer
#define LCD_ON								0xAF
#define LCD_OFF								0xAE
#define LCD_NORMAL						0xA6
#define LCD_REVERSE						0xA7
#define WIFITIMEOUTMS					10000 // 5*2000 = 5 Seconds

volatile uint32  counter1;
volatile uint8   BacklightTimeout, Brightness, triggered;
volatile uint16  BuzzerTimeout, UserActivityTimeout, WifiActivityTimeout, BarcodeActivityTimeout; // counts up 10 times a second resets if key pressed
volatile uint8   KeyBuf[KEYBUFLEN]; // high level buffer, holds processed keys read by readkey()
volatile uint16  RxData1In, RxData1Out, RxData2In, RxData2Out, KeyBufInPtr, KeyBufOutPtr; // indexes for char read in from uART and char read out by user
volatile uint8   RxBuffer1[SER1BUFLEN], RxBuffer2[SER2BUFLEN]; // interrupt receive buffers wi-fi data buffer should be bigger than barcode as it will be used more
uint8            WIFICMDMode;
tform            CurScr,SaveScr;

void DelayMicroseconds(uint32 uS) // Blocking wait of 1uS (Interrupts will still work ok)
{
  uint32 ExitCount = ReadCoreTimer() + (10*uS); // set future time

	if(ExitCount<ReadCoreTimer()) // Catch timer overflow condition, means we cannot use the timer for this so approximate the old fashioned way less accurate
	{
		for(ExitCount=0; ExitCount<uSDELAYMULTIPLIER*uS; ExitCount++)
			asm("NOP");
	}
	else
    while(ReadCoreTimer()<ExitCount); // wait for time to come around (warning overflow will cause a much shorter time if your very unlucky so *dont* use for critcal timing!)
}

void DelayMs(uint32 mS)
{
  DelayMicroseconds(mS*1000);
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

void LCDInit() // as reset pin is common with wifi and barcode then we assume physical reset has been issued before calling this
{
	int i;
	char greyscale[]={0x00,0x00,0xdd,0xdd,0xaa,0xaa,0xff,0xff};

  LCD_CS(0);

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
	DelayMs(10);
  LCD_RESET(1);
	DelayMs(20);

  // copied verbotem from example on east riding site, have tried to understand but only makes sense if you know how the ST7541 is wired up internally to their LCD I guess!

	LCDWrite(CMD,0x38); // Mode set to 77 hz booster efficiency level 2 (this is the default why issue it?)
	LCDWrite(CMD,0x04); //
  LCDWrite(CMD,0xAB); // Intern OSC ON
	LCDWrite(CMD,0x57); // Set LCD Bias last 3 bits = 1/12 again the default - wtf!)
	LCDWrite(CMD,0x38); // Mode set to 73 hz booster efficiency level 1 why we changing it again?
  LCDWrite(CMD,0x80); //
	LCDWrite(CMD,0x27); // Regulator internal resistance ratio of internal voltage regulator (111) = ratio of 7.2 ?
	LCDWrite(CMD,0x64); // DC DC Step up Vout = 3 x boost
	DelayMs(1);
	LCDWrite(CMD,0x2c); // PowerControll VC= ON VR=OFF VF=OFF
	DelayMs(1);
	LCDWrite(CMD,0x67); // boost level 6
	LCDWrite(CMD,0x2e); // PowerControll VC= ON VR=ON VF=OFF
	DelayMs(1);
	LCDWrite(CMD,0x2f); // PowerControll VC= ON VR=OFF VF=ON
	DelayMs(1);
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

void DoLogo()
{
  uint8 i;
	CLS();
	LCDSetContrast(0);
	Brightness=10;
	Pen(WHITE);
	LCDWriteStrAt(3,2,"          ");
	LCDWriteStrAt(3,3,"  ROVING  ");
	LCDWriteStrAt(3,5," DYNAMICS ");
	Pen(BLACK);
	LCDWriteStrAt(3,9,"rodyne.com");
	LCDWriteStrAt(2,12,"OPEN  SOURCE");
	LCDWriteStrAt(2,14,"  HARDWARE");
	for(i=0; i<28; i++)
	{
		LCDSetContrast(i);
		DelayMs(40);
	}
	for(i=28; i>NORMAL_CONTRAST; i--)
	{
		LCDSetContrast(i);
		DelayMs(80);
	}
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

void PurgeSerial(UART_MODULE id)
{
  asm("di");
  if(id==UART1)
  {
    RxData1Out=0;
    RxData1In=0;
  }
  if(id==UART2)
  {
    RxData2Out=0;
    RxData2In=0;
  }
  asm("ei");
}

uint8 SerialRead(UART_MODULE id) // Reads buffer. No checks here, you must use SerialDataAvailable before calling this...
{
  uint8 c;

  if(id==UART1)
  {
    if(RxData1Out<RxData1In) // check we actually have data first
    {
      c = RxBuffer1[RxData1Out++];
      if(RxData1Out==RxData1In) PurgeSerial(UART1); // once up to date reset the buffer indexes back to the begining
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
      if(RxData2Out==RxData2In)  PurgeSerial(UART2);// once up to date reset the buffer indexes back to the begining
      return c;
    }
    else
      return 0; // no data WTF, should have used serialDataAvaiable first!
  }

}

uint8 SerialReadStr(UART_MODULE id,uint16 TimeoutMs) // returns number of char in string, resulr (string) stored in StrBuf
{
  uint16 i=TimeoutMs;
  char ch=0;
  uint8 CharCount=0;

  while(i<TimeoutMs && ch!='\n')
  {
    if(SerialAvailable(id))
    {
      ch=SerialRead(id);
      if(ch!='\n') prg.SerialStrBuf[CharCount]=ch;
			if(CharCount<STRBUFMAX-1) CharCount++; else prg.ErrorCode=ERR1;
    }
  }
  prg.SerialStrBuf[CharCount]= 0x00;
  return CharCount;
}

uint8 LocateStrInSerialData(uint8 UARTID, char *match, uint16 TimeoutMs) // will timeout if no match
{
  uint8 i=0;
  while(--TimeoutMs) // keep in loop until timeout counter gets to zero
  {
    DelayMs(1);
    if(SerialAvailable(UARTID))
    {
      if(SerialRead(UARTID)!=match[i++]) i=0; // keep incrementing i while incoming serial char matches next posn in string, reset if char match fails
    }
    if(match[i]==0 && i>0) return TRUE;  // goto to end of string so must have matched
  }
  return FALSE;
}

/*
 *  WIFI ROUTINES
 *
 */


uint8 WriteWIFI(uint8 mode, char *param1, char *param2) // return 0 if ok
{
  uint16 timeout=900; // 900mS Max for getting to command mode
	if(mode==CMD)
	{
		if(WIFICMDMode!=1) // If not in command mode then enter command mode (assume it works, I guess user can reset if not)
		{
			// as per data sheet
			DelayMs(20);
			SerialWriteStr(WIFI_UART,"+++");
			while(timeout-- && !SerialAvailable(WIFI_UART) )
				DelayMicroseconds(1000); // wait here for response/timeout
			if( SerialAvailable(WIFI_UART))
			{
				if(SerialRead(WIFI_UART)=='a')
				{
					SerialWrite(WIFI_UART,'a');
					WIFICMDMode=1;
				}
			}
		}
		if(WIFICMDMode) // ok we are now in command mode
		{
			SerialWriteStr(WIFI_UART,param1); // write out first parameter
			if(param2!="") // if second parameter then add a space and write out second parameter
			{
				SerialWriteStr(WIFI_UART," ");
				SerialWriteStr(WIFI_UART,param2);
			}
			SerialWriteStr(WIFI_UART,"\r");
			if(param1=="AT+Z") WIFICMDMode=0;
			if(timeout) return 0; else return 1;
		}
		else
			return 0;
	}
	if(mode==DATA) // sending data to host service
	{
		if(WIFICMDMode) // if we are in cmd mode then get back into transparant mode
		{
			SerialWriteStr(WIFI_UART,"AT+ENTM");
			DelayMs(20);
		}
		SerialWriteStr(WIFI_UART,param1); // write first parameter
		if(param2!="") SerialWriteStr(WIFI_UART,param2); // if there is a second parameter write this immediately following first (no space like on a cmd)
		return 1; // return 1 to say has been sent (not sure if received tho!)
	}
}

uint8 WiFiInit(uint8 wait) // 100mS
{
	uint16 wtimeout;
	WIFICMDMode=DATA; // on reset the serial modules default to throughput mode (DATA)
  WIFI_RESET(0);
  PurgeSerial(WIFI_UART);
  DelayMs(20);
  WIFI_RESET(1);
	if(wait) wtimeout=counter1+200; else wtimeout=counter1+WIFITIMEOUTMS;
  while(!WIFI_LINK() && counter1<wtimeout); // wait for Wifi to connect
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

void Sleep() // power down system one module at a time
{
 // remember that 7mA+ is drawn if PICKit connected!
//	WriteWIFICMD("sleep","",3000); // power down WIFI
//	LCDSleep(); // TFT Enter Sleep mode
  asm("di");
  BACKLIGHT(0);
  IEC0=0;    // Enable External Interrupt 1 for further wake up
  IEC1=0;      //Disable the CN interrupts so PIC32 won't freak out
  PowerSaveSleep(); // have you disabled everything that can wake from sleep?
}

/*
 *  BARCODE ROUTINES
 *
 */

void InitBarcodeScan() // call this first to wake up and initiate scan
{
	BarcodeActivityTimeout=0;
  DelayMs(20);
  BARCODE_RESET(0);
  DelayMs(10);
  BARCODE_RESET(1);
  DelayMs(100);
  PurgeSerial(BARCODE_UART);
  SerialWriteStr(BARCODE_UART,"{MC11WT0,1}\0");
  BARCODE_TRIG(0); // initiate scan on MT700 (should wake up and led go on to read)
	triggered=1;
}

uint8 ReadBarcode() // Read in barcode string from above scan (up to max input size)
{
  uint8  BarcodeLen=0;
  uint32 endtime = counter1 + 20000; // set our timout in units of 250uS x 20000 = 5 seconds!
  prg.BarcodeStr[0]=0x00; // set length to 0
	if(!triggered) InitBarcodeScan();
  while(counter1<endtime && !SerialAvailable(BARCODE_UART)); // while we have not timed out and not decoded the barcode hang around here
	while(BarcodeLen<PSTRMAX && counter1<endtime)
  {
		beep(80);
    if(SerialAvailable(BARCODE_UART))
    {
      prg.BarcodeStr[BarcodeLen] = SerialRead(BARCODE_UART); // build up barcode string as received
      if(prg.BarcodeStr[BarcodeLen]=='\n') break;
      BarcodeLen++;
    }
  }
  BARCODE_TRIG(1); // end - put MT700 back in to idle mode (HOW?)
	triggered=0;
  prg.BarcodeStr[BarcodeLen]='\0'; // set end
  prg.BarcodeStr[STRLEN]=BarcodeLen; // set size
  return BarcodeLen;
}


/*
 *  KEYBOARD ROUTINES
 *
 */

void PurgeKeyBuf() // reset buffer (called automatically when all keys read can also call manually to purge key buffer if required)
{
  KeyBufInPtr=0;
	KeyBufOutPtr=0;
}

uint8 KeyPressed() // return true if a key is waiting in buffer
{
	return (KeyBufInPtr>KeyBufOutPtr) && (KeyBufInPtr>0);
}

uint8 ReadKey() // return key scan code from keybuffer and inc ptr to next key if there is one
{
	uint8 k;
  if(!KeyPressed())
		return 0; // should have checked!
	else
	{
	  k = KeyBuf[KeyBufOutPtr++];
		if(KeyBufInPtr==KeyBufOutPtr) PurgeKeyBuf();
		return k;
	}
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
	counter1=0;
	triggered=0;
  Brightness=prg.DefBrightness;
	UserActivityTimeout=0;
	WifiActivityTimeout=0;					// determine when to put wifi module to sleep (or power off) to save power
	BarcodeActivityTimeout=0;				// determine when to put barcode module to sleep (or power off)
	prg.InputTimeoutMs=30*2000;			// 30 seconds
	prg.DimTimeoutMs=10*2000;				// 10 seconds
	prg.OffTimeoutMs=60*60*2000;		// 60 minutes of no activity shuts off system
	prg.BarcodeTimeout=5*2000;			// 5 seconds
	prg.RFIDTimeout=5*2000;					// 5 seconds
	prg.BarcodePwrTimeout=30*2000;	// 30 seconds
	prg.WIFIPwrTimeout=30*2000;			// 30 seconds
}

void InitTerminal(void) // Initialise PIC32 and terminal io pins
{
  uint8 i;
  // init CPU
  DDPCONbits.JTAGEN = 0; // Turn off JTAG allows RA0, RA1, RA4, and RA5 to be used
  SYSTEMConfig(SYS_FREQ, SYS_CFG_WAIT_STATES | SYS_CFG_PCACHE);
  CVREFClose();//disables the CVREF module.

  AD1CHS = 0;

  // Init IO Pins using macros - why? - keep pin defs together in header for easier changing rather than going to top of program and then back here all the time!
	CFG_PORT_A_IN
	CFG_PORT_A_OUT
	CFG_PORT_B_IN
	CFG_PORT_B_OUT
	CFG_PORT_C_IN
	CFG_PORT_C_OUT

  // Set RFID_MOD pin (RA10) to open drain
  ODCA = 0x40;

  // set default states
  PWR_HOLD(1); // hold power on system after keypress (must do this first thing to keep power on!)
  BUZZER(0);
  BACKLIGHT(0);
  LCD_CS(1);
  BARCODE_TRIG(1);
  RFID_MOD(0);
  K0(1); K1(1); K2(1); K3(1); K4(1); K5(1); K6(1); K7(1);

  PPSUnLock; // Unlock PPS to allow UART AND SPI PIN Mapping
  SET_PPS_SDO;
  SET_PPS_WIFI_TX;
  SET_PPS_WIFI_RX;
  SET_PPS_BARCODE_TX;
  SET_PPS_BARCODE_RX;
  PPSLock; // lock PPS

  // SPI 8-bit, Master Mode, clk idle high, Baud =  8/4 = 2MHz
  SPI1CON = 0x00;
  i = SPI1BUF;
  SPI1BRG = 0x0004;
  SPI1STATbits.SPIROV = 0;
  SPI1CON = 0x00008260; // was 8260

	ResetDefaults();

	// set default timeouts

  // Configure UART1
  UARTConfigure(BARCODE_UART, UART_ENABLE_PINS_TX_RX_ONLY);
  UARTSetLineControl(BARCODE_UART, UART_DATA_SIZE_8_BITS | UART_PARITY_NONE | UART_STOP_BITS_1);
  UARTSetDataRate(BARCODE_UART, GetPeripheralClock(), BARCODE_BAUD);
  UARTEnable(BARCODE_UART, UART_ENABLE_FLAGS(UART_PERIPHERAL | UART_RX | UART_TX) );
  
  // Configure UART2 RX Interrupt (we dont use TX interrupts just fill the tx buff and wait for them to go)
  INTSetVectorPriority(INT_VECTOR_UART(BARCODE_UART), INT_PRIORITY_LEVEL_2);
  INTEnable(INT_SOURCE_UART_RX(BARCODE_UART), INT_ENABLED);

  // Configure UART2
  UARTConfigure(WIFI_UART, UART_ENABLE_PINS_TX_RX_ONLY);
  UARTSetLineControl(WIFI_UART, UART_DATA_SIZE_8_BITS | UART_PARITY_NONE | UART_STOP_BITS_1);
  UARTSetDataRate(WIFI_UART, GetPeripheralClock(), WIFI_BAUD);
  UARTEnable(WIFI_UART, UART_ENABLE_FLAGS(UART_PERIPHERAL | UART_RX | UART_TX) );

  // Configure UART1 RX Interrupt
  INTSetVectorPriority(INT_VECTOR_UART(WIFI_UART), INT_PRIORITY_LEVEL_3);
  INTEnable(INT_SOURCE_UART_RX(WIFI_UART),  INT_ENABLED);

  // Configure timer 1 backlight/buzzer
  OpenTimer1(T1_ON | T1_SOURCE_INT | T1_PS_1_256, TIMER1PERIOD); // We want timer interrupt approx 2000 times a second (for buzzer/backlight)
  ConfigIntTimer1(T1_INT_ON | T1_INT_PRIOR_4); // highest priority

  // Configure for Multi-Vectored Interrupts //
  INTConfigureSystem(INT_SYSTEM_CONFIG_MULT_VECTOR);

	// Enable Interrupts
  INTEnableInterrupts();

	LCDInit();
  WiFiInit(0);

}

// INTERRUPT HANDLERS ....

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

void __ISR( _TIMER_1_VECTOR, IPL4) _T1Interrupt( void) // interrput code for the timer 1 (2000 x a second)
{
	// declare all local variables static so they are persistant rather than temporary
  static uint8 i; // general purpose counter
	static uint8 KeyNo=0; // key scan counter
  static char  KeyState[17]; // low level key buffer hold no of pulses counted while key pressed and count reset on release (anti-bounce etc)

  counter1++;

  if(BuzzerTimeout>0) BuzzerTimeout--;
  BUZZER(BuzzerTimeout && BuzzerTimeout%2); // sound @ 2Khz while active
  if(++BacklightTimeout>10) BacklightTimeout=0; // Set PWM on TFT Backlight from 10% to 100% duty depending upon brightness
  BACKLIGHT(BacklightTimeout<Brightness);
  
  // Check for keypresses we have 17 keys each keypress registered increases the counter for that key in an array KeyState[]
  // once we get past the threshold we call the key pressed, any release during that time will reset the count
  // once key pressed we add it to the end of the keybuf (up to 8 key presses max)
  // note KeyNo overflows 255->0 so we only sample keypress every 30mS or so

	// use KeyNo as a stepper processing one key each change so we dont do too much during the interrupt
	if(KeyNo<16 && KeyNo%2==0)
	{
		// Set scan lines low in sequence
		K0(KeyNo!=0);
		K1(KeyNo!=2);
		K2(KeyNo!=4);
		K3(KeyNo!=6);
		K4(KeyNo!=8);
		K5(KeyNo!=10);
		K6(KeyNo!=12);
		K7(KeyNo!=14);
    if(KEYSTROBE0) KeyState[KeyNo]++; else KeyState[KeyNo]=0; // strob0 press
    if(KEYSTROBE1) KeyState[KeyNo+1]++; else KeyState[KeyNo+1]=0; // strob1 press
	}
	if(KeyNo==16) // check scan key seperately
  {
  	if(KEY_SCAN) KeyState[16]++; else KeyState[16]=0;
	}
	if(KeyNo>=20 && KeyNo<37) // process keystate information into actual keypresses
  {
		if(KeyState[KeyNo-20]>KEYTHRESHOLD) // if key press count goes above threshold we consider the key as "really pressed" (ie no debounce)
		{
			BuzzerTimeout=20; // key click (if thats possible!)
			KeyBuf[KeyBufInPtr]=ScanCode[KeyNo-20];
			if(KeyBufInPtr<KEYBUFLEN) KeyBufInPtr++; else BuzzerTimeout=200; // keep adding keypresses to the key buffer if we overflow discard key n beep looong error
			KeyState[KeyNo-20]=0;  // reset key counter
			UserActivityTimeout=0; // reset this if pressed a key
  		Brightness=prg.DefBrightness; // light up if pressed a key!
		}
  }
	KeyNo++;

  UserActivityTimeout++;
  WifiActivityTimeout++;
  BarcodeActivityTimeout++;

  // To Do any other shit!
  if(UserActivityTimeout>prg.DimTimeoutMs) Brightness=0; // no activity then power off backlight
  if(UserActivityTimeout>prg.OffTimeoutMs) PWR_HOLD(0); // no activity for a long time then power off terminal fully to save battery
//	if(WIFIActivityTimeout>prg.WIFIPwrTimeout) PWR_WIFI(0); // no wifi activity so power it down
//	if(BarcodeActivityTimeout>prg.BarcodePwrTimeout) PWR_BARCODE(0); // no barcode activity so power it down

  IFS0bits.T1IF = 0; // mark interrupt processed and allow next interrupt
} // end timer interrupt handler

#endif