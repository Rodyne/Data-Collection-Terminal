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

#define HARDWARE_REV 8
#define FIRMWARE_REV 1

#define _SUPPRESS_PLIB_WARNING			// Get rid of pesky MPLABX plib depreciation warnings
#include <plib.h>										// microchip pic32 peripheral libraries
#include "common-defs.h"						// Font tables etc
#include "fonts.c"									// Font table

#define SYS_FREQ	20000000ul				// 8MHZ XTAL PLL up to 20Mhz, can lower if not using USB or 115200 baud
#pragma config		FNOSC = PRIPLL		// Oscillator Selection = Primary Osc PLL
#pragma config    FPLLIDIV = DIV_2	// 8Mhz -> 4mhz
#pragma config    FPLLMUL = MUL_20	// 4Mhz x 20 = 80Mhz
#pragma config    FPLLODIV = DIV_4	// div4 = 20Mhz
#pragma config		POSCMOD = XT			// Primary Oscillator Configuration (XT = 3-10Mhz XTAL, HS >10Mhz XTAL)
#pragma config		IESO = OFF				// Internal/External Switch Over Disabled
#pragma config		FPBDIV = DIV_1		// Peripheral Clock Divisor (Pb_Clk is Sys_Clk/1)
#pragma config		OSCIOFNC = OFF		// CLKO Output Signal Active on the OSCO Pin Disabled
#pragma config		FSOSCEN = OFF			// Secondary Oscillator Disabled not required and osc2 pins are needed
#pragma config		FWDTEN  = OFF			// Watchdog Timer disabled (will enable in software when required)
#pragma config		WDTPS = PS1024		// watchdog timeout = 1024mS, use it to wake from sleep and do housekeeping
#pragma config		FCKSM = CSDCMD		// clock switching and checking disabled
#pragma config		ICESEL = ICS_PGx2	// required to DEBUG
#pragma config		FUSBIDIO = OFF
#pragma config		FVBUSONIO = ON    // VBUS input from USB

#define GetSystemClock()				(SYS_FREQ)       // Used instead of SYS_FREQ in some modules.
#define GetInstructionClock()		GetSystemClock() // GetSystemClock()/1 for PIC32.  Might need changing if using Doze modes.
#define GetPeripheralClock()		GetSystemClock() // GetSystemClock()/1 for PIC32.  Divisor may be different if using a PIC32 sinc

// PORT A - Hardware note A5 and A6 used for OSC/XTAL connection and cannot be used as I/O and pins A2 & A3 dont exist on this chip
#define CFG_PORT_A_OUT				PORTSetPinsDigitalOut(IOPORT_A, BIT_4 | BIT_7 | BIT_9 | BIT_10 );
#define CFG_PORT_A_IN					PORTSetPinsDigitalIn(IOPORT_A,  BIT_0 | BIT_1 | BIT_8 );
//
#define RFID_CLK()            PORTAbits.RA0           // IN (MUST HAVE WEAK PULL-UP ACTIVE!)
#define RFID_DATA()           PORTAbits.RA1           // IN (MUST HAVE WEAK PULL-UP ACTIVE!)
#define LCD_RST(state)				LATAbits.LATA4 = state  // OUT (Hard reset LCD - Required! Dont be tempted to tie to Vcc! learnt the hard way!)
#define K7(state)							LATAbits.LATA7 = state  // OUT
#define SET_PPS_WIFI_RX       PPSInput(2,U2RX,RPA8)   // IN  (UART 2 WIFI SERIAL RECEIVE)
#define BARCODE_TRIG(state)		LATAbits.LATA9 = state  // OUT (Active low, starts a barcode scan)
#define NU1(state)						LATAbits.LATA10 = state // OUT (Not used)

// PORT B - Hardware note B6, B10, B11 and B12 cannot be used as used for USB
#define CFG_PORT_B_OUT				PORTSetPinsDigitalOut(IOPORT_B, BIT_0 | BIT_1 | BIT_2 | BIT_3 | BIT_4 |BIT_5 | BIT_9 | BIT_10 | BIT_13 | BIT_14 | BIT_15 );
#define CFG_PORT_B_IN					PORTSetPinsDigitalIn(IOPORT_B,  BIT_7 | BIT_8 );
//
#define BUZZER(state)					LATBbits.LATB0 = state  // OUT modulate to sound buzzer
#define K5(state)							LATBbits.LATB1 = state  // OUT PWM for backlight LED control brightness
#define BACKLIGHT(state)		  LATBbits.LATB2 = !state // OUT (Directly connected to LED) (inverted) 1=Power ON, 0=Power OFF
#define RFID_PWR(state)				LATBbits.LATB3 = !state // OUT (inverted) 1=Power ON, 0=Power OFF
#define WIFI_PWR(state)				LATBbits.LATB4 = state  // OUT 1 = Power ON, 0 = shutdown
#define PWR_HOLD(state)				LATBbits.LATB5 = state  // OUT (set high to keep CPU powered on, drop to 'true' power off system fully)
#define KEYSTROBE0()          !PORTBbits.RB7          // IN  (active low) receives key presses
#define SET_PPS_INT4					PPSInput(1,INT4,RPB7)		//     (enable external interupt for above keypresses)
#define KEYSTROBE1()          !PORTBbits.RB8          // IN  (active low) receives key presses
#define SET_PPS_INT3					PPSInput(2,INT3,RPB8)		//     (enable external interupt for above keypresses)
#define K0(state)             LATBbits.LATB9  = state // OUT
#define SET_PPS_SDO           PPSOutput(3,RPB13,SDO1) // OUT SPI DATA OUT
#define LCD_CLK(state)        LATBbits.LATB14 = state // OUT SPI CLK
#define LCD_DC(state)         LATBbits.LATB15 = state // OUT

// PORT C
#define CFG_PORT_C_OUT				PORTSetPinsDigitalOut(IOPORT_C, BIT_0 | BIT_1 | BIT_2 | BIT_4 | BIT_5 | BIT_6 |BIT_7 | BIT_8 | BIT_9  );
#define CFG_PORT_C_IN					PORTSetPinsDigitalIn(IOPORT_C,  BIT_3 );
//
#define K6(state)							LATCbits.LATC0 = state  // OUT
#define ESP_PGM(state)				LATCbits.LATC1 = state	// OUT (connected to ESP8266 GPIO0 - Set to 0V to program firmware, and 1 to use normally)
#define UART2_TX(state)				LATCbits.LATC2 = state  // OUT (used to turn off tx pin during idle)
#define SET_PPS_WIFI_TX       PPSOutput(4,RPC2,U2TX)  // OUT (UART 2 WIFI TRANSMIT OUT)
#define SET_PPS_BARCODE_RX    PPSInput(3,U1RX,RPC3)   // IN  (UART 1 BARCODE RECEIVE IN)
#define BARCODE_PWR(state)		LATCbits.LATC4 = !state // OUT (inverted) 1=Power ON, 0=Power OFF
#define UART1_TX(state)				LATCbits.LATC5 = state  // OUT (used to turn off tx pin during idle)
#define SET_PPS_BARCODE_TX    PPSOutput(1,RPC5,U1TX)  // OUT (Note we dont actually have a need to write to the barcode module)
#define K1(state)             LATCbits.LATC6  = state // OUT
#define K2(state)             LATCbits.LATC7  = state // OUT
#define K3(state)             LATCbits.LATC8  = state // OUT
#define K4(state)             LATCbits.LATC9  = state // OUT


#define BARCODE_UART          UART1
#define BARCODE_BAUD          9600
#define WIFI_UART             UART2
#define WIFI_BAUD             9600

// defs only app to this specific hardware

#define LCD_PG                0xb0  // LCD Pg you can add offset to it
#define NORMAL_CONTRAST       0x14
#define LCD_ON								0xAF  // LCD Command to turn on LCD
#define LCD_OFF								0xAE  // LCD Command to turn off LCD
#define LCD_NORMAL						0xA6  // LCD Command to normalise LCD Display (black on white)
#define LCD_REVERSE						0xA7  // LCD Command to invert LCD display (white on black)

// EEPROM does not exist on PIC32 but we can use spare flash at end to simulate. Note can erase flash only up to 1000 times before chip is toast
#define NVM_END					(void*) (0xBD000000 + BMXPFMSZ) // End Address of PIC321xx FLASH Memory
#define NVM_START		    (void*) (NVM_END - 0x400)	       // Start address of last page is 1K back (Note 1024 page size on PIC32MX1xx Different for bigger pic32's )

uint32  *NVMPtr;
char    NVM[64];   // NVM is saved/read in 64 bytes so this is 16 longints

volatile uint16  RxData1In, RxData1Out, RxData2In, RxData2Out; // indexes for char read in from uART and char read out by user
volatile uint8   RxBuffer1[32], RxBuffer2[900]; // interrupt receive buffers wi-fi data buffer should be much bigger than barcode as it will be used more
volatile uint8   KeyScan; // what keypad line was active in interrupt

uint8 WIFIRetries = 0;  // Need this so we can see if wifi is failing or not and enter setup mode/warn if fails more than 3 times in a row.
uint8 HostRetries = 0;  // Need this so we can see if host is failing or not and enter setup mode/warn if fails more than 3 times in a row.
uint8 USBPowered  = 0;

char BarcodeData[32];
char RFIDData[32];
char KeypadData[32];
char WIFISendData[400];
char WIFIRecData[400];
char TABfield[10][64]; // Our 10 tab fields populated once wifi data read in


void DelayMs(uint32 time) // Blocking wait in milliseconds (approx)
{
	time = time * 5000;
	while(time--)	asm("NOP");
}

void DelayUs(uint32 time) // Blocking wait in microseconds (approx)
{
	time = time * 5;
	while(time--)	asm("NOP");
}

void beep(uint16 BCMilliseconds) // set buzzer active for this many mS - note this is blocking but interrupts still work
{
	while(BCMilliseconds--)
	{
		BUZZER(1);
		DelayUs(480);
		BUZZER(0);
		DelayUs(480);
	}
}


uint8 TABFIELD(char *src, char *DataStr, uint8 FieldNo) // returns true if set we set the field string to src
{
	static uint8 len;
	static uint16	i;

	len = 0;
	src = "";
	i = 0;

	while(*(DataStr+i)!='\0' && *(DataStr+i)!=STX) // skip to where tab delimeted data begins
		i++;

	while(*(DataStr+i)!='\0')
	{
		if(*DataStr == '\t') FieldNo--;
		if(!FieldNo)
		{
		  src[len++] = *DataStr;
		}
		i++;
	}
}

uint16 STRLEN(const char * str) // return legth of null terminated string minus null terminator - NO CHECKS!
{
	uint16 len = 0;
	while(*str!='\0')
	{
		len++;
		str++;
	}
	return len;
}

void STRCAT(char *src, char *s1) // concatinate s1 onto the end of src string return new str len  - NO CHECKS!
{
	while(*src!='\0')	src++; // get to end of src
	while(*s1!='\0') // Add s1 to end
	{
		*src = *s1;
		src++;
		s1++;
	}
	*src='\0';
}

void STRSET(char *src, char *s1) // clr src str and copy s1 to it - NO CHECKS!
{
	*src = '\0';
	while(*s1!='\0') // copy s1 to src
	{
	  *src = *s1;
		s1++;
		src++;
	}
	*src='\0';
}

char UCASE(char x) // convert input char to uppercase (for comparason below)
{
	if(x>96 && x<123) // conv a..A, b..B, c..C ... z..Z
		return x-32;
	else
		return x;
}


char LCDWrite(uint8 ntype, uint8 data) // write CMD or DATA byte to LCD over SPI Bus (LCD-CS is low permenantly)
{
	static uint8 dummy;
  LCD_DC(ntype);                // Set control to CMD or DATA
  SPI1BUF = data;               // write to shift register to begin transmission
  while( !SPI1STATbits.SPIRBF); // wait for last transfer to complete
  dummy = SPI1BUF;							// dummy read (required even though not used!)
}

void LCDMode(uint8 lcd_mode)
{
	LCDWrite(CMD,lcd_mode); // execute LCD CMD
}

void LCDWriteCharAt( uint8 x, uint8 y, uint8 ch) // write char ch at position x,y (note x,y are in multiples of 8 pixels same as the page size)
{
  register uint8 i;
  register int16 offset = (cfg.FontHeight*(ch-cfg.FontMin)); // seek offset into font bitmap

	if(offset+15>sizeof(font1)) return;
	if(x>cfg.MaxCursorX || y>cfg.MaxCursorY) return;

	LCDWrite(CMD,0x40); // set start line
	LCDWrite(CMD,0);    // 0x00 (we are working in 8 pixel rounding but if we set it >0 we can use pixels inbetween)

	LCDWrite(CMD,LCD_PG+y);
	LCDWrite(CMD,(x*8)&0x0f); // col lsb +stdot!
	LCDWrite(CMD,0x10+((x*8)>>4));		 // col msb
  for(i=0;i<8;i++)
  {
    if(cfg.pen) // pen colour > 0 (normal)
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
    if(cfg.pen)
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
    cfg.CursorY=cfg.CursorY+2;
    if(cfg.CursorY>cfg.MaxCursorY) cfg.CursorY=0;
		return;
	}
	if(ch=='\r')
	{
    cfg.CursorX=0;
		return;
	}
  if(cfg.CursorX<cfg.MaxCursorX && cfg.CursorY<cfg.MaxCursorY)
  {
    LCDWriteCharAt(cfg.CursorX,cfg.CursorY,ch);
  }
  cfg.CursorX++;
  if(cfg.CursorX>cfg.MaxCursorX)
  {
    cfg.CursorX=0;
    cfg.CursorY=cfg.CursorY+2;
    if(cfg.CursorY>cfg.MaxCursorY) cfg.CursorY=0;
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
  cfg.CursorX=x;
  cfg.CursorY=y;
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
  cfg.pen=BLACK;
  cfg.canvas=WHITE;
	LCDWrite(CMD,0x40); // set start line
	LCDWrite(CMD,0x00); // 0x00
	for(j=0;j<16;j++)
	{
		LCDWrite(CMD,0x00);
		LCDWrite(CMD,0x10);
		LCDWrite(CMD,0xb0+j);
		for(k=0;k<128;k++)
		{
			LCDWrite(DATA,cfg.canvas);
			LCDWrite(DATA,cfg.canvas);
		}
	}
  cfg.CursorX=0;
  cfg.CursorY=0;
}

void Pen(uint8 state)
{
  if(state==BLACK)
  {
    cfg.pen=BLACK;
    cfg.canvas=WHITE;
  }
  else
  {
    cfg.pen=WHITE;
    cfg.canvas=BLACK;
  }
}

void LCDSetContrast(uint8 clevel)
{
  if(clevel>0x30) clevel=0x30; // clip max
  if(clevel<0x8) clevel=0x8;   // clip min
 	LCDWrite(CMD,0x81);
 	LCDWrite(CMD,clevel);
}

void InitLCD() // copied verbotem from mfg web site, tried to understand but only makes sense if you know how the ST7541 is wired up internally to their LCD I guess!
{
	int i;
	char greyscale[]={0x00,0x00,0xdd,0xdd,0xaa,0xaa,0xff,0xff};

  cfg.MaxCursorX = 16;
  cfg.MaxCursorY = 15;
  cfg.ScrXMax = 128;
  cfg.ScrYMax = 128;

	cfg.CursorX=0;
  cfg.CursorY=0;
	cfg.pen=BLACK;
	cfg.canvas=WHITE;

	cfg.FontWidth=8;
	cfg.FontHeight=16;
	cfg.FontMin=0;
	cfg.FontMax=96;

	LCD_RST(1);
	DelayMs(10);
	LCD_RST(0);
	DelayMs(10);
	LCD_RST(1);
	DelayMs(50);  

	LCDWrite(CMD,0x38); // Mode set to 77 hz booster efficiency level 2 (this is the default why issue it?)
	LCDWrite(CMD,0x04); //
  LCDWrite(CMD,0xAB); // Intern OSC ON
	LCDWrite(CMD,0x57); // Set LCD Bias last 3 bits = 1/12 again the default - wtf!)
	LCDWrite(CMD,0x38); // Mode set to 73 hz booster efficiency level 1 why we changing it again?
  LCDWrite(CMD,0x80); //
	LCDWrite(CMD,0x27); // Regulator internal resistance ratio of internal voltage regulator (111) = ratio of 7.2 ?
	LCDWrite(CMD,0x64); // DC DC Step up Vout = 3 x boost
	DelayMs(1); // while the error comes in
	LCDWrite(CMD,0x2c); // PowerControll VC= ON VR=OFF VF=OFF
	DelayMs(1); // while the error comes in
	LCDWrite(CMD,0x67); // boost level 6
	LCDWrite(CMD,0x2e); // PowerControll VC= ON VR=ON VF=OFF
	DelayMs(1); // while the error comes in
	LCDWrite(CMD,0x2f); // PowerControll VC= ON VR=OFF VF=ON
	DelayMs(1); // while the error comes in
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
		UART1_TX(0);
	}
	else if(id==UART1)
  {
    INTEnable(INT_SOURCE_UART_RX(UART2), INT_DISABLED);
  	U2STAbits.UTXEN=0;
		UART2_TX(0);
	}
}

uint16 StrInWiFiData(char *str, uint16 MaxLen) // Case insensitive string match
{
	uint16 i = 0;
	while(i<MaxLen)
	{
		if(UCASE(*str)==UCASE(WIFIRecData[i]))
		{
			str++;
			if(*str=='\0') return 1;
		}
		else
			i++;
	}
  return 0;
}

void CloseWiFi()
{
	SerialClose(WIFI_UART);
	WIFI_PWR(0);
  ESP_PGM(0);  // Turn off I/O for power saving
}

uint8 WaitWiFiStr(char * MatchStr, uint16 TimeoutMs) // Wait for data matching matchstr or timeout (+save string into WifiRecData string/array return num chars rxd)
{
	static uint16 i;
	static char c;
	char *OrigMatchStr = MatchStr; // save the original address of the string in case we dont get a match and need to reset

  for(i=0; i<sizeof(WIFIRecData)-1; i++)
	  WIFIRecData[i]='\0';

  while(TimeoutMs--)
  {
		// dont debug except where indicated ot interrupts will not working properly and you will loose characters!
    while(SerialAvailable(WIFI_UART))
    {
			c = UCASE(SerialRead(WIFI_UART));
			WIFIRecData[i++]=c;
      if( UCASE(*MatchStr) == c)
			{
				MatchStr++;
				if(*MatchStr=='\0')
					return i; // debug here!
			}
			else
				MatchStr = OrigMatchStr; //reset search
    }
  	DelayMs(1);
  }
	return 0; // debug here!
}

uint16 ReadWifiData(uint16 TimeoutMs, uint8 MinLen)
{
	static uint16 DataLen;
	static uint16 i;
	for(i=0; i<sizeof(WIFIRecData)-1; i++)
		WIFIRecData[i]=0;
	DataLen = 0;
  while(TimeoutMs--)
  {
    while(SerialAvailable(WIFI_UART))
    {
      WIFIRecData[DataLen] = SerialRead(WIFI_UART); // build up string as received
      if(DataLen<sizeof(WIFIRecData)-1) DataLen++;
			if(DataLen>=MinLen) TimeoutMs = 100; // once we have the min chars we can end a bit earlier
    }
  	DelayMs(1);
  }
  WIFIRecData[DataLen] = '\0';	// terminate it with null just in case we use it as a string
	return DataLen;
}

void ESP8266Reset() // reset common parameters
{
	SerialWriteStr(WIFI_UART,"AT+CWJAP=\"\",\"\"\r\n"); // if connection fails then loose access point router settings
 	DelayMs(100);
 	SerialWriteStr(WIFI_UART,"AT+CIPMUX=0\r\n");	  // set to single socket connection
  DelayMs(10);
 	SerialWriteStr(WIFI_UART,"AT+CWMODE=1\r\n");	  // set to Station (slave) mode, device will talk to router/host
  DelayMs(10);
  SerialWriteStr(WIFI_UART,"AT+RST\r\n"); // Reset module after settings cleared, hopefully next retry will then work
 	DelayMs(1000);
  CloseWiFi();
}

// open wifi connection and send packet returns failure codes 0..3 or if >5 the num of chars received from query sent
uint16 QueryHost(uint8 FnCode, char * Field1, char *Field2, char *Field3, char *Field4)
{
	uint16 DataLen = 0;
	uint8 i,j;
	char	DataLenAsStr[6];
	char	prefix[4]					= { ASCII_SOH, FnCode, ASCII_STX, '\0' }; // null terminated prefix
	char	postfix[5]				= { ASCII_ETX, HARDWARE_REV, FIRMWARE_REV, ASCII_EOT, '\0' }; // null terminated postfix

	// remember 0x00 is invalid ASCII in our string as it will be perceived as the terminator so deviceid, fncode and fieldcount cannot be 0!

	/* Assumes ESP8266 firmware is 0.9.3, May have to tweek this function code/timings if not.
	*/
	WIFIRetries++;
	HostRetries++;
  ESP_PGM(1);  // Turn off firmware update mode by setting to 1 (leave off as firmware update is definately a todo as it will require a USB to serial implementation in the PIC firmware)
  WIFI_PWR(1); // power on wifi and wait for startup
	DelayMs(50); // Check with analyser first few milliseconds have boot/debug data which we can ignore
	SerialOpen(WIFI_UART);
	if(!WaitWiFiStr("ready", 1500))	// on power up the ESP8266 should return "ready" within 500mS if not the module is probably stuffed
	{
		CloseWiFi();
		return 1;			// return error code 1
	}
	DelayMs(150);
	SerialWriteStr(WIFI_UART,"ATE0\r\n");					// ECHO OFF
	DelayMs(10);
	SerialWriteStr(WIFI_UART,"AT+CWJAP=\"");			// Join access point (assume SSID and password already scanned and loaded in PIC EEPROM)
	SerialWriteStr(WIFI_UART,cfg.SSID);
	SerialWriteStr(WIFI_UART,"\",\"");
	SerialWriteStr(WIFI_UART,cfg.PASS);
	SerialWriteStr(WIFI_UART,"\"\r\n");
	if(!WaitWiFiStr("ok\r", 5000))	// Returns OK if connected to wifi
	{
		ESP8266Reset(); // cant connect so reset everything and abort
		return 2;				// return error code 2
	}
	WIFIRetries = 0;
	DelayMs(1000);
	SerialWriteStr(WIFI_UART,"AT+CIPSTART=\"TCP\",\""); // Connect to TCP Port
	SerialWriteStr(WIFI_UART,cfg.HOST);
	SerialWriteStr(WIFI_UART,"\",2001\r\n");
	if(!WaitWiFiStr("linked", 5000))									// returns OK if connected to socket (2000mS is probably too long)
	{
		ESP8266Reset(); // cant connect so reset everything and abort
		return 3;				// return error code 3
	}
	HostRetries = 0;
	// Got here, assume now connected to host socket..

	// build up transmit string
	STRSET(WIFISendData,prefix);
	STRCAT(WIFISendData,Field1);
	STRCAT(WIFISendData,"\t");
	STRCAT(WIFISendData,Field2);
	STRCAT(WIFISendData,"\t");
	STRCAT(WIFISendData,Field3);
	STRCAT(WIFISendData,"\t");
	STRCAT(WIFISendData,Field4);
	STRCAT(WIFISendData,"\t");
	STRCAT(WIFISendData,postfix);

	DataLen = STRLEN(WIFISendData);
	itoa(DataLenAsStr, DataLen, 10); // convert length of the data to a base 10 num string for sending in wifi cmd

	// send the data to the host
	SerialWriteStr(WIFI_UART,"AT+CIPSEND=");
	SerialWriteStr(WIFI_UART,DataLenAsStr);
	SerialWriteStr(WIFI_UART,"\r\n");
	DelayMs(10);
	SerialWriteStr(WIFI_UART,WIFISendData);

	DataLen = WaitWiFiStr("\x3\x4\r\nOK\r\n,", 3000); // wait response from server! should be  +IPD,<datalen>:msg\r\nOK\r\n, note \x3 = ASCII Char 0x03, \x4 = ASCII Char 0x04

	CloseWiFi(); // nothing so close connection/return error code!
	
	if(DataLen==0) return 4;

	// clear our fields before populating
	for(j=0; j<10; j++)
		for(k=0; k<64; k++)
      TABfield[j][k]='\0';

	WIFIRecData[i]

	while(WIFIRecData[i]!='\0' && WIFIRecData[i]!=STX) // skip rec buffer to where tab delimeted data begins
		i++;

	j=0;
	k=0;

	while(WIFIRecData[i+k]!=ETX) // split into fields
	{
		if(WIFIRecData[i+k] == '\t')
		{
			j++;
			k=0;
		}
		else
			TABField[j][k] = WIFIRecData[i+k];
		k++;
	}

	return DataLen;
}


void CloseRFID()
{
	RFID_PWR(0);
}

uint8 ReadRFID()
{
	RFID_PWR(1);
}


void CloseBarcode()
{
	SerialClose(BARCODE_UART);
	BARCODE_PWR(0);
  BARCODE_TRIG(0);
}

uint8 StrInBarcodeData(char *str, uint8 MaxLen)
{
	uint8 i = 0;
	while(i<MaxLen)
	{
		if(*str==BarcodeData[i])
		{
			str++;
			if(*str=='\0') return 1;
		}
		else
			i++;
	}
  return 0;
}

uint8 ReadBarcode() // Wake up and Read in barcode string from MT700 (up to max input size) or timeout
{
  uint8  BarcodeLen=0;
  uint32 timeleft = 1000; // set our timout for this operation in milliseconds*delay (=3000mS)
	
  BARCODE_TRIG(1);
	BARCODE_PWR(1);	// power up and reset module then enable interrupt and initiate scan
	DelayMs(50);
  SerialOpen(BARCODE_UART);
  BARCODE_TRIG(0); // initiate scan on MT700 (should wake up and led go on to read) as its a seperate CPU
	BarcodeData[0] = '\0';
	while(timeleft--)
  {
    if(SerialAvailable(BARCODE_UART))
    {
      BarcodeData[BarcodeLen] = SerialRead(BARCODE_UART); // build up barcode string as received
      if(BarcodeData[BarcodeLen]=='\n')
			{
    		beep(200);
        BarcodeData[BarcodeLen+1]='\0'; // set end
				CloseBarcode();
				return BarcodeLen+1;
			}
      if(BarcodeLen<30) BarcodeLen++;
    }
		DelayMs(3);
  }
	// at this point we have either read in a good barcode string or timed out, either way we are finished with MT700 for now so power it down
	CloseBarcode();
  return 0;
}


void InitTerminal(void) // Initialise PIC32 and terminal io pins
{
  DDPCONbits.JTAGEN = 0; // Turn off JTAG allows RA0, RA1, RA4, and RA5 to be used
  SYSTEMConfig(SYS_FREQ, SYS_CFG_WAIT_STATES | SYS_CFG_PCACHE);

 	U1PWRCbits.USBPWR=1;
	DelayMs(20);
	USBPowered=U1OTGSTATbits.SESVD;
 	U1PWRCbits.USBPWR=0;

	CVREFClose();//disables the CVREF module.

  PPSUnLock; // Unlock PPS to allow UART AND SPI PIN Mapping
  SET_PPS_SDO;
  SET_PPS_WIFI_TX;
  SET_PPS_WIFI_RX;
  SET_PPS_BARCODE_TX;
  SET_PPS_BARCODE_RX;
	SET_PPS_INT3;
	SET_PPS_INT4;
	PPSLock; // lock PPS

  // Init IO Pins using macros - why? - keep pin defs together in header for easier changing rather than going to top of program and then back here all the time!
	CFG_PORT_A_IN
	CFG_PORT_A_OUT
	CFG_PORT_B_IN
	CFG_PORT_B_OUT
	CFG_PORT_C_IN
	CFG_PORT_C_OUT

	// Note that IO Pins, when not in use, must be set to the same idle state of the pin they control to ensure current is not
	// accidentally supplied to sub-system when not in use and resuce power. Once the sub-system is powered up then we set the correct state.

	PWR_HOLD(1);		// hold power on system after keypress (firmware must do this first thing to latch power on!)
  BUZZER(0);			// Turn off
	WIFI_PWR(0);    // turn off
  ESP_PGM(0);			// 0=firmware update mode, 1=Normal mode, however when wifi is innactive we Turn off I/O for power saving
	BARCODE_PWR(1); // Note 0 does not mean 0V it means off (in this case a logic 1 turns the P-Channel MOSFET OFF so the #define is inveted to make an active 1)
  BACKLIGHT(0);   // Turn off
  BARCODE_TRIG(0);// 0=Trigger (take reading) 1=idle, however when barcode is innactive we Turn off I/O for power saving
  RFID_PWR(0);		// Note 0 does not mean 0V it means off (in this case a logic 1 turns the P-Channel MOSFET OFF so the #define is inveted to make an active 1)

	// turn off PIC32 modules not used to save power (a miliamp at most but it all helps)
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

  // SPI 8-bit, Master Mode, clk idle high, Baud =  pOSC/4
  SPI1CON = 0x00;
  SPI1BRG = 0x0004;
  SPI1STATbits.SPIROV = 0;
  SPI1CON = 0x00008260;

	// Configure interrupts but dont enable them until required

	// Use Ext Interrupts for KBD (PIC32 will be in sleep only WDT and ext int will wake it)
	// set up external interrupts INT3 (RB8/KEYSTROBE1)
	ConfigINT3(EXT_INT_DISABLE | FALLING_EDGE_INT | EXT_INT_PRI_2);
	// set up external interrupts INT4 (RB7/KEYSTROBE0)
	ConfigINT4(EXT_INT_DISABLE | FALLING_EDGE_INT | EXT_INT_PRI_3);

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

  // Configure for Multi-Vectored Interrupts //
  INTConfigureSystem(INT_SYSTEM_CONFIG_MULT_VECTOR);

	InitLCD();

	CloseWiFi();
	CloseBarcode();
	CloseRFID();

  INTEnableInterrupts(); // gloabally enable interrupts but all are currently configured off until required
}


// 99.9% of the time I expect the program to be here, where it will sit in low power mode until a key pressed or timeout indicated occurs

int8 WaitEvent(uint16 TimeoutSeconds, uint16 PowerOffTimeout)
{
  const  uint8 ScanCode[16] = { K_1, K_2, K_3, K_OK, K_4, K_5, K_6, K_CANCEL, K_7, K_8, K_9, K_UP, K_DECIMAL, K_0, K_DEL, K_DN }; // map scan 2 code

	static uint8 BacklightTimeout;

	uint8 KeyNo = 0;    // local and static
	
	BacklightTimeout = 16; // seconds

	// turn peropherals off
	CloseWiFi();
	CloseBarcode();
	CloseRFID();
	K0(0); K1(0); K2(0); K3(0); K4(0); K5(0); K6(0); K7(0); // all key lines low so when keypressed the strobe lines will activate
  mINT3IntEnable(1); // enable keystrobe1 line interrupt
  mINT4IntEnable(1); // enable keystrobe0 line interrupt
	KeyScan=0;

	while(KeyScan==0 && TimeoutSeconds--) // wait here for the above timeout or a key press (wakes up and does about 1 loop per second unless interrupted)
	{
		EnableWDT();
		ClearWDT();
	  PowerSaveSleep(); // wait for watchdog timeout (1024mS) or keypress. only GLCD drawing pwr, terminal consumption about 700uA at this point
		ClearWDT();
		DisableWDT();
		// wakes up every second (or if keypressed) and check for backlight timeout
		if(BacklightTimeout>0) BacklightTimeout--;
		if(BacklightTimeout==0)	BACKLIGHT(0);
		// check for power off inactivity timeout
    if(PowerOffTimeout>0) PowerOffTimeout--;
	  if(PowerOffTimeout==0) PWR_HOLD(0);
	}
	mINT3IntEnable(0);
	mINT4IntEnable(0);

	if(KeyScan==1) // SCAN Key caused the interrupt (Note the scan key is not active in R8 board - I forgot :-(
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
      	BACKLIGHT(1);
      	beep(200);
				return ScanCode[KeyNo];
			}
			if(KEYSTROBE1())
			{
      	BACKLIGHT(1);
				beep(200);
				return ScanCode[KeyNo+8];
			}
			KeyNo++;
		}
		return 0;
	}
	else
	  return 0;
}


void __ISR(_EXTERNAL_3_VECTOR,IPL2) External_Interrupt_3(void) // PRB8 - KEYSTROBE1 goes low
{
	KeyScan = 2;
  mINT3ClearIntFlag();
}

void __ISR(_EXTERNAL_4_VECTOR,IPL3) External_Interrupt_4(void) // PRB7 - KEYSTROBE0 goes low
{
	KeyScan = 3;
	mINT4ClearIntFlag();
}

void __ISR(_UART1_VECTOR, IPL4) IntUart1Handler(void) // UART 1 RX Interrupt Handlers
{
  if(IFS1bits.U1RXIF)  // RX char ready to read from UART into Rxbuffer
  {
    RxBuffer1[RxData1In] = UARTGetDataByte(UART1); // Read data from Rx.
    if(RxData1In<(sizeof(RxBuffer1)-1)) // Make sure buffer is not full and move the RxBuffer Index to indicate a char available if we are at the buffer limit this indicates an error
      RxData1In++;
    IFS1bits.U1RXIF=0; // All done, so Clear Interrupt flag to allow new interrupt
  }
} // end UART interrupt handler

void __ISR(_UART_2_VECTOR, IPL5) IntUart2Handler(void) // UART 2 RX Interrupt Handlers
{
  if(IFS1bits.U2RXIF)  // RX char ready to read from UART into Rxbuffer
  {
    RxBuffer2[RxData2In] = UARTGetDataByte(UART2); // Read data from Rx.
    if(RxData2In<(sizeof(RxBuffer2)-1)) // Make sure buffer is not full and move the RxBuffer Index to indicate a char available if we are at the buffer limit this indicates an error
      RxData2In++;
    IFS1bits.U2RXIF=0; // All done, so Clear Interrupt flag to allow new interrupt
  }
} // end UART interrupt handler


#endif