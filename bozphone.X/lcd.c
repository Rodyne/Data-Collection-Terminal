// GLCD routines and display helper functions for East Rising ER128128 GLCD (buydisplay.com)

#include "hardware.h"
#include "lcd.h"
#include "fonts.h"
#include "functions.h"

enum tdevicemode  { CMD = 0, DATA = 1 }; // LCD serial data can either be a command or data so we need to set mode

typedef struct // structure to put all LCD stuff
{
  uint8  pen,canvas;
} Tscr;

Tscr scr; // define local LCD variables (not accessable outside this module)

void LCDWrite(enum tdevicemode ntype, uint8 data) // write CMD or DATA byte to LCD over SPI Bus (LCD-CS must be low before writing)
{
	SPISelect(SelLCD);	// select LCD
  LCD_DC(ntype);			// Set control pin for CMD or DATA
	SPITransfer(data);	// Send SPI data
}

void LCDPen(uint8 state)
{
	scr.pen=state;
  if(state==BLACK) scr.canvas=WHITE; else scr.canvas=BLACK;
}

void LCDWriteCharAt( uint8 x, uint8 y, uint8 ch) // write char ch at position x,y (note x,y are in multiples of 8 pixels same as the page size)
{
  register uint8 i;
  register int16 offset = FONTHEIGHT * ch; // seek offset into font bitmap

	if(offset+15>sizeof(fonttable1)) return;
	if(x>MAXCURSORX || y>MAXCURSORY) return;

	LCDWrite(CMD,0x40);							// set start line
	LCDWrite(CMD,0);								// 0x00 (we are working in 8 pixel rounding but if we set it >0 we can use pixels inbetween)
	LCDWrite(CMD,LCD_PG+y);					// Set Row
	LCDWrite(CMD,(x*8)&0x0f);				// col lsb +stdot!
	LCDWrite(CMD,0x10+((x*8)>>4));	// col msb
  for(i=0;i<8;i++)
  {
    if(scr.pen) // pen colour > 0 (normal)
    {
      LCDWrite(DATA,fonttable1[offset+i]);
      LCDWrite(DATA,fonttable1[offset+i]);
    }
    else // pen colour=0 ignore background lets invert colours
    {
      LCDWrite(DATA,~fonttable1[offset+i]);
      LCDWrite(DATA,~fonttable1[offset+i]);
    }
  }
  LCDWrite(CMD,LCD_PG+y+1);
	LCDWrite(CMD,(x*8)&0x0f); // col lsb +stdot!
	LCDWrite(CMD,0x10+((x*8)>>4));		 // col msb
  for(i=0;i<8;i++)
  {
    if(scr.pen)
    {
      LCDWrite(DATA,fonttable1[offset+i+8]);
      LCDWrite(DATA,fonttable1[offset+i+8]);
    }
    else
    {
      LCDWrite(DATA,~fonttable1[offset+i+8]);
      LCDWrite(DATA,~fonttable1[offset+i+8]);
    }
  }
}

void LCDWriteStrAt( uint8 x, uint8 y, char *str) // call above to write a string
{
	if(x==CENTRE)
	{
		x = ( MAXCURSORX - StrLen(str) ) / 2;
		if (x<0) x=0;
	}
	while(*str!='\0')
	{
		LCDWriteCharAt(x++, y, *str);
		str++;
	}
}

void CLS() // clear screen and reset pen/cursors
{
	uint8 j,k;
  LCDPen(BLACK);
	LCDWrite(CMD,0x40); // set start line
	LCDWrite(CMD,0x00); // 0x00
	for(j=0;j<16;j++)
	{
		LCDWrite(CMD,0x00);
		LCDWrite(CMD,0x10);
		LCDWrite(CMD,0xb0+j);
		for(k=0;k<128;k++)
		{
			LCDWrite(DATA,scr.canvas);
			LCDWrite(DATA,scr.canvas);
		}
	}
}

void  LCDContrast(uint8 cont)
{
 	LCDWrite(CMD,0x81);
	LCDWrite(CMD,cont);
}

void Backlight(uint8 state) // turn backlight on or off and adjust contrast to make it clearer
{
	BACKLIGHT(state);
	if(state==ON)	LCDContrast(CONTRAST_LIT); else LCDContrast(CONTRAST_NORM);
}


void InitLCD() // copied verbotem from mfg web site, tried to understand but only makes sense if you know how the ST7541 is wired up internally to their LCD I guess!
{
	uint8 i;

	LCDPen(BLACK);

	LCDWrite(CMD,0xA1); // ADC Select reverse direction ? COM127 -> COM0 (A1 = upside down, A0 = right way up)
	LCDWrite(CMD,0xC8); // ComScanDirection (C8 = right to left, C0= Left to right)
  LCDWrite(CMD,0xAB); // Intern OSC ON
  LCDWrite(CMD,0x38); // Set Frame Rate and Booster Efficiency (2 part command)
  LCDWrite(CMD,0x04); // Frame rate=77Hz, BE level 2
	LCDWrite(CMD,0x27); // Regulator internal resistance ratio of internal voltage regulator (111) = ratio of 7.2 ?
	LCDWrite(CMD,0x97); // FRC PWM Set 3=FRC, 15=PWM
	LCDWrite(CMD,0x64); // DC DC boost level 3
	DelayMs(50);
	LCDWrite(CMD,0x2c); // PowerControll VC= ON VR=OFF VF=OFF
	LCDWrite(CMD,0x66); // DC-DC boost level 6
	DelayMs(50);
	LCDWrite(CMD,0x2e); // PowerControll VC= ON VR=ON VF=OFF
	DelayMs(50);
	LCDWrite(CMD,0x2f); // PowerControll VC= ON VR=ON VF=ON

	char greyscale[]={0x00,0x00,0x99,0x99,0xcc,0xcc,0xff,0xff};

	for(i=0;i<8;i++)
	{
		LCDWrite(CMD,0x88+i);
		LCDWrite(CMD,greyscale[i]);
	}

	LCDWrite(CMD,LCD_NORMAL);
	LCDWrite(CMD,LCD_ON);

	// do logo at end of init (the only time we show it)
	CLS();
	LCDPen(WHITE);
	LCDWriteStrAt(3,2,"          ");
	LCDWriteStrAt(3,3,"  ROVING  ");
	LCDWriteStrAt(3,5," DYNAMICS ");
	LCDPen(BLACK);
  LCDWriteStrAt(2,8,"Open Systems");
  LCDWriteStrAt(3,11,"rodyne.com");
	Backlight(ON);
	for(i=0; i<CONTRAST_LIT; i++) // bring up contrast (fade in logo)
	{
		LCDContrast(i);
		DelayMs(50);
	}
	DelayMs(600);
}

void LCDInform(char *title, char *msg) // display message full screen wait for ok button to exit
{
  uint8 x=0;
  uint8 y=4;
	CLS();
	LCDPen(WHITE);
	LCDWriteStrAt(CENTRE,0,title);
	LCDPen(BLACK);
	while(*msg!='\0')
	{
    if(*msg!='\r')
		  LCDWriteCharAt(x++, y, *msg);
    if(x>15 || *msg=='\r')
    {
      x=0;
      y=y+2;
    }  
		msg++;
	}
  
	LCDWriteStrAt(CENTRE,14,"Press OK");
	do
	{
		beep(100); // sound every 10 seconds as a reminder
	}
  while(WaitEvent(30)!=K_OK);
}

uint8 GetMenuItem(char *title, char *m1, char *m2, char *m3, char *m4, char *m5)
{
  // Very simple menu up to 5 menu items m1..m5, return which selected (1..5) or 0 if cancelled
	uint8 selection = 0;
	uint8 MaxSelection = 2;
	uint8 key = 0;
	while(1)
	{
		// draw menu
		CLS();
		LCDPen(WHITE);
		LCDWriteStrAt(0,0,"                ");
		LCDWriteStrAt(0,1,"                ");
		LCDWriteStrAt(1,1,title);

		if(selection==1) LCDPen(WHITE); else LCDPen(BLACK); // highlight selected
	  LCDWriteStrAt(0,5,"1.");
		LCDWriteStrAt(3,5,m1);
		if(selection==2) LCDPen(WHITE); else LCDPen(BLACK); // highlight selected
		LCDWriteStrAt(0,7,"2.");
		LCDWriteStrAt(3,7,m2);
		if(selection==3) LCDPen(WHITE); else LCDPen(BLACK); // highlight selected
	  if(*m3!='\0')
		{
			LCDWriteStrAt(0,9,"3.");
			MaxSelection = 3;
		}
		LCDWriteStrAt(3,9,m3);
		if(selection==4) LCDPen(WHITE); else LCDPen(BLACK); // highlight selected
		if(*m4!='\0')
		{
			LCDWriteStrAt(0,11,"4.");
			MaxSelection = 4;
		}
		LCDWriteStrAt(3,11,m4);
		if(selection==5) LCDPen(WHITE); else LCDPen(BLACK); // highlight selected
		if(*m5!='\0')
		{
			LCDWriteStrAt(0,13,"5.");
			MaxSelection = 5;
		}
		LCDWriteStrAt(3,13,m5);
		LCDPen(BLACK); // normalise pen after use

		DelayMs(500);

		// added so we highlight the selection we pressed before returning ie user can see what menu item number they pressed 1..5
		if(key==K_1 || key==K_2 || key==K_3 || key==K_4 || key==K_5) return selection;

		key = WaitEvent(9999); // wait for keypress event, will power down if none
		switch(key)
		{
			case K_UP	:	if(selection>1) selection--; break; // scroll up
			case K_DN	:	if(selection<5) selection++; break; // scroll down
			case K_OK	:	if(selection>0) return selection; break; // OK button
			case K_1	:	selection = 1; break;
			case K_2	:	selection = 2; break;
			case K_3	:	selection = 3; break;
			case K_4	:	selection = 4; break;
			case K_5	:	selection = 5; break;
			case K_0	:	return 0;  break;
			case K_CANCEL	:	return 0;  break;
		}
		if(selection>MaxSelection) selection=1;
	}
}

uint8 ReadNumberAt(uint8 x, uint8 y, uint16 timeout, uint8 MaxLen) // read integer number from keyboard into string
{
	uint8 key,len;
	char  cursor='_';
  
  for(len=0; len<sizeof(KeypadData); len++) // clr data
    KeypadData[len]=0;
  
  len=0;

	while(1)
	{
  	KeypadData[len]=0;
		key=0;
		while(key==0)
		{
      LCDWriteCharAt( x,y,cursor);
  	  key = WaitEvent(1);
			if(cursor=='_') cursor=' '; else cursor='_';
			if(--timeout==0) return 0;
		}
		if(key==K_CANCEL)
			return 0; // cancelled
		else
		if(key==K_OK)
		{
			return len; // return num of chars in num string
		}
		else
		if(key==K_DEL && len>0)
		{
			len--;
			x--;
		}
		else
		if(key==K_0 || key==K_1 || key==K_2 || key==K_3 || key==K_4 || key==K_5 || key==K_6 || key==K_7 || key==K_8 || key==K_9 )
		{
			if(key!=K_0 || len>0) // number cannot start with 0
			{
				if(len<MaxLen) // and less than max string size
				{
			    KeypadData[len++]=key;
          LCDWriteCharAt(x++,y,key);
				}
			}
		}
	}
}

uint8 ReadFloatAt(uint8 x, uint8 y, uint16 timeout, uint8 MaxLen) // read floating point number from keyboard into string
{
	uint8 key,len;
	char  cursor='_';
  
  for(len=0; len<sizeof(KeypadData); len++) // clr data
    KeypadData[len]=0;
  
  len=0;

	while(1)
	{
  	KeypadData[len]=0;
		key=0;
		while(key==0)
		{
      LCDWriteCharAt( x,y,cursor);
  	  key = WaitEvent(1);
			if(cursor=='_') cursor=' '; else cursor='_';
			if(--timeout==0) return 0;
		}
		if(key==K_CANCEL)
			return 0; // cancelled
		else
		if(key==K_OK)
		{
			return len; // return num of chars in num string
		}
		else
		if(key==K_DEL && len>0)
		{
			len--;
			x--;
		}
		else
		if(key==K_0 || key==K_1 || key==K_2 || key==K_3 || key==K_4 || key==K_5 || key==K_6 || key==K_7 || key==K_8 || key==K_9 || key==K_DECIMAL)
		{
			if(key!=K_DECIMAL || len>0) // number cannot start with decimal
			{
				if(len<MaxLen) // and less than max string size
				{
			    KeypadData[len++]=key;
          LCDWriteCharAt(x++,y,key);
				}
			}
		}
	}
}
