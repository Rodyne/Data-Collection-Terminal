#ifndef LCD_H
#define	LCD_H

#include "hardware.h"

#define MAXCURSORX			15
#define MAXCURSORY			15
#define FONTHEIGHT			16
#define CENTRE    			99
#define FONTWIDTH			  8
#define BLACK						1
#define WHITE						0
#define CONTRAST_NORM   14 //22
#define CONTRAST_LIT    16
#define LCD_PG          0xb0  // LCD Pg you can add offset to it
#define LCD_ON					0xAF  // LCD Command to turn on LCD
#define LCD_OFF					0xAE  // LCD Command to turn off LCD
#define LCD_NORMAL			0xA6  // LCD Command to normalise LCD Display (black on white)
#define LCD_REVERSE			0xA7  // LCD Command to invert LCD display (white on black)


// these are the publicly accessable functions for the LCD

void  Backlight(uint8 state);
void  LCDPen(uint8 state);
void  LCDWriteStrAt( uint8 x, uint8 y, char *str); // call above to write a string
void  CLS();      // clear screen and reset pen/cursors
void  InitLCD();  // init the LCD after hard reset
void  LCDInform(char *title, char *msg); // full screen message
uint8 GetMenuItem(char *title, char *m1, char *m2, char *m3, char *m4, char *m5); // user menu template
uint8 ReadNumberAt(uint8 x, uint8 y, uint16 timeout, uint8 MaxLen); // read keyboard number into string
uint8 ReadFloatAt(uint8 x, uint8 y, uint16 timeout, uint8 MaxLen); // read keyboard floating point number into string

#endif	/* LCD_H */

