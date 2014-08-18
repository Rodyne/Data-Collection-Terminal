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

#ifndef COMMON_DEFS_H
#define COMMON_DEFS_H

#define int8    char
#define int16   short
#define int32   int
#define uint8   unsigned char
#define uint16  unsigned short
#define uint32  unsigned int

#define CMD       0         // For WIFIModule and LCD Module
#define DATA      1         // For WIFIModule and LCD Module
#define PSTRMAX   30        // maximum usable string size in user program [0]..[18]
#define PSTRSIZE  PSTRMAX+2 // need 2 chars extra in array for trailing \0 and the last char in array is the string length
#define STRLEN    PSTRMAX+1 // pointer to string length element

#define STRBUFMAX 250       // maximum string size for serial buffer
#define BLACK     1         // Define colours
#define WHITE     0         // Define colours
#define ERR1      1         // Serial overrun error
#define CENTRE    1         // LCD text alignment
#define LEFT      2         // LCD text alignment
#define RIGHT     3         // LCD text alignment

// keypad keyscan return definitions (displayable ones the same as ASCII)

#define K_SCAN    0x01
#define K_OK      '\r'
#define K_CANCEL  0x02
#define K_UP      0x03
#define K_DN      0x04
#define K_DEL     0x05
#define K_DECIMAL '.'
#define K_0       '0'
#define K_1       '1'
#define K_2       '2'
#define K_3       '3'
#define K_4       '4'
#define K_5       '5'
#define K_6       '6'
#define K_7       '7'
#define K_8       '8'
#define K_9       '9'

const uint8 ScanCode[] = { K_1,K_7,K_2,K_8,K_3,K_9,K_OK,K_UP, K_4,K_DECIMAL,K_5,K_0,K_6,K_DEL,K_CANCEL,K_DN,K_SCAN }; // map our keyscan to ASCII chars

typedef uint8 tform[16][8];     // type define a screen form 16 x 8 characters
typedef uint8 pstring[PSTRSIZE]; // type define a fixed length string where elements at the end are space for additional \0 and strings length

typedef struct // structure for parser, put all variables the users program script can access in here (also makes it easier to remember names with auto-complete)
{
  uint16  PC,SP;
  uint8   volatile ErrorCode;
  uint32  InputTimeoutMs,DimTimeoutMs,OffTimeoutMs,BarcodeTimeout,RFIDTimeout;
  uint32  BarcodePwrTimeout,WIFIPwrTimeout;
  char    SerialTermStr;
  uint8   *CurrentFont;
  uint8   FontHeight,FontWidth,FontMin,FontMax;
  uint8   pen,canvas,DefBrightness;
  uint16  CursorX,CursorY,ScrXMax,ScrYMax,MaxCursorX,MaxCursorY;
  char    SerialStrBuf[STRBUFMAX];
  pstring AppTitle, BarcodeStr, RFIDStr, KeypadStr; // fixed length strings

} Tprg;

typedef struct
{
	uint16 ReturnLine;

} Tstack;

// declare system global vars, not really supposed to define in header but seems more logical here

Tprg    prg;             // script program variables - contains everything required (see definition document)

#endif