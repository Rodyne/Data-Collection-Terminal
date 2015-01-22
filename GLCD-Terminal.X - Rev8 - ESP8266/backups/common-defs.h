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

#define STRMAX   30        // use fixed length strings to simplify, fix maximum usable size of this string
#define STRSIZE  STRMAX+2  // but we need 2 chars extra in array for trailing \0 and also a last char in array to put the string length
#define STRLEN   STRMAX+1  // pointer to string length element

#define STRBUFMAX 250       // maximum string size for serial buffer
#define BLACK     1         // Define colours
#define WHITE     0         // Define colours
#define CENTRE    1         // LCD text alignment
#define LEFT      2         // LCD text alignment
#define RIGHT     3         // LCD text alignment

// keypad keyscan return definitions (use displayable ASCII ones)

#define K_SCAN    'S'
#define K_OK      'O'
#define K_CANCEL  'C'
#define K_UP      'U'
#define K_DN      'D'
#define K_DEL     'X'
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

const uint8 ScanCode[17] = { K_1, K_7, K_2, K_8, K_3, K_9, K_OK, K_UP, K_4, K_DECIMAL, K_5, K_0, K_6, K_DEL, K_CANCEL, K_DN, K_DEL }; // maps scan 2 code

typedef uint8 tform[16][8];        // type define a screen form 16 x 8 characters
typedef uint8 str30[STRSIZE]; // type define a fixed length string where elements at the end are space for additional \0 and strings length

enum tdevicemode { CMD, DATA, UNKNOWN };

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
  int16   ExpressionResult;
  uint16  CursorX,CursorY,ScrXMax,ScrYMax,MaxCursorX,MaxCursorY;
  char    SerialStrBuf[STRBUFMAX];
  str30   AppTitle, BarcodeStr, RFIDStr, KeypadStr; // fixed length strings (memory already assigned upto STRLEN)

} Tprg;

typedef struct
{
	uint16 ReturnLine;

} Tstack;




// declare system global vars, not really supposed to define in header but seems more logical here

Tprg    prg;             // script program variables - contains everything required (see definition document)

#endif