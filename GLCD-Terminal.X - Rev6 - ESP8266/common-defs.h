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

#define LOW_BYTE(x)     ((unsigned char)((x)&0xFF))
#define HIGH_BYTE(x)    ((unsigned char)(((x)>>8)&0xFF))

// ASCII
#define ASCII_SOH     0x01
#define ASCII_STX     0x02
#define ASCII_ETX     0x03
#define ASCII_EOT     0x04
#define ASCII_ACK     0x06
#define ASCII_NACK    0x15

#define BLACK   1         // Define colours
#define WHITE   0         // Define colours

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


enum talign       { CENTRE,LEFT,RIGHT };
enum tdevicemode  { CMD, DATA };


typedef struct // structure to put all config variables
{
  uint8   FontHeight,FontWidth,FontMin,FontMax;
  uint8   pen,canvas;
  uint16  CursorX,CursorY,ScrXMax,ScrYMax,MaxCursorX,MaxCursorY;
  uint32  SessionKey; // RND 32 bit Session key assigned by server on response
  char    SSID[16];   // SSID String of WiFi router,
  char    PASS[16];   // WIFI Security Key/passphrase
  char    HOST[16];   // IP address of host when converted to a string
} Tcfg;

Tcfg cfg; // declare system global vars, not really supposed to do in header but seems more logical here


#endif