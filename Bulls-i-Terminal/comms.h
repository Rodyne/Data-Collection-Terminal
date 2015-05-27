// Contains generic Wifi and comms string routines

#ifndef COMMS_H
#define	COMMS_H

#include "hardware.h"

// ASCII Chars used for message passing
#define ASCII_SOH     0x01
#define ASCII_STX     0x02
#define ASCII_ETX     0x03
#define ASCII_EOT     0x04
#define ASCII_ACK     0x06
#define ASCII_TAB     0x09
#define ASCII_NACK    0x15

extern char			TABfield[10][64];	// tab fields split out from rx string once wifi data read in
extern Tconfiguration	wificfg; // declare system global vars, not really supposed to do in header but seems more logical here

void   ReadWiFiCfg(); 	// retrieve last saved values from NVM (We dont have eeprom so using flash, and reflashing the pic32 will wipe the config)
void   SaveWiFiCfg();   // Save CFG (Note memory is 32 bit wide, we are saving bytes so we need to convert 4 x bytes to Longint before writing
char   StrUCase(char x); // convert input char to uppercase (for comparason below)
uint8  StrCpySubStr(char *substr, char *str, uint8 StartPos, uint8 NumChars); // copies str into substring from pos start for numchars or until null or \r
uint8  StrLocate(char *SubStr, char *str); // see if substr is in str and return 0 if false (case insensitive) or position of substr if true
void   CloseWiFi(); // power down wifi module (hard power down ie power goes off)
uint8  ConfigureESP8266(); // reset connection parameters, these will save to the ESP8266 EEPROM and connection will start automatically on power up
uint16 QueryHost(uint8 FnCode, char * Field1, char *Field2, char *Field3, char *Field4);


#endif	/* COMMS_H */

