// Contains generic Wifi and comms string routines

#ifndef COMMS_H
#define	COMMS_H

#include "hardware.h"

extern char field[10][32]; // these are the fields as split out of the received data field[0]..field[9]

uint8 CheckWIFI();
void  CloseWiFi(); // power down wifi module (hard power down ie power goes off)
uint8  SetWIFI(char * SSID, char *PASS, char *HOST); // Send new connection parameters, these will be saved to the ESP8266 EEPROM and connection will start automatically on power up
//uint8 WaitWiFiStr(char * MatchStr, uint16 TimeoutMs);
uint8 QueryHost(char *FnCode, char *UID, char *Metric); // returns 1 if OK and 0 if error

#endif	/* COMMS_H */

