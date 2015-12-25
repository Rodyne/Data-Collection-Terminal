// Provides ESP8266 WIFI and WIFI Communications functions

// WARNING: Assumes ESP8266 is flashed with "0.9.5" standard AT firmware.. You will have to replace function code/timings if not!

#include "hardware.h"
#include "comms.h"
#include "functions.h"
#include "lcd.h"

// Variables used by this module

char	WIFISendData[100];	// wifi tx string
char	WIFIRecData[100];   // wifi rx string 
char  field[10][32];      // received data string broken into fields

void CloseWiFi() // power down wifi module (hard power down ie power goes off and ESP8266 looses everything)
{
	SerialClose(WIFI_UART);
	WIFI_PWR(0);
  ESP_PGM(0);  // Turn off I/O for power saving
}

uint8 WaitWiFiStr(char * MatchStr, uint16 TimeoutMs) // Wait for data matching matchstr or timeout (+save string into WifiRecData string/array return num chars rxd)
{
  char CharNo=0;
	char c;
	char *OrigMatchStr = MatchStr; // save the original address of the string in case we dont get a match and need to reset

	SerialPurge(WIFI_UART);

  for(c=0; c<sizeof(WIFIRecData); c++)
    WIFIRecData[c]='\0';
  
  while(TimeoutMs--) // dont debug except where indicated or you WILL loose received characters!
  {		
    while(SerialAvailable(WIFI_UART))
    {
			c = StrUCase(SerialRead(WIFI_UART));
      WIFIRecData[CharNo++]=c;
      if( StrUCase(*MatchStr) == c)
			{
				MatchStr++;
				if(*MatchStr=='\0')
					return 1; // debug here!
			}
			else
				MatchStr = OrigMatchStr; //reset search
    }
  	DelayMs(1);
  }
	return 0; // debug here!
}

void WiFiError(uint8 ErrCode)
{
	CloseWiFi();
	if(ErrCode==0) LCDInform(" ERROR 0 ","Hardware!\rWiFi module is\rnot responding\rReturn unit!");
	if(ErrCode==1) LCDInform(" ERROR 1 ","WiFi Timeout!\try in other\rlocation or\rcheck battery");
	if(ErrCode==2) LCDInform(" ERROR 2 ","WiFi connected\rbut no response\rfrom the server.");
	if(ErrCode==3) LCDInform(" ERROR 3 ","Server has\rresponded with\rNot Authorised");
	if(ErrCode==4) LCDInform(" ERROR 4 ","Server has\rresponded with\rFn Data too big\rcall boz!");
	if(ErrCode==5) LCDInform(" ERROR 5 ","Server has\rresponded with\rJob Not Issued");
	if(ErrCode==6) LCDInform(" ERROR 6 ","Server has\rresponded with\rJob Error 6");
	if(ErrCode==7) LCDInform(" ERROR 7 ","Server has\rresponded with\rJob Error 7");
}

uint8 SetWIFI(char * SSID, char *PASS, char *HOST) // Send new connection parameters, these will be saved to the ESP8266 EEPROM and connection will start automatically on power up
{
	ESP_PGM(1);  // Turn off firmware update mode by setting to 1 (leave off. firmware update is definately a todo as it will require me to write a USB CDC - sniffle!)
  WIFI_PWR(1); // power on wifi and wait for startup
	DelayMs(200); // Check with analyser first few milliseconds have boot/debug data which we can ignore
	SerialOpen(WIFI_UART);
	if(!WaitWiFiStr("NodeMCU", 1500))	// on power up the ESP8266 should get lua welcome message first
	{
		WiFiError(0);
		return 0;			// return error code 1 (worst possible error to have!)
	}
  DelayMs(5000);
	SerialWriteStr(WIFI_UART,"SetWIFI(\"");	// Join access point (assume SSID and password already scanned and loaded in PIC EEPROM)
	SerialWriteStr(WIFI_UART,SSID);
	SerialWriteStr(WIFI_UART,"\",\"");
	SerialWriteStr(WIFI_UART,PASS);
	SerialWriteStr(WIFI_UART,"\",\"");
	SerialWriteStr(WIFI_UART,HOST);
	SerialWriteStr(WIFI_UART,"\")\r\n");
	if(!WaitWiFiStr("\r\nOK", 500))	// check it worked!
	{
		WiFiError(2);
		return 0;			// return error code 2 no response
	}
	CloseWiFi();
  LCDInform(" CONFIG ","WiFi Change OK");
  return 1;
}

uint8 QueryHost(char *FnCode, char *UID, char *Metric) // returns 0 if error and Received Field count if OK
{
  uint8  CharNo,CharCount,FieldNo,ColNo;

	ESP_PGM(1);  // Turn off firmware update mode by setting to 1 (leave off. firmware update is definately a todo as it will require me to write a USB CDC)
  WIFI_PWR(1); // power on wifi and wait for startup
	DelayMs(200); // Check with analyser first few milliseconds have boot/debug data which we can ignore
	SerialOpen(WIFI_UART);
  
	if(!WaitWiFiStr("NodeMCU", 500))	// on power up the ESP8266 should get lua welcome message first
	{
		WiFiError(0);
		return 0;			// return error code 1 (worst possible error to have!)
	}
  
	if(!WaitWiFiStr("\r\nInit time", 5000))	// then ESP8266 should return "OK" if can connect to a wifi
	{
		WiFiError(1);
		return 0;			// return error code 1 (worst possible error to have!)
	}
  
	if(!WaitWiFiStr("Connected", 5000))	// then ESP8266 should return "Connected" if can connect the broker server
	{
		WiFiError(2);
		return 0;
	}
  
  // build up the string to send to ESP module: LUA Command sk:send(FnCode .. "\t" .. node.chipid() .. "\t" .. UID .. "\t" .. Metric .. "\r\n")
	strcpy(WIFISendData,"sk:send(\"");
	strcat(WIFISendData,FnCode);
	strcat(WIFISendData,"\t .. node.chipid() .. \t");
	strcat(WIFISendData,UID);
	strcat(WIFISendData,"\t");
	strcat(WIFISendData,Metric);
	strcat(WIFISendData,"\t\r\n\")");

	SerialWriteStr(WIFI_UART,WIFISendData);   // and send data

	CharCount = WaitWiFiStr("\r\n", 4000); // wait response from server! should be quick!

	CloseWiFi(); // all receiving done. no longer need wifi so power it off and save power

	if(CharCount==0) // ouch, nothing was received, server is up but its not talking possibly due to bad data so return "not authorised" error3
	{
  	WiFiError(3);
		return 0;
	}

	// clr field data with zeros before populating
  for(FieldNo=0; FieldNo<9; FieldNo++)
    for(ColNo=0; ColNo<32; ColNo++)
      field[FieldNo][ColNo]='\0';
  
	FieldNo=0;
	ColNo=0;
  CharNo=0;

	while(WIFIRecData[CharNo]!='\r' && CharNo<CharCount) // split reply into string fields until cr or eof is detected
	{
		if(WIFIRecData[CharNo]=='\t') // tab advances to next field (also advance if we go over the field max length - error!)
		{
			FieldNo++;
      ColNo=0;
		}
		else
			field[FieldNo][ColNo++] = WIFIRecData[CharNo];
		CharNo++;
    if(ColNo>30) // field data exceeds its max length
    {
      WiFiError(4);
      return 0;
    }
	}
  
  if(field[0][0]!='0') // 
  { 
    if(field[0][0]=='1') WiFiError(5);
    else if(field[0][0]=='2') WiFiError(6);
    else WiFiError(7);
    return 0;
  }

	return FieldNo;
}

uint8 CheckWIFI()
{
	ESP_PGM(1);  // Turn off firmware update mode by setting to 1 (leave off. firmware update is definately a todo as it will require me to write a USB CDC)
  WIFI_PWR(1); // power on wifi and wait for startup
	DelayMs(200); // Check with analyser first few milliseconds have boot/debug data which we can ignore
	SerialOpen(WIFI_UART);
	if(!WaitWiFiStr("NodeMCU", 500))	// on power up the ESP8266 should get lua welcome message first
	{
		WiFiError(0);
		return 0;			// return error code 1 (worst possible error to have!)
	}  
	if(!WaitWiFiStr("\r\nInit time", 5000))	// then ESP8266 should return "OK" if can connect to a wifi
	{
		WiFiError(1);
		return 0;			// return error code 1 (worst possible error to have!)
	}  
	if(!WaitWiFiStr("Connected", 5000))	// then ESP8266 should return "Connected" if can connect the broker server
	{
		WiFiError(2);
		return 0;
	}  
	CloseWiFi(); // all receiving done. no longer need wifi so power it off and save power
  return 1;
}  
