// Provides ESP8266 WIFI and WIFI Communications functions

// WARNING: Assumes ESP8266 is flashed with "0.9.5" standard AT firmware.. You will have to replace function code/timings if not!

#include "hardware.h"
#include "comms.h"
#include "lcd.h"

enum TFnCodes	// CMD functions sent in message packets to bulls-i broker service via WiFi to retrieve or set data
{
	// Get
	FnGetJob					= 0x10,	// Send: F1=JobID.							Rx:	F1=StatusStr, F2=JobStatus, F3=JobStatusStr
	FnGetAssetVc			= 0x11,	// Send: f1=AssetID.						Rx: F1=StatusStr, F2=AssetName, F3=CurrVol, F4=BatchCd, F5=StatusCd, F6=Locn, F7=CurrTemp, F8=Setpoint, F9=SCADAReg, F10=Padlock Cd
	FnGetMovement			=	0x12, // Send: F1=JobID, F2=AssetID.	Rx: F1=StatusStr, F2=MovementID, F3=AssetName, F4=EstiDip, F5=iDip, F6=EstfDip, F7=fDip, F8=fVol
	FnGetAdditions		= 0x13, // Send: F1=MovementID					Rx: F1=StatusStr
	FnGetSMID					= 0x14, // Send: F1=SMID								Rx: F1=StatusStr, F2=ProductName,  F3=AddQtyStr (Incl UOM)
	FnGetUser					=	0x15,	// Send: f1=UserKey							Rx: F1=UserName,	F2=UserPermissions
	// Set
	FnSetJobStart			=	0x81,
	FnSetJobSignOn		=	0x82,
	FnSetJobEnd				=	0x83,
	FnSetJobSignOff		=	0x84,
	FnSetUnquarantine	=	0x92,
	FnSetAssetLocn		=	0x93,
	FnSetAssetCleaned	=	0x94,
	FnSetQuarantined	=	0x95,
	FnSetInitialDip		=	0xA0,
	FnSetFinalDip			=	0xA1,
	FnSetAddnComplete	=	0xB0,
	// Print

};



// Variables used by this module

char			WIFISendData[400];	// wifi tx string
char			WIFIRecData[400];	// wifi rx string

// 2 variables below are also declared extern in header and available to other modules

char			TABfield[10][64];	// tab fields split out from rx string once wifi data read in


char StrUCase(char x) // convert input char to uppercase (for comparason below)
{
	if(x>96 && x<123) // conv a..z to A..Z
		return x-32;
	else
		return x;
}

uint8 StrCpySubStr(char *substr, char *str, uint8 StartPos, uint8 NumChars) // copies str into substring from pos start for numchars or until null or \r
{
	*substr = '\0'; // clr substring;
	while(*str!='\0' && StartPos--)
		str++; // get to start of string where we want to copy from
	if(*str=='\0') return 0; // if we got to the end of the str but not to the posn we want there is an error
	while(*str!='\0' && *str!='\r' && NumChars--) // Add s1 to end until we have counted numchars to 0 (NOTE we also terminate on \r!)
	{
		*substr++ = *str++;
	}
	*substr='\0';
	if(NumChars) return 0; else return 1; // return 1 if ok and 0 if we could not copy all the chars
}

uint8 StrLocate(char *SubStr, char *str) // see if substr is in str and return 0 if false (case insensitive) or position of substr if true
{
	uint16 i=0;
	char *OrigMatchStr = SubStr; // save the original address of the string in case we dont get a match and need to reset
	while(*str!='\0')
	{
		if( StrUCase(*SubStr) == StrUCase(*str++))
		{
			SubStr++;
			if(*SubStr=='\0')	return i;
		}
		else
			SubStr = OrigMatchStr; //reset search
		i++;
	}
	return 0;
}

uint16 StrInWiFiData(char *str, uint16 MaxLen) // Case insensitive string match
{
	uint16 i = 0;
	while(i<MaxLen)
	{
		if(StrUCase(*str)==StrUCase(WIFIRecData[i]))
		{
			str++;
			if(*str=='\0') return 1;
		}
		else
			i++;
	}
  return 0;
}

void CloseWiFi() // power down wifi module (hard power down ie power goes off)
{
	SerialClose(WIFI_UART);
	WIFI_PWR(0);
  ESP_PGM(0);  // Turn off I/O for power saving
}

uint8 WaitWiFiStr(char * MatchStr, uint16 TimeoutMs) // Wait for data matching matchstr or timeout (+save string into WifiRecData string/array return num chars rxd)
{
	static char c;
	char *OrigMatchStr = MatchStr; // save the original address of the string in case we dont get a match and need to reset

	SerialPurge(WIFI_UART);

  while(TimeoutMs--)
  {
		// dont debug except where indicated ot interrupts will not working properly and you will loose characters!
    while(SerialAvailable(WIFI_UART))
    {
			c = SerialRead(WIFI_UART);
      if( StrUCase(*MatchStr) == StrUCase(c))
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

uint16 ReadWifiData(uint16 TimeoutMs, uint8 MinLen)
{
	static uint16 DataLen;
	static uint16 i;
	SerialPurge(WIFI_UART);
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

void WiFiError(uint8 ErrCode)
{
	CloseWiFi();
	if(ErrCode==1) LCDInform("ERROR 1","ESP8266 FAIL!");
	if(ErrCode==2) LCDInform("ERROR 2","Firmware Err");
	if(ErrCode==3) LCDInform("ERROR 3","Error joining AP");
	if(ErrCode==4) LCDInform("ERROR 4","Host Comm Timeout");
	if(ErrCode==5) LCDInform("ERROR 5","No Data!");
	if(ErrCode==6) LCDInform("ERROR 6","Bad Response from broker!");
}

uint8 ConfigureESP8266() // reset connection parameters, these will save to the ESP8266 EEPROM and connection will start automatically on power up
{
	CloseWiFi(); // power off ESP-01
	DelayMs(1000);
  ESP_PGM(1);  // Turn off firmware update mode by setting to 1 (leave off as firmware update is definately a todo as it will require a USB to serial implementation in the PIC firmware)
  WIFI_PWR(1); // power on wifi and wait for startup
	DelayMs(200); // Check with analyser, but first few milliseconds have boot/debug data which we can ignore
	SerialOpen(WIFI_UART);
	if(!WaitWiFiStr("ready\r", 500))
	{
		WiFiError(1);
		return 0;
	}
	DelayMs(10);
  SerialWriteStr(WIFI_UART,"ATE0\r\n");			// ECHO OFF
	DelayMs(20);
 	SerialWriteStr(WIFI_UART,"AT+CWMODE=1\r\n");	  // set to Station (slave) mode, device will talk to router/host. Stored in ESP8266 EEPROM
	if(!WaitWiFiStr("OK\r", 500))
	{
		WiFiError(2);
		return 0;
	}
  DelayMs(10);
	SerialWriteStr(WIFI_UART,"AT+CWJAP=\"");	// Join access point (assume SSID and password already scanned and loaded in PIC EEPROM)
	SerialWriteStr(WIFI_UART,config.field.SSID);
	SerialWriteStr(WIFI_UART,"\",\"");
	SerialWriteStr(WIFI_UART,config.field.PASS);
	SerialWriteStr(WIFI_UART,"\"\r\n");
	if(!WaitWiFiStr("OK\r", 9000))	// Returns OK if connected to wifi (give very generous amount of time). Stored in ESP8266 EEPROM
	{
		WiFiError(3);
		return 0;
	}
	CloseWiFi();
	return 1;
}

uint16 QueryHost(uint8 FnCode, char * Field1, char *Field2, char *Field3, char *Field4) // returns 1 if OK and 0 if error
{
	uint16 DataLen = 0;
  uint8  Retry = 6;
	char 	 DataLenAsStr[6];
  uint8  connected,i,j,k;
	char	 prefix[4]	= { ASCII_SOH, FnCode, ASCII_STX, '\0' }; // null terminated prefix
	char	 postfix[5]	= { ASCII_ETX, 1, 1, ASCII_EOT, '\0' }; // null terminated postfix

	ESP_PGM(1);  // Turn off firmware update mode by setting to 1 (leave off. firmware update is definately a todo as it will require me to write a USB CDC)
  WIFI_PWR(1); // power on wifi and wait for startup

	CLS();
	LCDWriteStrAt(1,3,config.field.SSID);
	LCDWriteStrAt(1,5,config.field.HOST);
	LCDWriteStrAt(1,8,"Connect.");
	DelayMs(200); // Check with analyser first few milliseconds have boot/debug data which we can ignore
	SerialOpen(WIFI_UART);
	if(!WaitWiFiStr("ready", 500))	// on power up the ESP8266 should return "ready" within 500mS if not the module is probably stuffed
	{
		WiFiError(1);
		return 0;			// return error code 1 (worst possible error to have!)
	}

	DelayMs(20);
  if(!DEBUG) SerialWriteStr(WIFI_UART,"ATE0\r\n");			// ECHO OFF
	DelayMs(20);

	connected=0;
	while(Retry-- && !connected) // wait for AP connect
	{
		LCDWriteStrAt(14-Retry,8,".");
		SerialWriteStr(WIFI_UART,"AT+CWJAP?\r\n"); // Check if connected
		if(WaitWiFiStr(config.field.SSID, 1000)) connected=1;
	}

	if(!connected)
	{
		WiFiError(3);
		return 0;	// return error code
	}
	DelayMs(20);

	SerialWriteStr(WIFI_UART,"AT+CIPSTART=\"TCP\",\""); // Connect to TCP Port
	SerialWriteStr(WIFI_UART,config.field.HOST);
	SerialWriteStr(WIFI_UART,"\",2001\r\n");
	if(!WaitWiFiStr("CONNECT", 3000))
	{
		WiFiError(4);
		return 0;				// return error code
	}

	// now connected, so build transmit string. Remember 0x00 is NOT a valid ASCII char in our string as it will be perceived as the terminator!
	strcpy(WIFISendData,prefix);
	strcat(WIFISendData,Field1);
	strcat(WIFISendData,"\t");
	strcat(WIFISendData,Field2);
	strcat(WIFISendData,"\t");
	strcat(WIFISendData,Field3);
	strcat(WIFISendData,"\t");
	strcat(WIFISendData,Field4);
	strcat(WIFISendData,"\t");
	strcat(WIFISendData,postfix);
	DataLen = strlen(WIFISendData);
	itoa(DataLenAsStr, DataLen, 10); // convert length of the data to a base 10 num string for sending in wifi cmd

	// send the data to the host
	DelayMs(10);
	SerialWriteStr(WIFI_UART,"AT+CIPSEND=");
	SerialWriteStr(WIFI_UART,DataLenAsStr);
	SerialWriteStr(WIFI_UART,"\r\n");
	DelayMs(10);
	SerialWriteStr(WIFI_UART,WIFISendData);

	DataLen = WaitWiFiStr("\x3\x4", 3000); // wait response from server! should be  +IPD,<datalen>:msg\r\nOK\r\n, we wait for EOH EOT string

	CloseWiFi(); // all receiving done. no longer need wifi so power it off and save power

	if(DataLen==0) // ouch, nothing was received, server is up but its not talking
	{
  	WiFiError(5);
		return 0;
	}

	// clear our fields before populating - \0 is string terminator so can treat array elements like strings as req
	for(j=0; j<10; j++)
		for(k=0; k<64; k++)
      TABfield[j][k]='\0';

	i=0;
	while(WIFIRecData[i]!='\0' && WIFIRecData[i]!=ASCII_STX) // skip rec buffer to where tab delimeted data begins
		i++;

	if(WIFIRecData[i]=='\0')
	{
  	WiFiError(6);
		return 0;
	}

	i++;
	j=0;
	k=0;

	while(WIFIRecData[i]!='\0' && WIFIRecData[i]!=ASCII_ETX) // split reply into 10 string fields
	{
		if(WIFIRecData[i] == ASCII_TAB)
		{
			j++;
			k=0;
		}
		else
		{
			TABfield[j][k] = WIFIRecData[i];
		  k++;
		}
		i++;
	}

	return 1;
}
