/* Helper functions and screens for the GLCD Data collection terminal
 *
 * Contains screen and window functions as well as pre-defined screens
 * such as the setup and keyboard string input screen
 *
 */

#ifndef HELPER_FUNCS_C
#define HELPER_FUNCS_C

#include "hardware.c"
#include "common-defs.h"

#define STACKMAX  200   // Max size of stack for user program script (APP)
#define APPMAX   10000  // Max size of program (byte) code - dont use all chip ram, need to have some for stack/heap

uint8   app[APPMAX];   // user source (byte) application code downloaded from server on first boot
Tstack  stack[STACKMAX]; // script program stack

pstring tempstr1,tempstr2,resultstr1,resultstr2; // temporary working fixed length char arrays (strings) for expression evaluation etc
uint8		loaded=0;


/*
 * String helper functions for fixed length pstrings (use pstrings where possible to try and prevent exceptions which would crash terminal)
 *
 */

void SetPStr(pstring str, char * ToVal) // convert a c char pointer (String) to fixed length string and adds str length to last element
{
  uint8 i = 0;

  while(i<PSTRMAX && *ToVal!='\0')
  {
    str[i++]=*ToVal++;
		str[i]='\0';
  }
  str[STRLEN]=i;  // element holds string length
}

void MID(pstring src, uint8 frompos, uint8 len) // same as MID() but implemented as a procedure and result is copied in to resultstr1
{
	SetPStr(resultstr1,"");
	uint8 i=0;
	while(i<len && i<PSTRMAX && frompos>0 && frompos+i<PSTRMAX) // check it to death so we dont get exceptions!
	{
		resultstr1[i]=src[frompos+i];
		i++;
	}
	resultstr1[i]='\0';
  resultstr1[STRLEN]=i;  // element holds string length
}

/*
 * Misc functions used by parser
 *
 */

void ResetProgramVars() // initializes all prg variables (generally on user app reset)
{
	prg.PC=100; // start running user code at line 100 of the app[] array
	prg.SP=0;
	prg.ErrorCode=0;
	SetPStr(prg.RFIDStr,"");
	SetPStr(prg.BarcodeStr,"");
	SetPStr(prg.KeypadStr,"");
}

void SaveForm() // saves a copy of the screen char map in case we need to overwrite and restore
{
  uint8 x,y;
	for(y=0; y<prg.MaxCursorY; y=y+2)
  	for(x=0; x<prg.MaxCursorX; x++)
			SaveScr[x][y]=CurScr[x][y];
}

void RestoreForm() // restores copy of screen as saved above
{
  uint8 x,y;
	for(y=0; y<prg.MaxCursorY; y=y+2)
  	for(x=0; x<prg.MaxCursorX; x++)
			LCDWriteCharAt(x,y,SaveScr[x][y]);
}

uint8 input(uint8 x, uint8 y, uint8 maxlen, uint8 decimal) // return strlength if user presses ok return 0 if timeout or cancel
{
	uint32  timeout = counter1+prg.InputTimeoutMs; // set timeout
	uint8   dx,key;
	uint8   len=0;

	SetPStr(prg.KeypadStr,""); // clr keyinput buffer

	SaveForm(); // Save current screen just in case we want to restore when finished

	// display blank input area
	for(dx=x;dx<maxlen-x;dx++)
	  LCDWriteCharAt(dx,y,'_');
	
	// get input string or timeout
	dx=x;
	while(1)
	{
		if(timeout>counter1) return 0; // timed out
		if(KeyPressed())
		{
			key=ReadKey();
			if (key>='0' && key<='9')
			{
				if(len<maxlen)
				{	
				  prg.KeypadStr[len++]=key; // 0 thro 9 go straight to input
  				LCDWriteCharAt(dx+len,y,key); // write it out to screen
				}
				else
					beep(400); // string too long!
			}
			if(decimal && key==K_DECIMAL) // decimal check is not first char
			{
				if(len<maxlen && len>0)
				{	
  				prg.KeypadStr[len++]=key; // only allow decimal key if decimal set
  				LCDWriteCharAt(dx+len,y,'.'); // write it out to screen
				}
				else
					beep(50);
			}
			if(key==K_DEL && len>0)
			{
				len--; // delete last input char
				LCDWriteCharAt(dx+len,y,'_'); // delete it on screen (overwrite with underscore string)
			}
			if(key==K_CANCEL) return 0; // cancelled
			if(key==K_OK)
			{
				if(len==0 || prg.KeypadStr[len-1]==K_DECIMAL) // input must be 1 char or greater and last char cannot be a decimal!
					beep(50);
				else
				{
					// all good (I think!)
					prg.KeypadStr[len]=len;
					prg.KeypadStr[STRLEN]=len;
				  return len; // all good
				}
			}
	      
		}
	}
}

uint8 SendHost(char * data) // send a packet of data to host and wait for reply
{
	uint8 ctimeout;

	if(!WIFI_LINK()) // no connection
		if(!WiFiInit(0)) // try quick connection
			WiFiInit(1);   // try longer to wait for connection

	if(!WIFI_LINK()) return 0; // no wifi connection cannot send to host

	WriteWIFI(DATA,data,"");
	ctimeout=counter1+WIFITIMEOUTMS;
  while(!SerialAvailable(WIFI_UART) && counter1<ctimeout); // wait for response
	return SerialAvailable(WIFI_UART); // return true if server has responded to request
}



int SetupWIFI()
{

  uint8   k,success;
  uint32  timeout;

	while(1)	// Keep in this loop until connected to an access point
	{
  	CLS();
		Pen(WHITE);
		LCDWriteStrAt(2,0," WIFI SETUP ");
		Pen(BLACK);

		if(!WIFI_LINK())
	    LCDWriteStrAt(0,3,"No Connection!");
		else
	    LCDWriteStrAt(0,3,"No Server!");

	  LCDWriteStrAt(0,6,"1. Scan CFG1");  // SSID
		LCDWriteStrAt(0,8,"2. Scan CFG2"); // Security (digit 1 = type (0=Open,1=WEP,2=WEP2,3=WPN,4=WPN2) remainder is pass phrase
		LCDWriteStrAt(0,10,"3. Scan CFG3"); // Server

  	while(!KeyPressed()); // wait here for key press

		InitBarcodeScan();

		k=ReadKey();

		if(k==0x49) // SCAN/SET SSID
			if(ReadBarcode())
				success=WriteWIFI(CMD,"AT+WSSSID=",prg.BarcodeStr);

		if(k==0x50) // scan/set security
			if(ReadBarcode())
			{
				if(prg.BarcodeStr[1]=='0') SetPStr(tempstr1,"AT+WSKEY=OPEN,NONE,");
				if(prg.BarcodeStr[1]=='1') SetPStr(tempstr1,"AT+WSKEY=WPAPSK,TKIP,");
				if(prg.BarcodeStr[1]=='2') SetPStr(tempstr1,"AT+WSKEY=WPAPSK,AES,");
				if(prg.BarcodeStr[1]=='3') SetPStr(tempstr1,"AT+WSKEY=WPA2PSK,TKIP,");
				if(prg.BarcodeStr[1]=='4') SetPStr(tempstr1,"AT+WSKEY=WPA2PSK,AES,");
				MID(prg.BarcodeStr,2,16);
				success=WriteWIFI(CMD,tempstr1,resultstr1);
			}

		if(k==0x51) // scan/set server host
			if(ReadBarcode())
			{

				success=WriteWIFI(CMD,"AT+WSSSID=",prg.BarcodeStr);
			}

		if(success) // If we successfully wrote the barcode to the wifi module then save it as the default and restart it to init connection
			if(WriteWIFI(CMD,"AT+CFGTF",""))
				if(!WiFiInit(0)) // try quick connection
					WiFiInit(1);   // try longer to wait for connection

		if(SendHost("PINGBACK")) return; // exit when host is up and talking!

	}
	CLS();

	while(1)
	{
		while(SerialAvailable(WIFI_UART))
		{
			LCDWriteChar(SerialRead(WIFI_UART));
		}
		if(ReadBarcode())
		{
			LCDWriteChar(SerialRead(WIFI_UART));
		}
	}

}

void load() 	// download byte code app from server
{
 //
}

void CrashAndBurn(char * message)
{
	CLS();
	Pen(WHITE);
	LCDWriteStrAt(2,0," APP FAIL! ");
	Pen(BLACK);
	LCDWriteStrAt(0,4,message);
	LCDWriteStrAt(0,6,"at line");
	LCDWriteStrAt(6,6,"xxx");
	LCDWriteStrAt(1,9,"Press any key");
	LCDWriteStrAt(2,11,"to Re-Start");


}


#endif