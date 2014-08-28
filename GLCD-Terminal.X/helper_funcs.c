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

#define STACKMAX 200   // Max size of stack for user program script (APP)
#define APPMAX   10000  // Max size of program (byte) code - dont use all chip ram, need to have some for 'c' stack/heap

uint8   app[APPMAX];   // user source (byte) application code downloaded from server on first boot
Tstack  stack[STACKMAX]; // script program stack

str30   tempstr1,tempstr2,resultstr1,resultstr2; // temporary working fixed length char arrays (strings) for expression evaluation etc
str30   SSID,Security,Pwd,PortHost;

uint8		loaded=0;


/*
 * String helper functions for fixed length pstrings (use pstrings where possible to reduce exceptions and increase speed)
 *
 */

void SetPStr(str30 str, char * ToVal) // convert a c char pointer (String) to fixed length string and adds str length to last element
{
  uint8 i = 0;

  while(i<STRMAX && *ToVal!='\0')
  {
    str[i++]=*ToVal++;
		str[i]='\0';
  }
  str[STRLEN]=i;  // element holds string length
}

void STRMID(str30 src, uint8 frompos, uint8 len) // same as MID() but implemented as a procedure and result is copied in to resultstr1
{
	SetPStr(resultstr1,"");
	uint8 i=0;
	while(i<len && i<STRMAX && frompos>0 && frompos+i<STRMAX) // check it to death so we dont get exceptions!
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

uint8 input(uint8 x, uint8 y, uint8 maxlen, uint8 decimal) // return strlength if user presses ok return 0 if timeout or cancel
{
	uint32  timeout = counter1+prg.InputTimeoutMs; // set timeout
	uint8   dx,key;
	uint8   len=0;

	SetPStr(prg.KeypadStr,""); // clr keyinput buffer

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

	WriteWIFIDATA(data,"");
	ctimeout=counter1+WIFITIMEOUTMS;
  while(!SerialAvailable(WIFI_UART) && counter1<ctimeout); // wait for response
	return SerialAvailable(WIFI_UART); // return true if server has responded to request
}

uint8 STRPOS(char *src, char *substr) // same as pacsal pos() fn returns if the substr was in str and 0 if no match
{
	// buggy
/*	uint8 cc=0;
	uint8 pos=0;
	char *match = substr;
  while(*src!='\0') // keep in loop until end of string or match
  {
		cc++;
		if(*src++ == *match) // chars match
		{
			if(pos==0) pos=cc; // on first match set pos where it occured
			match++;
		}
		else // chars dont match so reset everything
		{
			pos=0;
		  match=substr; // go to next char if match or reset to begining if match fails
		}
		if(*match='\0') return pos;
  }*/
	return 0; // timeout before match
}

uint8 GetLinkStatus() // return 1 if up 0 if down -1 if fail // tempstr1 holds the string returned from wifimodule
{
	char c;
	uint8 SOT=0; // assert once we re at the start of text we want to return
	uint8 i=0;
	char timeout=200;

	if(!WriteWIFICMD("AT+WSLQ","",0)) return -1; // turn off fall over to access point mode

	SetPStr(tempstr1,"");

  while(timeout--) // keep in loop until timeout or match/fail
  {
    if(SerialAvailable(WIFI_UART))
		{
			c=SerialRead(WIFI_UART);
			if(SOT)
			{
			  if(c=='\r' || c=='\n') break; // end of string
			  tempstr1[i++]=c;
			  tempstr1[i]='\0';
			}
			if(c==' ') SOT=1;
    }
    DelayMs(1);
  }
	return SOT;
}

uint8 WriteWifiCfg() // return error code or 0 if all good
{
	CLS();
	WiFiInit(0);
	LCDGotoXY(0,2);
	if(! WriteWIFICMD("AT+MDCH=OFF","",0)) return 1; // turn off fall over to access point mode
	if(! WriteWIFICMD("AT+WMODE=STA","",0) ) return 2; // set for station mode (NOT an access point)
	if(! WriteWIFICMD("AT+WANN=DHCP","",0)) return 3; // set to use DHCP
	if(! WriteWIFICMD("AT+WSSSID=",SSID,0)) return 4; // set ssid
	if(! WriteWIFICMD("AT+WSKEY=",Security,0)) return 5; // set security
	if(! WriteWIFICMD("AT+NETP=TCP,CLIENT,",PortHost,0)) return 6; // set host
	if(! WriteWIFICMD("AT+Z","",1)) return 8; // reset/reconnect
	return 0;
}

void SetupWIFI()
{
  uint8 k=0;
	uint8 s=0;
	uint8 changed=0;
	uint32 tm;

	SetPStr(SSID,"");
	SetPStr(Security,"");
	SetPStr(Pwd,"");
	SetPStr(PortHost,"");

	while(1)	// Keep in this loop until connected to an access point
	{
  	CLS();
		Pen(WHITE);
		LCDWriteStrAt(2,0," WIFI SETUP ");
		Pen(BLACK);

	  LCDWriteStrAt(0,6,"1. Scan SSID");  // SSID
		LCDWriteStrAt(0,8,"2. Scan Security"); // Security (digit 1 = type (0=Open,1=WEP,2=WEP2,3=WPN,4=WPN2) remainder is pass phrase
		LCDWriteStrAt(0,10,"3. Scan HostID"); // Server
		LCDWriteStrAt(0,12,"4. Defaults"); //

		PurgeKeyBuf();
  	while(!KeyPressed()) // wait here for key press
		{
			s=GetLinkStatus();
			LCDWriteStrAt(0,3,"LINK             ");
			if(s==-1)
				LCDWriteStrAt(5,3,"ERR!");
			else
			if(s==0)
				LCDWriteStrAt(5,3,"DOWN!");
			else
				LCDWriteStrAt(5,3,tempstr1);
			tm=counter1+2000*10;
			while(tm<counter1 && !KeyPressed())
			  DelayMs(10);
		};
		k=ReadKey();

		if(k==K_CANCEL) return; // just leave if cancel pressed

		if(k==K_1) // SCAN/SET SSID
		{
  		Pen(WHITE);
  		LCDWriteStrAt(0,6,"1. Scan SSID"); // Server
  		Pen(BLACK);
			if(ReadBarcode())	SetPStr(SSID,prg.BarcodeStr);
			if(WriteWIFICMD("AT+WSSSID=",SSID,0)) changed=1;
		}

		if(k==K_2) // scan/set security
		{
  		Pen(WHITE);
   		LCDWriteStrAt(0,8,"2. Scan Security"); // Server
  		Pen(BLACK);
			if(ReadBarcode())
			{
				// first char of barcode is '0'..'4' contains security info
				if(prg.BarcodeStr[1]=='0') SetPStr(Security,"OPEN,NONE,");
				if(prg.BarcodeStr[1]=='1') SetPStr(Security,"WPAPSK,TKIP,");
				if(prg.BarcodeStr[1]=='2') SetPStr(Security,"WPAPSK,AES,");
				if(prg.BarcodeStr[1]=='3') SetPStr(Security,"WPA2PSK,TKIP,");
				if(prg.BarcodeStr[1]=='4') SetPStr(Security,"WPA2PSK,AES,");
				{
					// remainder of barcode is pwd so use mid proc to copy into the resultstr1 variable then set it as password
					// ! to do.. scramble the barcode/password :-(
				  STRMID(prg.BarcodeStr,2,16);
				  SetPStr(Pwd,resultstr1);
				}
			}
			if(WriteWIFICMD("AT+WSSSID=",SSID,0)) changed=1;
		}

		if(k==K_3) // SCAN/SET Port/host
		{
  		Pen(WHITE);
  		LCDWriteStrAt(0,10,"1. Scan HostID"); // Server in format 667,192.168.0.5
  		Pen(BLACK);
			if(ReadBarcode())
				SetPStr(PortHost,prg.BarcodeStr);
		}

		if(k==K_4) // set all params to defaults
		{
  		Pen(WHITE);
   		LCDWriteStrAt(0,12,"4. Defaults"); //
  		Pen(BLACK);
			SetPStr(SSID,"CYBERNET");
			SetPStr(Security,"OPEN,NONE");
			SetPStr(Pwd,"");
			SetPStr(PortHost,"2000,192.168.0.5");
  		k=WriteWifiCfg();
			if(k)
 	  	  DelayMs(5000);
			else
				changed=1;
		}

	  if(WiFiInit(1)) // got a link
		{
		  if(changed) // if change was made then write it out
			{
      	if(WriteWIFICMD("AT+CFGTF","",0))
					return; // saved to EEPROM OK
			}
			else
				return; // link ok no save required (must have come up of its own accord)
		}
	}
	CLS();
}

void load() 	// download byte code app from server
{
 //
}

void AppCrash(char * message)
{
	CLS();
	Pen(WHITE);
	LCDWriteStrAt(2,0," APP FAIL! ");
	Pen(BLACK);
	LCDWriteStrAt(0,4,message);
	LCDWriteStrAt(0,6,"at line");
	LCDWriteStrAt(6,6,"xxx");
	LCDWriteStrAt(2,9,"Press any key");
	LCDWriteStrAt(1,11,"to restart app");
 	while(!KeyPressed()); // wait here for key press
  ResetProgramVars();
}

void DoLogo() // takes about 2 seconds!
{
  uint8 i;
	CLS();
	LCDSetContrast(0);
	Brightness=10;
	Pen(WHITE);
	LCDWriteStrAt(3,2,"          ");
	LCDWriteStrAt(3,3,"  ROVING  ");
	LCDWriteStrAt(3,5," DYNAMICS ");
	Pen(BLACK);
	 LCDWriteStrAt(3,9,"rodyne.com");
	LCDWriteStrAt(2,12,"OPEN SOURCE");
	LCDWriteStrAt(3,14,"HARDWARE.");
	for(i=0; i<NORMAL_CONTRAST; i++) // fade logo into view
	{
		LCDSetContrast(i);
		DelayMs(100);
	}
	DelayMs(500);
}


#endif