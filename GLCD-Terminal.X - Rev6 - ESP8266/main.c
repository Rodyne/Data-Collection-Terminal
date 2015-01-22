/* main loop of the GLCD Data collection terminal
 *
 * Note: if using mplabx then only add this file to the 'source files' of the project and keep the other files of the project in
 * the 'important files' alternatively put them in the 'source files' part but right click on their properties and exclude them
 * from the build. The reason for this is when you build the project it will compile source modules regardless of any ifndef and you
 * will end up with multiple definitions. There is probably a proper way to do this using headers if there is then please do it correctly
 * this works for me until somebody puts me right :-)
 */

#include "hardware.c"
#include "common-defs.h"

typedef struct // structure to put all program variables
{
  int32   Result32;
  int16   Result16;
  uint8   Result8, option, JobStatus;
  char    TempStr[16];
  char    JobID[16];
  char    LabID[16];
  char    AssetID[16];
  char    MovementID[16];
  char    NumResult[32];
  char    StrResult[32];
} Tdata;

Tdata data;

uint8 NVMWriteCfg() // Note if the WIFI Module does this then we do not need to do it!
{
	// we use the last page of flash 1024 bytes to store our config data, the start address of this last page is NVM_START then end is NVM_END
	// we use 48 bytes to store config so we can save 21 times before the page needs erasing. Note on erase all bytes are 0xff
	// Note NVMPtr will be set to our last address read so should be pointing to the next empty block
	uint8  i;
	if( (void*)(NVMPtr+sizeof(NVM)) >= NVM_END)  // check if used all flash. If so have to erase whole page and start again (max 1000 times!)
	{
		NVMErasePage(NVM_START);
		NVMPtr = NVM_START;
	}
	// copy HOSTIP[16] SSID[16] and PASS[16] to NVM[64]
	for(i=0; i<16; i++) NVM[i] = cfg.HOST[i];
	for(i=16; i<32; i++) NVM[i] = cfg.SSID[i-16];
	for(i=32; i<48; i++) NVM[i] = cfg.PASS[i-32];
	// copy NVM[64] to flash an integer (4 bytes) at a time
	for(i=0; i<16; i++)
	  NVMWriteWord( (void*) (NVMPtr+i) , (NVM[(i*4)+0] + NVM[(i*4)+1]*0x100 + NVM[(i*4)+2]*0x10000 + NVM[(i*4)+3] * 0x100000) );
	SoftReset(); // RESET
}

void NVMReadCfg() 	// retrieve last saved values from NVM (We dont have eeprom so using flash, and reflashing the pic32 will wipe the config)
{
	uint8 i;

	// set defaults in case nothing in NVM (use empty strings if live or real ones if testing)
	STRSET(cfg.HOST,"192.168.1.64");
	STRSET(cfg.SSID,"CYBERNET");
	STRSET(cfg.PASS,"");

	return;

  // Look for where NVM record data ends and erased flash begins, saved data is the bytes just before this
	NVMPtr = NVM_START;
	while( *NVMPtr != 0xffffffff && (void*)NVMPtr < NVM_END)
		NVMPtr++;

	NVMPtr = NVMPtr - 16;
	if( *(NVMPtr) != 0xffffffff) // looks like real data at NVMPtr position so retrieve stored values and overwrite defaults)
	{
		for(i=0; i<16; i++)
		{
			NVM[i*4]	  = *(NVMPtr+i);
			NVM[i*4+1]	= *(NVMPtr+i)/0x100;
			NVM[i*4+2]	= *(NVMPtr+i)/0x10000;
			NVM[i*4+3]	= *(NVMPtr+i)/0x1000000;
		}
		// copy NVM[64] tp IP[16] SSID[16] and PASS[16]
		for(i=0; i<16; i++) cfg.HOST[i] = NVM[i];
		for(i=16; i<32; i++) cfg.SSID[i] = NVM[i-16];
		for(i=32; i<48; i++) cfg.PASS[i] = NVM[i-32];
	}
}

void inform(char *title, char *m1, char *m2, char *m3, char *m4, char *m5)
{
	CLS();
	Pen(WHITE);
	LCDWriteStrAt(1,0,title);
	Pen(BLACK);
	LCDWriteStrAt(0,4,m1);
	LCDWriteStrAt(0,6,m2);
	LCDWriteStrAt(0,8,m3);
	LCDWriteStrAt(0,10,m4);
	LCDWriteStrAt(0,12,m5);
	beep(100);
	LCDWriteStrAt(2,14,"Press any key");
	WaitEvent(60,1800);
}


uint8 GetMenuItem(char *title, char *m1, char *m2, char *m3, char *m4, char *m5, uint16 TimeoutSeconds, uint16 PowerOffTimeout)
{
  // Very simple menu procedure up to 5 menu items m1..m5, return which selected (1..5) or 0 if cancelled
	uint8 selection = 0;
	uint8 key = 0;
	while(1)
	{
		// draw menu
		CLS();
		Pen(WHITE);
		LCDWriteStrAt(0,0,title);

		Pen(BLACK);
  	if(USBPowered)	LCDWriteCharAt(15,0,0x84);

		if(selection==1) Pen(WHITE); else Pen(BLACK); // highlight selected
	  LCDWriteStrAt(0,4,"1.");
		LCDWriteStrAt(3,4,m1);
		if(selection==2) Pen(WHITE); else Pen(BLACK); // highlight selected
		LCDWriteStrAt(0,6,"2.");
		LCDWriteStrAt(3,6,m2);
		if(selection==3) Pen(WHITE); else Pen(BLACK); // highlight selected
	  if(m3!="")	LCDWriteStrAt(0,8,"3.");
		LCDWriteStrAt(3,8,m3);
		if(selection==4) Pen(WHITE); else Pen(BLACK); // highlight selected
		if(m4!="")	LCDWriteStrAt(0,10,"4.");
		LCDWriteStrAt(3,10,m4);
		if(selection==5) Pen(WHITE); else Pen(BLACK); // highlight selected
		if(m5!="")	LCDWriteStrAt(0,12,"5.");
		LCDWriteStrAt(3,12,m5);
		Pen(BLACK); // normalise pen after use

		// added so we highlight the selection we pressed before returning ie user can see what menu item number they pressed 1..5
		if(key==K_1 || key==K_2 || key==K_3 || key==K_4 || key==K_5)
		{
			DelayMs(200);
			CLS();
			return selection;
		}

		key = WaitEvent(TimeoutSeconds,PowerOffTimeout); // wait up to 3 mins for keypress event and power down if none
		switch(key)
		{
			case K_UP	:	if(selection>1) selection--; break; // scroll up
			case K_DN	:	if(selection<5) selection++; break; // scroll down
			case K_OK	:	if(selection>0) return selection; break; // OK button
			case K_1	:	selection = 1; break;
			case K_2	:	selection = 2; break;
			case K_3	:	selection = 3; break;
			case K_4	:	selection = 4; break;
			case K_5	:	selection = 5; break;
			case K_CANCEL	:	return 0;  break;
		}
	}
}

void DoLogo() // display logo for about a second
{
  uint8 i;
	CLS();
	LCDSetContrast(0);
	BACKLIGHT(1);
	Pen(WHITE);
	LCDWriteStrAt(3,2,"          ");
	LCDWriteStrAt(3,3,"  ROVING  ");
	LCDWriteStrAt(3,5," DYNAMICS ");
	Pen(BLACK);
  LCDWriteStrAt(2,8,"Open Systems");
  LCDWriteStrAt(3,11,"rodyne.com");
	for(i=0; i<NORMAL_CONTRAST; i++) // bring up contrast (fade in)
	{
		LCDSetContrast(i);
		DelayMs(50);
	}
	DelayMs(900);
}

void SetupWIFI() // get and save wifi parameters so device can connect to network
{
	uint8 changed,i;

	while(1)
	{
		CloseWiFi();

		i = GetMenuItem(" WIFI SETUP ","Scan SSID","Scan Security","Scan HostID","Retry Connect","",200,120);

		if(i==0) return;

		changed = 0;

  	if(i==1) // SCAN/SET SSID
		{
			if(ReadBarcode())
			{
				STRSET(cfg.SSID,BarcodeData);
				changed = 1;
			}
		}

		if(i==2) // scan/set security
		{
			if(ReadBarcode())
			{
				STRSET(cfg.PASS,BarcodeData);
				changed = 1;
			}
		}

		if(i==3) // SCAN/SET Port/host
		{
			if(ReadBarcode())
			{
				STRSET(cfg.HOST,BarcodeData);
				changed = 1;
			}
		}

		if(i==4 || changed) // RESET Wifi SET SSID/Port/host
		{
			i = QueryHost(0x10,"2777","","","");
 		  CloseWiFi();
			if(i==0)
			{
				NVMWriteCfg();
  			LCDWriteStrAt(0,13,"SAVED");
				DelayMs(500);
				return;
			}
			LCDWriteStrAt(0,13,"ERR");
			LCDWriteCharAt(4,13,i+48);
			beep(800);
			DelayMs(2000);
		}

	}
}

// SYSTEM MENUS

uint8 JobMenu() // barcode data string should be a scan of the job order id (Remember barcode prefixed with J) return 0 if OK and error code otherwise
{
	if(QueryHost(0x10,BarcodeData,"","","")<5) return 1; //		Error, Cannot Connect to mobile server

	STRSET(data.JobID,BarcodeData); // set JobID
	STRSET(data.TempStr," Job ");
	STRCAT(data.TempStr,data.JobID); // concatinate job text so its readable on mobile

	if(!TABFIELD(data.TempStr,WIFIRecData,1)) return; // job status in field 1, will be "10" .. "90"

	data.JobStatus = atoi(data.TempStr);

	if(data.TempStr == "10") inform(" WARNING ",data.TempStr,"Is Not Issued!","Contact Office","","");
	else
	if(data.TempStr == "90") inform(" WARNING ",data.TempStr,"Has ended!","Contact Office","","");
	else
	{
		data.option = GetMenuItem(data.TempStr,"Scan Vessel","Update Job Status","","","",400,300);

		if(data.option==1) // scan vessel
		{
		}

	}
	return 0;
}

uint8 AssetMenu()
{
	data.option = GetMenuItem("Vessel Options","Scan Vessel","Update Job Status","","","",400,300);
}

uint8 StockMenu()
{
	data.option = GetMenuItem("Stock Options","Scan Vessel","Update Job Status","","","",400,300);
}

uint8 LabMenu()
{
	data.option = GetMenuItem("Lab Options","Scan Vessel","Update Job Status","","","",400,300);
}


int main()
{
	InitTerminal();

	DoLogo();
 	NVMReadCfg();

	if(RCON==3) inform(" POWER! ","Your Terminals","Battery Is Dead!","","","Please Charge"); // Oh No, a brown out!

	RCONbits.BOR = 0; // Reset brown out flag so we know if above is real when brown-out occurs

	while(1) // main program loop
	{
		// reset variables
		STRSET(data.JobID,"");
		STRSET(data.LabID,"");
		STRSET(data.AssetID,"");
		STRSET(data.MovementID,"");

		if(HostRetries>3 || WIFIRetries>3) SetupWIFI(); // problems communicating?

		// USBPowered variable set at init before we set SPI (as enabling USB uses an SPI pin and dispay wont work - need to investigate)
		// I'm sure it will be possible to remedy this when I eventually do a USB enabled version
		// anyway if USB powered remove the shutdown option and add the setup option as shutdown is not possible on USB power
		if(USBPowered)
	    data.option = GetMenuItem(" READY ","Scan Barcode","Scan RFID","Setup","","",400,120);
		else
	    data.option = GetMenuItem(" READY ","Scan Barcode","Scan RFID","Shutdown","","",400,120);

		if(data.option==1) // SCAN barcode
		{
			if(ReadBarcode()>1) // we got a barcode! Barcodes are encoded with the first letter designating the barcode type (or obviously the system wont have a clue!)
			{
				if(BarcodeData[0]=='J') JobMenu();
				if(BarcodeData[0]=='S' && BarcodeData[1]=='M') StockMenu();
				if(BarcodeData[0]=='A') AssetMenu();
				if(BarcodeData[0]=='L') LabMenu();
			}
		}

		if(data.option==2) // scn RFID tag
		{
			// to do, assign the terminal to user who logs in maybe?
		}

		if(data.option==3) // power off
		{
  		if(USBPowered)
				SetupWIFI();
			else
			  PWR_HOLD(0); // remove power to CPU
		}

	}
}
