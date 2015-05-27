// Main program

#include "hardware.h"
#include "lcd.h"
#include "rfid.h"
#include "barcode.h"
#include "comms.h"
#include "bulls-i-menus.h"


void SetupWIFI() // get and save wifi parameters so device can connect to network
{
	uint8 i;

	BarcodeData[0]='\0'; // clr (so we know if changed)

	while(1)
	{
		i = GetMenuItem("WIFI SETUP","Scan Control TAG","Retry Connect","Exit","","");

		if(i==0 || i==5) return;

  	if(i==1) // SCAN/SET SSID
		{
			if(ReadRFIDBlock(4)) strcpy(config.field.SSID,RFIDData);
			if(ReadRFIDBlock(5)) strcpy(config.field.HOST,RFIDData);
			if(ReadRFIDBlock(6)) strcpy(config.field.PASS,RFIDData);
		}

		if(i==1 || i==2) // RESET Wifi SET SSID/Port/host and save to NVM
		{
			if(ConfigureESP8266()==0)
			{
				i = QueryHost(0x10,"2777","","","");
				if(i>2)
				{
					if(i==1)  SaveConfig(); // only save if changed
					LCDWriteStrAt(0,6,"All OK!"); // display all good
					DelayMs(500);
					return;
				}
			}
			beep(800);
			DelayMs(5000);
		}

	}
}

void SetupMenu()
{
	uint8 i;

	while(1)
	{
	  i = GetMenuItem("TERMINAL SETUP","Wi-Fi Setup","Update Flash","Scan WiFi","Test Wifi","Exit");

  	if(i==1) SetupWIFI();

		if(i==2) {	LCDInform("WARNING","Not Implemented");	}

		if(i==3) {	LCDInform("WARNING","Not Implemented");	}

		if(i==4)
		{
			i = QueryHost(0x10,"2777","","","");
			if (i>0)
			{
				LCDInform("TEST WiFi","Pass OK");
				DelayMs(2000);
			}
		}

		if(i==0 ||i==5) return;
	}
}

// MAIN PROGRAM AND First Level Menus

int main()
{
	uint8 MenuSel;		// local var to hold result of which menu item we selected

	InitPIC32();			// initialise MPU
	CloseWiFi();			// Wifi powered Off
	CloseBarcode();		// Barcode powered Off
	PeripheralReset();
	InitLCD();				// initialise LCD
	CloseRFID();			// RFID off / sleep

/*	while(USBPowered) // If we are USB powered then wait here just showing the charging/logo with power off so that we turn off when unplugged
	{
		DelayMs(500);
    LCDWriteStrAt(4,14,"CHARGING");
		DelayMs(2000);
    LCDWriteStrAt(4,14,"        ");
		PWR_HOLD(0);
	}*/

	if(BrownOutReset()) // if the system crapped out previously, then just warn the user to charge the device and power off
	{
		LCDInform(" POWER! ","This Terminals\rBattery Is Dead!\r\rPlease Charge");
	  PWR_HOLD(0); // turn off power when above message acknowleged
	}

	ReadConfig(); // read in wifi settings (if previously saved)

	while(1) // main program loop (do this until powered off or sleep)
	{
    if(config.field.UserName[0]=='\0')
			 MenuSel = GetMenuItem("READY","Scan Barcode","Scan RFID","Login","Setup","Shutdown");
		else
			MenuSel = GetMenuItem(config.field.UserName,"Scan Barcode","Scan RFID","Logout","Setup","Shutdown");

		if(MenuSel==1) // SCAN barcode
		{
			if(ReadBarcode()>1) // we got a barcode! Barcodes are encoded with the first letter designating the barcode type (or obviously the system wont have a clue!)
			{
				if(BarcodeData[0]=='J') JobMenu(BarcodeData);
				else
				if(BarcodeData[0]=='S' && BarcodeData[1]=='M') StockMenu(BarcodeData);
				else
				if(BarcodeData[0]=='A') AssetMenu(BarcodeData);
				else
				if(BarcodeData[0]=='L') LabMenu(BarcodeData);
				else
  				LCDInform("UID",BarcodeData);
			}
		}

		if(MenuSel==2) // scan RFID tag
		{
			CLS();
      LCDWriteStrAt(1,6,"SCAN RFID TAG");
  		if (ReadRFIDUID()) // get UID
			{
				LCDInform("UID",uid.SerialNo);
				DelayMs(9999);
			}
		}

		if(MenuSel==3) // login/logout
		{
      if(config.field.UserName[0]=='\0')
				config.field.UserName[0]='\0';
			else
			{
				CLS();
        LCDWriteStrAt(4,6,"SCAN RFID TAG");
	  		if (ReadRFIDUID()) // get UID
				{
					// check UID and get username and user type
				}
			}
		}

		if(MenuSel==4) // setup menu
		{
   		SetupMenu();
		}

		if(MenuSel==5) // power off
		{
		  PWR_HOLD(0); // remove power to CPU - if on battery power this will turn off power to CPU and effectively end the program here!
		}

	}
}