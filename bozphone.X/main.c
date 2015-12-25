// Main program

#include "hardware.h"
#include "lcd.h"
#include "rfid.h"
#include "barcode.h"
#include "comms.h"
#include "bullseye.h"

void SetupWIFI() // get and save wifi parameters so device can connect to network
{
	uint8 i;
  uint8  SSID[16],password[16],HostIP[16];

	strcpy(BarcodeData,""); // clr (so we know if changed)

	while(1)
	{
		i = GetMenuItem("WIFI SETUP","Use Setup TAG","CLR Default","Exit","","");
		if(i==1)
    {  
      if(ReadRFIDBlock(1)) 
      {
        strcpy(SSID,RFIDData);              // SSID Stored in Mifare block 1 (zero terminated ascii string))
        if(ReadRFIDBlock(2))
        {
          strcpy(password,RFIDData);        // password Stored in Mifare block 2 (zero terminated ascii string)
          if(ReadRFIDBlock(4))
          {
            strcpy(HostIP,RFIDData);        // Broker, Host IP address Stored in Mifare block 4 (zero terminated ascii string)
            
            if(SetWIFI(SSID,password,HostIP)) // send settings and check for good write
              return;
          }
        }
      }
    }
		if(i==2)
    {
      strcpy(SSID,"CYBERNET");
      strcpy(password,"");
      strcpy(HostIP,"192.168.1.68");
      if(SetWIFI(SSID,password,HostIP)) return; // send settings and check for good write
    }    
		if(i==3) return;
  }  
}

void SetupMenu() // Set up menu
{
	uint8 i;

	while(1)
	{
	  i = GetMenuItem("TERMINAL SETUP","WiFi Setup","ESPUSB Fudge","Scan WiFi","Test Wifi","Exit");

  	if(i==1) SetupWIFI();

		if(i==2) // Open USB in CDC Mode passthough to ESP-01
    {
      SerialPurge(WIFI_UART);
      CLS();
      LCDWriteStrAt(0,4, "  USB <-> ESP01 ");
      LCDWriteStrAt(0,6, "   PROGRAMMIING   ");
      LCDWriteStrAt(0,10,"  PRESS KEY TO ");
      LCDWriteStrAt(0,12,"    CONTINUE    ");
      
      PORTSetPinsDigitalIn(IOPORT_C,  BIT_2 );
      PPSUnLock;					// Unlock PPS (Peripheral Pin Select) to allow PIN Mapping
      PPSOutput(4,RPC2,NULL); 
      PPSLock;						// lock PPS
      ESP_PGM(0);
      WIFI_PWR(1);
      i = WaitEvent(9999);
      CLS();
      WIFI_PWR(0);
      ESP_PGM(1);
  		DelayMs(500);
      WIFI_PWR(1);
      LCDWriteStrAt(0,4, "  USB <-> ESP01 ");
      LCDWriteStrAt(0,6, "   PASSTHROUGH   ");
      LCDWriteStrAt(0,10,"  POWER OFF TO ");
      LCDWriteStrAt(0,12,"     RESUME     ");
      while(1);
    }

		if(i==4) 
    {		
      if(CheckWIFI()) LCDInform("SUCCESS","Connection OK");
    }

		if(i==3) {	LCDInform("WARNING","Not Implemented");	}

		if(i==0 ||i==5) return;
	}
}

int main() // MAIN PROGRAM AND First Level Menu
{
	uint8 c,MenuSel;		// local var to hold result of which menu item we selected

	InitPIC32();			// initialise MPU
	CloseWiFi();			// Wifi powered Off
	CloseBarcode();		// Barcode powered Off
	PeripheralReset();
	InitLCD();				// initialise LCD
	CloseRFID();			// RFID off / sleep

	if(BrownOutReset()) // if the system crapped out previously, then just warn the user to charge the device and power off
	{
		LCDInform(" POWER! ","Please Charge!");
	  PWR_HOLD(0); // turn off power when above message acknowleged
	}

	while(1) // main program loop (do this until powered off or sleep)
	{
 	  MenuSel = GetMenuItem("READY","Scan Barcode","Scan RFID","Setup","Shutdown","");

		if(MenuSel==1) // SCAN barcode
		{
			if(ReadBarcode()>1) // we got a barcode! Barcodes are encoded with the first letter designating the barcode type (or obviously the system wont have a clue!)
			{
				if(BarcodeData[0]=='J') OldJobMenu(BarcodeData);
				else
				if(BarcodeData[0]=='X') NewJobMenu(BarcodeData);
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
        ReadRFIDBlock(1);
			}
		}

		if(MenuSel==3) // setup menu
		{
   		SetupMenu();
		}

		if(MenuSel==4) // power off
		{
		  PWR_HOLD(0); // remove power to CPU - if on battery power this will turn off power to CPU and effectively end the program here!
			CLS();
			if(ReadFloatAt(0,8,500,7))	LCDInform("Number was",KeypadData);
		}

	}
}