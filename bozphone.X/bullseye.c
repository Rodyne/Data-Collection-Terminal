// Contains the menus and logic specific to the indevin bulls-i MRP program
// This code is specific to my client and wont mean much if you dont know the bulls-i program but may have similarities to other MRP systems

#include "hardware.h"
#include "lcd.h"
#include "rfid.h"
#include "barcode.h"
#include "functions.h"
#include "bullseye.h"
#include "comms.h"

Tdata dat; // local session data variables

void NewSession() 	// reset all session variables
{
	dat.MenuSel1=0;
	dat.MenuSel2=0;
	dat.MenuSel3=0;
	dat.OldStatus=0;
	dat.NewStatus=0;
	strcpy(dat.JobID,"");
	strcpy(dat.LabID,"");
  strcpy(dat.AssetID,"");
	strcpy(dat.MvtID,"");
	strcpy(dat.UserID,"");
	strcpy(dat.SMID,"");
	strcpy(dat.OldMetric,"");
	strcpy(dat.NewMetric,"");
}

void NewJobMenu(char *ID) // barcode data string should be a scan of the job order id (Remember barcode prefixed with J) return 0 if OK and error code otherwise
{
  uint8 MetricLen=0;
  
	NewSession(); // Init bulls-i session variables
  
	if(QueryHost("20",ID,"")>0) return; // cannot GET job instruction info from server (function will display reason/error)

  /* Query Returns 
   *  Field2 = Job ID
   *  Field3 = Metric description eg "Enter final dip"
   *  Field4 = Metric Type (0=RFID Scan, 1=Barcode scan, 2=INteger, 3=decimal, 4=time)
   *  Field5 = UOM Text (eg "cm")
   *  Field6 = Metric Min String
   *  Field7 = Metric Max String  
   */
    
	strcpy(dat.temp[1],"Job ");    // concatinate job descriptor text so its readable on mobile ie tempstr says something like "Job 72332"
	strcat(dat.temp[1],dat.JobID);	

	while(1)
	{
		CLS();
		LCDPen(WHITE);
		LCDWriteStrAt(0,0,"                ");
		LCDWriteStrAt(0,1,"                ");
		LCDWriteStrAt(1,1,dat.temp[1]);

    if(field[4]=="0") // requires RFID Scan
    {  
      LCDWriteStrAt(0,5,"Waiting Scan..");
     	if (ReadRFIDUID()) // get UID
			{
        LCDWriteStrAt(0,5,uid.SerialNo);
        strcpy(dat.NewMetric,uid.SerialNo);
      }
    }

    if(field[4]=="1") // requires barcode scan
    {  
      LCDWriteStrAt(0,5,"Scan Barcode..");
     	if(ReadBarcode()>1)
			{
        LCDWriteStrAt(0,5,BarcodeData);
        strcpy(dat.NewMetric,BarcodeData);
      }
    }

    if(field[4]=="2" || field[4]=="3" || field[4]=="4") // requires keyboard entry
    {  
      LCDWriteStrAt(0,5,field[3]); // metric short code (text prompt!)
      LCDWriteStrAt(0,7,"Value");
      LCDWriteStrAt(7,7,dat.NewMetric);
      LCDWriteStrAt(0,11,"OK to Save or");
      LCDWriteStrAt(0,13,"C to Cancel");

      if(field[4]=="2") // requires integer entered on keyboard
      {  
        LCDWriteStrAt(0,9,"Enter Number.");
        MetricLen=ReadNumberAt(9,7,30000,7);
      }

      if(field[4]=="3") // requires decimal entered on keyboard
      {  
        LCDWriteStrAt(0,9,"Enter Decimal.");
        MetricLen=ReadFloatAt(9,7,30000,7);
      }

      if(field[4]=="4") // requires decimal entered on keyboard
      {  
        LCDWriteStrAt(0,9,"Enter Time.");
        MetricLen=ReadFloatAt(9,7,30000,5);
      }
      strcpy(dat.NewMetric,KeypadData);
    }
    
    if(QueryHost("21",ID,dat.NewMetric))
      if(field[1]>0)
        LCDInform("Error","Metric NOT set");

  }  
}

void OldJobMenu(char *ID)
{
	dat.MenuSel1 = GetMenuItem("Job Options","","","","","");
  LCDInform("Error","NOT Supported");
}

void AssetMenu(char *ID)
{
	dat.MenuSel1 = GetMenuItem("Asset Options","","","","","");
  LCDInform("Error","NOT Supported");
}

void StockMenu(char *ID)
{
	dat.MenuSel1 = GetMenuItem("Stock Options","Scan Vessel","Update Job Status","","","");
  LCDInform("Error","NOT Supported");
}

void LabMenu(char *ID)
{
	dat.MenuSel1 = GetMenuItem("Lab Options","Scan Vessel","Update Job Status","","","");
  LCDInform("Error","NOT Supported");
}

void UserMenu(char *ID)
{
	dat.MenuSel1 = GetMenuItem("User Preferences","Scan Vessel","Update Job Status","","","");
  LCDInform("Error","NOT Supported");
}