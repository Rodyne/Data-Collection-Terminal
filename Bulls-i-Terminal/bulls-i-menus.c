// Contains the menus and logic specific to the indevin bulls-i MRP program
// This code is specific to my client and wont mean much if you dont know the bulls-i program but may have similarities to other MRP systems

#include "hardware.h"
#include "lcd.h"
#include "rfid.h"
#include "barcode.h"
#include "comms.h"
#include "bulls-i-menus.h"

Tdata dat; // local session data variables


void NewSession() 	// reset all session variables
{
	dat.MenuSel1=0;
	dat.MenuSel2=0;
	dat.MenuSel3=0;
	dat.JobStatus=0;
	dat.FnCode=0;
	strcpy(dat.JobID,"");
	strcpy(dat.LabID,"");
  strcpy(dat.AssetID,"");
	strcpy(dat.MovementID,"");
	strcpy(dat.UserKey,"");
	strcpy(dat.SMID,"");
	strcpy(dat.JobStatusStr,"");
	strcpy(dat.VesselStatusStr,"");
	strcpy(dat.TempStr,"");
	strcpy(dat.TempStr2,"");
}

void JobMenu(char *ID) // barcode data string should be a scan of the job order id (Remember barcode prefixed with J) return 0 if OK and error code otherwise
{
	NewSession(); // Init bulls-i session variables
	if(QueryHost(0x10,ID,"","","")<5) return; // cannot get job info from server (may also return with nothing if illegal job no)

	StrCpySubStr(dat.JobID,ID,1,9); // set JobID (minus first character which is a J)
	strcpy(dat.TempStr,"Job ");
	strcat(dat.TempStr,dat.JobID);		 // concatinate job descriptor text so its readable on mobile ie tempstr says something like "Job 72332"
	dat.JobStatus = atoi(TABfield[2]);

	if(TABfield[0][0]=='0') // looks like job scanned in is not editable status ie closed or not yet issued
	{
  	strcat(dat.TempStr," is not in\ran updateable\rstatus.\r\rContact Office.");
		LCDInform(" ERROR ",dat.TempStr);
		return;
	}

	while(1)
	{
		// reset asset/movement variables
		strcpy(dat.AssetID,"");
		strcpy(dat.MovementID,"");

		// give different menu options to user depending upon job status (power off after 5 mins with no selection)
		if(dat.JobStatus==20)
			dat.MenuSel1 = GetMenuItem(dat.TempStr,"Scan Vessel","Start Job","Exit","","");
		else
		if(dat.JobStatus==30)
			dat.MenuSel1 = GetMenuItem(dat.TempStr,"Scan Vessel","Sign-On Job","Exit","","");
		else
		if(dat.JobStatus==60)
			dat.MenuSel1 = GetMenuItem(dat.TempStr,"Scan Vessel","End Job","Exit","","");
		else
		if(dat.JobStatus==70)
			dat.MenuSel1 = GetMenuItem(dat.TempStr,"Scan Vessel","Sign-off Job","Exit","","");
		else
			dat.MenuSel1 = GetMenuItem(dat.TempStr,"Scan Vessel","View Job","Exit","","");

		if(dat.MenuSel1==1) // scan vessel
		{
			if(ReadBarcode()>1) // we got a barcode! Barcodes are encoded with the first letter designating the barcode type (or obviously the system wont have a clue!)
			{
				if(BarcodeData[0]=='A') // Asset is prefixed with a 'A'
				{
					StrCpySubStr(dat.AssetID,BarcodeData,1,9); // get/set AssetID (minus first character which is a A)

					if(QueryHost(0x12,dat.JobID,dat.AssetID,"","")<5) // Get movement ID
						LCDInform(" Warning ","Vessel details\rcannot be read\rfrom network.\r\rTry Again");

					strcpy(dat.AssetID,"Vessel ");
					strcat(dat.TempStr,dat.AssetID); // concatinate vessel description text so its readable on mobile eg tempstr reads like "Vessel A1234"

					// create menu for vessel options. Note not all vessel options will be applicable, need to check vesselstatus word field
    			dat.MenuSel2 = GetMenuItem(dat.TempStr,"Initial Dip","Final Dip","Additions","Exit","");


				}
				else
					LCDInform("ERROR","This is not a\rVessel barcode!");
			}
		}

		if(dat.MenuSel1==2) // view/change status
		{
			CLS();
			LCDWriteStrAt(0,0,dat.TempStr);
			LCDWriteStrAt(0,3,TABfield[3]);
			LCDWriteStrAt(0,5,TABfield[2]);
			DelayMs(2000);

			dat.MenuSel2=0; // default is dont do menu, oterwise we have 4 statuses the user can update at, do a different confirmation for each
			// Tabfield[0][0] should be 2 for update mode
  		if(TABfield[0][0]=='2' && dat.JobStatus==20) dat.MenuSel2 = GetMenuItem("Confirm Start","Yes","No","","","");
  		if(TABfield[0][0]=='2' && dat.JobStatus==30) dat.MenuSel2 = GetMenuItem("Confirm Sign-On","Yes","No","","","");
  		if(TABfield[0][0]=='2' && dat.JobStatus==60) dat.MenuSel2 = GetMenuItem("Confirm End","Yes","No","","","");
  		if(TABfield[0][0]=='2' && dat.JobStatus==70) dat.MenuSel2 = GetMenuItem("Confirm Sign-Off","Yes","No","","","");

			if(dat.MenuSel2==1) // user has confirmed with Yes Option so send appropriate fnCode to server to change status on MRP system
			{
				if(dat.JobStatus==20) dat.FnCode=0x80;
				if(dat.JobStatus==30) dat.FnCode=0x81;
				if(dat.JobStatus==60) dat.FnCode=0x82;
				if(dat.JobStatus==70) dat.FnCode=0x83;
				if(QueryHost(dat.FnCode,BarcodeData,config.field.PASS,"","")>5)
				{
					LCDInform("Information","Update OK");
					return; // return from job menu with all good
				}
				LCDInform(" ERROR ","Cannot Connect\rwith Server..\r\rRetry may work!");
			}
		}

		if(dat.MenuSel1==3 || dat.MenuSel1==0) return; // time out or user exit
  }
}

void AssetMenu(char *ID) // Get here if you first scan from root menu is an asset
{
	uint8 i;

	if(QueryHost(0x10,ID,"","","")<5) return; // cannot get asset info from server

	StrCpySubStr(dat.AssetID,ID,1,9); // set AssetID (minus first character which is a A)
	strcpy(dat.TempStr,"Vessel ");
	strcat(dat.TempStr,TABfield[2]);		 // concatinate vessel descriptor text so its readable on mobile ie tempstr says something like "Vessel A1234"
	strcpy(dat.VesselStatusStr,TABfield[1]);

	while(1);
	{
		if(TABfield[3][0]=='0')
	    dat.MenuSel1 = GetMenuItem(dat.TempStr,"View Status","Cleaning","Quarantine","Location","Exit");
		else
	    dat.MenuSel1 = GetMenuItem(dat.TempStr,"View Status","SCADA","Quarantine","Location","Exit");

	  if(dat.MenuSel1==0 || dat.MenuSel1==5) return;

	  if(dat.MenuSel1==1) // view vessel status (just spit out the relevant facts for this vessel)
		{
			CLS();
			LCDWriteStrAt(0,0,dat.TempStr);
			LCDWriteStrAt(0,3,"Status:");
		 	if(dat.VesselStatusStr[2]=='1') LCDWriteStrAt(0,3,"Quarantined!");
			LCDWriteStrAt(0,5,"Volume:");
			if(TABfield[3][0]=='0')
			{
				LCDWriteStrAt(9,2,"Free");
				LCDWriteStrAt(9,5,"Empty");
				if(dat.VesselStatusStr[3]=='0')	LCDWriteStrAt(0,7,"Requires Clean!");
				if(dat.VesselStatusStr[3]=='1')	LCDWriteStrAt(0,7,"Clean");
				if(dat.VesselStatusStr[3]=='2')	LCDWriteStrAt(0,7,"Clean (SO2)");
				if(dat.VesselStatusStr[4]=='3')	LCDWriteStrAt(0,7,"Clean (Oxy)");
				i=9;
			}
			else
			{
				LCDWriteStrAt(9,2,"In-Use");
				LCDWriteStrAt(9,5,TABfield[3]); // write volume in litres
				LCDWriteStrAt(0,7,"Batch:"); // write batchcode xxxxxx-xxxx-xxxxxx
				StrCpySubStr(dat.TempStr2,TABfield[4],1,6);
			  LCDWriteStrAt(8,7,dat.TempStr2); // write batchcode xxxxxx-________
				StrCpySubStr(dat.TempStr2,TABfield[4],8,12);
			  LCDWriteStrAt(8,9,dat.TempStr2); // write batchcode ______-xxxx-xxxxxx
				if(dat.VesselStatusStr[9]=='d')
				i=11;
			}
			if(dat.VesselStatusStr[5]=='1') // not fixed/moveable
			{
			  LCDWriteStrAt(0,i,"Location:");
			  LCDWriteStrAt(9,i,TABfield[6]);
			}
    	WaitEvent(60);
		}

	  if(dat.MenuSel1==2) // vessel cleaning menu
		{

		}

	  if(dat.MenuSel1==3) // vessel quarantine menu
		{

		}

	  if(dat.MenuSel1==4) // view vessel status (just spit out the relevant facts for this vessel)
		{

		}

	}
}

void StockMenu(char *ID)
{
	dat.MenuSel1 = GetMenuItem("Stock Options","Scan Vessel","Update Job Status","","","");
}

void LabMenu(char *ID)
{
	dat.MenuSel1 = GetMenuItem("Lab Options","Scan Vessel","Update Job Status","","","");
}

void UserMenu(char *ID)
{
	dat.MenuSel1 = GetMenuItem("User Preferences","Scan Vessel","Update Job Status","","","");
}