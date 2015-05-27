// Contains the menus and logic specific to the bulls-i program

#ifndef BULLS_I_PASRSER_H
#define	BULLS_I_PASRSER_H

#include "hardware.h"

typedef struct // local structure to put all program helper variables local to this unit
{
  uint8 MenuSel1;           // First level menu selected item num (or 0)
  uint8 MenuSel2;           // second level menu selected item num (or 0)
  uint8 MenuSel3;           // Third level menu selected item num (or 0)
  uint8 JobStatus;          // 0=Not editable, 1=Editable
  uint8 FnCode;             // Received and Transmitted Function Code
  char  JobID[10];          // Set to JobID after barcode scan (includes leading J)
  char  LabID[10];          // Set to LabAnalysisID after barcode scan (includes leading L)
  char  AssetID[10];        // Set to AssetID after barcode or RFID scan (includes leading A)
  char  UserKey[10];        // Set to RFID UID after RFID scan (number string should match the same in the user account table)
  char  SMID[10];           // Set to Stock Movement ID after barcode  (includes leading SM)
  char  MovementID[10];
  char  JobStatusStr[10];
  char  VesselStatusStr[10];
  char  TempStr[32];
  char  TempStr2[80];
} Tdata;

// only top level menus need to be public

void JobMenu(char *ID);   // Get here if you first scan from root menu is a job order id (remember prefixed with a J)
void AssetMenu(char *ID); // Get here if you first scan from root menu is an asset id (remember prefixed with a A)
void StockMenu(char *ID); // Get here if you first scan from root menu is a stock movement id (remember prefixed with a SM)
void LabMenu(char *ID);   // Get here if you first scan from root menu is a Lab Analisis id (remember prefixed with a L)
void UserMenu(char *ID);  // Get here if you first scan from root menu is a user id (remember prefixed with a U)

#endif