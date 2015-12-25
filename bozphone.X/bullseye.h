// Contains the menus and logic specific to the bulls-i program

#ifndef BULLSEYE_H
#define	BULLSEYE_H

#include "hardware.h"

typedef struct // local structure to put all program helper variables local to this unit
{
  uint8 MenuSel1;       // First level menu selected item num (or 0)
  uint8 MenuSel2;       // second level menu selected item num (or 0)
  uint8 MenuSel3;       // Third level menu selected item num (or 0)
  char  JobID[10];      // Set to JobID after barcode scan (includes leading J)
  char  JIID[10];       // Set to JIID after barcode scan includes leading X
  char  LabID[10];      // Set to LabAnalysisID after barcode scan (includes leading L)
  char  TestID[10];     // Set to LabAnalysis TestID after barcode scan (includes leading T)
  char  AssetID[10];    // Set to AssetID after barcode or RFID scan (includes leading A)
  char  UserID[10];     // Set to RFID UID after RFID scan (number string should match the same in the user account table)
  char  SMID[10];       // Set to Stock Movement ID after barcode  (includes leading SM)
  char  MvtID[10];      // Cache last MovementID if required
  uint8 OldStatus;      // If required for changing statuses
  uint8 NewStatus;      // If required for changing statuses
  char  OldMetric[32];  // Current Metric (as read))
  char  NewMetric[32];  // New Metric as Typed or read from RFID/Barcode
  char  temp[9][32];    // temporary working strings...
} Tdata;

// only top level menus need to be public

void NewJobMenu(char *ID);
void OldJobMenu(char *ID);
void AssetMenu(char *ID);
void StockMenu(char *ID);
void LabMenu(char *ID);
void UserMenu(char *ID);

#endif