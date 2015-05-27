
#ifndef BARCODE_H
#define	BARCODE_H

#include "hardware.h"

extern char BarcodeData[32];

void  CloseBarcode();
uint8 ReadBarcode(); // Wake up and Read in barcode string from MT700 (up to max input size) or timeout

#endif	/* BARCODE_H */

