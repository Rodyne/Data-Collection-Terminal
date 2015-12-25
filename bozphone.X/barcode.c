
#include "hardware.h"
#include "barcode.h"

char BarcodeData[32];


void CloseBarcode()
{
	SerialClose(BARCODE_UART);
	BARCODE_PWR(0);
  BARCODE_TRIG(0);
}

uint8 StrInBarcodeData(char *str, uint8 MaxLen)
{
	uint8 i = 0;
	while(i<MaxLen)
	{
		if(*str==BarcodeData[i])
		{
			str++;
			if(*str=='\0') return 1;
		}
		else
			i++;
	}
  return 0;
}

uint8 ReadBarcode() // Wake up and Read in barcode string from MT700 (up to max input size) or timeout
{
  uint8  BarcodeLen=0;
  uint32 timeleft = 3000; // set our timout for this operation in milliseconds*delay (=3000mS)

  BARCODE_TRIG(1);
	BARCODE_PWR(1);	// power up and reset module then enable interrupt and initiate scan
	DelayMs(50);
  SerialOpen(BARCODE_UART);
  BARCODE_TRIG(0); // initiate scan on MT700 (should wake up and led go on to read) as its a seperate CPU
	BarcodeData[0] = '\0';
	while(timeleft--)
  {
    if(SerialAvailable(BARCODE_UART))
    {
      BarcodeData[BarcodeLen] = SerialRead(BARCODE_UART); // build up barcode string as received (Note we DONT want the \r\n
      if(BarcodeData[BarcodeLen]=='\r')
			{
				CloseBarcode();
    		beep(50);
				DelayMs(50);
    		beep(50);
        BarcodeData[BarcodeLen]='\0'; // set end
				return BarcodeLen;
			}
      if(BarcodeLen<30) BarcodeLen++;
    }
		DelayMs(3);
  }
	// at this point we have either read in a good barcode string or timed out, either way we are finished with MT700 for now so power it down
	CloseBarcode();
  return 0;
}
