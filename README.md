Data-Collection-Terminal
========================

Open Source Data Collection Terminal files - http://hackaday.io/project/1915-data-collection-terminal

Note there are two parts to the code

firstly a small C Program for a PIC32MX250 using the free Microchip MPLABX compiler suite and XC32 this will scan barcodes/rfid's into small simple wifi message packets (normally under 30 bytes) to be sent to a server and interpereted/executed. The program supplied is simply a demonstration which has all the drivers for reading barcodes/rfid tags and sending them over wifi. It is not the full program as this requires you to have the MRP system I developed for the winery but it is a skeleton program to see how it all fits together

The second part of the code is a server program which parses low level requests from the terminal into higher level requests such as SQL queries, SQL Updates and Print requests to a target MRP database. It also parses SQL query responses into very small messages back to the terminal.

The hardware for the version 1.0 terminal is now complete
