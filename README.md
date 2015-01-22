Data-Collection-Terminal
========================

Open Source Data Collection Terminal files - http://hackaday.io/project/1915-data-collection-terminal

Note there will be two parts to the code (possibly 3 if you seperate the management part out of the service), 

firstly a small C Program for a PIC32MX1xx/2xx using the free Microchip MPLABX compiler suite and XC32 this will scan barcodes/rfid's into small simple wifi message packets (normally under 30 bytes) to be sent to a server and interpereted/executed

Secondly a server program which will parse low level requests from the terminal into higher level requests such as SQL queries, SQL Updates and Print requests to a target MRP database.

The hardware for the terminal is almost complete

Hardware Schematics here => http://easyeda.com/project_view_Data-Entry-Terminal_neKEWQ8qI.htm

History of project here => http://rodyne.com/?page_id=13
