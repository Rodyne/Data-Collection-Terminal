Data-Collection-Terminal
========================

Open Source Data Collection Terminal files - http://hackaday.io/project/1915-data-collection-terminal


Note there are currently 3 parts to the code

1. The PIC32MX1xx/2xx C Program using the free Microchip MPLABX compiler suite and XC32 this will scan barcodes/rfid's into small simple wifi message packets (normally under 30 bytes) to be sent to a server and interpereted/executed

Note this firmware has custom menus for out MRP factory system which I have mostly left out. I have left the simple job update menu in though which should give you enough info to proceed.

I have tested the firmware for reading barcodes and RFID MIFAR blocks and reading/writing to the broker program, reading the keyboard and updating the display and these all work so you can basically use the code as a framework to build your own system. Note the setup of the wifi is done via programming the SSID, password and broker address into an RFID tag (in plain text atm) so setting up the device is just a case of going to setup menu and scanning the tag. I have also included the code for writting this RFID Tag seperately

2. The LUA Code for the ESP01 (Install NodeMCU and upload init.lua)

3. The broker program which will parse low level requests from the terminal into higher level requests to the target MRP database.

Note with the 3rd part I am reading and writing from a customers MySQL database which is proprietary. The program will allow you to connect to your own MySQL database and write your own queries etc but out of the box will not work until you have customised this for your system
You eill need to add two tables to your database for storage of the message queries and for logging, you will also have to add any extra security as required (not showing you mine!)
Hopefully the simple example I have put on to the system will be enough

The hardware for the terminal are at:

Hardware Schematics here => http://easyeda.com/project_view_Data-Entry-Terminal_neKEWQ8qI.htm

History of project here => http://rodyne.com/?page_id=13

With the current version I am getting about 2 weeks worth of work out of the terminal before needing to recharge which is mainly due to the aggressive turning off of power to modules that are not required and the fact that
actual wireless usage in practice is only a few minutes a day.
