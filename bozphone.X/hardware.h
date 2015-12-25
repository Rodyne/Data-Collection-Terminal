// PIC32 Pins, peripherals and other low level defs

#ifndef HARDWARE_H
#define	HARDWARE_H

#define _SUPPRESS_PLIB_WARNING                // Get rid of MPLABX plib depreciation warnings which become errors under strict mode
#define _DISABLE_OPENADC10_CONFIGPORT_WARNING // Gee.. Thanks for breaking my code Microchip!
#include <plib.h>							  // include microchip pic32 libraries

#define int8    char
#define int16   short
#define int32   int
#define uint8   unsigned char
#define uint16  unsigned short
#define uint32  unsigned int
#define uint64  unsigned long

// PortSetPins.. macros set the pin as digital input or output. Other pin macro defines are so we can refer to pin by name not number

// PORT A - Hardware note A5 and A6 used for OSC/XTAL connection and cannot be used as I/O and pins A2 & A3 dont exist on this chip
#define CFG_PORT_A_OUT		PORTSetPinsDigitalOut(IOPORT_A, BIT_0 | BIT_1 | BIT_4 | BIT_7 | BIT_9 | BIT_10 );
#define CFG_PORT_A_IN		PORTSetPinsDigitalIn(IOPORT_A,  BIT_8 );
//
#define K7(state)           LATAbits.LATA0 = state  // OUT (Keyboard scan line 7)
#define LCD_CS(state)       LATAbits.LATA1 = state  // OUT Select LCD active Low
#define BUZZER(state)		LATAbits.LATA4 = state  // OUT (idle should be ON +3.3V)  modulate to sound buzzer
#define EXT_RST(state)		LATAbits.LATA7 = state  // OUT (Hard reset peripherals if Required! eg LCD and RFID - Dont be tempted to tie to Vcc! learnt the hard way!)
#define SET_PPS_WIFI_RX     PPSInput(2,U2RX,RPA8)   // IN  (UART 2 WIFI SERIAL RECEIVE)
#define BARCODE_TRIG(state)	LATAbits.LATA9 = state  // OUT (Active low, starts a barcode scan)
#define K5(state)           LATAbits.LATA10 = state // OUT (Keyboard scan line 5)

// PORT B - Hardware note B6, B10, B11 and B12 cannot be used as used for USB
#define CFG_PORT_B_OUT		PORTSetPinsDigitalOut(IOPORT_B, BIT_0 | BIT_2 | BIT_3 | BIT_4 |BIT_5 | BIT_9 | BIT_10 | BIT_13 | BIT_14 | BIT_15 );
#define CFG_PORT_B_IN		PORTSetPinsDigitalIn(IOPORT_B,  BIT_1 | BIT_7 | BIT_8 );
//
#define K6(state)			LATBbits.LATB0 = state  // OUT (Keyboard scan line 6)
#define SET_PPS_SDI         PPSInput(2,SDI1,RPB1)   // IN SPI DATA INPUT
#define RFID_CS(state)		LATBbits.LATB2 = state  // OUT set low to select RFID reader on SPI BUS
#define BACKLIGHT(state)	LATBbits.LATB3 = !state // OUT (Directly connected to LED) (inverted) 1=Power ON, 0=Power OFF
#define WIFI_PWR(state)		LATBbits.LATB4 = state  // OUT 1 = Power ON, 0 = shutdown
#define PWR_HOLD(state)		LATBbits.LATB5 = state  // OUT (set high to keep CPU powered on, drop to 'true' power off system fully)
#define KEYSTROBE0()        !PORTBbits.RB7          // IN  (active low) receives key presses
#define SET_PPS_INT4		PPSInput(1,INT4,RPB7)		//     (enable external interupt for above keypresses)
#define KEYSTROBE1()        !PORTBbits.RB8          // IN  (active low) receives key presses
#define SET_PPS_INT3		PPSInput(2,INT3,RPB8)		//     (enable external interupt for above keypresses)
#define K0(state)           LATBbits.LATB9  = state // OUT (Keyboard scan line 0)
#define SET_PPS_SDO         PPSOutput(3,RPB13,SDO1) // OUT SPI DATA OUT
#define LCD_CLK(state)      LATBbits.LATB14 = state // OUT SPI CLK
#define LCD_DC(state)       LATBbits.LATB15 = state // OUT

// PORT C
#define CFG_PORT_C_OUT      PORTSetPinsDigitalOut(IOPORT_C, BIT_0 | BIT_1 | BIT_2 | BIT_4 | BIT_6 |BIT_7 | BIT_8 | BIT_9  );
#define CFG_PORT_C_IN		PORTSetPinsDigitalIn(IOPORT_C,  BIT_3 | BIT_5 );
//
#define NU(state)			LATCbits.LATC0 = state  // OUT not used
#define ESP_PGM(state)		LATCbits.LATC1 = state	// OUT (connected to ESP8266 GPIO0 - Set to 0V to program firmware, and 1 to use normally)
#define UART2_TX(state)		LATCbits.LATC2 = state  // OUT (used to turn off tx pin during idle)
#define SET_PPS_WIFI_TX     PPSOutput(4,RPC2,U2TX)  // OUT (UART 2 WIFI TRANSMIT OUT)
#define SET_PPS_BARCODE_RX  PPSInput(3,U1RX,RPC3)   // IN  (UART 1 BARCODE RECEIVE IN)
#define BARCODE_PWR(state)	LATCbits.LATC4 = !state // OUT (inverted) 1=Power ON, 0=Power OFF
#define PWR_BUT				PORTCbits.RC5						// IN		Goes low when SCAN Button pressed
#define K1(state)           LATCbits.LATC6  = state // OUT (Keyboard scan line 1)
#define K2(state)           LATCbits.LATC7  = state // OUT (Keyboard scan line 2)
#define K3(state)           LATCbits.LATC8  = state // OUT (Keyboard scan line 3)
#define K4(state)           LATCbits.LATC9  = state // OUT (Keyboard scan line 4)

#define BARCODE_UART        UART1 // Note the UART1 Transmit pin is not required for barcode and so is not defined anywhere
#define BARCODE_BAUD        9600
#define WIFI_UART           UART2
#define WIFI_BAUD           9600

// keypad keyscan return definitions (use displayable ASCII ones)

#define K_SCAN    'S'
#define K_OK      'O'
#define K_CANCEL  'C'
#define K_UP      'U'
#define K_DN      'D'
#define K_DEL     'X'
#define K_DECIMAL '.'
#define K_0       '0'
#define K_1       '1'
#define K_2       '2'
#define K_3       '3'
#define K_4       '4'
#define K_5       '5'
#define K_6       '6'
#define K_7       '7'
#define K_8       '8'
#define K_9       '9'

enum TSPISelect { SelNONE, SelRFID, SelLCD }; // Only one SPI device can be selected, use this func to ensure that!
enum TOnOff     { OFF=0, ON=1 };

extern uint8  USBPowered;
extern uint8  DEBUG;
extern uint8  KEYBEEP;
extern char   KeypadData[32];


void  PeripheralReset();
uint8 BrownOutReset();  //  returns true if power up was after a brown out
void  DelayMs(uint32 time); // Blocking wait in milliseconds (approx)
void  DelayUs(uint32 time); // Blocking wait in microseconds (approx)
void  beep(uint16 BCMilliseconds); // set 2Khz buzzer active for this many mS - note this is blocking but interrupts still work
void  SPISelect(enum TSPISelect device); // Only 1 SPI device can be selected
uint8 SPITransfer(uint8 reg);			// Generic Write/read SPI1 (Correct device must be selected!)
void  SerialWrite(UART_MODULE id, uint8 data); // write to UART directly and wait to finish (blocking - hangs execution until finished)
void  SerialWriteStr(UART_MODULE id, char *str); // use above to send a string
uint8 SerialAvailable(UART_MODULE id);
uint8 SerialRead(UART_MODULE id); // Reads buffer. No checks here, you must use SerialDataAvailable before calling this or we return 0x00
void  SerialPurge(UART_MODULE id); // discard remaining serial chars and clear errors and buffers
void  SerialOpen(UART_MODULE id); // discard remaining serial chars and clear errors and buffers
void  SerialClose(UART_MODULE id);
void  InitPIC32(void); // Initialise PIC32 and terminal io pins
int8  WaitEvent(uint16 TimeoutSeconds);

#endif	/* HARDWARE_H */