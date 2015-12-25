/* USB CDC Interface for PIC32MX250
 *
 * Butchered from various GPL Interfaces which I guess mostly derive from 
 * Microchip. Thanks guys.. Whoever you are..
 * 
 * <MINOR RANT>
 * I seriously can not and do not want to understand any of this, it is much
 * too complicated and even with the original comments was incomprehensible.
 * I pretty sure someone could strip another 95% of the code away and still 
 * get a better working CDC interface, why cant Microchip just have CDC/HID
 * defined in hardware as it is all 99.9% of us are going to use anyway.
 * </RANT>
 * 
 * THIS SOFTWARE IS PROVIDED IN AN 'AS IS' CONDITION. NO WARRANTIES,
 * WHETHER EXPRESS, BLAH, BLAH, BLAH ....
 * BASICALLY ONLY USE FOR HOBBY USE...
 */
 
#ifndef USB_H
#define USB_H
 
#include "hardware.h"

typedef enum {sDETATCHED=0,sATTACHED=1,sPOWERED=2,sDEFAULT=4,sADDRPENDING=8,sADDRESSED=0x10,sREADY=0x20} tUSBstate;

typedef union __attribute__ ((packed)) _BD_STAT // PIC32 Buffer Descriptor Status Register layout.
{
    struct __attribute__ ((packed)){
        unsigned            :2;
        unsigned    BSTALL  :1;     //Buffer Stall Enable
        unsigned    DTSEN   :1;     //Data Toggle Synch Enable
        unsigned            :2;     //Reserved - write as 00
        unsigned    DTS     :1;     //Data Toggle Synch Value
        unsigned    UOWN    :1;     //USB Ownership
    };
    struct __attribute__ ((packed)){
        unsigned            :2;
        unsigned    PID0    :1;
        unsigned    PID1    :1;
        unsigned    PID2    :1;
        unsigned    PID3    :1;
    };
    struct __attribute__ ((packed)){
        unsigned            :2;
        unsigned    PID     :4;         //Packet Identifier
    };
    uint16 Val;
} BD_STAT;

typedef union __attribute__ ((packed))__BDT
{
    struct __attribute__ ((packed))
    {
        BD_STAT     STAT;
        uint16      CNT:10;
        uint32      ADR;		//Buffer Address
    };
    struct __attribute__ ((packed))
    {
        unsigned    res  :16;
        unsigned    count:10;
    };
    uint32 w[2];
    uint16 v[4];
    uint64 Val;
} BDT_ENTRY;

typedef struct __attribute__ ((packed)) _USB_DEVICE_DESCRIPTOR
{
    uint8 bLength;              // Length of this descriptor.
    uint8 bDescriptorType;      // DEVICE descriptor type (USB_DESCRIPTOR_DEVICE).
    uint16 bcdUSB;              // USB Spec Release Number (BCD).
    uint8 bDeviceClass;         // Class code (assigned by the USB-IF). 0xFF-Vendor specific.
    uint8 bDeviceSubClass;      // Subclass code (assigned by the USB-IF).
    uint8 bDeviceProtocol;      // Protocol code (assigned by the USB-IF). 0xFF-Vendor specific.
    uint8 bMaxPacketSize0;      // Maximum packet size for endpoint 0.
    uint16 idVendor;            // Vendor ID (assigned by the USB-IF).
    uint16 idProduct;           // Product ID (assigned by the manufacturer).
    uint16 bcdDevice;           // Device release number (BCD).
    uint8 iManufacturer;        // Index of String Descriptor describing the manufacturer.
    uint8 iProduct;             // Index of String Descriptor describing the product.
    uint8 iSerialNumber;        // Index of String Descriptor with the device's serial number.
    uint8 bNumConfigurations;   // Number of possible configurations.
} USB_DEVICE_DESCRIPTOR;

typedef union __attribute__ ((packed)) _CTRL_TRF_SETUP
{
    struct __attribute__ ((packed))
    {
        uint8 bmRequestType; //from table 9-2 of USB2.0 spec
        uint8 bRequest; //from table 9-2 of USB2.0 spec
        uint16 wValue; //from table 9-2 of USB2.0 spec
        uint16 wIndex; //from table 9-2 of USB2.0 spec
        uint16 wLength; //from table 9-2 of USB2.0 spec
    };
    struct __attribute__ ((packed))
    {
        unsigned :8;
        unsigned :8;
        uint16 W_Value; //from table 9-2 of USB2.0 spec, allows byte/bitwise access
        uint16 W_Index; //from table 9-2 of USB2.0 spec, allows byte/bitwise access
        uint16 W_Length; //from table 9-2 of USB2.0 spec, allows byte/bitwise access
    };
    struct __attribute__ ((packed))
    {
        unsigned Recipient:5;   //Device,Interface,Endpoint,Other
        unsigned RequestType:2; //Standard,Class,Vendor,Reserved
        unsigned DataDir:1;     //Host-to-device,Device-to-host
        unsigned :8;
        uint8 bFeature;          //DEVICE_REMOTE_WAKEUP,ENDPOINT_HALT
        unsigned :8;
        unsigned :8;
        unsigned :8;
        unsigned :8;
        unsigned :8;
    };
    struct __attribute__ ((packed))
    {
        unsigned :8;
        unsigned :8;
        uint8 bDscIndex;        //For Configuration and String DSC Only
        uint8 bDescriptorType;  //Device,Configuration,String
        uint16 wLangID;         //Language ID
        unsigned :8;
        unsigned :8;
    };
    struct __attribute__ ((packed))
    {
        unsigned :8;
        unsigned :8;
        uint8 bDevADR;		//Device Address 0-127
        uint8 bDevADRH;         //Must equal zero
        unsigned :8;
        unsigned :8;
        unsigned :8;
        unsigned :8;
    };
    struct __attribute__ ((packed))
    {
        unsigned :8;
        unsigned :8;
        uint8 bConfigurationValue;         //Configuration Value 0-255
        uint8 bCfgRSD;           //Must equal zero (Reserved)
        unsigned :8;
        unsigned :8;
        unsigned :8;
        unsigned :8;
    };
    struct __attribute__ ((packed))
    {
        unsigned :8;
        unsigned :8;
        uint8 bAltID;            //Alternate Setting Value 0-255
        uint8 bAltID_H;          //Must equal zero
        uint8 bIntfID;           //Interface Number Value 0-255
        uint8 bIntfID_H;         //Must equal zero
        unsigned :8;
        unsigned :8;
    };
    struct __attribute__ ((packed))
    {
        unsigned :8;
        unsigned :8;
        unsigned :8;
        unsigned :8;
        uint8 bEPID;             //Endpoint ID (Number & Direction)
        uint8 bEPID_H;           //Must equal zero
        unsigned :8;
        unsigned :8;
    };
    struct __attribute__ ((packed))
    {
        unsigned :8;
        unsigned :8;
        unsigned :8;
        unsigned :8;
        unsigned EPNum:4;       //Endpoint Number 0-15
        unsigned :3;
        unsigned EPDir:1;       //Endpoint Direction: 0-OUT, 1-IN
        unsigned :8;
        unsigned :8;
        unsigned :8;
    };
} CTRL_TRF_SETUP;

typedef struct __attribute__ ((packed))
{
    union __attribute__ ((packed))
    {
        //Various options of pointers that are available to get the data from
        uint8 *bRam;
        const uint8 *bRom;
        uint16 *wRam;
        const uint16 *wRom;
    } pSrc;
    
    union __attribute__ ((packed))
    {
        struct __attribute__ ((packed))
        {
            //is this transfer from RAM or ROM?
            unsigned ctrl_trf_mem          :1;
            unsigned reserved              :5;
            //include a zero length packet after
            //data is done if data_size%ep_size = 0?
            unsigned includeZero           :1;
            //is this PIPE currently in use
            unsigned busy                  :1;
        } bits;
        uint8 Val;
    } info;
    
    uint16 wCount;
} IN_PIPE;

typedef struct __attribute__ ((packed))
{
    union __attribute__ ((packed))
    {
        //Various options of pointers that are available to
        // get the data from
        uint8 *bRam;
        uint16 *wRam;
    } pDst;
    
    union __attribute__ ((packed))
    {
        struct __attribute__ ((packed))
        {
            unsigned reserved              :7;
            //is this PIPE currently in use
            unsigned busy                  :1;
        } bits;
        uint8 Val;
    } info;
    
    uint16 wCount;
    void (*pFunc)(void);
} OUT_PIPE;

typedef union _LINE_CODING
{
    struct
    {
        uint8 _byte[7];
    };
    struct
    {
        uint32 dwDTERate;          // Complex data structure
        uint8 bCharFormat;
        uint8 bParityType;
        uint8 bDataBits;
    };
} LINE_CODING;

void DoCDC(void);

#endif