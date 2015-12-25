#include "hardware.h"
#include "usb-cdc.h"

#define USBCableConnected      U1OTGCONbits.VBUSON
#define PhysicalAddressOf(va)  ( (uint32) (va) & 0x1FFFFFFF )  // Translate virtual address to physical

uint8 short_packet_status;
uint8 control_transfer_state;
uint8 ustat_saved;
uint8 cdc_trf_state;              // States are defined cdc.h
uint8 cdc_rx_len;                 // total rx length
uint8 cdc_tx_len;                 // total tx length

tUSBstate USBstate;
IN_PIPE   usb_in_pipe;
OUT_PIPE  usb_out_pipe;

volatile BDT_ENTRY *pBDTEntryEP0OutCurrent;
volatile BDT_ENTRY *pBDTEntryEP0OutNext;
volatile BDT_ENTRY *pBDTEntryOut[4];
volatile BDT_ENTRY *pBDTEntryIn[4];
volatile BDT_ENTRY usb_buffer[16] __attribute__ ((aligned (512)));

volatile CTRL_TRF_SETUP usb_setup_pkt;

volatile uint8 ctrl_trf_data[8];

LINE_CODING cdc_line_coding;    // Buffer to store line coding information

static volatile BDT_ENTRY* data_out;
static volatile BDT_ENTRY* data_in;
static volatile uint8 cdc_data_rx[64];
static volatile uint8 cdc_data_tx[8];
static uint8 ignored_cmd_response[8];

const USB_DEVICE_DESCRIPTOR usb_device = { sizeof(USB_DEVICE_DESCRIPTOR), 1, 0x0200, 0x02, 0x00, 0x00, 8, 0x04D8, 0xFEAB, 0x10, 1, 2, 3, 1 };

#define USB_STRING_INIT(nchars) struct {uint8 bLength; uint8 bDescriptorType; uint16 string[nchars]; }

const USB_STRING_INIT(1)  string0_descriptor = {sizeof(string0_descriptor), 3, { 0x0409 } };
const USB_STRING_INIT(9)  string1_descriptor = {sizeof(string1_descriptor), 3, {'M','i','c','r','o','c','h','i','p'} };
const USB_STRING_INIT(28) string2_descriptor = {sizeof(string2_descriptor), 3, {'R','o','v','i','n','g',' ','D','y','n','a','m','i','c','s',' ','(','r','o','d','y','n','e','.','c','o','m',')' } };
const USB_STRING_INIT(10) string3_descriptor = {sizeof(string3_descriptor), 3, {'r','o','d','y','n','e','.','c','o','m'} };

const uint8 *const usb_string[5] = 
{
    (const uint8 *const) &string0_descriptor,
    (const uint8 *const) &string1_descriptor,
    (const uint8 *const) &string2_descriptor,
    (const uint8 *const) &string3_descriptor
};

const uint8 usb_config_descriptor[] = 
{
  9,     // Length of this descriptor.
  2,     // Configuration descriptor type
  67, 0, // Total length of data in this structure = 9 + 9 + 5 + 4 + 5 + 5 + 7 + 9 + 7 + 7
  2,     // Number of interfaces in this cfg
  1,     // Value of this configuration
  0,     // Configuration string index
  0xc0,  // Attributes  (_DEFAULT | _SELF) // USB_CFG_DSC_REQUIRED
  50,    // Max power consumption in 2mA units (100mA))

	9,      // Size of this USB Interface descriptor in bytes
	4,      // Interface descriptor type
	0,      // Interface number
	0x00,   // Alternate setting number
	0x01,   // Number of endpoints in this interface
	2,      // Class code
	2,      // Subclass code
	1,      // Protocol code
	0,      // Interface string index
    
  5,      // Size of CDCHeaderfunction descriptor in bytes
	0x24,   // bDescriptorType = CDCHeaderfunction
	0,      // bDescriptorSubtype
	0x20, 
  0x01,   // CDC 1.20

  4,      // Size of this descriptor
	0x24,   // bDescriptorType = CDCHeaderfunction
	2,      // bDescriptorSubtype
	0x00,   //(CDC_ACM_CAP_LINE_CODINGS | CDC_ACM_CAP_SEND_BREAK),// bmCapabilities: (see PSTN120.pdf Table 4)

  5,      // Size of this descriptor
	0x24,   // bDescriptorType
	6,      // bDescriptorSubtype
	0,      // bControlInterface
	1,      // bSubordinateInterface0
  
  5,      // Size of this descriptor
	0x24,   // bDescriptorType = CDC_CALL_MGT_FN
	1,      // bDescriptorSubtype
	1,      // bmCapabilities
	1,      // bDataInterface
  
	7,      // size of descriptor
	5,      // type = USB endpoint descriptor
	0x81,   // Endpoint address         // _EP01_IN
	3,      // Attributes
	8, 0x00,// Size
	1,      // Interval ms
  
	9,      // size of descriptor
	4,      // Interface descriptor type = USB_INTERFACE_DESCRIPTOR
	1,      // Interface number         
	0x00,   // Alternate setting number
	0x02,   // Number of endpoints in this interface
	0x0A,   // Class code
	0x00,   // Subclass code
	0,      // Protocol code
	0x00,   // Interface string index
  
	7,      // size of descriptor
	5,      // Endpoint descriptor = USB_ENDPOINT_DESCRIPTOR
	2,      // Endpoint address         // _EP02_OUT
	2,      // bulk
	64, 0,  // Size
	0,      // USB_POLLING_PERIOD Interval ms
  
	7,      // size of descriptor
	5,      // Endpoint descriptor = USB_ENDPOINT_DESCRIPTOR
	0x81,   // Endpoint address         // _EP02_IN
	2,      // bulk
	8, 0,   // Size
	0       // USB_POLLING_PERIOD Interval ms
};

const uint8 *const usb_config[] = {(const uint8 *const) &usb_config_descriptor,};

void InitUSB(void)
{
  uint8 i;
  uint32 phyaddrusbbuf = PhysicalAddressOf(&usb_buffer);
  
  for (i=0; i<(sizeof(usb_buffer)/sizeof(BDT_ENTRY)); i++) // Clear BDT entries
    usb_buffer[i].Val = 0x00;

  usb_in_pipe.info.Val = 0;
  usb_out_pipe.info.Val = 0;
  usb_out_pipe.wCount = 0;
  
  for(i = 0; i < 4; i++) // Initialize all pBDTEntryIn[] and pBDTEntryOut[] pointers to NULL, so they don't get used inadvertently.
  {
    pBDTEntryIn[i]  = 0;
    pBDTEntryOut[i] = 0;
  }

  USBstate = sPOWERED;
  
  pBDTEntryIn[0] = (volatile BDT_ENTRY*) &usb_buffer[2];    // Get ready for the first packet
  
  // Enable USB module
  U1PWRC = 0;
  U1PWRC   = U1PWRC | _U1PWRC_USBPWR_MASK;
  while(!U1CONbits.USBEN) U1CON = U1CON | _U1CON_USBEN_MASK;
  U1OTGCON = 0;
  U1CON = 0;
  U1OTGCON = U1OTGCON  | _U1OTGCON_DMPULUP_MASK | _U1OTGCON_DPPULUP_MASK; // Enable power to the D+/D- pull-up resistors
  U1BDTP1  = phyaddrusbbuf >> 8;
  U1BDTP2  = phyaddrusbbuf >> 16;
  U1BDTP3  = phyaddrusbbuf >> 24;
  U1ADDR   = 0; // Reset to default address
  U1CNFG1  = 0; // Reset configuration Must be preprogrammed prior to enabling the module.
  U1IE     = 0; // Enable USB interrupts The interrupt flag bits are cleared by writing a 1
  U1OTGIE  = _U1OTGIE_SESVDIE_MASK;
  U1IR     = 0xff;
  U1OTGIR  = _U1OTGIR_SESVDIF_MASK;
}

volatile BDT_ENTRY* usb_tx_one_packet (uint8 ep, uint8* data, uint8 len)
{
  volatile BDT_ENTRY* handle = pBDTEntryOut[ep];
  if (handle == 0) return 0;
  handle->ADR = PhysicalAddressOf(data);
  handle->CNT = len;
  handle->STAT.Val &= 0x40;
  handle->STAT.Val |= 0x88;
  *(uint8*)&pBDTEntryOut[ep] ^= 8;
  return handle;
}

volatile BDT_ENTRY* usb_rx_one_packet (uint8 ep, uint8* data, uint8 len)
{
  volatile BDT_ENTRY* handle = pBDTEntryIn[ep];
  if (handle == 0) return 0;
  handle->ADR = PhysicalAddressOf(data);
  handle->CNT = len;
  handle->STAT.Val &= 0x40;
  handle->STAT.Val |= 0x88;
  *(uint8*)&pBDTEntryIn[ep] ^= 8;
  return handle;
}

void usb_prepare_for_next_setup_trf(void)
{
  uint8 setup_cnt;
  BDT_ENTRY* p;

  if ( (control_transfer_state == 2) && (U1CON & _U1CON_PKTDIS_MASK) && (pBDTEntryEP0OutCurrent->CNT == sizeof(CTRL_TRF_SETUP)) )
  {  
    if ((pBDTEntryEP0OutCurrent->STAT.PID == 0x0d) && (pBDTEntryEP0OutNext->STAT.UOWN == 0) )
    {
      pBDTEntryEP0OutNext->ADR = PhysicalAddressOf(&usb_setup_pkt);
      for(setup_cnt = 0; setup_cnt < sizeof(CTRL_TRF_SETUP); setup_cnt++)
      {
        *(((uint8*) &usb_setup_pkt) + setup_cnt) = *(((uint8*) &ctrl_trf_data) + setup_cnt);
      }
    }
  }  
  else
  {
    control_transfer_state = 0;
    pBDTEntryEP0OutNext->CNT = 8;
    pBDTEntryEP0OutNext->ADR = PhysicalAddressOf(&usb_setup_pkt);
    pBDTEntryEP0OutNext->STAT.Val = 0x8c; // USIE|DAT0|DTSEN|BSTALL
    pBDTEntryIn[0]->STAT.Val = 0;
    p = (BDT_ENTRY*)(((uint32)pBDTEntryIn[0])^8);
    p->STAT.Val = 0;
  }
  //if still expecting data from the control transfer then make sure to terminate that request and let them know that they are done
  if (usb_out_pipe.info.bits.busy == 1)
  {
    if (usb_out_pipe.pFunc != 0) usb_out_pipe.pFunc();
    usb_out_pipe.info.bits.busy = 0;
  }
}

void usb_configure_endpoint(uint8 ep, uint8 dir)
{
  volatile BDT_ENTRY* handle;

  handle = (volatile BDT_ENTRY*) &usb_buffer[0];
  handle += (4*ep+2*dir); //Add in offset to the BDT of interest
  handle->STAT.UOWN = 0;
  if (dir == 0)
    pBDTEntryOut[ep] = handle;
  else
    pBDTEntryIn[ep] = handle;
  handle->STAT.DTS = 0;
  (handle+1)->STAT.DTS = 1;
}

void usb_enable_endpoint(uint8 ep, uint8 options)
{
  uint8* p;
    
  if (options & 8) usb_configure_endpoint(ep, 0); // out    
  if (options & 4) usb_configure_endpoint(ep, 1); // in 
  p = (uint8*) (&U1EP0 + (4 * ep));
  *p = options;
}

void usb_ctrl_trf_tx_service(void)
{
  uint8 byteToSend = 8;
  uint8 *dst;

  if (usb_in_pipe.wCount < 8)  // First, have to figure out how many byte of data to send.
  {
    byteToSend = usb_in_pipe.wCount;
    if (short_packet_status == 0)
      short_packet_status = 1;
    else if (short_packet_status == 1)
      short_packet_status = 2;
  }
  pBDTEntryIn[0]->CNT = byteToSend;

  usb_in_pipe.wCount = usb_in_pipe.wCount - byteToSend;  // Subtract the number of bytes just about to be sent from the total.
  
  dst = (uint8*) ctrl_trf_data;// Set destination pointer

  if (usb_in_pipe.info.bits.ctrl_trf_mem == 0)
  {
    while (byteToSend)
    {
      *dst++ = *usb_in_pipe.pSrc.bRom++;
      byteToSend--;
    }
  }
  else
  {
    while (byteToSend)
    {
      *dst++ = *usb_in_pipe.pSrc.bRam++;
      byteToSend--;
    }
  }
}

void usb_check_cdc_request()
{
  if (usb_setup_pkt.Recipient != 1) return;
  if (usb_setup_pkt.RequestType != 1) return;
  if (usb_setup_pkt.bIntfID > 1) return; // Interface ID must be CDC comm or data

  switch (usb_setup_pkt.bRequest) // just do requests required
  {
    case 0:
      usb_in_pipe.pSrc.bRam = (uint8*) & ignored_cmd_response;
      usb_in_pipe.wCount = 8; // dummy length
      usb_in_pipe.info.bits.ctrl_trf_mem = 1;
      usb_in_pipe.info.bits.busy = 1;
      break;

    case 1:
      usb_in_pipe.pSrc.bRam = (uint8*) & ignored_cmd_response;
      usb_in_pipe.info.bits.busy = 1;
      break;

    case 0x20:
      usb_out_pipe.wCount = usb_setup_pkt.wLength;
      usb_out_pipe.pDst.bRam = (uint8*) & cdc_line_coding._byte[0];
      usb_out_pipe.pFunc = 0;
      usb_out_pipe.info.bits.busy = 1;
      break;

    case 0x21:
      usb_in_pipe.pSrc.bRam = (uint8*) & cdc_line_coding;
      usb_in_pipe.wCount = 7;
      usb_in_pipe.info.Val = 0xc1;
      break;

    case 0x22:
      if (usb_setup_pkt.W_Value == 0x03)
          cdc_trf_state = 1;
      else
          cdc_trf_state = 0;
      usb_in_pipe.info.bits.busy = 1;
      break;
  }
}

void ServiceUSB(void)
{
  uint8 byteToRead, i;
  
  if (U1OTGIE & _U1OTGIE_ACTVIE_MASK && U1OTGIR & _U1OTGIR_ACTVIF_MASK) // Bus Activity Interrupt enabled & Activity on pins (wake up device)
  {  
    U1IE = _U1IE_URSTIE_MASK | _U1IE_IDLEIE_MASK;
    U1IR = _U1IR_URSTIF_MASK | _U1IR_IDLEIF_MASK;
    U1PWRCbits.USUSPEND = 0;
    U1OTGIR |= _U1OTGIR_ACTVIF_MASK;
  }
    
  if (U1IE & _U1IE_URSTIE_MASK && U1IR & _U1IR_URSTIF_MASK) // PC Woke up or issued USB reset
  {
    InitUSB();
    U1EP0 = 0x0d; // EP_CTRL | USB_HANDSHAKE_ENABLED;
    usb_buffer[0].ADR = PhysicalAddressOf(&usb_setup_pkt);
    usb_buffer[0].CNT = 8;
    usb_buffer[0].STAT.Val &= 3;
    usb_buffer[0].STAT.Val |= 0x8C; // _USIE|_DTSEN|_BSTALL;
    while (U1IR & _U1IR_TRNIF_MASK)
      U1IR |= _U1IR_TRNIF_MASK;
    U1CONbits.PKTDIS = 0;
    U1CONbits.PPBRST = 0;
    U1IE = _U1IE_IDLEIE_MASK  | _U1IE_TRNIE_MASK;
    U1IR = _U1IR_IDLEIF_MASK  | _U1IR_TRNIF_MASK | _U1IR_URSTIF_MASK;
    USBstate = sDEFAULT;
    U1IR = U1IR | _U1IR_URSTIF_MASK;
  }

  if (U1IE & _U1IE_STALLIE_MASK && U1IR & _U1IR_STALLIF_MASK) // process stall
  {
    if(U1EP0 & _U1EP0_EPSTALL_MASK) U1EP0CLR = _U1EP0_EPSTALL_MASK;
    U1IR |= _U1IR_STALLIF_MASK;
  }
  
  if (U1IE & _U1IE_UERRIE_MASK && U1IR & _U1IR_UERRIF_MASK) // process (clr) error)
  {
    U1EIR = 0xFF;               // This clears UERRIF
    U1IR |= _U1IR_UERRIF_MASK;
  }  
  
  if (U1IE & _U1IE_TRNIE_MASK  &&  U1IR & _U1IR_TRNIF_MASK)  // Handle USB data/cfg requests
  {
    ustat_saved = U1STAT;
    if ((ustat_saved & 0xfb) == 0)
    {
      // Point to the EP0 OUT buffer of the buffer that arrived
      pBDTEntryEP0OutCurrent = (volatile BDT_ENTRY*) & usb_buffer[(ustat_saved & 0xfc) >> 2];
      // Set the next out to the current out packet
      pBDTEntryEP0OutNext = pBDTEntryEP0OutCurrent;
      // Toggle it to the next ping pong buffer
      *(uint8*)&pBDTEntryEP0OutNext ^= 8;
      // If the current EP0 OUT buffer has a SETUP token
      if (pBDTEntryEP0OutCurrent->STAT.PID == 0x0d)
      {  
        if (pBDTEntryIn[0]->STAT.UOWN != 0) pBDTEntryIn[0]->STAT.Val = 0;// if  SIE owns the buffer give control back to the CPU Compensate for after a STALL
        short_packet_status = 0;    // Keep track of if a short packet has been sent yet or not
        control_transfer_state = 0;
        usb_in_pipe.wCount = 0;
        usb_in_pipe.info.Val = 0;

        // Check std requests
        if (usb_setup_pkt.bRequest == 6) // wants descriptor info
        {  
          if (usb_setup_pkt.bmRequestType == 0x80)
          {
            usb_in_pipe.info.Val = 0xc0; // USB_INPIPES_ROM | USB_INPIPES_BUSY | USB_INPIPES_INCLUDE_ZERO;
            switch(usb_setup_pkt.bDescriptorType)
            {
              case 1: // descriptor device
                usb_in_pipe.pSrc.bRom = (const uint8*) &usb_device;
                usb_in_pipe.wCount = sizeof(usb_device);
                break;

              case 2: // descriptor cfg
                usb_in_pipe.pSrc.bRom = *(usb_config + usb_setup_pkt.bDscIndex);
                usb_in_pipe.wCount = *(usb_in_pipe.pSrc.wRom+1);// Set data count
                break;

              case 3: // descriptor string
                usb_in_pipe.pSrc.bRom = *(usb_string + usb_setup_pkt.bDscIndex);
                usb_in_pipe.wCount = *usb_in_pipe.pSrc.bRom;
                break;

              default:
                usb_in_pipe.info.Val = 0;
                break;
            }
          }

        }  
        
        if (usb_setup_pkt.bRequest == 5)
        {
          usb_in_pipe.info.bits.busy = 1;
          USBstate = sADDRPENDING;
        }
        
        if (usb_setup_pkt.bRequest == 9) // Want configure. So Init CDC Endpoint  
        {          
          cdc_line_coding.dwDTERate = 9600;
          cdc_line_coding.bCharFormat = 0; // 1 stop bit
          cdc_line_coding.bParityType = 0; // no parity
          cdc_line_coding.bDataBits   = 8; // 8 data bits
          cdc_trf_state = 0;
          cdc_tx_len = 0;
          cdc_rx_len = 0;  
          usb_enable_endpoint(1, 0x15);
          usb_enable_endpoint(2, 0x1d);
          data_out = usb_tx_one_packet(2, (uint8*)&cdc_data_rx, sizeof(cdc_data_rx));
          data_in  = 0;
          USBstate = sREADY; // set state CONFIGURED
        }
        
        usb_check_cdc_request();
        
        // finish off request
        U1CONbits.PKTDIS = 0;

        if (usb_in_pipe.info.bits.busy == 0)
        {
          pBDTEntryEP0OutNext->CNT = 8;
          if (usb_out_pipe.info.bits.busy == 1)
          {
            control_transfer_state = 2;
            pBDTEntryIn[0]->CNT = 0;
            pBDTEntryIn[0]->STAT.Val = 0xc8; //USIE|DAT1|DTSEN;
            pBDTEntryEP0OutNext->ADR = PhysicalAddressOf(&ctrl_trf_data);
            pBDTEntryEP0OutNext->STAT.Val = 0xc8; //USIE|DAT1|DTSEN;
          }
          else
          {
            pBDTEntryEP0OutNext->ADR = PhysicalAddressOf(&usb_setup_pkt);
            pBDTEntryEP0OutNext->STAT.Val = 0x8c; //USIE|_DAT0|_DTSEN|_BSTALL;
            pBDTEntryIn[0]->STAT.Val = 0x84; //_USIE|_BSTALL;
          }
        }
        else
        {
          if (usb_out_pipe.info.bits.busy == 0)
          {
            pBDTEntryEP0OutNext->CNT = 8;
            if (usb_setup_pkt.DataDir == 1)
            {
              if (usb_setup_pkt.wLength < usb_in_pipe.wCount)
                usb_in_pipe.wCount = usb_setup_pkt.wLength;
              usb_ctrl_trf_tx_service();
              control_transfer_state = 1;
              pBDTEntryEP0OutNext->ADR = PhysicalAddressOf(&usb_setup_pkt);
              pBDTEntryEP0OutNext->STAT.Val = 0x80; // Note: DTSEN is 0!
              pBDTEntryEP0OutCurrent->CNT = 8;
              pBDTEntryEP0OutCurrent->ADR = PhysicalAddressOf(&usb_setup_pkt);
              pBDTEntryEP0OutCurrent->STAT.Val = 0x84; 
              pBDTEntryIn[0]->ADR = PhysicalAddressOf(&ctrl_trf_data);
              pBDTEntryIn[0]->STAT.Val = 0x8c;
            }
            else
            {
              control_transfer_state = 2;
              pBDTEntryIn[0]->CNT = 0;
              pBDTEntryIn[0]->STAT.Val = 0xc8;
              pBDTEntryEP0OutNext->ADR = PhysicalAddressOf(&usb_setup_pkt);
              pBDTEntryEP0OutNext->STAT.Val = 0x84;
            }
          }
        }
      }  
      else
      if (control_transfer_state == 2)
      {
        byteToRead = pBDTEntryEP0OutCurrent->CNT;

        // Accumulate total number of bytes read
        if (byteToRead > usb_out_pipe.wCount)
            byteToRead = usb_out_pipe.wCount;
        else
            usb_out_pipe.wCount -= byteToRead;

        for(i=0;i<byteToRead;i++)
            *usb_out_pipe.pDst.bRam++ = ctrl_trf_data[i];
        //If there is more data to read
        pBDTEntryEP0OutNext->CNT = 8;
        if (usb_out_pipe.wCount > 0)
        {
            pBDTEntryEP0OutNext->ADR = PhysicalAddressOf (&ctrl_trf_data);

            if (pBDTEntryEP0OutCurrent->STAT.DTS == 0)
                pBDTEntryEP0OutNext->STAT.Val = 0xc8;
            else
                pBDTEntryEP0OutNext->STAT.Val = 0x88;
        }
        else
        {
            pBDTEntryEP0OutNext->ADR = PhysicalAddressOf(&usb_setup_pkt);
            pBDTEntryEP0OutNext->STAT.Val = 0x84;
            if (usb_out_pipe.pFunc != 0)
                usb_out_pipe.pFunc();
            usb_out_pipe.info.bits.busy = 0;
        }
       
      }  
      else
        usb_prepare_for_next_setup_trf();
    }
    else 
    {
      *(uint8*)&pBDTEntryIn[0] ^= 8;
      if (USBstate == sADDRPENDING) // pending
      {
        U1ADDR = usb_setup_pkt.bDevADR;
        if (U1ADDR > 0)
          USBstate = sADDRESSED;
        else
          USBstate = sDEFAULT;
      }
      if (control_transfer_state == 1)
      {
        pBDTEntryIn[0]->ADR = PhysicalAddressOf(ctrl_trf_data);
        usb_ctrl_trf_tx_service();

        if (short_packet_status == 2)
          pBDTEntryIn[0]->STAT.Val = 0x84; // USIE|BSTALL;
        else
        {
          if (pBDTEntryIn[0]->STAT.DTS == 0)
            pBDTEntryIn[0]->STAT.Val = 0x8c; // USIE|DAT1|DTSEN;
          else
            pBDTEntryIn[0]->STAT.Val = 0x88; //USIE|DAT0|DTSEN;
        }
      }
      else
        usb_prepare_for_next_setup_trf();      
    }
      
    U1IR = U1IR | _U1IR_TRNIF_MASK;
  }
}
 
uint8 cdc_gets(char *buffer)
{
    uint8 len = 0;
   
    cdc_rx_len = 0;
    if (data_out->STAT.UOWN)
    {
        len = data_out->CNT;
        // Copy data from dual-ram buffer to user's buffer
        for (cdc_rx_len = 0; cdc_rx_len < len; cdc_rx_len++)
            buffer[cdc_rx_len] = cdc_data_rx[cdc_rx_len];
        data_out = usb_rx_one_packet(2, (uint8*)&cdc_data_rx, sizeof(cdc_data_rx));
    }
    return len;
}

char cdc_getc()
{
  char buffer[64];
  uint8 len;
  len = cdc_gets(buffer);
  if (len > 0)
    return buffer[0];
  else
    return 0;
}

void cdc_putc(char c)
{
  if (cdc_trf_state == 0)
  {
    cdc_tx_len = 1;
    cdc_trf_state = 1;
    cdc_data_tx[0] = c;
  }
}

void cdc_puts(const char *buffer, uint8 len)
{
  if (cdc_trf_state == 0)
  {
    cdc_tx_len = len;
    cdc_trf_state = 1;
    *cdc_data_tx = *buffer;
  }
}

void cdc_tx_service()
{
  uint8 byte_to_send;

  if (data_in->STAT.UOWN) return;

  if (cdc_trf_state == 3) cdc_trf_state = 0;
  
  if(cdc_trf_state == 0)  return; // If CDC_TX_READY state, nothing to do, just return.

  if (cdc_trf_state == 2) // If CDC_TX_BUSY_ZLP state, send zero length packet
  {
    data_in = usb_tx_one_packet(2, 0, 0);
    cdc_trf_state = 3;
  }
  else if (cdc_trf_state == 1)
  {
    // First, have to figure out how many byte of data to send.
    if (cdc_tx_len > sizeof(cdc_data_tx))
      byte_to_send = sizeof(cdc_data_tx);
    else
      byte_to_send = cdc_tx_len;

    // Subtract the number of bytes just about to be sent from the total.
    cdc_tx_len = cdc_tx_len - byte_to_send;

    // Lastly, determine if a zero length packet state is necessary.
    if (cdc_tx_len == 0)
    {
      if(byte_to_send == 64)
        cdc_trf_state = 2;
      else
        cdc_trf_state = 3;
    }
    data_in = usb_tx_one_packet(2, (uint8*)&cdc_data_tx, cdc_tx_len);
  }
}

void DoCDC()
{
  InitUSB();
  while(1)
  {
    ServiceUSB();
  }
}

