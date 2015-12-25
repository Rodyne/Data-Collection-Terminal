unit uFCoder;

interface

 Type
     TDLCardType = (
                    DL_MIFARE_ULTRALIGHT		    =	 $01,
                    DL_MIFARE_ULTRALIGHT_EV1_11	=	 $02,
                    DL_MIFARE_ULTRALIGHT_EV1_21	=	 $03,
                    DL_MIFARE_ULTRALIGHT_C		  =	 $04,
                    DL_NTAG_203				          =  $05,
                    DL_NTAG_210				          =  $06,
                    DL_NTAG_212				          =  $07,
                    DL_NTAG_213				          =  $08,
                    DL_NTAG_215				          =  $09,
                    DL_NTAG_216				          =  $0A,
                    DL_MIFARE_MINI				      =  $20,
                    DL_MIFARE_CLASSIC_1K			  =  $21,
                    DL_MIFARE_CLASSIC_4K			  =  $22,
                    DL_MIFARE_PLUS_S_2K			    =  $23,
                    DL_MIFARE_PLUS_S_4K			    =  $24,
                    DL_MIFARE_PLUS_X_2K			    =  $25,
                    DL_MIFARE_PLUS_X_4K			    =  $26,
                    DL_MIFARE_DESFIRE			      =  $27,
                    DL_MIFARE_DESFIRE_EV1_2K		=  $28,
                    DL_MIFARE_DESFIRE_EV1_4K		=  $29,
                    DL_MIFARE_DESFIRE_EV1_8K		=  $2A
                    );

const
  MIFARE_AUTHENT1A = $60;
  MIFARE_AUTHENT1B = $61;
  DL_OK            = 0;
const
    DLL_NAME='uFCoder-x86.dll';
type
    DL_STATUS = LongInt;

//--- sectors and max bytes ---
const
  MAX_SECTORS_1k         = 16;
  MAX_BYTES_CLASSIC_1K   = 752;
  MAX_BLOCK        = 15;
  FORMAT_SIGN      = $00;//$FF
  KEY_INDEX        = 0;

 

function ReaderOpen: DL_STATUS stdcall;
function ReaderClose: DL_STATUS stdcall;
function ReaderUISignal(light_signal_mode: Byte;beep_signal_mode: Byte): DL_STATUS  stdcall;
function GetReaderType(var lpulReaderType: LongInt): DL_STATUS stdcall;

function GetCardIdEx(var bCardType:Byte;
                     var bCardUID :Byte;
                     var bCardUIDSize :Byte):DL_STATUS stdcall;

function BlockWrite(const data:Pointer;  bBlockAddress: Byte;  bAuthMode : Byte;  bKeyIndex : Byte): DL_STATUS  stdcall;
function BlockRead(data:PByte; block_address: Byte; auth_mode: Byte; key_index: Byte): DL_STATUS stdcall;
function GetDlogicCardType(var pCardType:Byte):DL_STATUS stdcall;

implementation

function ReaderOpen;        external  DLL_NAME;
function ReaderClose;       external  DLL_NAME;
function ReaderUISignal;    external  DLL_NAME;
function GetReaderType;     external  DLL_NAME;
function GetCardIdEx;       external  DLL_NAME;
function BlockWrite;        external  DLL_NAME;
function BlockRead;         external  DLL_NAME;
function GetDlogicCardType; external  DLL_NAME;

end.