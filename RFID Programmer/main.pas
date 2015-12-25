unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ExtCtrls, ComCtrls, uFCoder, StdCtrls;

type
  TMainform = class(TForm)
    Timer: TTimer;
    lblWriteData: TLabel;
    WriteCard: TButton;
    lblCardUID: TLabel;
    CardUID: TEdit;
    ssid: TEdit;
    password: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    broker: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    ShowPWD: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure WriteCardClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure ShowPWDClick(Sender: TObject);
  private
    Connected: Boolean;
    ERROR_CODE: array [0 .. 180] of String;
  end;

var
  Mainform: TMainform;

implementation

{$R *.dfm}

procedure TMainform.FormCreate(Sender: TObject);
begin
  ERROR_CODE[$00] := 'Completed OK';
  ERROR_CODE[$01] := 'COMMUNICATION_ERROR ';
  ERROR_CODE[$02] := 'CHKSUM_ERROR ';
  ERROR_CODE[$03] := 'READING_ERROR ';
  ERROR_CODE[$04] := 'WRITING_ERROR ';
  ERROR_CODE[$05] := 'BUFFER_OVERFLOW ';
  ERROR_CODE[$06] := 'MAX_ADDRESS_EXCEEDED ';
  ERROR_CODE[$07] := 'MAX_KEY_INDEX_EXCEEDED ';
  ERROR_CODE[$08] := 'NO_CARD ';
  ERROR_CODE[$09] := 'COMMAND_NOT_SUPPORTED ';
  ERROR_CODE[$0A] := 'FORBIDEN_DIRECT_WRITE_IN_SECTOR_TRAILER ';
  ERROR_CODE[$0B] := 'ADDRESSED_BLOCK_IS_NOT_SECTOR_TRAILER ';
  ERROR_CODE[$0C] := 'WRONG_ADDRESS_MODE ';
  ERROR_CODE[$0D] := 'WRONG_ACCESS_BITS_VALUES ';
  ERROR_CODE[$0E] := 'Authorisation Error! The card has a key set and cannot be read';
  ERROR_CODE[$0F] := 'PARAMETERS_ERROR ';
  ERROR_CODE[$10] := 'MAX_SIZE_EXCEEDED ';
  ERROR_CODE[$11] := 'Unsupported card type: Only Mifare 1K Classic RFID cards supported.';
  ERROR_CODE[$42] := 'WRITE CARD OK';
  ERROR_CODE[$50] := 'COMMUNICATION_BREAK ';
  ERROR_CODE[$51] := 'NO_MEMORY_ERROR ';
  ERROR_CODE[$52] := 'CAN_NOT_OPEN_READER ';
  ERROR_CODE[$53] := 'READER_NOT_SUPPORTED ';
  ERROR_CODE[$54] := 'READER_OPENING_ERROR ';
  ERROR_CODE[$55] := 'READER_PORT_NOT_OPENED ';
  ERROR_CODE[$56] := 'CANT_CLOSE_READER_PORT ';
  ERROR_CODE[$70] := 'WRITE_VERIFICATION_ERROR ';
  ERROR_CODE[$71] := 'BUFFER_SIZE_EXCEEDED ';
  ERROR_CODE[$72] := 'VALUE_BLOCK_INVALID ';
  ERROR_CODE[$73] := 'VALUE_BLOCK_ADDR_INVALID ';
  ERROR_CODE[$74] := 'VALUE_BLOCK_MANIPULATION_ERROR ';
  ERROR_CODE[$75] := 'WRONG_UI_MODE';
  ERROR_CODE[$76] := 'KEYS_LOCKED';
  ERROR_CODE[$77] := 'KEYS_UNLOCKED';
  ERROR_CODE[$78] := 'WRONG_PASSWORD';
  ERROR_CODE[$79] := 'CAN_NOT_LOCK_DEVICE';
  ERROR_CODE[$7A] := 'CAN_NOT_UNLOCK_DEVICE';
  ERROR_CODE[$7B] := 'DEVICE_EEPROM_BUSY';
  ERROR_CODE[$7C] := 'RTC_SET_ERROR';
  ERROR_CODE[$A0] := 'FT_STATUS_ERROR_1';
  ERROR_CODE[$A1] := 'FT_STATUS_ERROR_2';
  ERROR_CODE[$A2] := 'FT_STATUS_ERROR_3';
  ERROR_CODE[$A3] := 'FT_STATUS_ERROR_4';
  ERROR_CODE[$A4] := 'FT_STATUS_ERROR_5';
  ERROR_CODE[$A5] := 'FT_STATUS_ERROR_6';
  ERROR_CODE[$A6] := 'FT_STATUS_ERROR_7';
  ERROR_CODE[$A7] := 'FT_STATUS_ERROR_8';
  ERROR_CODE[$A8] := 'FT_STATUS_ERROR_9';
  Connected := false;
end;

procedure TMainform.ShowPWDClick(Sender: TObject);
begin
  if ShowPWD.checked then password.passwordChar:=#0 else password.passwordchar:='#';
end;

procedure TMainform.WriteCardClick(Sender: TObject);
var
  FnResult:byte;
  wData: PByte;
  procedure WriteRFID(blockNo:byte; data:ansistring);
  begin
    wData:=PByte(data);
    Fnresult:=BlockWrite(wData, blockNo, MIFARE_AUTHENT1A, KEY_INDEX);
    if FnResult<>DL_OK then exception.Create(ERROR_CODE[FnResult]);
  end;
begin
  timer.enabled:=false;
  try
    New(wData);
    WriteRFID(1,ssid.text);
    WriteRFID(2,password.Text);
    WriteRFID(4,Broker.text);
    messageDLG('RFID CARD Written OK',mtinformation,[mbok],0);
  finally
    wData := nil;
    Dispose(wData);
    timer.enabled:=true;
  end;
end;


procedure TMainform.TimerTimer(Sender: TObject);
var
  iReaderType, iRResult, iCResult: Longint;
  bCardUIDSize, bCount, bCardType: Byte;
  baCardUID: array [0 .. 9] of Byte;
  sBuffer: ShortString;
  bDLCardType: Byte;
begin
  timer.enabled:=false; // stop recursion
  timer.tag:=timer.tag+1;
  try
    sBuffer := '';
    if not Connected then
    begin
      CardUID.text:=' - WAIT -';
      connected:= ReaderOpen() = DL_OK;
    end;
    if connected then
    begin
      iRResult := GetReaderType(iReaderType);
      if iRResult = DL_OK then
      begin
        iCResult := GetDlogicCardType(bDLCardType);
        if iCResult = DL_OK then
        begin
          iCResult := GetCardIdEx(bCardType, baCardUID[0], bCardUIDSize);
          if iCResult = DL_OK then
          begin
            for bCount := 0 to bCardUIDSize - 1 do
            begin
              sBuffer := sBuffer + IntToHex(baCardUID[bCount], 2);
            end;
          end;
          CardUID.Text := '0x' + sBuffer;
        end
        else
          CardUID.text:=' - WAIT -';
      end
      else
      begin
        ReaderClose();
        CardUID.text:=' - WAIT -';
      end;
    end;
    WriteCard.enabled:=(sbuffer<>'') and (bDLCardType = Byte(DL_MIFARE_CLASSIC_1K));
  finally
    timer.enabled:=true;
  end;
end;

end.
