unit MainUnit;

// Implements a multi-Threaded TCP Server to listen for Mobile client requests on specified port.
// As request is received it is checked for validity and the message processed via the DB database connection to bulls-i.
// Most of the TCP code copied from the synapser echo example and modified for this app. (ie all the clever bits are someone elses!)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, mysql55conn, sqldb, FileUtil, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls, Buttons, blcksock, synsock, DateUtils,
  Configuration;

const
  BOUNDPORT  = '2001';
  MAXDEVICES = 64;

  NULL       = #$00;
  SOH        = #$01;
  STX        = #$02;
  ETX        = #$03;
  EOT        = #$04;
  ACK        = #$06;
  NACK       = #$15;
  TAB        = #$09;

  cr = cr+lf;

type

  TTCPDaemon = class(TThread)
  private
    Sock:TTCPBlockSocket;
  public
    Constructor Create;
    Destructor Destroy; override;
    procedure Execute; override;
  end;

  TTCPThread = class(TThread)
  private
    Sock:TTCPBlockSocket;
    CSock: TSocket;
  public
    Constructor Create (hsock:tSocket);
    procedure Execute; override;
  end;


  { TMainForm }

  TMainForm = class(TForm)
    ConfigurationButton: TBitBtn;
    Label2: TLabel;
    Label3: TLabel;
    DevFree: TLabel;
    label22: TLabel;
    DevInUse: TLabel;
    Label4: TLabel;
    Panel4: TPanel;
    Panel5: TPanel;
    q: TSQLQuery;
    RefreshDeviceList: TButton;
    SQLLog: TMemo;
    HousekeepingTimer: TTimer;
    SystemLog: TMemo;
    DeviceList: TListBox;
    pc: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet3: TTabSheet;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    procedure ConfigurationButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure RefreshDeviceListClick(Sender: TObject);
  private
    procedure Updatelogs;
  end;

var
  MainForm: TMainForm;
  logEvent,LogSQL:string;

implementation

{$R *.lfm}

function  AsHex(input:string):string; // print a string as hexidecimal bytes
var
  i:integer;
begin
  result:='';
  for i:=1 to length(input) do
  begin
    result:=result+inttohex(ord(input[i]),2)+' ';
  end;
end;

procedure TMainForm.Updatelogs; // events are produced in a thread which cannot update main program except through the Synchronize procedure which calls this
begin
  if application.terminated then exit;
  try
    if LogEvent<>'' then SystemLog.Lines.Add(cr+cr+formatdatetime('hh:nn:ss',now)+' '+logevent);
    if logSQL<>'' then SQLLog.Lines.Add(cr+cr+formatdatetime('hh:nn:ss',now)+' '+logSQL);
  except
  end;
  logEvent:='';
  LogSQL:='';
end;

procedure TMainForm.ConfigurationButtonClick(Sender: TObject);
begin
  if Configurationform.ShowModal=mrok then Formshow(nil);
end;

Constructor TTCPDaemon.Create;
begin
  inherited create(false);
  sock:=TTCPBlockSocket.create;
  FreeOnTerminate:=true;
end;

Destructor TTCPDaemon.Destroy;
begin
  Sock.free;
end;

Constructor TTCPThread.Create(Hsock:TSocket);
begin
  inherited create(false);
  Csock := Hsock;
  FreeOnTerminate:=true;
end;

procedure TTCPDaemon.Execute;
var
  ClientSock:TSocket;
begin
  with sock do
  begin
    CreateSocket;
    setLinger(true,10000);
    bind('0.0.0.0',BOUNDPORT);
    listen;
    repeat
      if canread(1000) then
      begin
        ClientSock:=accept;
        if lastError=0 then TTCPThread.create(ClientSock);
      end;
    until terminated;
  end;
end;

// This is the function where the TCP stuff happens, messages to and from the client are very small (<1K)
procedure TTCPThread.Execute;
var
  DeviceID, QueryType, QueryDesc, QueryCode, ReportFormat, RawData, ResultString, ThreadLog, ThreadLogSQL, AssignedUserID, AssignedUserName : string;
  HardwareRev, FirmwareRev, FunctionCode, InputFieldCount, OutputFieldCount : byte;
  j,i:integer;
  q2,q3:TSQLQuery;
  querytime:tdatetime;
  field:array[1..4] of string;

begin
  querytime:=now;
  q2:=nil;
  q3:=nil;
  DeviceID:='';
  sock:=TTCPBlockSocket.create;
  try
    Sock.socket:=CSock;
    sock.GetSins;
    if sock.GetRemoteSinIP='' then exit;
    ThreadLog:='Device @'+sock.GetRemoteSinIP+' has Connected.'+cr;
    try
      RawData:=Sock.RecvTerminated(2000,EOT); // wait for data packet to be received (up to 2 seconds but should be instant)

      if Sock.lastError<>0 then raise exception.create(sock.LastErrorDesc);

      if RawData='' then exit; // nothing to do!

      threadlog:=threadlog+' >RECEIVED: '+AsHex(RawData)+cr; // log what was received (in hex)

      // request from mobile is: SOH + FnCode + STX + field1 + TAB + field2 + TAB + field3 + TAB + field4 + ETX + HWRevByte + SoftwareRevByte + EOT
      // response to mobile is: SOH + ACK/NACK + STX + field1 + TAB + field2 + TAB +  .......  + field9 ETX + EOT
      // Blank fields are still transmitted with TAB as delimeter as far easier for mobile to generate / decode (it only has a few K's ram, god bless the little bugger!)
      // note the service is stateless and packet must contain all the data required for the action.
      // start by parsing message string from mobile device

      if length(RawData)<10 then raise exception.create('Invalid header Packet rejected (Packet too short)');
      if RawData[1]<>SOH then raise exception.Create('Invalid header Packet rejected (No SOH)');
      if RawData[3]<>STX then raise exception.Create('Invalid header Packet rejected (No STX)');

      j:=4; // start of where tab-delimited-data starts in message

      for i:=1 to 4 do // parse the 4 fields from input data string in to field[] array
      begin
        field[i]:='';
        repeat
        begin
          if ord(RawData[j])>31 then field[i]:=field[i]+RawData[j];
          inc(j);
        end;
        until (RawData[j]=TAB) or (j>length(RawData)-2);
        threadlog:=threadlog+' >Field['+inttostr(i)+'] = "'+field[i]+'" (j='+inttostr(j)+')'+cr;
      end;

      inc(j);

      if RawData[j]<>ETX then raise exception.Create('Invalid header Packet rejected (ETX not at position '+inttostr(j)+')'); // just a check

      // parse the other data from message
      FunctionCode := ord(RawData[2]);
      HardwareRev := ord(RawData[j+1]);
      FirmwareRev := ord(RawData[j+2]);

      // NOTE: Everything stored/retreived from mySQL database NOT memory (system is stateless) and must be read/written real time (slower but we are not dealing with many transactions)

      // create two query components for querying database (we cannot use VCL as it is multi-threaded)
      q2:=TSQLQuery.Create(nil);
      q2.DataBase:=Configurationform.DB;
      q2.Transaction:=Configurationform.Transaction;
      q3:=TSQLQuery.Create(nil);
      q3.DataBase:=Configurationform.DB;
      q3.Transaction:=Configurationform.Transaction;

      // get the record for this IP address
      q2.sql.text:='SELECT DeviceID, AssignedUser FROM mobiledevices WHERE IPAddress="'+sock.GetRemoteSinIP+'"';
      q2.open;

      if q2.recordcount=0 then // no record of this IP address so add one (but dont action as it hasnt be assigned a user)
      begin
        q3.sql.text:='INSERT INTO mobiledevices (IPAddress,ConfigStr,LastPing,SessionLog,DeviceLog) VALUES("'+sock.GetRemoteSinIP+'","'+inttostr(HardwareRev)+'.'+inttostr(FirmwareRev)+'",NOW(),"Registered\r\n","New device\r\n")';
        q3.execSQL;
        // log and send back `not assigned to user` error code
        threadlog:=threadlog+' >New Device @ '+sock.GetRemoteSinIP+' Registered In'+cr;
        sock.SendString(SOH + NACK + STX + '1' + TAB + 'Not Assigned!' + TAB + TAB + ETX + EOT);
        Mainform.RefreshDeviceListClick(nil);
        exit;
      end;

      if q2.fieldbyname('AssignedUser').asstring='' then // exists in table but no user assigned so again just reject with error code `not assigned to user`
      begin
        threadlog:=threadlog+' >No User Assigned to device. Rejecting request.'+cr;
        sock.SendString(SOH + NACK + STX + '1' + TAB + 'Not Assigned!' + TAB + TAB + TAB + TAB + TAB + TAB + TAB + TAB + TAB + ETX + EOT); // send error back
        exit;
      end;

      // all seems good lets get and run the query

      DeviceID:=q2.fieldbyname('DeviceID').asstring; // get the device ID (as we will be updating record later on)
      q2.close; // finished with q2 now

      // Use Q3 to Lookup functioncode
      if q3.active then q3.close;
      q3.sql.text:='SELECT * FROM mobilequeries WHERE FunctionCode = '+ inttostr(FunctionCode);
      q3.open;

      if q3.recordcount=0 then // Hmmm, something suspicious here no such function code so reject with error msg.
      begin
        threadlog:=threadlog+' >### CHECK! Device Requested unsupported function code. Rejecting request. ###'+cr;
        sock.SendString(SOH + NACK + STX + '2' + TAB + 'Unsupported Fn!' + TAB + TAB + TAB + TAB + TAB + TAB + TAB + TAB + TAB + ETX + EOT); // send error back
        exit;
      end;

      // cach query data before actioning (as we re-use q3 to run the actual query)
      QueryType:=q3.fieldbyname('QueryType').asstring;
      QueryDesc:=q3.fieldbyname('QueryDesc').asstring;
      QueryCode:=q3.fieldbyname('QueryCode').asstring;
      ReportFormat:=q3.fieldbyname('ReportFormat').asstring;
      InputFieldCount:=q3.fieldbyname('InputFieldCount').asinteger;
      OutputFieldCount:=q3.fieldbyname('OutputFieldCount').asinteger;
      q3.close;

      // substitute fields from mobile message into query to form the actual query we require for the MRP system
      q3.sql.text:=StringReplace(QueryCode,  'InputField1',Field[1],[]);
      q3.sql.text:=StringReplace(q3.sql.text,'InputField2',Field[2],[]);
      q3.sql.text:=StringReplace(q3.sql.text,'InputField3',Field[3],[]);
      q3.sql.text:=StringReplace(q3.sql.text,'InputField4',Field[4],[]);

      ThreadLogSQL:=q3.sql.text; // for logging

      if QueryType='GET' then // get (SELECT QUERY)
      begin
        threadlog:=threadlog+' >(GET) Running Query: '+QueryDesc+cr;
        q3.open;
        if q3.RecordCount<>1 then threadlog:=threadlog+' >WARNING: GET Query did not return unique record! Only first record returned (See SQL Log)!'+cr;
        ResultString:=SOH + ACK + STX;
        for i:=1 to OutputfieldCount do
          resultString:=resultString + q3.Fieldbyname('OutputField'+inttostr(i)).AsString + TAB;
        for i:=OutputfieldCount to 10 do
          resultString:=resultString + TAB;
        resultString:=resultString+ ETX + EOT;
        sock.SendString(resultString);
        exit;
      end;

      if QueryType='SET' then // (SET) just execute and send ack
      begin
        threadlog:=threadlog+' >(SET) Executing Query: '+QueryDesc+cr;
        q3.ExecSQL;
        ResultString:=SOH + ACK + STX;
        for i:=1 to 10 do
          resultString:=resultString + TAB;
        resultString:=resultString+ ETX + EOT;
        sock.SendString(resultString);
      end;

      if QueryType='CMD' then // most difficult because we need to print a report on a printer. so the program needs a report component
      begin
        threadlog:=threadlog+' >(PRT) Running Report: '+QueryDesc+cr;
        q3.Open;
        // TO-DO Run report!
        ResultString:=SOH + ACK + STX;
        for i:=1 to 10 do
          resultString:=resultString + TAB;
        resultString:=resultString+ ETX + EOT;
        sock.SendString(resultString);
      end;

    except
      on e:exception do threadlog:=threadlog+e.Message+cr;// Catch all errors + log them them (send nothing to device it will timeout in a second or two and interperet that as a generic error!)
    end;
  finally
    if q3.active then q3.close;
    q3.sql.text:='UPDATE mobiledevices SET Sessionlog=CONCAT(SessionLog,"\r\n",NOW(),"'+safestring(ThreadLog)+'") WHERE DeviceID='+DeviceID;
    q3.ExecSQL;
    configurationform.Transaction.CommitRetaining;
    q2.free;
    q3.free;
    LogSQL:=ThreadLogSQL;
    LogEvent:=ThreadLog+' >Query Time '+formatfloat('#0.000',(now-QueryTime)*24*60*60)+cr+cr;
    Synchronize(@mainform.Updatelogs); //
    Sock.Free;
  end;
end;

procedure TMainForm.RefreshDeviceListClick(Sender: TObject);
begin
  refreshDeviceList.Enabled:=false;
  enabled:=false;
  screen.Cursor:=crSQLWait;
  pc.ActivePageIndex:=0;
  try
    DeviceList.Items.Clear;
    if q.Active then q.Close;
    q.Open;
    while not q.eof do
    begin
      DeviceList.Items.AddObject(q.FieldByName('DeviceInfo').asstring, tobject(q.FieldByName('DeviceID').asinteger) );
      q.next;
    end;
    exec('UPDATE mobiledevices SET LastPing=NOW() WHERE DeviceID=0'); // let any external programs know we are updating
  finally
    enabled:=true;
    refreshDeviceList.Enabled:=true;
    screen.Cursor:=crdefault;
  end;

end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  SQLLog.text:='';
end;


procedure TMainForm.FormShow(Sender: TObject);
begin
  cfg.TempDir:=SysUtils.GetTempDir;
  repeat
    try
      if not ConfigurationForm.ReadMySQLConfig then abort;
      ConfigurationForm.DB.open;
      SystemLog.text:='INIT: Connected to '+ConfigurationForm.DB.DatabaseName+' on '+ConfigurationForm.DB.HostName+' OK';
      caption:= 'Mobile Terminal Control Centre. Using database '+ConfigurationForm.DB.DatabaseName+'@'+ConfigurationForm.DB.HostName+'. Listening on Port '+BOUNDPORT;
      // next bit doesnt actually work if you have autoincrement DeviceID column (dohh!)
      if getquery('SELECT LastPing FROM mobiledevices WHERE DeviceId=0','')='' then
        exec('INSERT INTO mobiledevices (DeviceID,IPAddress,LastPing) VALUES(0,"localhost",NOW())');
      // open socket and listen on bound port
      TTCPDaemon.create;
      RefreshDeviceListClick(nil);
      HousekeepingTimer.Enabled:=true;
    except
      if ConfigurationForm.showmodal=mrcancel then
      begin
        messageDLG('Cannot connect to MySQL server. Program ending!',mtinformation,[mbok],0);
        application.Terminate;
      end;
    end;
  until ConfigurationForm.DB.connected;
end;


end.

