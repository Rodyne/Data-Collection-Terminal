unit MainUnit;

// Implements a multi-Threaded TCP Server to listen for Mobile client requests on specified port.
// As request is received it is checked for validity and the message processed via the DB database connection to bulls-i.
// Most of the TCP code copied from the synapser echo example and modified for this app. (ie all the clever bits are someone elses!)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, mysql55conn, sqldb, db, FileUtil, Forms, Controls,
  Graphics, Dialogs, ExtCtrls, StdCtrls, ComCtrls, Buttons, DBGrids, blcksock,
  synsock, DateUtils, Configuration, EditFunction;

type

  TTCPDaemon = class(TThread)
  private
    Sock:TTCPBlockSocket;
  public
    Constructor Create;
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
    dsDev: TDataSource;
    dt: TLabel;
    FnActive: TCheckBox;
    dsFn: TDataSource;
    gFn: TDBGrid;
    FnAdd: TButton;
    FnDelete: TButton;
    FnRefresh: TButton;
    gDev: TDBGrid;
    Label2: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    info: TLabel;
    Panel7: TPanel;
    Panel8: TPanel;
    DevRefresh: TButton;
    CreateTables: TSQLQuery;
    SetupButton: TBitBtn;
    Panel4: TPanel;
    Panel6: TPanel;
    qDevLog: TSQLQuery;
    HousekeepingTimer: TTimer;
    qFn: TSQLQuery;
    pc: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet3: TTabSheet;
    procedure ClrSQLLogClick(Sender: TObject);
    procedure FnAddClick(Sender: TObject);
    procedure FnDeleteClick(Sender: TObject);
    procedure FnRefreshClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure pcChange(Sender: TObject);
    procedure SetupButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HousekeepingTimerTimer(Sender: TObject);
    procedure DevRefreshClick(Sender: TObject);
  private
    sql:String;
    init:integer;
  end;

var
  MainForm: TMainForm;
  UpdateRequired:boolean;

implementation

{$R *.lfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  init:=0;
  if qfn.Active then qfn.close;
  if qDevLog.Active then qDevLog.close;
  sql:=qFn.sql.text;
  pc.ActivePageIndex:=0;
end;

procedure TMainForm.SetupButtonClick(Sender: TObject);
begin
  if Configurationform.ShowModal=mrok then Formshow(nil);
end;

procedure TMainForm.ClrSQLLogClick(Sender: TObject);
begin

end;

procedure TMainForm.FnRefreshClick(Sender: TObject);
begin
  if init=0 then exit;
  FnRefresh.enabled:=false;
  try
    if qFn.Active then qFn.Close;
    qFn.SQL.text:=sql;
    if FnActive.Checked then qFn.sql.text:=qFn.sql.text+cr+'WHERE QueryFlags&1';
    qFn.sql.text:=qFn.sql.text+cr+'ORDER BY FnCode';
    qFn.Open;
    FnDelete.Enabled:=qFn.RecordCount>0;
  finally
    FnRefresh.Enabled:=true;
    HousekeepingTimer.enabled:=true;
  end;
end;

procedure TMainForm.FnAddClick(Sender: TObject);
begin
  if sender=FnAdd then
    EditFunctionForm.QID:=''
  else
    EditFunctionForm.QID:=qFn.fieldbyname('Ref').asstring;
  if EditFunctionform.showmodal=mrok then
  begin
    FnRefreshClick(nil);
    qFn.Locate('Ref',EditFunctionForm.QID,[]);
  end;
end;

procedure TMainForm.FnDeleteClick(Sender: TObject);
begin
  if qfn.recordcount>0 then
    if messageDLG('DELETE Query Ref '+qfn.fieldbyname('Ref').asstring+'! You Sure?',mtconfirmation,[mbyes,mbno],0)=mryes then
    begin
      exec('DELETE FROM mobilequeries WHERE QID='+qfn.fieldbyname('Ref').asstring);
      FnRefreshClick(nil);
    end;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  cfg.TempDir:=SysUtils.GetTempDir;
  repeat
    try
      if not ConfigurationForm.ReadMySQLConfig then abort;
      ConfigurationForm.DB.open;
      info.caption:= 'Using database '+ConfigurationForm.DB.DatabaseName+'@'+ConfigurationForm.DB.HostName+'. Listening on Port 2001';
      // open socket and listen on bound port
      TTCPDaemon.create;
      init:=1;
      pcChange(nil);
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

procedure TMainForm.HousekeepingTimerTimer(Sender: TObject);
begin
  if UpdateRequired then DevRefreshClick(nil);
  dt.caption:=formatdatetime('hh:nn:ss',now);
end;

procedure TMainForm.pcChange(Sender: TObject);
begin
  if pc.ActivePageIndex=0 then DevRefreshClick(nil);
  if pc.ActivePageIndex=1 then FnRefreshClick(nil);
end;

procedure TMainForm.DevRefreshClick(Sender: TObject);
begin
  if init=0 then exit;
  if not Configurationform.db.Connected then exit;
  HousekeepingTimer.enabled:=false;
  UpdateRequired:=false;
  DevRefresh.Enabled:=false;
  screen.Cursor:=crSQLWait;
  try
    if qDevLog.Active then qDevLog.Close;
    qDevLog.Open;
  finally
    screen.Cursor:=crdefault;
    DevRefresh.Enabled:=true;
    HousekeepingTimer.enabled:=true;
  end;
end;

Constructor TTCPDaemon.Create;
begin
  inherited create(false);
  sock:=TTCPBlockSocket.create;
  FreeOnTerminate:=true;
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
    bind('0.0.0.0','2001');
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

// ALL the TCP broker stuff happens here. System is stateless, Everything stored/retreived from mySQL database NOT memory
procedure TTCPThread.Execute;
var
  QueryType, FnCode, UserID, QueryCode, RxData, TxData: string;
  i:integer;
  q2:TSQLQuery;
  querytime:tdatetime;
begin
  TxData:='';
  UserID:='';
  q2:=nil;
  querytime:=now;
  sock:=TTCPBlockSocket.create;
  try
    sock.socket:=CSock;
    sock.GetSins;
    if sock.GetRemoteSinIP='' then exit;
    try
      RxData:=Sock.RecvString(2000); // wait for data packet to be received (note with receivestring method the data must have \r\n at end!)
      if Sock.lastError<>0 then raise exception.create(sock.LastErrorDesc);

      // CreateTables new query components for querying database (we cannot use static VCL components as it is multi-threaded)
      q2:=TSQLQuery.Create(nil);
      q2.DataBase:=Configurationform.DB;
      q2.Transaction:=Configurationform.Transaction;

      // Get functioncode and read in query
      FnCode:=TXTField(RxData,1);
      q2.sql.text:='SELECT * FROM mobilequeries WHERE FnCode = "'+FnCode+'"';
      q2.open;
      if q2.recordcount=0 then raise exception.create('Bad Function Code '+FnCode+' Was sent by the terminal and was ignored');
      QueryType:=q2.fieldbyname('QueryType').asstring; // GET or SET
      QueryCode:=q2.fieldbyname('QuerySQL').asstring; // SQL
      q2.close;

      // substitute fields from mobile message into query to form the actual query we require for the MRP system

      q2.sql.text:=StringReplace(QueryCode,  'InputField1',TXTField(RxData,1),[rfReplaceAll]); // fn code
      q2.sql.text:=StringReplace(q2.sql.text,'InputField2',TXTField(RxData,2),[rfReplaceAll]); // deviceid
      q2.sql.text:=StringReplace(q2.sql.text,'InputField3',TXTField(RxData,3),[rfReplaceAll]); // users pasword md5 hash
      q2.sql.text:=StringReplace(q2.sql.text,'InputField4',TXTField(RxData,4),[rfReplaceAll]); // barcode (or rfid no)
      q2.sql.text:=StringReplace(q2.sql.text,'InputField5',TXTField(RxData,5),[rfReplaceAll]); // metric or blank

      // substitutions done. Run q2 to get results

      if QueryType='GET' then // get (SELECT QUERY)
      begin
        TxData:=q2.sql.text;
        q2.open;
        if q2.RecordCount=0 then  raise exception.create('GET Query did not return a record!');   // Nothing sent to client! something dodgy may also be happening ie another program maybe spoofing the connection!
        if q2.RecordCount<>1 then raise exception.Create('GET Query must return unique record!'); // Multiple records returned is also bad so nothing sent to client - See SQL Log
        TxData:='';
        for i:=0 to q2.FieldCount-1 do
        begin
          if q2.FindField('OutputField'+inttostr(i))=nil then
            TxData:=TxData + DELIMITER
          else
            TxData:=TxData + q2.Fieldbyname('OutputField'+inttostr(i)).AsString + DELIMITER;
        end;
        sock.SendString(PREFIX + TxData + POSTFIX);
      end;

      if QueryType='SET' then // (SET) just execute and send ack, note SET Commands require terminal to be assigned to a user!
      begin
        if TXTField(RxData,3)<>'' then UserID:=GetQuery('SELECT AccountID FROM account WHERE pin="'+TXTField(RxData,3)+'"','');
        q2.ExecSQL;
        sock.SendString(PREFIX + '0'+DELIMITER+DELIMITER+DELIMITER+DELIMITER+DELIMITER+DELIMITER+DELIMITER+DELIMITER+DELIMITER+DELIMITER + POSTFIX); // return 0 all ok
      end;

    except
      on e:exception do TxData:=e.Message;
    end;
  finally
    if q2<>nil then
    begin
      RxData:=stringreplace(copy(RxData,1,99),DELIMITER,',',[rfReplaceAll]);
      TxData:=stringreplace(copy(TxData,1,500),DELIMITER,',',[rfReplaceAll]);
      q2.sql.text:='INSERT INTO mobiledevicelog (LogTime,IPAddress,UID,AssignedUser,RxData,TxData,RunTimeMs) VALUES(NOW(),"'+sock.GetRemoteSinIP+'","'+TXTField(RxData,2)+'","'+TXTField(RxData,3)+'","'+safestring(RxData)+'","'+safestring(TxData)+'",'+inttostr(trunc((QueryTime-now())*1000))+')';
      q2.execSQL;
      q2.free;
      UpdateRequired:=true;
    end;
    Sock.Free;
  end;
end;


end.

