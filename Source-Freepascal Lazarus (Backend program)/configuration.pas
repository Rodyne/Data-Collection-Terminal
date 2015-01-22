unit configuration;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, mysql55conn, sqldb, FileUtil, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, Buttons, StdCtrls;

const
  ConnFilename = 'mysql.connection';

type

  { TConfigurationForm }

  // type for storing scambled string value
  Tscramble=array[1..30] of integer;

  // mysql connection parameters, use fixed string length as it wil be saved as record to a file
  Tconn=record
    pwd: Tscramble;
    Port:integer;
    server,db,user:string[30];
  end;

  tlog=(sys,sql);

  // type for global working variables, discarded on exit
  Tcfg=Record
    TempDir,SiteName :string;
  end;

  TConfigurationForm = class(TForm)
    ShowPassword: TCheckBox;
    db: TMySQL55Connection;
    MySQLPassword1: TLabel;
    MySQLServer: TEdit;
    MySQLDatabase: TEdit;
    MySQLPassword: TEdit;
    MySQLUsername: TEdit;
    MySQLPort: TEdit;
    SaveButton: TBitBtn;
    CancelButton: TBitBtn;
    Label1: TLabel;
    sdsdsd: TLabel;
    xxx: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Transaction: TSQLTransaction;
    procedure CancelButtonClick(Sender: TObject);
    procedure dbLog(Sender: TSQLConnection; EventType: TDBEventType; const Msg: String);
    procedure FormShow(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure ShowPasswordChange(Sender: TObject);
  public
    function ReadMySQLConfig:boolean;
  end;

  // NOTE System must be run stateless, ie every packet has everything

var
  ConfigurationForm: TConfigurationForm;
  ConnData:Tconn;
  Connfile:file of tconn;
  cfg:tcfg;

  Function TABfield(data:string; n:byte):string;// Splits out the nth field from a TAB delimited record or returns empty string if none exists!
  function SQLDate(dt:Tdate):string;
  function IsNumber(input:string):boolean;
  function IsReal(input:string):boolean;
  procedure Exec(sql:string); // execute statement
  function GetQuery(sql:string; default:string):string; overload; // run a simple SQL query returning 1 value
  function GetQuery(sql:string; default:integer):integer; overload; // run a simple SQL query returning 1 value
  function SafeString(input:string):string; // delimit (escape) user mysql strings prior to use to prevent errors and injection attack (note, I dont delimit _ and % as I use them in user searches)
  procedure LoadList(sql:string; list:Tstrings);


implementation

uses MainUnit;

{$R *.lfm}

{ TConfigurationForm }


function SafeString(input:string):string; // delimit (escape) user mysql strings prior to use to prevent errors and injection attack (note, I dont delimit _ and % as I use them in user searches)
begin
  Result:=input;
  Result:=StringReplace(Result,'\', '\\', [ rfReplaceAll ]); // backslash
  Result:=StringReplace(Result,#39, '\'+#39, [ rfReplaceAll ]); // single-quote '
  Result:=StringReplace(Result,#34, '\'+#34, [ rfReplaceAll ]); // double-quote "
  Result:=StringReplace(Result,#0,  '\0', [ rfReplaceAll ]); // null (#0)
  Result:=StringReplace(Result,#8,  '\b', [ rfReplaceAll ]); // backspace
  Result:=StringReplace(Result,#10, '\n', [ rfReplaceAll ]); // newline
  Result:=StringReplace(Result,#13, '\r', [ rfReplaceAll ]); // cr
  Result:=StringReplace(Result,#9,  '\t', [ rfReplaceAll ]); // tab
  Result:=StringReplace(Result,#26, '\Z', [ rfReplaceAll ]); // EOF
end;

function scramble(ClearText:string):Tscramble; var i:integer; // simple XOR scramble fn so we do not store things on disk in plain text
begin
  ClearText:=ClearText+#0;
  for i:=1 to 30 do
    result[i]:=ord(ClearText[i]) xor i;
end;

function unscramble(input:Tscramble):string; var  i:integer; // simple XOR unscramble fn
begin
  result:='';
  for i:=1 to 30 do
  begin
    if ord(input[i]) xor i=0 then break;
    result:=result+chr(ord(input[i]) xor i);
  end;
end;

Function TABfield(data:string; n:byte):string;// Splits out the nth field from a TAB delimited record or returns empty string if none exists!
const
  TAB = #9;
var
  fCount,pos:byte;
begin
  result:='';
  if data='' then exit;
  pos:=1;
  Fcount:=1; // start field count at first field
  repeat
  begin
    if data[pos]=TAB then //  next field
      inc(Fcount)
    else
    if Fcount=n then // at correct field
      result:=result+data[pos]; // so add to result
    inc(pos); // process next character..
  end;
  until (fcount>n) or (pos>length(data));
  result:=trim(result);
end;

function SQLDate(dt:Tdate):string;
begin
  if dt<now-9999 then result:='NULL' else result:=formatdatetime('yyyymmdd',dt);
end;

function IsNumber(input:string):boolean;
begin
  try
    if strtofloat(input)<-5 then abort;
    result:=true;
  except
    result:=false;
  end;
end;

function IsReal(input:string):boolean;
begin
  try
    if strtofloat(input)<-999 then abort;
    result:=true;
  except
    result:=false;
  end;
end;

procedure Exec(sql:string); // execute statement. return insertid of record if created
var
  q1:TSQLQuery;
begin
  if sql='' then exit;
  q1:=nil;
  screen.cursor:=crSQLwait;
  try
    q1:=TSQLQuery.Create(nil);
    q1.DataBase:=Configurationform.DB;
    q1.Transaction:=Configurationform.Transaction;
    q1.SQL.Text:=sql;
    application.ProcessMessages;
    q1.ExecSQL;
    configurationform.Transaction.CommitRetaining;
  finally
    q1.Free;
    screen.cursor:=crdefault;
  end;
end;

function GetQuery(sql:string; default:string):string; overload; // run a simple SQL query returning 1 value
var
  q1:TSQLQuery;
begin
  q1:=nil;
  result:=default;
  if sql='' then exit;
  screen.cursor:=crSQLwait;
  try
    q1:=TSQLQuery.Create(nil);
    q1.DataBase:=Configurationform.DB;
    q1.Transaction:=Configurationform.Transaction;
    q1.SQL.Text:=sql;
    application.ProcessMessages;
    q1.open;
    if NOT q1.Fields[0].IsNull then result:=q1.Fields[0].Asstring; // return first result
  finally
    q1.Free;
    screen.cursor:=crdefault
  end;
end;

function GetQuery(sql:string; default:integer):integer; overload; // run a simple SQL query returning 1 value
var
  q1:TSQLQuery;
begin
  q1:=nil;
  result:=default;
  if sql='' then exit;
  screen.cursor:=crSQLwait;
  try
    q1:=TSQLQuery.Create(nil);
    q1.DataBase:=Configurationform.DB;
    q1.Transaction:=Configurationform.Transaction;
    q1.SQL.Text:=sql;
    application.ProcessMessages;
    q1.open;
    if NOT q1.Fields[0].IsNull then result:=q1.Fields[0].Asinteger; // return first result
  finally
    q1.Free;
    screen.cursor:=crdefault
  end;
end;

procedure LoadList(sql:string; list:Tstrings);
var
  q1:TSQLQuery;
begin
  q1:=nil;
  screen.cursor:=crSQLwait;
  try
    q1:=TSQLQuery.Create(nil);
    q1.DataBase:=Configurationform.DB;
    q1.Transaction:=Configurationform.Transaction;
    q1.SQL.Text:=sql;
    q1.open;
    list.Clear;
    while not q1.EOF do
    begin
      if q1.FieldCount=1 then
        list.Add(q1.Fields[0].asstring)
      else
        list.AddObject(q1.Fields[0].asstring, tobject(q1.fields[1].asinteger));
    end;
  finally
    q1.Free;
    screen.cursor:=crdefault
  end;
end;

function TConfigurationForm.ReadMySQLConfig:boolean;
begin
  result:=false;
  if not fileexists(Connfilename) then exit;
  assignfile(connFile,ConnFilename);
  try
    reset(connFile);
    read(connFile,ConnData);
    closefile(Connfile);
    db.HostName:=ConnData.server;
    db.Port:=ConnData.port;
    db.UserName:=Conndata.user;
    db.DatabaseName:=Conndata.db;
    db.Password:=unscramble(ConnData.pwd);
    result:=true;
  except
  end;
end;

procedure TConfigurationForm.ShowPasswordChange(Sender: TObject);
begin
  if ShowPassword.Checked then
    MySQLPassword.PasswordChar:=#0
  else
    MySQLPassword.PasswordChar:='#';
end;

procedure TConfigurationForm.FormShow(Sender: TObject);
begin
  MySQLServer.Text:='';
  MySQLPort.text:='3306';
  MySQLUsername.text:='';
  MySQLPassword.text:='';
  MySQLDatabase.text:='';
  if ReadMySQLConfig then
  begin
    MySQLServer.Text:=db.HostName;
    MySQLPort.text:=inttostr(db.Port);
    MySQLUsername.text:=db.UserName;
    MySQLPassword.text:=db.Password;
    MySQLDatabase.text:=db.DatabaseName;
  end;
  CancelButton.SetFocus;
end;

procedure TConfigurationForm.SaveButtonClick(Sender: TObject);
begin
  SaveButton.Enabled:=false;
  enabled:=false;
  screen.Cursor:=crSQLWait;
  try
    try
      db.HostName:=MySQLServer.text;
      db.Port:=strtoint(MySQLPort.Text);
      db.UserName:=MySQLUsername.text;
      db.Password:=MySQLPassword.text;
      db.DatabaseName:=MySQLDatabase.text;
      db.Connected:=true;
      db.Connected:=false;
      assignfile(connFile,ConnFilename);
      rewrite(connFile);
      ConnData.server:=db.HostName;
      ConnData.port:=db.Port;
      Conndata.user:=db.UserName;
      Conndata.db:=db.DatabaseName;
      ConnData.pwd:=scramble(db.Password);
      write(connFile,ConnData);
      closefile(Connfile);
      Modalresult:=mrok;
    except
      on e:exception do MessageDLG('Cannot Save. Specific error is '+e.message,mterror,[mbok],0);
    end;
  finally
    screen.Cursor:=crDefault;
    SaveButton.Enabled:=true;
    enabled:=true;
  end;
end;

procedure TConfigurationForm.CancelButtonClick(Sender: TObject);
begin
  Modalresult:=mrCancel;
end;

procedure TConfigurationForm.dbLog(Sender: TSQLConnection;  EventType: TDBEventType; const Msg: String);
begin
  mainform.sqlLog.lines.add(msg);
end;

end.

