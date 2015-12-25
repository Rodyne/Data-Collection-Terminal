unit EditFunction;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynMemo, SynHighlighterSQL, Forms, Controls,
  Graphics, Dialogs, ExtCtrls, Buttons, StdCtrls, ComCtrls, Configuration,
  sqldb;

type

  { TEditFunctionForm }

  TEditFunctionForm = class(TForm)
    CancelButton: TBitBtn;
    PageControl1: TPageControl;
    QueryDesc: TEdit;
    Label10: TLabel;
    Label6: TLabel;
    Label9: TLabel;
    q: TSQLQuery;
    QueryActive: TCheckBox;
    QueryType: TComboBox;
    FunctionCode: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Panel4: TPanel;
    Panel5: TPanel;
    SaveButton: TBitBtn;
    SQL: TSynMemo;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TestButton: TBitBtn;
    SynSQLSyn1: TSynSQLSyn;
    procedure CancelButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SQLChange(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure TestButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    QID:string;
  end;

var
  EditFunctionForm: TEditFunctionForm;

implementation

{$R *.lfm}

{ TEditFunctionForm }

procedure TEditFunctionForm.CancelButtonClick(Sender: TObject);
begin
  modalresult:=mrcancel;
end;

procedure TEditFunctionForm.FormShow(Sender: TObject);
begin
  Testbutton.enabled:=false;
  FunctionCode.text:='';
  QueryType.itemindex:=0;
  Queryactive.checked:=true;
  SQL.text:='';
  if Isnumber(QID) then
  begin
    if q.Active then q.Close;
    q.sql.text:='SELECT * FROM mobilequeries WHERE QID='+QID;
    q.open;
    functionCode.text:=q.fieldbyname('FnCode').asstring;
    QueryDesc.text:=q.fieldbyname('QueryDesc').asstring;
    QueryType.ItemIndex:=QueryType.Items.IndexOf(q.fieldbyname('QueryType').asstring);
    QueryActive.checked:=q.fieldbyname('QueryFlags').asinteger<>0;
    SQL.text:=q.fieldbyname('QuerySQL').asstring;
    Testbutton.enabled:=true;
  end;
end;

procedure TEditFunctionForm.SQLChange(Sender: TObject);
begin
  Testbutton.enabled:=true;
  Savebutton.enabled:=true;
end;

procedure TEditFunctionForm.SaveButtonClick(Sender: TObject);
var
  s:string;
begin
  if TestButton.Enabled then TestButtonClick(nil);
  saveButton.Enabled:=Testbutton.enabled;
  if testbutton.Enabled then exit;
  if QueryActive.Checked then queryactive.tag:=1 else QueryActive.tag:=0;
  try
    if not Isnumber(QID) then
    begin
      if QueryActive.checked then
        if GetQuery('SELECT COUNT(*) FROM mobilequeries WHERE QueryActive AND FunctionCode='+functioncode.text,0)>0 then
          raise exception.Create('Cannot save. Function Code 0x'+FunctionCode.text+' is already in use and set active and we can only have 1 active function on the system');
      QID:=exec('INSERT INTO mobilequeries (FunctionCode,QueryActive) VALUES('+FunctionCode.text+',1)');
    end;

    s:='UPDATE mobilequeries'+cr;
    s:=s+'SET'+cr;
    s:=s+' FnCode='+FunctionCode.text+','+cr;
    s:=s+' QueryType="'+QueryType.text+'",'+cr;
    s:=s+' QueryDesc="'+safestring(QueryDesc.text)+'",'+cr;
    s:=s+' QuerySQL="'+safestring(sql.text)+'",'+cr;
    s:=s+' QueryFlags='+inttostr(QueryActive.tag)+cr;
    s:=s+'WHERE QID='+QID;

    exec(s);
    modalresult:=mrok;
  except
    on e:exception do
    begin
      SaveButton.enabled:=true;
      messageDLG(e.Message,mterror,[mbok],0);
    end;
  end;
end;

procedure TEditFunctionForm.TestButtonClick(Sender: TObject);
begin
  TestButton.enabled:=false;
  screen.Cursor:=crSQLWait;
  try
    try
      if pos(';',sql.text)>0 then raise exception.Create('Illegal semi-colon character in SQL text!');
      if pos('WHERE',sql.text)=0 then raise exception.Create('No WHERE clause!');
      if pos('InputField',sql.text)=0 then raise exception.Create('No InputField1..5 in WHERE clause!');
      if q.Active then q.Close;
      q.sql.text:=SQL.text;
      if QueryType.text='GET' then
      begin
        if pos('SELECT',sql.text)=0 then raise exception.Create('No SELECT clause!');
        q.open;
        if pos('OutputField',sql.text)=0 then raise exception.Create('No OutputField1 .. Outputfield9 alias in SELECT clause!');
      end
      else
      if QueryType.text='SET' then
      begin
        if pos('UPDATE',sql.text)=0 then raise exception.Create('No UPDATE clause!');
        if pos('LIMIT 1',sql.text)<>length(sql.text)-6 then raise exception.Create('Expecting `LIMIT 1` at the very end of the UPDATE query (for safety!)');
        q.ExecSQL;
      end
      else
        raise exception.Create('Query Type "PRN" Not yet supported');

      if sender<>nil then messageDLG('Query Seems OK',mtinformation,[mbok],0);
    except
      on e:exception do
      begin
        TestButton.enabled:=true;
        messageDLG(e.Message,mterror,[mbok],0);
      end;
    end;
  finally
    screen.Cursor:=crdefault;
  end;
end;

end.

