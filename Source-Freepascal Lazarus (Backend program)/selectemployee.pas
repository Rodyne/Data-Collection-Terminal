unit selectemployee;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, db, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, Buttons, DBGrids, Menus, common;

type

  { TSelectEmployeeForm }

  TSelectEmployeeForm = class(TForm)
    NewButton: TBitBtn;
    MaintPanel: TPanel;
    POP_New: TMenuItem;
    POP_Edit: TMenuItem;
    EmployeeRole: TComboBox;
    CancelButton: TBitBtn;
    DBGrid1: TDBGrid;
    ds: TDatasource;
    Filter: TEdit;
    FilterActive: TCheckBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    OKButton: TBitBtn;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    POP: TPopupMenu;
    q1: TSQLQuery;
    RefreshButton: TBitBtn;
    SelectPanel: TPanel;
    procedure CancelButtonClick(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure DBGrid1DblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure POPPopup(Sender: TObject);
    procedure POP_EditClick(Sender: TObject);
    procedure POP_NewClick(Sender: TObject);
    procedure RefreshButtonClick(Sender: TObject);
  public
    function SelectAssetID:tModalresult; // ope as select and return accountid
  private
    sql1,sort1:string;
  end;

var
  SelectEmployeeForm: TSelectEmployeeForm;

implementation

uses EditUsers;

{$R *.lfm}

{ TSelectEmployeeForm }

procedure TSelectEmployeeForm.OKButtonClick(Sender: TObject);
begin
  ModalResult:=mrok;
end;

procedure TSelectEmployeeForm.POPPopup(Sender: TObject);
begin
  POP_New.Enabled:=NewButton.Enabled;
  POP_Edit.Enabled:=NewButton.Enabled and (q1.RecordCount>0);
end;

procedure TSelectEmployeeForm.POP_EditClick(Sender: TObject);
begin
  EditUsersform.AssetID.caption:=q1.FieldByName('ID').asstring;
  if EditUsersform.showmodal<>mrok then exit;
  RefreshButtonClick(nil);
  q1.Locate('ID',EditUsersform.AssetID.caption,[]);
end;

procedure TSelectEmployeeForm.POP_NewClick(Sender: TObject);
begin
  EditUsersform.AssetID.caption:='NEW';
  if EditUsersForm.showmodal<>mrok then exit;
  RefreshButtonClick(nil);
  q1.Locate('ID',EditUsersForm.AssetID.caption,[]);
end;

procedure TSelectEmployeeForm.RefreshButtonClick(Sender: TObject);
var
  filterstr:string;
begin
  if q1.Active then q1.Close;
  case EmployeeRole.ItemIndex of
    1:filterStr:=' AND AssetFlags&0x01'; // Sysadmins
    2:filterStr:=' AND AssetFlags&0x02'; // purchasing
    3:filterStr:=' AND AssetFlags&0x04'; // Sales
    4:filterStr:=' AND AssetFlags&0x08'; // accounts
    5:filterStr:=' AND AssetFlags&0x10'; // prod mgr
    6:filterStr:=' AND AssetFlags&0x20'; // prod supervisor
    7:filterStr:=' AND AssetFlags&0x40'; // storeman
    8:filterStr:=' AND PIN IS NOT NULL'; // anyone with a pin
  end;
  if FilterActive.Checked then filterStr:=filterStr+' AND AssetStatus=2';
  if filter.text<>'' then filterstr:=filterstr+' AND (AssetName LIKE "%'+safestring(filter.text)+'%")';
  q1.sql.Text:=stringReplace(sql1,'/*AND*/',filterstr,[]) + 'ORDER BY `'+sort1+'`';
  q1.open;
  OKButton.Enabled:=q1.recordcount>0;
end;

procedure TSelectEmployeeForm.CancelButtonClick(Sender: TObject);
begin
  Modalresult:=mrCancel;
end;

procedure TSelectEmployeeForm.CloseButtonClick(Sender: TObject);
begin

end;

procedure TSelectEmployeeForm.DBGrid1DblClick(Sender: TObject);
begin
  if SelectPanel.Visible and OKButton.Enabled then OKButtonClick(nil);
  if not SelectPanel.Visible and (q1.RecordCount>0) then
  begin
    POP_EditClick(nil);
    RefreshButtonClick(nil);
  end;
end;

procedure TSelectEmployeeForm.FormCreate(Sender: TObject);
begin
  sql1:=q1.sql.text;
  sort1:='AssetName';
end;

procedure TSelectEmployeeForm.FormShow(Sender: TObject);
begin
  SelectPanel.Visible:=parent=nil;
  if SelectPanel.visible then caption:='Select Employee' else caption:='Maintain Employees';
  MaintPanel.Visible:=not SelectPanel.Visible;
  Panel3.Caption:=caption;
  RefreshButtonClick(nil);
end;

function TSelectEmployeeForm.SelectAssetID: tModalresult;
begin
  FilterActive.Checked:=true;
  parent := nil;
  BorderStyle:=bsSizeable;
  Align := alNone;
  height:=550;
  width:=832;
  SelectPanel.Visible:=true;
  result:=ShowModal;
end;

end.

