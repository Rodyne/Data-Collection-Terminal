unit selectlist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, Configuration;

type

  { TSelectListForm }

  TSelectListForm = class(TForm)
    OKButton: TBitBtn;
    CancelButton: TBitBtn;
    Panel1: TPanel;
    Panel2: TPanel;
    List: TListBox; // no access directly (why?)
    procedure ListSelectionChange(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
  public
    function Init(title,sql:string):Tmodalresult;
    function SelectedItemAsStr:string;
    function SelectedItemAsInt:int64;
  end;

var
  SelectListForm: TSelectListForm;

implementation


{$R *.lfm}

{ TSelectListForm }

function TSelectListForm.Init(title,sql:string):Tmodalresult;
begin
  caption:=title;
  Loadlist(sql,List.Items);
  OKButton.Enabled:=false;
  result:=showmodal;
end;

function TSelectListForm.SelectedItemAsStr:string;
begin
  result:=inttostr(ptrUint(List.items.objects[List.itemindex]));
end;

function TSelectListForm.SelectedItemAsInt:int64;
begin
  result:=ptrUint(List.items.objects[List.itemindex]);
end;

procedure TSelectListForm.CancelButtonClick(Sender: TObject);
begin
  Modalresult:=mrcancel;
end;

procedure TSelectListForm.OKButtonClick(Sender: TObject);
begin
  Modalresult:=mrok;
end;

procedure TSelectListForm.ListSelectionChange(Sender: TObject);
begin
  OKButton.Enabled:=List.ItemIndex>=0;
end;

end.

