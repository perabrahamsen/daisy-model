unit ShowList;
{
***************
* Unit ShowList *
***************

Project    : Graphical User Interface for the Daisy simulation package

Purpose    : Dialog Window to show which changes there will be made in the action list
             and to cancel the changes !

Author     : J&R Informatik I/S (Jens Jeppesen)

}

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, Globals, TypInfo;

type
  TOKBottomDlg = class(TForm)
    Panel1: TPanel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Memo1: TMemo;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    TimeList, DeleteList : TList;
    Msg:String;
  public
    { Public declarations }
    Procedure SetLists(a,b:TList;ShowMessage:String);
  end;

var
   OKBottomDlg: TOKBottomDlg;

implementation
uses IntAct;
{$R *.DFM}

Procedure TOKBottomDlg.SetLists(a,b:TList;ShowMessage:String);
begin
   TimeList := a;
   DeleteList := b;
   Msg := ShowMessage;
end;

procedure TOKBottomDlg.FormShow(Sender: TObject);
var IA:TInternAction;
    i : Integer;
begin
   Caption := Msg;
   if (DeleteList.Count = 0) and (TimeList.Count = 0) then begin
      ModalResult := mrOK;
   end else begin
      Memo1.Lines.Clear;
      if DeleteList.Count > 0 then begin
         Memo1.Lines.Add(LoadStr(RES_MSG_Actions_Delete));
         for i := 0 to DeleteList.Count - 1 do begin
            IA := DeleteList[i];
            Memo1.Lines.Add('   '+IntToStr(IA.WitchList.GetObjIdx(IA)) + ' ' + IA.ActionName);
         end;
      end;
      if TimeList.Count > 0 then begin
         if DeleteList.Count > 0 then
            Memo1.Lines.Add('');
         Memo1.Lines.Add(LoadStr(RES_MSG_Actions_Reset));
         for i := 0 to TimeList.Count - 1 do begin
            IA := TimeList[i];
            Memo1.Lines.Add('   '+IntToStr(IA.WitchList.GetObjIdx(IA)) + ' ' + IA.ActionName);
         end;
      end;
   end;
   SetLists(nil,nil,'');
end;

procedure TOKBottomDlg.FormCreate(Sender: TObject);
var
   i: Integer;
begin
   { Scale the form and its components... }
   Scaled := FALSE;
   AutoScroll := FALSE;
   if (NewScreenWidth <> OldScreenWidth) then begin
      ScaleBy(NewScreenWidth, OldScreenWidth);
      for i := ComponentCount - 1 downto 0 do
         with Components[i] do begin
            if GetPropInfo(ClassInfo, 'font') <> nil then
               Font.Size := (NewScreenWidth div OldScreenWidth) * Font.Size;
         end;
   end;

   { Form caption }
   // Caption is set dynamically by the caller
   Caption := '';

   { Button captions }
   BitBtn2.Caption := LoadStr(RES_BTN_OK);
   BitBtn1.Caption := LoadStr(RES_BTN_Cancel);

end;

end.
