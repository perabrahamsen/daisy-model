unit EditTill;
{
***************
* Unit EditTill *
***************

Project    : Graphical User Interface for the Daisy simulation package

Purpose    : Edit Tillage Action

Author     : J&R Informatik I/S (Jens Jeppesen)

}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, DsyTime, StdCtrls, Buttons,  Globals, TypInfo;

type
  TFrmEditTill = class(TForm)
    DaisyTimeComp1: TDaisyTimeComp;
    BtnOK: TBitBtn;
    BtnCancel: TBitBtn;
    BtnHelp: TBitBtn;
    LblTillageType: TLabel;
    CBTillageType: TComboBox;
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DaisyTimeComp1GetActions(List: TStrings; var Index: Integer);
    procedure CBTillageTypeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);
    procedure BtnHelpClick(Sender: TObject);
    procedure DaisyTimeComp1Exit(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Setup : TObject;
    Dirty : Boolean;
  end;

var
  FrmEditTill: TFrmEditTill;

implementation
uses ManGlob,IntAct;
const szHeight = 320;
      szWidth =  390;

{$R *.DFM}


procedure TFrmEditTill.FormResize(Sender: TObject);
begin
   if WindowState <> wsMinimized then begin
      if Height < szHeight then
         Height := szHeight;
      if Width < szWidth then
         Width := szWidth;
   end;
end;

procedure TFrmEditTill.FormShow(Sender: TObject);
begin
   Setup := nil;
   Dirty := False;
   CBTillageType.Items := Lib.GetTillList;
   with CurrAction as TTillage do begin
      if How <> '' then
         CBTillageType.ItemIndex := CBTillageType.Items.IndexOf(how)
   end;
   DaisyTimeComp1.ClearTime;
   DaisyTimeComp1.OnlyDate := False;

   if CurrAction.when <> nil then begin
      with CurrAction do begin
          if when is TCaDate then begin
             with when as TCaDate do
                DaisyTimeComp1.SetCaDate(Date(True))
          end else if when is TDate then begin
             with when as TDate do
                DaisyTimeComp1.SetDate(Date(True))
          end else if when is TRelToAction then begin
             with when as TRelToAction do begin
                Setup := Action;
                DaisyTimeComp1.SetRelTo(Displacement);
             end;
          end
      end;
   end;
   CBTillageType.SetFocus; { Make the form ready for entering data }
end;


procedure TFrmEditTill.DaisyTimeComp1GetActions(List: TStrings;
  var Index: Integer);
begin
   DoGetList(CurrAction,List);
   if Setup <> nil then begin
      Index := List.IndexOfObject(Setup);
      Setup := nil;
   end;
end;

procedure TFrmEditTill.CBTillageTypeChange(Sender: TObject);
begin
   Dirty := True;
end;

procedure TFrmEditTill.FormCreate(Sender: TObject);
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

   { Caption }
   Caption := LoadStr(RES_FRM_EditTill);

   { Button captions }
   BtnOK.Caption     := LoadStr(RES_BTN_OK);
   BtnCancel.Caption := LoadStr(RES_BTN_Cancel);
   BtnHelp.Caption   := LoadStr(RES_BTN_Help);

   { Label }
   LblTillageType.Caption := LoadStr(RES_LBL_EditTill_Tillage_Type);

   { Set time component resources }
   Globals.SetTimeComponentResources(True, DaisyTimeComp1);

      { Init HelpContexts... }
   CBTillageType.HelpContext:= HLP_Edit_Tillage_Action;
   DaisyTimeComp1.HelpContext := HLP_Time_of_Action;
   BtnOK.HelpContext:= HLP_Edit_Tillage_Action;
   BtnCancel.HelpContext:= HLP_Edit_Tillage_Action;
   BtnHelp.HelpContext:= HLP_Edit_Tillage_Action;
end;

procedure TFrmEditTill.BtnCancelClick(Sender: TObject);
begin
   Close;
end;

procedure TFrmEditTill.BtnOKClick(Sender: TObject);
begin
   If Dirty then with CurrAction as TTillage do
      How := CBTillageType.Text;
   EndTimeEdit(DaisyTimeComp1,CurrAction);
   ModalResult := mrOk;
   // Close;   -- Not necessary, as we set ModalResult explicitly
end;

procedure TFrmEditTill.BtnHelpClick(Sender: TObject);
begin
   Application.HelpContext(HLP_Edit_Tillage_Action);
end;

procedure TFrmEditTill.DaisyTimeComp1Exit(Sender: TObject);
begin
   { It is allowed to leave the form at any time... }
   if (ActiveControl <> BtnCancel) then
      DaisyTimeComp1.CheckDate;
end;

end.
