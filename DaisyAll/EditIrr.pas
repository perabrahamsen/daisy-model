unit EditIrr;
{
***************
* Unit EditIrr *
***************

Project    : Graphical User Interface for the Daisy simulation package

Purpose    : Edit Irrigation Action

Author     : J&R Informatik I/S (Jens Jeppesen)

}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Spin,  ExtCtrls, DsyTime, ManGlob,IntAct, Globals, TypInfo,
  Buttons;

type
  TFrmEditIrr = class(TForm)
    DaisyTimeComp1: TDaisyTimeComp;
    GBIrrParam: TGroupBox;
    ChBUseAir: TCheckBox;
    ChBTop: TCheckBox;
    SEAmount: TSpinEdit;
    SETemp: TSpinEdit;
    LblAmount: TLabel;
    LblAirUnits: TLabel;
    LblAmountUnits: TLabel;
    Bevel1: TBevel;
    BtnOK: TBitBtn;
    BtnCancel: TBitBtn;
    BtnHelp: TBitBtn;
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SETempChange(Sender: TObject);
    procedure SEAmountChange(Sender: TObject);
    procedure ChBTopClick(Sender: TObject);
    procedure ChBUseAirClick(Sender: TObject);
    procedure DaisyTimeComp1GetActions(List: TStrings; var Index: Integer);
    procedure FormCreate(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);
    procedure BtnHelpClick(Sender: TObject);
    procedure DaisyTimeComp1Exit(Sender: TObject);
  private
    { Private declarations }
    Dirty : Boolean;
    Setup : TObject;
  public
    { Public declarations }
  end;

var
  FrmEditIrr: TFrmEditIrr;

implementation

{$R *.DFM}
const szHeight = 410;
      szWidth  = 440;

procedure TFrmEditIrr.FormResize(Sender: TObject);
begin
   if WindowState <> wsMinimized then begin
      if Height < szHeight then
         Height := szHeight;
      if Width < szWidth then
         Width := szWidth;
   end;
end;

procedure TFrmEditIrr.FormShow(Sender: TObject);
begin
   Dirty := False;
   With CurrAction as TIrrigate do begin
      SEAmount.Value := HowMutch;
      SETemp.Value := Temp;
      ChBTop.Checked := OverheadIrr;
      ChBUseAir.Checked := UseAir;
      if UseAir then begin
         SETemp.Enabled := FALSE;
         LblAirUnits.Enabled := FALSE;
      end;

   End;
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
end;

procedure TFrmEditIrr.SETempChange(Sender: TObject);
begin
   Dirty := True;
end;

procedure TFrmEditIrr.SEAmountChange(Sender: TObject);
begin
   Dirty := True;
end;

procedure TFrmEditIrr.ChBTopClick(Sender: TObject);
begin
   Dirty := True;
end;

procedure TFrmEditIrr.ChBUseAirClick(Sender: TObject);
begin
   Dirty := True;
   if ChBUseAir.Checked then begin
      SETemp.Enabled := FALSE;
      LblAirUnits.Enabled := FALSE;
   end else begin
      SETemp.Enabled := TRUE;
      LblAirUnits.Enabled := TRUE;
   end;
end;

procedure TFrmEditIrr.DaisyTimeComp1GetActions(List: TStrings;
  var Index: Integer);
begin
   DoGetList(CurrAction,List);
   if Setup <> nil then begin
      Index := List.IndexOfObject(Setup);
      if Index = -1 then ShowMessage('Action list could not be retrieved.');
      Setup := nil;
   end;
end;

procedure TFrmEditIrr.FormCreate(Sender: TObject);
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
   Caption := LoadStr(RES_FRM_EditIrr);

   { Button captions }
   BtnOK.Caption     := LoadStr(RES_BTN_OK);
   BtnCancel.Caption := LoadStr(RES_BTN_Cancel);
   BtnHelp.Caption   := LoadStr(RES_BTN_Help);

   { Group caption }
   GBIrrParam.Caption := LoadStr(RES_GRP_EditIrr_Parameters);

   { Labels }
   LblAmount.Caption      := LoadStr(RES_LBL_EditIrr_Amount);
   ChBTop.Caption         := LoadStr(RES_LBL_EditIrr_Overhead);
   ChBUseAir.Caption      := LoadStr(RES_LBL_EditIrr_Use_Air_Temp);
   LblAmountUnits.Caption := '(' + LoadStr(RES_UNIT_Mm) + ')';
   LblAirUnits.Caption    := '(' + LoadStr(RES_UNIT_C) + ')';

   { Set time component resources }
   Globals.SetTimeComponentResources(True, DaisyTimeComp1);

      { Init HelpContexts... }
   GBIrrParam.HelpContext := HLP_Edit_Irrigation_Action;
   SEAmount.HelpContext := HLP_Edit_Irrigation_Action;
   ChBTop.HelpContext := HLP_Edit_Irrigation_Action;
   ChBUseAir.HelpContext := HLP_Edit_Irrigation_Action;
   SETemp.HelpContext := HLP_Edit_Irrigation_Action;
   DaisyTimeComp1.HelpContext := HLP_Time_of_Action;
   BtnOK.HelpContext:= HLP_Edit_Irrigation_Action;
   BtnCancel.HelpContext:= HLP_Edit_Irrigation_Action;
   BtnHelp.HelpContext:= HLP_Edit_Irrigation_Action;
end;

procedure TFrmEditIrr.BtnCancelClick(Sender: TObject);
begin
   Close;
end;

procedure TFrmEditIrr.BtnOKClick(Sender: TObject);
begin
   If Dirty then with CurrAction as TIrrigate do begin
      HowMutch := SEAmount.Value;
      Temp := SETemp.Value;
      UseAir := ChBUseAir.Checked;
      OverheadIrr := ChBTop.Checked;
   End;
   EndTimeEdit(DaisyTimeComp1,CurrAction);

   ModalResult := mrOk;
   // Close;   -- Not necessary, as we set ModalResult explicitly
end;

procedure TFrmEditIrr.BtnHelpClick(Sender: TObject);
begin
   Application.HelpContext(HLP_Edit_Irrigation_Action);
end;

procedure TFrmEditIrr.DaisyTimeComp1Exit(Sender: TObject);
begin
   { It is allowed to leave the form at any time... }
   if (ActiveControl <> BtnCancel) then
      DaisyTimeComp1.CheckDate;
end;

end.
