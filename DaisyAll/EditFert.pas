unit EditFert;
{
***************
* Unit EditFert *
***************

Project    : Graphical User Interface for the Daisy simulation package

Purpose    : Edit Fertilize action


Author     : J&R Informatik I/S (Jens Jeppesen)

}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, DsyTime, ExtCtrls, Globals, TypInfo, Buttons, Mask,
  Spin;

type
  TFrmEditFert = class(TForm)
    DaisyTimeComp1: TDaisyTimeComp;
    GroupBox1: TGroupBox;
    LblAmount: TLabel;
    LblTotal: TLabel;
    LblType: TLabel;
    CBType: TComboBox;
    BtnCalcDown: TSpeedButton;
    BtnCalcUp: TSpeedButton;
    LblAmountUnits: TLabel;
    LblTotalUnits: TLabel;
    MEditAmount: TMaskEdit;
    MEditTotal: TMaskEdit;
    BtnOK: TBitBtn;
    BtnCancel: TBitBtn;
    BtnHelp: TBitBtn;
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure CBTypeChange(Sender: TObject);
    procedure EBAmountChange(Sender: TObject);
    procedure DaisyTimeComp1GetActions(List: TStrings; var Index: Integer);
    procedure FormCreate(Sender: TObject);
    procedure BtnCalcUpClick(Sender: TObject);
    procedure BtnCalcDownClick(Sender: TObject);
    procedure MEditAmountChange(Sender: TObject);
    procedure MEditTotalChange(Sender: TObject);
    procedure BtnHelpClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);
    procedure DaisyTimeComp1Exit(Sender: TObject);
  private
    { Private declarations }
    Dirty : Boolean;
    Setup : TObject;
    procedure CalcTotal;
    procedure UpdateCalculation;
    procedure SwapCalculationDirection;
  public
    { Public declarations }
  end;


var
  FrmEditFert: TFrmEditFert;

implementation
uses ManGLob,IntAct,DaisyLib;
{$R *.DFM}
const szHeight = 350;
      szWidth  = 440;


procedure TFrmEditFert.UpdateCalculation;
var
   Fertilizer     : TFertilizer;
   Source, Target : ^TMaskEdit;
   SourceValue, TargetValue : Integer;
   Factor : Double;
begin
   { Read fertilizer type from Daisy library }
   Fertilizer := Lib.GetFertilizer(CBType.Text);

   { Define values dependant of direction of computation }
   if BtnCalcDown.Down then
      begin
         Source := @MEditAmount;
         Target := @MEditTotal;
         Factor := 1.0/Fertilizer.kgN;
      end
   else
      begin
         Source := @MEditTotal;
         Target := @MEditAmount;
         Factor := Fertilizer.kgN;
      end;

   { If the source field is empty, we make the target empty }
   if Source^.Text = '' then
      Target^.Text := ''

   { Else calculate the new value }
   else                 
      begin
         SourceValue := StrToInt(Source^.Text);
         if (Factor * SourceValue) > 999999999 then begin
            MessageDlg(LoadStr(RES_ERR_EditFert_Number_Too_Big)
                        , mtError, [mbOK], 0);
            Source^.Text := Copy(Source^.Text, 1, Length(Source^.Text)-1);
         end
         else begin
            TargetValue := Round(Factor * SourceValue);
            Target^.Text := FloatToStr(TargetValue);
         end;
      end;
{   if MEditAmount.Text <> '' then begin
      try
         i:=StrToInt(MEditAmount.Text);
         if CBType.Text <> '' then begin
            Obj := Lib.GetFertilizer(CBType.Text);
            MEditTotal.Text := FloatTostr(obj.kgN * i);
         end else
            MEditTotal.Text := '0';
      except
         on EConvertError do begin
         Application.Messagebox('Ammount MUST be a number or empty','Error',MB_Ok);
         MEditAmount.SetFocus;
      end;
   end;
   end; }
end;

procedure TFrmEditFert.SwapCalculationDirection;
begin
   { Swap properties of both input fields }
   if BtnCalcDown.Down then
      begin
         MEditAmount.Color := clWindow;
         MEditTotal.Color  := clInactiveCaptionText;
         MEditAmount.ReadOnly := FALSE;
         MEditTotal.ReadOnly  := TRUE;
         MEditAmount.SetFocus;
      end
   else
      begin
         MEditAmount.Color := clInactiveCaptionText;
         MEditTotal.Color  := clWindow;
         MEditAmount.ReadOnly := TRUE;
         MEditTotal.ReadOnly  := FALSE;
         MEditTotal.SetFocus;
      end;

   { Calculate the new values }
   UpdateCalculation;
end;


procedure TFrmEditFert.FormShow(Sender: TObject);
begin
   Dirty := False;
   CBType.Items := Lib.GetFertilizerList;

   with CurrAction as TFertilize do begin
      if WithWhat <> '' then begin
         CBType.ItemIndex := CBType.Items.IndexOf(WithWhat);
         MEditAmount.Text := IntToStr(Ammount);
      end else
         MEditAmount.Text := ''
   end;

   { Force calc up/down buttons in an inital state }
   BtnCalcDown.Down := TRUE;
   SwapCalculationDirection;

   {CalcTotal;}
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

procedure TFrmEditFert.FormResize(Sender: TObject);
begin
   if WindowState <> wsMinimized then begin
      if Height < szHeight then
         Height := szHeight;
      if Width < szWidth then
         Width := szWidth;
   end;
end;

procedure TFrmEditFert.CalcTotal;
var i:integer;
    Obj : TFertilizer;
begin
   if MEditAmount.Text <> '' then begin
      try
         i:=StrToInt(MEditAmount.Text);
         if CBType.Text <> '' then begin
            Obj := Lib.GetFertilizer(CBType.Text);
            MEditTotal.Text := FloatTostr(i / obj.kgN);
         end else
            MEditTotal.Text := '0';
      except
         on EConvertError do begin
         MessageDlg(LoadStr(RES_ERR_EditFert_Must_Be_Number_Or_Empty),
                     mtError, [mbOK], 0);
         MEditAmount.SetFocus;
      end;
   end;
   end;
end;


procedure TFrmEditFert.CBTypeChange(Sender: TObject);
begin
   Dirty := True;
{   CalcTotal;}
   UpdateCalculation;
end;

procedure TFrmEditFert.EBAmountChange(Sender: TObject);
begin
   Dirty := True;
{   CalcTotal;}
   UpdateCalculation;
end;

procedure TFrmEditFert.DaisyTimeComp1GetActions(List: TStrings;
  var Index: Integer);
begin
   DoGetList(CurrAction,List);
   if Setup <> nil then begin
      Index := List.IndexOfObject(Setup);
      Setup := nil;
   end;
end;

procedure TFrmEditFert.FormCreate(Sender: TObject);
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
   Caption := LoadStr(RES_FRM_EditFert);

   { Button captions }
   BtnOK.Caption     := LoadStr(RES_BTN_OK);
   BtnCancel.Caption := LoadStr(RES_BTN_Cancel);
   BtnHelp.Caption   := LoadStr(RES_BTN_Help);

   { Group captions }
   GroupBox1.Caption      := LoadStr(RES_GRP_EditFert_Parameters);

   { Label captions }
   LblType.Caption   := LoadStr(RES_LBL_EditFert_Fertilizer);
   LblAmount.Caption := LoadStr(RES_LBL_EditFert_Amount);
   LblTotal.Caption  := LoadStr(RES_LBL_EditFert_Total);
   LblAmountUnits.Caption := '(' + LoadStr(RES_UNIT_kg)
                              + '/' + LoadStr(RES_UNIT_ha) + ')';  // (kg/ha)
   LblTotalUnits.Caption  := '(' + LoadStr(RES_UNIT_kg_N)
                              + '/' + LoadStr(RES_UNIT_ha) + ')';  // (kg N/ha)

   // Set resources of the time component
   Globals.SetTimeComponentResources(True, DaisyTimeComp1);

      { Init HelpContexts... }
   GroupBox1.HelpContext := HLP_Edit_Fertilization_Action;
   CBType.HelpContext := HLP_Edit_Fertilization_Action;
   MEditAmount.HelpContext := HLP_Edit_Fertilization_Action;
   MEditTotal.HelpContext := HLP_Edit_Fertilization_Action;
   DaisyTimeComp1.HelpContext := HLP_Time_of_Action;
   BtnOK.HelpContext:= HLP_Edit_Fertilization_Action;
   BtnCancel.HelpContext:= HLP_Edit_Fertilization_Action;
   BtnHelp.HelpContext:= HLP_Edit_Fertilization_Action;
end;

procedure TFrmEditFert.BtnCalcUpClick(Sender: TObject);
begin
   SwapCalculationDirection;
end;

procedure TFrmEditFert.BtnCalcDownClick(Sender: TObject);
begin
  SwapCalculationDirection;
end;

procedure TFrmEditFert.MEditAmountChange(Sender: TObject);
begin
   UpdateCalculation;
end;

procedure TFrmEditFert.MEditTotalChange(Sender: TObject);
begin
   UpdateCalculation;
end;

procedure TFrmEditFert.BtnHelpClick(Sender: TObject);
begin
   Application.HelpContext(HLP_Edit_Fertilization_Action);
end;

procedure TFrmEditFert.BtnCancelClick(Sender: TObject);
begin
   Close;
end;

procedure TFrmEditFert.BtnOKClick(Sender: TObject);
begin
   if Dirty then with CurrAction as TFertilize do begin
      WithWhat := CBType.Text;
      If MEditAmount.Text <> '' then
         Ammount := StrToInt(MEditAmount.Text);
   end;

   EndTimeEdit(DaisyTimeComp1,CurrAction);
   ModalResult := mrOk;
   // Close;   -- Not necessary, as we set ModalResult explicitly
end;

procedure TFrmEditFert.DaisyTimeComp1Exit(Sender: TObject);
begin
   { It is allowed to leave the form at any time... }
   if (ActiveControl <> BtnCancel) then
      DaisyTimeComp1.CheckDate;
end;

end.
