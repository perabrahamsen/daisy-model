unit EditHarv;
{
***************
* Unit EditHarv *
***************

Project    : Graphical User Interface for the Daisy simulation package

Purpose    : Edit Harvest Action

Author     : J&R Informatik I/S (Jens Jeppesen)

}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, DsyTime, ExtCtrls,  Spin, ManGLob,IntAct, Globals, TypInfo,
  Buttons;

type
  TFrmEditHar = class(TForm)
    DaisyTimeComp1: TDaisyTimeComp;
    GroupBox1: TGroupBox;
    SEStem: TSpinEdit;
    LblStem: TLabel;
    LblLeaf: TLabel;
    SELeaf: TSpinEdit;
    LblDM: TLabel;
    SEDM: TSpinEdit;
    LblStub: TLabel;
    SEStub: TSpinEdit;
    LblSO: TLabel;
    SESO: TSpinEdit;
    LblStemUnits: TLabel;
    LblLeafUnits: TLabel;
    LblSOUnits: TLabel;
    LblDMUnits: TLabel;
    LblStubUnits: TLabel;
    BtnOK: TBitBtn;
    BtnCancel: TBitBtn;
    BtnHelp: TBitBtn;
    EBCrop: TEdit;
    LblCrop: TLabel;
    Bevel1: TBevel;
    procedure SEStubChange(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
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
    Func : Boolean;
  public
    { Public declarations }
  end;

var
  FrmEditHar: TFrmEditHar;

implementation

{$R *.DFM}
const szHeight = 410;
      szWidth  = 440;

procedure TFrmEditHar.SEStubChange(Sender: TObject);
begin
   Dirty := True;
end;

procedure TFrmEditHar.FormResize(Sender: TObject);
begin
   if WindowState <> wsMinimized then begin
      if Height < szHeight then
         Height := szHeight;
      if Width < szWidth then
         Width := szWidth;
   end;
end;

procedure TFrmEditHar.FormShow(Sender: TObject);
begin
   Dirty := False;
   Func := False;
   DaisyTimeComp1.Show;
   if CurrAction is TFuncHarvest then begin
      Func := True;
      with CurrAction as TFuncHarvest do begin
         EBCrop.Text := CropSowed.What;
         SESO.Value := SO;
         SEDM.Value := DM;
         SEStub.Value := Stub;
         SELeaf.Value := Leaf;
         SEStem.Value := Stem;
      End;
      DaisyTimeComp1.Hide;
   end else begin
      with CurrAction as THarvest do begin
         EBCrop.Text := CropSowed.What;
         SESO.Value := SO;
         SEDM.Value := DM;
         SEStub.Value := Stub;
         SELeaf.Value := Leaf;
         SEStem.Value := Stem;
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
end;

procedure TFrmEditHar.DaisyTimeComp1GetActions(List: TStrings;
  var Index: Integer);
begin
   DoGetList(CurrAction,List);
   if Setup <> nil then begin
      Index := List.IndexOfObject(Setup);
      Setup := nil;
   end;
end;

procedure TFrmEditHar.FormCreate(Sender: TObject);
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

   { Form caption}
   Caption := LoadStr(RES_FRM_EditHarv);

   { Button captions }
   BtnOK.Caption     := LoadStr(RES_BTN_OK);
   BtnCancel.Caption := LoadStr(RES_BTN_Cancel);
   BtnHelp.Caption   := LoadStr(RES_BTN_Help);

   { Group captions }
   GroupBox1.Caption := LoadStr(RES_GRP_EditHarv_Parameters);

   { Label captions }
   LblCrop.Caption := LoadStr(RES_LBL_EditHarv_Crop);
   LblStem.Caption := LoadStr(RES_LBL_EditHarv_Stem);
   LblLeaf.Caption := LoadStr(RES_LBL_EditHarv_Leaf);
   LblSO.Caption   := LoadStr(RES_LBL_EditHarv_Storage_Organs);
   LblDM.Caption   := LoadStr(RES_LBL_EditHarv_Dead_Material);
   LblStub.Caption := LoadStr(RES_LBL_EditHarv_Stub_Not_Included);

   { Units }
   LblStemUnits.Caption := '(' + LoadStr(RES_UNIT_Percent) + ')';
   LblLeafUnits.Caption := '(' + LoadStr(RES_UNIT_Percent) + ')';
   LblSOUnits.Caption   := '(' + LoadStr(RES_UNIT_Percent) + ')';
   LblDMUnits.Caption   := '(' + LoadStr(RES_UNIT_Percent) + ')';
   LblStubUnits.Caption := '(' + LoadStr(RES_UNIT_Cm) + ')';


   // Set resources of the time component
   Globals.SetTimeComponentResources(True, DaisyTimeComp1);

      { Init HelpContexts... }
   EBCrop.HelpContext := HLP_Edit_Harvest_Action;
   GroupBox1.HelpContext := HLP_Edit_Harvest_Action;
   SEStem.HelpContext := HLP_Edit_Harvest_Action;
   SELeaf.HelpContext := HLP_Edit_Harvest_Action;
   SESO.HelpContext := HLP_Edit_Harvest_Action;
   SEDM.HelpContext := HLP_Edit_Harvest_Action;
   SEStub.HelpContext := HLP_Edit_Harvest_Action;
   DaisyTimeComp1.HelpContext := HLP_Time_of_Action;
   BtnOK.HelpContext:= HLP_Edit_Harvest_Action;
   BtnCancel.HelpContext:= HLP_Edit_Harvest_Action;
   BtnHelp.HelpContext:= HLP_Edit_Harvest_Action;
end;

procedure TFrmEditHar.BtnCancelClick(Sender: TObject);
begin
   if Func then begin
      CurrAction.Free;
      CurrAction := nil;
   end;
   Close;
end;

procedure TFrmEditHar.BtnOKClick(Sender: TObject);
begin
   if Dirty then begin
      if Func then begin
         with CurrAction as TFuncHarvest do begin
            SO := SESO.Value;
            DM := SEDM.Value;
            Leaf := SELeaf.Value;
            Stem := SEStem.Value;
            Stub := SEStub.Value;
         End;
      end else begin
         with CurrAction as THarvest do begin
            SO := SESO.Value;
            DM := SEDM.Value;
            Leaf := SELeaf.Value;
            Stem := SEStem.Value;
            Stub := SEStub.Value;
         End;
      end;
   end;
   if not Func then
      EndTimeEdit(DaisyTimeComp1,CurrAction);

   ModalResult := mrOk;
   // Close;   -- Not necessary, as we set ModalResult explicitly
end;

procedure TFrmEditHar.BtnHelpClick(Sender: TObject);
begin
   Application.HelpContext(HLP_Edit_Harvest_Action);
end;

procedure TFrmEditHar.DaisyTimeComp1Exit(Sender: TObject);
begin
   { It is allowed to leave the form at any time... }
   if (ActiveControl <> BtnCancel) then
      DaisyTimeComp1.CheckDate;
end;

end.
