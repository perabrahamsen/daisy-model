unit EditAct;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, Spin, ExtCtrls, DsyTime, Globals, TypInfo;

type
  TEditAction = class(TForm)
    GBAction: TGroupBox;
    GBTime: TGroupBox;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    CBCrop: TComboBox;
    CBTimeType: TComboBox;
    CBAction: TComboBox;
    CBState: TComboBox;
    SEDegree: TSpinEdit;
    SEDays: TSpinEdit;
    LblType: TLabel;
    LblDate: TLabel;
    LblAction: TLabel;
    LblState: TLabel;
    LblDegree: TLabel;
    LblDays: TLabel;
    LblCrop: TLabel;
    EBDate: TEdit;
    GBParams: TGroupBox;
    CBModel: TComboBox;
    LblModel: TLabel;
    LblTilType: TLabel;
    CBTillage: TComboBox;
    LblFertilizer: TLabel;
    CBFertilizer: TComboBox;
    LblAmount: TLabel;
    EBAmount: TEdit;
    LblKGN: TLabel;
    LblAmountmm: TLabel;
    SEAmount: TSpinEdit;
    SEtemp: TSpinEdit;
    LblTemp: TLabel;
    ChBTop: TCheckBox;
    ChBUseAir: TCheckBox;
    LblSO: TLabel;
    LblDM: TLabel;
    LblStem: TLabel;
    LblLeaf: TLabel;
    LblStub: TLabel;
    SESO: TSpinEdit;
    SEStem: TSpinEdit;
    SEStub: TSpinEdit;
    SELeaf: TSpinEdit;
    SEDM: TSpinEdit;
    LblCropSow: TLabel;
    LblEks: TLabel;
    DaisyTimeComp1: TDaisyTimeComp;
    BtnClearTime: TBitBtn;
    procedure FormShow(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure CBCropChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CBTimeTypeChange(Sender: TObject);
    procedure BtnClearTimeClick(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure CBModelChange(Sender: TObject);
    procedure CBTillageChange(Sender: TObject);
    procedure CBFertilizerChange(Sender: TObject);
    procedure EBAmountChange(Sender: TObject);
    procedure EBAmountExit(Sender: TObject);
    procedure SEAmountChange(Sender: TObject);
    procedure SEtempChange(Sender: TObject);
    procedure ChBTopClick(Sender: TObject);
    procedure ChBUseAirClick(Sender: TObject);
    procedure SESOChange(Sender: TObject);
    procedure SEDMChange(Sender: TObject);
    procedure SEStubChange(Sender: TObject);
    procedure SEStemChange(Sender: TObject);
    procedure SELeafChange(Sender: TObject);
    procedure CBActionChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
      Dirty : Boolean;
      TimeType : Integer;
      procedure DoDate;
      procedure DoCaDate;
      procedure DoRelToAction;
      procedure DoRelToDevState;

      procedure TimeHideAll;

      procedure SowHide;
      procedure IrrHide;
      procedure FerHide;
      procedure HarHide;
      procedure TilHide;
      procedure HideAll;

      procedure SowShow;
      procedure IrrShow;
      procedure FerShow;
      procedure HarShow;
      procedure TilShow;

      procedure SetUpStartEdit;
      procedure SetUpEndEdit;
      procedure SetUpIrrigateEdit;
      procedure SetUpTillageEdit;
      procedure SetUpFertilizeEdit;
      procedure SetUpHarvestEdit;
      procedure SetUpSowingEdit;

      procedure EndStartEdit;
      procedure EndEndEdit;
      procedure EndIrrigateEdit;
      procedure EndTillageEdit;
      procedure EndFertilizeEdit;
      procedure EndHarvestEdit;
      procedure EndSowingEdit;

      procedure SetUpTimeEdit;
      procedure EndTimeEdit;


      procedure ShowMulti;
      procedure HideMulti;
  end;

var
  EditAction: TEditAction;

implementation
uses ManGlob, IntAct;
{$R *.DFM}

var
   OldWhat:String;

procedure TEditAction.ShowMulti;
begin
end;
procedure TEditAction.HideMulti;
begin
end;

procedure TEditAction.DoDate;
begin
   TimeHideAll;
   LblType.Show;
   CBTimeType.ItemIndex :=0;
   CBTimeType.Show;

   With LblDate do begin
      Left := 56;
      Top := 72;
      Show;
   End;
   With LblEks do begin
        Left := 256;
        Top := 72;
        Show;
   End;

   With EBDate do begin
      Left := 88;
      Top := 72;
      Show;
   End;
end;

procedure TEditAction.DoCaDate;
begin
   TimeHideAll;
   LblType.Show;
   CBTimeType.ItemIndex :=1;
   CBTimeType.Show;
   With LblDate do begin
      Left := 56;
      Top := 72;
      Show;
   End;
   With LblEks do begin
        Left := 256;
        Top := 72;
        Show;
   End;

   With EBDate do begin
      Left := 88;
      Top := 72;
      Show;
   End;
end;

procedure TEditAction.DoRelToAction;
var l:TList;
    i:Integer;
    o:TInternAction;
begin
   TimeHideAll;
   l := TList.Create;
   CBAction.Clear;
   InternalActionlist.GetListExSubTree(CurrAction,l);
   for i:= 0 to l.Count-1 do begin
       o := l.Items[i];
       CBAction.Items[i] := IntToStr(InternalActionlist.GetObjIdx(o))+' '+o.ActionName+' '+o.Specification;
       CBAction.Items.Objects[i] := o;
   end;
   l.Free;
   CBAction.ItemIndex :=0;
   LblType.Show;
   CBTimeType.ItemIndex :=2;
   CBTimeType.Show;
   With LblAction do begin
      Left := 48;
      Caption := 'Action';
      Show;
   End;
   CBAction.Show;
   LblDays.Show;
   SEDays.Show;

end;
procedure TEditAction.DoRelToDevState;
var l:TList;
    i,j:Integer;
    o:TInternAction;
begin
   TimeHideAll;
   l := TList.Create;
   InternalActionlist.GetListExSubTree(CurrAction,l);
   j:=0;
   CBAction.Clear;
   for i:= 0 to l.Count-1 do begin
       o := l.Items[i];
       if o is TSowing then begin
          CBAction.Items[j] := IntToStr(InternalActionlist.GetObjIdx(o))+' '+o.ActionName+' '+o.Specification;
          CBAction.Items.Objects[j] := o;
          Inc(j);
       end;
   end;
   l.Free;
   CBState.Clear;
   CBState.Items[0] := 'Emergence';
   CBState.Items[1] := 'Anthesis/Flowering';
   CBState.Items[2] := 'Mature';
   CBState.ItemIndex :=0;
   LblType.Show;
   CBTimeType.ItemIndex :=3;
   CBTimeType.Show;
   LblState.Show;
   CBState.Show;
   with LblAction do begin
      Left := 8;
      Caption := 'Sowing Action';
      Show;
   End;
   CBAction.Show;
   LblDegree.Show;
   SEDegree.Show;
   LblDays.Show;
   SEDays.Show;
end;



procedure TEditAction.SetUpStartEdit;
begin
     GBAction.Hide;
     GBParams.Hide;
end;
procedure TEditAction.SetUpEndEdit;
begin
     GBAction.Hide;
     GBParams.Hide;
end;
procedure TEditAction.SetUpIrrigateEdit;
begin
   With CurrAction as TIrrigate do begin
      SEAmount.Value := HowMutch;
      SETemp.Value := Temp;
      ChBTop.Checked := OverheadIrr;
      ChBUseAir.Checked := UseAir;
   End;
   Dirty := False;
   IrrShow;
end;
procedure TEditAction.SetUpTillageEdit;
begin
   Dirty := False;
   CBTillage.Items := Lib.GetTillList;
   with CurrAction as TTillage do begin
      if How <> '' then
         CBTillage.ItemIndex := CBTillage.Items.IndexOf(how)
   end;
   TilShow;
end;
procedure TEditAction.SetUpFertilizeEdit;
begin
   Dirty := False;
   CBFertilizer.Items := Lib.GetFertilizerList;
   with CurrAction as TFertilize do begin
      if WithWhat <> '' then begin
         CBFertilizer.ItemIndex := CBFertilizer.Items.IndexOf(WithWhat);
         EBAmount.Text := IntToStr(Ammount);
      end else
         EBAmount.Text := '0'
   end;
   FerShow;
end;
procedure TEditAction.SetUpHarvestEdit;
begin
   with CurrAction as THarvest do begin
      SESO.Value := SO;
      SEDM.Value := DM;
      SEStub.Value := Stub;
      SELeaf.Value := Leaf;
      SEStem.Value := Stem;
   End;
   Dirty:=False;
   HarShow;
end;
procedure TEditAction.SetUpSowingEdit;
Var Crop : TCrop;
begin
   {GBAction.Caption := CurrAction.ActionName;}
   CBCrop.Items := Lib.GetCropList;
   CBModel.Items := Lib.GetModelList;
   with CurrAction as TSowing do begin
      CBCrop.Text := what;
      OldWhat := what;
      CBModel.Text := model;
      Crop := Lib.GetCrop(what);
      if Crop.Multi then
         ShowMulti;
   end;
   SowShow;
end;
procedure TEditAction.EndStartEdit;
begin
end;
procedure TEditAction.EndEndEdit;
begin
end;
procedure TEditAction.EndIrrigateEdit;
begin
   If Dirty then with CurrAction as TIrrigate do begin
      HowMutch := SEAmount.Value;
      Temp := SETemp.Value;
      UseAir := ChBUseAir.Checked;
      OverheadIrr := ChBTop.Checked;
   End;
end;
procedure TEditAction.EndTillageEdit;
begin
   If Dirty then with CurrAction as TTillage do
      How := CBTillage.Text;
end;
procedure TEditAction.EndFertilizeEdit;
begin
   if Dirty then with CurrAction as TFertilize do begin
      WithWhat := CBFertilizer.Text;
      If EBAmount.Text <> '' then
         Ammount := StrToInt(EBAmount.Text);
   end;
end;
procedure TEditAction.EndHarvestEdit;
begin
   if Dirty then with CurrAction as THarvest do begin
      SO := SESO.Value;
      DM := SEDM.Value;
      Leaf := SELeaf.Value;
      Stem := SEStem.Value;
      Stub := SEStub.Value;
   End;
end;

procedure TEditAction.EndSowingEdit;
begin
   If Dirty then with CurrAction as TSowing do begin
      what := CBCrop.Text;
      model := CBModel.Text;
   end;
end;

procedure TEditAction.SetUpTimeEdit;
var
   CropParams : TCrop;
begin
   if CurrAction.when <> nil then begin
      with CurrAction do begin
          if when is TCaDate then begin
             DoCaDate;
             with when as TCaDate do
                EBDate.Text := Date(True);
          end else if when is TDate then begin
             DoDate;
             with when as TDate do
                EBDate.Text := Date(True);
          end else if when is TRelToAction then begin
             DoRelToAction;
             with when as TRelToAction do
                CBAction.ItemIndex := CBAction.Items.IndexOfObject(Action);
          end{ else if when is TRelToDevelopmentState then begin
             DoRelToDevState;
             with when as TRelToDevelopmentState do begin
                CBAction.ItemIndex := CBAction.Items.IndexOfObject(SowingAction);
                CBState.ItemIndex := Stage;
                SEDays.Value := Displacement;
                SEDegree.Value := Degree;
                   CropParams := Lib.GetCrop(SowingAction.what);
                   with CropParams as TCropParams do
                      if Multi then begin
                      end;
             end;
          end;}
      end;
   end else begin
      SEDays.Value := 0;
      SEDegree.Value := 100;
      EBDate.Text := '';
      TimeType := 0;
      DoDate;
   end;
end;

procedure TEditAction.EndTimeEdit;
var
   o : TObject;
begin
   case TimeType of
      0:
         begin
            if not CurrAction.SetDato(EBDate.Text) then begin
            end;
         end;
      1:
         begin
            if not CurrAction.SetCaDato(EBDate.Text) then begin
            end;
         end;
      2:
         begin
            CurrAction.UnRelate;
            if CBAction.ItemIndex <> -1 then begin
               o := CBAction.Items.Objects[CBAction.ItemIndex];
               CurrAction.RelateToAction(o as TInternAction,SEDays.Value);
            end;
         end;
      {3:
         begin
            CurrAction.UnRelate;
            if CBAction.ItemIndex <> -1 then begin
               o := CBAction.Items.Objects[CBAction.ItemIndex];
               CurrAction.RelateToDevelopmentState(o as TSowing,CBState.ItemIndex,SEDegree.Value,SEDays.Value);
            end;
         end;}
   end;
end;

procedure TEditAction.FormShow(Sender: TObject);
begin
   Dirty := False;
   HideAll;
   GBTime.Caption := GetString(600);
   Caption := 'Edit ' + CurrAction.ActionName + ' action';
   if CurrAction is TSowing then
      SetUpSowingEdit
   else if CurrAction is THarvest then
      SetUpHarvestEdit
   else if CurrAction is TFertilize then
      SetUpFertilizeEdit
   else if CurrAction is TTillage then
      SetUpTillageEdit
   else if CurrAction is TIrrigate then
      SetUpIrrigateEdit
   else if CurrAction is TStart then
      SetUpStartEdit
   else if CurrAction is TEnd then
      SetUpEndEdit;
   SetUpTimeEdit;
end;

procedure TEditAction.BitBtn1Click(Sender: TObject);
begin
   if CurrAction is TSowing then
      EndSowingEdit
   else if CurrAction is THarvest then
      EndHarvestEdit
   else if CurrAction is TFertilize then
      EndFertilizeEdit
   else if CurrAction is TTillage then
      EndTillageEdit
   else if CurrAction is TIrrigate then
      EndIrrigateEdit
   else if CurrAction is TStart then
      EndStartEdit
   else if CurrAction is TEnd then
      EndEndEdit;
   EndTimeEdit;
   Close;
end;

procedure TEditAction.CBCropChange(Sender: TObject);
Var prevCrop, newCrop :TCrop;
begin
   with CurrAction as TSowing do begin
      if CBCrop.Text <> OldWhat then begin
         Dirty := True;
         prevCrop := Lib.GetCrop(what);
         newCrop := Lib.GetCrop(CBCrop.Text);
         if not prevCrop.Multi = newCrop.Multi then begin
            if prevCrop.Multi then begin
               HideMulti;
            end else begin
               ShowMulti;
            end;
         end;
         OldWhat := CBCrop.Text;
      End;
   End;
end;

procedure TEditAction.FormCreate(Sender: TObject);
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

   CBTimeType.Items.Clear;
   CBTimeType.Items.Add(GetString(610));
   CBTimeType.Items.Add(GetString(611));
   CBTimeType.Items.Add(GetString(612));
{   CBTimeType.Items.Add(GetString(613));}
end;

procedure TEditAction.CBTimeTypeChange(Sender: TObject);
begin
   TimeType := CBTimeType.ItemIndex;
   if (CurrAction is TStart) or (CurrAction is TEnd) then begin
      CBTimeType.ItemIndex := 0;
      TimeType := CBTimeType.ItemIndex;
      DoDate()
   end else begin
      TimeType := CBTimeType.ItemIndex;
       case CBTimeType.ItemIndex of
            0: DoDate();
            1: DoCaDate();
            2: DoRelToAction;
            {3: DoRelToDevState;}
            else
           {TimeHideAll()};
       end;
   end;
end;
procedure TEditAction.TimeHideAll;
{ Hide all Time components }
begin
   {GBTime components Hide 'em}
   {Label's}
   LblType.Hide;
   LblDate.Hide;
   LblEks.Hide;
   LblAction.Hide;
   LblState.Hide;
   LblDegree.Hide;
   LblDays.Hide;
   {Spin Edit's}
   SEDegree.Hide;
   SEDays.Hide;
   {ComboBox's}
   CBTimeType.Hide;
   EBDate.Hide;
   CBAction.Hide;
   CBState.Hide;
end;

procedure TEditAction.HideAll;
begin
   SowHide;
   IrrHide;
   TilHide;
   FerHide;
   HarHide;
end;

procedure TEditAction.SowHide;
begin
   CBCrop.Hide;
   CBModel.Hide;
   LblCrop.Hide;
   LblModel.Hide;
end;
procedure TEditAction.IrrHide;
begin
   SEAmount.Hide;
   LblAmountmm.Hide;
   SETemp.Hide;
   LblTemp.Hide;
   ChBTop.Hide;
   ChBUseAir.Hide;
end;

procedure TEditAction.FerHide;
begin
   LblFertilizer.Hide;
   CBFertilizer.Hide;
   LblAmount.Hide;
   EBAmount.Hide;
   LblKGN.Hide;
end;
procedure TEditAction.HarHide;
begin
   LblCropSow.Hide;
   LblStem.Hide;
   LblLeaf.Hide;
   LblSO.Hide;
   LblDM.Hide;
   LblStub.Hide;
   SESO.Hide;
   SEDM.Hide;
   SEStub.Hide;
   SELeaf.Hide;
   SEStem.Hide;
end;
procedure TEditAction.TilHide;
begin
   LblTilType.Hide;
   CBTillage.Hide;
end;

procedure TEditAction.SowShow;
begin
   With GBAction do begin
      Caption := CurrAction.ActionName;
      Show;
   end;
   With LblCrop do begin
      Caption := 'Crop';
      Left := 8;
      Top := 24;
      Show;
   end;
   With CBCrop do begin
      Left := 40;
      Top := 24;
      Show;
   End;
   With GBParams do begin
      Caption := 'Parameters';
      Show;
   End;
   With LblModel do begin
      Caption := 'Irrigation Model';
      Left := 16;
      Top := 24;
      Show;
   End;
   With CBModel do begin
      Left := 96;
      Top := 24;
      Show;
   End;
end;

procedure TEditAction.IrrShow;
begin
   GBAction.Hide;
   With GBParams do begin
      Caption := 'Parameters';
      Show;
   End;
   With LblAmount do begin
      Caption := 'Amount (mm)';
      Left := 40;
      Top := 24;
      Show;
   End;
   With SEAmount do begin
      Left := 112;
      Top := 24;
      Show;
   End;
   With ChBTop do begin
      Caption := 'Top Irrigation';
      Left := 216;
      Top := 24;
      Show;
   End;
   With LblTemp do begin
      Caption := 'Air Temperature (C)';
      Left := 8;
      Top := 64;
      Show;
   End;
   With SETemp do begin
      Left := 112;
      Top := 64;
      Show;
   End;
   With ChBUseAir do begin
      Left := 184;
      Top := 64;
      Show;
   End;
end;
procedure TEditAction.FerShow;
begin
   With GBAction do begin
      Caption := CurrAction.ActionName;
      Show;
   End;
   With GBParams do begin
      Caption := 'Parameters';
      Show;
   End;
   With LblFertilizer do begin
      Caption := 'Type';
      Left := 8;
      Top := 24;
      Show;
   End;
   With CBFertilizer do begin
      Left := 40;
      Top := 24;
      Show;
   End;
   With LblAmount do begin
      Caption := 'Amount (Tons)';
      Left := 8;
      Top := 24;
      Show;
   End;
   With EBAmount do begin
      Left := 88;
      Top := 24;
      Show;
   End;
   With LblKGN do begin
      Left := 168;
      Top := 24;
      Caption := 'Kg N';
      Show;
   end;
end;
procedure TEditAction.HarShow;
begin
   With GBAction do begin
      Caption := CurrAction.ActionName;
      Show;
   End;
   With GBParams do begin
      Caption := 'Parameters';
      Show;
   End;
   With LblCropSow do begin
      With CurrAction as THarvest do
         Caption := 'Crop Sowed: ' + CropSowed.What;
      Left := 8;
      Top := 24;
      Show;
   End;
   With LblStem do begin
      Left := 192;
      Top := 24;
      Show;
   End;
   With LblLeaf do begin
      Left := 192;
      Top := 64;
      Show;
   End;
   With LblSO do begin
      Left := 16;
      Top := 24;
      Show;
   End;
   With LblDM do begin
      Left := 24;
      Top := 64;
      Show;
   End;
   With LblStub do begin
      Left := 8;
      Top := 104;
      Show;
   End;
   With SESO do begin
      Left := 120;
      Top := 24;
      Show;
   End;
   With SEDM do begin
      Left := 120;
      Top := 64;
      Show;
   End;
   With SEStub do begin
      Left := 120;
      Top := 104;
      Show;
   End;
   With SELeaf do begin
      Left := 240;
      Top := 24;
      Show;
   End;
   With SEStem do begin
      Left := 240;
      Top := 64;
      Show;
   End;
end;
procedure TEditAction.TilShow;
begin
   GBParams.Hide;
   With GBAction do begin
      Caption := CurrAction.ActionName;
      Show;
   End;
   With LblTilType do begin
      Caption := 'Type';
      Left := 8;
      Top := 24;
      Show;
   End;
   With CBTillage do begin
      Left := 40;
      Top := 24;
      Show;
   End;
end;

procedure TEditAction.BtnClearTimeClick(Sender: TObject);
begin
   if CurrAction.when <> nil then begin
      CurrAction.when.Free;
      CurrAction.when := nil;
   end;
   TimeType := 0;
   EBDate.Text := '';
   DoDate;
end;

procedure TEditAction.BitBtn2Click(Sender: TObject);
begin
   if Dirty then begin
   end;
   Close;
end;

procedure TEditAction.CBModelChange(Sender: TObject);
begin
   Dirty := True;
end;

procedure TEditAction.CBTillageChange(Sender: TObject);
begin
   Dirty := True;
end;

procedure TEditAction.CBFertilizerChange(Sender: TObject);
begin
   Dirty := True;
end;

procedure TEditAction.EBAmountChange(Sender: TObject);
var i,Code:integer;

begin
   Dirty:=True;
end;

procedure TEditAction.EBAmountExit(Sender: TObject);
var i:integer;
    Obj : TFertilizer;
begin
   if EBAmount.Text <> '' then begin
      try
         i:=StrToInt(EBAmount.Text);
         if CBFertilizer.Text <> '' then begin
            Obj := Lib.GetFertilizer(CBFertilizer.Text);
            LblKGN.Caption := 'Kg N ' + FloatTostr(obj.kgN * i);
         end;
      except
         on EConvertError do begin
         Application.Messagebox('Der skal indtastes et tal eller ingenting','Indtastnings Fejl',MB_Ok);
         EBAmount.SetFocus;
      end;
   end;
   end;
end;

procedure TEditAction.SEAmountChange(Sender: TObject);
begin
   Dirty := True;
end;

procedure TEditAction.SEtempChange(Sender: TObject);
begin
   Dirty := True;
end;

procedure TEditAction.ChBTopClick(Sender: TObject);
begin
   Dirty := True;
end;

procedure TEditAction.ChBUseAirClick(Sender: TObject);
begin
   Dirty := True;
end;

procedure TEditAction.SESOChange(Sender: TObject);
begin
   Dirty := True;
end;

procedure TEditAction.SEDMChange(Sender: TObject);
begin
   Dirty := True;
end;

procedure TEditAction.SEStubChange(Sender: TObject);
begin
   Dirty := True;
end;

procedure TEditAction.SEStemChange(Sender: TObject);
begin
   Dirty := True;
end;

procedure TEditAction.SELeafChange(Sender: TObject);
begin
   Dirty := True;
end;

procedure TEditAction.CBActionChange(Sender: TObject);
begin
   {if TimeType = 3 then begin
   end;}
end;

end.
