unit EditSow;
{
***************
* Unit EditSow *
***************

Project    : Graphical User Interface for the Daisy simulation package

Purpose    : Edit a Sowing action
             1) Edit the time of the action
             2) Edit the irrigation model
             3) Optionally add "functional harvest" action(s)
                a1) Allow only to generate one action iff crop is for one season
                a2) Allow to generate 10 actions for 100 years iff crop is for more than one season
                b) if any functional harvest actions are defined then remove a previously defined
                   "normal" harvest action

             Any changes made in this window and any (sub)windows are only commited if the OK-button
             is selected and any warnings (if any) are accepted.
             The main datastructures are
                a) TSowingAction, THarvest and TFuncHarvest
                b) A 2D list of TFuncHarvest objects to manage the removing/insertion and changing of
                   functional harvest actions

Author     : J&R Informatik I/S (Jens Jeppesen)

Date(s)    :
}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, DsyTime, ExtCtrls,  Buttons, Spin,IntAct,EditHarv,
  Globals, TypInfo;

type
  TFrmEditSow = class(TForm)
    DaisyTimeComp1: TDaisyTimeComp;
    GroupBox1: TGroupBox;
    LblCrop: TLabel;
    LblModel: TLabel;
    CBModel: TComboBox;
    ChBFuncHarv: TCheckBox;
    GBDevStage: TGroupBox;
    TBDevStage: TTrackBar;
    LblEmergence: TLabel;
    LblFlowering: TLabel;
    LblMature: TLabel;
    EBDevStage: TEdit;
    BtnHarvParam: TBitBtn;
    SENoHarv: TSpinEdit;
    LblNoHarv: TLabel;
    EBCrop: TEdit;
    LblYears: TLabel;
    SENoYears: TSpinEdit;
    BtnOK: TBitBtn;
    BtnCancel: TBitBtn;
    BtnHelp: TBitBtn;
    LblFuncHarv: TLabel;
    procedure CBModelChange(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ChBFuncHarvClick(Sender: TObject);
    procedure SENoHarvChange(Sender: TObject);
    procedure EBDevStageChange(Sender: TObject);
    procedure TBDevStageChange(Sender: TObject);
    procedure DaisyTimeComp1GetActions(List: TStrings; var Index: Integer);
//    procedure ButtonPanel1CancelClick;
//    procedure ButtonPanel1OkClick;
    procedure BtnHarvParamClick(Sender: TObject);
    procedure SENoYearsChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnHelpClick(Sender: TObject);
    procedure DaisyTimeComp1Exit(Sender: TObject);
  private
    { Private declarations }
    Dirty : Boolean;
    TinyDirty : Boolean;
    RemoveHarvActions : Boolean;
    Setup : TObject;
    OldNo : Integer;
    OldNoYears: Integer;
    New_i,New_j : Integer; {Year, NoInYear}
    OldCurrAction : TInternAction;
    Procedure DevStageShow;
    Procedure DevStageHide;
    Procedure ClearDevStage;
    Procedure ShowMulti(Multi:Boolean);
    Function GetFuncHarvObj(Sow:TSowing):TFuncHarvest;
    Procedure SetParams(FromFH,ToFH:TFuncHarvest);
    Procedure CreateFuncHarv(Number,Year,NoInYear:Integer;r:Real);
    Procedure DeleteFuncHarv(FH:TFuncHarvest);
    Procedure SetFuncHarv(Number,Year,NoInYear:Integer;FH:TFuncHarvest;r:Real);
    Function Witch_Case: Integer;
    Function Action2Do(i,j:Integer) : Integer;
  public
    { Public declarations }
  end;

var
  FrmEditSow: TFrmEditSow;

implementation

{$R *.DFM}
uses ManGlob,DaisyLib,ShowList;
const szHeight = 350;
      szWidth  = 440;

      DoCreate = 0;
      DoDelete = 1;
      DoSet    = 2;
      DoNoop   = 3;
type
TwoDimList = Class(TList)
   private
      pi,pj : Integer;
   public
      Function GetItem(i,j:Integer):Pointer;
      Function InsertItem(i,j:Integer;Item:Pointer):Boolean;
      Constructor Create(i,j:Integer);
      Destructor Free;
end;

Function TwoDimList.GetItem(i,j:Integer):Pointer;
Var L : TList;
begin
   if (pi > 0) and (pj>0) then begin
      if (i < pi) and (j < pj) then begin
         L := Items[i];
         if L <> nil then
            Result := L.Items[j]
         else
            Result := nil;
      end else
         Result := nil;
   end else
      Result := nil;
end;

Function TwoDimList.InsertItem(i,j:Integer;Item:Pointer):Boolean;
Var L : TList;
begin
   if (pi > 0) and (pj>0) then begin
      if (i < pi) and (j < pj) then begin
         L := Items[i];
         if L <> nil then begin
             try
                L.Insert(j,Item);
                Result := True;
             except
                On EListError do
                   Result := False;
             end;
         end else
            Result := False;
      end else
         Result := False;
   end else
      Result := False;
end;
Constructor TwoDimList.Create(i,j:Integer);
Var L:TList;
begin
   if (i < 0) or (j < 0) then
      Raise EListError.Create('TwoDimList: Negative Indicies Given to Create');
   if (i = 0) and (j > 0) then
      Raise EListError.Create('TwoDimList: Row Zero and Column Posetive');
   if (i > 0) and (j = 0) then
      Raise EListError.Create('TwoDimList: Row Posetive and Column Zero');
   inherited Create;
   if not ((i = 0) and (j = 0)) then begin
      pi := i; pj := j;
      for i := 0 to pi-1 do begin
         L := TList.Create;
         L.Capacity := pj;
         Add(L);
      end;
   end;
end;
Destructor TwoDimList.Free;
Var L:Tlist;
    i:Integer;
begin
   if (pi > 0) and (pj > 0) then begin
       for i := 0 to pi-1 do begin
          L := Items[i];
          L.Free;
       end;
   end;
   {Inherited Free;}
end;

Procedure TFrmEditSow.CreateFuncHarv(Number,Year,NoInYear:Integer;r:Real);
Var FH:TFuncHarvest;
begin
   FH := TFuncHarvest.Create;
   FH.CropSowed := CurrAction as TSowing;
   SetParams(OldcurrAction as TFuncHarvest,FH);
   FH.CropSowed.WitchList.DoAdd(FH.CropSowed.WitchList.GetObjIdx(FH.CropSowed) + (Year*Number) + NoInYear, FH);
   FH.RelateToDevelopmentState(FH.CropSowed,r,NoInYear+1,Year+1);
end;

Procedure TFrmEditSow.DeleteFuncHarv(FH:TFuncHarvest);
Var l: TList;
    i: integer;
    o: TInternAction;
begin
   l := TList.Create;
   FH.TimeDepends(l);
   for i:= 0 to l.Count-1 do begin
      o := l.Items[i];
      o.Unrelate;
   end;
   l.Free;
   FH.Unrelate;
   {FH.WitchList.DoDelete(FH);}
   FH.Free;
end;

Procedure TFrmEditSow.SetFuncHarv(Number,Year,NoInYear:Integer;FH:TFuncHarvest;r:Real);
begin
   SetParams(OldCurrAction as TFuncHarvest,FH);
   with FH.When as TRelToDevelopmentState do
      DevStage := r;
   FH.CropSowed.WitchList.DoAdd(FH.CropSowed.WitchList.GetObjIdx(FH.CropSowed) + (Year*Number) + NoInYear, FH);
end;
Function TFrmEditSow.Witch_Case: Integer;
Var i,j : Integer;
begin
   if OldNo > New_j then
      j := 1
   else if OldNo < New_j then
      j := 2
   else
      j := 0;
   if OldNoYears > New_i then
      i := 3
   else if OldNoYears < New_i then
      i := 6
   else
      i := 0;
   Result := i+j;
end;

Function TFrmEditSow.Action2Do(i,j:Integer) : Integer;
begin
   Case Witch_Case of
      0:
         begin
            Result := DoSet;
         end;
      1:
         begin
            if j < New_j then
               Result := DoSet
            else
               Result := DoDelete;
         end;
      2:
         begin
            if j < OldNo then
               Result := DoSet
            else
               Result := DoCreate;
         end;
      3:
         begin
            if i < New_i then
               Result := DoSet
            else
               Result := DoDelete;
         end;
      4:
         begin
            if (i < New_i) and (j < New_j) then
               Result := DoSet
            else
               Result := DoDelete;
         end;
      5:
         begin
            if (i < New_i) then begin
               if (j < OldNo) then
                  Result := DoSet
               else
                  Result := DoCreate;
            end else begin
               if (j < OldNo) then
                  Result := DoDelete
               else
                  Result := DoNoop;
            end;
         end;
      6:
         begin
            if i < OldNoYears then
               Result := DoSet
            else
               Result := DoCreate;
         end;
      7:
         begin
            if (i < OldNoYears) then begin
               if (j < New_j) then
                  Result := DoSet
               else
                  Result := DoDelete;
            end else begin
               if (j < New_j) then
                  Result := DoCreate
               else
                  Result := DoNoop;
            end;
         end;
      8:
         begin
            if (i < OldNoYears) AND (j < OldNo) then
               Result := DoSet
            else
               Result := DoCreate;
         end;
   end;
end;

Procedure TFrmEditSow.SetParams(FromFH,ToFH:TFuncHarvest);
begin
   if (FromFH <> nil) and (ToFH <> nil) then begin
      with FromFH do begin
         ToFH.SO := SO;
         ToFH.Stem := Stem;
         ToFH.DM := DM;
         ToFH.Leaf:= Leaf;
         ToFH.Stub:= Stub;
      end;
   end;
end;

Function TFrmEditSow.GetFuncHarvObj(Sow:TSowing):TFuncHarvest;
Var L:TList;
    i:Integer;
    Found : Boolean;
begin
   L := TList.Create;
   Sow.WitchList.GetList(TFuncHarvest,L);
   i := 0; Found := False;
   while (i < L.Count) and not Found do begin
      Result := L.Items[i];
      Found := Result.CropSowed = Sow;
      inc(i);
   end;
   if not Found then
      Result := nil;
end;

Procedure TFrmEditSow.ShowMulti(Multi:Boolean);
Var FH:TFuncHarvest;
    S : String;
    r : Real;
    w : TRelToDevelopmentState;
begin
   ChBFuncHarv.Show;
   with CurrAction as TSowing do begin
      OldNo := NoHarvActions;
      OldNoYears := NoYears;
   end;
   if OldNo > 0 then begin
      ChbFuncHarv.Checked := True;
      FH := GetFuncHarvObj(CurrAction as TSowing);
      if FH <> nil then begin
         w := FH.When as TRelToDevelopmentState;
(*
         Str(w.DevStage:5:4,s);
         EBDevStage.Text := s;
*)
         EBDevStage.Text := FormatFloat('0.0000', w.DevStage);
         TBDevStage.Position := Trunc(w.DevStage*1000.0);
      end;
      SENoHarv.Value := OldNo;
      SENoYears.Value := OldNoYears;
      DevStageShow;
   end;
   if Multi then begin
      SENoHarv.MinValue := 1;
      SENoHarv.MaxValue := 10;
      SENoYears.MinValue := 1;
      SENoYears.MaxValue := 100;
   end else begin
      SENoHarv.MinValue := 1;
      SENoHarv.MaxValue := 1;
      SENoYears.MinValue := 1;
      SENoYears.MaxValue := 1;
   end;
end;

Procedure TFrmEditSow.ClearDevStage;
begin
   TBDevStage.Position := 0;
//   EBDevStage.Text := '0.0000';
   EBDevStage.Text := FormatFloat('0.0000', 0);
   SENoHarv.Value := 1;
   SENoYears.Value := 1;
end;

Procedure TFrmEditSow.DevStageShow;
begin
(*
   GBDevStage.Show;
   LblEmergence.Show;
   LblFlowering.Show;
   LblMature.Show;
   TBDevStage.Show;
   EBDevStage.Show;
   BtnHarvParam.Show;
   LblNoHarv.Show;
   SENoHarv.Show;
   LblYears.Show;
   SENoYears.Show;
*)
   GBDevStage.Font.Color := clWindowText;
   GBDevStage.Enabled := TRUE;
   LblEmergence.Enabled := TRUE;
   LblFlowering.Enabled := TRUE;
   LblMature.Enabled := TRUE;
   TBDevStage.Enabled := TRUE;
   EBDevStage.Enabled := TRUE;
   BtnHarvParam.Enabled := TRUE;
   LblNoHarv.Enabled := TRUE;
   SENoHarv.Enabled := TRUE;
   LblYears.Enabled := TRUE;
   SENoYears.Enabled := TRUE;
end;

Procedure TFrmEditSow.DevStageHide;
begin
(*
   GBDevStage.Hide;
   LblEmergence.Hide;
   LblFlowering.Hide;
   LblMature.Hide;
   TBDevStage.Hide;
   EBDevStage.Hide;
   BtnHarvParam.Hide;
   LblNoHarv.Hide;
   SENoHarv.Hide;
   LblYears.Hide;
   SENoYears.Hide;
*)
   GBDevStage.Font.Color := clInactiveCaption; { Neccesary to make text disabled }
   GBDevStage.Enabled := FALSE;
   LblEmergence.Enabled := FALSE;
   LblFlowering.Enabled := FALSE;
   LblMature.Enabled := FALSE;
   TBDevStage.Enabled := FALSE;
   EBDevStage.Enabled := FALSE;
   BtnHarvParam.Enabled := FALSE;
   LblNoHarv.Enabled := FALSE;
   SENoHarv.Enabled := FALSE;
   LblYears.Enabled := FALSE;
   SENoYears.Enabled := FALSE;
end;

procedure TFrmEditSow.CBModelChange(Sender: TObject);
begin
   TinyDirty := True;
   if CBModel.ItemIndex = 0 then CBModel.ItemIndex := -1;
end;

procedure TFrmEditSow.FormResize(Sender: TObject);
begin
   if WindowState <> wsMinimized then begin
      if Height < szHeight then
         Height := szHeight;
      if Width < szWidth then
         Width := szWidth;
   end;
end;

procedure TFrmEditSow.FormShow(Sender: TObject);
Var Crop : TCrop;
begin
   {Init Form}
   DevStageHide;
   ClearDevStage;
   ChbFuncHarv.Hide;
   ChBFuncHarv.checked := False;
   Dirty := False;
   TinyDirty := False;
   RemoveHarvActions := False;
   OldNo := -1;
   CBModel.Items := Lib.GetModelList;
   (* ' ' *)
   CBModel.Items.Insert(0,'- none -');
   (*¨' ' *)

   with CurrAction as TSowing do begin
      EBCrop.Text := what;
      if Model <> '' then
         CBModel.ItemIndex := CBModel.Items.IndexOf(model);
      Crop := Lib.GetCrop(what);
      ShowMulti(Crop.Multi);
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
end;

procedure TFrmEditSow.ChBFuncHarvClick(Sender: TObject);
var
   H          : THarvest;
   TimeList   : TList;
   DeleteList : TList;
   Temp       : TInternAction;
   i          : Integer;
begin
   Dirty := True;
   if ChBFuncHarv.Checked then begin
      DevStageShow;
      DaisyTimeComp1.Enabled := FALSE;
   end
   else begin
      DevStageHide;
      DaisyTimeComp1.Enabled := TRUE;
   end
end;

procedure TFrmEditSow.SENoHarvChange(Sender: TObject);
var Crop : TCrop;
begin
   Crop := Lib.GetCrop(EBCrop.Text);
   if not Crop.Multi then
      SENoHarv.Value := 1
   else
      Dirty := True;
end;

procedure TFrmEditSow.EBDevStageChange(Sender: TObject);
var
//    Code : Integer;
   r : Real;
   x, y: Integer;
   GoodNum: Boolean;
begin
   Dirty := True;
   if EBDevStage.Text <> '' then begin
      EBDevStage.Text := Trim(EBDevStage.Text);
      GoodNum := TRUE;
      try
         r := StrToFloat(EBDevStage.Text);
      except
         on E:Exception do
            GoodNum := FALSE;
      end;
      if GoodNum and (r >= 0) and (r <= 2) then
         TBDevStage.Position := Trunc(r * 1000.0)
      else begin
         x := FrmEditSow.Left + GroupBox1.Left
              + GBDevStage.Left + EBDevStage.Left - 100;
         y := FrmEditSow.Top + GroupBox1.Top
              + GBDevStage.Top + EBDevStage.Top + 100;
         MessageDlgPos(GBDevStage.Caption + ':' + CHR(10) + CHR(13)
                       + 'The value must be in the interval [0.0, 2.0].'
                       , mtInformation, [mbOK], 0, x, y);
         EBDevStage.Text := '';
         EBDevStage.SetFocus;
      end;
(*
      try
         if EBDevStage.Text[Length(EBDevStage.Text)] = '.' then
            Val(EBDevStage.Text+'0',r,Code)
         else
            Val(EBDevStage.Text,r,Code);
         if (Code = 0)and(r >= 0.0) and (r <= 2.0) then
             TBDevStage.Position := Trunc(r * 1000.0)
         else begin
            Application.Messagebox('Development Stage MUST be a number between 0.0 and 2.0','Error',MB_Ok);
            EBDevStage.SetFocus;
         end;
      except
         on EConvertError do begin
         Application.Messagebox('Development Stage MUST be a number between 0.0 and 2.0','Error',MB_Ok);
         EBDevStage.SetFocus;
         end;
      end;
*)
   end else begin
      TBDevStage.Position := 0;
      EBDevStage.Text := FOrmatFloat('0.0000', 0);
   end;
end;

procedure TFrmEditSow.TBDevStageChange(Sender: TObject);
(*
var temp : Real;
    s    : String;
*)
begin
   Dirty := True;
(*
   temp := TBDevStage.Position;
   if temp <> 0.0 then begin
      Str(temp/1000.0:5:4,s);
      EBDevStage.Text := s;
   end else
      EBDevStage.Text := '0.0000';
*)
   if TBDevStage.Position <> 0 then
      EBDevStage.Text := FormatFloat('0.0000', TBDevStage.Position/1000)
   else
      EBDevStage.Text := FormatFloat('0.0000', 0);;
end;

procedure TFrmEditSow.DaisyTimeComp1GetActions(List: TStrings;
  var Index: Integer);
begin
   DoGetList(CurrAction,List);
   if Setup <> nil then begin
      Index := List.IndexOfObject(Setup);
      Setup := nil;
   end;
end;

(*
procedure TFrmEditSow.ButtonPanel1CancelClick;
begin
   Close;
end;
*)

(*
procedure TFrmEditSow.ButtonPanel1OkClick;
Var L,L2:TList;
    i,j,k      : Integer;
    Temp       : TInternAction;
    FH         : TFuncHarvest;
    w          : TRelToDevelopmentState;
    r          : Real;
    c,idx      : Integer;
    TwoDim     : TwoDimList;
    Crop       : TCrop;
    Ok         : Boolean;
    TimeList   : TList;
    DeleteList : TList;
    H          : THarvest;
    function Max(i,j:Integer):Integer;
    begin
       if i<j then Result := j else Result := i;
    end;
begin
   EndTimeEdit(DaisyTimeComp1,CurrAction);
   If Dirty then with CurrAction as TSowing do begin
      Crop := Lib.GetCrop(What);
      if TRUE{ChbFuncHarv.Checked} then begin
         TwoDim := TwoDimList.Create(OldNoYears,OldNo);
         if ChbFuncHarv.Checked then begin
            noHarvActions := SENoHarv.Value;
            NoYears := SENoYears.Value;
            New_i := NoYears;
            New_j := NoHarvActions;
         end else begin
            noHarvActions := 0;
            NoYears := 0;
            New_i := NoYears;
            New_j := NoHarvActions;
         end;
         begin
            L  := TList.Create;
            L2 := TList.Create;
            CurrAction.WitchList.GetList(TFuncHarvest,L);
            for i := 0 to L.Count-1 do begin
               FH := L.Items[i];
               if FH.CropSowed = CurrAction then
                  L2.Add(FH);
            end;
            L.Free;
            Val(EBDevStage.Text,r,c);
            for i := 0 to L2.Count-1 do begin
               FH := L2.Items[i];
               w := FH.When as TRelToDevelopmentState;
               TwoDim.InsertItem(w.YearNumber-1, w.NumberInYear-1,FH);
               {FH.WitchList.DoDelete(FH);}
            end;
            if (OldCurrAction = nil) and (L2.Count > 0) then begin
               OldCurrAction := TFuncHarvest.Create;
               SetParams(L2.Items[0], OldCurrAction as TFuncHarvest);
            end;
         end;
         TimeList := TList.Create;
         DeleteList := TList.Create;
         {Check if an old "plain" harvest action exists}
         H := GetHarvestAction;
         if H <> nil then begin
            H.TimeDepends(TimeList);
         end;

            for i:=0 to max(OldNoYears-1,New_i-1) do begin
               for j:=0 to Max(OldNo-1,New_j-1) do begin
                  Case Action2Do(i,j) of
                     DoNoop:
                        Begin
                        End;
                     DoCreate:
                        Begin
                        End;
                     DoDelete:
                        Begin
                           FH := TwoDim.GetItem(i,j);
                           DeleteList.Add(FH);
                        End;
                     DoSet:
                        Begin
                        End;
                  End;
               end;
            end;
         for i:= 0 to DeleteList.Count-1 do begin
             FH := DeleteList.Items[i];
             with FH as TInternAction do
                Depends(TimeList);
         end;

         if H <> nil then begin
            idx := DeleteList.Add(H);
            if DeleteList.Count > 1 then
               DeleteList.ExChange(0,idx);
         end;
         if (Timelist.Count > 0) or (DeleteList.Count >0) then begin
            Ok := FALSE;
            OKBottomDlg.SetLists(TimeList, DeleteList,'Warning: Accept the following changes');
            OK :=  OKBottomDlg.ShowModal = mrOK;

         end else
            Ok := True;
         DeleteList.Free;
         if Ok then begin
            TimeList.Clear;
            if H <> nil then begin
               H.TimeDepends(TimeList);
               for k :=0 to TimeList.Count-1 do begin
                  Temp := TimeList.Items[k];
                  Temp.UnRelate;
               end;
               H.WitchList.DoDelete(h);
               h.Free;
            end;

            {Remove all old TFuncHarvest from action list}
            for i := 0 to L2.Count-1 do begin
               FH := L2.Items[i];
               FH.WitchList.DoDelete(FH);
            end;
            {Now Change/Delete/Create actions and put them back i action list}
            for i:=0 to max(OldNoYears-1,New_i-1) do begin
               for j:=0 to Max(OldNo-1,New_j-1) do begin
                  Case Action2Do(i,j) of
                     DoNoop:
                        Begin
                        End;
                     DoCreate:
                        Begin
                           CreateFuncHarv(New_j,i,j,r);
                        End;
                     DoDelete:
                        Begin
                           FH := TwoDim.GetItem(i,j);
                           DeleteFuncHarv(FH);
                        End;
                     DoSet:
                        Begin
                           FH := TwoDim.GetItem(i,j);
                           SetFuncHarv(New_j,i,j,FH,r);
                        End;
                  End;
               end;
            end;
            if OldCurrAction <> nil then begin
               OldCurrAction.Free;
               OldCurrAction := nil;
            end;
            L2.Free;
            TwoDim.Free;
            model := CBModel.Text;
            Close;
         end else begin
            noHarvActions := OldNo;
            NoYears := OldNoYears;
            L2.Free;
            TwoDim.Free;
         end;
         TimeList.Free;
      end else { if TRUE }
         Close;
   end else if TinyDirty then begin
      with CurrAction as TSowing do
          model := CBModel.Text;
      Close;
   end else {IF Dirty}
      Close;
end;
*)

procedure TFrmEditSow.BtnHarvParamClick(Sender: TObject);
Var Temp:TInternAction;
    FH:TFuncHarvest;
begin
   Dirty := True;
   OldCurrAction := CurrAction;
   CurrAction := TFuncHarvest.Create;
   with CurrAction as TFuncHarvest do
      CropSowed := OldCurrAction as TSowing;

   SetParams(GetFuncHarvObj(OldCurrAction as TSowing),CurrAction as TFuncHarvest);
   FrmEditHar.ShowModal;
   Temp := CurrAction;
   CurrAction := OldCurrAction;
   OldCurrAction := Temp;
end;

procedure TFrmEditSow.SENoYearsChange(Sender: TObject);
var Crop : TCrop;
begin
   Crop := Lib.GetCrop(EBCrop.Text);
   if not Crop.Multi then
      SENoYears.Value := 1
   else
      Dirty := True;
end;

procedure TFrmEditSow.FormCreate(Sender: TObject);
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
   Caption := LoadStr(RES_FRM_EditSow);

   { Button captions }
   BtnOK.Caption        := LoadStr(RES_BTN_OK);
   BtnCancel.Caption    := LoadStr(RES_BTN_Cancel);
   BtnHelp.Caption      := LoadStr(RES_BTN_Help);
   BtnHarvParam.Caption := LoadSTR(RES_BTN_Harv_Param);

   { Group caption }
   GroupBox1.Caption  := LoadStr(RES_GRP_EditSow_Parameters);
   GBDevStage.Caption := LoadStr(RES_GRP_EditSow_Devel_Stage);

   { Labels }
   LblCrop.Caption      := LoadStr(RES_LBL_EditSow_Crop);
   LblModel.Caption     := LoadStr(RES_LBL_EditSow_Irr_Model);
   LblFuncHarv.Caption  := LoadStr(RES_LBL_EditSow_Generate_Actions);
   LblNoHarv.Caption    := LoadStr(RES_LBL_EditSow_Harvest_Actions);
   LblYears.Caption     := LoadStr(RES_LBL_EditSow_No_Years);
   LblEmergence.Caption := LoadStr(RES_LBL_EditSow_Emergence);
   LblFlowering.Caption := LoadStr(RES_LBL_EditSow_Flowering);
   LblMature.Caption    := LoadStr(RES_LBL_EditSow_Mature);

   { Set time component resources }
   Globals.SetTimeComponentResources(True, DaisyTimeComp1);


      { Init HelpContexts... }
   GroupBox1.HelpContext := HLP_Edit_Sowing_Action;
   EBCrop.HelpContext := HLP_Edit_Sowing_Action;
   CBModel.HelpContext := HLP_Edit_Sowing_Action;
   ChBFuncHarv.HelpContext := HLP_Edit_Sowing_Action;
   SENoHarv.HelpContext := HLP_Edit_Sowing_Action;
   SENoYears.HelpContext := HLP_Edit_Sowing_Action;
   BtnHarvParam.HelpContext := HLP_Edit_Sowing_Action;
   GBDevStage.HelpContext := HLP_Edit_Sowing_Action;
   EBDevStage.HelpContext := HLP_Edit_Sowing_Action;
   TBDevStage.HelpContext := HLP_Edit_Sowing_Action;
   DaisyTimeComp1.HelpContext := HLP_Time_of_Action;
   BtnOK.HelpContext:= HLP_Edit_Sowing_Action;
   BtnCancel.HelpContext:= HLP_Edit_Sowing_Action;
   BtnHelp.HelpContext:= HLP_Edit_Sowing_Action;
end;

procedure TFrmEditSow.BtnOKClick(Sender: TObject);
Var L,L2:TList;
    i,j,k      : Integer;
    Temp       : TInternAction;
    FH         : TFuncHarvest;
    w          : TRelToDevelopmentState;
    r          : Real;
    c,idx      : Integer;
    TwoDim     : TwoDimList;
    Crop       : TCrop;
    Ok         : Boolean;
    TimeList   : TList;
    DeleteList : TList;
    H          : THarvest;
    function Max(i,j:Integer):Integer;
    begin
       if i<j then Result := j else Result := i;
    end;
begin
   EndTimeEdit(DaisyTimeComp1,CurrAction);
   If Dirty then with CurrAction as TSowing do begin
      Crop := Lib.GetCrop(What);
      if TRUE{ChbFuncHarv.Checked} then begin
         TwoDim := TwoDimList.Create(OldNoYears,OldNo);
         if ChbFuncHarv.Checked then begin
            noHarvActions := SENoHarv.Value;
            NoYears := SENoYears.Value;
            New_i := NoYears;
            New_j := NoHarvActions;
         end else begin
            noHarvActions := 0;
            NoYears := 0;
            New_i := NoYears;
            New_j := NoHarvActions;
         end;
         begin
            L  := TList.Create;
            L2 := TList.Create;
            CurrAction.WitchList.GetList(TFuncHarvest,L);
            for i := 0 to L.Count-1 do begin
               FH := L.Items[i];
               if FH.CropSowed = CurrAction then
                  L2.Add(FH);
            end;
            L.Free;
            Val(EBDevStage.Text,r,c);
            for i := 0 to L2.Count-1 do begin
               FH := L2.Items[i];
               w := FH.When as TRelToDevelopmentState;
               TwoDim.InsertItem(w.YearNumber-1, w.NumberInYear-1,FH);
               {FH.WitchList.DoDelete(FH);}
            end;
            if (OldCurrAction = nil) and (L2.Count > 0) then begin
               OldCurrAction := TFuncHarvest.Create;
               SetParams(L2.Items[0], OldCurrAction as TFuncHarvest);
            end;
         end;
         TimeList := TList.Create;
         DeleteList := TList.Create;
         {Check if an old "plain" harvest action exists}
         H := GetHarvestAction;
         if H <> nil then begin
            H.TimeDepends(TimeList);
         end;

            for i:=0 to max(OldNoYears-1,New_i-1) do begin
               for j:=0 to Max(OldNo-1,New_j-1) do begin
                  Case Action2Do(i,j) of
                     DoNoop:
                        Begin
                        End;
                     DoCreate:
                        Begin
                        End;
                     DoDelete:
                        Begin
                           FH := TwoDim.GetItem(i,j);
                           DeleteList.Add(FH);
                        End;
                     DoSet:
                        Begin
                        End;
                  End;
               end;
            end;
         for i:= 0 to DeleteList.Count-1 do begin
             FH := DeleteList.Items[i];
             with FH as TInternAction do
                Depends(TimeList);
         end;

         if H <> nil then begin
            idx := DeleteList.Add(H);
            if DeleteList.Count > 1 then
               DeleteList.ExChange(0,idx);
         end;
         if (Timelist.Count > 0) or (DeleteList.Count >0) then begin
            Ok := FALSE;
            OKBottomDlg.SetLists(TimeList, DeleteList,'Warning: Accept the following changes');
            OK :=  OKBottomDlg.ShowModal = mrOK;

         end else
            Ok := True;
         DeleteList.Free;
         if Ok then begin
            TimeList.Clear;
            if H <> nil then begin
               H.TimeDepends(TimeList);
               for k :=0 to TimeList.Count-1 do begin
                  Temp := TimeList.Items[k];
                  Temp.UnRelate;
               end;
               H.WitchList.DoDelete(h);
               h.Free;
            end;

            {Remove all old TFuncHarvest from action list}
            for i := 0 to L2.Count-1 do begin
               FH := L2.Items[i];
               FH.WitchList.DoDelete(FH);
            end;
            {Now Change/Delete/Create actions and put them back i action list}
            for i:=0 to max(OldNoYears-1,New_i-1) do begin
               for j:=0 to Max(OldNo-1,New_j-1) do begin
                  Case Action2Do(i,j) of
                     DoNoop:
                        Begin
                        End;
                     DoCreate:
                        Begin
                           CreateFuncHarv(New_j,i,j,r);
                        End;
                     DoDelete:
                        Begin
                           FH := TwoDim.GetItem(i,j);
                           DeleteFuncHarv(FH);
                        End;
                     DoSet:
                        Begin
                           FH := TwoDim.GetItem(i,j);
                           SetFuncHarv(New_j,i,j,FH,r);
                        End;
                  End;
               end;
            end;
            if OldCurrAction <> nil then begin
               OldCurrAction.Free;
               OldCurrAction := nil;
            end;
            L2.Free;
            TwoDim.Free;
            model := CBModel.Text;

            ModalResult := mrOk;
            // Close;   -- Not necessary, as we set ModalResult explicitly
         end else begin
            noHarvActions := OldNo;
            NoYears := OldNoYears;
            L2.Free;
            TwoDim.Free;
         end;
         TimeList.Free;
      end else { if TRUE }
         ModalResult := mrOk;
         // Close;   -- Not necessary, as we set ModalResult explicitly
   end else if TinyDirty then begin
      with CurrAction as TSowing do
          model := CBModel.Text;
      ModalResult := mrOk;
      // Close;   -- Not necessary, as we set ModalResult explicitly
   end else {IF Dirty}
      ModalResult := mrOk;
      // Close;   -- Not necessary, as we set ModalResult explicitly
end;

procedure TFrmEditSow.BtnCancelClick(Sender: TObject);
begin
   Close;
end;

procedure TFrmEditSow.BtnHelpClick(Sender: TObject);
begin
   Application.HelpContext(HLP_Edit_Sowing_Action);
end;

procedure TFrmEditSow.DaisyTimeComp1Exit(Sender: TObject);
begin
   { It is allowed to leave the form at any time... }
   if (ActiveControl <> BtnCancel) then
      DaisyTimeComp1.CheckDate;
end;

end.
