unit dsetup;


{
***************
* Unit dsetup *
***************

Project    : Graphical User Interface for the Daisy simulation package

Purpose    : This is the unit for the Action Setup Window

Author     : J&R Informatik I/S (Rino Ranheim)

Date(s)    : 1/12 - 96 Created (RR)
             13/2 - 97 Revised (Jens Jeppesen) to make use of Unit IntAct
             20/3 - 97 Added Draabe og Draabe2 bitmap
}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, StdCtrls, Buttons, Menus, ManGlob, IntAct, Printers,
  ExtCtrls, ComCtrls, Globals, TypInfo;
type
  TFormSetup = class(TForm)
    MMDaisySetup: TMainMenu;
    File1: TMenuItem;
    N1: TMenuItem;
    PrintSetup1: TMenuItem;
    Print1: TMenuItem;
    N2: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    HowtoUseHelp1: TMenuItem;
    SearchforHelpOn1: TMenuItem;
    Contents1: TMenuItem;
    OpenDlgProject: TOpenDialog;
    SaveDlgProject: TSaveDialog;
    PrinterSetupDlg: TPrinterSetupDialog;
    PBtns: TPanel;
    BtnCancel: TBitBtn;
    BtnHelp: TBitBtn;
    PDefinedActions: TPanel;
    PNewAction: TPanel;
    PTopDA: TPanel;
    PLeftDA: TPanel;
    PRightDA: TPanel;
    PBottomDA: TPanel;
    GrpDefinedActions: TGroupBox;
    PTopHalfDA: TPanel;
    PBottomHalfDA: TPanel;
    BtnDeleteAction: TButton;
    BtnEditAction: TButton;
    PTopPTHDA: TPanel;
    PLeftPTHDA: TPanel;
    PRightPTHDA: TPanel;
    PBottomPTHDA: TPanel;
    PCenterPTHDA: TPanel;
    SGActions: TStringGrid;
    PTopNA: TPanel;
    PLeftNA: TPanel;
    PRightNA: TPanel;
    PBottomNA: TPanel;
    GrpNewAction: TGroupBox;
    BtnSowing: TButton;
    BtnHarvest: TButton;
    BtnFertilization: TButton;
    BtnTillage: TButton;
    BtnIrrigation: TButton;
    N3: TMenuItem;
    BtnOK: TBitBtn;
    ClearManager1: TMenuItem;
    LoadManager1: TMenuItem;
    SaveManager1: TMenuItem;
    Close1: TMenuItem;
    EBManagerHeading: TEdit;
    LblManagerHeading: TLabel;
    SaveManagerAs1: TMenuItem;
    procedure InitSGActions;
{    procedure InsertNewAction(a: TAction; before: Boolean);}
    procedure InsertNewAction(a: TInternAction; before: Boolean);
{    procedure WantToClose;}
    procedure GetSelectedAction(var a: TInternAction);

    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure BtnSowingClick(Sender: TObject);
    procedure BtnHarvestClick(Sender: TObject);
    procedure BtnFertilizationClick(Sender: TObject);
    procedure BtnTillageClick(Sender: TObject);
    procedure BtnIrrigationClick(Sender: TObject);
    procedure BtnEditActionClick(Sender: TObject);
    procedure PrintSetup1Click(Sender: TObject);
    procedure MakeDaisySetupFile1Click(Sender: TObject);
    procedure SGActionsColumnMoved(Sender: TObject; FromIndex,
      ToIndex: Longint);
    procedure BtnDeleteActionClick(Sender: TObject);
    procedure SGActionsDrawCell(Sender: TObject; Col, Row: Longint;
      Rect: TRect; State: TGridDrawState);
    procedure FormShow(Sender: TObject);
    procedure Print1Click(Sender: TObject);
    procedure PCenterPTHDAResize(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure PBottomHalfDAResize(Sender: TObject);
    procedure PBtnsResize(Sender: TObject);
    procedure SGActionsDblClick(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);
    procedure SetFormFromVars;
    function  CanClose(Cancel: Boolean): Boolean;
    procedure BtnCancelClick(Sender: TObject);
    procedure Close1Click(Sender: TObject);
    procedure LoadManager1Click(Sender: TObject);
    procedure Exporttofile1Click(Sender: TObject);
    procedure Importfromfile1Click(Sender: TObject);
    procedure EBManagerHeadingChange(Sender: TObject);
    procedure SaveManager1Click(Sender: TObject);
    procedure ClearManager1Click(Sender: TObject);
    procedure SGActionsSelectCell(Sender: TObject; Col, Row: Longint;
      var CanSelect: Boolean);
    procedure Contents1Click(Sender: TObject);
    procedure SearchforHelpOn1Click(Sender: TObject);
    procedure HowtoUseHelp1Click(Sender: TObject);
    procedure BtnHelpClick(Sender: TObject);
    procedure SaveManagerAs1Click(Sender: TObject);
    procedure SGActionsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
     LibraryHeading : String;
     procedure UpdateButtons;                      { Update state of buttons }
    { Private declarations }
  public
     procedure Populate;
     Function GetNumLines:Integer;
     Procedure GetOneLine(S:TStringList;I:Integer);
    { Public declarations }
  end;

var
  FormSetup: TFormSetup;
  ColIdx : Array[0..3] of Longint;

implementation
uses SADlg, Parser, EditTill, EditIrr, EditHarv, EditFert, EditSow, ShowList,
   About, LibComp, UseLib, DHeading, EditDate, SaveAs;
{$R *.DFM}
Type
{Til Print}
PrintDimentions = Record
     PWidth, PHeight :Integer;
     ColPos : Array [1..7] of Integer;
     ColWidths : Array [1..8] of Integer;
     LineHeight : Integer;
     Margin : Integer;
End;
{Til Print}



var
   Draabe : TBitmap;
   Draabe2 : TBitmap;
   Ovs_Line         : String;
   Ovs_Action       : String;
   Ovs_Action_Spec  : String;
   Ovs_Action_Time  : String;

function LogToPhys(LogIdx:Longint): Longint;
var idx:longint;
begin
     idx := 0;
     while ColIdx[idx] <> LogIdx do idx := idx + 1;
     LogToPhys := idx;
end;

procedure TFormSetup.UpdateButtons;
begin
   { If the Start or End actions is selected, the <Delete> button
   { should be disabled. }
   if (SGActions.Row = 1) or (SGActions.Row = SGActions.RowCount - 1) then
      BtnDeleteAction.Enabled := FALSE
   else
      BtnDeleteAction.Enabled := TRUE;
end;

procedure TFormSetup.SetFormFromVars;
begin
   InternalActionList := TInternActionList.Create;
   with TempManagerVars do begin
      if ManagerHeading = '' then begin
         InternalActionList.NewList;
         EBManagerHeading.Text:= '';
         EBManagerHeading.Enabled := True;
         LibraryHeading := '';
      end else begin
         EBManagerHeading.Text := ManagerHeading;
         EBManagerHeading.Enabled := False;
         TheAlist := GetLibraryObject('action',ManagerHeading);
         if CreateActionListFromAlist(TheAlist, InternalActionList as TList) then begin
         end else begin
            // ShowMessage('Error: Unable to create Manager actions');
            InternalActionList.NewList;
         end;
      end;
   end;
end;

procedure TFormSetup.Populate;
var action : TInternAction;
    i : integer;
begin
     if InternalActionList.Count = 0 then begin
        SGActions.RowCount := 2;
        SGActions.Rows[1].Clear;
        SGActions.Objects[LogToPhys(1),1] := nil;
     end else
        SGActions.RowCount := InternalActionList.Count+1;
     with SGActions, InternalActionList do begin
          for i := 0 to InternalActionList.Count-1 do begin
             action := Items[i];
             Cells[LogToPhys(0),i+1]   := IntToStr(i+1);
             Cells[LogToPhys(1),i+1]   := action.ActionName;
             Objects[LogToPhys(1),i+1] := action;
             if Objects[LogToPhys(1),i+1] = nil then
                Application.Terminate;
             { Specification is not enough when a harvest action }
             { also need an index from the action list }
             {if action is THarvest then begin
                with action as THarvest do begin
                   if CropSowed <> nil then
                      Cells[LogToPhys(2),i+1]:= IntToStr(IndexOf(CropSowed)+1) + ' ' + action.Specification
                   else
                      Cells[LogToPhys(2),i+1]   := action.Specification;
                end;
             end else}
                Cells[LogToPhys(2),i+1]   := action.Specification;
             if action.when <> nil then
                Cells[LogToPhys(3),i+1]   := action.when.TimeSpec
             else
                Cells[LogToPhys(3),i+1]   := '';
          end;
     end;
end;


procedure TFormSetup.InitSGActions;
var i: Integer;
begin
   with SGActions do
   begin
      for i:=0 to RowCount-1 do
         Rows[i].Clear;
      Rowcount := 2;
      { Setup column header strings }
      Cells[LogToPhys(0), 0] := LoadStr(RES_HED_DSetup_Man_Actions_Line);
      Cells[LogToPhys(1), 0] := LoadStr(RES_HED_DSetup_Man_Actions_Type);
      Cells[LogToPhys(2), 0] := LoadStr(RES_HED_DSetup_Man_Actions_Spec);
      Cells[LogToPhys(3), 0] := LoadStr(RES_HED_DSetup_Man_Actions_Time);
   end;
end;


procedure TFormSetup.InsertNewAction(a: TInternAction; before: Boolean);
var
   SelRow, i: LongInt;
   b: Integer;
begin
   if before then b := 1 else b := 0;
   with SGActions do
   begin
        if Row <> RowCount-1 then
            SelRow := Row+1
        else
            SelRow := Row;
   end;
   with InternalActionList do begin
       DoAdd(SelRow-1,a);
   end;
   Populate;
   SGActions.Row := SelRow;
end;

procedure TFormSetup.GetSelectedAction(var a: TInternAction);
begin
   with SGActions do
   begin
      a := (Objects[LogToPhys(1), Row] as TInternAction);
   end;
end;


procedure TFormSetup.FormCreate(Sender: TObject);
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
     { THIS IS INCOMPLETE!!!!! }
(*
   Draabe := TBitmap.Create;
   Draabe.LoadFromFile('Dråbe2.bmp');
   Draabe2 := TBitmap.Create;
   Draabe2.LoadFromFile('Dråbe4.bmp');
*)
   Draabe := TBitmap.Create;
   Draabe.LoadFromResourceName(HInstance,'draabex');
   Draabe2 := TBitmap.Create;
   Draabe2.LoadFromResourceName(HInstance,'draabey');
   ColIdx[0] := 0;
   ColIdx[1] := 1;
   ColIdx[2] := 2;
   ColIdx[3] := 3;

   { Set form caption }
   FormSetup.Caption          := LoadStr(RES_FRM_DSetup) + ' - ' + CurrProject;
   { Set groupbox captions }
   GrpNewAction.Caption       := LoadStr(RES_GRP_DSetup_Man_New_Action);
   GrpDefinedActions.Caption  := LoadStr(RES_GRP_DSetup_Man_Defined_Actions);
   { Set button captions }
   BtnOK.Caption              := LoadStr(RES_BTN_Ok);
   BtnCancel.Caption          := LoadStr(RES_BTN_Cancel);
   BtnHelp.Caption            := LoadStr(RES_BTN_Help);
   BtnTillage.Caption         := LoadStr(RES_BTN_Tillage);
   BtnSowing.Caption          := LoadStr(RES_BTN_Sowing);
   BtnFertilization.Caption   := LoadStr(RES_BTN_Fertilization);
   BtnHarvest.Caption         := LoadStr(RES_BTN_Harvest);
   BtnIrrigation.Caption      := LoadStr(RES_BTN_Irrigation);
   BtnEditAction.Caption      := LoadStr(RES_BTN_Edit);
   BtnDeleteAction.Caption    := LoadStr(RES_BTN_Delete);
   { Set label captions }
   LblManagerHeading.Caption  := LoadStr(RES_LBL_DSetup_Man_Heading);
   { Set Grid Headings }
   Ovs_Line                   := LoadStr(RES_HED_DSetup_Man_Actions_Line);
   Ovs_Action                 := LoadStr(RES_HED_DSetup_Man_Actions_Type);
   Ovs_Action_Spec            := LoadStr(RES_HED_DSetup_Man_Actions_Spec);
   Ovs_Action_Time            := LoadStr(RES_HED_DSetup_Man_Actions_Time);
   { Init menues... }
   File1.Caption              := LoadStr(RES_MNU_File);
   ClearManager1.Caption      := LoadStr(RES_MNU_Clear);
   LoadManager1.Caption       := LoadStr(RES_MNU_Load);
   SaveManager1.Caption       := LoadStr(RES_MNU_Save);
   SaveManagerAs1.Caption     := LoadStr(RES_MNU_Save_As);
   Print1.Caption             := LoadStr(RES_MNU_Man_Print_Actions);
   PrintSetup1.Caption        := LoadStr(RES_MNU_Printer_Setup);
   Close1.Caption             := LoadStr(RES_MNU_Close);
   Help1.Caption              := LoadStr(RES_MNU_Help);
   Contents1.Caption          := LoadStr(RES_MNU_Help_Contents);
   SearchforHelpOn1.Caption   := LoadStr(RES_MNU_Help_Search);
   HowtoUseHelp1.Caption      := LoadStr(RES_MNU_Help_How_To);
   About1.Caption             := LoadStr(RES_MNU_Help_About);

      { Init HelpContexts... }
   EBManagerHeading.HelpContext := HLP_Management_Heading;
   GrpNewAction.HelpContext := HLP_New_Action;
   BtnTillage.HelpContext := HLP_New_Action;
   BtnSowing.HelpContext := HLP_New_Action;
   BtnFertilization.HelpContext := HLP_New_Action;
   BtnHarvest.HelpContext := HLP_New_Action;
   BtnIrrigation.HelpContext := HLP_New_Action;
   GrpDefinedActions.HelpContext := HLP_Defined_Actions;
   SGActions.HelpContext := HLP_Defined_Actions;
   BtnEditAction.HelpContext := HLP_Edit_Action;
   BtnDeleteAction.HelpContext := HLP_Defined_Actions;
   BtnOK.HelpContext := HLP_Management;
   BtnCancel.HelpContext := HLP_Management;
   BtnHelp.HelpContext := HLP_Management;




   InitSGActions;
end;

procedure TFormSetup.FormActivate(Sender: TObject);
begin
   FormSetup.Caption := LoadStr(RES_FRM_DSetup) + ' - ' + CurrProject;
end;


procedure TFormSetup.BtnSowingClick(Sender: TObject);
Var SActions : TStringList;
begin
   with DlgSelectAction do
   begin
      ActionType := 1;
      SActions := Lib.GetCropList;
      SetListText(SActions);
      ShowModal;
      SActions.Free;
   end;
end;


procedure TFormSetup.BtnHarvestClick(Sender: TObject);
var tempSow : TSowing;
    tempHarv : THarvest;
    tempFuncHarv : TFuncHarvest;
    SowList : TList;
    HarvList : TList;
    FuncHarvList : TList;
    i,j : integer;
    tempStr : string;
    HActions : TStringList;
    found : boolean;
begin
   with DlgSelectAction do
   begin
      {Get All Sowing actions }
      SowList := TList.Create;
      HarvList := TList.Create;
      FuncHarvList := TList.Create;
      with InternalActionList do begin
          {Get all sowing actions}
          GetList(TSowing,SowList);
          {Get all harvest actions}
          GetList(THarvest,HarvList);
          GetList(TFuncHarvest,FuncHarvList);
          HActions := TStringList.Create;
          for i:= 0 to SowList.Count -1 do begin
              tempSow := SowList.Items[i];
              j:=0; found := false;
              while (j<HarvList.Count) and not found do begin
                 tempHarv := HarvList.Items[j];
                 found := tempSow = tempHarv.CropSowed;
                 inc(j);
              end;
              if not found then begin
                 j:=0;
                 while (j<FuncHarvList.Count) and not found do begin
                    tempFuncHarv := FuncHarvList.Items[j];
                    found := tempSow = tempFuncHarv.CropSowed;
                    inc(j);
                 end;
              end;
              {Insert only sowing actions not allready harvested}
              if not found then begin
                  tempStr:= IntToStr(IndexOf(tempSow)+1) + '   ' + tempSow.Specification;
                  HActions.Add(tempStr);
              end;
          end;
      end;
      ActionType := 3;
      SetListText(HActions);
      ShowModal;
      SowList.Free;
      HarvList.Free;
      HActions.Free;
   end;
end;


procedure TFormSetup.BtnFertilizationClick(Sender: TObject);
Var FActions : TStringList;
begin
   with DlgSelectAction do
   begin
      ActionType := 2;
      FActions := Lib.GetFertilizerList;
      SetListText(FActions);
      ShowModal;
      FActions.Free;
   end;
end;

procedure TFormSetup.BtnTillageClick(Sender: TObject);
Var TActions:TStringList;
begin
   with DlgSelectAction do
   begin
      ActionType := 0;
      TActions := Lib.GetTillList;
      SetListText(TActions);
      ShowModal;
      TActions.Free;
   end;
end;



procedure TFormSetup.BtnIrrigationClick(Sender: TObject);
var IrrA : TIrrigate;
begin
   IrrA := TIrrigate.Create;
   InsertNewAction(IrrA, InsertBefore);
   { Manager form is now dirty. }
   TempManagerVars.Dirty := True;
end;


procedure TFormSetup.BtnEditActionClick(Sender: TObject);
var
   ANumber, x, y : Integer;
   ButtonPushed  : TModalResult;
begin
   { Initially no <Ok> button has been clicked. }
   ButtonPushed := mrNone;

   with SGActions do
   begin
      if (Row > 0) and (InternalActionList.Count > 0) then { A valid row is selected }
      begin
         x := FormSetup.Left + SGActions.Left + 100;
         y := FormSetup.Top  + SGActions.Top + 180;
         GetSelectedAction(CurrAction);
         if CurrAction is TTillage then
            ButtonPushed := FrmEditTill.ShowModal
         else if CurrAction is TIrrigate then
            ButtonPushed := FrmEditIrr.ShowModal
         else if CurrAction is THarvest then
            ButtonPushed := FrmEditHar.ShowModal
         else if CurrAction is TFertilize then
            ButtonPushed := FrmEditFert.ShowModal
         else if CurrAction is TSowing then
            ButtonPushed := FrmEditSow.ShowModal
         else if CurrAction is TFuncHarvest then begin
             CurrAction := TFuncHarvest(CurrAction).CropSowed;
            ButtonPushed := FrmEditSow.ShowModal
         end else if CurrAction is TStart then begin

            with FormDate do begin
               InitFromAction(LoadStr(RES_MSC_DSetup_Man_StartDate)
                              , CurrAction, HLP_Time_of_Action, x, y);
               ButtonPushed := ShowModal;
               if ButtonPushed = mrOK then
                  SetActionDateFromFormDate(CurrAction);
            end
(*
            FrmEditStart.SetCaption('Edit Start Action');
            FrmEditStart.ShowModal
*)
         end else if CurrAction is TEnd then begin

            with FormDate do begin
               InitFromAction(LoadStr(RES_MSC_DSetup_Man_EndDate)
                              , CurrAction, HLP_Time_of_Action, x, y);
               ButtonPushed := ShowModal;
               if ButtonPushed = mrOK then
                  SetActionDateFromFormDate(CurrAction);
            end
(*
            FrmEditStart.SetCaption('Edit End Action');
            FrmEditStart.ShowModal
*)
         end;
         If ButtonPushed = mrOk then
            begin
{               FormDate.SetActionDateFromFormDate(CurrAction);}
               Populate;
               TempManagerVars.Dirty := True;
            end;
      end;
   end;
end;


procedure TFormSetup.PrintSetup1Click(Sender: TObject);
begin
   PrinterSetupDlg.Execute;
end;

procedure TFormSetup.MakeDaisySetupFile1Click(Sender: TObject);
begin
(*
   if InternalActionList.TopSort(TRUE) then begin
      Save1Click(Self);
   end else begin
      Application.MessageBox('Unabel to create setup file - some action(s) are missing timespecification','Error',MB_APPLMODAL);
   end;
*)
end;

procedure TFormSetup.SGActionsColumnMoved(Sender: TObject; FromIndex,
  ToIndex: Longint);
var
  temp, diff, step, idx: Longint;
begin
   diff := FromIndex - ToIndex;
   case (diff) of
   -3,-2,2,3 :
          begin
             temp := ColIdx[FromIndex];
             if diff < 0 then begin
                for idx := FromIndex to ToIndex-1 do
                    ColIdx[idx] := ColIdx[idx+1];
                ColIdx[ToIndex] := temp;
             end else begin
                for idx := FromIndex downto ToIndex+1 do
                    ColIdx[idx] := ColIdx[idx-1];
                ColIdx[ToIndex] := temp;
             end;

          end;
   -1,1 : begin
             temp := ColIdx[ToIndex];
             ColIdx[ToIndex] := ColIdx[FromIndex];
             ColIdx[FromIndex] := temp;
          end;
   end;
end;


procedure TFormSetup.BtnDeleteActionClick(Sender: TObject);
var
   SelRow, i: LongInt;
   hindex: LongInt;
   Action, tAction, temp : TInternAction;
   TimeList, HarvestList : TList;
   j,k:integer;
begin
   if (SGActions.Row > 0) and (InternalActionList.count > 0)  then begin
       SelRow := SGActions.Row;
       with InternalActionList do begin
         Action := Items[SelRow-1];
         if not ((Action is TStart) or (Action is TEnd) or (Action is TFuncHarvest)) then begin
           { Deletion of a row makes the form dirty}
           TempManagerVars.Dirty := True;

           BtnDeleteAction.Cursor := crHourGlass;
           TimeList := TList.Create;
           HarvestList := TList.Create;
           Action.TimeDepends(TimeList);
           if Action is TSowing then with Action as TSowing do begin
              HarvestDepends(HarvestList);
              for j := 0 to HarvestList.Count-1 do begin
                 temp := HarvestList.Items[j];
                 temp.TimeDepends(TimeList);
              end;
              for j := 0 to HarvestList.Count-1 do begin
                  temp := HarvestList.Items[j];
                  k := TimeList.IndexOf(temp);
                  if k <> -1 then
                     TimeList.Delete(k);
              end;
           end;
           if (TimeList.Count > 0) or (HarvestList.Count > 0) then begin
              OKBottomDlg.SetLists(TimeList, HarvestList, LoadStr(RES_WRN_Warning)
                                   + LoadStr(RES_WRN_DSetup_Man_Action_Dependence));
              if OKBottomDlg.ShowModal = mrOK then
              {if Application.MessageBox('Other Actions depends of this action, OK to remove dependencies', 'Warning', mb_OKCancel +
                                         mb_DefButton1) = IDOK then}
              begin
                 for j := 0 to TimeList.Count-1 do begin
                    temp := TimeList.Items[j];
                    temp.UnRelate;
                 end;
                 for j:= 0 to HarvestList.Count-1 do begin
                    temp := HarvestList.Items[j];
                    DoDelete(temp);
                    temp.Free;
                 end;
              end else begin
                 TimeList.Free;
                 HarvestList.Free;
                 BtnDeleteAction.Cursor := crDefault;
                 Exit;
              end;
           end;
           BtnDeleteAction.Cursor := crDefault;
           TimeList.Free;
           HarvestList.Free;
           DoDelete(Action);
           Action.Free;
         end else
            Exit;
{         if SelRow -1 > 1 then
            SGActions.Row := SelRow -1 ;}
       end;
       Populate;
   end;

   UpdateButtons;
end;

procedure TFormSetup.SGActionsDrawCell(Sender: TObject; Col, Row: Longint;
  Rect: TRect; State: TGridDrawState);
var obj : TObject;
  c,r : integer;
begin
    c := Col;
    r := Row;
     with SGActions do begin
        if Objects[c,r] <> nil then begin
           obj := Objects[c,r];
           if obj is TSowing then begin
              with obj as TSowing do begin
                  if model <> '' then begin
                     with Rect do begin
                        if SGActions.Row = r then
                           Canvas.Draw(Right-Draabe2.Width-1, Top + ((Bottom - Top - Draabe2.Height)DIV 2),Draabe2)
                        else
                           Canvas.Draw(Right-Draabe.Width-1, Top + ((Bottom - Top - Draabe.Height)DIV 2),Draabe);
                     end;
                  end;
              end;
           end;
        end;
     end;

   UpdateButtons;
end;

procedure TFormSetup.FormShow(Sender: TObject);
begin
   { Initially the form is not dirty. }
   TempManagerVars.Dirty := False;

   { First set colwidth to fill stringgrids in total width }
   SGActions.ColWidths[0] := Trunc(2.5 * SGActions.ClientWidth / 20);
   SGActions.ColWidths[1] := Trunc(4 * SGActions.ClientWidth / 20);
   SGActions.ColWidths[2] := Trunc(9 * SGActions.ClientWidth / 20);
   SGActions.ColWidths[3] := SGActions.ClientWidth - SGActions.ColWidths[0]
                             - SGActions.ColWidths[1] - SGActions.ColWidths[2] - 1;

   LibraryHeading := '';
   SetFormFromVars;

   { If we are defining a new manager, the manager heading is enabled. }
   { Otherwise it is disabled. As a consequence it should only have the }
   { initial focus if it is enabled. }
   if EBManagerHeading.Enabled then begin
      EBManagerHeading.SetFocus;
      SaveManager1.Enabled := TRUE;
   end
   else begin
      SGActions.SetFocus;
      SaveManager1.Enabled := FALSE;
   end;

   {Update;}
   Populate;
   UpdateButtons;
end;

{***** P R I N T *****}



{*********************** P R I N T    *****************}
{
Draw Frame and return Vert pos or first line in frame
}

Procedure DrawBitMap(dx,dy:LongInt;TheCanvas: TCanvas; BitMap : TBitMap);
var
    x,y : LongInt;
begin
   with BitMap.Canvas do begin
      for x:=0 to BitMap.Width - 1 do
          for y := 0 to BitMap.Height do
              TheCanvas.Pixels[dx+x,dy+y] := Pixels[x,y];
   end;
end;


Function MakeFrame(var pd:PrintDimentions;c:TCanvas;PageNo:Integer; Logo: TBitMap):Integer;
var
   TheRect   : TRect;
   VCenter   : Integer;
   HCenter   : Integer;
   ColMarg   : Integer;
   VFrameTop : Integer;
   HFrameTop : Integer;
   SaveSize  : Integer;
   Function Max(i,j : Integer) : Integer;
   begin
      if i < j then result := j else result := i;
   end;
begin
   With c,pd do begin
      With Brush do begin
         Color := clBlack;
         Style := bsVertical;
      End;
      {DRAW HEADER}
      With Font do begin
         SaveSize := Size;
         Size := Size * 2;
      end;

      VFrameTop := TextHeight('Vejdir')+(TextHeight('Vejdir')div 2);
      VFrameTop := Max(VFrameTop,Logo.Height+2);
      VCenter   := (VFrameTop-Logo.Height)div 2;
      DrawBitMap(Margin,VCenter,c,Logo);
      VCenter   := (VFrameTop-TextHeight('Vejdir'))div 2;
      TextOut(Margin+Logo.Width+TextWidth(' '),VCenter,LoadStr(RES_MSC_AFA)
               + ', ' + LoadStr(RES_MSC_DSetup_Man_List_of_Actions) + ', '
               + LoadStr(RES_MSC_Page) + ' ' + IntToStr(PageNo));
      Font.Size := SaveSize;
      HFrameTop := Margin;
      VCenter := (LineHeight-TextHeight(Ovs_Line))div 2;
      ColMarg := TextWidth(' ');
      { Draw Frame }
      With TheRect do begin
         Left   := HFrameTop;
         Top    := VFrameTop;
         Right  := PWidth;
         Bottom := PHeight - 1;
      End;
      FrameRect(TheRect);
      {Linie No}
      MoveTo(ColPos[1],VFrameTop);
      LineTo(ColPos[1],PHeight);
      TextOut(Margin+ColMarg,VFrameTop+VCenter,Ovs_Line);
      {Action Type}
      MoveTo(ColPos[2],VFrameTop);
      LineTo(ColPos[2],PHeight);
      TextOut(ColPos[1]+ColMarg,VFrameTop+VCenter,Ovs_Action);
      {Action Specification}
      MoveTo(ColPos[3],VFrameTop);
      LineTo(ColPos[3],PHeight);
      TextOut(ColPos[2]+ColMarg,VFrameTop+VCenter,Ovs_Action_Spec);
      {Time Of Action}
      {MoveTo(ColPos[4],VFrameTop+LineHeight);
      LineTo(ColPos[4],PHeight);}
      TextOut(ColPos[3]+ColMarg,VFrameTop+VCenter,Ovs_Action_Time);
      {...}

      MoveTo(Margin,VFrameTop+2*LineHeight);
      LineTo(PWidth,VFrameTop+2*LineHeight);
      Result := VFrameTop + VCenter + 2*LineHeight;
   End;
end;

procedure DoSetup(var pd:PrintDimentions;Prn:TPrinter);

var LineNoWidth              : Integer;
    ActionTypeWidth          : Integer;
    ActionSpecificationWidth : Integer;
    TimeOfActionWidth        : Integer;
Begin
   with pd, Prn do begin
      PWidth := PageWidth ;
      PHeight:= PageHeight;
      With Canvas do begin
         LineNoWidth              := TextWidth (Ovs_Line + ' ') * 2;
         ActionTypeWidth          := TextWidth (Ovs_Action + ' ') * 2;
         LineHeight               := TextHeight('Mjgf')+(TextHeight('Mjgf') div 2);
      end;
      ActionSpecificationWidth := (PageWidth-(LineNoWidth)-(ActionTypeWidth)) div 2;
      Margin    := (PageWidth-(LineNoWidth)-(ActionTypeWidth)) mod 2;

      ColWidths[1] :=Margin+LineNoWidth;
      ColWidths[2] :=ActionTypeWidth;
      ColWidths[3] :=ActionSpecificationWidth;
      ColWidths[4] :=ActionSpecificationWidth;

      ColPos[1] := ColWidths[1];
      ColPos[2] := ColPos[1] + ColWidths[2];
      ColPos[3] := ColPos[2] + ColWidths[3];
      ColPos[4] := ColPos[3] + ColWidths[4];
   End;
End;

Function TFormSetup.GetNumLines:Integer;
begin
   with SGActions do begin
      if RowCount = 2 then
         result := 0
      else begin
         if Cells[0,RowCount-1] = '' then
            result := RowCount - 2
         else
            result := RowCount -1;
      end;
   End;
end;

Procedure TFormSetup.GetOneLine(S:TStringList;I:Integer);
var j : Integer;
Begin
   with SGActions do begin
      S.Clear;
      for j := 0 to 3 do
         S.Add(Cells[j,i+1]);
      end;
End;

Function CalcHeight(pd:PrintDimentions;c:TCanvas;S:TStringList):Integer;
Begin
   With c,pd do begin
      if (TextWidth(S[0]) >= ColWidths[1]) or (TextWidth(S[1]) >= ColWidths[2]) then
         Result := 2 * LineHeight
      else
         Result := LineHeight;
   end;
End;

Procedure SplitString(s:String;var rs1:String;var rs2:String);
begin
end;

Function PrintOneLine(pd:PrintDimentions;c:TCanvas;CurY:Integer;S:TStringList):Integer;
var
   Marg : Integer;
   TwoLines     : Boolean;
   VCenter      : Integer;
   i            : Integer;
   SavePenStyle : TPenStyle;

Begin
   with pd,c do begin
      Marg := TextWidth(' ');
      TwoLines := False;
      VCenter := (LineHeight-TextHeight(Ovs_Line))div 2;

      if TextWidth(S[0]) >= ColWidths[1] then begin
         TwoLines := True;
         TextOut(Marg,CurY+VCenter,Copy(S[0],1,Length(S[0])div 2));
         TextOut(Marg,CurY+LineHeight+VCenter,Copy(S[0],Length(S[0])div 2,Length(S[0])div 2));
      end else
         TextOut(Marg,CurY+VCenter,S[0]);

      if TextWidth(S[1]) >= ColWidths[2] then begin
         TwoLines := True;
         TextOut(ColPos[1]+Marg,CurY+VCenter,Copy(S[1],1,Length(S[1])div 2));
         TextOut(ColPos[1]+Marg,CurY+LineHeight+VCenter,Copy(S[1],Length(S[1])div 2,Length(S[1])div 2));
      end else
         TextOut(ColPos[1]+Marg,CurY+VCenter,S[1]);
      for i := 2 to 3 do begin
          if TwoLines then
             TextOut(ColPos[i]+Marg,CurY+LineHeight+VCenter,S[i])
          else
             TextOut(ColPos[i]+Marg,CurY+VCenter,S[i]);
      end;
      with pen do begin
         SavePenStyle := Style;
         Style := psDot;
         if TwoLines then begin
            MoveTo(Margin,CurY+2*LineHeight);
            LineTo(PWidth,CurY+2*LineHeight);
         end else begin
            MoveTo(Margin,CurY+LineHeight);
            LineTo(PWidth,CurY+LineHeight);
         end;
         Style := SavePenStyle;
      end;
      if TwoLines then
         Result := 2*LineHeight
      else
         Result := LineHeight;
   end;
End;


procedure TFormSetup.Print1Click(Sender: TObject);
var PrintText : Textfile;
    Line      : Integer;
    PrnHeight, PrnWidth : Integer;
    Page, LineNo : Integer;
    TheRect1,TheRect2 : TRect;

    {***}
    SaveOrient   : TPrinterOrientation;
    PD           : PrintDimentions;
    NumLines     : Integer;
    PageNo       : Integer;
    CurY         : Integer;
    NeededY      : Integer;
    i            : Integer;
    Strings      : TStringList;
    LogoBMP      : TBitMap;
    xx           : array[0..255] of char;
begin
   LogoBMP := TBitMap.Create;
   LogoBMP.Handle := LoadBitMap(HInstance, StrPCopy(xx,GetString(61)));
   With Printer do begin
      SaveOrient := Orientation;
      Orientation := poLandscape;
      DoSetup(PD,Printer);
      NumLines := GetNumLines;
      if NumLines > 0 then begin
         Strings := TStringList.Create;
         BeginDoc;
         PageNo := 1;
         CurY:=MakeFrame(PD,Canvas,PageNo,LogoBMP);
         for i := 0 to NumLines - 1 do begin
            GetOneLine(Strings,i);
            NeededY := CalcHeight(PD,Canvas,Strings);
            if CurY + NeededY >= PD.PHeight then begin
               Inc(PageNo);
               NewPage;
               CurY := MakeFrame(PD,Canvas,PageNo,LogoBMP);
            end;
            CurY := CurY + PrintOneLine(PD,Canvas,CurY,Strings);
         end;
         Strings.Free;
         EndDoc;
         Orientation := SaveOrient;
      end else begin
      end;
   End;
   LogoBMP.Free;
end;



{***** P R I N T  S L U T *****}

procedure TFormSetup.PCenterPTHDAResize(Sender: TObject);
var size    : Real;
    i, OldW : integer;
begin
   with SGActions do begin
      OldW   := Width;
      Width  := PCenterPTHDA.Width;
      Height := PCenterPTHDA.Height;
      for i := 0 to 3 do
         ColWidths[i] := Trunc(ColWidths[i] / OldW * Width);
   end;
end;

procedure TFormSetup.FormResize(Sender: TObject);
begin
   with Self as TFormSetup do begin
      if WindowState <> wsMinimized then begin
         if Height < 521 then
            Height := 521;
         if Width  <  587 then
            Width  := 587;
      end;
   end;
end;

procedure TFormSetup.PBottomHalfDAResize(Sender: TObject);
begin
   with BTNDeleteAction do begin
      Left := PBottomHalfDA.Width - Width - 10;
   end;
end;

procedure TFormSetup.PBtnsResize(Sender: TObject);
begin
   BtnOK.Left := PBtns.Width - 300;
   BtnCancel.Left := PBtns.Width - 200;
   BtnHelp.Left := PBtns.Width - 100;
end;

procedure TFormSetup.SGActionsDblClick(Sender: TObject);
begin
   BtnEditActionClick(Sender);
end;

function TFormSetup.CanClose(Cancel:Boolean): boolean;
begin
(* *)
   result := TRUE;
   if not Cancel and (TempManagerVars.Dirty or InternalActionList.Dirty) then begin
      With TempManagerVars do begin
         if Length(EBManagerHeading.Text) = 0 then begin
            MessageDlg(LoadStr(RES_ERR_DSetup_Heading_Missing_Manager)
                       , mtError, [mbOK], 0);
            result := FALSE;
            EBManagerHeading.SetFocus;
            exit;
         end else if IsNameSafe(EBManagerHeading.Text,'action',false) then begin
            SaveInLibrary('action',InternalActionList.CreateAttributeList,EBManagerHeading.Text,MainFormVars.SimSpecLib);
            MainFormVars.UpdateLib(4,0);
         end else begin
            MessageDlg(LoadStr(RES_ERR_DSetup_Heading_Used_Manager)
                       , mtError, [mbOK], 0);
            result := FALSE;
            EBManagerHeading.SetFocus;
            exit;
         end;
      end;
   end;
end;

procedure TFormSetup.BtnCancelClick(Sender: TObject);
begin
(* *)
   if CanClose(TRUE) then
      ModalResult := mrCancel
   else ;
end;

procedure TFormSetup.Close1Click(Sender: TObject);
begin
   (* *)
   if CanClose(TRUE) then
      ModalResult := mrCancel
   else ;
end;

procedure TFormSetup.BtnOKClick(Sender: TObject);
var
   i: Integer;
   s: String;
   EditMode: Boolean;
begin
(*
   if CanClose(FALSE) then
      ModalResult := mrOK
   else ;
*)
   EditMode := not EBManagerHeading.Enabled;
   if EBManagerHeading.Text <> '' then begin
      { Check that no other Manager exists with the same name (heading) }
      i := UsedInLib(EBManagerHeading.Text , 'action');
      if i = 0 then begin
         SaveInLibrary('action', InternalActionList.CreateAttributeList
                        ,EBManagerHeading.Text, MainFormVars.SimSpecLib);
         // { Commit the changes in simulation specific library }
         // SaveDaisyLibrary(MainFormVars.SimSpecLib,MainFormVars.SimSpecLib,nil);
         MainFormVars.UpdateLib(4, 0);
         ModalResult := mrOK;
      end
      else if i = 5 then begin
         if not EditMode then
            if MessageDlg(LoadStr(RES_ERR_DSetup_Man_Exist_in_SimSpec1)
                           + ' ' + EBManagerHeading.Text
                           + ' ' + LoadStr(RES_ERR_DSetup_Man_Exist_in_SimSpec2)
                           + CHR(10) + CHR(13) + CHR(13)
                           + LoadStr(RES_MSG_Overwrite),
                           mtWarning, [mbYes, mbNo], 0) = mrYes then
               EditMode := TRUE;
         if EditMode then begin
            DeleteLibraryObject('action', EBManagerHeading.Text); { better safe than sorry ;-) }
            SaveInLibrary('action', InternalActionList.CreateAttributeList
                           ,EBManagerHeading.Text, MainFormVars.SimSpecLib);
            // { Commit the changes in simulation specific library }
            // SaveDaisyLibrary(MainFormVars.SimSpecLib,MainFormVars.SimSpecLib,nil);
            MainFormVars.UpdateLib(4, 0);
            ModalResult := mrOK;
         end
         else begin
            ModalResult := mrNone; // Should not be neccesary :-(
            exit; { quit saving! }
         end;
      end
      else begin { i <> 0 and i <> 5 }
         case i of
            1: s := LoadStr(RES_MSC_StdRef);
            2: s := LoadStr(RES_MSC_UsrRef);
            3: s := LoadStr(RES_MSC_StdTempl);
            4: s := LoadStr(RES_MSC_UsrTempl);
         else
            s := 'Unknown BUG Library';
         end;
         MessageDlg(LoadStr(RES_ERR_DSetup_Man_Exist_in_Lib1)
                     + EBManagerHeading.Text
                     + LoadStr(RES_MSG_Exist_in_Lib2) + ' ' + s + ' '
                     + LoadStr(RES_MSG_Exist_in_Lib3) + CHR(10) + CHR(13)
                     + LoadStr(RES_ERR_DSetup_Man_Exist_in_Lib4)
                     , mtError, [mbOK], 0);
         ModalResult := mrNone; // Should not be neccesary :-(
         exit; { quit saving! }
      end;
   end
   else begin
      { Prompt for heading }
      MessageDlgPos(LoadStr(RES_INF_Valid_Heading_Before_OK)
                  , mtInformation, [mbOK], 0
                  , FormSetup.Left - 20
                  , FormSetup.Top + 50);
      EBManagerHeading.SetFocus;
      ModalResult := mrNone; // Should not be neccesary :-(
   end;
end;

procedure TFormSetup.LoadManager1Click(Sender: TObject);
var
   TempList,Swap: TInternActionList;
   TheName : String;
   DoReset : Boolean;
begin
   DoReset := False;

   if TempManagerVars.Dirty then
      begin
         if MessageDlgPos(LoadStr(RES_WRN_DSetup_Load_Lost1)
                           + CHR(10) + CHR(13)
                           + LoadStr(RES_WRN_DSetup_Load_Lost2)
                           , mtWarning, [mbOK, mbCancel], 0
                           , FormSetup.Left - 20
                           , FormSetup.Top + 50) = mrOK then
            DoReset := True;
      end
   else
      DoReset := True;

   if DoReset then
      begin
      with FormSelFromLib do begin
         Init(0, 'Manager');
         Left := FormSetup.Left - 20;
         Top := FormSetup.Top + 50;
         ShowModal;
         if ModalResult = mrOK then begin { Something IS selected in the list! }
            TempManagerVars.Dirty := True;
            TheName:= LBSelFromLib.Items[LBSelFromLib.ItemIndex];
            TempList := TInternActionList.Create;
            if CreateActionListFromAlist(GetLibraryObject('action',TheName), TempList as TList) then begin
               swap := InternalActionList;
               InternalActionList := TempList;
               TempList := swap;
            end else begin
               MessageDlg(LoadStr(RES_ERR_Load_From_Lib), mtError, [mbOK], 0);
            end;
            TempList.Free;
            Populate;

            { The form is no longer dirty. }
            TempManagerVars.Dirty := False;
         end;
      end;
   end;

end;

procedure TFormSetup.Exporttofile1Click(Sender: TObject);
begin
   if SaveDlgProject.Execute then begin
      InternalActionList.Export_it(SaveDlgProject.Filename);
   end;
end;

procedure TFormSetup.Importfromfile1Click(Sender: TObject);
var
   TempList,Swap: TInternActionList;
begin
   if OpenDlgProject.ExeCute then begin
      TempList := TInternActionList.Create;
      if TempList.Import_it(OpenDlgProject.FileName) then begin
         TempManagerVars.Dirty := True;
         Swap := InternalActionList;
         InternalActionList := TempList;
         TempList := Swap;
      end;
      TempList.Free;
      Populate;
   end;
end;

procedure TFormSetup.EBManagerHeadingChange(Sender: TObject);
begin
   TempManagerVars.Dirty := True;
end;

procedure TFormSetup.SaveManager1Click(Sender: TObject);
var
   {TheName, LibName} SaveToLib, SelLibStr, s : string;
   i: Integer;
   OkToSave: Boolean;
begin
(*
   with OKBottomDlg1 do begin
      Heading := LibraryHeading;
      if ShowModal = mrOK then begin
         TheName := Heading; LibName := 'action';
         if IsNameSafe(TheName,LibName,True) then begin
            if FromLibrary(TheName, LibName, MainFormVars.UserTemplLib) then begin
               if MessageDlg('Manager name : ''+ TheName+ '' allready exists' + CHR(10) + CHR(13)
                         + 'Select <Yes> to overwrite, ' + CHR(10) + CHR(13)
                         + 'Select <No> to cancel saving',
                         mtWarning, [mbYes,mbNo], 0) = mrNO then
                  exit
               else begin
                  DeleteLibraryObject(LibName, TheName)
               end;
            end;
            SaveInLibrary('action',InternalActionList.CreateAttributeList,TheName,MainFormVars.UserTemplLib);
            SaveDaisyLibrary(MainFormVars.UserTemplLib,MainFormVars.UserTemplLib,nil);
            MainFormVars.UpdateLib(3,0);
            LibraryHeading := TheName;
         end else begin
            MessageDlg('Can not save using  manager name : '+ TheName
                        + ' because it allready exists, and cannot be overwritten'
                        + CHR(10) + CHR(13),
                         mtWarning, [mbOK], 0);
         end;
      end;
   end;
*)

   { Test if previously saved... }
   case TempManagerVars.CurrentSaveLib of
      1: begin
            SelLibStr := MainFormVars.UserRefLib;    { User Ref }
            SaveToLib := LoadStr(RES_MSC_UsrRef);
         end;
      3: begin
            SelLibStr := MainFormVars.UserTemplLib;  { User Templ }
            SaveToLib := LoadStr(RES_MSC_UsrTempl);
         end;
   else
      TempManagerVars.CurrentSaveLib := -1;  // Just to be safe...
   end;

   if TempManagerVars.CurrentSaveLib < 0 then  // Not saved yet...
      SaveManagerAs1Click(Sender)
   else begin
      // First test if there is given a valid heading
      EBManagerHeading.Text := Trim(EBManagerHeading.Text);
      if EBManagerHeading.Text = '' then begin
         MessageDlg('A valid heading must be provided before saving the manager.'
                    , mtError, [mbOk], 0);
         exit;
      end;

      OkToSave := False;
      i := UsedInLib(EBManagerHeading.Text, 'action');
      if i = 0 then begin
         OkToSave := True;
(*
         SaveInLibrary('action', InternalActionList.CreateAttributeList,
                        EBManagerHeading.Text, MainFormVars.UserTemplLib);
         { Commit the changes in user templ library }
         SaveDaisyLibrary(MainFormVars.UserTemplLib,MainFormVars.UserTemplLib,nil);
         MainFormVars.UpdateLib(3,0);
         LibraryHeading := EBManagerHeading.Text;
*)
      end
      else if i = 2 then begin
         if MessageDlg('Manager '
                        + EBManagerHeading.Text
                        + ' allready exists in the User Reference Library.'
                        + CHR(10) + CHR(13) + CHR(13)
                        + 'Continue saving to ' + SaveToLib + ' and overwrite?'
                        , mtWarning, [mbYes, mbNo], 0) = mrYes then
            OkToSave := True;
      end
      else if i = 4 then begin
         if MessageDlg(LoadStr(RES_ERR_DSetup_Man_Exist_in_UsrTempl1) + ' '
                        + EBManagerHeading.Text + ' '
                        + LoadStr(RES_ERR_DSetup_Man_Exist_in_UsrTempl2)
                        + CHR(10) + CHR(13) + CHR(13)
                        + 'Continue saving to ' + SaveToLib + ' and overwrite?'
                        , mtWarning, [mbYes, mbNo], 0) = mrYes then
            OkToSave := True;
      end
      else begin { i <> 0 and i <> 4 }
         case i of
            1: s := LoadStr(RES_MSC_StdRef);
//            2: s := LoadStr(RES_MSC_UsrRef);
            3: s := LoadStr(RES_MSC_StdTempl);
            5: s := LoadStr(RES_MSC_SimSpec);
         else
            s := 'Unknown BUG Library';
         end;
         MessageDlg(LoadStr(RES_ERR_DSetup_Man_Exist_in_Lib1)
                     + EBManagerHeading.Text
                     + LoadStr(RES_MSG_Exist_in_Lib2) + ' ' + s + ' '
                     + LoadStr(RES_MSG_Exist_in_Lib3)
                     , mtError, [mbOK], 0);
         exit; { quit saving! }
      end;

      if OkToSave then begin
         if i <> 0 then
            DeleteLibraryObject('action', EBManagerHeading.Text); { better safe than sorry ;-) }
         SaveInLibrary('action', InternalActionList.CreateAttributeList,
                        EBManagerHeading.Text, SelLibStr);
         { Commit the changes in user templ library }
         SaveDaisyLibrary(SelLibStr, SelLibStr, nil);
         MainFormVars.UpdateLib(TempManagerVars.CurrentSaveLib, 0);
         LibraryHeading := EBManagerHeading.Text;

         { The form is no longer dirty. }
         TempManagerVars.Dirty := False;
      end;
   end;
end;

procedure TFormSetup.SaveManagerAs1Click(Sender: TObject);
var
   i, SelLib: Integer;
   s, ManName, SelLibStr: String;
   OkToSave: Boolean;
begin
   { Now get the save-as name... }
   FormSaveAs.Init(4, Left+30, Top+40);
   if FormSaveAs.ShowModal = mrOK then begin
      FormSaveAs.Read(ManName, SelLib);
      if SelLib = 0 then begin
         SelLib := 3; { User Templ }
         SelLibStr := MainFormVars.UserTemplLib;
      end
      else begin
         SelLib := 1; { User Ref }
         SelLibStr := MainFormVars.UserRefLib;
      end;

      { check if used in some library... }
      i := UsedInLib(ManName, 'action'); { check if name is already used }
      OkToSave := FALSE;
      case i of
         0: OkToSave := TRUE;  // Don't exist in any lib
         1: s := LoadStr(RES_MSC_StdRef);
//         2: s := LoadStr(RES_MSC_UsrRef);
         2: OkToSave := TRUE;
         3: s := LoadStr(RES_MSC_StdTempl);
         4: OkToSave := TRUE;
         5: s := LoadStr(RES_MSC_SimSpec);
      else
         s := 'Unknown BUG Library';
      end;
      if (not OkToSave) then begin
         MessageDlg(LoadStr(RES_ERR_DSetup_Man_Exist_in_SimSpec1)
                     + ' ' + ManName + ' '
                     + LoadStr(RES_MSG_Exist_in_Lib2)
                     + ' ' + s + ' '
                    + LoadStr(RES_MSG_Exist_in_Lib3)
                    , mtError, [mbOK], 0);
         exit; { quit saving! }
      end;

      { Possible overwrite of existing object... }
      if i = 2 then begin
         if MessageDlg(LoadStr(RES_ERR_DSetup_Man_Exist_in_Lib1)
                        + ' ' + ManName + ' '
                        + LoadStr(RES_MSG_Exist_in_Lib2) + ' '
                        + LoadStr(RES_MSC_UsrRef) + '.'
                        + CHR(10) + CHR(13) + CHR(13)
                        + LoadStr(RES_MSG_Overwrite),
                        mtWarning, [mbYes, mbNo], 0) <> mrYes then
            exit;  { quit saving }
      end;
      if i = 4 then begin
         if MessageDlg(LoadStr(RES_ERR_DSetup_Man_Exist_in_Lib1)
                        + ' ' + ManName + ' '
                        + LoadStr(RES_MSG_Exist_in_Lib2) + ' '
                        + LoadStr(RES_MSC_UsrTempl) + '.'
                        + CHR(10) + CHR(13) + CHR(13)
                        + LoadStr(RES_MSG_Overwrite),
                        mtWarning, [mbYes, mbNo], 0) <> mrYes then
            exit;  { quit saving }
      end;

      { So far so good - Now do the saving... }
      if i <> 0 then
         DeleteLibraryObject('action', ManName); { better safe than sorry ;-) }
      SaveInLibrary('action', InternalActionList.CreateAttributeList,
                     ManName, SelLibStr);
      { Commit the changes in user templ library }
      SaveDaisyLibrary(SelLibStr, SelLibStr, nil);
      MainFormVars.UpdateLib(SelLib, 0);
      TempManagerVars.CurrentSaveLib := SelLib;
   end;
end;

procedure TFormSetup.ClearManager1Click(Sender: TObject);
var
   DoReset : Boolean;
begin
   DoReset := False;

   if TempManagerVars.Dirty then
      begin
         if MessageDlgPos(LoadStr(RES_WRN_DSetup_Clear_Lost1)
                           + CHR(10) + CHR(13)
                           + LoadStr(RES_WRN_DSetup_Clear_Lost2)
                           , mtWarning, [mbOK, mbCancel], 0
                           , FormSetup.Left - 20
                           , FormSetup.Top + 50) = mrOK then
            DoReset := True;
      end
   else
      DoReset := True;

   if DoReset then
      begin
         if EBManagerHeading.Enabled then
            EBManagerHeading.Text := '';
         InternalActionList.NewList;
         Populate;

         { The form is no longer dirty. }
         TempManagerVars.Dirty := False;
      end;
end;

procedure TFormSetup.SGActionsSelectCell(Sender: TObject; Col,
  Row: Longint; var CanSelect: Boolean);
begin
   UpdateButtons;
end;

procedure TFormSetup.About1Click(Sender: TObject);
begin
   FormAbout.ShowModal;
end;

procedure TFormSetup.Contents1Click(Sender: TObject);
begin
   Application.HelpCommand(HELP_TAB, 0);
end;

procedure TFormSetup.SearchforHelpOn1Click(Sender: TObject);
begin
   Application.HelpCommand(HELP_PARTIALKEY, HelpEmptyKey);
end;

procedure TFormSetup.HowtoUseHelp1Click(Sender: TObject);
begin
   Application.HelpCommand(HELP_HELPONHELP, 0);
end;

procedure TFormSetup.BtnHelpClick(Sender: TObject);
begin
   Application.HelpContext(HLP_Management);
end;

procedure TFormSetup.SGActionsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
   { If the <Delete> button is enabled, and the [Delete] key is pressed, then
     simulate that the <Delete> button was clicked. }
   If BtnDeleteAction.Enabled and (Key = VK_Delete) then
      BtnDeleteActionClick(Sender);
end;

end.

(*
procedure TFormSetup.WantToClose;
begin
   if InternalActionList.IsDirty then
      case Application.MessageBox(PChar(CurrProject + ' has changed do you want to save the changes'),'Exit',MB_YesNoCancel) of
      IDYES:
         begin
            Save1Click(self);
            FormSetup.Hide;
         end;
      IDNO:
         begin
            FormSetup.Hide;
         end;
      IDCANCEL:
         begin
         end;
      end
   else
         FormSetup.Hide;
end;
*)
(*
procedure TFormSetup.Exit1Click(Sender: TObject);
begin
     BtnOKClick(Sender);
end;
*)
(*
procedure TFormSetup.Open1Click(Sender: TObject);
var DoOpen : Boolean;
    TempProject : String;
    TempInternalActionList,Swap:TInternActionList;
begin
   DoOpen := True;
   if InternalActionList.IsDirty then begin
      case Application.MessageBox('Save Changes','Warning',MB_YesNoCancel) of
      IDYES:
         begin
            Save1Click(self);
         end;
      IDNO:
         begin
         end;
      IDCANCEL:
         begin
            DoOpen := False;
         end;
      end;
   end;
   if DoOpen then begin
      if OpenDlgProject.Execute then begin
         TempProject := OpenDlgProject.Filename;
         TempInternalActionList := TInternActionList.Create;
         if not TempInternalActionList.LoadFromFile(TempProject) then begin
            Application.MessageBox(PChar('Error: This is not a valid projectfile'),PChar('Error'),mb_OKCancel);
         end else begin
            Swap := InternalActionList;
            InternalActionList := TempInternalActionList;
            TempInternalActionList := Swap;
            CurrProject := TempProject;
         end;
         TempInternalActionList.Free;
         Caption := CurrProject;
         Populate;
         Update;
      end;
   end;
end;
*)
(*
procedure TFormSetup.SaveAs1Click(Sender: TObject);
begin
   if SaveDlgProject.Execute then begin
      CurrProject := SaveDlgProject.Filename;
      InternalActionList.SaveToFile2(CurrProject);
      Caption := GetString(201) + CurrProject;
   end;
end;
*)
(*
procedure TFormSetup.Save1Click(Sender: TObject);
begin
   if CurrProject = 'Project1.dsy' then begin
      if SaveDlgProject.Execute then begin
         CurrProject := SaveDlgProject.Filename;
         InternalActionList.SaveToFile(CurrProject);
      end;
   end else
      InternalActionList.SaveToFile(CurrProject);
   Caption := GetString(201) + CurrProject;
end;
*)
(*
procedure TFormSetup.New1Click(Sender: TObject);
begin
   if InternalActionList.IsDirty then begin
      case Application.MessageBox('Save Changes','Warning',MB_YesNoCancel) of
      IDYES:
         begin
            Save1Click(self);
            InternalActionList.NewList;
            CurrProject := 'Project1.dsy';
            Caption := GetString(201)+CurrProject;
            Populate;
         end;
      IDNO:
         begin
            InternalActionList.NewList;
            CurrProject := 'Project1.dsy';
            Caption := GetString(201)+CurrProject;
            Populate;
         end;
      IDCANCEL:
      end;
   end else begin
      InternalActionList.NewList;
      CurrProject := 'Project1.dsy';
      Caption := GetString(201)+CurrProject;
      Populate;
   end;
end;
*)
(*
procedure TFormSetup.SaveNew1Click(Sender: TObject);
begin
   if SaveDlgProject.Execute then begin
         InternalActionList.SaveToFile3(SaveDlgProject.Filename);
   end;
end;
*)

