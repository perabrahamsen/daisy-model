unit SADlg;
{
***************
* Unit SADlg *
***************

Project    : Graphical User Interface for the Daisy simulation package

Purpose    : Prototype Select Action Dialog Window

Author     : J&R Informatik I/S (Rino Ranheim)

Date(s)    : 1/12 - 96 Created
}

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, Globals, TypInfo;

type
  TDlgSelectAction = class(TForm)
    BevelSelectAction: TBevel;
    BtnOK: TBitBtn;
    BtnCancel: TBitBtn;
    BtnHelp: TBitBtn;
    ListSelectAction: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure SetListText(t: TStringList);
    procedure BtnOKClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    ActionType : Integer;
  end;


var
  DlgSelectAction: TDlgSelectAction;

implementation
uses
   ManGlob, IntAct,dsetup;
{$R *.DFM}

procedure TDlgSelectAction.SetListText(t: TStringList);
begin
   DlgSelectAction.ListSelectAction.Items := t;
   DlgSelectAction.ListSelectAction.Update;
end;

procedure TDlgSelectAction.FormCreate(Sender: TObject);
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
   // Default we have no caption. The caption is defined dynamically, according
   // to the current action.
   Caption := '';

   { Button captions }
   BtnOK.Caption     := LoadStr(RES_BTN_Add);
   BtnCancel.Caption := LoadStr(RES_BTN_Cancel);
   BtnHelp.Caption   := LoadStr(RES_BTN_Help);
end;

procedure TDlgSelectAction.BtnOKClick(Sender: TObject);
var TilA    : TTillage;
    SowA    : TSowing;
    FertA   : TFertilize;
    HarvA   : THarvest;
    i       : Integer;
    numStr  : String;
    tempStr : String;
begin
   case ActionType of
      0: {Tillage}
         begin
            if ListSelectAction.ItemIndex >= 0 then
            begin
               TilA := TTillage.Create;
               TilA.how := ListSelectAction.Items.Strings[ListSelectAction.ItemIndex];
               FormSetup.InsertNewAction(TilA, InsertBefore);
               { Manager form is now dirty. }
               TempManagerVars.Dirty := True;
            end;
         end;
      1: {Sowing}
         begin
            if ListSelectAction.ItemIndex >= 0 then
            begin
               SowA := TSowing.Create;
               SowA.what := ListSelectAction.Items.Strings[ListSelectAction.ItemIndex];
               FormSetup.InsertNewAction(SowA, InsertBefore);
               { Manager form is now dirty. }
               TempManagerVars.Dirty := True;
            end;
         end;
      2: {Fertilize}
         begin
            if ListSelectAction.ItemIndex >= 0 then
            begin
               FertA := TFertilize.Create;
               FertA.withWhat := ListSelectAction.Items.Strings[ListSelectAction.ItemIndex];
               FormSetup.InsertNewAction(FertA, InsertBefore);
               { Manager form is now dirty. }
               TempManagerVars.Dirty := True;
            end;
         end;
      3: {Harvest}
         begin
            if ListSelectAction.ItemIndex >= 0 then
            begin
               HarvA := THarvest.Create;
               tempStr := ListSelectAction.Items.Strings[ListSelectAction.ItemIndex];
               i := 1; numStr := '';
               while (i <= Length(tempStr)) and (tempStr[i] in ['0'..'9']) do begin
                  numStr := numStr + tempStr[i];
                  inc(i);
               end;
               HarvA.CropSowed := InternalActionList.Items[StrToInt(numStr)-1];
               {HarvA.CropSowed.Harvest := HarvA;}
               FormSetup.InsertNewAction(HarvA, InsertBefore);
               ListSelectAction.Items.Delete(ListSelectAction.ItemIndex);
               { Manager form is now dirty. }
               TempManagerVars.Dirty := True;
            end;
         end;
   end;
end;

procedure TDlgSelectAction.FormActivate(Sender: TObject);
begin
   case ActionType of
      0: {Tillage}
         Caption := LoadStr(RES_FRM_SADlg_Tillage);
      1: {Sowing}
         Caption := LoadStr(RES_FRM_SADlg_Sowing);
      2: {Fertilize}
         Caption := LoadStr(RES_FRM_SADlg_Fertilize);
      3: {Harvest}
         Caption := LoadStr(RES_FRM_SADlg_Harvest);
   end;
end;

end.
