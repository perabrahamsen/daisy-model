unit profil;                     

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, StdCtrls, Grids, ExtCtrls, Buttons, StatInit, Horizon, Discret,
  Spin, OneEdit, Comments, Mask, Globals, About, LibComp, UseLib, TypInfo;

type
  TFormProfile = class(TForm)
    MMenuProfile: TMainMenu;
    File1: TMenuItem;
    NewProfile1: TMenuItem;
    OpenProfile1: TMenuItem;
    SaveProfile1: TMenuItem;
    LblHeading: TLabel;
    EditHeading: TEdit;
    GBCompose: TGroupBox;
    RGSelect: TRadioGroup;
    SGHorizons: TStringGrid;
    SGProfile: TStringGrid;
    BtnAdd: TButton;
    BtnDeleteProfile: TButton;
    BtnNewHorizon: TButton;
    BtnEditHorizon: TButton;
    BtnDeleteHorizon: TButton;
    LblRootingDepth: TLabel;
    LblWaterFlow: TLabel;
    ComboWaterFlow: TComboBox;
    BtnWaterFlow: TButton;
    LblDiscretization: TLabel;
    BtnStateInit: TButton;
    BtnOK: TBitBtn;
    BtnCancel: TBitBtn;
    BtnHelp: TBitBtn;
    BtnDiscretization: TButton;
    N1: TMenuItem;
    Close1: TMenuItem;
    LblRootingDepthUnits: TLabel;
    EditRootingDepth: TEdit;
    SBProfileUp: TSpeedButton;
    SBProfileDown: TSpeedButton;
    BtnHeadingComments2: TBitBtn;
    BtnHeadingComments1: TButton;
    LblDispersibilityUnits: TLabel;
    EditDispersibility: TEdit;
    LblDispersibility: TLabel;
    OpenDlgProfile: TOpenDialog;
    Help1: TMenuItem;
    About1: TMenuItem;
    HowtoUseHelp1: TMenuItem;
    SearchforHelpOn1: TMenuItem;
    Contents1: TMenuItem;
    N2: TMenuItem;
    LblAbsorption: TLabel;
    ComboAbsorption: TComboBox;
    BtnAbsorption: TButton;
    SaveProfileAs1: TMenuItem;
    procedure BtnStateInitClick(Sender: TObject);
    procedure BtnEditHorizonClick(Sender: TObject);
    procedure OpenProfile1Click(Sender: TObject);
    procedure BtnDiscretizationClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ComboWaterFlowChange(Sender: TObject);
    procedure BtnWaterFlowClick(Sender: TObject);
    procedure BtnHeadingCommentsClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnAddClick(Sender: TObject);
    procedure RGSelectClick(Sender: TObject);
    procedure BtnDeleteProfileClick(Sender: TObject);
    procedure SGProfileClick(Sender: TObject);
    procedure SBProfileUpClick(Sender: TObject);
    procedure SBProfileDownClick(Sender: TObject);
    procedure GBComposeExit(Sender: TObject);
    procedure EditHeadingExit(Sender: TObject);
    procedure BtnNewHorizonClick(Sender: TObject);
    procedure BtnDeleteHorizonClick(Sender: TObject);
    procedure EditRootingDepthExit(Sender: TObject);
    procedure SGProfileSelectCell(Sender: TObject; Col, Row: Longint;
      var CanSelect: Boolean);
    procedure Close1Click(Sender: TObject);
    procedure SGHorizonsKeyPress(Sender: TObject; var Key: Char);
    procedure BtnOKClick(Sender: TObject);
    procedure SaveProfile1Click(Sender: TObject);
    procedure NewProfile1Click(Sender: TObject);
    procedure EditDispersibilityExit(Sender: TObject);
    procedure FormShow(Sender: TObject);

    procedure SetSGHorizonsFromVars;
    procedure SetSGProfileFromVars;
    procedure SetFormFromVars;
    procedure SetVarsFromForm;
    procedure BtnProfileHeadingCommentUpdate;
    procedure BtnWaterFlowUpdate; { Show or hide the parameters button for water flow }
    procedure BtnStateInitUpdate; { State init only allowed if 'Lower Boundary' is set }
    procedure BtnAbsorptionUpdate;
    procedure BtnDiscretizationUpdate;
    procedure BtnProfileUpdate; { Makes the buttons under SGProfile enabled or disabled }
    procedure BtnHorizonUpdate; { Shows or hides the three buttons under SGHorizon}
    procedure BtnsProfileUpdate;
    procedure About1Click(Sender: TObject);
    procedure ComboAbsorptionChange(Sender: TObject);
    procedure BtnAbsorptionClick(Sender: TObject);
    procedure Contents1Click(Sender: TObject);
    procedure SearchforHelpOn1Click(Sender: TObject);
    procedure HowtoUseHelp1Click(Sender: TObject);
    procedure BtnHelpClick(Sender: TObject);
    function CheckValidFields(msg: Boolean): Boolean;
    procedure EditHeadingChange(Sender: TObject);
    procedure EditRootingDepthChange(Sender: TObject);
    procedure EditDispersibilityChange(Sender: TObject);
    procedure SaveProfileAs1Click(Sender: TObject);
    procedure SGProfileKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SGHorizonsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);

  private
    { Private declarations }
  public
    { Public declarations }
  end;

  function GetDeepestGroundwater(fgf: string; var dd, mm, yy: Integer; var d: Double): Boolean;

var
   FormProfile: TFormProfile;


implementation

uses DHeading, SaveAs;

{$R *.DFM}


{ Check all input fields on the FORM for valid values             }
{ Return FALSE when first invalid found (and focus it)            }
{ msg=TRUE => show errormessages                                  }
function TFormProfile.CheckValidFields(msg: Boolean): Boolean;
var
   x, y, i: Integer;
   lowest, tempd: double;
   s: String;
   Missing: Boolean;
begin
   Result := FALSE; { Fear the worst... }

   { Fields that are allways ok, are commented out... }

//      EditHeading1;

{ SGProfile - Start check here }
   with SGProfile do begin
      if TempProfileVars.NumSGProfileItems > 0 then begin { Updates on Add/Remove!! }
         Missing := FALSE;
         s := '';
         for i:=1 to RowCount-1 do { First find everything wrong... }
            try
               Cells[1,i] := StrTo1DecPosNotZeroStr(Cells[1,i]);
            except
               on E: Exception do begin
                  Missing := TRUE;
                  if s = '' then
                     s := s + IntToStr(i)
                  else
                     s := s + ', ' + IntToStr(i);
   {               break;} { don't break, but check/convert all rows. }
               end;
            end;
         if Missing then begin
            if msg then begin
               x := Left + GBCompose.Left + FormProfile.Left - 240;
               y := Top + GBCompose.Top + FormProfile.Top + 10;
               MessageDlgPos(GBCompose.Caption + ':' + Chr(10) + Chr(13)
                              + 'All horizons must have a valid size.' + Chr(10) + Chr(13)
                              + 'Lines ' + s + ' in the profile has invalid size definition.'
                              + Chr(10) + Chr(13)
                              + 'Use only integer of floating point numbers > 0 as sizes.',
                              mtInformation, [mbOK], 0, x, y);
               end;
            SetFocus;
            exit;
         end;
      end;  { if TempProfileVars.NumSGProfileItems > 0 }
   end;
{ SGProfile - End check here }

{ EditRootingDepth - Start check here }
   with EditRootingDepth do begin
      Text := Trim(Text); { Get rid of leading/trailing spaces}
      if Text <> '' then begin { '' is a valid value! }
         x := Left + FormProfile.Left - 150;
         y := Top + FormProfile.Top - 85;

         try Text := StrTo1DecPosStr(Text);
         except
            on E: Exception do begin
               if msg then
                  MessageDlgPos(LblRootingDepth.Caption + ':' + CHR(10) + CHR(13)
                                + 'Not a valid positive floating point number!',
                                 mtInformation, [mbOK], 0, x, y);
               SetFocus;
               exit;
            end;
         end;

         { OK numberformat, now check if number is below lowest horizon }
         lowest := TempProfileVars.GetLowestHor;
         tempd := StrToFloat(Text);
         if (lowest < 0) then begin { no horizons in profile }
            if (tempd <> 0) then begin
               if msg then
                  MessageDlgPos(LblRootingDepth.Caption + ':' + CHR(10) + CHR(13)
                              + 'The value can not be below the deepest horizon.'
                              + CHR(10) + CHR(13)
                              + 'Please add horizons to the profile before setting the value',
                              mtInformation, [mbOK], 0, x, y);
               SetFocus;
               exit;
            end
            else ; { 0 is an OK value when no horizons are in profile! }
         end
         else if tempd > lowest then begin
            if msg then
               MessageDlgPos(LblRootingDepth.Caption + ':' + CHR(10) + CHR(13)
                           + 'The value for ' + LblRootingDepth.Caption
                           + ' can not be below the deepest horizon (which '
                           + 'is at ' + FormatFloat('0.0', lowest)
                           + ' cm)' + CHR(10) + CHR(13)
                           + 'Please enter a valid value.',
                           mtInformation, [mbOK], 0, x, y);
            SetFocus;
            exit;
         end;
      end; { if Text <> '' }
   end; { with EditRootingDepth do }
{ EditRootingDepth - End check here }

{ EditDispersibility - Start check here }
   with EditDispersibility do begin
      Text := Trim(Text); { Get rid of leading/trailing spaces}
      if Text <> '' then begin { '' is a valid value! }
         try Text := StrTo1DecPosStr(Text);
         except
            on E: Exception do begin
               if msg then begin
                  x := Left + FormHorizon.Left - 200;
                  y := Top + FormHorizon.Top - 80;
                  MessageDlgPos(LblDispersibility.Caption + ':' + CHR(10) + CHR(13)
                                 + 'Not a valid floating point number!',
                                 mtInformation, [mbOK], 0, x, y);
               end;
               SetFocus;
               exit;
            end;
         end;
      end; { if Text <> '' }
   end; { with EditDispersibility do }
{ EditDispersibility - End check here }

//   ComboWaterFlow;

//   ComboAbsorption;

{ Everything Checked OK :-) }
   Result := TRUE;
end;

procedure TFormProfile.SetSGProfileFromVars;
var
   i, j: Integer;
begin
   with SGProfile do begin
      if TempProfileVars.NumSGProfileItems <= 0 then begin
         RowCount := 2;
         Rows[1].Clear;
      end
      else begin
         RowCount := TempProfileVars.NumSGProfileItems + 1;
         for i:=0 to ColCount-1 do
            for j:=1 to RowCount-1 do
               Cells[i, j] := TempProfileVars.SGProfileItems.Cells[i, j-1];
         Row := TempProfileVars.SGProfileItems.Row + 1;
      end;
   end;
   BtnProfileUpdate;
end;

procedure TFormProfile.SetSGHorizonsFromVars;
var
   LibItems: ^TStringList;
   i: Integer;
   s1, s2: String;
begin
   { Update SGHorizons }
   case RGSelect.ItemIndex of
      0: LibItems := @MainFormVars.StdRefLibHorItems;
      1: LibItems := @MainFormVars.UserRefLibHorItems;
      2: LibItems := @MainFormVars.SimSpecLibHorItems;
   else
      LibItems := nil;
   end;

   with SGHorizons do begin { in case of #@$$#¤"... }
      if LibItems = nil then begin
         RowCount := 2;
         Rows[1].Clear;
      end
      else if LibItems^.Count <= 0 then begin  { nothing in library... }
         RowCount := 2;
         Rows[1].Clear;
      end
      else begin  { Set stringgrid from var... }
         RowCount := LibItems^.Count + 1;
         for i := 1 to RowCount - 1 do begin
            if (LibItems^)[i-1] <> 'mini' then begin { don't show 'mini' horizon!!! }
               if SplitHorizonName(s1, s2, (LibItems^)[i-1]) = 0 then begin
                  Cells[0, i] := s1;
                  Cells[1, i] := s2;
               end;
            end;
         end;
      end;
      { Now set heading... }
      Cells[0,0] := '';
      Cells[1,0] := 'Soil Horizon';
   end;

   { Update Horizon Buttons }
   BtnHorizonUpdate;
end;

procedure TFormProfile.SetFormFromVars;
var
   i, j: Integer;
begin
   { Set the visible components... }
   Caption := 'Soil Profile';
      { Init edit boxes }
   EditHeading.Text := TempProfileVars.ProfileHeading;
   if TempProfileVars.RootingDepth = NotSet then EditRootingDepth.Text := ''
   else EditRootingDepth.Text := FloatToStr(TempProfileVars.RootingDepth);
   if TempProfileVars.Dispersivity = NotSet then EditDispersibility.Text := ''
   else EditDispersibility.Text := FloatToStr(TempProfileVars.Dispersivity);
      { Init RadioGroups }
   RGSelect.ItemIndex := TempProfileVars.RGSelectIdx;
      { Init combos }
(*   ComboWaterFlow.Items.Clear;
   ComboWaterFlow.Items.AddStrings(TempProfileVars.WaterFlowItems);*)
   ComboWaterFlow.ItemIndex := TempProfileVars.WaterFlowIdx;
   ComboAbsorption.ItemIndex := TempProfileVars.AbsorptionIsotermIdx;
      { Init String Grids... }
   SetSGHorizonsFromVars;  { RGSelect must be set first! - and it is 6 lines earlier }

   with SGProfile do begin
      if TempProfileVars.NumSGProfileItems < 1 then RowCount := 2
      else RowCount := TempProfileVars.NumSGProfileItems + 1;
      Cells[0,0] := ' Soil Profile';
      Cells[1,0] := 'Size (cm)';
      if TempProfileVars.NumSGProfileItems > 0 then
         for i := 1 to RowCount - 1 do
            for j := 0 to ColCount - 1 do
               Cells[j, i] := TempProfileVars.SGProfileItems.Cells[j, i-1]
      else
         Rows[1].Clear;
   end;

   { ...and update the states of the buttons... }
   BtnsProfileUpdate;
end;

procedure TFormProfile.SetVarsFromForm;
var
   i, j: Integer;
begin
      { Read edit boxes }
   TempProfileVars.ProfileHeading := Trim(EditHeading.Text);
   if Trim(EditRootingDepth.Text) = '' then TempProfileVars.RootingDepth := NotSet
   else TempProfileVars.RootingDepth := StrToFloat(EditRootingDepth.Text);
   if Trim(EditDispersibility.Text) = '' then TempProfileVars.Dispersivity := NotSet
   else TempProfileVars.Dispersivity := StrToFloat(EditDispersibility.Text);
      { Read RadioGroups }
   TempProfileVars.RGSelectIdx := RGSelect.ItemIndex;
(*      { Read combos }
   TempProfileVars.WaterFlowItems.Clear;
   TempProfileVars.WaterFlowItems.AddStrings(ComboWaterFlow.Items);
*)
   TempProfileVars.WaterFlowIdx := ComboWaterFlow.ItemIndex;
   TempProfileVars.AbsorptionIsotermIdx := ComboAbsorption.ItemIndex;

      { Read String Grids... }
(*
   with SGHorizons do begin
      if (RowCount < 3) and (Trim(Cells[1, 1]) = '') then begin
         TempProfileVars.NumSGHorizonsItems := 0;
         TempProfileVars.SGHorizonsItems.RowCount := 0; { means := 1 ##@$# }
      end
      else begin
         TempProfileVars.NumSGHorizonsItems := RowCount - 1;
         TempProfileVars.SGHorizonsItems.RowCount
               := TempProfileVars.NumSGHorizonsItems;
      end;
      TempProfileVars.SGHorizonsItems.ColCount := ColCount;
      if TempProfileVars.NumSGHorizonsItems > 0 then
         for i := 1 to RowCount - 1 do
            for j := 0 to ColCount - 1 do
               TempProfileVars.SGHorizonsItems.Cells[j, i-1] := Cells[j, i]
   end;
*)
   with SGProfile do begin
      if (RowCount < 3) and (Trim(Cells[0, 1]) = '') then begin
         TempProfileVars.NumSGProfileItems := 0;
         TempProfileVars.SGProfileItems.RowCount := 0;  { means := 1 ##@$# }
      end
      else begin
         TempProfileVars.NumSGProfileItems := RowCount - 1;
         TempProfileVars.SGProfileItems.RowCount
               := TempProfileVars.NumSGProfileItems;
      end;
      TempProfileVars.SGProfileItems.ColCount := ColCount;
      if TempProfileVars.NumSGProfileItems > 0 then
         for i := 1 to RowCount - 1 do
            for j := 0 to ColCount - 1 do
               TempProfileVars.SGProfileItems.Cells[j, i-1] := Cells[j, i];
   end;
end;


procedure TFormProfile.BtnProfileHeadingCommentUpdate;
begin
   if TempProfileVars.ProfileComments = '' then begin  { Don't show finger!! }
      BtnHeadingComments2.Hide;
      BtnHeadingComments1.Show;
   end
   else begin
      BtnHeadingComments1.Hide;
      BtnHeadingComments2.Show;
   end;
end;

procedure TFormProfile.BtnWaterFlowUpdate; { Show or hide the parameters button for water flow }
begin
   {  Grovimetric Flow => No params
      Fixed Groundwater table => 'Depth of Groundwater' table
      Fluctuating Groundwater table => enter Filename
      Lysimeter => 'Lysimeter Depth'
      ... if params requested => compulsary
   }
   case ComboWaterFlow.ItemIndex of
      -1, 0: BtnWaterFlow.Hide;
   else
      BtnWaterFlow.Show;
   end;
end;

procedure TFormProfile.BtnAbsorptionUpdate;
begin
{  Daisy v.1 => no parameters
   Langmuir => parameters (not yet defined)
   Freundlich => parameters (not yet defined)
   Linear Freundlich => parameters (not yet defined) }
   case ComboAbsorption.ItemIndex of
      -1, 0: BtnAbsorption.Hide;
   else
      BtnAbsorption.Show;
   end;
end;

procedure TFormProfile.BtnStateInitUpdate; { State init only allowed if 'Lower Boundary' is set }
begin
   if ComboWaterFlow.ItemIndex >= 0 then
      BtnStateInit.Enabled := TRUE
   else
      BtnStateInit.Enabled := FALSE;
end;

procedure TFormProfile.BtnDiscretizationUpdate;
begin
   { No sense in talking about a discretization if no horizons are defined }
   if SGProfile.Cells[0,1] = '' then begin
      BtnDiscretization.Enabled := FALSE;
      TempProfileVars.DiscretizationViewed := TRUE;
   end
   else
      BtnDiscretization.Enabled := TRUE;
end;

procedure TFormProfile.BtnProfileUpdate; { Makes the buttons under SGProfile enabled or disabled }
begin
   if (SGProfile.RowCount = 2) and (SGProfile.Cells[0,1] = '') then
      BtnDeleteProfile.Enabled := FALSE
   else
      BtnDeleteProfile.Enabled := TRUE;

   if SGProfile.Row > 1 then
      SBProfileUp.Enabled := TRUE
   else
      SBProfileUp.Enabled := FALSE;

   if (SGProfile.Row < SGProfile.RowCount-1) and (SGProfile.Row > 0) then
      SBProfileDown.Enabled := True
   else
      SBProfileDown.Enabled := FALSE;
end;

procedure TFormProfile.BtnHorizonUpdate; { Shows or hides the three buttons under SGHorizon}
begin
   if RGSelect.ItemIndex < 2 then begin
      BtnEditHorizon.Hide;
      BtnNewHorizon.Hide;
      BtnDeleteHorizon.Hide;
   end
   else begin
      BtnEditHorizon.Show;
      BtnDeleteHorizon.Show;
      BtnNewHorizon.Show;
      if (SGHorizons.Row > 0)
            and (SGHorizons.Cells[0, SGHorizons.Row] <> '') then begin
         BtnEditHorizon.Enabled := TRUE;
         BtnDeleteHorizon.Enabled := TRUE;
      end
      else begin
         BtnEditHorizon.Enabled := FALSE;
         BtnDeleteHorizon.Enabled := FALSE;
      end;
   end;
end;

procedure TFormProfile.BtnsProfileUpdate;  { Updates all buttons on the form }
begin
   BtnProfileHeadingCommentUpdate;
   BtnWaterFlowUpdate; { Show or hide the parameters button for water flow }
   BtnAbsorptionUpdate;
   BtnStateInitUpdate; { State init only allowed if 'Lower Boundary' is set }
   BtnDiscretizationUpdate;
   BtnProfileUpdate; { Makes the buttons under SGProfile enabled or disabled }
   BtnHorizonUpdate; { Shows or hides the three buttons under SGHorizon}
end;

procedure TFormProfile.BtnStateInitClick(Sender: TObject);
var
   x, y: Integer;
begin
   x := FormProfile.Left + BtnStateInit.Left + (BtnStateInit.Width div 2);
   y := FormProfile.Top + BtnStateInit.Top - 100;
   PlaceFormStateInit(x, y);
   { Update variables... }
   Temp2StateVars.Assign(TempProfileVars.StateVars);

   FormStateInit.ShowModal;
   if FormStateInit.ModalResult = mrOK then begin
      { Update variables... }
      TempProfileVars.StateVars.Assign(Temp2StateVars);
      TempProfileVars.Dirty := TRUE;
   end;
end;

procedure TFormProfile.OpenProfile1Click(Sender: TObject);
var
   TempHeading, s, CurrHor: String;
   DoLoad: Boolean;
   i: Integer;
begin
   DoLoad := TRUE;
   if TempProfileVars.Dirty then
      if MessageDlgPos('Loading a new Profile will cause all unsaved data to be lost.'
                  + CHR(10) + CHR(13)
                  + 'Select <OK> to load a new Profile, '
                  + 'or <Cancel> to regret.',
                  mtWarning, [mbOK, mbCancel], 0
                  , FormProfile.Left - 20
                  , FormProfile.Top + 50) <> mrOK then
         DoLoad := FALSE;
   if DoLoad then begin
      with FormSelFromLib do begin
         Init(2, 'Profile');
         Left := FormProfile.Left - 20;
         Top := FormProfile.Top + 50;
         ShowModal;
         if ModalResult = mrOK then begin { Something IS selected in the list! }
            TempHeading := LBSelFromLib.Items[LBSelFromLib.ItemIndex];
            TempProfileVars.Load(TempHeading, 0); { Load vars from user templ lib }

            { Now check that all horizons in profile exists in library }
(* Done in load - as good as possible...
            s := '';
            if TempProfileVars.NumSGProfileItems > 0 then
               for i := 0 to TempProfileVars.NumSGProfileItems - 1 do begin
                  CurrHor := TempProfileVars.SGProfileItems.Cells[0, i];
                  if UsedInLib(CurrHor, 'horizon') = 0 then begin
                     if s = '' then
                        s := CurrHor
                     else
                        s := s + ', ' + CurrHor;
                  end;
               end;
            if s <> '' then begin { There were horz referred, that don't exist in libs }
               MessageDlgPos('The following horizons were referred in the profile composition, '
                  + 'but did not exist in the library:'
                  + CHR(10) + CHR(13)
                  + s
                  + CHR(10) + CHR(13)
                  + 'The profile will be reset.'
                  , mtError, [mbOK], 0
                  , FormProfile.Left - 20
                  , FormProfile.Top + 50);
               TempProfileVars.Reset;
            end;
*)
            FormProfile.SetFormFromVars; { Set visible components on form}
            FormProfile.Update; { and update just to be sure ;-) }
            TempProfileVars.Dirty := FALSE;
         end;
      end;
   end;
end;

procedure TFormProfile.SaveProfile1Click(Sender: TObject);
var
   SaveToLib, SelLibStr, ProfName: String;
   EditMode: Boolean;
begin
   { Check all fields for valid values first... }
   if not CheckValidFields(TRUE) then
      exit;

   { Update vars from form... (all checks should be done here)}
   { Update might also have been done earlier (on exit from edit-boxes etc.), but ... }
   SetVarsFromForm;

   { Test if previously saved... }
   case TempProfileVars.CurrentSaveLib of
      1: begin
            SelLibStr := MainFormVars.UserRefLib;    { User Ref }
            SaveToLib := LoadStr(RES_MSC_UsrRef);
         end;
      3: begin
            SelLibStr := MainFormVars.UserTemplLib;  { User Templ }
            SaveToLib := LoadStr(RES_MSC_UsrTempl);
         end;
   else
      TempProfileVars.CurrentSaveLib := -1;  // Just to be safe...
   end;

   if TempProfileVars.CurrentSaveLib < 0 then  // Not saved yet...
      SaveProfileAs1Click(Sender)
   else begin
      ProfName := TempProfileVars.ProfileHeading;
      if (ProfName <> '') then begin { Valid ProfileName ( more restrictions might come ) }
         EditMode := not EditHeading.Enabled;
         { Check that no other Profile exists with the same name (heading) }
         { (done in .Save) }
         if TempProfileVars.Save(ProfName, 0, EditMode) then begin
            MainFormVars.UpdateLib(TempProfileVars.CurrentSaveLib, 2);
            { Make the saving visible to the user... }
            MessageDlg('Profile ' + ProfName
                        + ' successfully saved to the ' + SaveToLib + '.',
                        mtInformation, [mbOK], 0);
            TempProfileVars.Dirty := FALSE;
         end
      end
      else begin
         { Prompt for heading }
         MessageDlgPos('A valid heading must be entered before saving.',
                        mtInformation, [mbOK], 0
                        , FormProfile.Left - 20
                        , FormProfile.Top + 50);
         EditHeading.SetFocus;
      end;
   end;
end;

procedure TFormProfile.SaveProfileAs1Click(Sender: TObject);
var
   ProfName, SelLibStr, SaveToLib: String;
   SelLib, OldSelLib: Integer;
begin
   { Check all fields for valid values first... }
   if not CheckValidFields(TRUE) then
      exit;

   { Update vars from form... (all checks should be done here)}
   { Update might also have been done earlier (on exit from edit-boxes etc.), but ... }
   SetVarsFromForm;

   { Now get the save-as name... }
   FormSaveAs.Init(3, Left+30, Top+40);
   if FormSaveAs.ShowModal = mrOK then begin
      FormSaveAs.Read(ProfName, SelLib);
      OldSelLib := TempProfileVars.CurrentSaveLib;
      if SelLib = 0 then begin
         SelLib := 3; { User Templ }
         SelLibStr := MainFormVars.UserTemplLib;
         SaveToLib := LoadStr(RES_MSC_UsrTempl);
      end
      else begin
         SelLib := 1; { User Ref }
         SelLibStr := MainFormVars.UserRefLib;
         SaveToLib := LoadStr(RES_MSC_UsrRef);
      end;

      { and then save... }
      { Check that no other Profile exists with the same name (heading) }
      { (done in .Save) }
      TempProfileVars.CurrentSaveLib := SelLib; // Is used in Save()
      if TempProfileVars.Save(ProfName, 0, FALSE) then begin
         MainFormVars.UpdateLib(SelLib, 2); { user templ prof lib }
         { Make the saving visible to the user... }
         MessageDlg('Profile ' + ProfName
                     + ' successfully saved to the ' + SaveToLib + '.',
                     mtInformation, [mbOK], 0);
      end
      else
         TempProfileVars.CurrentSaveLib := OldSelLib;
   end;
end;

procedure TFormProfile.NewProfile1Click(Sender: TObject);
var
   x, y: Integer;
   DoReset: Boolean;
begin
   DoReset := TRUE;
   if TempProfileVars.Dirty then begin
      x := FormProfile.Left - 20;
      y := FormProfile.Top + 50;
      if MessageDlgPos('The current profile definition will be deleted!'
                 + Chr(10) + Chr(13)
                 + 'Select <OK> to start new profile, or <Cancel> to regret',
                 mtWarning, [mbOK, mbCancel], 0, x, y)
                 <> mrOK then
         DoReset := FALSE;
   end;
   if DoReset then begin
      { Reset all variables for formprofile and subforms... }
      TempProfileVars.Reset;
      { ...and then update the form... }
      SetFormFromVars;
      TempProfileVars.Dirty := FALSE;
   end;
end;

procedure TFormProfile.BtnDiscretizationClick(Sender: TObject);
var
   x, y: Integer;
begin
   x := FormProfile.Left + BtnDiscretization.Left;
   y := FormProfile.Top + BtnDiscretization.Top;

   with FormDiscretization do begin
      Left := abs(x - Width); { Place with lower right corner close to button }
      Top := abs(y - Height);
      if TempProfileVars.DiscVars.Discretization.Count > 0 then begin { Just to be absolutely secure from Delphi bugs... }
         { Discretization is up to date at this point, so just update variables }
         Temp2DiscretizationVars.Assign(TempProfileVars.DiscVars);
         ShowModal;
         if ModalResult = mrOK then begin
            { Update Variables... }
            TempProfileVars.DiscVars.Assign(Temp2DiscretizationVars);
            TempProfileVars.DiscretizationViewed := TRUE;
            TempProfileVars.Dirty := TRUE;
         end;
      end;
   end;
end;

procedure TFormProfile.FormCreate(Sender: TObject);
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

   { THIS IS INCOMPLETE!!!! }
   { THIS IS INCOMPLETE!!!! }
   { THIS IS INCOMPLETE!!!! }
   { Init all labels and other text...                     }
   { To be replaced later with text from ressource file... }
   with FormProfile do begin  { Init all the strings here... }
         { Init Labels... }
      LblHeading.Caption := 'Soil Profile Heading';
      LblRootingDepth.Caption := 'Max Allowed Rooting Depth in Profile';
      LblDispersibility.Caption := 'Solute Convective Transport - Dispersivity';
      LblRootingDepthUnits.Caption := 'cm';
      LblDispersibilityUnits.Caption := 'cm';
      LblWaterFlow.Caption := 'Lower Boundary Conditions for Water Flow';
      LblAbsorption.Caption := 'Adsorption Isoterm (NH4+)';
      LblDiscretization.Caption := 'Discretization';
         { Init Buttons... }
      BtnHeadingComments1.Caption := 'Comments';
      BtnHeadingComments2.Caption := 'Comments';
      BtnNewHorizon.Caption := 'New';
      BtnEditHorizon.Caption := 'Edit';
      BtnDeleteHorizon.Caption := 'Delete';
{      SBProfileUp.Glyph.LoadFromFile('ArrowUp.bmp');
      SBProfileDown.Glyph.LoadFromFile('ArrowDn.bmp'); }
      BtnDeleteProfile.Caption := 'Remove';
      BtnAdd.Caption := 'Add >>';
      BtnDiscretization.Caption := 'Edit';
      BtnAbsorption.Caption := 'Parameters';
      BtnStateInit.Caption := 'Soil Profile State Variable Initialization';
      BtnOK.Caption := 'OK';
      BtnCancel.Caption := 'Cancel';
      BtnHelp.Caption := 'Help';
(*         { Init the group boxes...}
      with RGSelect do begin
         Caption := 'Select Horizons from';
         Items.Clear;
         Items.Add('Standard Library');
         Items.Add('User Defined Library');
      end;
*)
      GBCompose.Caption := 'Compose Soil Profile';
         { Init Combos... }
      ComboWaterFlow.Clear;
      ComboWaterFlow.Items.Add('Grawimetric flow');
      ComboWaterFlow.Items.Add('Fixed Groundwater table');
      ComboWaterFlow.Items.Add('Fluctuating Groundwater table');
      ComboWaterFlow.Items.Add('Lysimeter');
      ComboAbsorption.Items.Clear;
      ComboAbsorption.Items.Add('Daisy v.1');
      ComboAbsorption.Items.Add('Langmuir');
      ComboAbsorption.Items.Add('Freundlich');
      ComboAbsorption.Items.Add('Linear Freundlich');
         { Init Menues... }
      with MMenuProfile do begin
         File1.Caption := 'File';
         NewProfile1.Caption := 'Clear Profile';
         OpenProfile1.Caption := 'Load Profile...';
         SaveProfile1.Caption := 'Save Profile...';
         Close1.Caption := 'Close';
         Help1.Caption := 'Help';
         Contents1.Caption := 'Contents';
         SearchforHelpOn1.Caption := 'Search for Help On...';
         HowtoUseHelp1.Caption := 'How to Use Help';
         About1.Caption := 'About';
      end;

      { Init HelpContexts }
      EditHeading.HelpContext := HLP_Soil_Profile_Heading;
      BtnHeadingComments2.HelpContext := HLP_Soil_Profile_Heading;
      BtnHeadingComments1.HelpContext := HLP_Soil_Profile_Heading;
      GBCompose.HelpContext := HLP_Soil_Profile_Composer;
      RGSelect.HelpContext := HLP_RG_SelHorFromLib;
      SGHorizons.HelpContext := HLP_Soil_Horizon_List;
      BtnAdd.HelpContext := HLP_Add_Horizon_btn;
      SGProfile.HelpContext := HLP_Soil_Profile_List;
      BtnNewHorizon.HelpContext := HLP_New_Horizon_btn;
      BtnEditHorizon.HelpContext := HLP_Edit_Horizon_btn;
      BtnDeleteHorizon.HelpContext := HLP_Delete_horizon_btn;
//      SBProfileUp.HelpContext := HLP_MoveHorUpDown_button;
//      SBProfileDown.HelpContext := HLP_MoveHorUpDown_button;
      BtnDeleteProfile.HelpContext := HLP_Remove_Horizon_btn;
      EditRootingDepth.HelpContext := HLP_Max_Rooting_Depth;
      EditDispersibility.HelpContext := HLP_SolConvTrsp_Dispersivity;
      ComboWaterFlow.HelpContext := HLP_Bound_Cond_Water_Flow;
      BtnWaterFlow.HelpContext := HLP_Bound_Cond_Water_Flow;
      ComboAbsorption.HelpContext := HLP_Adsorption_Isoterm;
      BtnAbsorption.HelpContext := HLP_Adsorption_Isoterm;
      BtnDiscretization.HelpContext := HLP_Discretization;
      BtnStateInit.HelpContext := HLP_StateVar_Init;
      BtnOK.HelpContext := HLP_Soil_Profile;
      BtnCancel.HelpContext := HLP_Soil_Profile;
      BtnHelp.HelpContext := HLP_Soil_Profile;
   end;
end;

procedure TFormProfile.ComboWaterFlowChange(Sender: TObject);
begin
   TempProfileVars.WaterFlowIdx := ComboWaterFlow.ItemIndex;
   BtnWaterFlowUpdate;
   BtnStateInitUpdate;
   TempProfileVars.Dirty := TRUE;
end;


procedure TFormProfile.BtnWaterFlowClick(Sender: TObject);
var
   x, y: Integer;
   MaxValue, Deflt: Double;
begin
   MaxValue := TempProfileVars.GetLowestHor; { If nothing else is set, use this as default }
   if MaxValue < 0 then { No horizons in profile }
      MaxValue := 0;
   Deflt := MaxValue; { If nothing else is set, use this as default }
   x := FormProfile.Left + BtnWaterFlow.Left;
   y := FormProfile.Top + BtnWaterFlow.Top;
   with ComboWaterFlow do begin
      case ItemIndex of
         1: begin { Fixed groundwater table }
               If (TempProfileVars.DepthOfGroundWater >= 0) and
                        (TempProfileVars.DepthOfGroundWater < MaxValue) then
                  Deflt := TempProfileVars.DepthOfGroundWater;
               InitOneEdit(x, y, Items[ItemIndex],'Depth of Groundwater table'
                              , 'cm', 1, Deflt, MaxValue
                              , HLP_LowBndCond_Fixed_Groundw_tbl);
               FormOneEdit.ShowModal;
               if FormOneEdit.ModalResult = mrOK then begin
                  TempProfileVars.DepthOfGroundWater := StrToFloat(ReadOneEdit); { Returnvalue valid float! }
                  TempProfileVars.Dirty := TRUE;
               end;
            end;
         2: begin { Fluctuating Groundwater Table }
               with OpenDlgProfile do begin
                  DefaultExt := '';
                  FileName   := TempProfileVars.FluctuatingGroundwaterFile;
                  Filter     := '';
                  FilterIndex:= 1;
                  HelpContext:= HLP_LowBndCond_Fluct_Grdwater_tbl;
                  Title      := 'Fluctuating Groundwater table';
                  if Execute then begin
                     TempProfileVars.FluctuatingGroundwaterFile := FileName;
                     TempProfileVars.Dirty := TRUE;
                  end;
               end;
            end;
         3: begin { Lysimeter depth }
               If (TempProfileVars.LysimeterDepth >= 0) and
                        (TempProfileVars.LysimeterDepth < MaxValue)then
                  Deflt := TempProfileVars.LysimeterDepth;
               InitOneEdit(x, y, Items[ItemIndex],'Lysimeter depth', 'cm', 1
                           , Deflt, MaxValue, HLP_LowBndCond_Lysimeter);
               FormOneEdit.ShowModal;
               if FormOneEdit.ModalResult = mrOK then begin
                  TempProfileVars.LysimeterDepth := StrToFloat(ReadOneEdit);  { Returnvalue valid float! }
                  TempProfileVars.Dirty := TRUE;
               end;
            end;
      { Button is not shown for itemindex=0 ! }
      end;
   end;
end;

procedure TFormProfile.BtnHeadingCommentsClick(Sender: TObject);
var
   x, y: Integer;
begin
   x := FormProfile.Left + BtnHeadingComments1.Left;
   y := FormProfile.Top + BtnHeadingComments1.Top;
   InitComments(x, y, 'Profile Comments', TempProfileVars.ProfileComments);
   if FormComments.ShowModal = mrOK then begin
      ReadComments(TempProfileVars.ProfileComments);
      BtnProfileHeadingCommentUpdate;
      TempProfileVars.Dirty := TRUE;
   end;
end;

procedure TFormProfile.BtnCancelClick(Sender: TObject);
begin
   ModalResult := mrCancel;
end;


procedure TFormProfile.BtnAddClick(Sender: TObject);
var
   horname: string;
   i, insertafter: Integer;
   selhor, selprof: LongInt;
begin
   with SGHorizons do
      if (Cells[0, Row] = '') and (Cells[1, Row] = '') then
         selhor := 0  { In case of first row selcted when empty }
      else
         selhor := Row;
   selprof := SGProfile.Row - 1;

   insertafter := 1;  { May be modified at runtime if wanted }

   if selhor > 0 then begin  { A horizon must be selected to add into profile }
      MkHorizonName(SGHorizons.Cells[0,selhor],
                    SGHorizons.Cells[1,selhor], HorName);
      with TempProfileVars.SGProfileItems do begin
         if TempProfileVars.NumSGProfileItems <= 0 then begin
            RowCount := 1;
            Cells[0, 0] := horname;
            Cells[1, 0] := '';
            Cells[2, 0] := IntToStr(TempProfileVars.RGSelectIdx); { The library the horizon was selected from }
            TempProfileVars.NumSGProfileItems := 1;
         end
         else begin
            if selprof=0 then begin
               { Select insert before or after first element here... }
               { insertafter := ... }
            end;
            RowCount := Rowcount + 1;
            for i := RowCount-1 downto selprof+1+insertafter do  { Move the rows following the insertion point }
               Rows[i] := Rows[i-1];
            Cells[0, selprof+insertafter] := horname;
            Cells[1, selprof+insertafter] := '';
            Cells[2, selprof+insertafter] := IntToStr(TempProfileVars.RGSelectIdx); { The library the horizon was selected from }
            Row := selprof + insertafter;  { Make the newly inserted row selected }
            TempProfileVars.NumSGProfileItems := TempProfileVars.NumSGProfileItems + 1;
         end;
      end;
      SetSGProfileFromVars; { Update StringGrid and buttons }
      { Focus second column and make rows editable }
      SGProfile.Col := 1;
      SGProfile.Options := [goFixedVertLine,goFixedHorzLine,goVertLine,goHorzLine,goEditing];

      TempProfileVars.DiscretizationViewed := FALSE;
      BtnDiscretizationUpdate;

      TempProfileVars.Dirty := TRUE;
   end;
end;

procedure TFormProfile.RGSelectClick(Sender: TObject);
begin
   TempProfileVars.RGSelectIdx := RGSelect.ItemIndex; { Set ItemIdx variable }
   SetSGHorizonsFromVars; { ...and Update SGHorizon + HorizonButtons }
end;

procedure TFormProfile.BtnDeleteProfileClick(Sender: TObject);
var
   i: Integer;
   selprof: LongInt;
begin
   selprof := SGProfile.Row - 1;

   with TempProfileVars.SGProfileItems do begin
      if TempProfileVars.NumSGProfileItems <= 1 then begin { Only one horizon inserted }
         Rows[0].Clear;    { Clear content but dont remove row }
         TempProfileVars.NumSGProfileItems := 0;
         { Make rows not editable }
         SGProfile.Options := [goFixedVertLine,goFixedHorzLine,goVertLine,goHorzLine];
      end
      else begin
         for i := selprof to RowCount-2 do  { Move the rows following the insertion point }
            Rows[i] := Rows[i+1];
         Rows[RowCount-1].Clear;  { Better safe than sorry... }
         RowCount := Rowcount - 1;
         TempProfileVars.NumSGProfileItems := RowCount;
      end;
      if selprof <= RowCount - 1 then
         Row := selprof
      else
         Row := RowCount - 1;
   end;
   SetSGProfileFromVars;

   TempProfileVars.DiscretizationViewed := FALSE;
   BtnDiscretizationUpdate;

   TempProfileVars.Dirty := TRUE;
end;

procedure TFormProfile.SGProfileClick(Sender: TObject);
begin
{ Do this in 'BtnRemoveClick' and BtnAddClick' and 'OnSelectCell' instead to be correctly updated }
{
   with SGProfile do begin
      if (Col=1) and (Cells[0,1]<>'') then
         Options := [goFixedVertLine,goFixedHorzLine,goVertLine,goHorzLine,goEditing]
      else
         Options := [goFixedVertLine,goFixedHorzLine,goVertLine,goHorzLine];
   end;
}
   BtnProfileUpdate;
end;

procedure TFormProfile.SBProfileUpClick(Sender: TObject);
var
   temp0, temp1, temp2: string;
begin
   with TempProfileVars.SGProfileItems do begin
      Row := SGProfile.Row - 1;
      temp0 := Cells[0, Row];
      temp1 := Cells[1, Row];
      temp2 := Cells[2, Row];
      Cells[0, Row] := Cells[0, Row-1];
      Cells[1, Row] := Cells[1, Row-1];
      Cells[2, Row] := Cells[2, Row-1];
      Cells[0, Row-1] := temp0;
      Cells[1, Row-1] := temp1;
      Cells[2, Row-1] := temp2;
      Row := Row - 1;
   end;
   SetSGProfileFromVars;

   TempProfileVars.DiscretizationViewed := FALSE;
   BtnDiscretizationUpdate;

   TempProfileVars.Dirty := TRUE;
end;

procedure TFormProfile.SBProfileDownClick(Sender: TObject);
var
   temp0, temp1, temp2: string;
begin
   with TempProfileVars.SGProfileItems do begin
      Row := SGProfile.Row - 1;
      temp0 := Cells[0, Row];
      temp1 := Cells[1, Row];
      temp2 := Cells[2, Row];
      Cells[0, Row] := Cells[0, Row+1];
      Cells[1, Row] := Cells[1, Row+1];
      Cells[2, Row] := Cells[2, Row+1];
      Cells[0, Row+1] := temp0;
      Cells[1, Row+1] := temp1;
      Cells[2, Row+1] := temp2;
      Row := Row + 1;
   end;
   SetSGProfileFromVars;

   TempProfileVars.DiscretizationViewed := FALSE;
   BtnDiscretizationUpdate;

   TempProfileVars.Dirty := TRUE;
end;

procedure TFormProfile.GBComposeExit(Sender: TObject);
var
   i, x, y: Integer;
   Missing, msg: Boolean;
   lowest, tempd: double;
   s: string;
begin
   { It is allowed to leave the form at any time... }
   if (ActiveControl <> BtnCancel) then begin
(* Cannot be done all in once because of discretization...
      { Check if valid values }
      if not CheckValidFields(TRUE) then
         exit;
*)

      msg := TRUE;
      { SGProfile - Start check here }
      with SGProfile do begin
         if TempProfileVars.NumSGProfileItems > 0 then begin { Updates on Add/Remove!! }
            Missing := FALSE;
            s := '';
            for i:=1 to RowCount-1 do { First find everything wrong... }
               try
                  Cells[1,i] := StrTo1DecPosNotZeroStr(Cells[1,i]);
               except
                  on E: Exception do begin
                     Missing := TRUE;
                     if s = '' then
                        s := s + IntToStr(i)
                     else
                        s := s + ', ' + IntToStr(i);
      {               break;} { don't break, but check/convert all rows. }
                  end;
               end;
            if Missing then begin
               if msg then begin
                  x := Left + GBCompose.Left + FormProfile.Left - 240;
                  y := Top + GBCompose.Top + FormProfile.Top + 10;
                  MessageDlgPos(GBCompose.Caption + ':' + Chr(10) + Chr(13)
                                 + 'All horizons must have a valid size.' + Chr(10) + Chr(13)
                                 + 'Lines ' + s + ' in the profile has invalid size definition.'
                                 + Chr(10) + Chr(13)
                                 + 'Use only integer of floating point numbers > 0 as sizes.',
                                 mtInformation, [mbOK], 0, x, y);
                  end;
               SetFocus;
               exit;
            end;
         end;  { if TempProfileVars.NumSGProfileItems > 0 }
      end;
      { SGProfile - End check here }

      if TempProfileVars.NumSGProfileItems > 0 then  { Update vars... }
         for i:=1 to TempProfileVars.NumSGProfileItems do
            TempProfileVars.SGProfileItems.Cells[1,i-1] := SGProfile.Cells[1,i];
      SetSGProfileFromVars;

      { Chech if the discretization is valid with respect to the current profile   }
      if not TempProfileVars.DiscVars.IsHorsInDisc(TempProfileVars) then begin
         TempProfileVars.DiscVars.Reset(TempProfileVars);
         TempProfileVars.DiscretizationViewed := FALSE;
         MessageDlg('The profile has changed, which caused the discretization to be reset.',
                     mtInformation, [mbOK], 0);
      end;
      BtnDiscretizationUpdate;

      { EditRootingDepth - Start check here }
      with EditRootingDepth do begin
         Text := Trim(Text); { Get rid of leading/trailing spaces}
         if Text <> '' then begin { '' is a valid value! }
            x := Left + FormProfile.Left - 170;
            y := Top + FormProfile.Top - 110;

            try Text := StrTo1DecPosStr(Text);
            except
               on E: Exception do begin
                  if msg then
                     MessageDlgPos(LblRootingDepth.Caption + ':' + CHR(10) + CHR(13)
                                   + 'Not a valid positive floating point number!',
                                    mtInformation, [mbOK], 0, x, y);
                  SetFocus;
                  exit;
               end;
            end;

            { OK numberformat, now check if number is below lowest horizon }
            lowest := TempProfileVars.GetLowestHor;
            tempd := StrToFloat(Text);
            if (lowest < 0) then begin { no horizons in profile }
               if (tempd <> 0) then begin
                  if msg then
                     MessageDlgPos(LblRootingDepth.Caption + ':' + CHR(10) + CHR(13)
                                 + 'The value can not be below the deepest horizon.'
                                 + CHR(10) + CHR(13)
                                 + 'Please add horizons to the profile before setting the value',
                                 mtInformation, [mbOK], 0, x, y);
                  SetFocus;
                  exit;
               end
               else ; { 0 is an OK value when no horizons are in profile! }
            end
            else if tempd > lowest then begin
               if msg then
                  MessageDlgPos(LblRootingDepth.Caption + ':' + CHR(10) + CHR(13)
                              + 'The value for ' + LblRootingDepth.Caption
                              + ' can not be below the deepest horizon (which '
                              + 'is at ' + FormatFloat('0.0', lowest)
                              + ' cm)' + CHR(10) + CHR(13)
                              + 'Please enter a valid value.',
                              mtInformation, [mbOK], 0, x, y);
               SetFocus;
               exit;
            end;
         end; { if Text <> '' }
      end; { with EditRootingDepth do }
      { EditRootingDepth - End check here }
   end;


   (*
    Missing := FALSE;
   { It is allowed to leave the form at any time... }
   if (ActiveControl <> BtnCancel) then begin
      { ...but not to leave the group unless sizes are defined for all horizons }
      OkToLeave := TRUE;
      x := SGProfile.Left + GBCompose.Left + FormProfile.Left - 240;
      y := SGProfile.Top + GBCompose.Top + FormProfile.Top + 10;
      with SGProfile do begin
         if TempProfileVars.NumSGProfileItems > 0 then begin
            s := '';
            for i:=1 to RowCount-1 do
               try begin
                  Cells[1,i] := StrTo1DecPosNotZeroStr(Cells[1,i]);
                  TempProfileVars.SGProfileItems.Cells[1,i-1] := Cells[1,i];
               end;
               except
                  on E: Exception do begin
                     Missing := TRUE;
                     if s = '' then
                        s := s + IntToStr(i)
                     else
                        s := s + ', ' + IntToStr(i);
      {               break;} { don't break, but check/convert all rows. }
                  end;
               end;
            if Missing then begin
               MessageDlgPos('All horizons must have a valid size.' + Chr(10) + Chr(13)
                              + 'Lines ' + s + ' in the profile has invalid size definition.'
                              + Chr(10) + Chr(13)
                              + 'Use only real or integer numbers > 0 as sizes.',
                           mtInformation, [mbOK], 0, x, y);
               OkToLeave := FALSE;
            end;
         end;  { if SGProfile.Cells[0,1] <> '' }
      end;

      SetSGProfileFromVars;

      if OkToLeave then begin { Update Variables... }

         if not TempProfileVars.DiscVars.IsHorsInDisc(TempProfileVars) then begin
            TempProfileVars.DiscVars.Reset(TempProfileVars);
            TempProfileVars.DiscretizationViewed := FALSE;
         end;
         BtnDiscretizationUpdate;
         if TempProfileVars.DiscretizationViewed = FALSE then
            MessageDlgPos('The profile has changed, which caused the discretization to be reset.',
                        mtInformation, [mbOK], 0, x, y);

// previously commented out start
         if TempProfileVars.DiscVars.IsValid(TempProfileVars.GetLowestHor)
               and (TempProfileVars.GetLowestHor >= 0) then
            TempProfileVars.DiscVars.Update(TempProfileVars)
         else
            TempProfileVars.DiscVars.Reset(TempProfileVars); { + possible user warning here... }
         BtnDiscretizationUpdate;
         if TempProfileVars.NumSGProfileItems > 0 then
            TempProfileVars.DiscretizationViewed := FALSE;
// previously commented out end
      end
      else { NOT OkToLeave }
         SGProfile.SetFocus;
   end; { if (ActiveControl <> BtnCancel) }
*)
end;

procedure TFormProfile.EditHeadingExit(Sender: TObject);
begin
   { Check if valid heading }

   {- and then...}
   TempProfileVars.ProfileHeading := EditHeading.Text;
end;

procedure TFormProfile.BtnNewHorizonClick(Sender: TObject);
begin
   Temp2HorizonVars.Reset;
   Temp2HorizonVars.LoadDefault;
   Temp2HorizonVars.HorizonHeading1 := '';
   Temp2HorizonVars.HorizonHeading2 := '';
   with FormHorizon do begin
      EditHeading1.Enabled := TRUE;
      EditHeading2.Enabled := TRUE;
      Caption := 'New Horizon';
      SetFormFromVars;
      ShowModal;
      if ModalResult = mrOK then begin
         { Horizon saved to lib (SimSpec) onOKClick, now update list }
         { ...but don't put it into profile grid - it has to be selected first! }
      //   MainFormVars.UpdateLib(4, 3); // NO - do this when saving to lib!!
         TempProfileVars.Dirty := TRUE;
      end;
   end;

   { Re-input horizons into horizonlist if new horizon was saved, }
   { or if OK was selected in FormHorizon                         }
   SetSGHorizonsFromVars;
end;

procedure TFormProfile.BtnEditHorizonClick(Sender: TObject);
var
   i, x, y: Integer;
   horname: String;
   CanEdit: Boolean;
begin
   x := BtnEditHorizon.Left + GBCompose.Left
        + FormProfile.Left - 100;
   y := BtnEditHorizon.Top + GBCompose.Top + FormProfile.Top - 50;
   with SGHorizons do begin
      { If empty horizon list, 'edit' means 'new'... }
      { (edit-button only enabled if a horizon is selected, but just to be safe...) }
      if (Cells[0, Row] = '') or (Cells[1, Row] = '') then
         BtnNewHorizonClick(Sender)
      else begin
         CanEdit := TRUE;
         MkHorizonName(Cells[0, Row], Cells[1, Row], horname);
         if not FromLibrary(horname, 'horizon', MainFormVars.SimSpecLib) then begin
            CanEdit := FALSE;
            MessageDlgPos('Selected horizon "' +  horname
                          + '" not found in library.' + Chr(10) + Chr(13)
                          + 'Please report to program developer!'
                          , mtError, [mbOK], 0, x, y);
         end;
         if CanEdit then begin
            { Check that the selected horizon does not exist in the profile }
            for i:=0 to TempProfileVars.NumSGProfileItems-1 do begin
               if TempProfileVars.SGProfileItems.Cells[0, i] = horname then begin
                  if MessageDlgPos('Selected horizon "' +  horname
                             + '" exists in the current profile.' + Chr(10) + Chr(13)
                             + 'All changes made to the horizon, will also have '
                             + 'effect for the instances decleared in the profile.'
                             + Chr(10) + Chr(13)
                             + 'Select <OK> to do it, or <Cancel> to regret'
                             , mtWarning, [mbOK, mbCancel], 0, x, y) <> mrOK then
                     CanEdit := FALSE;
                  Break;
               end;
            end;
         end;
         if CanEdit then begin
            { InitHorizon with horizondata from libfile }
            with Temp2HorizonVars do begin
               SplitHorizonName(HorizonHeading1, HorizonHeading2, HorName);
               Load(HorName, 1);
            end;
            FormHorizon.Caption := 'Edit Horizon';
            FormHorizon.EditHeading1.Enabled := FALSE;
            FormHorizon.EditHeading2.Enabled := FALSE;
            FormHorizon.SetFormFromVars;
            FormHorizon.ShowModal;
            if FormHorizon.ModalResult = mrOK then begin
               { Horizon saved to lib (SimSpec) onOKClick, now update list }
               { ...but don't put it into profile grid - it has to be selected first! }
               TempProfileVars.Dirty := TRUE;
            end;

            { Re-input horizons into horizonlist if new horizon was saved, }
            { or if OK was selected in FormHorizon                         }
            SetSGHorizonsFromVars;
         end; { if CanEdit }
      end;
   end;
end;

procedure TFormProfile.BtnDeleteHorizonClick(Sender: TObject);
var
   i, x, y: Integer;
   horname: String;
   CanDelete: Boolean;
begin
   x := BtnDeleteHorizon.Left + GBCompose.Left
        + FormProfile.Left - 100;
   y := BtnDeleteHorizon.Top + GBCompose.Top + FormProfile.Top - 50;

   with SGHorizons do begin
      { A horizon must exist in the list }
      if (Cells[0, Row] <> '') and (Cells[1, Row] <> '') then begin
         { Delete the horizon from the library, and then  }
         { update the grid                                }
         MkHorizonName(Cells[0, Row], Cells[1, Row], horname);
         CanDelete := TRUE;
         { Check that the selected horizon does not exist in the profile }
         for i:=0 to TempProfileVars.NumSGProfileItems-1 do begin
            if TempProfileVars.SGProfileItems.Cells[0, i] = horname then begin
               CanDelete := FALSE;
               Break;
            end;
         end;
         if CanDelete then begin
            { Give a warning before deletion of a horizon! }
            if MessageDlgPos('The selected horizon will be deleted from the Simulation Specific Library.'
                  + ' Select <OK> to do it, or <Cancel> to regret'
                  , mtWarning, [mbOK, mbCancel], 0, x, y) = mrOK then begin
               if FromLibrary(horname, 'horizon'
                              , MainFormVars.SimSpecLib) then begin
                  DeleteLibraryObject('horizon', horname);
                  MainFormVars.UpdateLib(4, 3);
                  SetSGHorizonsFromVars;
                  TempProfileVars.Dirty := TRUE;
               end
               else
                  MessageDlgPos('Selected horizon "' +  horname
                                + '" not found in library.' + Chr(10) + Chr(13)
                                + 'Please report to program developer!'
                                , mtError, [mbOK], 0, x, y);
            end;
         end
         else
            MessageDlgPos('Selected horizon "' +  horname
                          + '" exists in the current profile' + Chr(10) + Chr(13)
                          + 'Please delete the horizon from the current '
                          + 'profile before deleting it from the library.'
                          , mtInformation, [mbOK], 0, x, y);
      end; { if (Cells[0, Row] <> '') }
   end; { with SGHorizons do }
end;

procedure TFormProfile.EditRootingDepthExit(Sender: TObject);
var
   x, y: Integer;
   lowest: Double;
   GoodNum: Boolean;
begin
   { It is allowed to leave the form at any time... }
   if (ActiveControl <> BtnCancel) then begin
      { ...but not to leave an invalid fraction in the edit box}
      EditRootingDepth.Text := Trim(EditRootingDepth.Text); { Get rid of leading/trailing spaces}
      if EditRootingDepth.Text <> '' then begin
         GoodNum := TRUE;
         x := EditRootingDepth.Left + FormProfile.Left - 150;
         y := EditRootingDepth.Top + FormProfile.Top - 85;

         try EditRootingDepth.Text := StrTo1DecPosStr(EditRootingDepth.Text);
         except
            on E: Exception do begin
               MessageDlgPos('Not a valid positive floating point number!',
                              mtInformation, [mbOK], 0, x, y);
               EditRootingDepth.SetFocus;
               GoodNum := FALSE;
            end;
         end;
         if GoodNum then begin { Check if number is below lowest horizon }
            lowest := TempProfileVars.GetLowestHor;
            if lowest < 0 then begin
               MessageDlgPos('The value for ' + '"Max allowed Rooting Depth"'
                           + ' can not be below the deepest horizon.'
                           + CHR(10) + CHR(13)
                           + 'Please add horizons to the profile before setting.'
                           + '"Max allowed Rooting Depth".',
                              mtInformation, [mbOK], 0, x, y);
               EditRootingDepth.SetFocus;
            end
            else if StrToFloat(EditRootingDepth.Text) > lowest then begin
               MessageDlgPos('The value for ' + '"Max allowed Rooting Depth"'
                           + ' can not be below the deepest horizon (which '
                           + 'is at ' + FormatFloat('0.0', TempProfileVars.GetLowestHor)
                           + ' cm)' + CHR(10) + CHR(13)
                           + 'Please enter a valid value.',
                              mtInformation, [mbOK], 0, x, y);
               EditRootingDepth.SetFocus;
            end
            else { Update Variable }
               TempProfileVars.RootingDepth := StrToFloat(EditRootingDepth.Text);
         end;
      end
      else
         TempProfileVars.RootingDepth := NotSet;
   end;
end;

procedure TFormProfile.SGProfileSelectCell(Sender: TObject; Col,
  Row: Longint; var CanSelect: Boolean);
begin
   { Not allowed to edit the first column }
   if Col = 0 then
      SGProfile.Options :=
         [goFixedVertLine,goFixedHorzLine,goVertLine,goHorzLine]
   else if SGProfile.Cells[0,1]<>'' then
           SGProfile.Options :=
              [goFixedVertLine,goFixedHorzLine,goVertLine,goHorzLine,goEditing];
//   TempProfileVars.DiscretizationViewed := FALSE; Discretization checked on GBComposeExit!
   BtnProfileUpdate;
end;

procedure TFormProfile.Close1Click(Sender: TObject);
begin
   BtnCancelClick(Sender);
end;

procedure TFormProfile.SGHorizonsKeyPress(Sender: TObject; var Key: Char);
var
   s: String;
   c, i, maxi: Integer;
begin
   { 13 = <Return> - but works only if nothing on the form has }
   { the 'default' property set!!!                             }
   if Key = Chr(13) then
      BtnAddClick(Sender)
   else begin
      c := Ord(CharToUpper(Key));
      if (c > 64) and (c < 91) then begin { A..Z }
         i := 1;
         maxi := SGHorizons.RowCount - 1;
         while i <= maxi do begin
            s := SGHorizons.Cells[0, i];
            if c <= Ord(CharToUpper(s[1])) then
               break
            else
               i := i + 1;
         end;
         if i <= maxi then begin
            SGHorizons.TopRow := i;
            SGHorizons.Row := i;
         end;
      end;
   end;
{   SGHorizons.TopRow := SGHorizons.Row;}
end;

procedure TFormProfile.BtnOKClick(Sender: TObject);
var
   x, y, gwaterdd, gwatermm, gwateryy: Integer;
   gwaterdeepest: Double;
   GwaterOK, EditMode: Boolean;
begin
   { Check all fields for valid values first... }
   if not CheckValidFields(TRUE) then
      exit;

   x := FormProfile.Left + 100;
   y := FormProfile.Top + 300;

   { If 'Fluctuating Grounwater Table' is selected in 'Lower Boundary- }
   { conditions for Water Flow', then check if deepest groundwater in  }
   { the file is shallower than the deepest horizon defined in profile }
   if FormProfile.ComboWaterFlow.ItemIndex = 2 then begin
      if TempProfileVars.FluctuatingGroundwaterFile <> '' then begin
         if GetDeepestGroundwater(TempProfileVars.FluctuatingGroundwaterFile,
               gwaterdd, gwatermm, gwateryy, gwaterdeepest) then begin
            if gwaterdeepest > TempProfileVars.GetLowestHor then begin
               MessageDlgPos('"Fluctuating Groundwater Table" is selected in '
                           + LblWaterFlow.Caption
                           + ', but the lowest groundwater in the file: '
                           + '((dd-mm-yy) (' + IntToStr(gwaterdd) + '-'
                           + IntToStr(gwatermm) + '-'
                           + IntToStr(gwateryy) + '), '
                           + FormatFloat('0.0', gwaterdeepest) + ' cm )'
                           + ' is deeper than the deepest horizon.'
                           + CHR(10) + CHR(13)
                           + 'Please correct the defined horizons, or select'
                           + ' a valid groundwater file.',
                              mtInformation, [mbOK], 0, x, y);
               BtnWaterFlow.SetFocus;
               ModalResult := mrNone;
               exit;
            end;
         end
         else begin
            MessageDlgPos('"Fluctuating Groundwater Table" is selected in '
                           + '"Lower Boundary Conditions for Water Flow"'
                           + ', but the groundwater file selected is not correct.'
                           + CHR(10) + CHR(13)
                           + 'Please select a valid groundwater file.',
                              mtError, [mbOK], 0, x, y);
            BtnWaterFlow.SetFocus;
            ModalResult := mrNone;
            exit;
         end; { if GetDeepestGroundwater(FluctuatingGroundwaterFile,... }
      end; { if FluctuatingGroundwaterFile <> '' }
   end; { if FormProfile.ComboWaterFlow.ItemIndex = 2 }

   { Valid discretization must be viewed before selecting <OK> }
   if not TempProfileVars.DiscretizationViewed then begin
      MessageDlgPos('The profile has changed since the discretization'
                     + ' was last viewed.'
                     + CHR(10) + CHR(13)
                     + 'Please open the discretization window and confirm the'
                     +' current discretization before pressing <OK>.',
                        mtInformation, [mbOK], 0, x, y);
      BtnDiscretization.SetFocus;
      ModalResult := mrNone;
      exit;
   end;

   { Update vars from form... (all checks should be done here)}
   { Update might also have been done earlier (on exit from edit-boxes etc.), but ... }
   SetVarsFromForm;

   if (TempProfileVars.ProfileHeading <> '') then begin
      EditMode := not EditHeading.Enabled;
      { Check that no other Profiles exists with the same name (heading) }
      {...done in Save! }
      if TempProfileVars.Save(TempProfileVars.ProfileHeading, 1, EditMode) then begin
         MainFormVars.UpdateLib(4, 2); { Sim spec prof lib }
         { ...and then it's ok }
         ModalResult := mrOK;
      end
      else begin
         ModalResult := mrNone; // Should not be neccesary :-(
         exit;
      end;
   end
   else begin
      { Prompt for heading }
      MessageDlgPos('A valid heading must be entered before selecting <OK>.',
                  mtInformation, [mbOK], 0
                  , FormProfile.Left - 20
                  , FormProfile.Top + 50);
      EditHeading.SetFocus;
      ModalResult := mrNone;
      exit;
   end;
end;

procedure TFormProfile.EditDispersibilityExit(Sender: TObject);
var
   x, y: Integer;
   GoodNum: Boolean;
begin
   { It is allowed to leave the form at any time... }
   if (ActiveControl <> BtnCancel) then begin
      { ...but not to leave an invalid real number in the edit box }
      EditDispersibility.Text := Trim(EditDispersibility.Text); { Get rid of leading/trailing spaces}
      if EditDispersibility.Text <> '' then begin
         GoodNum := TRUE;
         try EditDispersibility.Text := StrTo1DecPosStr(EditDispersibility.Text);
         except
            on E: Exception do begin
               x := EditDispersibility.Left + FormHorizon.Left - 200;
               y := EditDispersibility.Top + FormHorizon.Top - 80;
               MessageDlgPos('Not a valid floating point number!',
                              mtInformation, [mbOK], 0, x, y);
               EditDispersibility.SetFocus;
               GoodNum := FALSE;
            end;
         end;
         if GoodNum then
            TempProfileVars.Dispersivity := StrToFloat(EditDispersibility.Text);
      end
      else
         TempProfileVars.Dispersivity := NotSet;
   end;
end;

procedure TFormProfile.FormShow(Sender: TObject);
begin
   { First set colwidth to fill stringgrids in total width }
   SGHorizons.ColWidths[0] := Trunc(SGHorizons.ClientWidth / 10);
   SGHorizons.ColWidths[1] := SGHorizons.ClientWidth - SGHorizons.ColWidths[0] - 1;
   SGProfile.ColWidths[1] := Trunc(SGProfile.ClientWidth / 4.3);
   SGProfile.ColWidths[0] := SGProfile.ClientWidth - SGProfile.ColWidths[1] - 1;
   { Update the visible components on the form }
   FormProfile.SetFormFromVars;
   if EditHeading.Enabled then begin
      EditHeading.SetFocus; { Make the form ready for entering data }
      SaveProfile1.Enabled := TRUE;
   end
   else begin
      EditRootingDepth.SetFocus; { if form shown in Edit-mode }
      SaveProfile1.Enabled := FALSE;
   end;
   FormProfile.Update;
end;

procedure TFormProfile.About1Click(Sender: TObject);
begin
   FormAbout.ShowModal;
end;

{ Gets the deepest groundwater from groundwater file with syntax: }
{ dd:int mm:int yy:int depth:real                                 }
function GetDeepestGroundwater(fgf: string; var dd, mm, yy: Integer; var d: Double): Boolean;
var
   f: Text;
   tempdd, tempmm, tempyy, tdd, tmm, tyy: Integer;
   tempdeepest, tdeepest: Double;
begin
   Result := TRUE;
   tempdd := -1; tempmm := -1; tempyy := -1; tempdeepest := -1;

   try begin
      AssignFile(f, fgf);
      Reset(f);
      while not EOF(f) do begin
         ReadLn(f, tdd, tmm, tyy, tdeepest);
         if tdeepest > tempdeepest then begin { Update deepest groundwater so far }
            tempdeepest := tdeepest;
            tempdd := tdd; tempmm := tmm; tempyy := tyy;
         end;
      end;
      CloseFile(f);
   end;
   except
      on E:Exception do begin
         Result := FALSE;
         CloseFile(f);
      end;
   end;

   if Result then begin { Update real vars... }
      dd := tempdd; mm := tempmm; yy := tempyy; d := tempdeepest;
   end;
end;


procedure TFormProfile.ComboAbsorptionChange(Sender: TObject);
begin
   TempProfileVars.AbsorptionIsotermIdx := ComboAbsorption.ItemIndex;
   BtnAbsorptionUpdate;
   TempProfileVars.Dirty := TRUE;
end;

procedure TFormProfile.BtnAbsorptionClick(Sender: TObject);
var
   p: Array [0..256] of Char;
begin
   with ComboAbsorption do begin
      case ItemIndex of
         1..3: begin
                  StrPCopy(p, Items[ItemIndex]);
                  Application.MessageBox('Here comes some parameters -- later!!',p , mb_OK);
//                  TempProfileVars.Dirty := TRUE;
               end;
      end;
   end;
end;

procedure TFormProfile.Contents1Click(Sender: TObject);
begin
   Application.HelpCommand(HELP_TAB, 0);
end;

procedure TFormProfile.SearchforHelpOn1Click(Sender: TObject);
begin
   Application.HelpCommand(HELP_PARTIALKEY, HelpEmptyKey);
end;

procedure TFormProfile.HowtoUseHelp1Click(Sender: TObject);
begin
   Application.HelpCommand(HELP_HELPONHELP, 0);
end;

procedure TFormProfile.BtnHelpClick(Sender: TObject);
begin
   Application.HelpContext(HLP_Soil_Profile);
end;

procedure TFormProfile.EditHeadingChange(Sender: TObject);
begin
   TempProfileVars.Dirty := TRUE;
end;

procedure TFormProfile.EditRootingDepthChange(Sender: TObject);
begin
   TempProfileVars.Dirty := TRUE;
end;

procedure TFormProfile.EditDispersibilityChange(Sender: TObject);
begin
   TempProfileVars.Dirty := TRUE;
end;

procedure TFormProfile.SGProfileKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
   { If the <Delete> button is enabled, and the [Delete] key is pressed, then
     simulate that the <Delete> button was clicked. }
   If BtnDeleteProfile.Enabled and (Key = VK_Delete) then
      BtnDeleteProfileClick(Sender);
end;

procedure TFormProfile.SGHorizonsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
   { If the <Delete> button is enabled, and the [Delete] key is pressed, then
     simulate that the <Delete> button was clicked. }
   If BtnDeleteHorizon.Enabled and (Key = VK_Delete) then
      BtnDeleteHorizonClick(Sender);
end;

end.
