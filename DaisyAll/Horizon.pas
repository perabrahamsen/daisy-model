unit Horizon;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Menus, Buttons, Tabel, TwoEdit, Comments, Globals,
  Mask, About, LibComp, DHeading, TypInfo;

type
  TFormHorizon = class(TForm)
    MMenuHorizon: TMainMenu;
    File1: TMenuItem;
    NewHorizon1: TMenuItem;
    OpenHorizon1: TMenuItem;
    SaveHorizon1: TMenuItem;
    N1: TMenuItem;
    Close1: TMenuItem;
    LblHeading: TLabel;
    EditHeading2: TEdit;
    BtnHeadingComments1: TButton;
    EditHeading1: TEdit;
    GBTexture: TGroupBox;
    LblOrganic: TLabel;
    LblClay: TLabel;
    LblSilt: TLabel;
    LblFine: TLabel;
    LblCoarse: TLabel;
    LblBulk: TLabel;
    LblHydraulic: TLabel;
    LblSolute: TLabel;
    LblThermal: TLabel;
    EditBulk: TEdit;
    ComboHydraulic: TComboBox;
    ComboSolute: TComboBox;
    ComboThermal: TComboBox;
    BtnHydraulic: TButton;
    BtnSolute: TButton;
    BtnThermal: TButton;
    BtnOK: TBitBtn;
    BtnCancel: TBitBtn;
    BtnHelp: TBitBtn;
    GBSoil: TGroupBox;
    LblPassive: TLabel;
    LblActive: TLabel;
    LblCNPassive: TLabel;
    LblCNActive: TLabel;
    EditPassive: TEdit;
    EditActive: TEdit;
    EditCNPassive: TEdit;
    EditCNActive: TEdit;
    LblBulkUnits: TLabel;
    EditOrganic: TEdit;
    EditClay: TEdit;
    EditSilt: TEdit;
    EditFine: TEdit;
    EditCoarse: TEdit;
    BtnHeadingComments2: TBitBtn;
    Help1: TMenuItem;
    About1: TMenuItem;
    HowtoUseHelp1: TMenuItem;
    SearchforHelpOn1: TMenuItem;
    Contents1: TMenuItem;
    N2: TMenuItem;
    OpenDlgHorizon: TOpenDialog;
    SaveHorizonAs1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure ComboHydraulicChange(Sender: TObject);
    procedure ComboSoluteChange(Sender: TObject);
    procedure ComboThermalChange(Sender: TObject);
    procedure BtnThermalClick(Sender: TObject);
    procedure BtnHydraulicClick(Sender: TObject);
    procedure BtnSoluteClick(Sender: TObject);
    procedure BtnHeadingCommentsClick(Sender: TObject);
    procedure EditHeading1Exit(Sender: TObject);
    procedure EditHeading2Exit(Sender: TObject);
    procedure EditTextureExit(Sender: TObject);
    procedure GBTextureExit(Sender: TObject);
    procedure EditCNPassiveExit(Sender: TObject);
    procedure EditBulkExit(Sender: TObject);
    procedure OpenHorizon1Click(Sender: TObject);
    procedure EditActiveExit(Sender: TObject);
    procedure EditPassiveExit(Sender: TObject);
    procedure GBSoilExit(Sender: TObject);
    procedure FormShow(Sender: TObject);

    procedure SetFormFromVars;
    procedure SetVarsFromForm;
    procedure BtnsHorizonUpdate;

    procedure BtnHorizonHeadingCommentUpdate; { Comments or not? }
    procedure BtnHydraulicUpdate; { Show or hide the parameters buttons }
    procedure BtnSoluteUpdate;
    procedure BtnThermalUpdate;
    procedure BtnOKClick(Sender: TObject);
    procedure NewHorizon1Click(Sender: TObject);
    procedure SaveHorizon1Click(Sender: TObject);
    procedure EditCNActiveExit(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure BtnHelpClick(Sender: TObject);
    procedure Contents1Click(Sender: TObject);
    procedure HowtoUseHelp1Click(Sender: TObject);
    procedure SearchforHelpOn1Click(Sender: TObject);
    function CheckValidFields(msg: Boolean): Boolean;
    procedure EditHeading1Change(Sender: TObject);
    procedure EditHeading2Change(Sender: TObject);
    procedure EditOrganicChange(Sender: TObject);
    procedure EditClayChange(Sender: TObject);
    procedure EditSiltChange(Sender: TObject);
    procedure EditFineChange(Sender: TObject);
    procedure EditCoarseChange(Sender: TObject);
    procedure EditBulkChange(Sender: TObject);
    procedure EditPassiveChange(Sender: TObject);
    procedure EditActiveChange(Sender: TObject);
    procedure EditCNPassiveChange(Sender: TObject);
    procedure EditCNActiveChange(Sender: TObject);
    procedure SaveHorizonAs1Click(Sender: TObject);

  private
    { Private declarations }
    NoOfTextureEditSet: Integer;
  public
    { Public declarations }
  end;


var
   FormHorizon: TFormHorizon;


implementation
uses Profil, Hydraul, SaveAs;

{$R *.DFM}


{ Check all input fields on the FORM for valid values             }
{ Return FALSE when first invalid found (and focus it)            }
{ msg=TRUE => show errormessages                                  }
function TFormHorizon.CheckValidFields(msg: Boolean): Boolean;
var
   tempvalue, PassiveActiveVal: Double;
   x, y: Integer;
//   MissingPtr, HighestPtr: ^TEdit;
//   OrganicSet, ClaySet, SiltSet, FineSet, CoarseSet, SetSum: Integer;
//   OrganicFrac, ClayFrac, SiltFrac, FineFrac, CoarseFrac, FracSum, Reminder, Highest: Integer;
begin
   Result := FALSE; { Fear the worst... }

   { Fields that are allways ok, are commented out... }

//      EditHeading1;

//      EditHeading2;

{ Texture composition - Start check here }
(*
   OrganicSet := 0; ClaySet := 0; SiltSet := 0; FineSet := 0; CoarseSet := 0;
   OrganicFrac := 0; ClayFrac := 0; SiltFrac := 0; FineFrac := 0; CoarseFrac := 0;
*)
   x := GBTexture.Left + FormHorizon.Left - 170;
   y := GBTexture.Top + FormHorizon.Top + 130;

   with EditOrganic do begin
      Text := Trim(Text); { Get rid of leading/trailing spaces}
      if Text <> '' then begin
         try Text := StrTo3DecFracStr(Text);
         except
            on E: Exception do begin
               if msg then
                  MessageDlgPos(GBTexture.Caption + ' -> ' + LblOrganic.Caption
                                 + ':'  + CHR(10) + CHR(13)
                                 + FracErrMsg, mtInformation, [mbOK], 0, x, y);
               SetFocus;
               exit;
            end;
         end;
//         OrganicSet := 1;
//         OrganicFrac := StrToInt(FormatFloat('0', 1000 * StrToFloat(Text)));
      end  { if text <> '' }
   end;

   with EditClay do begin
      Text := Trim(Text); { Get rid of leading/trailing spaces}
      if Text <> '' then begin
         try Text := StrTo3DecFracStr(Text);
         except
            on E: Exception do begin
               if msg then
                  MessageDlgPos(GBTexture.Caption + ' -> ' + LblClay.Caption
                                 + ':' + CHR(10) + CHR(13)
                                 + FracErrMsg, mtInformation, [mbOK], 0, x, y);
               SetFocus;
               exit;
            end;
         end;
//         ClaySet := 2;
//         ClayFrac := StrToInt(FormatFloat('0', 1000 * StrToFloat(Text)));
      end  { if text <> '' }
   end;

   with EditSilt do begin
      Text := Trim(Text); { Get rid of leading/trailing spaces}
      if Text <> '' then begin
         try Text := StrTo3DecFracStr(Text);
         except
            on E: Exception do begin
               if msg then
                  MessageDlgPos(GBTexture.Caption + ' -> ' + LblSilt.Caption
                                 + ':' + CHR(10) + CHR(13)
                                 + FracErrMsg, mtInformation, [mbOK], 0, x, y);
               SetFocus;
               exit;
            end;
         end;
//         SiltSet := 4;
//         SiltFrac := StrToInt(FormatFloat('0', 1000 * StrToFloat(Text)));
      end  { if text <> '' }
   end;

   with EditFine do begin
      Text := Trim(Text); { Get rid of leading/trailing spaces}
      if Text <> '' then begin
         try Text := StrTo3DecFracStr(Text);
         except
            on E: Exception do begin
               if msg then
                  MessageDlgPos(GBTexture.Caption + ' -> ' + LblFine.Caption
                                 + ':' + CHR(10) + CHR(13)
                                 + FracErrMsg, mtInformation, [mbOK], 0, x, y);
               SetFocus;
               exit;
            end;
         end;
//         FineSet := 8;
//         FineFrac := StrToInt(FormatFloat('0', 1000 * StrToFloat(Text)));
      end  { if text <> '' }
   end;

   with EditCoarse do begin
      Text := Trim(Text); { Get rid of leading/trailing spaces}
      if Text <> '' then begin
         try Text := StrTo3DecFracStr(Text);
         except
            on E: Exception do begin
               if msg then
                  MessageDlgPos(GBTexture.Caption + ' -> ' + LblCoarse.Caption
                                 + ':' + CHR(10) + CHR(13)
                                 + FracErrMsg, mtInformation, [mbOK], 0, x, y);
               SetFocus;
               exit;
            end;
         end;
//         CoarseSet := 16;
//         CoarseFrac := StrToInt(FormatFloat('0', 1000 * StrToFloat(Text)));
      end  { if text <> '' }
   end;

(* // This must be done on GBTextureExit, not here!!!!!
   SetSum := OrganicSet + ClaySet + SiltSet + FineSet + CoarseSet;
   FracSum := OrganicFrac + ClayFrac + SiltFrac + FineFrac + CoarseFrac;
   Reminder := 1000 - FracSum;
   MissingPtr := nil;

   { Find the highest fraction (in case of error) }
   Highest := 0;
   if OrganicFrac > Highest then begin Highest := OrganicFrac; HighestPtr := @EditOrganic; end;
   if ClayFrac > Highest then begin Highest := ClayFrac; HighestPtr := @EditClay; end;
   if SiltFrac > Highest then begin Highest := SiltFrac; HighestPtr := @EditSilt; end;
   if FineFrac > Highest then begin Highest := FineFrac; HighestPtr := @EditFine; end;
   if CoarseFrac > Highest then begin Highest := CoarseFrac; HighestPtr := @EditCoarse; end;

   case (SetSum) of  { 4 or 5 fractions defined by the user }
      15:   MissingPtr := @EditCoarse;    { EditCoarse missing }
      23:   MissingPtr := @EditFine;      { EditFine missing }
      27:   MissingPtr := @EditSilt;      { EditSilt missing }
      29:   MissingPtr := @EditClay;      { EditClay missing }
      30:   MissingPtr := @EditOrganic;   { EditOrganic missing }
      31:   { Nothing's missing => check if fraction sum = 1 (1000) }
            if FracSum <> 1000 then begin
               MessageDlgPos(GBTexture.Caption + ':' + CHR(10) + CHR(13)
                             + 'The sum of all five fractions, must equal 1.'
                             + CHR(10) + CHR(13) + '(Tip: Leave one box empty '
                             + ', and it will be calculated automatically.)',
                           mtInformation, [mbOK], 0, x, y);
               GBTexture.SetFocus;
//               HighestPtr.SetFocus;
               exit;
            end;
   end;
   if Reminder >= 0 then begin
      if MissingPtr <> nil then { Fill in the missing number }
         MissingPtr^.Text := FormatFloat('0.000', Reminder / 1000);
      { else: more than one missing => ok }
   end
   else if SetSum <> 31 then begin { Problem already taken care of if SetSum = 31 }
      MessageDlgPos(GBTexture.Caption + ':' + CHR(10) + CHR(13)
                    + 'The sum of all five fractions, must equal 1. At the '
                    + 'moment, the input fractions sums up to a value > 1.',
                    mtError, [mbOK], 0, x, y);
      GBTexture.SetFocus;
//      HighestPtr.SetFocus;
      exit;
   end;
*)
{ Texture composition - End check here }


{ EditBulk - Start check here}
   with EditBulk do begin
      Text := Trim(Text); { Get rid of leading/trailing spaces}
      if Text <> '' then begin { '' is a valid value! }
         x := Left + FormHorizon.Left - 200;
         y := Top + FormHorizon.Top - 80;
         try Text := StrTo3DecPosStr(Text);
         except
            on E: Exception do begin
               if msg then
                  MessageDlgPos(LblBulk.Caption + ':' + CHR(10) + CHR(13)
                                + 'Not a valid positive floating point number!',
                                 mtInformation, [mbOK], 0, x, y);
               SetFocus;
               exit;
            end;
         end;
         { check if variable is in [0.7 , 2.6] }
         tempvalue := StrToFloat(Text);
         if ((tempvalue <= 0.6999) or (tempvalue >= 2.6001)) then begin
            // Silly comparization because of @£$2% Delphi that calcs
            // tempvalue = 0.7 => (tempvalue < 0.7) = TRUE !!!
            if msg then
               MessageDlgPos(LblBulk.Caption + ':' + CHR(10) + CHR(13)
                             + 'The value must be in the interval [0.7, 2.6].',
                             mtInformation, [mbOK], 0, x, y);
            SetFocus;
            exit;
         end;
      end;
   end;
{ EditBulk - End check here}

//   ComboHydraulic;
//   ComboSolute;
//   ComboThermal;

{ EditActive + EditPassive - Start check here}
   PassiveActiveVal := 0;
   with EditPassive do begin
      Text := Trim(Text); { Get rid of leading/trailing spaces}
      if Text <> '' then begin { '' is a valid value! }
         try Text := StrTo3DecFracStr(Text);
         except
            on E: Exception do begin
               if msg then begin
                  x := Left + GBSoil.Left + FormHorizon.Left - 250;
                  y := Top + GBSoil.Top + FormHorizon.Top - 90;
                  MessageDlgPos(LblPassive.Caption + ':' + CHR(10) + CHR(13)
                                + FracErrMsg, mtInformation, [mbOK], 0, x, y);
               end;
               SetFocus;
               exit;
            end;
         end; { try }
         PassiveActiveVal := StrToFloat(Text);
      end; { if Text <> '' }
   end; { with EditPassive do }

   with EditActive do begin
      Text := Trim(Text); { Get rid of leading/trailing spaces}
      if Text <> '' then begin { '' is a valid value! }
         try Text := StrTo3DecFracStr(Text);
         except
            on E: Exception do begin
               if msg then begin
                  x := Left + GBSoil.Left + FormHorizon.Left - 250;
                  y := Top + GBSoil.Top + FormHorizon.Top - 90;
                  MessageDlgPos(LblActive.Caption + ':' + CHR(10) + CHR(13)
                                + FracErrMsg, mtInformation, [mbOK], 0, x, y);
               end;
               SetFocus;
               exit;
            end;
         end; { try }
         PassiveActiveVal := PassiveActiveVal + StrToFloat(Text);
      end; { if Text <> '' }
   end; { with EditActive do }
(*  Annoying to do this here :(  Do it on GBSoilExit instead
   if PassiveActiveVal > 1 then begin { only if both are set! (else exited earlier) }
      if msg then begin
         x := GBSoil.Left + FormHorizon.Left;
         y := GBSoil.Top + FormHorizon.Top - 90;
         MessageDlgPos('The sum of ' + LblPassive.Caption + ' and '
                       + LblActive.Caption + ' must be <= 1.',
                       mtError, [mbOK], 0, x, y);
      end;
      EditPassive.SetFocus;
      exit;
   end;
*)
{ EditActive + EditPassive - End check here}

{ EditCNPassive - Start check here}
   with EditCNPassive do begin
      Text := Trim(Text); { Get rid of leading/trailing spaces}
      if Text <> '' then begin { '' is a valid value! }
         x := Left + GBSoil.Left + FormHorizon.Left - 250;
         y := Top + GBSoil.Top + FormHorizon.Top - 90;
         try Text := StrTo1DecPosStr(Text);
         except
            on E: Exception do begin
               if msg then
                  MessageDlgPos(LblCNPassive.Caption + ':' + CHR(10) + CHR(13)
                                + 'Not a valid positive floating point number!',
                                 mtInformation, [mbOK], 0, x, y);
               SetFocus;
               exit;
            end;
         end;
         tempvalue := StrToFloat(Text);
         if ((tempvalue <= 5.9999) or (tempvalue >= 25.0001)) then begin
            // Silly comparization because of @£$2% Delphi that calcs
            // tempvalue = 6.0 => (tempvalue < 6.0) = TRUE !!!
            if msg then
               MessageDlgPos(LblCNPassive.Caption + ':' + CHR(10) + CHR(13)
                             + 'The value must be in the interval [6.0, 25.0].',
                              mtInformation, [mbOK], 0, x, y);
            SetFocus;
            exit;
         end;
      end; { if Text <> '' }
   end; { with EditCNPassive do begin }
{ EditCNPassive - End check here}

{ EditCNActive - Start check here}
   with EditCNActive do begin
      Text := Trim(Text); { Get rid of leading/trailing spaces}
      if Text <> '' then begin { '' is a valid value! }
         x := Left + GBSoil.Left + FormHorizon.Left - 250;
         y := Top + GBSoil.Top + FormHorizon.Top - 90;
         try Text := StrTo1DecPosStr(Text);
         except
            on E: Exception do begin
               if msg then
                  MessageDlgPos(LblCNActive.Caption + ':' + CHR(10) + CHR(13)
                                + 'Not a valid positive floating point number!',
                                 mtInformation, [mbOK], 0, x, y);
               SetFocus;
               exit;
            end;
         end;
         tempvalue := StrToFloat(Text);
         if ((tempvalue <= 5.9999) or (tempvalue >= 25.0001)) then begin
            // Silly comparization because of @£$2% Delphi that calcs
            // tempvalue = 6.0 => (tempvalue < 6.0) = TRUE !!!
            if msg then
               MessageDlgPos(LblCNActive.Caption + ':' + CHR(10) + CHR(13)
                             + 'The value must be in the interval [6.0, 25.0].',
                              mtInformation, [mbOK], 0, x, y);
            SetFocus;
            exit;
         end;
      end; { if Text <> '' }
   end; { with EditCNActive do begin }
{ EditCNActive - End check here}

{ Everything Checked OK :-) }
   Result := TRUE;
end;

procedure TFormHorizon.SetFormFromVars;
{var
   reminder: Integer;}
begin
{ Now set the immediately visible components after variable's content... }
   EditHeading1.Text := Temp2HorizonVars.HorizonHeading1;
   EditHeading2.Text := Temp2HorizonVars.HorizonHeading2;

{
   // Don't do the input check here (do it when loading file)
   // Just assume data is ok...
   reminder := 1000
               - Temp2HorizonVars.TextureOrganicFrac
               + Temp2HorizonVars.TextureClayFrac
               + Temp2HorizonVars.TextureSiltFrac
               + Temp2HorizonVars.TextureFineFrac
               + Temp2HorizonVars.TextureCoarseFrac;

   if reminder = 0 then begin
      EditOrganic.Text := FormatFloat('0.000', Temp2HorizonVars.TextureOrganicFrac / 1000);
      EditClay.Text := FormatFloat('0.000', Temp2HorizonVars.TextureClayFrac / 1000);
      EditSilt.Text := FormatFloat('0.000', Temp2HorizonVars.TextureSiltFrac / 1000);
      EditFine.Text := FormatFloat('0.000', Temp2HorizonVars.TextureFineFrac / 1000);
      EditCoarse.Text := FormatFloat('0.000', Temp2HorizonVars.TextureCoarseFrac / 1000);
      TextureOrganicFracSet := 1;
      TextureClayFracSet := 2;
      TextureSiltFracSet := 4;
      TextureFineFracSet := 8;
      TextureCoarseFracSet := 16;
   end
   else begin   // Nothing valid set - initialize to nothing set
      EditOrganic.Text := '';
      EditClay.Text := '';
      EditSilt.Text := '';
      EditFine.Text := '';
      EditCoarse.Text := '';
      Temp2HorizonVars.TextureOrganicFrac := 0;
      Temp2HorizonVars.TextureClayFrac := 0;
      Temp2HorizonVars.TextureSiltFrac := 0;
      Temp2HorizonVars.TextureFineFrac := 0;
      Temp2HorizonVars.TextureCoarseFrac := 0;
      TextureOrganicFracSet := 0;
      TextureClayFracSet := 0;
      TextureSiltFracSet := 0;
      TextureFineFracSet := 0;
      TextureCoarseFracSet := 0;
   end;
}

   if Temp2HorizonVars.TextureOrganicFrac = NotSet then begin
      EditOrganic.Text := '';
      Temp2HorizonVars.TextureOrganicFracSet := 0;
   end else begin
      EditOrganic.Text := FormatFloat('0.000', Temp2HorizonVars.TextureOrganicFrac / 1000);
      Temp2HorizonVars.TextureOrganicFracSet := 1;
   end;

   if Temp2HorizonVars.TextureClayFrac = NotSet then begin
      EditClay.Text := '';
      Temp2HorizonVars.TextureClayFracSet := 0;
   end else begin
      EditClay.Text := FormatFloat('0.000', Temp2HorizonVars.TextureClayFrac / 1000);
      Temp2HorizonVars.TextureClayFracSet := 2;
   end;

   if Temp2HorizonVars.TextureSiltFrac = NotSet then begin
      EditSilt.Text := '';
      Temp2HorizonVars.TextureSiltFracSet := 0;
   end else begin
      EditSilt.Text := FormatFloat('0.000', Temp2HorizonVars.TextureSiltFrac / 1000);
      Temp2HorizonVars.TextureSiltFracSet := 4;
   end;

   if Temp2HorizonVars.TextureFineFrac = NotSet then begin
      EditFine.Text := '';
      Temp2HorizonVars.TextureFineFracSet := 0;
   end else begin
      EditFine.Text := FormatFloat('0.000', Temp2HorizonVars.TextureFineFrac / 1000);
      Temp2HorizonVars.TextureFineFracSet := 8;
   end;

   if Temp2HorizonVars.TextureCoarseFrac = NotSet then begin
      EditCoarse.Text := '';
      Temp2HorizonVars.TextureCoarseFracSet := 0;
   end else begin
      EditCoarse.Text := FormatFloat('0.000', Temp2HorizonVars.TextureCoarseFrac / 1000);
      Temp2HorizonVars.TextureCoarseFracSet := 8;
   end;

   if Temp2HorizonVars.DryBulk = NotSet then EditBulk.Text := ''
   else EditBulk.Text := StrTo3DecPosStr(FloatToStr(Temp2HorizonVars.DryBulk));

   ComboHydraulic.ItemIndex := Temp2HorizonVars.HydraulicPropertiesIdx;
   ComboSolute.ItemIndex := Temp2HorizonVars.SoluteDiffusiveIdx;
   ComboThermal.ItemIndex := Temp2HorizonVars.ThermalPropertiesIdx;

   if Temp2HorizonVars.PassiveSlowly = NotSet then EditPassive.Text := ''
   else EditPassive.Text := StrTo3DecFracStr(FloatToStr(Temp2HorizonVars.PassiveSlowly));
   if Temp2HorizonVars.ActiveEasily = NotSet then EditActive.Text := ''
   else EditActive.Text := StrTo3DecFracStr(FloatToStr(Temp2HorizonVars.ActiveEasily));

   if Temp2HorizonVars.CNofPassive = NotSet then EditCNPassive.Text := ''
   else EditCNPassive.Text := StrTo1DecPosStr(FloatToStr(Temp2HorizonVars.CNofPassive));
   if Temp2HorizonVars.CNofActive = NotSet then EditCNActive.Text := ''
   else EditCNActive.Text := StrTo1DecPosStr(FloatToStr(Temp2HorizonVars.CNofActive));

   { Editboxes etc. in parameter windows will be set when the window opens... }

   { Update the states of the buttons in the form }
   BtnsHorizonUpdate;
end;

procedure TFormHorizon.SetVarsFromForm;
var
   s: String;
begin
   Temp2HorizonVars.HorizonHeading1 := EditHeading1.Text;
   Temp2HorizonVars.HorizonHeading2 := EditHeading2.Text;

   s := Trim(EditOrganic.Text);
   if s = '' then begin
      Temp2HorizonVars.TextureOrganicFrac := NotSet;
      Temp2HorizonVars.TextureOrganicFracSet := 0;
   end else begin
      Temp2HorizonVars.TextureOrganicFrac :=
               StrToInt(FormatFloat('0', 1000 * StrToFloat(s)));
      Temp2HorizonVars.TextureOrganicFracSet := 1;
   end;
   s := Trim(EditClay.Text);
   if s = '' then begin
      Temp2HorizonVars.TextureClayFrac := NotSet;
      Temp2HorizonVars.TextureClayFracSet := 0;
   end else begin
      Temp2HorizonVars.TextureClayFrac :=
               StrToInt(FormatFloat('0', 1000 * StrToFloat(s)));
      Temp2HorizonVars.TextureClayFracSet := 2;
   end;
   s := Trim(EditSilt.Text);
   if s = '' then begin
      Temp2HorizonVars.TextureSiltFrac := NotSet;
      Temp2HorizonVars.TextureSiltFracSet := 0;
   end else begin
      Temp2HorizonVars.TextureSiltFrac :=
               StrToInt(FormatFloat('0', 1000 * StrToFloat(s)));
      Temp2HorizonVars.TextureSiltFracSet := 4;
   end;
   s := Trim(EditFine.Text);
   if s = '' then begin
      Temp2HorizonVars.TextureFineFrac := NotSet;
      Temp2HorizonVars.TextureFineFracSet := 0;
   end else begin
      Temp2HorizonVars.TextureFineFrac :=
               StrToInt(FormatFloat('0', 1000 * StrToFloat(s)));
      Temp2HorizonVars.TextureFineFracSet := 8;
   end;
   s := Trim(EditCoarse.Text);
   if s = '' then begin
      Temp2HorizonVars.TextureCoarseFrac := NotSet;
      Temp2HorizonVars.TextureCoarseFracSet := 0;
   end else begin
      Temp2HorizonVars.TextureCoarseFrac
            := StrToInt(FormatFloat('0', 1000 * StrToFloat(s)));
      Temp2HorizonVars.TextureCoarseFracSet := 16;
   end;

   s := Trim(EditBulk.Text);
   if s = '' then Temp2HorizonVars.DryBulk := NotSet
   else Temp2HorizonVars.DryBulk := StrToFloat(s);

   Temp2HorizonVars.HydraulicPropertiesIdx := ComboHydraulic.ItemIndex;
   Temp2HorizonVars.SoluteDiffusiveIdx := ComboSolute.ItemIndex;
   Temp2HorizonVars.ThermalPropertiesIdx := ComboThermal.ItemIndex;

   s := Trim(EditPassive.Text);
   if s = '' then Temp2HorizonVars.PassiveSlowly := NotSet
   else Temp2HorizonVars.PassiveSlowly := StrToFloat(s);

   s := Trim(EditActive.Text);
   if s = '' then Temp2HorizonVars.ActiveEasily := NotSet
   else Temp2HorizonVars.ActiveEasily := StrToFloat(s);

   s := Trim(EditCNPassive.Text);
   if s = '' then Temp2HorizonVars.CNofPassive := NotSet
   else Temp2HorizonVars.CNofPassive := StrToFloat(s);

   s := Trim(EditCNActive.Text);
   if s = '' then Temp2HorizonVars.CNofActive := NotSet
   else Temp2HorizonVars.CNofActive := StrToFloat(s);

   { Editboxes etc. in parameter windows will be set when the window closes... }
end;

procedure TFormHorizon.BtnHorizonHeadingCommentUpdate;
begin
   if Length(Temp2HorizonVars.HorizonComments) = 0 then begin  { Don't show finger!! }
      BtnHeadingComments2.Hide;
      BtnHeadingComments1.Show;
   end
   else begin
      BtnHeadingComments1.Hide;
      BtnHeadingComments2.Show;
   end;
end;

procedure TFormHorizon.BtnHydraulicUpdate;
begin
{ 'Pedo-transfer functions' not yet implemented }
   case ComboHydraulic.ItemIndex of
      -1, 6: BtnHydraulic.Hide;
   else
      BtnHydraulic.Show;
   end;
end;

procedure TFormHorizon.BtnSoluteUpdate;
begin
{  Millington Quirke => no parameters
   Linear model => parameters a and b }
   case ComboSolute.ItemIndex of
      -1, 0: BtnSolute.Hide;
   else
      BtnSolute.Show;
   end;
end;

procedure TFormHorizon.BtnThermalUpdate;
begin
{  No selectable choises (yet...) }
   case ComboThermal.ItemIndex of
      -1: BtnThermal.Hide;
       0: BtnThermal.Hide; { No parameters for de Vries... }
   else
      BtnThermal.Show;
   end;
end;

procedure TFormHorizon.BtnsHorizonUpdate;
begin
    BtnHorizonHeadingCommentUpdate;
    BtnHydraulicUpdate;
    BtnSoluteUpdate;
    BtnThermalUpdate;
end;

procedure TFormHorizon.FormCreate(Sender: TObject);
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

   { To be replaced later with text from ressource file... }
   with FormHorizon do begin  { Init all the strings here... }
         { Init Labels... }
      LblHeading.Caption := 'Soil Horizon Heading';
      LblOrganic.Caption := 'Soil Organic Matter';
      LblClay.Caption := 'Clay';
      LblSilt.Caption := 'Silt';
      LblFine.Caption := 'Fine Sand';
      LblCoarse.Caption := 'Coarse Sand';
      LblBulk.Caption := 'Dry Bulk Density';
      LblBulkUnits.Caption := 'g/cm3';
      LblHydraulic.Caption := 'Hydraulic Properties';
      LblSolute.Caption := 'Solute Diffusive Transport - Impedance Factor';
      LblThermal.Caption := 'Thermal Properties of Soil';
      LblPassive.Caption := 'Passive/Slowly Decomposable Fraction';
      LblActive.Caption := 'Active/Easily Decomposable Fraction';
      LblCNPassive.Caption := 'C/N of Passive/Slowly Decomposable';
      LblCNActive.Caption := 'C/N of Active/Easily Decomposable';
         { Init Buttons... }
      BtnHeadingComments1.Caption := 'Comments';
      BtnHeadingComments2.Caption := 'Comments';
      BtnHydraulic.Caption := 'Parameters';
      BtnSolute.Caption := 'Parameters';
      BtnThermal.Caption := 'Parameters';
      BtnOK.Caption := 'OK';
      BtnCancel.Caption := 'Cancel';
      BtnHelp.Caption := 'Help';
         { Init Group Boxes... }
      GBTexture.Caption := 'Texture Composition - Fractions';
      GBSoil.Caption := 'Soil Organic Matter Turnover Model';
         { Init Combo Boxes... }
      ComboHydraulic.Items.Clear;
      ComboHydraulic.Items.Add('van Genuchten/Mualem');
      ComboHydraulic.Items.Add('van Genuchten/Burdine');
      ComboHydraulic.Items.Add('Brooks and Corey/Mualem');
      ComboHydraulic.Items.Add('Brooks and Corey/Burdine');
      ComboHydraulic.Items.Add('Modified Brooks and Corey - Smith(92)');
      ComboHydraulic.Items.Add('Table - Daisy v.1 format');
      ComboHydraulic.Items.Add('Pedo-transfer functions'); { Not yet implemented }
      ComboSolute.Items.Clear;
      ComboSolute.Items.Add('Millington Quirke');
      ComboSolute.Items.Add('Linear model (Daisy v.1)');
      ComboThermal.Items.Clear;
      ComboThermal.Items.Add('de Vries model/Daisy v.1');
         { Init Menues... }
      with MMenuHorizon do begin
         File1.Caption := 'File';
         NewHorizon1.Caption := 'Clear Horizon';
         OpenHorizon1.Caption := 'Load Horizon...';
         SaveHorizon1.Caption := 'Save Horizon...';
         Close1.Caption := 'Close';
         Help1.Caption := 'Help';
         Contents1.Caption := 'Contents';
         SearchforHelpOn1.Caption := 'Search for Help On...';
         HowtoUseHelp1.Caption := 'How to Use Help';
         About1.Caption := 'About';
      end;
         { Init HelpContexts... }
      EditHeading1.HelpContext := HLP_Horizon_Heading;
      EditHeading2.HelpContext := HLP_Horizon_Heading;
      BtnHeadingComments2.HelpContext := HLP_Horizon_Heading;
      BtnHeadingComments1.HelpContext := HLP_Horizon_Heading;
      GBTexture.HelpContext := HLP_Texture_Composition;
      EditOrganic.HelpContext := HLP_Soil_Organic_Matter;
      EditClay.HelpContext := HLP_Clay;
      EditSilt.HelpContext := HLP_Silt;
      EditFine.HelpContext := HLP_Fine_Sand;
      EditCoarse.HelpContext := HLP_Coarse_Sand;
      EditBulk.HelpContext := HLP_Dry_Bulk_Density;
      ComboHydraulic.HelpContext := HLP_Hydraulic_Properties;
      BtnHydraulic.HelpContext := HLP_Hydraulic_Properties;
      ComboSolute.HelpContext := HLP_Solute_Diffusive_Transport_Impedance_Factor;
      BtnSolute.HelpContext := HLP_Solute_Diffusive_Transport_Impedance_Factor;
      ComboThermal.HelpContext := HLP_Thermal_Properties_of_Soil;
      BtnThermal.HelpContext := HLP_Thermal_Properties_of_Soil;
      GBSoil.HelpContext := HLP_Soil_Organic_Matter_Turnover_Model;
      EditPassive.HelpContext := HLP_Passive_Slowly_Decomposable_Fraction;
      EditActive.HelpContext := HLP_Active_Easily_Decomposable_Fraction;
      EditCNPassive.HelpContext := HLP_C_N_of_Passive_Slowly_Decomposable;
      EditCNActive.HelpContext := HLP_C_N_of_Active_Easily_Decomposable;
      BtnOK.HelpContext := HLP_Soil_Horizon;
      BtnCancel.HelpContext := HLP_Soil_Horizon;
      BtnHelp.HelpContext := HLP_Soil_Horizon;
   end;
end;

procedure TFormHorizon.FormShow(Sender: TObject);
begin
   { Update the visible components on the form }
   FormHorizon.SetFormFromVars;
   if Editheading1.Enabled then begin
      EditHeading1.SetFocus; { Make the form ready for entering data }
      SaveHorizon1.Enabled := TRUE;
   end
   else begin
      EditOrganic.SetFocus; { if form shown in Edit-mode }
      SaveHorizon1.Enabled := FALSE;
   end;
   FormHorizon.Update;
   Temp2HorizonVars.Dirty := FALSE; { Nice and clean... }
end;

procedure TFormHorizon.ComboHydraulicChange(Sender: TObject);
var
   p: Array [0..256] of Char;
begin
   if ComboHydraulic.ItemIndex = 6 then begin { No support implemented }
      StrPCopy(p, ComboHydraulic.Items[ComboHydraulic.ItemIndex]);
      Application.MessageBox('Not implemented (at time being...)', p, mb_OK);
      ComboHydraulic.ItemIndex := Temp2HorizonVars.HydraulicPropertiesIdx;
   end;
   Temp2HorizonVars.HydraulicPropertiesIdx := ComboHydraulic.ItemIndex;
   BtnHydraulicUpdate;
   Temp2HorizonVars.Dirty := TRUE;
end;

procedure TFormHorizon.ComboSoluteChange(Sender: TObject);
begin
   Temp2HorizonVars.SoluteDiffusiveIdx := ComboSolute.ItemIndex;
   BtnSoluteUpdate;
   Temp2HorizonVars.Dirty := TRUE;
end;

(* Moved to profile 060398
procedure TFormHorizon.ComboAbsorptionChange(Sender: TObject);
begin
   Temp2HorizonVars.AbsorptionIsotermIdx := ComboAbsorption.ItemIndex;
   BtnAbsorptionUpdate;
end;
*)

procedure TFormHorizon.ComboThermalChange(Sender: TObject);
begin
   Temp2HorizonVars.ThermalPropertiesIdx := ComboThermal.ItemIndex;
   BtnThermalUpdate;
   Temp2HorizonVars.Dirty := TRUE;
end;

procedure TFormHorizon.BtnThermalClick(Sender: TObject);
begin
   Application.MessageBox('Not accessable (at time being...)', 'de Vries Model/Daisy v.1', mb_OK);
   // Temp2HorizonVars.Dirty := TRUE; { Form is now dirty! }
end;

procedure TFormHorizon.BtnHydraulicClick(Sender: TObject);
var
   x, y, i: Integer;
   s1, s2, s3, s4, s5: String;
   GoodNums: Boolean;
begin
   x := FormHorizon.Left + BtnHydraulic.Left;
   y := FormHorizon.Top + BtnHydraulic.Top;
   i := ComboHydraulic.ItemIndex;
   case i of
      0..1: begin
               InitHydraulic(x, y, ComboHydraulic.ItemIndex,
                             ComboHydraulic.Items[ComboHydraulic.ItemIndex] + ' Parameters',
                             FloatToStr(Temp2HorizonVars.SaturatedWaterContent),
                             FloatToStr(Temp2HorizonVars.ResidualWaterContent),
                             FloatToStr(Temp2HorizonVars.VanGenuchtenAlpha),
                             FloatToStr(Temp2HorizonVars.VanGenuchtenN),
                             FloatToStr(Temp2HorizonVars.SaturatedHydrCond));
               if FormHydraulic.ShowModal = mrOK then begin
                  ReadHydraul(s1, s2, s3, s4, s5);
                  GoodNums := TRUE;
                  try begin
                     StrToFloat(s1);
                     StrToFloat(s2);
                     StrToFloat(s3);
                     StrToFloat(s4);
                     StrToFloat(s5);
                  end
                  except on e: Exception do begin
                     MessageDlg('Error reading Hydraulic properties' + IntToStr(i) + '.'
                     + Chr(10) + Chr(13)
                     + 'Please report to program developer :-('
                     , mtError, [mbOK], 0);
                     GoodNums := FALSE;
                  end;
                  end; { try - except - end }
                  if GoodNums then begin
                     Temp2HorizonVars.SaturatedWaterContent := StrToFloat(s1);
                     Temp2HorizonVars.ResidualWaterContent  := StrToFloat(s2);
                     Temp2HorizonVars.VanGenuchtenAlpha     := StrToFloat(s3);
                     Temp2HorizonVars.VanGenuchtenN         := StrToFloat(s4);
                     Temp2HorizonVars.SaturatedHydrCond     := StrToFloat(s5);
                     Temp2HorizonVars.Dirty := TRUE; { Form is now dirty! }
                  end;
               end;
            end;
      2..4: begin
               InitHydraulic(x, y, ComboHydraulic.ItemIndex,
                             ComboHydraulic.Items[ComboHydraulic.ItemIndex] + ' Parameters',
                             FloatToStr(Temp2HorizonVars.SaturatedWaterContent),
                             FloatToStr(Temp2HorizonVars.ResidualWaterContent),
                             FloatToStr(Temp2HorizonVars.BublingPressure),
                             FloatToStr(Temp2HorizonVars.PoreSizeDistIdx),
                             FloatToStr(Temp2HorizonVars.SaturatedHydrCond));
               if FormHydraulic.ShowModal = mrOK then begin
                  ReadHydraul(s1, s2, s3, s4, s5);
                  GoodNums := TRUE;
                  try begin
                     StrToFloat(s1);
                     StrToFloat(s2);
                     StrToFloat(s3);
                     StrToFloat(s4);
                     StrToFloat(s5);
                  end
                  except on e: Exception do begin
                     MessageDlg('Error reading Hydraulic properties' + IntToStr(i) + '.'
                     + Chr(10) + Chr(13)
                     + 'Please report to program developer :-('
                     , mtError, [mbOK], 0);
                     GoodNums := FALSE;
                  end;
                  end; { try - except - end }
                  if GoodNums then begin
                     Temp2HorizonVars.SaturatedWaterContent := StrToFloat(s1);
                     Temp2HorizonVars.ResidualWaterContent  := StrToFloat(s2);
                     Temp2HorizonVars.BublingPressure       := StrToFloat(s3);
                     Temp2HorizonVars.PoreSizeDistIdx       := StrToFloat(s4);
                     Temp2HorizonVars.SaturatedHydrCond     := StrToFloat(s5);
                     Temp2HorizonVars.Dirty := TRUE; { Form is now dirty! }
                  end;
               end;
            end;
      5:    begin
               with OpenDlgHorizon do begin
                  DefaultExt := '';
                  FileName   := Temp2HorizonVars.TableDaisyV1File;
                  Filter     := '';
                  FilterIndex:= 1;
                  Title      := 'Hydraulic properties table - Daisy v.1';
                  if Execute then begin
                     Temp2HorizonVars.TableDaisyV1File := FileName;
                     Temp2HorizonVars.Dirty := TRUE; { Form is now dirty! }
               end;
               end;
            end;
      6:    begin
               { Not implemented yet... }
            end;
   end;
end;

procedure TFormHorizon.BtnSoluteClick(Sender: TObject);
var
   x, y: Integer;
   s1, s2: String;
begin
   x := FormHorizon.Left + BtnSolute.Left;
   y := FormHorizon.Top + BtnSolute.Top;
   with ComboSolute do begin
      case ItemIndex of
         1: begin
               InitTwoEdit(x, y, Items[ItemIndex],'Parameter A', '', 'Parameter B',
                           '', 1, 2, FloatToStr(Temp2HorizonVars.ParameterA),
                           FloatToStr(Temp2HorizonVars.ParameterB));
               SetTwoEditHelp(HLP_Parameter_A, HLP_Parameter_B,
                              HLP_SolDiffTransp_Linear_model_Daisy_v1);
               if FormTwoEdit.ShowModal = mrOK then begin
                  ReadTwoEdit(s1, s2);
                  if s1 = '' then
                     Temp2HorizonVars.ParameterA := NotSet
                  else
                     Temp2HorizonVars.ParameterA := StrToFloat(s1);
                  if s2 = '' then
                     Temp2HorizonVars.ParameterB := NotSet
                  else
                     Temp2HorizonVars.ParameterB := StrToFloat(s2);
                  Temp2HorizonVars.Dirty := TRUE; { Form is now dirty! }
               end; { if modalresult = mrOK }
            end; { 1: begin }
      end; { case ItemIndex of }
   end;
end;

procedure TFormHorizon.BtnHeadingCommentsClick(Sender: TObject);
var
   x, y: Integer;
begin
   x := FormHorizon.Left + BtnHeadingComments1.Left;
   y := FormHorizon.Top + BtnHeadingComments1.Top;
   InitComments(x, y, 'Horizon Comments', Temp2HorizonVars.HorizonComments);
   if FormComments.ShowModal = mrOK then begin
      ReadComments(Temp2HorizonVars.HorizonComments);
      Temp2HorizonVars.Dirty := TRUE;
      if Length(Temp2HorizonVars.HorizonComments) = 0 then begin  { Don't show finger!! }
         BtnHeadingComments2.Hide;
         BtnHeadingComments1.Show;
      end
      else begin
         BtnHeadingComments1.Hide;
         BtnHeadingComments2.Show;
      end;
   end;
end;

procedure TFormHorizon.EditHeading1Exit(Sender: TObject);
begin
   { It is allowed to leave the form at any time... }
   if (ActiveControl <> BtnCancel) then begin
      { Check if valid heading }
      if not CheckValidFields(TRUE) then
         exit;
      {- and then if OK...}
      Temp2HorizonVars.HorizonHeading1 := EditHeading1.Text;
   end;
end;

procedure TFormHorizon.EditHeading2Exit(Sender: TObject);
begin
   { It is allowed to leave the form at any time... }
   if (ActiveControl <> BtnCancel) then begin
      { Check if valid heading }
      if not CheckValidFields(TRUE) then
         exit;
      {- and then if OK...}
      Temp2HorizonVars.HorizonHeading2 := EditHeading2.Text;
   end;
end;

procedure TFormHorizon.EditTextureExit(Sender: TObject);
var
   Deleted: Boolean;
   s: string;
begin
   { It is allowed to leave the form at any time... }
   if (ActiveControl <> BtnCancel) then begin
      { Check if valid field value }
      if not CheckValidFields(TRUE) then
         exit;
      {- and then if OK - Update the proper variable }
      with Sender as TEdit do begin
         Text := Trim(Text); { Get rid of leading/trailing spaces}
         Deleted := (Text = '');
         if not Deleted then
            s := FormatFloat('0', 1000 * StrToFloat(Text));

         if Name = 'EditOrganic' then
            if Deleted then begin
               Temp2HorizonVars.TextureOrganicFracSet := 0;
               Temp2HorizonVars.TextureOrganicFrac := NotSet;
            end
            else begin
               Temp2HorizonVars.TextureOrganicFracSet := 1;
               Temp2HorizonVars.TextureOrganicFrac := StrToInt(s);
            end;
         if Name = 'EditClay' then
            if Deleted then begin
               Temp2HorizonVars.TextureClayFracSet := 0;
               Temp2HorizonVars.TextureClayFrac := NotSet;
            end
            else begin
               Temp2HorizonVars.TextureClayFracSet := 2;
               Temp2HorizonVars.TextureClayFrac := StrToInt(s);
            end;
         if Name = 'EditSilt' then
            if Deleted then begin
               Temp2HorizonVars.TextureSiltFracSet := 0;
               Temp2HorizonVars.TextureSiltFrac := NotSet;
            end
            else begin
               Temp2HorizonVars.TextureSiltFracSet := 4;
               Temp2HorizonVars.TextureSiltFrac := StrToInt(s);
            end;
         if Name = 'EditFine' then
            if Deleted then begin
               Temp2HorizonVars.TextureFineFracSet := 0;
               Temp2HorizonVars.TextureFineFrac := NotSet;
            end
            else begin
               Temp2HorizonVars.TextureFineFracSet := 8;
               Temp2HorizonVars.TextureFineFrac := StrToInt(s);
            end;
         if Name = 'EditCoarse' then
            if Deleted then begin
               Temp2HorizonVars.TextureCoarseFracSet := 0;
               Temp2HorizonVars.TextureCoarseFrac := NotSet;
            end
            else begin
               Temp2HorizonVars.TextureCoarseFracSet := 16;
               Temp2HorizonVars.TextureCoarseFrac := StrToInt(s);
            end;
      end; { with Sender as TEdit do }
   end; { if (ActiveControl <> BtnCancel) then }
(*
   { It is allowed to leave the form at any time... }
   if (ActiveControl <> BtnCancel) then begin
      GoodFrac := TRUE;
      { ...but not to leave an invalid fraction in the edit box}
      with Sender as TEdit do begin
         Text := Trim(Text); { Get rid of leading/trailing spaces}
         if Text <> '' then begin
            Deleted := FALSE;
            try Text := StrTo3DecFracStr(Text);
            except
               on E: Exception do begin
                  x := Left + GBTexture.Left + FormHorizon.Left - 150;
                  y := Top + GBTexture.Top + FormHorizon.Top - 80;
                  MessageDlgPos('Not a valid fraction! ' +
                                'A fraction must be in the interval [0.0, 1.0],' +
                                ' using the systems national decimal point',
                                 mtInformation, [mbOK], 0, x, y);
                  SetFocus;
                  GoodFrac := FALSE;
               end;
            end;
         end  { if text <> '' }
         else
            Deleted := TRUE;

         if GoodFrac then begin
            if not Deleted then
               s := FormatFloat('0', 1000 * StrToFloat(Text));

            if Name = 'EditOrganic' then
               if Deleted then begin
                  Temp2HorizonVars.TextureOrganicFracSet := 0;
                  Temp2HorizonVars.TextureOrganicFrac := NotSet;
               end
               else begin
                  Temp2HorizonVars.TextureOrganicFracSet := 1;
                  Temp2HorizonVars.TextureOrganicFrac := StrToInt(s);
               end;
            if Name = 'EditClay' then
               if Deleted then begin
                  Temp2HorizonVars.TextureClayFracSet := 0;
                  Temp2HorizonVars.TextureClayFrac := NotSet;
               end
               else begin
                  Temp2HorizonVars.TextureClayFracSet := 2;
                  Temp2HorizonVars.TextureClayFrac := StrToInt(s);
               end;
            if Name = 'EditSilt' then
               if Deleted then begin
                  Temp2HorizonVars.TextureSiltFracSet := 0;
                  Temp2HorizonVars.TextureSiltFrac := NotSet;
               end
               else begin
                  Temp2HorizonVars.TextureSiltFracSet := 4;
                  Temp2HorizonVars.TextureSiltFrac := StrToInt(s);
               end;
            if Name = 'EditFine' then
               if Deleted then begin
                  Temp2HorizonVars.TextureFineFracSet := 0;
                  Temp2HorizonVars.TextureFineFrac := NotSet;
               end
               else begin
                  Temp2HorizonVars.TextureFineFracSet := 8;
                  Temp2HorizonVars.TextureFineFrac := StrToInt(s);
               end;
            if Name = 'EditCoarse' then
               if Deleted then begin
                  Temp2HorizonVars.TextureCoarseFracSet := 0;
                  Temp2HorizonVars.TextureCoarseFrac := NotSet;
               end
               else begin
                  Temp2HorizonVars.TextureCoarseFracSet := 16;
                  Temp2HorizonVars.TextureCoarseFrac := StrToInt(s);
               end;
         end;  { if GoodFrac }
      end;  { with Sender as TEdit }
   end;  { ActiveControl <> BtnCancel }
*)
end;

// The groupbox' OnExit event will be called AFTER the editbox' !!!
procedure TFormHorizon.GBTextureExit(Sender: TObject);
var
   MissingPtr: ^TEdit;
   Reminder, SetSum, Fractions: Integer;
   OkToLeave: Boolean;
   x, y: Integer;
begin
   { It is allowed to leave the form at any time... }
   if (ActiveControl <> BtnCancel) then begin
      x := GBTexture.Left + FormHorizon.Left - 170;
      y := GBTexture.Top + FormHorizon.Top + 130;
      MissingPtr := Nil;  { <> Nil if exactly one edit box is empty }
      OkToLeave := TRUE;  { Legal to let focus leave the group }

      SetVarsFromForm; { Just to be sure... }

      SetSum := Temp2HorizonVars.TextureOrganicFracSet
                  + Temp2HorizonVars.TextureClayFracSet
                  + Temp2HorizonVars.TextureSiltFracSet
                  + Temp2HorizonVars.TextureFineFracSet
                  + Temp2HorizonVars.TextureCoarseFracSet;

      Fractions := 0; { Fractions so far }
      if Temp2HorizonVars.TextureOrganicFrac <> NotSet then
         Fractions := Fractions + Temp2HorizonVars.TextureOrganicFrac;
      if Temp2HorizonVars.TextureClayFrac <> NotSet then
         Fractions := Fractions + Temp2HorizonVars.TextureClayFrac;
      if Temp2HorizonVars.TextureSiltFrac <> NotSet then
         Fractions := Fractions + Temp2HorizonVars.TextureSiltFrac;
      if Temp2HorizonVars.TextureFineFrac <> NotSet then
         Fractions := Fractions + Temp2HorizonVars.TextureFineFrac;
      if Temp2HorizonVars.TextureCoarseFrac <> NotSet then
         Fractions := Fractions + Temp2HorizonVars.TextureCoarseFrac;

      Reminder := 1000 - Fractions; { Current reminder }

      case (SetSum) of  { 4 or 5 fractions defined by the user }
         15:   MissingPtr := @EditCoarse;    { EditCoarse missing }
         23:   MissingPtr := @EditFine;      { EditFine missing }
         27:   MissingPtr := @EditSilt;      { EditSilt missing }
         29:   MissingPtr := @EditClay;      { EditClay missing }
         30:   MissingPtr := @EditOrganic;   { EditOrganic missing }
         31:   { Nothing's missing => check if fraction sum = 1 (1000) }
               if Fractions <> 1000 then begin
                  MessageDlgPos('The sum must equal 1. (Tip: Leave one box empty ' +
                                'when leaving the group, and it will be ' +
                                'calculated automatically.)',
                              mtInformation, [mbOK], 0, x, y);
                  OkToLeave := FALSE;
               end;
      end;
      if Reminder >= 0 then begin
         if MissingPtr <> Nil then begin
            MissingPtr^.Text := FormatFloat('0.000', Reminder / 1000);
            case (SetSum) of { Set the updated editbox as 'updated' }
               15:   begin
                        Temp2HorizonVars.TextureCoarseFracSet := 16;
                        Temp2HorizonVars.TextureCoarseFrac := reminder;
                     end;
               23:   begin
                        Temp2HorizonVars.TextureFineFracSet := 8;
                        Temp2HorizonVars.TextureFineFrac := reminder;
                     end;
               27:   begin
                        Temp2HorizonVars.TextureSiltFracSet := 4;
                        Temp2HorizonVars.TextureSiltFrac := reminder;
                     end;
               29:   begin
                        Temp2HorizonVars.TextureClayFracSet := 2;
                        Temp2HorizonVars.TextureClayFrac := reminder;
                     end;
               30:   begin
                        Temp2HorizonVars.TextureOrganicFracSet := 1;
                        Temp2HorizonVars.TextureOrganicFrac := reminder;
                     end;
            end;
         end;
      end
      else if SetSum <> 31 then begin { Problem already taken care of if SetSum = 31 }
         MessageDlgPos('The sum of all five fractions, must equal 1. At the ' +
                    'moment, the input fractions sums up to a value > 1.',
                     mtError, [mbOK], 0, x, y);
         OkToLeave := FALSE;
      end;
      if not OkToLeave then
         GBTexture.SetFocus;
   end; { if ActiveComtrol <> BtnCancel }
end;

procedure TFormHorizon.EditActiveExit(Sender: TObject);
(*
var
   x, y: Integer;
   GoodNum: Boolean;
*)
begin
   { It is allowed to leave the form at any time... }
   if (ActiveControl <> BtnCancel) then begin
      { Check if valid heading }
      if not CheckValidFields(TRUE) then
         exit;
      {- and then if OK...}
      if EditActive.Text = '' then
         Temp2HorizonVars.ActiveEasily := NotSet
      else
         Temp2HorizonVars.ActiveEasily := StrToFloat(EditActive.Text);
   end;
(*
   { It is allowed to leave the form at any time... }
   if (ActiveControl <> BtnCancel) then begin
      { ...but not to leave an invalid fraction in the edit box}
      with EditActive do begin
         Text := Trim(Text); { Get rid of leading/trailing spaces}
         if Text <> '' then begin
            GoodNum := TRUE;
            try Text := StrTo3DecFracStr(Text);
            except
               on E: Exception do begin
                  x := Left + GBSoil.Left + FormHorizon.Left - 250;
                  y := Top + GBSoil.Top + FormHorizon.Top - 80;
                  MessageDlgPos('Not a valid fraction! ' +
                                'A fraction must be in the interval [0.0, 1.0],' +
                                ' using the systems national decimal point',
                                 mtInformation, [mbOK], 0, x, y);
                  SetFocus;
                  GoodNum := FALSE;
               end;
            end;
            if GoodNum then  { Update variable... }
               Temp2HorizonVars.ActiveEasily := StrToFloat(Text);
         end;
      end;
   end;
*)
end;

procedure TFormHorizon.EditPassiveExit(Sender: TObject);
(*
var
   x, y: Integer;
   GoodNum: Boolean;
*)
begin
   { It is allowed to leave the form at any time... }
   if (ActiveControl <> BtnCancel) then begin
      { Check if valid heading }
      if not CheckValidFields(TRUE) then
         exit;
      {- and then if OK...}
      if EditPassive.Text = '' then
         Temp2HorizonVars.PassiveSlowly := NotSet
      else
         Temp2HorizonVars.PassiveSlowly := StrToFloat(EditPassive.Text);
   end;
(*
   { It is allowed to leave the form at any time... }
   if (ActiveControl <> BtnCancel) then begin
      { ...but not to leave an invalid fraction in the edit box}
      with EditPassive do begin
         Text := Trim(Text); { Get rid of leading/trailing spaces}
         if Text <> '' then begin
            GoodNum := TRUE;
            try Text := StrTo3DecFracStr(Text);
            except
               on E: Exception do begin
                  x := Left + GBSoil.Left + FormHorizon.Left - 250;
                  y := Top + GBSoil.Top + FormHorizon.Top - 80;
                  MessageDlgPos('Not a valid fraction! ' +
                                'A fraction must be in the interval [0.0, 1.0],' +
                                ' using the systems national decimal point',
                                 mtInformation, [mbOK], 0, x, y);
                  SetFocus;
                  GoodNum := FALSE;
               end;
            end;
            if GoodNum then  { Update variable... }
               Temp2HorizonVars.PassiveSlowly := StrToFloat(Text);
         end;
      end;
   end;
*)
end;

procedure TFormHorizon.EditCNPassiveExit(Sender: TObject);
(*
var
   x, y: Integer;
   tempvalue: Double;
   GoodNum: Boolean;
*)
begin
   { It is allowed to leave the form at any time... }
   if (ActiveControl <> BtnCancel) then begin
      { Check if valid fraction }
      if not CheckValidFields(TRUE) then
         exit;
      {- and then if OK...}
      if EditCNPassive.Text = '' then
         Temp2HorizonVars.CNofPassive := NotSet
      else
         Temp2HorizonVars.CNofPassive := StrToFloat(EditCNPassive.Text);
   end;
(*
   { It is allowed to leave the form at any time... }
   if (ActiveControl <> BtnCancel) then begin
      { ...but not to leave an invalid fraction in the edit box}
      with EditCNPassive do begin
         Text := Trim(Text); { Get rid of leading/trailing spaces}
         if Text <> '' then begin
            GoodNum := TRUE;
            x := Left + GBSoil.Left + FormHorizon.Left - 250;
            y := Top + GBSoil.Top + FormHorizon.Top - 80;
            try Text := StrTo1DecPosStr(Text);
            except
               on E: Exception do begin
                  MessageDlgPos('Not a valid positive floating point number!',
                                 mtInformation, [mbOK], 0, x, y);
                  SetFocus;
                  GoodNum := FALSE;
               end;
            end;
            if GoodNum then begin { check if variable is in [6.0 , 25.0] }
               tempvalue := StrToFloat(Text);
               if ((tempvalue <= 5.9999) or (tempvalue >= 25.0001)) then begin
                  // Silly comparization because of @£$2% Delphi that calcs
                  // tempvalue = 6.0 => (tempvalue < 6.0) = TRUE !!!
                  MessageDlgPos('The value for '
                                 + 'C/N of Passive/Slowly Decomposable'
                                 + ' must be in the interval [6.0, 25.0].',
                                 mtInformation, [mbOK], 0, x, y);
                  SetFocus;
               end
               else { Update variable... }
                  Temp2HorizonVars.CNofPassive := tempvalue;
            end;
         end;
      end;
   end;
*)
end;

procedure TFormHorizon.EditCNActiveExit(Sender: TObject);
(*
var
   x, y: Integer;
   tempvalue: Double;
   GoodNum: Boolean;
*)
begin
   { It is allowed to leave the form at any time... }
   if (ActiveControl <> BtnCancel) then begin
      { Check if valid fraction }
      if not CheckValidFields(TRUE) then
         exit;
      {- and then if OK...}
      if EditCNActive.Text = '' then
         Temp2HorizonVars.CNofActive := NotSet
      else
         Temp2HorizonVars.CNofActive := StrToFloat(EditCNActive.Text);
   end;
(*
   { It is allowed to leave the form at any time... }
   if (ActiveControl <> BtnCancel) then begin
      { ...but not to leave an invalid fraction in the edit box}
      with EditCNActive do begin
         Text := Trim(Text); { Get rid of leading/trailing spaces}
         if Text <> '' then begin
            GoodNum := TRUE;
            x := Left + GBSoil.Left + FormHorizon.Left - 250;
            y := Top + GBSoil.Top + FormHorizon.Top - 80;
            try Text := StrTo1DecPosStr(Text);
            except
               on E: Exception do begin
                  MessageDlgPos('Not a valid positive floating point number!',
                                 mtInformation, [mbOK], 0, x, y);
                  SetFocus;
                  GoodNum := FALSE;
               end;
            end;
            if GoodNum then begin { check if variable is in [6.0 , 25.0] }
               tempvalue := StrToFloat(Text);
               if ((tempvalue <= 5.9999) or (tempvalue >= 25.0001)) then begin
                  // Silly comparization because of @£$2% Delphi that calcs
                  // tempvalue = 6.0 => (tempvalue < 6.0) = TRUE !!!
                  MessageDlgPos('The value for '
                                 + 'C/N of Active/Easily Decomposable'
                                 + ' must be in the interval [6.0, 25.0].',
                                 mtInformation, [mbOK], 0, x, y);
                  SetFocus;
               end
               else { Update variable... }
                  Temp2HorizonVars.CNofActive := tempvalue;
            end;
         end;
      end;
   end;
*)
end;


procedure TFormHorizon.EditBulkExit(Sender: TObject);
(*
var
   x, y: Integer;
   tempvalue: Double;
   GoodNum: Boolean;
*)
begin
   { It is allowed to leave the form at any time... }
   if (ActiveControl <> BtnCancel) then begin
      { Check if valid values }
      if not CheckValidFields(TRUE) then
         exit;
      {- and then if OK...}
      if EditBulk.Text = '' then
         Temp2HorizonVars.DryBulk := NotSet
      else
         Temp2HorizonVars.DryBulk := StrToFloat(EditBulk.Text);
   end;
(*
   { It is allowed to leave the form at any time... }
   if (ActiveControl <> BtnCancel) then begin
      { ...but not to leave an invalid real number in the edit box }
      with EditBulk do begin
         Text := Trim(Text); { Get rid of leading/trailing spaces}
         if Text <> '' then begin
            GoodNum := TRUE;
            x := Left + FormHorizon.Left - 200;
            y := Top + FormHorizon.Top - 80;
            try Text := StrTo3DecPosStr(Text);
            except
               on E: Exception do begin
                  MessageDlgPos('Not a valid positive floating point number!',
                                 mtInformation, [mbOK], 0, x, y);
                  SetFocus;
                  GoodNum := FALSE;
               end;
            end;
            if GoodNum then begin { check if variable is in [0.7 , 2.6] }
               tempvalue := StrToFloat(Text);
               if ((tempvalue <= 0.6999) or (tempvalue >= 2.6001)) then begin
                  // Silly comparization because of @£$2% Delphi that calcs
                  // tempvalue = 0.7 => (tempvalue < 0.7) = TRUE !!!
                  MessageDlgPos('The value for '
                                 + 'Dry Bulk Density'
                                 + ' must be in the interval [0.7, 2.6].',
                                 mtInformation, [mbOK], 0, x, y);
                  SetFocus;
               end
               else { Update variable... }
                  Temp2HorizonVars.DryBulk := tempvalue;
            end;
         end;
      end;
   end;
*)
end;

procedure TFormHorizon.OpenHorizon1Click(Sender: TObject);
var
   TempHeading: String;
   DoLoad: Boolean;
begin
   DoLoad := TRUE;
   if Temp2HorizonVars.Dirty then
      if MessageDlg('Loading a new Horizon template will cause all unsaved data to be lost.'
                  + CHR(10) + CHR(13)
                  + 'Select <OK> to load a new Horizon template, '
                  + 'or <Cancel> to regret.',
                  mtWarning, [mbOK, mbCancel], 0) <> mrOK then
         DoLoad := FALSE;
   if DoLoad then begin
      with FormSelFromLib do begin
         Init(3, 'Horizon');
         Left := FormHorizon.Left - 20;
         Top := FormHorizon.Top + 50;
         ShowModal;
         if ModalResult = mrOK then begin { Something IS selected in the list! }

            TempHeading := LBSelFromLib.Items[LBSelFromLib.ItemIndex];
            Temp2HorizonVars.Load(TempHeading, 0); { Load vars from user templ lib }
            FormHorizon.SetFormFromVars; { Set visible components on form}
            FormHorizon.Update; { and update just to be sure ;-) }
            Temp2HorizonVars.Dirty := FALSE;
         end;
      end;
   end;
end;

procedure TFormHorizon.SaveHorizon1Click(Sender: TObject);
var
   SaveToLib, SelLibStr, HorName: String;
   ValidName: Integer;
   EditMode: Boolean;
begin
   { Check all fields for valid values first... }
   if not CheckValidFields(TRUE) then
      exit;
   { Update might also have been done earlier (on exit from edit-boxes etc.), but ... }

   SetVarsFromForm;

   { Test if previously saved... }
   case Temp2HorizonVars.CurrentSaveLib of
      1: begin
            SelLibStr := MainFormVars.UserRefLib;    { User Ref }
            SaveToLib := LoadStr(RES_MSC_UsrRef);
         end;
      3: begin
            SelLibStr := MainFormVars.UserTemplLib;  { User Templ }
            SaveToLib := LoadStr(RES_MSC_UsrTempl);
         end;
   else
      Temp2HorizonVars.CurrentSaveLib := -1;  // Just to be safe...
   end;

   if Temp2HorizonVars.CurrentSaveLib < 0 then  // Not saved yet...
      SaveHorizonAs1Click(Sender)
   else begin
      ValidName := MkHorizonName(Temp2HorizonVars.HorizonHeading1,
                                 Temp2HorizonVars.HorizonHeading2,
                                 HorName);
      if (ValidName = 0) then begin
         EditMode := not EditHeading1.Enabled;
         { Check that no other Horizons exists with the same name (heading) }
         { (done in .Save) }
         if Temp2HorizonVars.Save(HorName, 0, EditMode) then begin
            MainFormVars.UpdateLib(Temp2HorizonVars.CurrentSaveLib, 3); { user templ hor lib }
            { Make the saving visible to the user... }
            MessageDlg('Horizon ' + HorName
                        + ' successfully saved to the ' + SaveToLib + '.',
                        mtInformation, [mbOK], 0);
            Temp2HorizonVars.Dirty := FALSE; { Not dirty anymore... }
         end
      end
      else begin
         { Prompt for heading }
         MessageDlgPos('A valid heading must be entered before saving.',
                        mtInformation, [mbOK], 0
                        , FormHorizon.Left - 20
                        , FormHorizon.Top + 50);
         if ValidName = 1 then
            EditHeading1.SetFocus
         else
            EditHeading2.SetFocus;
      end;
   end;
end;

procedure TFormHorizon.SaveHorizonAs1Click(Sender: TObject);
var
   HorName, SelLibStr, SaveToLib: String;
   SelLib, OldSelLib: Integer;
begin
   { Check all fields for valid values first... }
   if not CheckValidFields(TRUE) then
      exit;
   { Update might also have been done earlier (on exit from edit-boxes etc.), but ... }
   SetVarsFromForm;

   { Now get the save-as name... }
   FormSaveAs.Init(2, Left+30, Top+40);
   if FormSaveAs.ShowModal = mrOK then begin
      FormSaveAs.Read(HorName, SelLib);
      OldSelLib := Temp2HorizonVars.CurrentSaveLib;
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

      { Check that no other Horizons exists with the same name (heading) }
      { (done in .Save) }
      Temp2HorizonVars.CurrentSaveLib := SelLib; // Is used in Save()
      if Temp2HorizonVars.Save(HorName, 0, FALSE) then begin
         MainFormVars.UpdateLib(SelLib, 3); { user templ hor lib }
         { Make the saving visible to the user... }
         MessageDlg('Horizon ' + HorName
                     + ' successfully saved to the ' + SaveToLib + '.',
                     mtInformation, [mbOK], 0);
      end
      else
         Temp2HorizonVars.CurrentSaveLib := OldSelLib;
   end;
end;

procedure TFormHorizon.GBSoilExit(Sender: TObject);
var
   x, y: Integer;
//   f: Double;
//   PassControl: Boolean; { Pass control to the editbox's OnExit event }
begin
   { It is allowed to leave the form at any time... }
   if (ActiveControl <> BtnCancel) then begin
      { Check if the sum of Passive + Active Decomp. is <= 1 }
      if (EditPassive.Text <> '') and (EditActive.Text <> '') then begin
         { Everythig is checked here (OnEditXXExit), so just convert strtofloat safely... }
         if (StrToFloat(EditPassive.Text) + StrToFloat(EditActive.Text) > 1) then begin
            x := GBSoil.Left + FormHorizon.Left;
            y := GBSoil.Top + FormHorizon.Top - 90;
            MessageDlgPos(GBSoil.Caption + ':' + CHR(10) + CHR(13)
                          + 'The sum of ' + LblPassive.Caption + ' and '
                          + LblActive.Caption + ' must be <= 1.',
                          mtInformation, [mbOK], 0, x, y);
            GBSoil.SetFocus;
         end;
      end;
(*
      if (EditPassive.Text <> '') and (EditActive.Text <> '') then begin
         f := 0;
         PassControl := FALSE;
         try f := StrToFloat(EditPassive.Text);
         except
            on E: Exception do begin
               PassControl := TRUE;
            end;
         end;
         try f := f + StrToFloat(EditActive.Text);
         except
            on E: Exception do begin
               PassControl := TRUE;
            end;
         end;
         { If nothing is wrong with the contents of the editboxes, then check the sum }
         if (not PassControl) and (f > 1) then begin
            x := GBSoil.Left + FormHorizon.Left;
            y := GBSoil.Top + FormHorizon.Top;
            MessageDlgPos('The sum of ' + LblPassive.Caption + ' and '
                           + LblActive.Caption + ' must be <= 1.',
                           mtError, [mbOK], 0, x, y);
            EditPassive.SetFocus;
         end;
      end;
*)
   end;
end;

procedure TFormHorizon.BtnOKClick(Sender: TObject);
var
   horname: String;
   missing: Integer;
   EditMode: Boolean;
begin
   { Check all fields now! }
   if not CheckValidFields(TRUE) then
      exit;
   { Then update all variables to be safe. }
   SetVarsFromForm;

   with Temp2HorizonVars do begin
      missing := MkHorizonName(HorizonHeading1, HorizonHeading2, horname);
      if missing = 0 then begin
         EditMode := not EditHeading1.Enabled;
         { Check that no other Horizons exists with the same name (heading) }
         { (done in .Save) }
         if Save(horname, 1, EditMode) then begin
            MainFormVars.UpdateLib(4, 3); { Sim spec hor lib }
            { ...and then it's ok }
            ModalResult := mrOK;
         end
         else
            ModalResult := mrNone; // Should not be neccesary :-(
      end
      else begin
         { Prompt for heading }
         MessageDlgPos('A valid heading must be entered before selecting <OK>.',
                     mtInformation, [mbOK], 0
                     , FormHorizon.Left - 20
                     , FormHorizon.Top + 50);
         if missing = 1 then
            EditHeading1.SetFocus
         else
            EditHeading2.SetFocus;
         ModalResult := mrNone; // Should not be neccesary :-(
      end;
   end;
end;

procedure TFormHorizon.NewHorizon1Click(Sender: TObject);
var
   DoReset: Boolean;
begin
   DoReset := TRUE;
   if Temp2HorizonVars.Dirty then
      if MessageDlg('Clearing a horizon will cause all unsaved data to be lost.'
                  + CHR(10) + CHR(13)
                  + 'Select <OK> to make a new Horizon, '
                  + 'or <Cancel> to regret.',
                  mtWarning, [mbOK, mbCancel], 0) <> mrOK then
         DoReset := FALSE;
   if DoReset then begin
      Temp2HorizonVars.Reset;
      Temp2HorizonVars.LoadDefault;
      FormHorizon.Caption := 'New Horizon - ';
      FormHorizon.SetFormFromVars;
      FormHorizon.Update;
      Temp2HorizonVars.Dirty := FALSE;
   end;
end;

procedure TFormHorizon.About1Click(Sender: TObject);
begin
   FormAbout.ShowModal;
end;

procedure TFormHorizon.BtnHelpClick(Sender: TObject);
begin
   Application.HelpContext(HLP_Soil_Horizon);
end;

procedure TFormHorizon.Contents1Click(Sender: TObject);
begin
   Application.HelpCommand(HELP_TAB, 0);
end;

procedure TFormHorizon.SearchforHelpOn1Click(Sender: TObject);
begin
   Application.HelpCommand(HELP_PARTIALKEY, HelpEmptyKey);
end;

procedure TFormHorizon.HowtoUseHelp1Click(Sender: TObject);
begin
   Application.HelpCommand(HELP_HELPONHELP, 0);
end;

procedure TFormHorizon.EditHeading1Change(Sender: TObject);
begin
   Temp2HorizonVars.Dirty := TRUE;
end;

procedure TFormHorizon.EditHeading2Change(Sender: TObject);
begin
   Temp2HorizonVars.Dirty := TRUE;
end;

procedure TFormHorizon.EditOrganicChange(Sender: TObject);
begin
   Temp2HorizonVars.Dirty := TRUE;
end;

procedure TFormHorizon.EditClayChange(Sender: TObject);
begin
   Temp2HorizonVars.Dirty := TRUE;
end;

procedure TFormHorizon.EditSiltChange(Sender: TObject);
begin
   Temp2HorizonVars.Dirty := TRUE;
end;

procedure TFormHorizon.EditFineChange(Sender: TObject);
begin
   Temp2HorizonVars.Dirty := TRUE;
end;

procedure TFormHorizon.EditCoarseChange(Sender: TObject);
begin
   Temp2HorizonVars.Dirty := TRUE;
end;

procedure TFormHorizon.EditBulkChange(Sender: TObject);
begin
   Temp2HorizonVars.Dirty := TRUE;
end;

procedure TFormHorizon.EditPassiveChange(Sender: TObject);
begin
   Temp2HorizonVars.Dirty := TRUE;
end;

procedure TFormHorizon.EditActiveChange(Sender: TObject);
begin
   Temp2HorizonVars.Dirty := TRUE;
end;

procedure TFormHorizon.EditCNPassiveChange(Sender: TObject);
begin
   Temp2HorizonVars.Dirty := TRUE;
end;

procedure TFormHorizon.EditCNActiveChange(Sender: TObject);
begin
   Temp2HorizonVars.Dirty := TRUE;
end;

end.
