unit globinfo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, Menus, Globals, About, LibComp, DHeading, UseLib, TypInfo,
  Mask;

type
  TFormGlobalInfo = class(TForm)
    GBGeo: TGroupBox;
    LblLatitude: TLabel;
    EditLatitude: TEdit;
    LblLatitudeUnits: TLabel;
    EditElevation: TEdit;
    LblElevation: TLabel;
    LblElevationUnits: TLabel;
    EditUTM1: TEdit;
    LblUTM: TLabel;
    LblUTMUnits: TLabel;
    EditUTM2: TEdit;
    GBTemp: TGroupBox;
    LblAvTmp: TLabel;
    LblAvTmpUnits: TLabel;
    LblAvTmpAmp: TLabel;
    LblAvTmpAmpUnits: TLabel;
    LblDateMaxTmp: TLabel;
    LblDateMaxTmpUnits: TLabel;
    EditAvTmp: TEdit;
    EditAvTmpAmp: TEdit;
    GBAtmospher: TGroupBox;
    LblNH4Conc: TLabel;
    LblNH4ConcUnits: TLabel;
    LblNH4Dry: TLabel;
    LblNH4DryUnits: TLabel;
    LblNO3Conc: TLabel;
    LblNO3ConcUnits: TLabel;
    EditNH4Conc: TEdit;
    EditNH4Dry: TEdit;
    EditNO3Conc: TEdit;
    LblNO3Dry: TLabel;
    EditNO3Dry: TEdit;
    LblNO3DryUnits: TLabel;
    BtnHelp: TBitBtn;
    BtnCancel: TBitBtn;
    BtnOK: TBitBtn;
    GBMet: TGroupBox;
    LblMetFile: TLabel;
    EditMetFile: TEdit;
    SpeedButton1: TSpeedButton;
    GlobInfoMenu1: TMainMenu;
    File1: TMenuItem;
    NewGlobalInfoFile1: TMenuItem;
    LoadGlobalInfoFile1: TMenuItem;
    SaveGlobalInfoFile1: TMenuItem;
    Close1: TMenuItem;
    N1: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    HowtoUseHelp1: TMenuItem;
    SearchforHelpOn1: TMenuItem;
    Contents1: TMenuItem;
    N2: TMenuItem;
    EditHeading: TEdit;
    LblHeading: TLabel;
    OpenDlgGlobalInfo: TOpenDialog;
    MEditDateMaxTmp: TMaskEdit;
    SaveGlobalInfoAs1: TMenuItem;
    procedure SpeedButton1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure NewGlobalInfoFile1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);
    procedure SaveGlobalInfoFile1Click(Sender: TObject);
    procedure LoadGlobalInfoFile1Click(Sender: TObject);
    procedure Close1Click(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    procedure About1Click(Sender: TObject);

    procedure SetFormFromVars;
    procedure SetVarsFromForm;
//    procedure EditFloatExit(Sender: TObject);
    procedure EditDateMaxTmpExit(Sender: TObject);
    procedure EditHeadingExit(Sender: TObject);
    function CheckValidFields(msg: Boolean): Boolean;
    procedure EditLatitudeExit(Sender: TObject);
    procedure EditElevationExit(Sender: TObject);
    procedure EditUTM1Exit(Sender: TObject);
    procedure EditUTM2Exit(Sender: TObject);
    procedure EditAvTmpExit(Sender: TObject);
    procedure EditAvTmpAmpExit(Sender: TObject);
    procedure EditNH4ConcExit(Sender: TObject);
    procedure EditNH4DryExit(Sender: TObject);
    procedure EditNO3ConcExit(Sender: TObject);
    procedure EditNO3DryExit(Sender: TObject);
    procedure EditMetFileExit(Sender: TObject);
    procedure Contents1Click(Sender: TObject);
    procedure SearchforHelpOn1Click(Sender: TObject);
    procedure HowtoUseHelp1Click(Sender: TObject);
    procedure BtnHelpClick(Sender: TObject);
    procedure EditHeadingChange(Sender: TObject);
    procedure EditLatitudeChange(Sender: TObject);
    procedure EditElevationChange(Sender: TObject);
    procedure EditUTM1Change(Sender: TObject);
    procedure EditUTM2Change(Sender: TObject);
    procedure EditAvTmpChange(Sender: TObject);
    procedure EditAvTmpAmpChange(Sender: TObject);
    procedure MEditDateMaxTmpChange(Sender: TObject);
    procedure EditNH4ConcChange(Sender: TObject);
    procedure EditNH4DryChange(Sender: TObject);
    procedure EditNO3ConcChange(Sender: TObject);
    procedure EditNO3DryChange(Sender: TObject);
    procedure EditMetFileChange(Sender: TObject);
    procedure SaveGlobalInfoAs1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormGlobalInfo: TFormGlobalInfo;

implementation
uses SaveAs;

{$R *.DFM}


{ Check all input fields on the FORM for valid values             }
{ Return FALSE when first invalid found (and focus it)            }
{ msg=TRUE => show errormessages                                  }
function TFormGlobalInfo.CheckValidFields(msg: Boolean): Boolean;
var
   x, y: Integer;
   ErrFloat: String; // to be removed when resource file is made...
   Month, Day: String;
begin
   Result := FALSE; { Fear the worst... }
   ErrFloat := 'Not a valid floating point number!';
   { Fields that are allways ok, are commented out... }

//      EditHeading;

{ EditLatitude - Start check here }
   with EditLatitude do begin
      Text := Trim(Text); { Get rid of leading/trailing spaces}
      if Text <> '' then { '' is a valid value! }
         try StrToFloat(Text);
         except
            on E: Exception do begin
               if msg then begin
                  x := Left + GBGeo.Left + FormGlobalInfo.Left - 200;
                  y := Top + GBGeo.Top + FormGlobalInfo.Top - 80;
                  MessageDlgPos(LblLatitude.Caption + ':' + CHR(10) + CHR(13)
                                + ErrFloat, mtInformation, [mbOK], 0, x, y);
               end;
               SetFocus;
               exit;
            end;
         end;
   end;
{ EditLatitude - End check here }

{ EditElevation - Start check here }
   with EditElevation do begin
      Text := Trim(Text); { Get rid of leading/trailing spaces}
      if Text <> '' then { '' is a valid value! }
         try StrToFloat(Text);
         except
            on E: Exception do begin
               if msg then begin
                  x := Left + GBGeo.Left + FormGlobalInfo.Left - 200;
                  y := Top + GBGeo.Top + FormGlobalInfo.Top - 80;
                  MessageDlgPos(LblElevation.Caption + ':' + CHR(10) + CHR(13)
                                + ErrFloat, mtInformation, [mbOK], 0, x, y);
               end;
               SetFocus;
               exit;
            end;
         end;
   end;
{ EditElevation - End check here }

{ EditUTM1 - Start check here }
   with EditUTM1 do begin
      Text := Trim(Text); { Get rid of leading/trailing spaces}
      if Text <> '' then { '' is a valid value! }
         try StrToFloat(Text);
         except
            on E: Exception do begin
               if msg then begin
                  x := Left + GBGeo.Left + FormGlobalInfo.Left - 200;
                  y := Top + GBGeo.Top + FormGlobalInfo.Top - 80;
                  MessageDlgPos(LblUTM.Caption + ':' + CHR(10) + CHR(13)
                                + ErrFloat, mtInformation, [mbOK], 0, x, y);
               end;
               SetFocus;
               exit;
            end;
         end;
   end;
{ EditUTM1 - End check here }

{ EditUTM2 - Start check here }
   with EditUTM2 do begin
      Text := Trim(Text); { Get rid of leading/trailing spaces}
      if Text <> '' then { '' is a valid value! }
         try StrToFloat(Text);
         except
            on E: Exception do begin
               if msg then begin
                  x := Left + GBGeo.Left + FormGlobalInfo.Left - 200;
                  y := Top + GBGeo.Top + FormGlobalInfo.Top - 80;
                  MessageDlgPos(LblUTM.Caption + ':' + CHR(10) + CHR(13)
                                + ErrFloat, mtInformation, [mbOK], 0, x, y);
               end;
               SetFocus;
               exit;
            end;
         end;
   end;
{ EditUTM2 - End check here }

{ EditAvTmp - Start check here }
   with EditAvTmp do begin
      Text := Trim(Text); { Get rid of leading/trailing spaces}
      if Text <> '' then { '' is a valid value! }
         try StrToFloat(Text);
         except
            on E: Exception do begin
               if msg then begin
                  x := Left + GBTemp.Left + FormGlobalInfo.Left - 200;
                  y := Top + GBTemp.Top + FormGlobalInfo.Top - 80;
                  MessageDlgPos(LblAvTmp.Caption + ':' + CHR(10) + CHR(13)
                                + ErrFloat, mtInformation, [mbOK], 0, x, y);
               end;
               SetFocus;
               exit;
            end;
         end;
   end;
{ EditAvTmp - End check here }

{ EditAvTmpAmp - Start check here }
   with EditAvTmpAmp do begin
      Text := Trim(Text); { Get rid of leading/trailing spaces}
      if Text <> '' then { '' is a valid value! }
         try StrToFloat(Text);
         except
            on E: Exception do begin
               if msg then begin
                  x := Left + GBTemp.Left + FormGlobalInfo.Left - 200;
                  y := Top + GBTemp.Top + FormGlobalInfo.Top - 80;
                  MessageDlgPos(LblAvTmpAmp.Caption + ':' + CHR(10) + CHR(13)
                                + ErrFloat, mtInformation, [mbOK], 0, x, y);
               end;
               SetFocus;
               exit;
            end;
         end;
   end;
{ EditAvTmpAmp - End check here }

{ MEditDateMaxTmp - Start check here }
   with MEditDateMaxTmp do begin
      Month := Trim(Copy(Text, 1, 2));
      Day   := Trim(Copy(Text, 4, 2));
      x := Left + GBTemp.Left + FormGlobalInfo.Left - 200;
      y := Top + GBTemp.Top + FormGlobalInfo.Top - 100;

      if (Month <> '') or (Day <> '') then begin  // Both blank => ok
         if (Month = '') or (Day = '') then begin  // One blank => not valid
            MessageDlgPos(LblDateMaxTmp.Caption + ':'  + CHR(10) + CHR(13)
                           + 'Both the "Month" and the "Date" part must contain'
                           + ' a value, or both must be left blank.'
                           , mtInformation, [mbOK], 0, x, y);
            SetFocus;
            exit;
         end;
         if (StrToInt(Month) > 12) or (StrToInt(Month) < 1) or
            (StrToInt(Day) > 31) or (StrToInt(Day) < 1)then begin
            MessageDlgPos(LblDateMaxTmp.Caption + ':'  + CHR(10) + CHR(13)
                           + 'The "Month" or the "Date" part contain an invalid'
                           + ' value. Valid values are in the intervals [1,12]'
                           + ' and [1,31] respectively.'
                           , mtInformation, [mbOK], 0, x, y);
            SetFocus;
            exit;
         end;
         { OK, now format nicely... }
         if Length(Month) = 1 then
            Month := '0' + Month;
         if Length(Day) = 1 then
            Day := '0' + Day;
         EditText := Month + EditText[3] + Day;
      end;  { if (Month <> '') or (Day <> '') }
   end;
{ MEditDateMaxTmp - End check here }

{ EditNH4Conc - Start check here }
   with EditNH4Conc do begin
      Text := Trim(Text); { Get rid of leading/trailing spaces}
      if Text <> '' then { '' is a valid value! }
         try StrToFloat(Text);
         except
            on E: Exception do begin
               if msg then begin
                  x := Left + GBAtmospher.Left + FormGlobalInfo.Left - 200;
                  y := Top + GBAtmospher.Top + FormGlobalInfo.Top - 80;
                  MessageDlgPos(LblNH4Conc.Caption + ':' + CHR(10) + CHR(13)
                                + ErrFloat, mtInformation, [mbOK], 0, x, y);
               end;
               SetFocus;
               exit;
            end;
         end;
   end;
{ EditNH4Conc - End check here }

{ EditNH4Dry - Start check here }
   with EditNH4Dry do begin
      Text := Trim(Text); { Get rid of leading/trailing spaces}
      if Text <> '' then { '' is a valid value! }
         try StrToFloat(Text);
         except
            on E: Exception do begin
               if msg then begin
                  x := Left + GBAtmospher.Left + FormGlobalInfo.Left - 200;
                  y := Top + GBAtmospher.Top + FormGlobalInfo.Top - 80;
                  MessageDlgPos(LblNH4Dry.Caption + ':' + CHR(10) + CHR(13)
                                + ErrFloat, mtInformation, [mbOK], 0, x, y);
               end;
               SetFocus;
               exit;
            end;
         end;
   end;
{ EditNH4Dry - End check here }

{ EditNO3Conc - Start check here }
   with EditNO3Conc do begin
      Text := Trim(Text); { Get rid of leading/trailing spaces}
      if Text <> '' then { '' is a valid value! }
         try StrToFloat(Text);
         except
            on E: Exception do begin
               if msg then begin
                  x := Left + GBAtmospher.Left + FormGlobalInfo.Left - 200;
                  y := Top + GBAtmospher.Top + FormGlobalInfo.Top - 80;
                  MessageDlgPos(LblNO3Conc.Caption + ':' + CHR(10) + CHR(13)
                                + ErrFloat, mtInformation, [mbOK], 0, x, y);
               end;
               SetFocus;
               exit;
            end;
         end;
   end;
{ EditNO3Conc - End check here }

{ EditNO3Dry - Start check here }
   with EditNO3Dry do begin
      Text := Trim(Text); { Get rid of leading/trailing spaces}
      if Text <> '' then { '' is a valid value! }
         try StrToFloat(Text);
         except
            on E: Exception do begin
               if msg then begin
                  x := Left + GBAtmospher.Left + FormGlobalInfo.Left - 200;
                  y := Top + GBAtmospher.Top + FormGlobalInfo.Top - 80;
                  MessageDlgPos(LblNO3Dry.Caption + ':' + CHR(10) + CHR(13)
                                + ErrFloat, mtInformation, [mbOK], 0, x, y);
               end;
               SetFocus;
               exit;
            end;
         end;
   end;
{ EditNO3Dry - End check here }

{ EditMetFile - Start check here }
   with EditMetFile do begin
      Text := Trim(Text); { Get rid of leading/trailing spaces}
      {                   }
      { Check file?????   }
      {                   }
   end;
{ EditMetFile - End check here }

{ Everything Checked OK :-) }
   Result := TRUE;
end;


procedure TFormGlobalInfo.SetFormFromVars;
begin
   { Update the visible components on the form }
   Caption := 'Global Information and Controls';
      { Init Edit boxes... }
   EditHeading.Text := TempGlobalInfoVars.GlobalInfoHeading;

   if TempGlobalInfoVars.Latitude = NotSet then EditLatitude.Text := ''
   else EditLatitude.Text := FormatFloat('0.000', TempGlobalInfoVars.Latitude);

   if TempGlobalInfoVars.Elevation = NotSet then EditElevation.Text := ''
   else EditElevation.Text := FormatFloat('0.000', TempGlobalInfoVars.Elevation);

   if TempGlobalInfoVars.UTM1 = NotSet then EditUTM1.Text := ''
   else EditUTM1.Text := FormatFloat('0.000', TempGlobalInfoVars.UTM1);

   if TempGlobalInfoVars.UTM2 = NotSet then EditUTM2.Text := ''
   else EditUTM2.Text := FormatFloat('0.000', TempGlobalInfoVars.UTM2);

   if TempGlobalInfoVars.AvAnnTemp = NotSet then EditAvTmp.Text := ''
   else EditAvTmp.Text := FormatFloat('0.000', TempGlobalInfoVars.AvAnnTemp);

   if TempGlobalInfoVars.AvAnnTempAmp = NotSet then EditAvTmpAmp.Text := ''
   else EditAvTmpAmp.Text := FormatFloat('0.000', TempGlobalInfoVars.AvAnnTempAmp);

   if Length(TempGlobalInfoVars.DateMaxTemp) >= 4 then
      MEditDateMaxTmp.EditText := Copy(TempGlobalInfoVars.DateMaxTemp, 1, 2)
                                + MEditDateMaxTmp.EditText[3]
                                + Copy(TempGlobalInfoVars.DateMaxTemp, 3, 2)
   else
      MEditDateMaxTmp.EditText := '  ' + MEditDateMaxTmp.EditText[3] + '  ';

   if TempGlobalInfoVars.NH4ConcPrec = NotSet then EditNH4Conc.Text := ''
   else EditNH4Conc.Text := FormatFloat('0.000', TempGlobalInfoVars.NH4ConcPrec);

   if TempGlobalInfoVars.NH4DryDep = NotSet then EditNH4Dry.Text := ''
   else EditNH4Dry.Text := FormatFloat('0.000', TempGlobalInfoVars.NH4DryDep);

   if TempGlobalInfoVars.NO3ConcPrec = NotSet then EditNO3Conc.Text := ''
   else EditNO3Conc.Text := FormatFloat('0.000', TempGlobalInfoVars.NO3ConcPrec);

   if TempGlobalInfoVars.NO3DryDep = NotSet then EditNO3Dry.Text := ''
   else EditNO3Dry.Text := FormatFloat('0.000', TempGlobalInfoVars.NO3DryDep);

   EditMetFile.Text := TempGlobalInfoVars.MeteorologicalDataFile;
end;

procedure TFormGlobalInfo.SetVarsFromForm;
var
   s: String;
begin
   TempGlobalInfoVars.GlobalInfoHeading := Trim(EditHeading.Text);

   s := Trim(EditLatitude.Text);
   if s = '' then TempGlobalInfoVars.Latitude := NotSet
   else TempGlobalInfoVars.Latitude := StrToFloat(s);

   s := Trim(EditElevation.Text);
   if s = '' then TempGlobalInfoVars.Elevation := NotSet
   else TempGlobalInfoVars.Elevation := StrToFloat(s);

   s := Trim(EditUTM1.Text);
   if s = '' then TempGlobalInfoVars.UTM1 := NotSet
   else TempGlobalInfoVars.UTM1 := StrToFloat(s);

   s := Trim(EditUTM2.Text);
   if s = '' then TempGlobalInfoVars.UTM2 := NotSet
   else TempGlobalInfoVars.UTM2 := StrToFloat(s);

   s := Trim(EditAvTmp.Text);
   if s = '' then TempGlobalInfoVars.AvAnnTemp := NotSet
   else TempGlobalInfoVars.AvAnnTemp := StrToFloat(s);

   s := Trim(EditAvTmpAmp.Text);
   if s = '' then TempGlobalInfoVars.AvAnnTempAmp := NotSet
   else TempGlobalInfoVars.AvAnnTempAmp := StrToFloat(s);

   if Trim(Copy(MEditDateMaxTmp.EditText, 1, 2)) = '' then
      TempGlobalInfoVars.DateMaxTemp := ''
   else
      TempGlobalInfoVars.DateMaxTemp := Copy(MEditDateMaxTmp.EditText, 1, 2)
                                      + Copy(MEditDateMaxTmp.EditText, 4, 2);

   s := Trim(EditNH4Conc.Text);
   if s = '' then TempGlobalInfoVars.NH4ConcPrec := NotSet
   else TempGlobalInfoVars.NH4ConcPrec := StrToFloat(s);

   s := Trim(EditNH4Dry.Text);
   if s = '' then TempGlobalInfoVars.NH4DryDep := NotSet
   else TempGlobalInfoVars.NH4DryDep := StrToFloat(s);

   s := Trim(EditNO3Conc.Text);
   if s = '' then TempGlobalInfoVars.NO3ConcPrec := NotSet
   else TempGlobalInfoVars.NO3ConcPrec := StrToFloat(s);

   s := Trim(EditNO3Dry.Text);
   if s = '' then TempGlobalInfoVars.NO3DryDep := NotSet
   else TempGlobalInfoVars.NO3DryDep := StrToFloat(s);

   TempGlobalInfoVars.MeteorologicalDataFile := Trim(EditMetFile.Text);
end;

procedure TFormGlobalInfo.SpeedButton1Click(Sender: TObject);
begin
   with OpenDlgGlobalInfo do begin
      DefaultExt := 'met';
      Filter     := '';
      FilterIndex:= 1;
      Title      := 'Select Meteorological Data File';
      FileName   := TempGlobalInfoVars.MeteorologicalDataFile;
      if Execute then begin
         TempGlobalInfoVars.MeteorologicalDataFile := FileName;
         EditMetFile.Text := TempGlobalInfoVars.MeteorologicalDataFile;
      end;
   end;
end;

procedure TFormGlobalInfo.FormCreate(Sender: TObject);
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
   with FormGlobalInfo do begin  { Init all the strings here... }
         { Init Labels... }
      LblHeading.Caption := 'Global Information Heading';
      LblLatitude.Caption := 'Latitude (+ : N.hemisphere, - : S.hemisphere)';
      LblLatitudeUnits.Caption := '(degrees, minutes)';
      LblElevation.Caption := 'Elevation';
      LblElevationUnits.Caption := '(meters above sea level)';
      LblUTM.Caption := 'UTM-coordinates or Grid coordinates';
      LblUTMUnits.Caption := '';
      LblAvTmp.Caption := 'Average Annual Temperature';
      LblAvTmpUnits.Caption := '(C)';
      LblAvTmpAmp.Caption := 'Average Annual Temperature Amplitude';
      LblAvTmpAmpUnits.Caption := '(C)';
      LblDateMaxTmp.Caption := 'Date of Maximum Temperature';
      LblDateMaxTmpUnits.Caption := '(month - day)';
      LblNH4Conc.Caption := 'NH4 - Concentration in Precipitation';
      LblNH4ConcUnits.Caption := '(ppm)';
      LblNH4Dry.Caption := 'NH4 - Dry deposition';
      LblNH4DryUnits.Caption := '(kg N/ha/day)';
      LblNO3Conc.Caption := 'NO3 - Concentration in Precipitation';
      LblNO3ConcUnits.Caption := '(ppm)';
      LblNO3Dry.Caption := 'NO3 - Dry Deposition';
      LblNO3DryUnits.Caption := '(kg N/ha/day)';
      LblMetFile.Caption := 'Meteorological Data File';
         { Init Buttons... }
      BtnOK.Caption := 'OK';
      BtnCancel.Caption := 'Cancel';
      BtnHelp.Caption := 'Help';
         { Init Group Boxes... }
      GBGeo.Caption := 'Geographical Location';
      GBTemp.Caption := 'Temperature Regime';
      GBAtmospher.Caption := 'Atmospheric Deposition';
      GBMet.Caption := 'Meteorological Data';
         { Init Menues... }
      with GlobInfoMenu1 do begin
         File1.Caption := 'File';
         NewGlobalInfoFile1.Caption := 'Clear Global Info';
         LoadGlobalInfoFile1.Caption := 'Load Global Info...';
         SaveGlobalInfoFile1.Caption := 'Save Global Info...';
         Close1.Caption := 'Close';
         Help1.Caption := 'Help';
         Contents1.Caption := 'Contents';
         SearchforHelpOn1.Caption := 'Search for Help On...';
         HowtoUseHelp1.Caption := 'How to Use Help';
         About1.Caption := 'About';
      end;
         { Init HelpContexts... }
      EditHeading.HelpContext := HLP_Global_Information_Heading;
      GBGeo.HelpContext := HLP_Geographical_Location;
      EditLatitude.HelpContext := HLP_Geographical_Location;
      EditElevation.HelpContext := HLP_Geographical_Location;
      EditUTM1.HelpContext := HLP_Geographical_Location;
      EditUTM2.HelpContext := HLP_Geographical_Location;
      GBTemp.HelpContext := HLP_Temperature_Regime;
      EditAvTmp.HelpContext := HLP_Temperature_Regime;
      EditAvTmpAmp.HelpContext := HLP_Temperature_Regime;
      MEditDateMaxTmp.HelpContext := HLP_Temperature_Regime;
      GBAtmospher.HelpContext := HLP_Atmospheric_Deposition;
      EditNH4Conc.HelpContext := HLP_Atmospheric_Deposition;
      EditNH4Dry.HelpContext := HLP_Atmospheric_Deposition;
      EditNO3Conc.HelpContext := HLP_Atmospheric_Deposition;
      EditNO3Dry.HelpContext := HLP_Atmospheric_Deposition;
      GBMet.HelpContext := HLP_Meteorological_Data;
      EditMetFile.HelpContext := HLP_Meteorological_Data;
      BtnOK.HelpContext := HLP_Global_Information;
      BtnCancel.HelpContext := HLP_Global_Information;
      BtnHelp.HelpContext := HLP_Global_Information;
   end;
end;

procedure TFormGlobalInfo.FormShow(Sender: TObject);
begin
   SetFormFromVars;
   if Editheading.Enabled then begin
      EditHeading.SetFocus; { Make the form ready for entering data }
      SaveGlobalInfoFile1.Enabled := TRUE;
   end
   else begin
      EditLatitude.SetFocus; { if form shown in Edit-mode }
      SaveGlobalInfoFile1.Enabled := FALSE;
   end;
   Update;
   TempGlobalInfoVars.Dirty := FALSE;
end;

procedure TFormGlobalInfo.BtnOKClick(Sender: TObject);
var
   EditMode: Boolean;
begin
   { Check all fields now! }
   if not CheckValidFields(TRUE) then
      exit;
   { Then update all variables to be safe. }
   SetVarsFromForm;

   if TempGlobalInfoVars.GlobalInfoHeading <> '' then begin
      EditMode := not EditHeading.Enabled;
      { Check that no other Global Info exists with the same name (heading) }
      if TempGlobalInfoVars.Save(TempGlobalInfoVars.GlobalInfoHeading, 1, EditMode) then begin
         MainFormVars.UpdateLib(4, 1); { Sim spec globinfo lib }
         ModalResult := mrOK
      end
      else
         ModalResult := mrNone; // Should not be neccesary :-(
   end
   else begin
      { Prompt for heading }
      MessageDlgPos('A valid heading must be entered before selecting <OK>.',
                  mtInformation, [mbOK], 0
                  , FormGlobalInfo.Left - 20
                  , FormGlobalInfo.Top + 50);
      EditHeading.SetFocus;
      ModalResult := mrNone; // Should not be neccesary :-(
   end;
end;

procedure TFormGlobalInfo.NewGlobalInfoFile1Click(Sender: TObject);
var
   DoReset: Boolean;
begin
   DoReset := TRUE;
   if TempGlobalInfoVars.Dirty then
      if MessageDlgPos('Making a new Global Information Setup will caused all unsaved data to be lost.'
                  + CHR(10) + CHR(13)
                  + 'Select <OK> to make a new Global Information Setup, '
                  + 'or <Cancel> to regret.'
                  , mtWarning, [mbOK, mbCancel], 0
                  , FormGlobalInfo.Left - 20
                  , FormGlobalInfo.Top + 50) <> mrOK then
         DoReset := FALSE;
   if DoReset then begin
      TempGlobalInfovars.Reset;
      TempGlobalInfovars.LoadDefault; { Init to default values }
      FormGlobalInfo.SetFormFromVars;
      FormGlobalInfo.Update;
      TempGlobalInfoVars.Dirty := FALSE;
   end;
end;

procedure TFormGlobalInfo.SaveGlobalInfoFile1Click(Sender: TObject);
var
   SaveToLib, SelLibStr, GlobName: String;
   EditMode: Boolean;
begin
   { Check all fields for valid values first... }
   if not CheckValidFields(TRUE) then
      exit;

   { Update vars from form... (all checks should be done here)}
   { Update might also have been done earlier (on exit from edit-boxes etc.), but ... }
   SetVarsFromForm;

   { Test if previously saved... }
   case TempGlobalInfoVars.CurrentSaveLib of
      1: begin
            SelLibStr := MainFormVars.UserRefLib;    { User Ref }
            SaveToLib := LoadStr(RES_MSC_UsrRef);
         end;
      3: begin
            SelLibStr := MainFormVars.UserTemplLib;  { User Templ }
            SaveToLib := LoadStr(RES_MSC_UsrTempl);
         end;
   else
      TempGlobalInfoVars.CurrentSaveLib := -1;  // Just to be safe...
   end;

   if TempGlobalInfoVars.CurrentSaveLib < 0 then  // Not saved yet...
      SaveGlobalInfoAs1Click(Sender)
   else begin
      GlobName := TempGlobalInfoVars.GlobalInfoHeading;
      if (GlobName <> '') then begin { Valid Global Info Name ( more restrictions might come ) }
         EditMode := not EditHeading.Enabled;
         { Check that no other GlobalInfo exists with the same name (heading) }
         { (done in .Save) }
         if TempGlobalInfoVars.Save(GlobName, 0, EditMode) then begin
            MainFormVars.UpdateLib(TempGlobalInfoVars.CurrentSaveLib, 1);
            { Make the saving visible to the user... }
            MessageDlg('Global Information ' + GlobName
                        + ' successfully saved to the ' + SaveToLib + '.',
                        mtInformation, [mbOK], 0);
            TempGlobalInfoVars.Dirty := FALSE; { Not dirty anymore... }
         end
      end
      else begin
         { Prompt for heading }
         MessageDlgPos('A valid heading must be entered before saving.',
                        mtInformation, [mbOK], 0
                        , FormGlobalInfo.Left - 20
                        , FormGlobalInfo.Top + 50);
         EditHeading.SetFocus;
      end;
   end;
end;

procedure TFormGlobalInfo.SaveGlobalInfoAs1Click(Sender: TObject);
var
   GlobName, SelLibStr, SaveToLib: string;
   SelLib, OldSelLib: Integer;
begin
   { Check all fields for valid values first... }
   if not CheckValidFields(TRUE) then
      exit;

   { Update vars from form... (all checks should be done here)}
   { Update might also have been done earlier (on exit from edit-boxes etc.), but ... }
   SetVarsFromForm;

   { Now get the save-as name... }
   FormSaveAs.Init(1, Left+30, Top+40);
   if FormSaveAs.ShowModal = mrOK then begin
      FormSaveAs.Read(GlobName, SelLib);
      OldSelLib := TempGlobalInfoVars.CurrentSaveLib;
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
      { Check that no other GlobalInfo exists with the same name (heading) }
      { (done in .Save) }
      TempGlobalInfoVars.CurrentSaveLib := SelLib; // Is used in Save()
      if TempGlobalInfoVars.Save(GlobName, 0, FALSE) then begin
         MainFormVars.UpdateLib(SelLib, 1);
         { Make the saving visible to the user... }
         MessageDlg('Global Information ' + GlobName
                     + ' successfully saved to the ' + SaveToLib + '.',
                     mtInformation, [mbOK], 0);
      end
      else
         TempGlobalInfoVars.CurrentSaveLib := OldSelLib;
   end;
end;

procedure TFormGlobalInfo.LoadGlobalInfoFile1Click(Sender: TObject);
var
   TempHeading: String;
   DoLoad: Boolean;
begin
   DoLoad := TRUE;
   if TempGlobalInfoVars.Dirty then
      if MessageDlgPos('Loading a new Global Information Setup will caused all unsaved data to be lost.'
                  + CHR(10) + CHR(13)
                  + 'Select <OK> to load a new Global Information Setup, '
                  + 'or <Cancel> to regret.',
                  mtWarning, [mbOK, mbCancel], 0
                  , FormGlobalInfo.Left - 20
                  , FormGlobalInfo.Top + 50) <> mrOK then
         DoLoad := FALSE;
   if DoLoad then begin
      with FormSelFromLib do begin
         Init(1, 'Global Information');
         Left := FormGlobalInfo.Left - 20;
         Top := FormGlobalInfo.Top + 50;
         ShowModal;
         if ModalResult = mrOK then begin { Something IS selected in the list! }
            TempHeading := LBSelFromLib.Items[LBSelFromLib.ItemIndex];
            TempGlobalInfovars.Reset; { Reset values to "NotSet" }
//            TempGlobalInfovars.LoadDefault; { Initialize with default values }
            TempGlobalInfoVars.Load(TempHeading, 0); { Load vars from user templ lib }
            FormGlobalInfo.SetFormFromVars; { Set visible components on form}
            FormGlobalInfo.Update; { and update just to be sure ;-) }
            TempGlobalInfoVars.Dirty := FALSE;  { Not dirty anymore... }
         end;
      end;
   end;
end;

procedure TFormGlobalInfo.Close1Click(Sender: TObject);
begin
   BtnCancelClick(Sender);
end;

procedure TFormGlobalInfo.BtnCancelClick(Sender: TObject);
begin
   ModalResult := mrCancel;
end;

(*
procedure TFormGlobalInfo.EditFloatExit(Sender: TObject);
var
   x, y: Integer;
   GoodNum: Boolean;
begin
   { It is allowed to leave the form at any time... }
   if (ActiveControl <> BtnCancel) then begin
      { ...but not to leave an invalid real number in the edit box }
      with Sender as TEdit do begin
         Text := Trim(Text); { Get rid of leading/trailing spaces}
         if Text <> '' then begin
            GoodNum := TRUE;
            try StrToFloat(Text);
            except
               on E: Exception do begin
                  x := Left + FormGlobalInfo.Left - 200;
                  y := Top + FormGlobalInfo.Top - 80;
                  MessageDlgPos('Not a valid floating point number!',
                                 mtInformation, [mbOK], 0, x, y);
                  SetFocus;
                  GoodNum := FALSE;
               end;
            end;
            if GoodNum then begin  { Update variable... }
               if Name = 'EditLatitude' then TempGlobalInfoVars.Latitude := StrToFloat(Text)
               else if Name = 'EditElevation' then TempGlobalInfoVars.Elevation := StrToFloat(Text)
               else if Name = 'EditUTM1' then TempGlobalInfoVars.UTM1 := StrToFloat(Text)
               else if Name = 'EditUTM2' then TempGlobalInfoVars.UTM2 := StrToFloat(Text)
               else if Name = 'EditAvTmp' then TempGlobalInfoVars.AvAnnTemp := StrToFloat(Text)
               else if Name = 'EditAvTmpAmp' then TempGlobalInfoVars.AvAnnTempAmp := StrToFloat(Text)
               else if Name = 'EditNH4Conc' then TempGlobalInfoVars.NH4ConcPrec := StrToFloat(Text)
               else if Name = 'EditNH4Dry' then TempGlobalInfoVars.NH4DryDep := StrToFloat(Text)
               else if Name = 'EditNO3Conc' then TempGlobalInfoVars.NO3ConcPrec := StrToFloat(Text)
               else if Name = 'EditNO3Dry' then TempGlobalInfoVars.NO3DryDep := StrToFloat(Text);
            end;  { if GoodNum then begin }
         end;  { if Text <> '' }
      end;  { with Sender as TEdit do }
   end;  { if (ActiveControl <> BtnCancel) }
end;
*)

procedure TFormGlobalInfo.EditDateMaxTmpExit(Sender: TObject);
begin
   { It is allowed to leave the form at any time... }
   if (ActiveControl <> BtnCancel) then begin
      { Check if valid values }
      if not CheckValidFields(TRUE) then
         exit;
      {- and then if OK...}
   if Trim(Copy(MEditDateMaxTmp.EditText, 1, 2)) = '' then
      TempGlobalInfoVars.DateMaxTemp := ''
   else
      TempGlobalInfoVars.DateMaxTemp := Copy(MEditDateMaxTmp.EditText, 1, 2)
                                      + Copy(MEditDateMaxTmp.EditText, 4, 2);
   end;
end;

procedure TFormGlobalInfo.EditHeadingExit(Sender: TObject);
begin
   { It is allowed to leave the form at any time... }
   if (ActiveControl <> BtnCancel) then begin
      { Check if valid values }
      if not CheckValidFields(TRUE) then
         exit;
      {- and then if OK...}
      TempGlobalInfoVars.GlobalInfoHeading := Trim(EditHeading.Text);
   end;
end;

procedure TFormGlobalInfo.EditLatitudeExit(Sender: TObject);
begin
   { It is allowed to leave the form at any time... }
   if (ActiveControl <> BtnCancel) then begin
      { Check if valid values }
      if not CheckValidFields(TRUE) then
         exit;
      {- and then if OK...}
      if EditLatitude.Text = '' then
         TempGlobalInfoVars.Latitude := NotSet
      else
         TempGlobalInfoVars.Latitude := StrToFloat(EditLatitude.Text);
   end;
end;

procedure TFormGlobalInfo.EditElevationExit(Sender: TObject);
begin
   { It is allowed to leave the form at any time... }
   if (ActiveControl <> BtnCancel) then begin
      { Check if valid values }
      if not CheckValidFields(TRUE) then
         exit;
      {- and then if OK...}
      if EditElevation.Text = '' then
         TempGlobalInfoVars.Elevation := NotSet
      else
         TempGlobalInfoVars.Elevation := StrToFloat(EditElevation.Text);
   end;
end;

procedure TFormGlobalInfo.EditUTM1Exit(Sender: TObject);
begin
   { It is allowed to leave the form at any time... }
   if (ActiveControl <> BtnCancel) then begin
      { Check if valid values }
      if not CheckValidFields(TRUE) then
         exit;
      {- and then if OK...}
      if EditUTM1.Text = '' then
         TempGlobalInfoVars.UTM1 := NotSet
      else
         TempGlobalInfoVars.UTM1 := StrToFloat(EditUTM1.Text);
   end;
end;

procedure TFormGlobalInfo.EditUTM2Exit(Sender: TObject);
begin
   { It is allowed to leave the form at any time... }
   if (ActiveControl <> BtnCancel) then begin
      { Check if valid values }
      if not CheckValidFields(TRUE) then
         exit;
      {- and then if OK...}
      if EditUTM2.Text = '' then
         TempGlobalInfoVars.UTM2 := NotSet
      else
         TempGlobalInfoVars.UTM2 := StrToFloat(EditUTM2.Text);
   end;
end;

procedure TFormGlobalInfo.EditAvTmpExit(Sender: TObject);
begin
   { It is allowed to leave the form at any time... }
   if (ActiveControl <> BtnCancel) then begin
      { Check if valid values }
      if not CheckValidFields(TRUE) then
         exit;
      {- and then if OK...}
      if EditAvTmp.Text = '' then
         TempGlobalInfoVars.AvAnnTemp := NotSet
      else
         TempGlobalInfoVars.AvAnnTemp := StrToFloat(EditAvTmp.Text);
   end;
end;

procedure TFormGlobalInfo.EditAvTmpAmpExit(Sender: TObject);
begin
   { It is allowed to leave the form at any time... }
   if (ActiveControl <> BtnCancel) then begin
      { Check if valid values }
      if not CheckValidFields(TRUE) then
         exit;
      {- and then if OK...}
      if EditAvTmpAmp.Text = '' then
         TempGlobalInfoVars.AvAnnTempAmp := NotSet
      else
         TempGlobalInfoVars.AvAnnTempAmp := StrToFloat(EditAvTmpAmp.Text);
   end;
end;

procedure TFormGlobalInfo.EditNH4ConcExit(Sender: TObject);
begin
   { It is allowed to leave the form at any time... }
   if (ActiveControl <> BtnCancel) then begin
      { Check if valid values }
      if not CheckValidFields(TRUE) then
         exit;
      {- and then if OK...}
      if EditNH4Conc.Text = '' then
         TempGlobalInfoVars.NH4ConcPrec := NotSet
      else
         TempGlobalInfoVars.NH4ConcPrec := StrToFloat(EditNH4Conc.Text);
   end;
end;

procedure TFormGlobalInfo.EditNH4DryExit(Sender: TObject);
begin
   { It is allowed to leave the form at any time... }
   if (ActiveControl <> BtnCancel) then begin
      { Check if valid values }
      if not CheckValidFields(TRUE) then
         exit;
      {- and then if OK...}
      if EditNH4Dry.Text = '' then
         TempGlobalInfoVars.NH4DryDep := NotSet
      else
         TempGlobalInfoVars.NH4DryDep := StrToFloat(EditNH4Dry.Text);
   end;
end;

procedure TFormGlobalInfo.EditNO3ConcExit(Sender: TObject);
begin
   { It is allowed to leave the form at any time... }
   if (ActiveControl <> BtnCancel) then begin
      { Check if valid values }
      if not CheckValidFields(TRUE) then
         exit;
      {- and then if OK...}
      if EditNO3Conc.Text = '' then
         TempGlobalInfoVars.NO3ConcPrec := NotSet
      else
         TempGlobalInfoVars.NO3ConcPrec := StrToFloat(EditNO3Conc.Text);
   end;
end;

procedure TFormGlobalInfo.EditNO3DryExit(Sender: TObject);
begin
   { It is allowed to leave the form at any time... }
   if (ActiveControl <> BtnCancel) then begin
      { Check if valid values }
      if not CheckValidFields(TRUE) then
         exit;
      {- and then if OK...}
      if EditNO3Dry.Text = '' then
         TempGlobalInfoVars.NO3DryDep := NotSet
      else
         TempGlobalInfoVars.NO3DryDep := StrToFloat(EditNO3Dry.Text);
   end;
end;

procedure TFormGlobalInfo.EditMetFileExit(Sender: TObject);
begin
   { It is allowed to leave the form at any time... }
   if (ActiveControl <> BtnCancel) then begin
      { Check if valid values }
      if not CheckValidFields(TRUE) then
         exit;
      {- and then if OK...}
      TempGlobalInfoVars.MeteorologicalDataFile := EditMetFile.Text;
   end;
end;

procedure TFormGlobalInfo.About1Click(Sender: TObject);
begin
   FormAbout.ShowModal;
end;

procedure TFormGlobalInfo.Contents1Click(Sender: TObject);
begin
   Application.HelpCommand(HELP_TAB, 0);
end;

procedure TFormGlobalInfo.SearchforHelpOn1Click(Sender: TObject);
begin
   Application.HelpCommand(HELP_PARTIALKEY, HelpEmptyKey);
end;

procedure TFormGlobalInfo.HowtoUseHelp1Click(Sender: TObject);
begin
   Application.HelpCommand(HELP_HELPONHELP, 0);
end;

procedure TFormGlobalInfo.BtnHelpClick(Sender: TObject);
begin
   Application.HelpContext(HLP_Global_Information);
end;

procedure TFormGlobalInfo.EditHeadingChange(Sender: TObject);
begin
   TempGlobalInfoVars.Dirty := TRUE;
end;

procedure TFormGlobalInfo.EditLatitudeChange(Sender: TObject);
begin
   TempGlobalInfoVars.Dirty := TRUE;
end;

procedure TFormGlobalInfo.EditElevationChange(Sender: TObject);
begin
   TempGlobalInfoVars.Dirty := TRUE;
end;

procedure TFormGlobalInfo.EditUTM1Change(Sender: TObject);
begin
   TempGlobalInfoVars.Dirty := TRUE;
end;

procedure TFormGlobalInfo.EditUTM2Change(Sender: TObject);
begin
   TempGlobalInfoVars.Dirty := TRUE;
end;

procedure TFormGlobalInfo.EditAvTmpChange(Sender: TObject);
begin
   TempGlobalInfoVars.Dirty := TRUE;
end;

procedure TFormGlobalInfo.EditAvTmpAmpChange(Sender: TObject);
begin
   TempGlobalInfoVars.Dirty := TRUE;
end;

procedure TFormGlobalInfo.MEditDateMaxTmpChange(Sender: TObject);
begin
   TempGlobalInfoVars.Dirty := TRUE;
end;

procedure TFormGlobalInfo.EditNH4ConcChange(Sender: TObject);
begin
   TempGlobalInfoVars.Dirty := TRUE;
end;

procedure TFormGlobalInfo.EditNH4DryChange(Sender: TObject);
begin
   TempGlobalInfoVars.Dirty := TRUE;
end;

procedure TFormGlobalInfo.EditNO3ConcChange(Sender: TObject);
begin
   TempGlobalInfoVars.Dirty := TRUE;
end;

procedure TFormGlobalInfo.EditNO3DryChange(Sender: TObject);
begin
   TempGlobalInfoVars.Dirty := TRUE;
end;

procedure TFormGlobalInfo.EditMetFileChange(Sender: TObject);
begin
   TempGlobalInfoVars.Dirty := TRUE;
end;


end.
