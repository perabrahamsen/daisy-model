unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, Buttons, StdCtrls,  About, {SetLib,} ExtCtrls, IniFiles, TypInfo;

type
   TFormMainForm = class(TForm)
      LblHeading: TLabel;
      EditHeading: TEdit;
      LblInit: TLabel;
      ComboInit: TComboBox;
      BtnInit: TButton;
      BtnClose: TBitBtn;
      BtnHelp: TBitBtn;
      BtnStart: TBitBtn;
      MMSimulation: TMainMenu;
      File1: TMenuItem;
      LoadSimulation1: TMenuItem;
      SaveSimulation1: TMenuItem;
      N1: TMenuItem;
      Quit1: TMenuItem;
      OpenDlgSimulation: TOpenDialog;
      NewSimulation1: TMenuItem;
      Help1: TMenuItem;
      About1: TMenuItem;
      HowtoUseHelp1: TMenuItem;
      SearchforHelpOn1: TMenuItem;
      Contents1: TMenuItem;
      Libraries1: TMenuItem;
      StandardLibrary1: TMenuItem;
      N2: TMenuItem;
      SaveDlgSimulation: TSaveDialog;
      UserDefinedLibrary1: TMenuItem;
      SaveSimulationAs1: TMenuItem;
      GBSimSetup: TGroupBox;
      EditSelManagement: TEdit;
      EditSelGlobalInfo: TEdit;
      EditSelProfile: TEdit;
      LblSelManagement: TLabel;
      LblSelGlobalInfo: TLabel;
      LblSelProfile: TLabel;
      RGSelLib: TRadioGroup;
      RGModule: TRadioGroup;
      BtnNew: TButton;
      BtnEdit: TButton;
      ComboSelModule: TComboBox;
      Bevel1: TBevel;
      StdReference: TMenuItem;
      StdTemplate: TMenuItem;
      UserReference: TMenuItem;
      UserTemplate: TMenuItem;
    N3: TMenuItem;
    MakeDaisySetupFile1: TMenuItem;
    LblSelModule: TLabel;
    BtnOutput: TButton;
      procedure ComboInitChange(Sender: TObject);
      procedure BtnInitClick(Sender: TObject);
      procedure About1Click(Sender: TObject);
      procedure BtnCloseClick(Sender: TObject);
      procedure StdReferenceClick(Sender: TObject);
      procedure FormShow(Sender: TObject);
      procedure NewSimulation1Click(Sender: TObject);
      procedure LoadSimulation1Click(Sender: TObject);
      procedure SaveSimulation1Click(Sender: TObject);
      procedure SaveSimulationAs1Click(Sender: TObject);

      procedure SetFormFromVars;
      procedure SetVarsFromForm;
      procedure BtnsMainFormUpdate;
      procedure FormCreate(Sender: TObject);
      procedure FormDestroy(Sender: TObject);
      procedure SelModuleClick(Sender: TObject);
      procedure ComboSelModuleChange(Sender: TObject);
      procedure EditSelectedClick(Sender: TObject);
      procedure RGSelLibExit(Sender: TObject);
      procedure RGModuleExit(Sender: TObject);
      procedure StdTemplateClick(Sender: TObject);
      procedure UserReferenceClick(Sender: TObject);
      procedure UserTemplateClick(Sender: TObject);
    procedure BtnStartClick(Sender: TObject);
    procedure BtnNewClick(Sender: TObject);
    procedure BtnEditClick(Sender: TObject);
    procedure EditHeadingChange(Sender: TObject);
    procedure BtnHelpClick(Sender: TObject);
    procedure Contents1Click(Sender: TObject);
    procedure SearchforHelpOn1Click(Sender: TObject);
    procedure HowtoUseHelp1Click(Sender: TObject);
    procedure EditSelManagementChange(Sender: TObject);
    procedure EditSelGlobalInfoChange(Sender: TObject);
    procedure EditSelProfileChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BtnOutputClick(Sender: TObject);
    procedure MakeDaisySetupFile1Click(Sender: TObject);
   private
      { Private declarations }
   public
      { Public declarations }
      function DoSave(Saving:Boolean) : boolean;
   end;


var
   FormMainForm: TFormMainForm;

implementation

uses globinfo, dsetup, profil, Globals, manglob, RUN, UseLib,pasdaisy,parser,
  EditDate, IntAct, (*FormSelOutput*) SelOutput;

{$R *.DFM}

procedure TFormMainForm.BtnsMainFormUpdate;
var bManIsOurs: boolean;
begin
   { Update BtnInit }
   if ComboInit.ItemIndex > 0 then
      BtnInit.Enabled := TRUE
   else
      BtnInit.Enabled := FALSE;

   { Update BtnNew }
   if (RGSelLib.ItemIndex < 2) or (RGModule.ItemIndex < 0) then
      BtnNew.Hide
   else
      BtnNew.Show;

   { Update BtnEdit }
   { JJ:
     Rettet 28/12-98 saa man kan "edit" start tid for en
     "fremmed" manager.
   }
   if (RGSelLib.ItemIndex < 2) or (RGModule.ItemIndex < 0) or (ComboSelModule.ItemIndex < 1) then begin
      if (RGModule.ItemIndex = 0) and (ComboSelModule.ItemIndex >= 1) then begin
         BtnEdit.Show;
      end else begin
         BtnEdit.Hide;
      end;
   end else
      BtnEdit.Show;

   { Be sure to remove pre/trail blanks }
   EditSelManagement.Text := Trim(EditSelManagement.Text);
   EditSelGlobalInfo.Text := Trim(EditSelGlobalInfo.Text);
   EditSelProfile.Text := Trim(EditSelProfile.Text);

   { Update StartButton }
   (*JJ: 301298 *)
   if (EditSelManagement.Text <> '') then
      GetManagementStartTime(EditSelManagement.Text,bManIsOurs);
   (*JJ: 301298 *)
   if (EditSelManagement.Text <> '')
            and (EditSelGlobalInfo.Text <> '')
            and (EditSelProfile.Text <> '')
            (*JJ:301298*)
            and (MainFormVars.SelOutput.Count > 0)
            and ((MainFormVars.SimulationStartTime <> nil)
                 or (bManIsOurs))
            (*JJ:301298*)
            then
      BtnStart.Enabled := TRUE
   else
      BtnStart.Enabled := FALSE;
end;

procedure TFormMainForm.SetFormFromVars;
begin
   { Set the visible components... }
   Caption := MainFormVars.CurrSimulationFile;
   EditHeading.Text := MainFormVars.SimulationHeading;
   ComboInit.ItemIndex := MainFormVars.InitializationIdx;
(*
   EditSelManagement.Text := MainFormVars.ManVars.ManagerHeading;
   EditSelGlobalInfo.Text := MainFormVars.GlobInfoVars.GlobalInfoHeading;
   EditSelProfile.Text := MainFormVars.ProfVars.ProfileHeading;
*)
   EditSelManagement.Text := MainFormVars.SelMan;
   EditSelGlobalInfo.Text := MainFormVars.SelGlob;
   EditSelProfile.Text := MainFormVars.SelProf;

   RGSelLib.ItemIndex := MainFormVars.RGSelLibIdx;
   RGModule.ItemIndex := MainFormVars.RGModuleIdx;
   SelModuleClick(Self);  { Update the content of the combobox }

   { ...and update the states of the buttons... }
   BtnsMainFormUpdate;
end;

procedure TFormMainForm.SetVarsFromForm;
begin
   MainFormVars.SimulationHeading := Trim(EditHeading.Text);
   MainFormVars.InitializationIdx := ComboInit.ItemIndex;

   MainFormVars.SelMan := Trim(EditSelManagement.Text);
   MainFormVars.SelGlob := Trim(EditSelGlobalInfo.Text);
   MainFormVars.SelProf := Trim(EditSelProfile.Text);

   MainFormVars.RGSelLibIdx := RGSelLib.ItemIndex;
   MainFormVars.RGModuleIdx := RGModule.ItemIndex;
end;

procedure TFormMainForm.ComboInitChange(Sender: TObject);
begin
   MainFormVars.InitializationIdx := ComboInit.ItemIndex;
   MainFormVars.Dirty := TRUE;
   BtnsMainFormUpdate;
end;

procedure TFormMainForm.BtnInitClick(Sender: TObject);
begin
   with OpenDlgSimulation do begin
      DefaultExt := '';
      Filter     := '';
      FilterIndex:= 1;
      Title      := 'Load Initialization File';
      FileName := MainFormVars.CurrSimulationFile;
      if Execute then begin
         MainFormVars.CurrSimulationFile := FileName;
         MainFormVars.Load;
         SetFormFromVars;
         Update;
         MainFormVars.Dirty := TRUE;
      end;
   end;
end;

procedure TFormMainForm.BtnCloseClick(Sender: TObject);
var
   IniFile: TIniFile;
   CanClose: Boolean;
begin
   CanClose := TRUE;
   if MainFormVars.Dirty then
      if MessageDlgPos('Want to close without saving?'
                  , mtWarning, [mbYes, mbNo], 0
                  , FormMainForm.Left - 20
                  , FormMainForm.Top + 200) <> mrYes then
         CanClose := FALSE;
   if CanClose then begin
      SetCurrentDir(DaisyHomeDir);
      IniFile := TIniFile.Create('.\Daisy.ini');
      IniFile.WriteString('Libraries', 'StdRefLib', MainFormVars.StdRefLib);
      IniFile.WriteString('Libraries', 'UserRefLib', MainFormVars.UserRefLib);
      IniFile.WriteString('Libraries', 'StdTemplLib', MainFormVars.StdTemplLib);
      IniFile.WriteString('Libraries', 'UserTemplLib', MainFormVars.UserTemplLib);
      IniFile.Free;

      Application.Terminate;
   end;
end;

procedure TFormMainForm.StdReferenceClick(Sender: TObject);
var
   moduletype, x, y: Integer;
   xx        : daisy_alist;
   IniFile: TIniFile;
begin
   with OpenDlgSimulation do begin { Set standard reference library }
      DefaultExt := 'dai';
      Filter     := 'Daisy library files|*.dai';
      FilterIndex:= 1;
      Title      := 'Set Standard Reference Library';
      FileName   := MainFormVars.StdRefLib;
      if Execute then begin
         { Initialize lib }
         DeleteLibraryObjects(MainFormVars.StdRefLib);
         { Initialize vars from lib }
         if LoadDaisyLibrary(FileName) = nil then begin
            x := FormMainForm.Left;
            y := FormMainForm.Top + 100;
            MessageDlgPos('Could not load library "' + FileName + '".' + CHR(10) + CHR(13)
                           + 'It may not be a valid Daisy Library file.'
                           , mtWarning, [mbOK], 0, x, y);
            // Old library ruined :-(
            exit;
         end;
         MainFormVars.StdRefLib := FileName;
         Lib.Reload(MainFormVars.StdRefLib,'','','');
         for moduletype := 0 to 3 do
            MainFormVars.UpdateLib(0, moduletype); { Initialize variables from library }
         { Update the inifile at once! }
         SetCurrentDir(DaisyHomeDir);
         IniFile := TIniFile.Create('.\Daisy.ini');
         IniFile.WriteString('Libraries', 'StdRefLib', MainFormVars.StdRefLib);
         IniFile.Free;
         MainFormVars.Dirty := TRUE;
(*
         MainFormVars.StdRefLib := FileName;
         { Initialize vars from lib }
         empty_output := LoadDaisyLibrary(MainFormVars.StdRefLib);
{         if xx <> nil then begin
            if not GetVariable(xx,'output',daisy_type_alist,@empty_output) then
               empty_output := nil;
         end; }
         Lib.Reload(MainFormVars.StdRefLib,'','','');
         for moduletype := 0 to 3 do
            MainFormVars.UpdateLib(0, moduletype); { Initialize variables from library }
*)
      end;
   end;
end;

procedure TFormMainForm.StdTemplateClick(Sender: TObject);
var
   moduletype, x, y: Integer;
   IniFile: TIniFile;
begin
   with OpenDlgSimulation do begin { Set standard template library }
      DefaultExt := 'dai';
      Filter     := 'Daisy library files|*.dai';
      FilterIndex:= 1;
      Title      := 'Set Standard Template Library';
      FileName   := MainFormVars.StdTemplLib;
      if Execute then begin
         { Initialize lib }
         DeleteLibraryObjects(MainFormVars.StdTemplLib);
         { Initialize vars from lib }
         if LoadDaisyLibrary(FileName) = nil then begin
            x := FormMainForm.Left;
            y := FormMainForm.Top + 100;
            MessageDlgPos('Could not load library "' + FileName + '".' + CHR(10) + CHR(13)
                           + 'It may not be a valid Daisy Library file.'
                           , mtWarning, [mbOK], 0, x, y);
            // Old library ruined :-(
            exit;
         end;
         MainFormVars.StdTemplLib := FileName;
         Lib.Reload(MainFormVars.StdTemplLib,'','','');
         for moduletype := 0 to 3 do
            MainFormVars.UpdateLib(2, moduletype); { Initialize variables from library }
         { Update the inifile at once! }
         SetCurrentDir(DaisyHomeDir);
         IniFile := TIniFile.Create('.\Daisy.ini');
         IniFile.WriteString('Libraries', 'StdTemplLib', MainFormVars.StdTemplLib);
         IniFile.Free;
         MainFormVars.Dirty := TRUE;
(*
         { Initialize lib }
         DeleteLibraryObjects(MainFormVars.StdTemplLib);
         MainFormVars.StdTemplLib := FileName;
         { Initialize vars from lib }
         LoadDaisyLibrary(MainFormVars.StdTemplLib);
         Lib.Reload('',MainFormVars.StdTemplLib,'','');
         for moduletype := 0 to 3 do
            MainFormVars.UpdateLib(2, moduletype); { Initialize variables from library }
*)
      end;
   end;
end;

procedure TFormMainForm.UserReferenceClick(Sender: TObject);
var
   moduletype, x, y: Integer;
   IniFile: TIniFile;
begin
   with OpenDlgSimulation do begin { Set user defined reference library }
      DefaultExt := 'dai';
      Filter     := 'Daisy library files|*.dai';
      FilterIndex:= 1;
      Title      := 'Set User Defined Reference Library';
      FileName   := MainFormVars.UserRefLib;
      if Execute then begin
         { Initialize lib }
         DeleteLibraryObjects(MainFormVars.UserRefLib);
         { Initialize vars from lib }
         if LoadDaisyLibrary(FileName) = nil then begin
            x := FormMainForm.Left;
            y := FormMainForm.Top + 100;
            MessageDlgPos('Could not load library "' + FileName + '".' + CHR(10) + CHR(13)
                           + 'It may not be a valid Daisy Library file.'
                           , mtWarning, [mbOK], 0, x, y);
            // Old library ruined :-(
            exit;
         end;
         MainFormVars.UserRefLib := FileName;
         Lib.Reload(MainFormVars.UserRefLib,'','','');
         for moduletype := 0 to 3 do
            MainFormVars.UpdateLib(1, moduletype); { Initialize variables from library }
         { Update the inifile at once! }
         SetCurrentDir(DaisyHomeDir);
         IniFile := TIniFile.Create('.\Daisy.ini');
         IniFile.WriteString('Libraries', 'UserRefLib', MainFormVars.UserRefLib);
         IniFile.Free;
         MainFormVars.Dirty := TRUE;
(*
         { Initialize lib }
         DeleteLibraryObjects(MainFormVars.UserRefLib);
         MainFormVars.UserRefLib := FileName;
         { Initialize vars from lib }
         LoadDaisyLibrary(MainFormVars.UserRefLib);
         Lib.Reload('','',MainFormVars.UserRefLib,'');
         for moduletype := 0 to 3 do
            MainFormVars.UpdateLib(1, moduletype); { Initialize variables from library }
*)
      end;
   end;
end;

procedure TFormMainForm.UserTemplateClick(Sender: TObject);
var
   moduletype, x, y: Integer;
   IniFile: TIniFile;
begin
   with OpenDlgSimulation do begin { Set user template library }
      DefaultExt := 'dai';
      Filter     := 'Daisy library files|*.dai';
      FilterIndex:= 1;
      Title      := 'Set User Template Library';
      FileName   := MainFormVars.UserTemplLib;
      if Execute then begin
         { Initialize lib }
         DeleteLibraryObjects(MainFormVars.UserTemplLib);
         { Initialize vars from lib }
         if LoadDaisyLibrary(FileName) = nil then begin
            x := FormMainForm.Left;
            y := FormMainForm.Top + 100;
            MessageDlgPos('Could not load library "' + FileName + '".' + CHR(10) + CHR(13)
                           + 'It may not be a valid Daisy Library file.'
                           , mtWarning, [mbOK], 0, x, y);
            // Old library ruined :-(
            exit;
         end;
         MainFormVars.UserTemplLib := FileName;
         Lib.Reload(MainFormVars.UserTemplLib,'','','');
         for moduletype := 0 to 3 do
            MainFormVars.UpdateLib(3, moduletype); { Initialize variables from library }
         { Update the inifile at once! }
         SetCurrentDir(DaisyHomeDir);
         IniFile := TIniFile.Create('.\Daisy.ini');
         IniFile.WriteString('Libraries', 'UserTemplLib', MainFormVars.UserTemplLib);
         IniFile.Free;
         MainFormVars.Dirty := TRUE;
(*
         { Initialize lib }
         DeleteLibraryObjects(MainFormVars.UserTemplLib);
         MainFormVars.UserTemplLib := FileName;
         { Initialize vars from lib }
         LoadDaisyLibrary(MainFormVars.UserTemplLib);
         Lib.Reload('','',MainFormVars.UserTemplLib,'');
         for moduletype := 0 to 3 do
            MainFormVars.UpdateLib(3, moduletype); { Initialize variables from library }
*)
      end;
   end;
end;

procedure TFormMainForm.FormShow(Sender: TObject);
var
   libtype, moduletype: Integer;
begin
   { Vars allocated on Globals.Create - Initialize here }
   MainFormVars.Reset;
   for libtype := 0 to 3 do
      for moduletype := 0 to 3 do
         MainFormVars.UpdateLib(libtype, moduletype); { Initialize variables from library }
   FormMainForm.SetFormFromVars;
   EditHeading.SetFocus; { Make the form ready for entering data }
   FormMainForm.Update;
   MainFormVars.Dirty := FALSE;
end;

function TFormMainForm.DoSave(Saving:Boolean) : boolean;
var
   SimulationAlist:daisy_alist;
begin
   SetVarsFromForm;
   result := true;
   if MainFormVars.dirty or Saving then begin
      if not Saving then
      begin
         case MessageDlg('Simulation has changed.' + CHR(10) + CHR(13)
                         + 'Select <Yes> to save the changes, ' + CHR(10) + CHR(13)
                         + 'Select <No> NOT to save the changes.',
                         mtWarning, [mbYes,mbNo,mbCancel], 0) of
            mrYes:
               begin
                  { Empty statement }
               end;
            mrNo:
               begin
                  Result := True;
                  Exit;
               end;
            mrCancel:
               begin;
                  Result := False;
                  Exit;
               end;
         end;
      end;

      if MainFormVars.ExternalSimSpecLib = '' then begin
         with SaveDlgSimulation do begin
            DefaultExt := 'sim';
            Filter     := 'Simulation files|*.sim';
            FilterIndex:= 1;
            Title      := 'Save Simulation Project as...';
            (*
            FileName := MainFormVars.CurrSimulationFile;
            InitialDir := MainFormVars.CurrSimulationIniDir;
            *)
            if Execute then begin
               MainFormVars.ExternalSimSpecLib := FileName;
               MainFormVars.CurrSimulationFile := FileName;
               SimulationAlist := MainFormVars.CreateAlistFromVars;
               if SimulationAlist <> nil then
                  begin
                     SaveDaisyLibrary(MainFormVars.SimSpecLib,MainFormVars.ExternalSimSpecLib,SimulationAlist);
                     SetFormFromVars;
                     _daisy_alist_delete(SimulationAlist);

                     { Form is no longer dirty }
                     MainFormVars.Dirty := False;
                  end;
            end else begin
               result := false;
            end;
         end; (* With *)
      end else begin
         SimulationAlist := MainFormVars.CreateAlistFromVars;
         SaveDaisyLibrary(MainFormVars.SimSpecLib,MainFormVars.ExternalSimSpecLib,SimulationAlist);
         if SimulationAlist <> nil then
            begin
               _daisy_alist_delete(SimulationAlist);

               { Form is no longer dirty }
               MainFormVars.Dirty := False;
            end;
      end;
   end;
end;

procedure TFormMainForm.NewSimulation1Click(Sender: TObject);
var
   mdl : integer;
   DoReset: Boolean;
begin
   DoReset := TRUE;
   if MainFormVars.Dirty then // Something is changed - make warning
      if MessageDlgPos('Making a new Simulation Setup will cause all unsaved data to be lost.'
                     + CHR(10) + CHR(13)
                     + 'Select <OK> to make a new Simulation Setup, '
                     + 'or <Cancel> to regret.'
                     , mtWarning, [mbOK, mbCancel], 0
                     , FormGlobalInfo.Left - 20
                     , FormGlobalInfo.Top + 50) <> mrOK then
         DoReset := FALSE;
   if DoReset then begin
      // All ready to reset (also if not dirty!)
      DeleteLibraryObjects(MainFormVars.SimSpecLib);
      MainFormVars.Reset;
      for mdl:=0 to 3 do
         MainFormVars.UpdateLib(4,mdl);
      FormMainForm.SetFormFromVars;
      FormMainForm.Update;
      MainFormVars.Dirty := FALSE;  { Not dirty anymore... }
   end;
(*
   if DoSave(false) then begin
      DeleteLibraryObjects(MainFormVars.SimSpecLib);
      MainFormVars.Reset;
      for mdl:=0 to 3 do
         MainFormVars.UpdateLib(4,mdl);
      FormMainForm.SetFormFromVars;
      FormMainForm.Update;
   end;
*)
end;

procedure TFormMainForm.LoadSimulation1Click(Sender: TObject);
var mdl:integer;
    al : daisy_alist;
begin
   if DoSave(false) then begin
      with OpenDlgSimulation do begin
         DefaultExt := 'sim';
         Filter     := 'Simulation files |*.sim';
         FilterIndex:= 1;
         Title      := 'Load Simulation Project';
         (*
         FileName   := MainFormVars.CurrSimulationFile;
         InitialDir := MainFormVars.CurrSimulationIniDir;
         *)
         if Execute then begin
            // DeleteLibraryObjects(MainFormVars.SimSpecLib);
            MainFormVars.Reset;
            MainFormVars.SimSpecLib := FileName;
            MainFormVars.ExternalSimSpecLib := FileName;
            MainFormVars.CurrSimulationFile := FileName;

            al := LoadDaisyLibrary(MainFormVars.ExternalSimSpecLib);
            MainFormVars.CreateVarsFromAlist(al);
            for mdl:=0 to 3 do
               MainFormVars.UpdateLib(4,mdl);
            SetFormFromVars; { Set visible components on form}
            Update; { and update just to be sure ;-) }
            MainFormVars.Dirty := FALSE;
         end;
      end;
   end;
end;

procedure TFormMainForm.SaveSimulation1Click(Sender: TObject);
begin
   if DoSave(true) then
      MainFormVars.Dirty := FALSE;
end;

procedure TFormMainForm.SaveSimulationAs1Click(Sender: TObject);
var Backup : String;
begin
   Backup := MainFormVars.ExternalSimSpecLib;
   MainFormVars.ExternalSimSpecLib := '';
   if not DoSave(true) then
      MainFormVars.ExternalSimSpecLib := Backup;
end;

procedure TFormMainForm.FormCreate(Sender: TObject);
var
   i: Integer;
   IniFile: TIniFile;
   moduletype: Integer;
begin
   { This is main form => get sizes for initialization here!! }
   NewScreenWidth := Screen.Width;

   { Now scale the form and its components... }
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

   { *** Remember to initialize strings (labels etc.)!!! }
   {...}


      { Init Comboboxes... }
   ComboInit.Items.Clear;
      { Notice: Sequence of adding is important because actions are taken dependant of index! }
   ComboInit.Items.Add('Standard Mode');
   {  Denne er ikke implementeret i Daisy derfor blanket ud Henrik
   ComboInit.Items.Add('Hot Start');}



   // GlobalInit; { Create managervars }
   if not Lib.LoadFromFile(DaisyHomeDir+'\DaisyLibrary\NewDaisyLib.txt') then begin
      Application.MessageBox('Could not open Libary','Error',MB_OKCancel);
   end;
   { Init libpaths from ini-file... }
   SetCurrentDir(DaisyHomeDir);
   IniFile := TIniFile.Create('.\Daisy.ini');
   MainFormVars.StdRefLib := IniFile.ReadString('Libraries', 'StdRefLib', '');
   MainFormVars.UserRefLib := IniFile.ReadString('Libraries', 'UserRefLib', '');
   MainFormVars.StdTemplLib := IniFile.ReadString('Libraries', 'StdTemplLib', '');
   MainFormVars.UserTemplLib := IniFile.ReadString('Libraries', 'UserTemplLib', '');
   IniFile.Free;
   { Initialize vars from lib }
   empty_output := LoadDaisyLibrary(MainFormVars.StdRefLib);
   LoadDaisyLibrary(MainFormVars.UserRefLib);
   LoadDaisyLibrary(MainFormVars.StdTemplLib);
   LoadDaisyLibrary(MainFormVars.UserTemplLib);
   Lib.Reload(MainFormVars.StdRefLib,MainFormVars.UserRefLib,MainFormVars.StdTemplLib,MainFormVars.UserTemplLib);
   for moduletype := 0 to 3 do
      MainFormVars.UpdateLib(0, moduletype); { Initialize variables from library }

      { Init HelpContexts... }
   EditHeading.HelpContext := HLP_Sim_Description;
   ComboInit.HelpContext := HLP_Sim_Initialization;
   BtnInit.HelpContext := HLP_Sim_Initialization;
   BtnStart.HelpContext := HLP_Sim_Start_Simulation;
   GBSimSetup.HelpContext := HLP_Sim_Setup;
   RGSelLib.HelpContext := HLP_Sim_Library;
   RGModule.HelpContext := HLP_Sim_Module;
   BtnNew.HelpContext := HLP_Sim_Setup;
   BtnEdit.HelpContext := HLP_Sim_Setup;
   ComboSelModule.HelpContext := HLP_Sim_Sel_Mod_from_Lib;
   EditSelManagement.HelpContext := HLP_Sim_Sel_Man;
   EditSelGlobalInfo.HelpContext := HLP_Sim_Sel_GlobInfo;
   EditSelProfile.HelpContext := HLP_Sim_Sel_Soil_Init;
   BtnClose.HelpContext := HLP_Sim_Setup;
   BtnHelp.HelpContext := HLP_Sim_Setup;

end;

procedure TFormMainForm.FormDestroy(Sender: TObject);
begin
   // GlobalTerminate;
end;

procedure TFormMainForm.SelModuleClick(Sender: TObject);
begin
   { Update ComboSelModule }
   with ComboSelModule do begin
      Items.Clear;
      Items.Add('-none-');  { To make it possible to clear the selection }
      { Init after selection in the two radiogroups... }

      case RGSelLib.ItemIndex of
         0: case RGModule.ItemIndex of
               0: Items.AddStrings(MainFormVars.StdRefLibManItems);
               1: Items.AddStrings(MainFormVars.StdRefLibGlobItems);
               2: Items.AddStrings(MainFormVars.StdRefLibProfItems);
            end;
         1: case RGModule.ItemIndex of
               0: Items.AddStrings(MainFormVars.UserRefLibManItems);
               1: Items.AddStrings(MainFormVars.UserRefLibGlobItems);
               2: Items.AddStrings(MainFormVars.UserRefLibProfItems);
            end;
         2: case RGModule.ItemIndex of
               0: Items.AddStrings(MainFormVars.SimSpecLibManItems);
               1: Items.AddStrings(MainFormVars.SimSpecLibGlobItems);
               2: Items.AddStrings(MainFormVars.SimSpecLibProfItems);
            end;
      end;
      ItemIndex := 0; { Nothing selected }
   end;

   { Update Buttons }
   BtnsMainFormUpdate;
end;

procedure TFormMainForm.ComboSelModuleChange(Sender: TObject);
var
   s: String;
   SimSpec: Boolean;
begin
   if ComboSelModule.ItemIndex < 1 then  { 'blank' or '-none-' }
   s := '-none-'
   else
      s := ComboSelModule.Text;

   case RGModule.ItemIndex of
      0: begin
//            MainFormVars.ManVars.ManagerHeading := s;
            MainFormVars.SelMan := s;
            EditSelManagement.Text := MainFormVars.SelMan;
            MainFormVars.SelManType := RGSelLib.ItemIndex;
         end;
      1: begin
//            MainFormVars.GlobInfoVars.GlobalInfoHeading := s;
            MainFormVars.SelGlob := s;
            EditSelGlobalInfo.Text := MainFormVars.SelGlob;
            MainFormVars.SelGlobType := RGSelLib.ItemIndex;
         end;
      2: begin
//            MainFormVars.ProfVars.ProfileHeading := s;
            MainFormVars.SelProf := s;
            EditSelProfile.Text := MainFormVars.SelProf;
            MainFormVars.SelProfType := RGSelLib.ItemIndex;
         end;
   end;
   { Update Buttons... }
   MainFormVars.Dirty := TRUE;
   BtnsMainFormUpdate;
end;

procedure TFormMainForm.EditSelectedClick(Sender: TObject);
begin
   { Update radiogroups with relevant information, so it is possible to see }
   { which type the selected object is...                                   }
   with Sender as TEdit do
      if Name = 'EditSelManagement' then begin
         RGSelLib.ItemIndex := MainFormVars.SelManType;
         RGModule.ItemIndex := 0;
         { Update combo with manager items }
         {...}
         ComboSelModule.ItemIndex
               := ComboSelModule.Items.IndexOf(EditSelManagement.Text);
      end
      else if Name = 'EditSelGlobalInfo' then begin
         RGSelLib.ItemIndex := MainFormVars.SelGlobType;
         RGModule.ItemIndex := 1;
         { Update combo with global info items }
         {...}
         ComboSelModule.ItemIndex
               := ComboSelModule.Items.IndexOf(EditSelGlobalInfo.Text);
      end
      else if Name = 'EditSelProfile' then begin
         RGSelLib.ItemIndex := MainFormVars.SelProfType;
         RGModule.ItemIndex := 2;
         { Update combo with profile items }
         {...}
         ComboSelModule.ItemIndex
               := ComboSelModule.Items.IndexOf(EditSelProfile.Text);
      end;

      { Update buttons... }
      BtnsMainFormUpdate;
end;

procedure TFormMainForm.RGSelLibExit(Sender: TObject);
begin
   MainFormVars.RGSelLibIdx := RGSelLib.ItemIndex;
end;

procedure TFormMainForm.RGModuleExit(Sender: TObject);
begin
   MainFormVars.RGModuleIdx := RGModule.ItemIndex;
end;

procedure TFormMainForm.BtnStartClick(Sender: TObject);
begin
   FormRunSim.ShowModal;
end;

procedure TFormMainForm.BtnNewClick(Sender: TObject);
var
   s: String;
   lastidx: Integer;
begin
   case RGModule.ItemIndex of
      0: begin
            { Reset TempVariables }
            TempManagerVars.Reset;

            FormSetup.ShowModal;

            if FormSetup.ModalResult = mrOK then begin { Update Variables }
//               MainFormVars.ManVars.Assign(TempManagerVars);
               { Update the list of selectable items... }
(*
               if MainFormVars.UserTemplLibManItems.Count > 0 then begin
                  lastidx := MainFormVars.UserTemplLibManItems.Count - 1;
                  s := copy(MainFormVars.UserTemplLibManItems[lastidx], 1, 9 );
                  { This userdefined simulationspecific item is prefixed 'SimSpec: ' }
                  { There can only be one such item in the list...                   }
                  if s = 'SimSpec: ' then
                     MainFormVars.UserTemplLibManItems.Delete(lastidx);
               end;
               MainFormVars.UserTemplLibManItems.Add
                           ('SimSpec: ' + MainFormVars.ManVars.ManagerHeading);
*)
               SelModuleClick(Sender); { Update the content of the combobox }
               with ComboSelModule do begin { Update the EditSelManagement }
                  ItemIndex := Items.Count - 1;
                  EditSelManagement.Text := Items[ItemIndex];
               end;
               ComboSelModuleChange(Sender); { Update vars etc... }
               MainFormVars.SelManType := RGSelLib.ItemIndex;
               MainFormVars.Dirty := TRUE;
            end;
         end;
      1: begin
            { Reset TempVariables }
            TempGlobalInfoVars.Reset;
            TempGlobalInfoVars.GlobalInfoHeading := '';
            TempGlobalInfoVars.LoadDefault; { Init to default values }
            FormGlobalInfo.EditHeading.Enabled := TRUE; { Make it possible to assign a heading }
            FormGlobalInfo.ShowModal;

            if FormGlobalInfo.ModalResult = mrOK then begin { Update Variables }
               SelModuleClick(Sender); { Update the content of the combobox }
               with ComboSelModule do begin { Update the EditSelGlobalInfo }
                  ItemIndex := Items.Count - 1;
                  EditSelGlobalInfo.Text := Items[ItemIndex];
               end;
               ComboSelModuleChange(Sender); { Update vars etc... }
               MainFormVars.SelGlobType := RGSelLib.ItemIndex;
               MainFormVars.Dirty := TRUE;
            end;
         end;
      2: begin
            { Reset TempVariables }
            TempProfileVars.Reset;
            TempProfileVars.ProfileHeading := '';
            FormProfile.EditHeading.Enabled := TRUE; { Make it possible to assign a heading }
            FormProfile.ShowModal;

            if FormProfile.ModalResult = mrOK then begin  { Update Variables }
               SelModuleClick(Sender); { Update the content of the combobox }
               with ComboSelModule do begin { Update the EditSelProfile }
                  ItemIndex := Items.Count - 1;
                  EditSelProfile.Text := Items[ItemIndex];
               end;
               ComboSelModuleChange(Sender); { Update vars etc... }
               MainFormVars.SelProfType := RGSelLib.ItemIndex;
               MainFormVars.Dirty := TRUE;
           end;
         end;
   end;
   BtnsMainFormUpdate;
end;

procedure TFormMainForm.BtnEditClick(Sender: TObject);
var
   s: String;
   lastidx: Integer;
   start : TInternAction;
   DatoStr : String;
begin
   case RGModule.ItemIndex of
      0: begin { Manager }
            {JJ:30/12-98 Rettet saa man kan editerer start tid for en "fremmed" manager.
             Hjælpen passer dog p.t. ikke helt endnu!}
            if (RGModule.ItemIndex = 0) and (ComboSelModule.ItemIndex >= 1) and (RGSelLib.ItemIndex < 2) then begin
               start := TStart.Create;
               with FormDate do begin
                  with MainFormVars do begin
                  if SimulationStartTime <> nil then
                  DatoStr   := IntToStr(_daisy_time_get_year(SimulationStartTime)) + '-' +
                               IntToStr(_daisy_time_get_month(SimulationStartTime)) + '-' +
                               IntToStr(_daisy_time_get_mday(SimulationStartTime));
                  end;
                  start.SetDato(DatoStr);
                  InitFromAction(LoadStr(RES_MSC_DSetup_Man_StartDate)
                                , start, HLP_Time_of_Action, 100, 100);

                  if ShowModal = mrOK then begin
                     SetActionDateFromFormDate(start);
                     with MainFormVars do begin
                        if (SimulationStartTime <> nil) and bStartTimeIsOurs then
                           _daisy_time_delete(SimulationStartTime);
                        SimulationStartTime := _daisy_time_create((start.when as TDate).year, (start.when as TDate).month, (start.when as TDate).day,1);
                        bStartTimeIsOurs := true;
                     end;
                  end;
               end
            end else begin
               TempManagerVars.Reset; // JJ: MUST Be called before Assign
               if EditSelManagement.Text = '' then
                  BtnNewClick(Sender)
               else begin  { Initialize TempVariables }
                  TempManagerVars.ManagerHeading := EditSelManagement.Text; { Set template }
                  FormSetup.EBManagerHeading.Enabled := FALSE; { Not allowed to change heading during edit }
                  FormSetup.ShowModal;

                  if FormSetup.ModalResult = mrOK then begin
                     SelModuleClick(Sender); { Update the content of the combobox }
                     with ComboSelModule do begin { Update the EditSelManagement }
                        ItemIndex := Items.Count - 1;
                        EditSelManagement.Text := Items[ItemIndex];
                     end;
                     ComboSelModuleChange(Sender); { Update vars etc... }
                     MainFormVars.SelManType := RGSelLib.ItemIndex;
                     MainFormVars.Dirty := TRUE;
                  end;
               end;
            end;
         end;
      1: begin { Global Information }
            if EditSelGlobalInfo.Text = '' then
               BtnNewClick(Sender)
            else begin  { Initialize TempVariables }
               TempGlobalInfovars.Reset; { Reset vars to "NotSet" }
//               TempGlobalInfovars.LoadDefault; { Initialize to default values }
               TempGlobalInfoVars.Load(EditSelGlobalInfo.Text, 1);  { Load info from template }
               TempGlobalInfoVars.GlobalInfoHeading := EditSelGlobalInfo.Text;
               FormGlobalInfo.EditHeading.Enabled := FALSE; { Not allowed to change heading during edit }
               FormGlobalInfo.ShowModal;

               if FormGlobalInfo.ModalResult = mrOK then begin { Update Variables }
                  { Update the list of selectable items... }
                  SelModuleClick(Sender); { Update the content of the combobox }
                  with ComboSelModule do begin { Update the EditSelGlobalInfo }
                     ItemIndex := Items.Count - 1;
                     EditSelGlobalInfo.Text := Items[ItemIndex];
                  end;
                  ComboSelModuleChange(Sender); { Update vars etc... }
                  MainFormVars.SelGlobType := RGSelLib.ItemIndex;
                  MainFormVars.Dirty := TRUE;
               end;
            end;
         end;
      2: begin { Profile }
            if EditSelProfile.Text = '' then
               BtnNewClick(Sender)
            else begin  { Initialize TempVariables }
               TempProfileVars.Load(EditSelProfile.Text, 1);  { Load info from template }
               TempProfileVars.ProfileHeading := EditSelProfile.Text;
               FormProfile.EditHeading.Enabled := FALSE; { Not allowed to change heading during edit }
               FormProfile.ShowModal;

               if FormProfile.ModalResult = mrOK then begin { Update Variables }
                  { Update the list of selectable items... }
                  SelModuleClick(Sender); { Update the content of the combobox }
                  with ComboSelModule do begin { Update the EditSelGlobalInfo }
                     ItemIndex := Items.Count - 1;
                     EditSelProfile.Text := Items[ItemIndex];
                  end;
                  ComboSelModuleChange(Sender); { Update vars etc... }
                  MainFormVars.SelProfType := RGSelLib.ItemIndex;
                  MainFormVars.Dirty := TRUE;
               end;
            end;
         end;
   end;
   BtnsMainFormUpdate;
end;

procedure TFormMainForm.EditHeadingChange(Sender: TObject);
begin
   MainFormVars.SimulationHeading := Trim(Text);
   MainFormVars.Dirty := TRUE;
end;

procedure TFormMainForm.BtnHelpClick(Sender: TObject);
begin
   Application.HelpContext(HLP_Sim_Setup);
end;

procedure TFormMainForm.About1Click(Sender: TObject);
begin
   FormAbout.ShowModal;
end;

procedure TFormMainForm.Contents1Click(Sender: TObject);
begin
   Application.HelpCommand(HELP_TAB, 0);
end;

procedure TFormMainForm.SearchforHelpOn1Click(Sender: TObject);
begin
   Application.HelpCommand(HELP_PARTIALKEY, HelpEmptyKey);
end;

procedure TFormMainForm.HowtoUseHelp1Click(Sender: TObject);
begin
   Application.HelpCommand(HELP_HELPONHELP, 0);
end;

procedure TFormMainForm.EditSelManagementChange(Sender: TObject);
begin
   MainFormVars.Dirty := TRUE;
end;

procedure TFormMainForm.EditSelGlobalInfoChange(Sender: TObject);
begin
   MainFormVars.Dirty := TRUE;
end;

procedure TFormMainForm.EditSelProfileChange(Sender: TObject);
begin
   MainFormVars.Dirty := TRUE;
end;

procedure TFormMainForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
   { This subprogram is called if the user closes the application via the
     system menu. It must simulate that the Close button was clicked. }

   { Don't close the form indiscriminately. }
   Action := caNone;

   { Simulate that the Close button was clicked. }
   BtnCloseClick(Sender);
end;

procedure TFormMainForm.BtnOutputClick(Sender: TObject);
var
   DummyOutPutDir : String;
begin
   with FormOutput do begin
      PrepareForm(MainFormVars.StdRefLibOutputItems,MainFormVars.SelOutput,DummyOutPutDir);
      if ShowModal = mrOK then begin
         if GetSelection(MainFormVars.SelOutput,DummyOutPutDir) then
            MainFormVars.Dirty := True;
      end;
   end;
(*
   with FrmSelOutput do begin
      PrepareForm(MainFormVars.StdRefLibOutputItems,MainFormVars.SelOutput,DummyOutPutDir);
      if ShowModal = mrOK then begin
         if GetSelection(MainFormVars.SelOutput,DummyOutPutDir) then
            MainFormVars.Dirty := True;
      end;
   end;
*)
end;

function EmitInputFile(filename:string):string;
var i: integer;
begin
   result := '(input file "';
   for i:=1 to Length(filename) do begin
      if filename[i] = '\' then
         result := result + '/'
      else
         result := result + filename[i];
   end;
   result := result +'")';
end;

procedure TFormMainForm.MakeDaisySetupFile1Click(Sender: TObject);
var StandAloneSetup: TStringList;
    TempFileName : String;
    TempFileName2 : String;
    TempSimulationStartTime : daisy_time;
    bIsOurs : Boolean;
begin
   if DoSave(true) then begin
      MainFormVars.Dirty := FALSE;
      with MainFormVars do begin
         StandAloneSetup := TStringList.Create;
         StandAloneSetup.Add(EmitInputFile(StdRefLib));
         StandAloneSetup.Add(EmitInputFile(UserRefLib));
         StandAloneSetup.Add(EmitInputFile(StdTemplLib));
         StandAloneSetup.Add(EmitInputFile(UserTemplLib));
         TempFileName := ExtractFileName(CurrSimulationFile);
         StandAloneSetup.Add(EmitInputFile(TempFileName));
//         TempSimulationStartTime := GetManagementStartTime(SelMan,bIsOurs);
//         if bIsOUrs then begin
//            StandAloneSetup.Add(TimeToString(TempSimulationStartTime));
//         end;
         TempFileName := Copy(TempFileName,0,Length(TempFileName)-Length(ExtractFileExt(TempFileName))) + '.dai';
         StandAloneSetup.SaveToFile(TempFileName);
         StandAloneSetup.Free;
      end;
   end;
end;


end.
