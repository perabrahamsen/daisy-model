program DaisyAll;

uses
  Forms,
  About in 'About.pas' {FormAbout},
  Comments in 'Comments.pas' {FormComments},
  DaisyLib in 'DaisyLib.pas',
  Discret in 'Discret.pas' {FormDiscretization},
  dsetup in 'dsetup.pas' {FormSetup},
  EditFert in 'EditFert.pas' {FrmEditFert},
  EditHarv in 'EditHarv.pas' {FrmEditHar},
  EditIrr in 'EditIrr.pas' {FrmEditIrr},
  EditSow in 'EditSow.pas' {FrmEditSow},
  EditTill in 'EditTill.pas' {FrmEditTill},
  Globals in 'Globals.pas' {FormGlobals},
  globinfo in 'globinfo.pas' {FormGlobalInfo},
  Horizon in 'Horizon.pas' {FormHorizon},
  Hydraul in 'Hydraul.pas' {FormHydraulic},
  IntAct in 'IntAct.pas',
  LibComp in 'LibComp.pas' {FormSelFromLib},
  main in 'main.pas' {FormMainForm},
  ManGlob in 'ManGlob.Pas',
  OneEdit in 'OneEdit.pas' {FormOneEdit},
  Parser in 'Parser.pas',
  pasdaisy in 'pasdaisy.pas',
  profil in 'profil.pas' {FormProfile},
  SADlg in 'SADlg.pas' {DlgSelectAction},
  ShowList in 'ShowList.pas' {OKBottomDlg},
  StatInit in 'StatInit.pas' {FormStateInit},
  Tabel in 'Tabel.pas' {FormTable},
  TwoEdit in 'TwoEdit.pas' {FormTwoEdit},
  SimSynt in 'SimSynt.pas',
  DsyTime in 'DsyTime.pas',
  EditDate in 'EditDate.pas' {FormDate},
  SaveAs in 'SaveAs.pas' {FormSaveAs},
  UseLib in 'UseLib.pas',
  SelOutput in 'SelOutput.pas' {FormOutput},
  RUN in 'RUN.pas' {FormRunSim};

{$R *.RES}

begin
  Application.Initialize;
  Application.HelpFile := 'Helpdsy.hlp';
  Application.CreateForm(TFormMainForm, FormMainForm);
  Application.CreateForm(TFormAbout, FormAbout);
  Application.CreateForm(TFormComments, FormComments);
  Application.CreateForm(TFormDiscretization, FormDiscretization);
  Application.CreateForm(TFormSetup, FormSetup);
  Application.CreateForm(TFrmEditFert, FrmEditFert);
  Application.CreateForm(TFrmEditHar, FrmEditHar);
  Application.CreateForm(TFrmEditIrr, FrmEditIrr);
  Application.CreateForm(TFrmEditSow, FrmEditSow);
  Application.CreateForm(TFrmEditTill, FrmEditTill);
  Application.CreateForm(TFormGlobals, FormGlobals);
  Application.CreateForm(TFormGlobalInfo, FormGlobalInfo);
  Application.CreateForm(TFormHorizon, FormHorizon);
  Application.CreateForm(TFormHydraulic, FormHydraulic);
  Application.CreateForm(TFormSelFromLib, FormSelFromLib);
  Application.CreateForm(TFormOneEdit, FormOneEdit);
  Application.CreateForm(TFormProfile, FormProfile);
  Application.CreateForm(TDlgSelectAction, DlgSelectAction);
  Application.CreateForm(TOKBottomDlg, OKBottomDlg);
  Application.CreateForm(TFormStateInit, FormStateInit);
  Application.CreateForm(TFormTable, FormTable);
  Application.CreateForm(TFormTwoEdit, FormTwoEdit);
  Application.CreateForm(TFormDate, FormDate);
  Application.CreateForm(TFormSaveAs, FormSaveAs);
  Application.CreateForm(TFormOutput, FormOutput);
  Application.CreateForm(TFormRunSim, FormRunSim);
  Application.Run;
end.
