unit StatInit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, StdCtrls, Tabel, Grids, Globals, TypInfo;

type
  TFormStateInit = class(TForm)
    LblWater: TLabel;
    LblTemperature: TLabel;
    LblNitrate: TLabel;
    LblAmmonia: TLabel;
    ComboWater: TComboBox;
    ComboTemperature: TComboBox;
    ComboNitrate: TComboBox;
    ComboAmmonia: TComboBox;
    BtnWater: TButton;
    BtnTemperature: TButton;
    BtnNitrate: TButton;
    BtnAmmonia: TButton;
    BtnHelp: TBitBtn;
    BtnCancel: TBitBtn;
    BtnOK: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure ComboWaterChange(Sender: TObject);
    procedure ComboTemperatureChange(Sender: TObject);
    procedure ComboNitrateChange(Sender: TObject);
    procedure ComboAmmoniaChange(Sender: TObject);
    procedure BtnWaterClick(Sender: TObject);
    procedure BtnTemperatureClick(Sender: TObject);
    procedure BtnNitrateClick(Sender: TObject);
    procedure BtnAmmoniaClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);

    procedure SetFormFromVars;
    procedure SetVarsFromForm;
    procedure BtnWaterUpdate;
    procedure BtnTemperatureUpdate;
    procedure BtnNitrateUpdate;
    procedure BtnAmmoniaUpdate;
    procedure BtnsStateInitUpdate;
    procedure BtnHelpClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

procedure PlaceFormStateInit(x, y: Integer);

var
  FormStateInit: TFormStateInit;


implementation

uses OneEdit;

{$R *.DFM}


procedure TFormStateInit.SetFormFromVars;
begin
{ Set the immediately visible components after variable's content... }
   with FormStateInit do begin
   // Caption := 'Soil Profile State Variable Initialization';
   // Caption is set in 'FormCreate'

//      ComboWater.Clear;
//      ComboWater.Items.AddStrings(Temp2StateVars.SoilWaterContItems);
      ComboWater.ItemIndex := Temp2StateVars.SoilWaterContIdx;
//      ComboTemperature.Clear;
//      ComboTemperature.Items.AddStrings(Temp2StateVars.SoilTemperatureItems);
      ComboTemperature.ItemIndex := Temp2StateVars.SoilTemperatureIdx;
//      ComboNitrate.Clear;
//      ComboNitrate.Items.AddStrings(Temp2StateVars.SoilNitrateContItems);
      ComboNitrate.ItemIndex := Temp2StateVars.SoilNitrateContIdx;
//      ComboAmmonia.Clear;
//      ComboAmmonia.Items.AddStrings(Temp2StateVars.SoilAmmoniaContItems);
      ComboAmmonia.ItemIndex := Temp2StateVars.SoilAmmoniaContIdx;
   end;

{ Update the states of the buttons in the form }
   BtnsStateInitUpdate;
end;

procedure TFormStateInit.SetVarsFromForm;
begin
{ Set the immediately visible components after variable's content... }
   with FormStateInit do begin
//      Temp2StateVars.SoilWaterContItems.Clear;
//      Temp2StateVars.SoilWaterContItems.AddStrings(ComboWater.Items);
      Temp2StateVars.SoilWaterContIdx := ComboWater.ItemIndex;
//      Temp2StateVars.SoilTemperatureItems.Clear;
//      Temp2StateVars.SoilTemperatureItems.AddStrings(ComboTemperature.Items);
      Temp2StateVars.SoilTemperatureIdx := ComboTemperature.ItemIndex;
//      Temp2StateVars.SoilNitrateContItems.Clear;
//      Temp2StateVars.SoilNitrateContItems.AddStrings(ComboNitrate.Items);
      Temp2StateVars.SoilNitrateContIdx := ComboNitrate.ItemIndex;
//      Temp2StateVars.SoilAmmoniaContItems.Clear;
//      Temp2StateVars.SoilAmmoniaContItems.AddStrings(ComboAmmonia.Items);
      Temp2StateVars.SoilAmmoniaContIdx := ComboAmmonia.ItemIndex;
   end;
end;

procedure TFormStateInit.BtnWaterUpdate;
begin
{
   Field Capacity => no parameters
   Equlibrium Profile => no params (only selectable if groundwater
                                    is chosen for "nedre rand")
   Potential vs. Depth => table (dybde vs. h)
   Water Content vs. Depth => table (dybde vs. theta)
}
   case ComboWater.ItemIndex of
      -1, 0, 1: BtnWater.Hide;
   else
      BtnWater.Show;
   end;
end;

procedure TFormStateInit.BtnTemperatureUpdate;
begin
{
   Euilibrium Profile => no parameters
   Temperature vs. Depth => table (dybde vs. temp)
}
   case ComboTemperature.ItemIndex of
      -1, 0: BtnTemperature.Hide;
   else
      BtnTemperature.Show;
   end;
end;

procedure TFormStateInit.BtnNitrateUpdate;
begin
{
   Uniform Profile => no parameters
   Nitrate Concentration vs. Depth => table (dybde vs. NO3- conc)
}
   case ComboNitrate.ItemIndex of
      -1: BtnNitrate.Hide;
   else
      BtnNitrate.Show;
   end;
end;

procedure TFormStateInit.BtnAmmoniaUpdate;
begin
{
   Uniform Profile => no parameters
   Ammonia Content vs. Depth => table (dybde vs. Am.cont.)
}
   case ComboAmmonia.ItemIndex of
      -1: BtnAmmonia.Hide;
   else
      BtnAmmonia.Show;
   end;
end;

procedure TFormStateInit.BtnsStateInitUpdate;
begin
   BtnWaterUpdate;
   BtnTemperatureUpdate;
   BtnNitrateUpdate;
   BtnAmmoniaUpdate;
end;


procedure TFormStateInit.FormCreate(Sender: TObject);
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
   Caption := LoadStr(RES_FRM_StatInit);

   { Button labels }
   BtnOK.Caption          := LoadStr(RES_BTN_Ok);
   BtnCancel.Caption      := LoadStr(RES_BTN_Cancel);
   BtnHelp.Caption        := LoadStr(RES_BTN_Help);
   BtnWater.Caption       := LoadStr(RES_BTN_Parameters);
   BtnTemperature.Caption := LoadStr(RES_BTN_Parameters);
   BtnNitrate.Caption     := LoadStr(RES_BTN_Parameters);
   BtnAmmonia.Caption     := LoadStr(RES_BTN_Parameters);

   { Label captions }
   LblWater.Caption       := LoadStr(RES_LBL_StatInit_Water_Content);
   LblTemperature.Caption := LoadStr(RES_LBL_StatInit_Soil_Temp);
   LblNitrate.Caption     := LoadStr(RES_LBL_StatInit_Nitrate_Content);
   LblAmmonia.Caption     := LoadStr(RES_LBL_StatInit_Ammonia_Content);


   { Combos }
   ComboWater.Items.Clear;
   ComboWater.Items.Add(LoadStr(RES_LBL_StatInit_Field_Capacity));
   ComboWater.Items.Add(LoadStr(RES_LBL_StatInit_Equilibrium_Prof));
   ComboWater.Items.Add(LoadStr(RES_LBL_StatInit_Potential_Depth));
   ComboWater.Items.Add(LoadStr(RES_LBL_StatInit_Water_Cont_Depth));

   ComboTemperature.Items.Clear;
   ComboTemperature.Items.Add(LoadStr(RES_LBL_StatInit_Equilibrium_Prof));
   ComboTemperature.Items.Add(LoadStr(RES_LBL_StatInit_Temp_Depth));

   ComboNitrate.Items.Clear;
   ComboNitrate.Items.Add(LoadStr(RES_LBL_StatInit_Uniform_Prof));
   ComboNitrate.Items.Add(LoadStr(RES_LBL_StatInit_Nitrate_Conc_Depth));

   ComboAmmonia.Items.Clear;
   ComboAmmonia.Items.Add(LoadStr(RES_LBL_StatInit_Uniform_Prof));
   ComboAmmonia.Items.Add(LoadStr(RES_LBL_StatInit_Ammonia_Cont_Depth));

   { Init HelpContexts... }
   ComboWater.HelpContext := HLP_Soil_Water_Content;
   BtnWater.HelpContext := HLP_Soil_Water_Content;
   ComboTemperature.HelpContext := HLP_Soil_Temperature;
   BtnTemperature.HelpContext := HLP_Soil_Temperature;
   ComboNitrate.HelpContext := HLP_Soil_Nitrate_Conc;
   BtnNitrate.HelpContext := HLP_Soil_Nitrate_Conc;
   ComboAmmonia.HelpContext := HLP_Soil_Ammonia_Content;
   BtnAmmonia.HelpContext := HLP_Soil_Ammonia_Content;
   BtnOK.HelpContext := HLP_StateVar_Init;
   BtnCancel.HelpContext := HLP_StateVar_Init;
   BtnHelp.HelpContext := HLP_StateVar_Init;
end;

procedure TFormStateInit.ComboWaterChange(Sender: TObject);
begin
   Temp2StateVars.SoilWaterContIdx := ComboWater.ItemIndex;
   BtnWaterUpdate;
end;

procedure TFormStateInit.ComboTemperatureChange(Sender: TObject);
begin
   Temp2StateVars.SoilTemperatureIdx := ComboTemperature.ItemIndex;
   BtnTemperatureUpdate;
end;

procedure TFormStateInit.ComboNitrateChange(Sender: TObject);
begin
   Temp2StateVars.SoilNitrateContIdx := ComboNitrate.ItemIndex;
   BtnNitrateUpdate;
end;

procedure TFormStateInit.ComboAmmoniaChange(Sender: TObject);
begin
   Temp2StateVars.SoilAmmoniaContIdx := ComboAmmonia.ItemIndex;
   BtnAmmoniaUpdate;
end;

procedure TFormStateInit.BtnWaterClick(Sender: TObject);
var
   x, y: Integer;
begin
   x := FormStateInit.Left + BtnWater.Left;
   y := FormStateInit.Top + BtnWater.Top;
   with ComboWater do begin
      case ItemIndex of
         2: begin
               InitTable(x, y, Items[ItemIndex], 'Depth (cm)', 'h (cm)'
                         , Temp2StateVars.NumPotentialVsDepthItems
                         , Temp2StateVars.PotentialVsDepth, 1
                         , HLP_SWC_Potential_vs_Depth);
               FormTable.ShowModal;
               if FormTable.ModalResult = mrOK then
                  Temp2StateVars.NumPotentialVsDepthItems := ReadTable(Temp2StateVars.PotentialVsDepth);
            end;
         3: begin
               InitTable(x, y, Items[ItemIndex], 'Depth (cm)', 'theta'
                         , Temp2StateVars.NumWaterContVsDepthItems
                         , Temp2StateVars.WaterContVsDepth, 1
                         , HLP_SWC_Water_vs_Depth);
               FormTable.ShowModal;
               if FormTable.ModalResult = mrOK then
                  Temp2StateVars.NumWaterContVsDepthItems := ReadTable(Temp2StateVars.WaterContVsDepth);
            end;
      end;
   end;
end;

procedure TFormStateInit.BtnTemperatureClick(Sender: TObject);
var
   x, y: Integer;
begin
   x := FormStateInit.Left + BtnTemperature.Left;
   y := FormStateInit.Top + BtnTemperature.Top;
   with ComboTemperature do begin
      case ItemIndex of
         1: begin
               InitTable(x, y, Items[ItemIndex], 'Depth (cm)', 'Temp (C)'
                         , Temp2StateVars.NumTemperatureVsDepthItems
                         , Temp2StateVars.TemperatureVsDepth, 1
                         , HLP_ST_Temp_vs_Depth);
               FormTable.ShowModal;
               if FormTable.ModalResult = mrOK then
                  Temp2StateVars.NumTemperatureVsDepthItems := ReadTable(Temp2StateVars.TemperatureVsDepth);
            end;
      end;
   end;
end;

procedure TFormStateInit.BtnNitrateClick(Sender: TObject);
var
   x, y: Integer;
   s: String;
begin
   x := FormStateInit.Left + BtnNitrate.Left;
   y := FormStateInit.Top + BtnNitrate.Top;
   with ComboNitrate do begin
      case ItemIndex of
         0: begin
               InitOneEdit(x, y, 'Nitrate Concentration', 'Nitrate Concentration',
                           'mg/l', 2, Temp2StateVars.NitrateConc, -1
                           , HLP_SNC_Uniform_Profile);
               if FormOneEdit.ShowModal = mrOK then begin
                  s := ReadOneEdit;
                  if s = '' then
                     Temp2StateVars.NitrateConc := NotSet
                  else
                     Temp2StateVars.NitrateConc := StrToFloat(s);
               end;
            end;
         1: begin
               InitTable(x, y, Items[ItemIndex], 'Depth (cm)', 'NO3- N (mg/l)'
                         , Temp2StateVars.NumNitrateVsDepthItems
                         , Temp2StateVars.NitrateVsDepth, 1
                         , HLP_SNC_Nitrate_Conc_vs_Depth);
               FormTable.ShowModal;
               if FormTable.ModalResult = mrOK then
                  Temp2StateVars.NumNitrateVsDepthItems := ReadTable(Temp2StateVars.NitrateVsDepth);
            end;
      end;
   end;
end;

procedure TFormStateInit.BtnAmmoniaClick(Sender: TObject);
var
   x, y: Integer;
   s: String;
begin
   x := FormStateInit.Left + BtnAmmonia.Left;
   y := FormStateInit.Top + BtnAmmonia.Top;
   with ComboAmmonia do begin
      case ItemIndex of
         0: begin
               InitOneEdit(x, y, 'Ammonium content', 'Ammonium content',
                           'ppm', 3, Temp2StateVars.AmmoniaContent, -1
                           , HLP_SAC_Uniform_Profile);
               if FormOneEdit.ShowModal = mrOK then begin
                  s := ReadOneEdit;
                  if s = '' then
                     Temp2StateVars.AmmoniaContent := NotSet
                  else
                     Temp2StateVars.AmmoniaContent := StrToFloat(s);
               end;
            end;
         1: begin
               InitTable(x, y, Items[ItemIndex], 'Depth (cm)', 'Amm. - N (ppm)'
                         , Temp2StateVars.NumAmmoniaVsDepthItems
                         , Temp2StateVars.AmmoniaVsDepth, 1
                         , HLP_SAC_Ammonia_vs_Depth);
               FormTable.ShowModal;
               if FormTable.ModalResult = mrOK then
                  Temp2StateVars.NumAmmoniaVsDepthItems := ReadTable(Temp2StateVars.AmmoniaVsDepth);
            end;
      end;
   end;
end;

procedure PlaceFormStateInit(x, y: Integer);
begin
   with FormStateInit do begin
      Left := x;
      Top := y - Height;
   end;
end;

procedure TFormStateInit.FormShow(Sender: TObject);
begin
   { Update the visible components on the form }
   FormStateInit.SetFormFromVars;
   ComboWater.SetFocus; { Make the form ready for entering data }
   FormStateInit.Update;
end;


procedure TFormStateInit.BtnOKClick(Sender: TObject);
begin
   { Update vars from form... (all checks should be done here)}
   { Update might also have been done earlier (on exit from edit-boxes etc.), but ... }
   SetVarsFromForm;

   { ...and then it's ok }
   ModalResult := mrOK;
end;

procedure TFormStateInit.BtnHelpClick(Sender: TObject);
begin
   Application.HelpContext(HLP_StateVar_Init);
end;

end.
