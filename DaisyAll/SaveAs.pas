unit SaveAs;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, Globals, TypInfo, ExtCtrls;

type
  TFormSaveAs = class(TForm)
    EditHeading2: TEdit;
    EditHeading1: TEdit;
    LblSaveAs: TLabel;
    BtnHelp: TBitBtn;
    BtnCancel: TBitBtn;
    BtnOK: TBitBtn;
    RGSelLib: TRadioGroup;
    procedure BtnOKClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    ModType: Integer;   {  Current module type        }
                        {     1: Global Information   }
                        {     2: Soil Horizon         }
                        {     3: Soil Profile         }
                        {     4: Manager              }
  public
    { Public declarations }
    procedure Init(ModuleType, x, y: Integer); { see above for moduletypes }
    procedure Read(var SelName: string; var SelLib: Integer); { Selected name and library to save in }
  end;

var
  FormSaveAs: TFormSaveAs;

implementation

{$R *.DFM}

procedure TFormSaveAs.Init(ModuleType, x, y: Integer); { see above for moduletypes }
var
   s: String;
begin
   EditHeading2.Text := '';
   EditHeading1.Text := '';
   { First position the form properly }
   FormSaveAs.Left := x;
   FormSaveAs.Top  := y;
   { Specialize for specific modultype }
   ModType := ModuleType;
   case ModType of
      1: s := 'Global Information';
      2: s := 'Soil Horizon';
      3: s := 'Soil Profile';
      4: s := 'Manager';
   end;
   LblSaveAs.Caption := 'The current ' + s
                        + ' will be saved to the selected library,'
                        + ' using the name entered below.';
   case ModuleType of
      1,3,4 :  begin
                  EditHeading1.Visible := FALSE;
                  EditHeading2.Left  := BtnOK.Left;
                  EditHeading2.Width := BtnHelp.Left + BtnHelp.Width - BtnOK.Left;
               end;
      2     :  begin
                  EditHeading1.Visible := TRUE;
                  EditHeading2.Left  := EditHeading1.Left + EditHeading1.Width + 5;
                  EditHeading2.Width := BtnHelp.Left + BtnHelp.Width
                                        - EditHeading1.Left - EditHeading1.Width - 5;
               end;
   end;
end;

procedure TFormSaveAs.Read(var SelName: string; var SelLib: Integer); { Selected name and library to save in }
begin
   EditHeading2.Text := Trim(EditHeading2.Text);
   EditHeading1.Text := Trim(EditHeading1.Text);
   case ModType of
      1,3,4 :  SelName := EditHeading2.Text;
      2     :  MkHorizonName(EditHeading1.Text, EditHeading2.Text, SelName);
   end;
   SelLib := RGSelLib.ItemIndex;
end;


procedure TFormSaveAs.BtnOKClick(Sender: TObject);
var
   ValidHeading: Boolean;
   s: String;
begin
   EditHeading2.Text := Trim(EditHeading2.Text);
   EditHeading1.Text := Trim(EditHeading1.Text);
   ValidHeading := TRUE;
   case ModType of
      1,3,4 :  if EditHeading2.Text = '' then
                  ValidHeading := FALSE;
      2     :  if (EditHeading1.Text = '') or (EditHeading2.Text = '') then
                  ValidHeading := FALSE;
   end;
   if ValidHeading then
      ModalResult := mrOK
   else begin
      case ModType of
         1: s := 'Global Information';
         2: s := 'Soil Horizon';
         3: s := 'Soil Profile';
         4: s := 'Manager';
      end;
      { Prompt for heading }
      MessageDlgPos('A valid ' + s + ' heading must be entered before selecting <OK>.',
                  mtInformation, [mbOK], 0
                  , FormSaveAs.Left - 20
                  , FormSaveAs.Top + 50);
      EditHeading2.SetFocus;
      ModalResult := mrNone; // Should not be neccesary :-(
   end;
end;

procedure TFormSaveAs.BtnCancelClick(Sender: TObject);
begin
   ModalResult := mrCancel;
end;

procedure TFormSaveAs.FormCreate(Sender: TObject);
var
   i: Integer;
begin
   { Now scale the form and its components... }
   Font.Style := [];
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

end;

end.
