unit LibComp;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, FileCtrl, Globals, TypInfo;

type
   TFormSelFromLib = class(TForm)
      LBSelFromLib: TListBox;
      BtnHelp: TBitBtn;
      BtnCancel: TBitBtn;
      BtnOK: TBitBtn;
      RGSelectLib: TRadioGroup;
      LblSelect: TLabel;
    procedure RGSelectLibClick(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LBSelFromLibDblClick(Sender: TObject);
   private
      { Private declarations }
   public
      procedure Init(t: Integer; hd: String); { Initializes standard/user library (directory) + Heading/Label }
      { Public declarations }
   end;

var
   FormSelFromLib: TFormSelFromLib;
   moduletype: Integer;  { 0:Manager, 1:GlobalInfo, 2:Profile }

implementation

{$R *.DFM}

procedure TFormSelFromLib.Init(t: Integer; hd: String); { Initializes standard/user library (directory) }
begin
   Caption := 'Select ' + hd;
   moduletype := t;
   LblSelect.Caption := 'Select ' + hd;
end;


procedure TFormSelFromLib.RGSelectLibClick(Sender: TObject);
begin
   LBSelFromLib.Items.Clear;

   { Fill listbox from selected lib }
   case RGSelectLib.ItemIndex of
      0: case moduletype of
            0: LBSelFromLib.Items.AddStrings(MainFormVars.StdTemplLibManItems);
            1: LBSelFromLib.Items.AddStrings(MainFormVars.StdTemplLibGlobItems);
            2: LBSelFromLib.Items.AddStrings(MainFormVars.StdTemplLibProfItems);
            3: LBSelFromLib.Items.AddStrings(MainFormVars.StdTemplLibHorItems);
         end;
      1: case moduletype of
            0: LBSelFromLib.Items.AddStrings(MainFormVars.UserTemplLibManItems);
            1: LBSelFromLib.Items.AddStrings(MainFormVars.UserTemplLibGlobItems);
            2: LBSelFromLib.Items.AddStrings(MainFormVars.UserTemplLibProfItems);
            3: LBSelFromLib.Items.AddStrings(MainFormVars.UserTemplLibHorItems);
         end;
   end
end;

procedure TFormSelFromLib.BtnOKClick(Sender: TObject);
begin
   if LBSelFromLib.ItemIndex > -1 then
      ModalResult := mrOK
   else begin
      MessageDlgPos('Nothing is selected.'
                  + CHR(10) + CHR(13)
                  + 'Please make a selection from the list before pressing <OK>.'
                  , mtInformation, [mbOK], 0
                  , FormSelFromLib.Left - 20
                  , FormSelFromLib.Top + 20);
      LBSelFromLib.SetFocus;
   end;
end;

procedure TFormSelFromLib.FormCreate(Sender: TObject);
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

{ Init strings... }
{...}
end;

procedure TFormSelFromLib.LBSelFromLibDblClick(Sender: TObject);
begin
   BtnOkClick(Sender);
end;

end.
