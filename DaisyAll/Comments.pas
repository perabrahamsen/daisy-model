unit Comments;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, Globals, TypInfo;

type
  TFormComments = class(TForm)
    MemoComments: TMemo;
    BtnOK: TBitBtn;
    BtnCancel: TBitBtn;
    BtnHelp: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);
    function CheckValidFields(mgs: Boolean): Boolean;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

procedure InitComments(x, y: Integer; title: String; S: String);
procedure ReadComments(var S: String);

var
  FormComments: TFormComments;

implementation

{$R *.DFM}

function TFormComments.CheckValidFields(mgs: Boolean): Boolean;
begin
   { Anything is valid at time being :) }
   Result := TRUE;
end;

procedure InitComments(x, y: Integer; title: String; S: String);
begin
   with FormComments do begin
      Caption := title;
      Left := x - Width;
      Top := y;
      MemoComments.Text := S;
   end;
end;

procedure ReadComments(var S: String);
begin
   S := FormComments.MemoComments.Text;
end;

procedure TFormComments.FormCreate(Sender: TObject);
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

   { Set button captions }
   BtnOK.Caption     := LoadStr(RES_BTN_Ok);
   BtnCancel.Caption := LoadStr(RES_BTN_Cancel);
   BtnHelp.Caption   := LoadStr(RES_BTN_Help);
end;

procedure TFormComments.BtnOKClick(Sender: TObject);
begin
   if not CheckValidFields(TRUE) then
      exit;
   ModalResult := mrOK;
end;

procedure TFormComments.FormShow(Sender: TObject);
begin
   { Just to make life easier... }
   MemoComments.SetFocus;
end;

end.
