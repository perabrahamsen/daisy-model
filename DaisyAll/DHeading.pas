unit DHeading;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, Globals, TypInfo;

type
  TOKBottomDlg1 = class(TForm)
    EditHeading: TEdit;
    BtnOK: TBitBtn;
    BtnCancel: TBitBtn;
    LblHeading: TLabel;
    BitBtn1: TBitBtn;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    function GetHeading:String;
    procedure SetHeading(h:String);
  public
    { Public declarations }
    property Heading:String read GetHeading write SetHeading;
  end;

var
  OKBottomDlg1: TOKBottomDlg1;

implementation

{$R *.DFM}

function TOKBottomDlg1.GetHeading:String;
begin
   Result := EditHeading.Text;
end;

procedure TOKBottomDlg1.SetHeading(h:String);
begin
   EditHeading.Text := h;
end;

procedure TOKBottomDlg1.FormCreate(Sender: TObject);
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
end;

end.
