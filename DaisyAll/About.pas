unit About;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, TypInfo, Globals;

type
  TFormAbout = class(TForm)
    ImageAbout: TImage;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    LblTitle1: TLabel;
    LblVersion1: TLabel;
    LblDevBy: TLabel;
    LblDevBy1: TLabel;
    LblCopyRight1: TLabel;
    LblCopyRight2: TLabel;
    BtnOK: TBitBtn;
    LblTitle2: TLabel;
    LblVersion2: TLabel;
    LblDevBy2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormAbout: TFormAbout;

implementation
Uses UseLib;
{$R *.DFM}

procedure TFormAbout.FormCreate(Sender: TObject);
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
   BtnOk.Caption        := LoadStr(RES_BTN_Ok);

   { Set form captions }
   FormAbout.Caption    := LoadStr(RES_FRM_About);

   { Set label captions }
   LblTitle1.Caption    := LoadStr(RES_LBL_Version_Title1);
   LblVersion1.Caption  := LoadStr(RES_LBL_Version) + ' ' + Daisy_DLL_Version;
   LblTitle2.Caption    := LoadStr(RES_LBL_Version_Title2);
   LblVersion2.Caption  := LoadStr(RES_LBL_Version) + ' ' + Daisy_GUI_Version;
   LblDevBy.Caption     := LoadStr(RES_LBL_Developer_Title);
   LblDevBy1.Caption    := LoadStr(RES_LBL_Developer1);
   LblDevBy2.Caption    := LoadStr(RES_LBL_Developer2);
   LblCopyRight1.Caption:= LoadStr(RES_LBL_Copyright);
   LblCopyRight2.Caption:= LoadStr(RES_LBL_Copyright_By);
end;

procedure TFormAbout.FormShow(Sender: TObject);
begin
   BtnOK.SetFocus; { Make the form ready for easy exit (bad sales strategy ;-)) }
end;

end.
