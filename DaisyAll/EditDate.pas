unit EditDate;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, Mask, IntAct, TypInfo;

type
  TFormDate = class(TForm)
    MEditDate: TMaskEdit;
    LblDateTitle: TLabel;
    LblDateFormat: TLabel;
    BtnHelp: TBitBtn;
    BtnCancel: TBitBtn;
    BtnOK: TBitBtn;
    procedure BtnHelpClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MEditDateExit(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);
  private
    HLPText: Integer;
    { Private declarations }
  public
    { Public declarations }
    procedure InitFromAction(Title: String; Action: TInternAction; HlpTxt, x, y: Integer);
    procedure SetActionDateFromFormDate(var Action: TInternAction);
  end;

var
  FormDate: TFormDate;

implementation
uses Globals;

{$R *.DFM}
var
   OriginalEmptyDate: String;


procedure TFormDate.InitFromAction(Title: String; Action: TInternAction; HlpTxt, x, y: Integer);
var
   StrDate: String;
begin
   FormDate.Left := x;
   FormDate.Top  := y;
   FormDate.Caption := LoadStr(RES_FRM_EditDate) + ' ' + Title;
   LblDateTitle.Caption := Title;
   MEditDate.HelpContext := HlpTxt;
   BtnOK.HelpContext := HlpTxt;
   BtnCancel.HelpContext := HlpTxt;
   BtnHelp.HelpContext := HlpTxt;
   HLPText := HlpTxt;

   if Action.when <> nil then begin
      with Action.when as TDate do
         StrDate := Date(TRUE);  { TRUE => 0-padded date format }
      MEditDate.EditText := Copy(StrDate, 1, 4) + MEditDate.EditText[5] +
                            Copy(StrDate, 6, 2) + MEditDate.EditText[8] +
                            Copy(StrDate, 9, 2);
   end
   else
      MEditDate.EditText := OriginalEmptyDate;
(*
      StrDate := '    -  -  ';  { blank date }

   MEditDate.EditText := Copy(StrDate, 1, 4) + MEditDate.EditText[5] +
                         Copy(StrDate, 6, 2) + MEditDate.EditText[8] +
                         Copy(StrDate, 9, 2);
*)
end;

procedure TFormDate.SetActionDateFromFormDate(var Action: TInternAction);
var
   StrDate: String;
begin
   { Copy date to format expected by Action.SetDato() }
   StrDate := Copy(MEditDate.Text, 1, 4);                 { copy year }
   StrDate := StrDate + '-' + Copy(MEditDate.Text, 6, 2); { copy month }
   StrDate := StrDate + '-' + Copy(MEditDate.Text, 9, 2); { copy date }
   if CompareStr(MEditDate.EditText, OriginalEmptyDate) = 0 then
      Action.SetDato(StrDate) { Invalid date => resets Action.When to nil }
   else
      if not Action.SetDato(StrDate) then { Here the date should work }
         MessageDlg(LoadStr(RES_ERR_EditDate_Date_Could_Not_Be_Set)
                     + ' ' + LoadStr(RES_ERR_Developer),
                        mtError, [mbOK], 0);
end;


procedure TFormDate.BtnHelpClick(Sender: TObject);
begin
   Application.HelpContext(HLPText);
end;

procedure TFormDate.FormCreate(Sender: TObject);
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

   { Button captions }
   BtnOk.Caption     := LoadStr(RES_BTN_OK);
   BtnCancel.Caption := LoadStr(RES_BTN_Cancel);
   BtnHelp.Caption   := LoadStr(RES_BTN_Help);

   { Label captions }
   LblDateFormat.Caption := '(' + LoadStr(RES_MSC_yyyy)
                              + MEditDate.EditText[5]       // y/m separator
                              + LoadStr(RES_MSC_mm)
                              + MEditDate.EditText[8]       // m/d separator
                              + LoadStr(RES_MSC_dd) + ')';

   { We do not set 'LblDateTitle.Caption' as it is set to a string supplied by
     the caller. }
   // LblDateTitle.Caption := '';

   OriginalEmptyDate := MEditDate.EditText;
end;

procedure TFormDate.FormShow(Sender: TObject);
begin
   MEditDate.SetFocus;
end;

procedure TFormDate.MEditDateExit(Sender: TObject);
var
   Day, Month, Year, OldFormatString: String;
   x, y: Integer;
begin
   { It is allowed to leave the form at any time... }
   if (ActiveControl <> BtnCancel) then begin
      { Check if valid values }
      with MEditDate do begin
         Year := Trim(Copy(Text, 1, 4));
         Month := Trim(Copy(Text, 6, 2));
         Day   := Trim(Copy(Text, 9, 2));
         x := Left + FormDate.Left - 50;
         y := Top + FormDate.Top + 50;

         if (Month <> '') or (Day <> '') or (Year <> '') then begin  // All blank => ok
            if (Month = '') or (Day = '') or (Year = '') then begin  // One blank => not valid
               MessageDlgPos(LblDateTitle.Caption + ':'  + CHR(10) + CHR(13)
                              + LoadStr(RES_ERR_EditDate_YYMMDD_Val_or_blank),
                                 mtError, [mbOK], 0, x, y);
               SetFocus;
               exit;
            end;
            { Check if valid date... }
            OldFormatString := ShortDateFormat;
            ShortDateFormat := 'y/m/d';
            try
               StrToDate(Text);
            except
               on E:Exception do begin
                  MessageDlgPos(LblDateTitle.Caption + ':'  + CHR(10) + CHR(13)
                                 + LoadStr(RES_ERR_EditDate_Not_Valid_Date),
                                    mtError, [mbOK], 0, x, y);
                  SetFocus;
                  exit;
               end;
            end;
            ShortDateFormat := OldFormatString;
            { OK, now format nicely... }
            if Length(Year) < 4 then
               Year := Copy('0000', 1, 4-Length(Year)) + Year;
            if Length(Month) = 1 then
               Month := '0' + Month;
            if Length(Day) = 1 then
               Day := '0' + Day;
            EditText := Year + EditText[5] + Month + EditText[8] + Day;
         end;  { if (Year <> '') or (Month <> '') or (Day <> '') }
      end;  { with MEditDate do begin }
   end;  { if (ActiveControl <> BtnCancel) then begin }
end;

procedure TFormDate.BtnOKClick(Sender: TObject);
var
   Day, Month, Year, OldFormatString: String;
   x, y: Integer;
begin
   { Check if valid values }
   with MEditDate do begin
      Year := Trim(Copy(Text, 1, 4));
      Month := Trim(Copy(Text, 6, 2));
      Day   := Trim(Copy(Text, 9, 2));
      x := Left + FormDate.Left - 50;
      y := Top + FormDate.Top + 50;

      if (Month <> '') or (Day <> '') or (Year <> '') then begin  // All blank => ok
         if (Month = '') or (Day = '') or (Year = '') then begin  // One blank => not valid
            MessageDlgPos(LblDateTitle.Caption + ':'  + CHR(10) + CHR(13)
                              + LoadStr(RES_ERR_EditDate_YYMMDD_Val_or_blank),
                                 mtError, [mbOK], 0, x, y);
            SetFocus;
            ModalResult := mrNone;
            exit;
         end;
         { Check if valid date... }
         OldFormatString := ShortDateFormat;
         ShortDateFormat := 'y/m/d';
         try
            StrToDate(Text);
         except
            on E:Exception do begin
               MessageDlgPos(LblDateTitle.Caption + ':'  + CHR(10) + CHR(13)
                                 + LoadStr(RES_ERR_EditDate_Not_Valid_Date),
                                    mtError, [mbOK], 0, x, y);
               SetFocus;
               ModalResult := mrNone;
               exit;
            end;
         end;
         ShortDateFormat := OldFormatString;
         { OK, now format nicely... }
         if Length(Year) < 4 then
            Year := Copy('0000', 1, 4-Length(Year)) + Year;
         if Length(Month) = 1 then
            Month := '0' + Month;
         if Length(Day) = 1 then
            Day := '0' + Day;
         EditText := Year + EditText[5] + Month + EditText[8] + Day;
      end;  { if (Year <> '') or (Month <> '') or (Day <> '') }
   end;  { with MEditDate do begin }
   ModalResult := mrOK;
end;

end.
