unit TwoEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, Globals, TypInfo;

type
  TFormTwoEdit = class(TForm)
    BtnOK: TBitBtn;
    BtnCancel: TBitBtn;
    BtnHelp: TBitBtn;
    EditTwoEdit1: TEdit;
    LblTwoEdit1: TLabel;
    EditTwoEdit2: TEdit;
    LblTwoEdit2: TLabel;
    LblTwoEditUnits1: TLabel;
    LblTwoEditUnits2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure EditTwoEdit1Exit(Sender: TObject);
    procedure EditTwoEdit2Exit(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BtnHelpClick(Sender: TObject);
    procedure EditTwoEdit1Enter(Sender: TObject);
    function CheckValidFields(msg: Boolean): Boolean;
    procedure BtnHelpExit(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

procedure InitTwoEdit(x, y:Integer; title, lbl1, units1, lbl2, units2: String;
                      nt1, nt2: Integer; s1, s2: String);
procedure SetTwoEditHelp(hlp1, hlp2, hlp3: Integer);
procedure ReadTwoEdit(var s1, s2: String);

var
  FormTwoEdit: TFormTwoEdit;

implementation

{$R *.DFM}

var
   { NumberType: 1 - 3 Decimal Fraction       }
   {             2 - 3 Decimal Positive Float }
   NumberType1, NumberType2: Integer;

   LastActive: ^TEdit;
   hlp: Integer;


procedure InitTwoEdit(x, y:Integer; title, lbl1, units1, lbl2, units2: String; nt1, nt2: Integer; s1, s2: String);
var
   MinWidth, TextWidth1, TextWidth2, Spacing, TempWidth: Integer;
begin
   Spacing := 7;
   NumberType1 := nt1;
   NumberType2 := nt2;
   with FormTwoEdit do begin
      { Set the text... }
      Caption := title;
      LblTwoEdit1.Caption := lbl1;
      LblTwoEditUnits1.Caption := units1;
      LblTwoEdit2.Caption := lbl2;
      LblTwoEditUnits2.Caption := units2;
      MinWidth := BtnOK.Width + BtnCancel.Width + BtnHelp.Width + 4*Spacing;
      TextWidth1 := LblTwoEdit1.Width + EditTwoEdit1.Width +
                   LblTwoEditUnits1.Width + 4*Spacing;
      TextWidth2 := LblTwoEdit2.Width + EditTwoEdit2.Width +
                   LblTwoEditUnits2.Width + 4*Spacing;

      { Set width after text... }
      if (MinWidth>TextWidth1) and (MinWidth>TextWidth2)  then
         TempWidth := MinWidth
      else
         if TextWidth1<TextWidth2 then
            TempWidth := TextWidth2
         else
            TempWidth := TextWidth1;

      ClientWidth := TempWidth; { Must be done like this ...or buggy bugs!!! }

      { Position buttons end text... }
      BtnHelp.Left := TempWidth - Spacing - BtnHelp.Width;
      BtnCancel.Left := BtnHelp.Left - Spacing - BtnCancel.Width;
      BtnOK.Left := BtnCancel.Left - Spacing - BtnHelp.Width;
      LblTwoEditUnits1.Left := TempWidth - Spacing - LblTwoEditUnits1.Width;
      EditTwoEdit1.Left := LblTwoEditUnits1.Left - Spacing - EditTwoEdit1.Width;
      LblTwoEdit1.Left := EditTwoEdit1.Left - Spacing - LblTwoEdit1.Width;
      LblTwoEditUnits2.Left := TempWidth - Spacing - LblTwoEditUnits2.Width;
      EditTwoEdit2.Left := LblTwoEditUnits2.Left - Spacing - EditTwoEdit2.Width;
      LblTwoEdit2.Left := EditTwoEdit2.Left - Spacing - LblTwoEdit2.Width;

      Left := x - Width;
      Top := y - (Height div 2);

      { Now set values... }
      if s1 = FloatToStr(NotSet) then EditTwoEdit1.Text := ''
      else EditTwoEdit1.Text := s1;
      if s2 = FloatToStr(NotSet) then EditTwoEdit2.Text := ''
      else EditTwoEdit2.Text := s2;
      { Format the numbers nicely... Invalid numbers => blank! }
      case NumberType1 of
         1: begin
               try EditTwoEdit1.Text := StrTo3DecFracStr(EditTwoEdit1.Text)
               except on e: Exception do
                  EditTwoEdit1.Text := '';
               end;
            end;
         2: begin
               try EditTwoEdit1.Text := StrTo3DecPosStr(EditTwoEdit1.Text)
               except on e: Exception do
                  EditTwoEdit1.Text := '';
               end;
            end;
      end;
      case NumberType2 of
         1: begin
               try EditTwoEdit2.Text := StrTo3DecFracStr(EditTwoEdit2.Text)
               except on e: Exception do
                  EditTwoEdit2.Text := '';
               end;
            end;
         2: begin
               try EditTwoEdit2.Text := StrTo3DecPosStr(EditTwoEdit2.Text)
               except on e: Exception do
                  EditTwoEdit2.Text := '';
               end;
            end;
      end;
   end;
end;

procedure SetTwoEditHelp(hlp1, hlp2, hlp3: Integer);
begin
   with FormTwoEdit do begin
      EditTwoEdit1.HelpContext := hlp1; { helptext for first edit box }
      EditTwoEdit2.HelpContext := hlp2; { helptext for second edit box }
      BtnOK.HelpContext := hlp3;        { same helptext for all buttons }
      BtnCancel.HelpContext := hlp3;
      BtnHelp.HelpContext := hlp3;
   end;
   hlp := hlp3; { General help to come on pressed helpbutton }
end;

procedure ReadTwoEdit(var s1, s2: String);
begin
   s1 := Trim(FormTwoEdit.EditTwoEdit1.Text);
   s2 := Trim(FormTwoEdit.EditTwoEdit2.Text);
end;

procedure TFormTwoEdit.FormCreate(Sender: TObject);
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

   { Button captions }
   BtnOK.Caption     := LoadStr(RES_BTN_Ok);
   BtnCancel.Caption := LoadStr(RES_BTN_Cancel);
   BtnHelp.Caption   := LoadStr(RES_BTN_Help);
end;

function TFormTwoEdit.CheckValidFields(msg: Boolean): Boolean;
var
   x, y: Integer;
begin
   x := FormTwoEdit.Left - 150;
   y := FormTwoEdit.Top - 80;
   Result := FALSE; { Fear the worst... }

{ EditTwoEdit1 - Start check here }
   with EditTwoEdit1 do begin
      Text := Trim(Text); { Get rid of leading/trailing spaces}
      if Text <> '' then begin { '' is a valid value! }
         case NumberType1 of { 3 decimal fraction - the only one needed so far... }
            1: begin
                  try Text := StrTo3DecFracStr(Text);
                  except
                     on E: Exception do begin
                        if msg then
                           MessageDlgPos(LblTwoEdit1.Caption + ':' + CHR(10) + CHR(13)
                                         + FracErrMsg, mtInformation, [mbOK], 0, x, y);
                        SetFocus;
                        exit;
                     end;
                  end;
               end;
            2: begin
                  try Text := StrTo3DecPosStr(Text);
                  except
                     on E: Exception do begin
                        if msg then
                           MessageDlgPos(LblTwoEdit1.Caption + ':' + CHR(10) + CHR(13)
                                         + 'Not a valid positive floating point number!',
                                          mtInformation, [mbOK], 0, x, y);
                        SetFocus;
                        exit;
                     end;
                  end;
               end;
         end; { case }
      end; { if Text <> ''  }
   end; { with EditTwoEdit1 do }
{ EditTwoEdit1 - End check here }

{ EditTwoEdit2 - Start check here }
   with EditTwoEdit2 do begin
      Text := Trim(Text); { Get rid of leading/trailing spaces}
      if Text <> '' then begin { '' is a valid value! }
         case NumberType2 of
            1: begin
                  try Text := StrTo3DecFracStr(Text);
                  except
                     on E: Exception do begin
                        if msg then
                           MessageDlgPos(LblTwoEdit2.Caption + ':' + CHR(10) + CHR(13)
                                         + FracErrMsg, mtInformation, [mbOK], 0, x, y+25);
                        SetFocus;
                        exit;
                     end;
                  end;
               end;
            2: begin { 3 decimal pos float - the only one needed so far... }
                  try Text := StrTo3DecPosStr(Text);
                  except
                     on E: Exception do begin
                        if msg then
                           MessageDlgPos(LblTwoEdit2.Caption + ':' + CHR(10) + CHR(13)
                                         + 'Not a valid positive floating point number!',
                                          mtInformation, [mbOK], 0, x, y+25);
                        SetFocus;
                        exit;
                     end;
                  end;
               end;
         end; { case }
      end; { if Text <> ''  }
   end; { with EditTwoEdit2 do }
{ EditTwoEdit2 - End check here }

{ Everything Checked OK :-) }
   Result := TRUE;
end;

procedure TFormTwoEdit.EditTwoEdit1Exit(Sender: TObject);
(*
var
   x, y: Integer;
*)
begin
   { It is allowed to leave the form at any time... }
   if (ActiveControl <> BtnCancel) and (ActiveControl <> BtnHelp) then begin
      { Check if valid values }
      if not CheckValidFields(TRUE) then
         exit;
      { No variables should be updated here - use the ReadTwoEdit procedure.  }
   end;
(*
   { It is allowed to leave the form at any time... }
   if (ActiveControl <> BtnCancel) and (ActiveControl <> BtnHelp) then begin
      with EditTwoEdit1 do begin
         { ...but not to leave an invalid fraction in the edit box}
         Text := Trim(Text); { Get rid of leading/trailing spaces}
         if Text <> '' then begin
            if NumberType1 = 1 then  { 3 decimal fraction - the only one needed so far... }
               try Text := StrTo3DecFracStr(Text);
               except
                  on E: Exception do begin
                     x := Left + FormTwoEdit.Left - 250;
                     y := Top + FormTwoEdit.Top - 80;
                     MessageDlgPos('Not a valid fraction! ' +
                                   'A fraction must be in the interval [0.0, 1.0],' +
                                   ' using the systems national decimal point',
                                   mtInformation, [mbOK], 0, x, y);
                     SetFocus;
                  end;
               end;
         end;
      end;
   end;
*)
end;

procedure TFormTwoEdit.EditTwoEdit2Exit(Sender: TObject);
(*
var
   x, y: Integer;
*)
begin
   { It is allowed to leave the form at any time... }
   if (ActiveControl <> BtnCancel) and (ActiveControl <> BtnHelp) then begin
      { Check if valid values }
      if not CheckValidFields(TRUE) then
         exit;
      { No variables should be updated here - use the ReadTwoEdit procedure.  }
   end;
(*
   { It is allowed to leave the form at any time... }
   if (ActiveControl <> BtnCancel) and (ActiveControl <> BtnHelp) then begin
      with EditTwoEdit2 do begin
         { ...but not to leave an invalid float in the edit box}
         Text := Trim(Text); { Get rid of leading/trailing spaces}
         if Text <> '' then begin
            if NumberType2 = 1 then  { 3 decimal pos real - the only one needed so far... }
               try Text := StrTo3DecPosStr(Text);
               except
                  on E: Exception do begin
                     x := Left + FormTwoEdit.Left - 250;
                     y := Top + FormTwoEdit.Top - 80;
                     MessageDlgPos('Not a valid positive floating point number!',
                                    mtInformation, [mbOK], 0, x, y);
                     SetFocus;
                  end;
               end;
         end;
      end;
   end;
*)
end;

procedure TFormTwoEdit.BtnOKClick(Sender: TObject);
(*
var
   x, y: Integer;
*)
begin
   if not CheckValidFields(TRUE) then begin
      LastActive^.SetFocus;
      exit;
   end
   else
      ModalResult := mrOK;

(*   { No invalid values in the controls here, so just check for blanks... }
   if (Trim(EditTwoEdit1.Text) = '')
         or (Trim(EditTwoEdit2.Text) = '') then begin
      x := BtnOk.Left + FormTwoEdit.Left - 80;
      y := BtnOk.Top + FormTwoEdit.Top - 80;
      if MessageDlgPos('Some of the input fields are left blank!'
                       + CHR(10) + CHR(13) + 'Select <OK> to leave them blank, '
                       + 'or <Cancel> to return to the input fields.',
                        mtInformation, [mbOK, mbCancel], 0, x, y)
               = mrOK then begin
         ModalResult := mrOK;
      end
      else
         LastActive^.SetFocus;
   end
   else
      ModalResult := mrOK;
*)
end;

procedure TFormTwoEdit.FormShow(Sender: TObject);
begin
   EditTwoEdit1.SetFocus; { Make the form ready for entering data }
   LastActive := @EditTwoEdit1;
end;

procedure TFormTwoEdit.BtnHelpClick(Sender: TObject);
begin
   { First show help }
   Application.HelpContext(hlp);
end;

procedure TFormTwoEdit.BtnHelpExit(Sender: TObject);
begin
   { Then set focus back to last active control... }
   LastActive^.SetFocus;
end;

procedure TFormTwoEdit.EditTwoEdit1Enter(Sender: TObject);
begin
   with Sender as TEdit do begin
      if Name = 'EditTwoEdit1' then LastActive := @EditTwoEdit1;
      if Name = 'EditTwoEdit2' then LastActive := @EditTwoEdit2;
   end;
end;


end.
