unit OneEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, Globals, TypInfo;

type
  TFormOneEdit = class(TForm)
    EditOneEdit: TEdit;
    LblOneEdit: TLabel;
    BtnOK: TBitBtn;
    BtnCancel: TBitBtn;
    BtnHelp: TBitBtn;
    LblOneEditUnits: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    function CheckValidFields(msg: Boolean): Boolean;
    procedure BtnHelpClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

procedure InitOneEdit(x, y:Integer; title, lbl, units: String;
                      it: Integer; deflt, max: Real; Hlp1: Integer);
function ReadOneEdit: String;

var
  FormOneEdit: TFormOneEdit;

implementation

uses profil;

{$R *.DFM}

var
   InputType: Integer; { Decides which filter that applies to input in edit box }
                       { 0:Fixed Groundwater  1:Lysimeter depth  }
                       { 2: Nitrate concentration }
                       { 3: Ammonia content }
   MaxValue: Real;  { Maximum value of input. (<0) => infinit }
   Hlp: Integer; { HelpTopic to be shown on BtnHelpClick and context sensitive }

function TFormOneEdit.CheckValidFields(msg: Boolean): Boolean;
var
   x, y: Integer;
begin
   x := EditOneEdit.Left + FormOneEdit.Left - 250;
   y := EditOneEdit.Top + FormOneEdit.Top - 100;
   Result := FALSE; { Fear the worst... }

   with EditOneEdit do begin
      Text := Trim(Text); { Get rid of leading/trailing spaces}

//      if Text = '' then begin { '' is a valid value! }
//         Result := TRUE;
//         exit;
//      end;
//      { Text <> '' }
      case InputType of
         0, 1: begin { 0:Fixed Groundwater  1:Lysimeter depth  }
                  try Text := StrTo1DecPosStr(Text);
                  except
                     on E: Exception do begin
                        if msg then
                           MessageDlgPos('Not a valid positive floating point number!',
                                          mtInformation, [mbOK], 0, x, y);
                        SetFocus;
                        exit;
                     end;
                  end;
                  { Check if < lowest horizon }
                  if (MaxValue >= 0) and (StrToFloat(Text) > MaxValue) then begin
                     if msg then
                        MessageDlgPos('The depth can not be below the deepest horizon in the profile!'
                                      + CHR(10) + CHR(13) + 'Please enter a valid depth less than '
                                      + FormatFloat('0.0', MaxValue) + ' cm.',
                                       mtInformation, [mbOK], 0, x, y);
                     SetFocus;
                     exit;
                  end;
               end;
         2:    begin  { Nitrate concentration }
                  try Text := StrTo1DecPosNotZeroStr(Text);
                  except
                     on E: Exception do begin
                        if msg then
                           MessageDlgPos('Not a valid positive floating point number!',
                                          mtInformation, [mbOK], 0, x, y);
                        SetFocus;
                        exit;
                     end;
                  end;
                  { Check if < max allowed }
                  if (MaxValue >= 0) and (StrToFloat(Text) > MaxValue) then begin
                     if msg then
                        MessageDlgPos('The nitrate concentration can not be this high!?!'
                                      + CHR(10) + CHR(13) + 'Please enter a nitrate concentration less than '
                                      + FormatFloat('0.0', MaxValue) + ' ' + LblOneEditUnits.Caption + '.',
                                       mtInformation, [mbOK], 0, x, y);
                     SetFocus;
                     exit;
                  end;
               end;
         3:    begin  { Ammonia content }
                  try Text := StrTo1DecPosNotZeroStr(Text);
                  except
                     on E: Exception do begin
                        if msg then
                           MessageDlgPos('Not a valid positive floating point number!',
                                          mtInformation, [mbOK], 0, x, y);
                        SetFocus;
                        exit;
                     end;
                  end;
                  { Check if < max allowed }
                  if (MaxValue >= 0) and (StrToFloat(Text) > MaxValue) then begin
                     if msg then
                        MessageDlgPos('The ammonia content can not be this high!?!'
                                      + CHR(10) + CHR(13) + 'Please enter a valid ammonia content less than '
                                      + FormatFloat('0.0', MaxValue) + ' ' + LblOneEditUnits.Caption + '.',
                                       mtInformation, [mbOK], 0, x, y);
                     SetFocus;
                     exit;
                  end
               end;
      end;  { case InputType of }
   end;  { with EditOneEdit do begin }

{ Everything Checked OK :-) }
   Result := TRUE;
end;

procedure InitOneEdit(x, y:Integer; title, lbl, units: String;
                      it: Integer; deflt, max: Real; Hlp1: Integer);
var
   MinWidth, TextWidth, Spacing, TempWidth: Integer;
   s: String;
begin
   Spacing := 7;
   InputType := it;
   MaxValue := max;
   with FormOneEdit do begin
      { Set the text... }
      Caption := title;
      LblOneEdit.Caption := lbl;
      LblOneEditUnits.Caption := units;
      if (deflt < 0) or (deflt = NotSet) then
         EditOneEdit.Text := ''
      else
         EditOneEdit.Text := FormatFloat('0.0', deflt); { Format while FloatToStr returns rounderrors #¤%#£$}
      MinWidth := BtnOK.Width + BtnCancel.Width + BtnHelp.Width + 4*Spacing;
      TextWidth := LblOneEdit.Width + EditOneEdit.Width +
                   LblOneEditUnits.Width + 4*Spacing;

      { Set width after text... }
      if MinWidth>TextWidth then
         TempWidth := MinWidth
      else
         TempWidth := TextWidth;

      ClientWidth := TempWidth; { Must be done like this ...or buggy bugs!!! }

      { Position buttons end text... }
      BtnHelp.Left := TempWidth - Spacing - BtnHelp.Width;
      BtnCancel.Left := BtnHelp.Left - Spacing - BtnCancel.Width;
      BtnOK.Left := BtnCancel.Left - Spacing - BtnHelp.Width;
      LblOneEditUnits.Left := TempWidth - Spacing - LblOneEditUnits.Width;
      EditOneEdit.Left := LblOneEditUnits.Left - Spacing - EditOneEdit.Width;
      LblOneEdit.Left := EditOneEdit.Left - Spacing - LblOneEdit.Width;

      Left := x - Width;
      Top := y - Height div 2;

      { Set Global and context help }
      Hlp := Hlp1;
      EditOneEdit.HelpContext := Hlp;
   end;
end;

function ReadOneEdit: String;
begin
   Result := Trim(FormOneEdit.EditOneEdit.Text);
end;

procedure TFormOneEdit.FormCreate(Sender: TObject);
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

      { Init Buttons... }
   BtnOK.Caption := '&OK';
   BtnCancel.Caption := '&Cancel';
   BtnHelp.Caption := '&Help';
end;

procedure TFormOneEdit.BtnOKClick(Sender: TObject);
(*
var
   x, y: Integer;
   GoodNum: Bool;
*)
begin
   if not CheckValidFields(TRUE) then
      exit
   else
      ModalResult := mrOK;
(*
   EditOneEdit.Text := Trim(EditOneEdit.Text); { Get rid of leading/trailing spaces}
   if EditOneEdit.Text <> '' then begin
      GoodNum := TRUE;
      case InputType of
         0, 1: begin { 0:Fixed Groundwater  1:Lysimeter depth  }
                  try EditOneEdit.Text := StrTo1DecPosStr(EditOneEdit.Text);
                  except
                     on E: Exception do begin
                        GoodNum := FALSE;
                        x := EditOneEdit.Left + FormOneEdit.Left - 250;
                        y := EditOneEdit.Top + FormOneEdit.Top - 80;
                        MessageDlgPos('Not a valid positive floating point number!',
                                       mtInformation, [mbOK], 0, x, y);
                        EditOneEdit.SetFocus;
                     end;
                  end;
                  if GoodNum then begin { Check if < lowest horizon }
                        if (MaxValue >= 0) and
                              (StrToFloat(EditOneEdit.Text) > MaxValue) then begin
                           x := EditOneEdit.Left + FormOneEdit.Left - 250;
                           y := EditOneEdit.Top + FormOneEdit.Top - 80;
                           MessageDlgPos('The depth can not be below the deepest horizon in the profile!'
                                         + CHR(10) + CHR(13) + 'Please enter a valid depth below '
                                         + FormatFloat('0.0', MaxValue) + ' cm.',
                                          mtInformation, [mbOK], 0, x, y);
                           EditOneEdit.SetFocus;
                        end
                        else
                           ModalResult := mrOK; { Return from form }
                  end;
               end;
         2:    begin  { Nitrate concentration }
                  try StrToFloat(EditOneEdit.Text);
                  except
                     on E: Exception do begin
                        GoodNum := FALSE;
                        x := EditOneEdit.Left + FormOneEdit.Left - 250;
                        y := EditOneEdit.Top + FormOneEdit.Top - 80;
                        MessageDlgPos('Not a valid floating point number!',
                                       mtInformation, [mbOK], 0, x, y);
                        EditOneEdit.SetFocus;
                     end;
                  end;
                  if GoodNum then begin { Check if < max allowed }
                        if (MaxValue >= 0) and
                              (StrToFloat(EditOneEdit.Text) > MaxValue) then begin
                           x := EditOneEdit.Left + FormOneEdit.Left - 250;
                           y := EditOneEdit.Top + FormOneEdit.Top - 80;
                           MessageDlgPos('The nitrate concentration can not be this high!?!'
                                         + CHR(10) + CHR(13) + 'Please enter a nitrate concentration below '
                                         + FormatFloat('0.0', MaxValue) + ' ' + LblOneEditUnits.Caption + '.',
                                          mtInformation, [mbOK], 0, x, y);
                           EditOneEdit.SetFocus;
                        end
                        else
                           ModalResult := mrOK; { Return from form }
                  end;
               end;
         3:    begin  { Ammonia content }
                  try StrToFloat(EditOneEdit.Text);
                  except
                     on E: Exception do begin
                        GoodNum := FALSE;
                        x := EditOneEdit.Left + FormOneEdit.Left - 250;
                        y := EditOneEdit.Top + FormOneEdit.Top - 80;
                        MessageDlgPos('Not a valid floating point number!',
                                       mtInformation, [mbOK], 0, x, y);
                        EditOneEdit.SetFocus;
                     end;
                  end;
                  if GoodNum then begin { Check if < max allowed }
                        if (MaxValue >= 0) and
                              (StrToFloat(EditOneEdit.Text) > MaxValue) then begin
                           x := EditOneEdit.Left + FormOneEdit.Left - 250;
                           y := EditOneEdit.Top + FormOneEdit.Top - 80;
                           MessageDlgPos('The ammonia content can not be this high!?!'
                                         + CHR(10) + CHR(13) + 'Please enter a valid ammonia content below '
                                         + FormatFloat('0.0', MaxValue) + ' ' + LblOneEditUnits.Caption + '.',
                                          mtInformation, [mbOK], 0, x, y);
                           EditOneEdit.SetFocus;
                        end
                        else
                           ModalResult := mrOK; { Return from form }
                  end;
               end;
      end;
   end
   else begin { edit.text = '' }
      x := EditOneEdit.Left + FormOneEdit.Left - 250;
      y := EditOneEdit.Top + FormOneEdit.Top - 80;
      if MessageDlgPos('The input field is left blank!'
                       + CHR(10) + CHR(13) + 'Select <OK> to leave it blank, '
                       + 'or <Cancel> to return to the input field.',
                        mtInformation, [mbOK, mbCancel], 0, x, y)
               = mrOK then begin
         ModalResult := mrOK;
      end
      else
         EditOneEdit.SetFocus;
   end;
*)
end;


procedure TFormOneEdit.FormShow(Sender: TObject);
begin
   EditOneEdit.SetFocus;  { Make the form ready for entering data }
end;

procedure TFormOneEdit.BtnHelpClick(Sender: TObject);
begin
   Application.HelpContext(Hlp);
end;

end.
