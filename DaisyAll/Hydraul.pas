unit Hydraul;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, Globals, TypInfo;

type
  TFormHydraulic = class(TForm)
    LblHydraulic1: TLabel;
    LblHydraulic2: TLabel;
    LblHydraulic3: TLabel;
    LblHydraulic4: TLabel;
    LblHydraulic5: TLabel;
    EditHydraulic1: TEdit;
    EditHydraulic2: TEdit;
    EditHydraulic3: TEdit;
    EditHydraulic4: TEdit;
    EditHydraulic5: TEdit;
    LblHydraulic1x: TLabel;
    LblHydraulic2x: TLabel;
    LblHydraulic3x: TLabel;
    LblHydraulic4x: TLabel;
    LblHydraulic5x: TLabel;
    BtnOK: TBitBtn;
    BtnCancel: TBitBtn;
    BtnHelp: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure EditHydraulic1_2Exit(Sender: TObject);
    procedure EditHydraulic3Exit(Sender: TObject);
    procedure EditHydraulic4Exit(Sender: TObject);
    procedure EditHydraulic5Exit(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);
    procedure EditHydraulic1Enter(Sender: TObject);
    procedure BtnHelpClick(Sender: TObject);
    function CheckValidFields(msg: Boolean): Boolean;
    procedure EditHydraulicAnyExit(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  procedure InitHydraulic(x, y, i: Integer; title: string; s1, s2, s3, s4, s5: String);
  procedure ReadHydraul(var s1, s2, s3, s4, s5: String);

var
  FormHydraulic: TFormHydraulic;

implementation
var
   HydrType: Integer;
   LastActive: ^TEdit;
   CurrentHelp: Integer;

{$R *.DFM}

function TFormHydraulic.CheckValidFields(msg: Boolean): Boolean;
var
   x, y: Integer;
begin
   x := FormHydraulic.Left - 100;
   y := FormHydraulic.Top - 100;
   Result := FALSE; { Fear the worst... }

{ EditHydraulic1 - Start check here }
   with EditHydraulic1 do begin
      Text := Trim(Text); { Get rid of leading/trailing spaces}
      if Text <> '' then begin { '' is a valid value! }
         case HydrType of
            0, 1, 2, 3: { At current time, all HydrType's gives same first edit box }
               begin
                  try Text := StrTo3DecFracStr(Text);
                  except
                     on E: Exception do begin
                        if msg then
                           MessageDlgPos(LblHydraulic1.Caption + ':' + CHR(10) + CHR(13)
                                         + FracErrMsg, mtInformation, [mbOK], 0, x, y);
                        SetFocus;
                        exit;
                     end;
                  end;
               end;
         end; { case i of }
      end; { if Text <> ''  }
   end; {  with EditHydraulic1 }
{ EditHydraulic1 - End check here }

{ EditHydraulic2 - Start check here }
   with EditHydraulic2 do begin
      Text := Trim(Text); { Get rid of leading/trailing spaces}
      if Text <> '' then begin { '' is a valid value! }
         case HydrType of
            0, 1, 2, 3: { At current time, all HydrType's gives same second edit box }
               begin
                  try Text := StrTo3DecFracStr(Text);
                  except
                     on E: Exception do begin
                        if msg then
                           MessageDlgPos(LblHydraulic2.Caption + ':' + CHR(10) + CHR(13)
                                         + FracErrMsg, mtInformation, [mbOK], 0, x, y+25);
                        SetFocus;
                        exit;
                     end;
                  end;
               end;
         end; { case i of }
      end; { if Text <> ''  }
   end; {  with EditHydraulic1 }
{ EditHydraulic2 - End check here }

{ EditHydraulic3 - Start check here }
   with EditHydraulic3 do begin
      Text := Trim(Text); { Get rid of leading/trailing spaces}
      if Text <> '' then begin { '' is a valid value! }
         case HydrType of
            0, 1: begin
                     try Text := StrToPosFloatStr(Text);
                     except
                        on E: Exception do begin
                           if msg then
                              MessageDlgPos(LblHydraulic3.Caption + ':' + CHR(10) + CHR(13)
                                            + 'Not a valid positive floating point number!',
                                            mtInformation, [mbOK], 0, x, y+50);
                           SetFocus;
                           exit;
                        end;
                     end;
                  end;
            2..4: begin
                     try StrToFloat(Text);
                     except
                        on E: Exception do begin
                           if msg then
                              MessageDlgPos(LblHydraulic3.Caption + ':' + CHR(10) + CHR(13)
                                            + 'Not a valid floating point number!',
                                            mtInformation, [mbOK], 0, x, y+50);
                           SetFocus;
                           exit;
                        end;
                     end;
                     { Make Bubbling Pressure negative when leaving the edit box }
                     if Text[1]<>'-' then
                        Text := '-' + Text;
                  end;
         end; { case i of }
      end; { if Text <> ''  }
   end; { with EditHydraulic3 }
{ EditHydraulic3 - End check here }

{ EditHydraulic4 - Start check here }
   with EditHydraulic4 do begin
      Text := Trim(Text); { Get rid of leading/trailing spaces}
      if Text <> '' then begin { '' is a valid value! }
         case HydrType of
            0, 1: begin
                     try Text := StrToPosFloatStr(Text);
                     except
                        on E: Exception do begin
                           if msg then
                              MessageDlgPos(LblHydraulic4.Caption + ':' + CHR(10) + CHR(13)
                                            + 'Not a valid positive floating point number!',
                                            mtInformation, [mbOK], 0, x, y+75);
                           SetFocus;
                           exit;
                        end;
                     end;
                  end;
            2..4: begin
                     try Text := StrToPosFloatStr(Text);
                     except
                        on E: Exception do begin
                           MessageDlgPos(LblHydraulic4.Caption + ':' + CHR(10) + CHR(13)
                                         + 'Not a valid positive floating point number!',
                                         mtInformation, [mbOK], 0, x, y+75);
                           SetFocus;
                           exit;
                        end;
                     end;
                  end;
         end; { case i of }
      end; { if Text <> ''  }
   end; { with EditHydraulic4 }
{ EditHydraulic4 - End check here }

{ EditHydraulic5 - Start check here }
   with EditHydraulic5 do begin
      Text := Trim(Text); { Get rid of leading/trailing spaces}
      if Text <> '' then begin { '' is a valid value! }
         case HydrType of
            0, 1, 2, 3: { At current time, all HydrType's gives same fifth edit box }
               begin
                  try Text := StrToPosFloatStr(Text);
                  except
                     on E: Exception do begin
                        if msg then
                           MessageDlgPos(LblHydraulic4.Caption + ':' + CHR(10) + CHR(13)
                                         + 'Not a valid positive floating point number!',
                                         mtInformation, [mbOK], 0, x, y+100);
                        SetFocus;
                        exit;
                     end;
                  end;
               end;
         end; { case HydrType of }
      end; { if Text <> ''  }
   end; { with EditHydraulic5 }
{ EditHydraulic5 - End check here }

{ EditHydraulic1 + EditHydraulic2 - Start dependency check here }
   case HydrType of 0, 1, 2, 3:
      { Check if Sat. Water Cont. > Res. Water Cont. }
      if (Trim(EditHydraulic1.Text) <> '')
            and (Trim(EditHydraulic2.Text) <> '') then begin
         if StrToFloat(EditHydraulic1.Text) <= StrToFloat(EditHydraulic2.Text) then begin
            MessageDlgPos(LblHydraulic1.Caption
                        + ' must be greater than ' + LblHydraulic2.Caption + '.',
                        mtInformation, [mbOK], 0, x, y);
               BtnOK.SetFocus;
            exit;
         end;
      end;
   end;
{ EditHydraulic1 + EditHydraulic2 - End dependency check here }

{ Everything Checked OK :-) }
   Result := TRUE;
end;


procedure InitHydraulic(x, y, i: Integer; title: String; s1, s2, s3, s4, s5: String);
begin
   HydrType := i;
   with FormHydraulic do begin
      Left := x - Width;
      Top := y - (Height div 2);
      Caption := title;
      if s1 = FloatToStr(NotSet) then EditHydraulic1.Text := ''
      else EditHydraulic1.Text := s1;
      if s2 = FloatToStr(NotSet) then EditHydraulic2.Text := ''
      else EditHydraulic2.Text := s2;
      if s3 = FloatToStr(NotSet) then EditHydraulic3.Text := ''
      else EditHydraulic3.Text := s3;
      if s4 = FloatToStr(NotSet) then EditHydraulic4.Text := ''
      else EditHydraulic4.Text := s4;
      if s5 = FloatToStr(NotSet) then EditHydraulic5.Text := ''
      else EditHydraulic5.Text := s5;

      { Set overall help... }
      case HydrType of
         0: CurrentHelp := HLP_HydrProp_van_Genuchten_Mualem;
         1: CurrentHelp := HLP_HydrProp_van_Genuchten_Burdine;
         2: CurrentHelp := HLP_HydrProp_Brooks_and_Corey_Mualem;
         3: CurrentHelp := HLP_HydrProp_Brooks_and_Corey_Burdine;
         4: CurrentHelp := HLP_HydrProp_Modified_Brooks_and_Corey_Smith_92;
      end;
      BtnOK.HelpContext := CurrentHelp;
      BtnCancel.HelpContext := CurrentHelp;
      BtnHelp.HelpContext := CurrentHelp;

      case HydrType of
         0, 1: begin
                  LblHydraulic1.Caption := 'Saturated Water Content';
                  LblHydraulic2.Caption := 'Residual Water Content';
                  LblHydraulic3.Caption := 'van Genuchten "alpha"';
                  LblHydraulic4.Caption := 'van Genuchten n';
                  LblHydraulic5.Caption := 'Saturated Hydraulic Conductivity';
                  LblHydraulic1x.Caption := '(fraction)';
                  LblHydraulic2x.Caption := '(fraction)';
                  LblHydraulic3x.Caption := 'cm-1';
                  LblHydraulic4x.Caption := '(real)';
                  LblHydraulic5x.Caption := 'cm/h';
                  { Set the proper helpcontexts... }
                  EditHydraulic1.HelpContext := HLP_Saturated_Water_Content;
                  EditHydraulic2.HelpContext := HLP_Residual_Water_Content;
                  EditHydraulic3.HelpContext := HLP_van_Genuchten_alpha;
                  EditHydraulic4.HelpContext := HLP_van_Genuchten_n;
                  EditHydraulic5.HelpContext := HLP_Saturated_Hydraulic_Conductivity;
                  { Format the numbers nicely... Invalid numbers => blank! }
                  try EditHydraulic1.Text := StrTo3DecFracStr(EditHydraulic1.Text)
                  except on e: Exception do
                     EditHydraulic1.Text := '';
                  end;
                  try EditHydraulic2.Text := StrTo3DecFracStr(EditHydraulic2.Text)
                  except on e: Exception do
                     EditHydraulic2.Text := '';
                  end;
                  try EditHydraulic3.Text := StrToPosFloatStr(EditHydraulic3.Text)
                  except on e: Exception do
                     EditHydraulic3.Text := '';
                  end;
                  try EditHydraulic4.Text := StrToPosFloatStr(EditHydraulic4.Text)
                  except on e: Exception do
                     EditHydraulic4.Text := '';
                  end;
                  try EditHydraulic5.Text := StrToPosFloatStr(EditHydraulic5.Text)
                  except on e: Exception do
                     EditHydraulic5.Text := '';
                  end;
               end;
         2..4: begin
                  LblHydraulic1.Caption := 'Saturated Water Content';
                  LblHydraulic2.Caption := 'Residual Water Content';
                  LblHydraulic3.Caption := 'Bubling Pressure';
                  LblHydraulic4.Caption := 'Pore-size Distribution Index';
                  LblHydraulic5.Caption := 'Saturated Hydraulic Conductivity';
                  LblHydraulic1x.Caption := '(fraction)';
                  LblHydraulic2x.Caption := '(fraction)';
                  LblHydraulic3x.Caption := 'cm';
                  LblHydraulic4x.Caption := '';
                  LblHydraulic5x.Caption := 'cm/h';
                  { Set the proper helpcontexts... }
                  EditHydraulic1.HelpContext := HLP_Saturated_Water_Content;
                  EditHydraulic2.HelpContext := HLP_Residual_Water_Content;
                  EditHydraulic3.HelpContext := HLP_Bubling_Pressure;
                  EditHydraulic4.HelpContext := HLP_Pore_size_Distribution_Index;
                  EditHydraulic5.HelpContext := HLP_Saturated_Hydraulic_Conductivity;
                  { Format the numbers nicely... Invalid numbers => blank! }
                  try EditHydraulic1.Text := StrTo3DecFracStr(EditHydraulic1.Text)
                  except on e: Exception do
                     EditHydraulic1.Text := '';
                  end;
                  try EditHydraulic2.Text := StrTo3DecFracStr(EditHydraulic2.Text)
                  except on e: Exception do
                     EditHydraulic2.Text := '';
                  end;
                  try StrToFloat(EditHydraulic3.Text)
                  except on e: Exception do
                     EditHydraulic3.Text := '';
                  end;
                  try EditHydraulic4.Text := StrToPosFloatStr(EditHydraulic4.Text)
                  except on e: Exception do
                     EditHydraulic4.Text := '';
                  end;
                  try EditHydraulic5.Text := StrToPosFloatStr(EditHydraulic5.Text)
                  except on e: Exception do
                     EditHydraulic5.Text := '';
                  end;
               end;
      else
            begin
                  LblHydraulic1.Caption := 'BUG!!';
                  LblHydraulic2.Caption := 'BUG!!';
                  LblHydraulic3.Caption := 'BUG!!';
                  LblHydraulic4.Caption := 'BUG!!';
                  LblHydraulic5.Caption := 'BUG!!';
                  LblHydraulic1x.Caption := 'BUG!!';
                  LblHydraulic2x.Caption := 'BUG!!';
                  LblHydraulic3x.Caption := 'BUG!!';
                  LblHydraulic4x.Caption := 'BUG!!';
                  LblHydraulic5x.Caption := 'BUG!!';
            end;
      end;
   end;
end;

procedure ReadHydraul(var s1, s2, s3, s4, s5: String);
begin
   if Trim(FormHydraulic.EditHydraulic1.Text) = '' then s1 := FloatToStr(NotSet)
   else s1 := Trim(FormHydraulic.EditHydraulic1.Text);
   if Trim(FormHydraulic.EditHydraulic2.Text) = '' then s2 := FloatToStr(NotSet)
   else s2 := Trim(FormHydraulic.EditHydraulic2.Text);
   if Trim(FormHydraulic.EditHydraulic3.Text) = '' then s3 := FloatToStr(NotSet)
   else s3 := Trim(FormHydraulic.EditHydraulic3.Text);
   if Trim(FormHydraulic.EditHydraulic4.Text) = '' then s4 := FloatToStr(NotSet)
   else s4 := Trim(FormHydraulic.EditHydraulic4.Text);
   if Trim(FormHydraulic.EditHydraulic5.Text) = '' then s5 := FloatToStr(NotSet)
   else s5 := Trim(FormHydraulic.EditHydraulic5.Text);
end;

procedure TFormHydraulic.FormCreate(Sender: TObject);
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
   HydrType := 0;
   LastActive := nil;
end;

procedure TFormHydraulic.EditHydraulic1_2Exit(Sender: TObject);
var
   x, y: Integer;
begin
(*
   { It is allowed to leave the form at any time... }
   if (ActiveControl <> BtnCancel) and (ActiveControl <> BtnHelp) then begin
      { ...but not to leave an invalid fraction in the edit box}
      with Sender as TEdit do begin
         Text := Trim(Text); { Get rid of leading/trailing spaces}
         if Text <> '' then begin
            case HydrType of
               0, 1: begin
                        try Text := StrTo3DecFracStr(Text);
                        except
                           on E: Exception do begin
                              x := Left + FormHydraulic.Left - 100;
                              y := Top + FormHydraulic.Top - 100;
                              MessageDlgPos('Not a valid fraction! ' + CHR(10) + CHR(13) +
                                            'A fraction must be in the interval [0.0, 1.0],' +
                                            CHR(10) + CHR(13) +
                                            ' using the systems national decimal point',
                                             mtInformation, [mbOK], 0, x, y);
                              SetFocus;
                           end;
                        end;
                     end;
               2..4: begin
                        try Text := StrTo3DecFracStr(Text);
                        except
                           on E: Exception do begin
                              x := Left + FormHydraulic.Left - 100;
                              y := Top + FormHydraulic.Top - 100;
                              MessageDlgPos('Not a valid fraction! ' + CHR(10) + CHR(13) +
                                            'A fraction must be in the interval [0.0, 1.0],' +
                                            CHR(10) + CHR(13) +
                                            ' using the systems national decimal point',
                                             mtInformation, [mbOK], 0, x, y);
                              SetFocus;
                           end;
                        end;
                     end;
            end; { case i of }
         end; { if Text <> ''  }
      end; { with Sender as TEdit }
   end; { if (ActiveControl <> BtnCancel) }
*)
end;

procedure TFormHydraulic.EditHydraulic3Exit(Sender: TObject);
var
   x, y: Integer;
   GoodNum: Bool;
begin
(*
   { It is allowed to leave the form at any time... }
   if (ActiveControl <> BtnCancel) and (ActiveControl <> BtnHelp) then begin
      { ...but not to leave an invalid float in the edit box}
      with Sender as TEdit do begin
         Text := Trim(Text); { Get rid of leading/trailing spaces}
         if Text <> '' then begin
            case HydrType of
               0, 1: begin
                        try Text := StrToPosFloatStr(Text);
                        except
                           on E: Exception do begin
                              x := Left + FormHydraulic.Left - 100;
                              y := Top + FormHydraulic.Top - 100;
                              MessageDlgPos('Not a valid positive floating point number!',
                                             mtInformation, [mbOK], 0, x, y);
                              SetFocus;
                           end;
                        end;
                     end;
               2..4: begin
                        GoodNum := TRUE;
                        try StrToFloat(Text);
                        except
                           on E: Exception do begin
                              x := Left + FormHydraulic.Left - 100;
                              y := Top + FormHydraulic.Top - 100;
                              MessageDlgPos('Not a valid floating point number!',
                                             mtInformation, [mbOK], 0, x, y);
                              GoodNum := FALSE;
                              SetFocus;
                           end;
                        end;
                        { Make Bubbling Pressure negative when leaving the edit box }
                        if GoodNum and (Text[1]<>'-') then
                           Text := '-' + Text;
                     end;
            end; { case i of }
         end; { if Text <> ''  }
      end; { with Sender as TEdit }
   end; { if (ActiveControl <> BtnCancel) }
*)
end;

procedure TFormHydraulic.EditHydraulic4Exit(Sender: TObject);
var
   x, y: Integer;
begin
(*
   { It is allowed to leave the form at any time... }
   if (ActiveControl <> BtnCancel) and (ActiveControl <> BtnHelp) then begin
      { ...but not to leave an invalid float in the edit box}
      with Sender as TEdit do begin
         Text := Trim(Text); { Get rid of leading/trailing spaces}
         if Text <> '' then begin
            case HydrType of
               0, 1: begin
                        try Text := StrToPosFloatStr(Text);
                        except
                           on E: Exception do begin
                              x := Left + FormHydraulic.Left - 100;
                              y := Top + FormHydraulic.Top - 100;
                              MessageDlgPos('Not a valid positive floating point number!',
                                             mtInformation, [mbOK], 0, x, y);
                              SetFocus;
                           end;
                        end;
                     end;
               2..4: begin
                        try Text := StrToPosFloatStr(Text);
                        except
                           on E: Exception do begin
                              x := Left + FormHydraulic.Left - 100;
                              y := Top + FormHydraulic.Top - 100;
                              MessageDlgPos('Not a valid positive floating point number!',
                                             mtInformation, [mbOK], 0, x, y);
                              SetFocus;
                           end;
                        end;
                     end;
            end; { case i of }
         end; { if Text <> ''  }
      end; { with Sender as TEdit }
   end; { if (ActiveControl <> BtnCancel) }
*)
end;

procedure TFormHydraulic.EditHydraulic5Exit(Sender: TObject);
var
   x, y: Integer;
begin
(*
   { It is allowed to leave the form at any time... }
   if (ActiveControl <> BtnCancel) and (ActiveControl <> BtnHelp) then begin
      { ...but not to leave an invalid float in the edit box}
      with Sender as TEdit do begin
         Text := Trim(Text); { Get rid of leading/trailing spaces}
         if Text <> '' then begin
            case HydrType of
               0, 1: begin
                        try Text := StrToPosFloatStr(Text);
                        except
                           on E: Exception do begin
                              x := Left + FormHydraulic.Left - 100;
                              y := Top + FormHydraulic.Top - 100;
                              MessageDlgPos('Not a valid positive floating point number!',
                                             mtInformation, [mbOK], 0, x, y);
                              SetFocus;
                           end;
                        end;
                     end;
               2..4: begin
                        try Text := StrToPosFloatStr(Text);
                        except
                           on E: Exception do begin
                              x := Left + FormHydraulic.Left - 100;
                              y := Top + FormHydraulic.Top - 100;
                              MessageDlgPos('Not a valid positive floating point number!',
                                             mtInformation, [mbOK], 0, x, y);
                              SetFocus;
                           end;
                        end;
                     end;
            end; { case i of }
         end; { if Text <> ''  }
      end; { with Sender as TEdit }
   end; { if (ActiveControl <> BtnCancel) }
*)
end;

procedure TFormHydraulic.FormShow(Sender: TObject);
begin
//   LastActive := @EditHydraulic1;
   EditHydraulic1.SetFocus; { Make the form ready for entering data }
end;

procedure TFormHydraulic.BtnOKClick(Sender: TObject);
//var
//   x, y: Integer;
//   SatOK: Boolean;
begin
   if not CheckValidFields(TRUE) then
      exit
   else
      ModalResult := mrOK;

(*
   SatOK := TRUE;
   x := BtnOk.Left + FormHydraulic.Left - 80;
   y := BtnOk.Top + FormHydraulic.Top - 80;

   case HydrType of 0, 1, 2, 3:
      { Check if Sat. Water Cont. > Res. Water Cont. }
      if (Trim(EditHydraulic1.Text) <> '')
            and (Trim(EditHydraulic2.Text) <> '') then begin
         if StrToFloat(EditHydraulic1.Text) <= StrToFloat(EditHydraulic2.Text) then begin
            MessageDlgPos('Saturated Water Content'
                        + ' must be greater than ' + 'Residual Water Content' + '.',
                        mtInformation, [mbOK], 0, x, y);
            EditHydraulic1.SetFocus;
            SatOK := FALSE;
         end;
      end;
   end;
   { No invalid values in the controls here, so just check for blanks... }
   if SatOK then begin
      if (Trim(EditHydraulic1.Text) = '')
            or (Trim(EditHydraulic2.Text) = '')
            or (Trim(EditHydraulic3.Text) = '')
            or (Trim(EditHydraulic4.Text) = '')
            or (Trim(EditHydraulic5.Text) = '') then begin
         if MessageDlgPos('Some of the input fields are left blank!'
                       + CHR(10) + CHR(13) + 'Select <OK> to leave them blank, '
                       + 'or <Cancel> to return to the input fields.',
                        mtInformation, [mbOK, mbCancel], 0, x, y)
               = mrOK then begin
            ModalResult := mrOK;
         end
         else
            LastActive.SetFocus;
      end
      else
         ModalResult := mrOK;
   end; { if SatOK... }
*)
end;

procedure TFormHydraulic.EditHydraulic1Enter(Sender: TObject);
begin
(*
   with Sender as TEdit do begin
      if Name = 'EditHydraulic1' then LastActive := @EditHydraulic1;
      if Name = 'EditHydraulic2' then LastActive := @EditHydraulic2;
      if Name = 'EditHydraulic3' then LastActive := @EditHydraulic3;
      if Name = 'EditHydraulic4' then LastActive := @EditHydraulic4;
      if Name = 'EditHydraulic5' then LastActive := @EditHydraulic5;
   end;
*)
end;

procedure TFormHydraulic.BtnHelpClick(Sender: TObject);
begin
   Application.HelpContext(CurrentHelp);
//   LastActive^.SetFocus;
end;

procedure TFormHydraulic.EditHydraulicAnyExit(Sender: TObject);
begin
   { It is allowed to leave the form at any time... }
   if (ActiveControl <> BtnCancel) then begin
      { Check if valid values }
      if not CheckValidFields(TRUE) then
         exit;
      { No variables should be updated here - use the ReadHydraulic procedure.  }
   end;
end;

end.
