unit DsyTime;
{
***************
* Unit DsyTime *
***************

Project    : Graphical User Interface for the Daisy simulation package

Purpose    : Daisy Time Component
             Delphi Component to manage the editing of "time of action"
             (
             NOTE: Some support functions are in the Globals UNIT, because
                   the use in the different Edit"Action" Windows are the *same*
             )

Author     : J&R Informatik I/S (Jens Jeppesen)

}

interface
uses SysUtils,WinTypes,WinProcs,Messages,Classes,Graphics,Controls,
     Forms,Dialogs,StdCtrls,ExtCtrls,spin, Mask;

Const szHeight = 145;
      szWidth  = 320;
type
   TGetActionsEvent = procedure (List:TStrings; var Index:Integer) of object;
   SimplePanel = class(TPanel)
   constructor Create(AOwner:TComponent); override;
end;

TDaisyTimeComp=class(SimplePanel)
   private
   init          : Boolean;
   Dirty         : Boolean;
   Setting       : Boolean;
   types         : TStringList;
   pType         : Integer; { 0 = Date, 1 = CaDate, 2 = Rel. to Action, 3 = ONLYDATE }
   pLblTimeType  : TLabel;
   pLblActions   : TLabel;
   pLblDate      : TLabel;
   pLblDays      : TLabel;
   pTimeType     : TComboBox;
   pActions      : TComboBox;
//   pDate         : TEdit;
   pDateM        : TMaskEdit;
   pETimeType    : TEdit;
   pDays         : TSpinEdit;
   pGB           : TGroupBox;
   pGetActions   : TGetActionsEvent;
   pOnlyDate     : Boolean;
   DateLabelText : string;

   // Error messages
   ErrorMessageHeadText    : string;
   ErrorMessageBlankText   : string;
   ErrorMessageInvalidText : string;

   procedure Size;
   procedure Show;
   procedure PanelResize(Sender: TObject);
   procedure TimeTypeChange(Sender: TObject);
   procedure ActionsChange(Sender: TObject);
   procedure GetActions;
   procedure DaysChange(Sender:TObject);
   procedure DateChange(Sender:TObject);
   procedure DateExit(Sender:TObject);
   function GetItemIndex:Integer;
   function GetObject : TObject;
   procedure SetOnlyDate(pod:Boolean);
   procedure SetEnabled(e: Boolean);
   function GetEnabled: Boolean;
   function GetCaption : TCaption;
   procedure SetCaption(Caption : TCaption);
   function GetTimeTypeLabel : TLabel;
   function GetDisplacementLabel : TLabel;
   function GetActionsLabel : TLabel;
   procedure SetDateLabel(Caption : String);

   // Subprograms used by 'TimeTypeDate', 'TimeTypeApprox' and
   // 'TimeTypeRelative' write properties.
   procedure SetTimeTypeDate(Caption : string);
   procedure SetTimeTypeApprox(Caption : string);
   procedure SetTimeTypeRelative(Caption : string);

   // Subprograms used by 'ErrorMessageHead', 'ErrorMessageBlank' and
   // 'ErrorMessageInvalid' write properties.
   procedure SetErrorMessageHead(MessageText : string);
   procedure SetErrorMessageBlank(MessageText : string);
   procedure SetErrorMessageInvalid(MessageText : string);

public
   constructor Create(AOwner:TComponent); override;
   procedure SetDate(Date:String);
   procedure SetCaDate(Date:String);
   procedure SetRelTo(Days:Integer);
   procedure GetTime(Var T:Integer;Var Str:String;Var D:Integer);
   procedure ClearTime;
   function CheckDate: Integer;  { 0: Date OK, 1: Parts missing, 2: Invalid Date}
published
   // Caption of Time component
   property Caption           : TCaption read GetCaption write SetCaption;

   // Label of Time type combobox
   property TimeTypeLabel     : TLabel   read GetTimeTypeLabel;

   // Labels of actions and displacement fields
   property DisplacementLabel : TLabel   read GetDisplacementLabel;
   property ActionsLabel      : TLabel   read GetActionsLabel;

   // This label is used only if we are in One-Date-Only mode
   property DateLabel         : string   write SetDateLabel;

   // Write properties for setting the label for the 3 time types
   // (absolut, approx., relative)
   property TimeTypeDate     : string write SetTimeTypeDate;
   property TimeTypeApprox   : string write SetTimeTypeApprox;
   property TimeTypeRelative : string write SetTimeTypeRelative;

   // Set head of error messages ('Invalid date')
   property ErrorMessageHead    : string write SetErrorMessageHead;
   // Set error message (all fields must be blank or all non-blank)
   property ErrorMessageBlank   : string write SetErrorMessageBlank;
   // Set error message (invalid calendar date)
   property ErrorMessageInvalid : string write SetErrorMessageInvalid;

   property OnGetActions : TGetActionsEvent read pGetActions write pGetActions;
   property Changed : Boolean read Dirty;
   property Enabled : Boolean read GetEnabled write SetEnabled;
   property TheIndex : Integer read GetItemIndex;
   property TheObject : TObject read GetObject;
   property OnlyDate : Boolean read pOnlyDate write SetOnlyDate;
end;

procedure Register;

implementation
var
   OriginalEmptyDate: String;

// Caption property begins
function TDaisyTimeComp.GetCaption : TCaption;
begin
   Result := pGB.Caption;
end;

procedure TDaisyTimeComp.SetCaption(Caption : TCaption);
begin
   pGB.Caption := Caption;
end;
// Caption property ends

// TimeTypeLabel property begins
function TDaisyTimeComp.GetTimeTypeLabel : TLabel;
begin
   Result := pLblTimeType;
end;

function TDaisyTimeComp.GetDisplacementLabel : TLabel;
begin
   Result := pLblDays;
end;
// TimeTypeLabel property ends

function TDaisyTimeComp.GetActionsLabel : TLabel;
begin
   Result := pLblActions;
end;

// One-Date-Only 'DateLabel' property
procedure TDaisyTimeComp.SetDateLabel(Caption : String);
begin
   DateLabelText := Caption;
end;

// TimeType properties

// First entry: 'Date'
procedure TDaisyTimeComp.SetTimeTypeDate(Caption : string);
begin
   Types.Strings[0] := Caption;
end;

// Second entry: 'Approx. Date'
procedure TDaisyTimeComp.SetTimeTypeApprox(Caption : string);
begin
   Types.Strings[1] := Caption;
end;

// Third entry: 'Relative to Action'
procedure TDaisyTimeComp.SetTimeTypeRelative(Caption : string);
begin
   Types.Strings[2] := Caption;
end;

// Error message properties
procedure TDaisyTimeComp.SetErrorMessageHead(MessageText : string);
begin
   ErrorMessageHeadText := MessageText;
end;

procedure TDaisyTimeComp.SetErrorMessageBlank(MessageText : string);
begin
   ErrorMessageBlankText := MessageText;
end;

procedure TDaisyTimeComp.SetErrorMessageInvalid(MessageText : string);
begin
   ErrorMessageInvalidText := MessageText;
end;


procedure TDaisyTimeComp.SetEnabled(e: Boolean);
begin
   if e then
      pGB.Font.Color := clWindowText
   else
      pGB.Font.Color := clInactiveCaption; { Neccesary to make text disabled }
   pGB.Enabled := e;
   pLblTimeType.Enabled := e;
   pLblActions.Enabled := e;
   pLblDate.Enabled := e;
   pLblDays.Enabled := e;
   pTimeType.Enabled := e;
   pActions.Enabled := e;
//   pDate.Enabled := e;
   pDateM.Enabled := e;
   pETimeType.Enabled := e;
   pDays.Enabled := e;
end;
function TDaisyTimeComp.GetEnabled;
begin
   Result := pGB.Enabled;
end;

constructor SimplePanel.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   Caption := ' ';
end;

function TDaisyTimeComp.GetItemIndex:Integer;
begin
   Result := pActions.ItemIndex;
end;

function TDaisyTimeComp.GetObject : TObject;
begin
   Result := pActions.Items.Objects[GetItemIndex];
end;

procedure TDaisyTimeComp.SetOnlyDate(pod:Boolean);
begin
   pOnlyDate := pod;
   Show;
end;

constructor TDaisyTimeComp.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   {Align := alBottom;}
   BevelOuter := bvNone;
   init := False;
   Dirty := False;
   Setting := False;
   width := szWidth;
   height := szHeight;
   OnResize := PanelResize;
   {pType := 0;}

   types := TStringList.Create;

   // The next 3 lines are used to initialize the list entries only. The actual
   // entries are defined via properties 'TimeTypeDate', 'TimeTypeApprox' and
   // 'TimeTypeRelative'.
   types.Add('Date');
   types.Add('Approx. Date');
   types.Add('Relative to Action');

   pGB := TGroupBox.Create(Self);
   with pGB do begin
      Parent := self;
      Align  := alClient;

      // Initially value. It should be set via the 'Caption' property.
      Caption := 'Time of Action';
   end;

   pTimeType := TComboBox.Create(pGB);
   with pTimeType do begin
      Style      := csDropDownList;
      Parent     := pGB;
      OnChange   := TimeTypeChange;
   end;

   pETimeType := TEdit.Create(pGB);
   with pETimeType do begin
      Parent     := pGB;
      Enabled    := FALSE;
   end;

   pActions := TComboBox.Create(pGB);
   with pActions do begin
      Parent     := pGB;
      OnChange   := ActionsChange;
      Style      := csDropDownList;
   end;

   pLblActions          := TLabel.Create(pGB);
   pLblActions.Parent   := pGB;

   // Initial value. It should be set via the 'ActionsLabel.Caption' property
   pLblActions.Caption  := 'Actions';

   pLblTimeType         := TLabel.Create(pGB);
   pLblTimeType.Parent  := pGB;

   // Initial value. It should be set via the 'TimeTypeLabel.Caption' property
   pLblTimeType.Caption := 'Time type';

   pLblDate             := TLabel.Create(pGB);
   pLblDate.Parent      := pGB;

   pDateM                := TMaskEdit.Create(pGB);
   pDateM.Parent         := pGB;
   pDateM.EditMask       := '9999/99/99;1;_';
   pDateM.MaxLength      := 10;
   pDateM.Width          := 65;
   pDateM.OnChange       := DateChange;
   pDateM.OnExit         := DateExit;
   OriginalEmptyDate := pDateM.EditText;
(*
   pDate                := TEdit.Create(pGB);
   pDate.Parent         := pGB;
   pDate.OnChange       := DateChange;
   pDate.OnExit         := DateExit;
*)
   pLblDays             := TLabel.Create(pGB);
   pLbldays.Parent      := pGB;

   // Initial value. It should be set via the 'DisplacementLabel.Caption' property
   pLblDays.Caption     := 'Displacement in days';

   pDays                := TSpinEdit.Create(pGB);
   with pDays do begin
      Parent            := pGB;
      MinValue          := 0;
      MaxValue          := 365;
      Width             := 45;
      OnChange          := DaysChange;
   end;
   OnlyDate := False;
   Size;
   {Show;}
end;

procedure TDaisyTimeComp.ClearTime;
begin
   if pOnlyDate then
      pType := 3
   else
      pType := 0;
   pTimeType.ItemIndex := pType;
   pDays.Value := 0;
   pDateM.EditText := OriginalEmptyDate;
//   pDate.Text := '';
   Show;
end;

procedure TDaisyTimeComp.TimeTypeChange(Sender: TObject);
begin
   pType := pTimeType.ItemIndex;
   Dirty := True;
   pDays.Value := 0;
   pDateM.EditText := OriginalEmptyDate;
//   pDate.Text := '';
   Show;
end;

procedure TDaisyTimeComp.ActionsChange(Sender: TObject);
begin
   Dirty := True;
end;

procedure TDaisyTimeComp.DateExit(Sender:TObject);
begin
   { Validate the date from the "outside" when needed, using 'CheckDate' }
end;

procedure TDaisyTimeComp.DaysChange(Sender:TObject);
begin
   Dirty := True;
end;

procedure TDaisyTimeComp.DateChange(Sender:TObject);
begin
   Dirty := True;
end;

procedure TDaisyTimeComp.PanelResize(Sender: TObject);
begin
   if not init then begin
      init := True;
      pTimeType.Items := Types;
      pTimeType.ItemIndex := pType;
   end;
   Size;
end;

procedure TDaisyTimeComp.Show;
begin
   pLblActions.Hide;
   pActions.Hide;
   pLblDate.Hide;
   pDateM.Hide;
//   pDate.Hide;
   pLblDays.Hide;
   pDays.Hide;
   pTimeType.Hide;
   pETimeType.Hide;
   if pOnlyDate then
      pType := 3;
   case pType of
      0: {Date}
         begin
            pTimeType.Show;

            // Previously the label was defined statically, now we reuse the
            // string from the 'Types' instance of TStringList
            pLblDate.Caption := Types.Strings[0];     // Previously: 'Date';

            pLblDate.Show;
            pDateM.Show;
//            pDate.Show;
         end;
      1: {CaDate}
         begin
            pTimeType.Show;

            // Previously the label was defined statically, now we reuse the
            // string from the 'Types' instance of TStringList
            pLblDate.Caption := Types.Strings[1];     // Previously: 'Approx. Date';

            pLblDate.Show;
            pDateM.Show;
//            pDate.Show;
         end;
      2:
         begin
            pTimeType.Show;
            pLblActions.Show;
            pActions.Show;
            pLblDays.Show;
            pDays.Show;
            GetActions;
         end;
      3:
         begin
            // MARK SAYS: I think this branch is used only when it must be
            // possible to edit a date, and nothing else.
            pETimeType.Show;

            // Previously the label was defined statically, now we se the
            // value set by the 'DateLabel' property.
            pLblDate.Caption := DateLabelText;       // Previously: 'Date';

            pLblDate.Show;
            pDateM.Show;
//            pDate.Show;
            pETimeType.Text := DateLabelText;        // Previously: 'Date';
         end;
   end;
end;

procedure TDaisyTimeComp.Size;
var Center : Integer;
begin
   if Width < szWidth then
      Width := szWidth;
   if Height < szHeight then
      Height := szHeight;
   Center := Width div 2;
   with pLblTimeType do begin
      left := 24;
      top := 24;
   end;
   with pTimeType do begin
      left := 24;
      top  := 40;
   end;
   with pETimeType do begin
      left := 24;
      top  := 40;
   end;
   with pLblActions do begin
      left := 24;
      top := 80;
   end;
   with pActions do begin
      left := 24;
      top  := 96;
   end;
   with pLblDate do begin
      left := 24;
      top := 80;
   end;
   with pDateM do begin
      left := 24;
      top  := 96;
   end;
(*
   with pDate do begin
      left := 24;
      top  := 96;
   end;
*)
   with pLblDays do begin
      left := 24+Center;
      top := 24;
   end;
   with pDays do begin
      left := 24+Center;
      top  := 40;
   end;
end;

procedure TDaisyTimeComp.SetDate(Date:String);
begin
   Setting := True;
   if pOnlyDate then
      pType := 3
   else
       pType := 0;
   pTimeType.ItemIndex := pType;
   pDateM.EditText := Copy(Date, 1, 4) + pDateM.EditText[5] +
                      Copy(Date, 6, 2) + pDateM.EditText[8] +
                      Copy(Date, 9, 2);
//   pDate.Text := Date;
   Show;
end;

procedure TDaisyTimeComp.SetCaDate(Date:String);
begin
   if not pOnlyDate then begin
      Setting := True;
      pType := 1;
      pTimeType.ItemIndex := pType;
      pDateM.EditText := Copy(Date, 1, 4) + pDateM.EditText[5] +
                         Copy(Date, 6, 2) + pDateM.EditText[8] +
                         Copy(Date, 9, 2);
//      pDate.Text := Date;
      Show;
   end;
end;

procedure TDaisyTimeComp.SetRelTo(Days:Integer);
begin
   if not pOnlyDate then begin
      Setting := True;
      pType := 2;
      pTimeType.ItemIndex := pType;
      pDays.Value := Days;
      Show;
   end;
end;

{ CheckDate should be called before useing this procedure... }
procedure TDaisyTimeComp.GetTime(var T:Integer; var Str:String; var D:Integer);
begin
   T := pType;
   case T of
      0,3: begin
            Str := Copy(pDateM.Text, 1, 4);             { copy year }
            Str := Str + '-' + Copy(pDateM.Text, 6, 2); { copy month }
            Str := Str + '-' + Copy(pDateM.Text, 9, 2); { copy date }
//            Str := pDate.Text;
            D := -1;
         end;
      1: begin
            Str := Copy(pDateM.Text, 1, 4);             { copy year }
            Str := Str + '-' + Copy(pDateM.Text, 6, 2); { copy month }
            Str := Str + '-' + Copy(pDateM.Text, 9, 2); { copy date }
//            Str := pDate.Text;
            D := -1;
         end;
      2: begin
            Str := pActions.Items[pActions.ItemIndex];
            D := pDays.Value;
         end;
   end
end;

procedure TDaisyTimeComp.GetActions;
var index:Integer;
begin
   index := -1;
   if Assigned(pGetActions) then begin
      pGetActions(pActions.Items,index);
      if (index > -1) and (index < pActions.Items.Count) then
         pActions.ItemIndex := index
      else
         pActions.ItemIndex := -1;
   end;
end;

function TDaisyTimeComp.CheckDate: Integer;  { 0: Date OK, 1: Parts missing, 2: Invalid Date}
var
   Day, Month, Year, OldFormatString: String;
begin
   Result := 0;
   { Only something to check if fixed date or approx. date }
   if (pType = 0) or (pType = 1) then begin
      { Check if valid values }
      with pDateM do begin
         Year := Trim(Copy(Text, 1, 4));
         Month := Trim(Copy(Text, 6, 2));
         Day   := Trim(Copy(Text, 9, 2));
//         x := Left + FormDate.Left - 50;
//         y := Top + FormDate.Top + 50;

         if (Month <> '') or (Day <> '') or (Year <> '') then begin  // All blank => ok
            if (Month = '') or (Day = '') or (Year = '') then begin  // One blank => not valid
            MessageDlg(ErrorMessageHeadText + ':' + CHR(10) + CHR(13)
                        + ErrorMessageBlankText,
                           mtError, [mbOK], 0);
               Result := 1;
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
                  MessageDlg(ErrorMessageHeadText + ':' + CHR(10) + CHR(13)
                              + ErrorMessageInvalidText,
                                 mtError, [mbOK], 0);
                  Result := 2;
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
         Result := 0;
      end;  { with MEditDate do begin }
   end;
end;

procedure register;
begin
   registercomponents('Daisy',[TDaisyTimeComp]);
end;


end.
