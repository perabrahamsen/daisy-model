unit Discret;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, Grids, Globals, TypInfo;

type
  TFormDiscretization = class(TForm)
    BtnOK: TBitBtn;
    BtnCancel: TBitBtn;
    BtnHelp: TBitBtn;
    SGDiscretization: TStringGrid;
    GBEditDisc: TGroupBox;
    BtnDiscretizationClear: TButton;
    BtnDiscretizationSort: TButton;
    SBDiscretizationUp: TSpeedButton;
    SBDiscretizationDown: TSpeedButton;
    BtnDiscretizationDelete: TButton;
    BtnDiscretizationInsert: TButton;
    GBApplyDisc: TGroupBox;
    BtnFine: TButton;
    BtnMedium: TButton;
    BtnCoarse: TButton;
    procedure BtnDiscretizationClearClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BtnDiscretizationInsertClick(Sender: TObject);
    procedure BtnDiscretizationDeleteClick(Sender: TObject);
    procedure SBDiscretizationUpClick(Sender: TObject);
    procedure SBDiscretizationDownClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BtnApplyClick(Sender: TObject);
    procedure BtnDiscretizationSortClick(Sender: TObject);
    procedure SGDiscretizationSelectCell(Sender: TObject; Col,
      Row: Longint; var CanSelect: Boolean);
    procedure BtnOKClick(Sender: TObject);

    function CheckSGDiscretization(msg: Boolean): Boolean;

    procedure SetFormFromVars;
    procedure SetVarsFromForm;
    procedure BtnsDiscretizationUpdate(selrow: LongInt);
    procedure SGDiscretizationExit(Sender: TObject);
    procedure BtnHelpClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


var
  FormDiscretization: TFormDiscretization;


implementation

{$R *.DFM}


procedure TFormDiscretization.SetFormFromVars;
var
   i, r: Integer;
begin
   r := Temp2DiscretizationVars.Discretization.Count;
   with SGDiscretization do begin
      Options := [goFixedVertLine,goFixedHorzLine,goVertLine,goHorzLine,goEditing];

      if r > 0 then begin
         RowCount := r + 1;
         for i:=1 to RowCount-1 do
            Cells[0, i] := Temp2DiscretizationVars.Discretization[i-1];
      end
      else begin { Should never happend, because there must be a horizon to edit discretization }
         RowCount := 2; { Needed, because RowCount=1 trashes the first fixed row!!! }
         Rows[1].Clear;
      end;
      Row := 1;
      Options := [goFixedVertLine,goFixedHorzLine,goVertLine,goHorzLine];
      Update;
   end;
   BtnsDiscretizationUpdate(1);
end;

{ Sets the Temp2DiscretizationVars.Discretization variable from content of discretization grid     }
{ (Temp2DiscretizationVars.Untouchables should be up to date here, so no need to worry about that) }
procedure TFormDiscretization.SetVarsFromForm;
var
   i: Integer;
begin
   if CheckSGDiscretization(TRUE) then begin { Check if syntax is right for depths }
      Temp2DiscretizationVars.Discretization.Clear;
      with FormDiscretization.SGDiscretization do
         for i:=1 to RowCount-1 do
            Temp2DiscretizationVars.Discretization.Add(Cells[0,i]);
   end;
end;

{ Update the buttons status from which row is selected in the stringgrid }
{ The selected row as argument (or Delphi goes tilt!!!)                  }
procedure TFormDiscretization.BtnsDiscretizationUpdate(selrow: LongInt);
begin
   if Temp2DiscretizationVars.UnTouchables.Count < (selrow - 1) then
      MessageDlg(LoadStr(RES_ERR_Discret_Mismatch_Untouch_Gridrows1)
                  + inttostr(Temp2DiscretizationVars.UnTouchables.Count)
                  + LoadStr(RES_ERR_Discret_Mismatch_Untouch_Gridrows2)
                  + inttostr(selrow), mtError, [mbOK], 0)
   else if selrow > 0 then begin
      if Temp2DiscretizationVars.UnTouchables[selrow-1] = 'U' then
         BtnDiscretizationDelete.Enabled := FALSE
      else
         BtnDiscretizationDelete.Enabled := TRUE;
      if selrow = 1 then
         SBDiscretizationUp.Enabled := FALSE
      else
         SBDiscretizationUp.Enabled := TRUE;
      if selrow = SGDiscretization.RowCount - 1 then
         SBDiscretizationDown.Enabled := FALSE
      else
         SBDiscretizationDown.Enabled := TRUE;
   end;
end;

{ Check if the defined depths have valid format                         }
{ msg = TRUE will caused an error message to be viewed in case of errors }
function TFormDiscretization.CheckSGDiscretization(msg: Boolean): Boolean;
var
   x, y, i: Integer;
   deepest: Double;
   errlines, errdepths, errmsg: String;
   GoodNum: Boolean;
begin
   Result := TRUE;
   errlines := '';
   errdepths := '';

   { Get the deepest horizon for later check }
   deepest := 0;
   for i:=0 to TempProfileVars.NumSGProfileItems - 1 do
      deepest := deepest + StrToFloat(TempProfileVars.SGProfileItems.Cells[1, i]);

   { Now check the content of the string grid... }
   with FormDiscretization.SGDiscretization do begin
      for i:=1 to RowCount - 1 do begin
         GoodNum := TRUE;
         try
            Cells[0,i] :=
               StrTo1DecPosNotZeroStr(Cells[0,i]);
         except
            on E: Exception do begin
               GoodNum := FALSE;
               if errlines = '' then
                  errlines := errlines + IntToStr(i)
               else
                  errlines := errlines + ', ' + IntToStr(i);
   {              break;} { don't break, but check/convert all rows. }
            end;
         end;
         if GoodNum then  { Check if depth is below deepest horizon }
            if StrToFloat(Cells[0, i]) > deepest then
               if errdepths = '' then
                  errdepths := errdepths + IntToStr(i)
               else
                  errdepths := errdepths + ', ' + IntToStr(i);
      end;

      if (errlines <> '') or (errdepths <> '') then begin { There are errors }
         Result := FALSE;
         if msg then begin
            x := Left + FormDiscretization.Left - 240;
            y := Top + FormDiscretization.Top + 10;
            errmsg := LoadStr(RES_ERR_Discret_Contains_Invalid_Depth_Defs1)
                        + Chr(10) + Chr(13) + Chr(13);

            if errlines <> '' then
               errmsg := errmsg
                           + LoadStr(RES_ERR_Discret_Contains_Invalid_Depth_Defs2_1)
                           + errlines
                           + LoadStr(RES_ERR_Discret_Contains_Invalid_Depth_Defs2_2)
                           + Chr(10) + Chr(13)
                           + LoadStr(RES_ERR_Discret_Contains_Invalid_Depth_Defs2_3)
                           + Chr(10) + Chr(13) + Chr(13);

            if errdepths <> '' then
               errmsg := errmsg
                           + LoadStr(RES_ERR_Discret_Contains_Invalid_Depth_Defs3_1)
                           + ' ' + errdepths + ' '
                           + LoadStr(RES_ERR_Discret_Contains_Invalid_Depth_Defs3_2)
                           + Chr(10) + Chr(13)
                           + LoadStr(RES_ERR_Discret_Contains_Invalid_Depth_Defs3_3)
                           + ' ' + FormatFloat('0.0', deepest) + ' '
                           + LoadStr(RES_ERR_Discret_Contains_Invalid_Depth_Defs3_4);

            MessageDlgPos(errmsg, mtError, [mbOK], 0, x, y);
            SetFocus;
         end;
      end;
   end;
end;

procedure TFormDiscretization.BtnDiscretizationClearClick(Sender: TObject);
var
   x, y: Integer;
begin
   x := FormDiscretization.Left + BtnDiscretizationClear.Left - 20;
   y := FormDiscretization.Top + BtnDiscretizationClear.Top - 100;
   if MessageDlgPos(LoadStr(RES_MSG_Discret_Action_Clear_Depths)
                     + CHR(10) + CHR(13) + CHR(13)
                     + LoadStr(RES_MSG_Ok_or_Cancel),
                     mtConfirmation, [mbOK, mbCancel], 0, x, y)
      = mrOK then begin
      Temp2DiscretizationVars.Reset(TempProfileVars);
      SetFormFromVars;
   end;
end;

procedure TFormDiscretization.FormCreate(Sender: TObject);
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

   Caption := LoadStr(RES_FRM_Discret);
   { Group captions }
   GBApplyDisc.Caption := LoadStr(RES_GRP_Discret_Apply);
   GBEditDisc.Caption  := LoadStr(RES_GRP_Discret_Edit);
      { Init Buttons... }
   BtnDiscretizationClear.Caption  := LoadStr(RES_BTN_Discret_Clear);
   BtnDiscretizationSort.Caption   := LoadStr(RES_BTN_Discret_Sort);
   BtnDiscretizationDelete.Caption := LoadStr(RES_BTN_Discret_Delete);
   BtnDiscretizationInsert.Caption := LoadStr(RES_BTN_Discret_Insert);
   BtnCoarse.Caption := LoadStr(RES_BTN_Discret_Coarse);
   BtnMedium.Caption := LoadStr(RES_BTN_Discret_Medium);
   BtnFine.Caption   := LoadStr(RES_BTN_Discret_Fine);
   BtnOK.Caption     := LoadStr(RES_BTN_Ok);
   BtnCancel.Caption := LoadStr(RES_BTN_Cancel);
   BtnHelp.Caption   := LoadStr(RES_BTN_Help);;
      { Init StringGrid... }
   SGDiscretization.Cells[0,0] := LoadStr(RES_LBL_Discret_Depth)
                                    + ' (' + LoadStr(Res_UNIT_Cm) + ')';

      { Init HelpContexts... }
   GBApplyDisc.HelpContext := HLP_Discr_Apply_Discretization;
   BtnFine.HelpContext := HLP_Discr_Apply_Discretization;
   BtnMedium.HelpContext := HLP_Discr_Apply_Discretization;
   BtnCoarse.HelpContext := HLP_Discr_Apply_Discretization;
   GBEditDisc.HelpContext := HLP_Discr_Edit_Discretization;
   BtnDiscretizationInsert.HelpContext := HLP_Discr_Edit_Discretization;
   BtnDiscretizationDelete.HelpContext := HLP_Discr_Edit_Discretization;
//   SBDiscretizationUp.HelpContext := HLP_Discr_Edit_Discretization;
//   SBDiscretizationDown.HelpContext := HLP_Discr_Edit_Discretization;
   BtnDiscretizationSort.HelpContext := HLP_Discr_Edit_Discretization;
   BtnDiscretizationClear.HelpContext := HLP_Discr_Edit_Discretization;
   SGDiscretization.HelpContext := HLP_Discr_Discretization_Depths;
   BtnOK.HelpContext := HLP_Discretization;
   BtnCancel.HelpContext := HLP_Discretization;
   BtnHelp.HelpContext := HLP_Discretization;
end;

{ Inserts an empty row AFTER the selected row in the grid }
procedure TFormDiscretization.BtnDiscretizationInsertClick(Sender: TObject);
var
   i, selrow : Integer;
begin
   with SGDiscretization do begin
      if Row > 0 then begin { A row is selected }
         selrow := Row;
         RowCount := RowCount + 1;
         Temp2DiscretizationVars.Untouchables.Add(''); { Make space for one more element }
         Temp2DiscretizationVars.Discretization.Add('');
         for i:=RowCount-1 downto selrow+2 do begin
            Rows[i] := Rows[i-1];                   { one based (first row heading) }
            Temp2DiscretizationVars.Untouchables[i-1]
                  := Temp2DiscretizationVars.Untouchables[i-2]; { zero based }
            Temp2DiscretizationVars.Discretization[i-1]
                  := Temp2DiscretizationVars.Discretization[i-2];
         end;
         Rows[selrow+1].Clear;
         Temp2DiscretizationVars.UnTouchables[selrow] := '';
         Temp2DiscretizationVars.Discretization[selrow] := '';
         Row := selrow + 1;  { Make the new empty row selected }
         Refresh;
         BtnsDiscretizationUpdate(Row);
      end;
   end;
end;

{ Deletes the selected row (Delete is invalid if selected is untouchable) }
procedure TFormDiscretization.BtnDiscretizationDeleteClick(Sender: TObject);
var
   i, selrow : Integer;
begin
   with SGDiscretization do begin
      { Use (untouchable = '') just to be hypersecure! }
      if (Row > 0) and (Temp2DiscretizationVars.UnTouchables[Row-1]='') then begin { A row is selected }
         selrow := Row;
         for i:=selrow to RowCount-2 do
            Rows[i] := Rows[i+1];  { one based (first row heading) }
         Rows[RowCount-1].Clear;
         RowCount := RowCount - 1;
         Temp2DiscretizationVars.UnTouchables.Delete(selrow-1);
         Temp2DiscretizationVars.Discretization.Delete(selrow-1);
         { Make the row following after the deleted, selected... }
         if selrow > (RowCount-1) then { Last row deleted }
            Row := RowCount-1
         else
            Row := selrow;
         Refresh;
         BtnsDiscretizationUpdate(Row);
      end;
   end;
end;

procedure TFormDiscretization.SBDiscretizationUpClick(Sender: TObject);
var
   temps: String;
begin
   with SGDiscretization do begin
      if Row > 1 then begin { Button should be disabled otherwise, but just to be sure }
         { Move rows in grid... }
         temps := Cells[0, Row-1];
         Cells[0, Row-1] := Cells[0, Row];
         Cells[0, Row] := temps;
         { Move content of TempCurrDiscretization variable... }
         temps := Temp2DiscretizationVars.Discretization[Row-2];
         Temp2DiscretizationVars.Discretization[Row-2]
               := Temp2DiscretizationVars.Discretization[Row-1];
         Temp2DiscretizationVars.Discretization[Row-1] := temps;
         { Move content of TempUntouchables variable... }
         temps := Temp2DiscretizationVars.Untouchables[Row-2];
         Temp2DiscretizationVars.Untouchables[Row-2]
               := Temp2DiscretizationVars.Untouchables[Row-1];
         Temp2DiscretizationVars.Untouchables[Row-1] := temps;
         { ...and finally focus the row at its new position... }
         Row := Row - 1;
         BtnsDiscretizationUpdate(Row);
      end;
   end;
end;

procedure TFormDiscretization.SBDiscretizationDownClick(Sender: TObject);
var
   temps: String;
begin
   with SgDiscretization do begin
      if Row < (RowCount - 1) then begin { Button should be disabled otherwise, but just to be sure }
         { Move rows in grid... }
         temps := Cells[0, Row+1];
         Cells[0, Row+1] := Cells[0, Row];
         Cells[0, Row] := temps;
         { Move content of TempCurrDiscretization variable... }
         temps := Temp2DiscretizationVars.Discretization[Row-1];
         Temp2DiscretizationVars.Discretization[Row-1]
               := Temp2DiscretizationVars.Discretization[Row];
         Temp2DiscretizationVars.Discretization[Row] := temps;
         { Move content of TempUntouchables variable... }
         temps := Temp2DiscretizationVars.Untouchables[Row-1];
         Temp2DiscretizationVars.Untouchables[Row-1]
               := Temp2DiscretizationVars.Untouchables[Row];
         Temp2DiscretizationVars.Untouchables[Row] := temps;
         { ...and finally focus the row at its new position... }
         Row := Row + 1;
         BtnsDiscretizationUpdate(Row);
      end;
   end;
end;

procedure TFormDiscretization.FormShow(Sender: TObject);
begin
   { First set colwidth to fill stringgrids in total width }
   SGDiscretization.ColWidths[0] := SGDiscretization.ClientWidth - 1;

   FormDiscretization.SetFormFromVars;
   FormDiscretization.Update;
end;

procedure TFormDiscretization.BtnApplyClick(Sender: TObject);
var
   disctype: Integer;
begin
   with Sender as TButton do
      if Name = 'BtnFine' then disctype := 0
      else if Name = 'BtnMedium' then disctype := 1
      else disctype := 2;  { coarse }
   if TempProfileVars.SGProfileItems.Cells[1,0] <> '' then begin
      Temp2DiscretizationVars.Apply(disctype);
      SetFormFromVars;
   end;
end;

procedure TFormDiscretization.BtnDiscretizationSortClick(Sender: TObject);
begin
   if CheckSGDiscretization(TRUE) then begin
      SetVarsFromForm;
      Temp2DiscretizationVars.Sort;
      SetFormFromVars;
   end;
end;

procedure TFormDiscretization.SGDiscretizationSelectCell(Sender: TObject;
  Col, Row: Longint; var CanSelect: Boolean);
begin
   { Update the buttons after selected row }
   BtnsDiscretizationUpdate(Row);
   { Make the cell (grid) editable or not (last if untouchable) }
   if Temp2DiscretizationVars.UnTouchables[Row-1] = 'U' then
      SGDiscretization.Options := [goFixedVertLine,goFixedHorzLine,goVertLine,goHorzLine]
   else
      SGDiscretization.Options := [goFixedVertLine,goFixedHorzLine,goVertLine,goHorzLine,goEditing];
end;

procedure TFormDiscretization.BtnOKClick(Sender: TObject);
begin
   { If valid stringgrid, update variable and exit }
   if CheckSGDiscretization(TRUE) then begin
      { Update vars from form... (all checks should be done here)}
      { Update might also have been done earlier (on exit from edit-boxes etc.), but ... }
      BtnDiscretizationSortClick(Sender);
      SetVarsFromForm;

      { ...and then it's ok }
      ModalResult := mrOK;
   end;
end;

procedure TFormDiscretization.SGDiscretizationExit(Sender: TObject);
begin
   SetVarsFromForm;
end;

procedure TFormDiscretization.BtnHelpClick(Sender: TObject);
begin
   Application.HelpContext(HLP_Discretization);
end;

end.


