unit Tabel;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, StdCtrls, Buttons, Globals, TypInfo;

type
  TFormTable = class(TForm)
    BtnOK: TBitBtn;
    BtnCancel: TBitBtn;
    BtnHelp: TBitBtn;
    SGTable: TStringGrid;
    procedure FormCreate(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);
    procedure BtnHelpClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

{ NumItems: Number of items (rows) in 'content' }
procedure InitTable(x, y: Integer; title, c0, c1: String; NumItems: Integer;
                     var content: TStringGrid; RestType, Hlp1: Integer);
function ReadTable(var content: TStringGrid): Integer;
function CheckTable(msg: Boolean): Boolean;

var
  FormTable: TFormTable;

implementation

var
   Hlp: Integer; { The help context to be showed on button click  }
                 { and context sensitive on table                 }
   RestrictionType: Integer;
      { Determines what restrictions are set by the last init }
      { Restrictions implemented are:                         }
      { 1: Only one positive floats allowed in grid - will be }
      {    converted to one decimal floats on table leave.    }


{$R *.DFM}


{ Check the table content according to restriction   }
{ if msg then 'print error message if there are any' }
function CheckTable(msg: Boolean): Boolean;
var
   i: Integer;
   errlines: String;
begin
   Result := TRUE;
   errlines := '';
   with FormTable.SGTable do begin
      for i:=1 to RowCount-1 do begin
         Cells[0, i] := Trim(Cells[0, i]);
         Cells[1, i] := Trim(Cells[1, i]);
         case RestrictionType of  { test on the restriction type for input }
            1: begin
                  { Allow blank rows, but the whole row must be blank... }
                  if (Cells[0, i] <> '') or (Cells[1, i] <> '') then begin
                     try begin
                        StrTo1DecPosStr(Cells[0,i]);
                        StrTo1DecPosStr(Cells[1,i]);
                     end;
                     except
                        on E: Exception do begin
                           Result := FALSE;
                           if errlines = '' then
                              errlines := IntToStr(i)
                           else
                              errlines := errlines + ', ' + IntToStr(i);
                           end;
                     end;  { try }
                  end;
               end; { case 1 }
               else begin
                   { No restrictions }
               end;
         end; { case }
      end; { for i:=1... }
      if (not Result) and msg then
         MessageDlg('The table containes invalid data in lines ' + errlines
                     + Chr(10) + Chr(13) + Chr(13)
                     + 'Please correct or delete the content of the lines'
                     + ' before leaving the table!',
                     mtError, [mbOK], 0);
   end; { with FormTable.SG... }
end;


{ Reads the StringGrid and puts the content into var 'content' }
{ 'content' must have at least two columns!                    }
{ Returns number of rows read                                  }
function ReadTable(var content: TStringGrid): Integer;
var
   i, j: Integer;
begin
   with FormTable.SGTable do
      if CheckTable(TRUE) then begin { tablecontent ok - now copy to content }
         j := 0;
         Result := 0;
         content.FixedRows := 0;
         content.RowCount := 1;  // Must be 1 higher than FixedRows - @£$##%
         for i:=1 to RowCount-1 do begin
            if Cells[0, i] <> '' then begin { not a blank line }
               if i>1 then
                  content.RowCount := content.RowCount + 1;
               Result := Result + 1;
               content.Cells[0, j] := Cells[0, i];
               content.Cells[1, j] := Cells[1, i];
               j := j + 1;
            end;
         end;
      end;
end;


{ Initializes the table. Beware of var content! Declared var to prevent bugs in }
{ TStringGrid.Create for parameter. DON'T change content inside this function!! }
procedure InitTable(x, y: Integer; title, c0, c1: String; NumItems: Integer;
                     var content: TStringGrid; RestType, Hlp1: Integer);
var
   i, j: Integer;
   GoodVal: Boolean;
begin
   Formtable.Left := x - FormTable.Width;
   Formtable.Top := y - (FormTable.Height div 4);
   Formtable.Caption := title;
   RestrictionType := RestType;

   with FormTable.SGTable do begin
      RowCount := 50; { max or should it be dynamically increased?? }
      if content.RowCount > (RowCount + 1) then  { If arg bigger, then prevent breakdown }
         RowCount := content.RowCount + 11; { make 10 extra rows #-) }

      Cells[0,0] := c0; { Init column headings }
      Cells[1,0] := c1;

      for i := 1 to RowCount - 1 do { Clear table }
         Rows[i].Clear;

      if NumItems > 0 then begin { put content into grid and make the rest empty }
         for i := 0 to NumItems - 1 do begin
            GoodVal := TRUE;
            case RestrictionType of  { test on the restriction type for input }
               1: begin
                     try begin
                        StrTo1DecPosStr(content.Cells[0,i]);
                        StrTo1DecPosStr(content.Cells[1,i]);
                     end;
                     except
                        on E: Exception do
                           GoodVal := FALSE;
                     end;  { try }
                     if GoodVal then begin
                        Cells[0,i+1] := StrTo1DecPosStr(content.Cells[0,i]);
                        Cells[1,i+1] := StrTo1DecPosStr(content.Cells[1,i]);
                        j := j + 1;
                     end;
                  end; { case 1 }
               else begin  { Accept anything... }
                  Cells[0,i+1] := content.Cells[0,i];
                  Cells[1,i+1] := content.Cells[1,i];
               end;
            end; { case }
         end;  { for i := 1 to NumItems-1 }
      end;  { NumItems > 0 }

      { Set Help }
      HelpContext := Hlp1;
      Hlp := Hlp1; { global help topic for the form }
   end;  { with FormTable.SGTable do }
end;


procedure TFormTable.FormCreate(Sender: TObject);
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
   BtnOK.Caption := 'OK';
   BtnCancel.Caption := 'Cancel';
   BtnHelp.Caption := 'Help';
   RestrictionType := 0;
end;

procedure TFormTable.BtnOKClick(Sender: TObject);
begin
   if not CheckTable(TRUE) then
      SGTable.SetFocus
   else
      ModalResult := mrOK;
end;

procedure TFormTable.BtnHelpClick(Sender: TObject);
begin
   Application.HelpContext(Hlp);
end;

procedure TFormTable.FormShow(Sender: TObject);
begin
   { First set colwidth to fill stringgrids in total width }
   SGTable.ColWidths[0] := Trunc(SGTable.ClientWidth / 2);
   SGTable.ColWidths[1] := SGTable.ClientWidth - SGTable.ColWidths[0] - 1;
   SGTable.SetFocus; { Make the form ready for entering data }
end;

end.
