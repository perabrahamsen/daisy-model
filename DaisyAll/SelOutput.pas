unit SelOutput;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, Grids, TypInfo;

type
  TFormOutput = class(TForm)
    SGOutput: TStringGrid;
    BtnOK: TBitBtn;
    BtnCancel: TBitBtn;
    BtnHelp: TBitBtn;
    procedure SGOutputDrawCell(Sender: TObject; Col, Row: Longint;
      Rect: TRect; State: TGridDrawState);
    procedure SGOutputMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    SelectFrom, Selected : TStringList;
    Dirty : Boolean;
    SelectedIcon   : TIcon;
    SelectObject : TList;
  public
    { Public declarations }
    procedure PrepareForm(SelFrom, Sel : TStringList; OutputDir : String);
    function GetSelection(Sel:TStringList; var OutputDir : String):boolean;
  end;

var
  FormOutput: TFormOutput;

implementation

{$R *.DFM}
uses pasdaisy, uselib, globals;


function GetWhere(OutputName:String):String;
var
   ObjectAlist : daisy_alist;
   temp        : string;
begin
   result := '';
   ObjectAlist := GetLibraryObject('log',OutputName);
   if ObjectAlist <> nil then begin
      if GetVariable(ObjectAlist, 'where', daisy_type_string, @temp) then
         result := temp;
   end;
end;


procedure TFormOutput.PrepareForm(SelFrom, Sel : TStringList; OutputDir : String);
begin
   SelectFrom.Clear;
   SelectFrom.AddStrings(SelFrom);
   Selected.Clear;
   Selected.AddStrings(Sel);
end;

function TFormOutput.GetSelection(Sel : TStringList; var OutputDir : String):boolean;
var i : integer;
begin
   result := false;
   if Sel <> nil then begin
      if Dirty then begin
         Selected.Clear;
         for i := 0 to SelectFrom.Count - 1 do begin
            if SelectObject[i] <> nil then
               Selected.Add(SelectFrom[i]);
         end;
         Sel.Clear;
         Sel.AddStrings(Selected);
         result := true;
      end;
   end;
end;

procedure TFormOutput.SGOutputDrawCell(Sender: TObject; Col, Row: Longint;
  Rect: TRect; State: TGridDrawState);
begin
   if (Col = 0) and (row > 0)and (row <= SelectFrom.Count) then begin
      if SelectObject[Row-1] <> nil then begin
         SGOutput.Canvas.Draw(Rect.Left,Rect.Top,SelectedIcon);
      end;
   end;
end;

procedure TFormOutput.SGOutputMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
   ACol, ARow : LongInt;
begin
   if Button = mbLeft then begin
      SGOutput.MouseToCell(X,Y,ACol, ARow);
      if (ACol = 0) and (ARow > 0) and (Arow <= SelectFrom.Count) then begin
         if SelectObject[ARow-1] <> nil then
            SelectObject[ARow-1] := nil
         else
            SelectObject[ARow-1] := SelectedIcon;
         // Make shure OnDrawCell is generated
         SGOutput.Cells[ACol,ARow] := '';
         SGOutput.Update;
         Dirty := true;
      end;
   end;
end;

procedure TFormOutput.FormCreate(Sender: TObject);
var
   i: integer;
   buf : array[0..30] of char;
begin
(*
   { Now scale the form and its components... }
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
*)

   { *** Remember to initialize strings (labels etc.)!!! }
   {...}



   SelectFrom := TStringList.Create;
   Selected   := TStringList.Create;
   SelectedIcon := TIcon.Create;
   SelectObject := TList.Create;
   SelectedIcon.Handle :=  LoadIcon(HINSTANCE,strpcopy(buf,'OKIco'));
   Dirty := false;
end;

procedure TFormOutput.FormDestroy(Sender: TObject);
begin
   SelectFrom.Free;
   Selected.Free;
   SelectedIcon.Free;
   SelectObject.Free;
end;

procedure TFormOutput.FormShow(Sender: TObject);
var i : integer;
    where : String;
    function Max(i,j:integer):integer;
    begin
       result := i;
       if i < j then
          result := j;
    end;
begin
   Dirty := false;
   SelectObject.Clear;
   with SGOutput do begin
      ColCount := 3;
      RowCount := SelectFrom.Count + 1;
      Cells[0,0] := 'Selected';
      Cells[1,0] := 'Name';
      Cells[2,0] := 'Output filename';
      ColWidths[0] := Canvas.TextWidth(Cells[0,0]+ '  ');
      ColWidths[1] := Canvas.TextWidth(Cells[1,0]+ '  ');
      ColWidths[2] := Canvas.TextWidth(Cells[2,0]+ '  ');
      for i := 0 to SelectFrom.Count-1 do begin
         Rows[i+1].Clear;
         Cells[1,i+1] := SelectFrom.Strings[i];
         Cells[2,i+1] := GetWhere(SelectFrom.Strings[i]);
         ColWidths[1] := Max(ColWidths[1],Canvas.TextWidth(Cells[1,i+1]+ '  '));
         ColWidths[2] := Max(ColWidths[2],Canvas.TextWidth(Cells[2,i+1]+ '  '));
         if Selected.IndexOf(SelectFrom.Strings[i]) <> -1 then
            SelectObject.Add(SelectedIcon)
         else
            SelectObject.Add(nil);
      end;
      SetFocus;
   end;
end;



end.
