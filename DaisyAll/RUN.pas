unit RUN;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,Pasdaisy, Globals, TypInfo;

type
  TFormRunSim = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }

    SimulationAlist   : daisy_alist;
    M,C,W,Mc,Cc,Wc    : daisy_alist;
    Ms, Cs, Ws, OutPut: daisy_syntax;
    Start,temp        : daisy_time;
    OutputType        : String;
    ManComplete, ColumnComplete, WeatherComplete, OutputComplete : Boolean;
    syntax:daisy_syntax;
    alist,AddManager:daisy_alist;
    parser:daisy_parser;
    daisy:daisy_daisy;
    time : daisy_time;
    columns : Integer;



    function BuildDaisyObject : boolean;
    procedure RunIt;

  public
    { Public declarations }
    procedure AddMemoLine(s:String);
  end;

var
  FormRunSim: TFormRunSim;
  ManagerAlist : daisy_alist;
  empty_output : daisy_alist; (* Hmmn *)
  FileName     : TFileName;
  StrBuf1, StrBuf2: Array[0..255] of char;
  DoCancel,Running : Boolean;

implementation
Uses UseLib,Parser,IntAct;
{$R *.DFM}
procedure TFormRunSim.RunIt;
var i : integer;
begin
  (* Run the simulation. *)
    Running := True;
    Button1.Caption := 'Cancel';
    Button1.Show;
    time := _daisy_daisy_get_time (daisy);
    columns := _daisy_daisy_count_columns (daisy);
    AddMemoLine('Starting simulation.');
    _daisy_daisy_start (daisy);
    while (0<>_daisy_daisy_is_running (daisy)) and not DoCancel do begin
	_daisy_daisy_tick_action (daisy);
	_daisy_daisy_tick_weather (daisy);
        for i := 0 to columns-1 do begin
	  (* daisy_column* column = daisy_daisy_get_column (daisy, i); *)
    	   _daisy_daisy_tick_column (daisy, i);
        end;
        _daisy_daisy_tick_logs (daisy);
        _daisy_daisy_tick_time (daisy);
        if (_daisy_time_get_hour (time) = 0) then
	   AddMemoLine(IntToStr(_daisy_time_get_year (time))+'-'+
		       IntToStr(_daisy_time_get_month (time))+'-'+
		       IntToStr(_daisy_time_get_mday (time)));
        Application.ProcessMessages;

    end;
    Running := False;
    Button1.Caption := 'Close';
    if DoCancel then
       AddMemoLine('Simulation terminated by user')
    else
       AddMemoLine('Simulation end.');
end;

function TFormRunSim.BuildDaisyObject : boolean;
var
   bIsOurs_dummy : boolean;
   i		 : Integer;
//   WType	 : String;
   dummy_syntax	 : daisy_syntax;
begin
   with MainFormVars do begin
      M := GetLibraryObject('action',SelMan);
      C := GetLibraryObject('column',SelProf);
      W := GetLibraryObject('weather',SelGlob);
      Ms:= GetObjectSyntax('action','mini');
      Cs:= GetObjectSyntax('column','mini');
      Ws:= GetObjectSyntax('weather','file');
      temp := GetManagementStartTime(SelMan,bIsOurs_dummy);

//      GetVariable(W, 'type', daisy_type_string, @WType);
//      ShowMessage(WType);
      if bIsOurs_dummy then
         ManComplete := CheckManager(Ms,M)
      else begin
         if SimulationStartTime <> nil then begin
            Temp := SimulationStartTime;
            ManComplete := true;
         end else
            ManComplete := false;
      end;
      ColumnComplete := 0<> _daisy_syntax_check (Cs, C, StrPCopy(StrBuf1,'daisy'));
      WeatherComplete := 0<> _daisy_syntax_check (Ws, W, StrPCopy(StrBuf1,'daisy'));
      if  ManComplete and ColumnComplete and WeatherComplete then begin
         SimulationAlist := _daisy_alist_create;
	 dummy_syntax := _daisy_syntax_create;
	 _daisy_load (dummy_syntax, SimulationAlist);
         // empty_output := CreateDummyOutputAlist;
         Mc := _daisy_alist_clone(M);
         Cc := _daisy_alist_clone(C);
         Wc := _daisy_alist_clone(W);
         SetVariable(SimulationAlist,'manager',daisy_type_alist,@Mc);
         SetVariableAt(SimulationAlist,'column',0,daisy_type_alist,@Cc);
         SetVariable(SimulationAlist,'weather',daisy_type_alist,@Wc);
         Start:=_daisy_time_create(_daisy_time_get_year(temp),
                                   _daisy_time_get_month(temp),
                                   _daisy_time_get_mday(temp),
                                   {_daisy_time_get_hour(temp)}0);
         SetVariable(SimulationAlist,'time',daisy_type_time,@Start);
         for i:= 0 to MainFormVars.SelOutPut.Count-1 do begin
            empty_output := GetOutputClone(MainFormVars.SelOutPut.Strings[i]);
            SetVariableAt(SimulationAlist,'output',i,daisy_type_alist,@empty_output);
         end;
         result := true;
      end else begin
         AddMemoLine('The simulation could not be started for the following reason(s):');
         if not ManComplete then
            AddMemoLine('The specified management is not complete');
         if not ColumnComplete then
            AddMemoLine('The specified (soil) profile is not complete');
         if not WeatherComplete then
            AddMemoLine('The specified Global information is not complete');
         result := false;
      end;
   end;
end;


procedure TFormRunSim.AddMemoLine(s:String);
var i:integer;
begin
   if Memo1.Lines.Count > 40 then
      Memo1.Lines.Delete(0);
   Memo1.Lines.Add(s);
   Application.ProcessMessages;
end;

procedure TFormRunSim.Button1Click(Sender: TObject);
begin
   if Running then
      DoCancel := True
   else
      ModalResult := mrOk;
end;

procedure TFormRunSim.FormActivate(Sender: TObject);
var
  dummy_alist: daisy_alist;
begin
  (* Initialize syntax  *)
  Memo1.Clear;
  DoCancel := False;
  Running := False;
  Button1.Hide;
  Update;
  Application.ProcessMessages;
  syntax := _daisy_syntax_create;
  dummy_alist := _daisy_alist_create;
  _daisy_load (syntax, dummy_alist);
  if BuildDaisyObject then begin
     daisy  := _daisy_daisy_create (syntax, SimulationAlist(* ManagerAlist *));
     if (0=_daisy_daisy_check (daisy, syntax)) then begin
        Button1.Caption := 'Close';
        Button1.Show;
        AddMemoLine('Daisy Syntax error');
     end else begin
        RunIt;
        _daisy_daisy_delete (daisy);
     end;
  end else begin
     Button1.Caption := 'Close';
     Button1.Show;
  end;
  (* Cleanup. *)
  _daisy_syntax_delete (syntax);
  _daisy_alist_delete (dummy_alist);
end;

procedure TFormRunSim.FormCreate(Sender: TObject);
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
