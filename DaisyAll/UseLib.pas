unit UseLib;

interface
uses   Windows,Classes,SysUtils,Globals, PasDaisy;
const
   daisy_type_integer = 0;
   daisy_type_number = 1;
   daisy_type_string = 2;
   daisy_type_alist = 3;
   daisy_type_time = 4;

var
   Daisy_DLL_Version : String;

function EnumerateObjects(ts:TStringList; LibName: String; FromFile:TFileName):boolean;
function LoadDaisyLibrary(FileName:TFileName) : daisy_alist;
function FromLibrary(obj:String;LibName: String; FromFile:TFileName):boolean;
function IsNameUsed(obj:String;LibName: String; SetUp: TMainFormVars):boolean;
procedure SaveInLibrary(LibName:String; alist :daisy_alist; Name: String; FileName:TFileName);
procedure SaveDaisyLibrary(InternalName, ExternalName : String; Alist:daisy_alist);
procedure DeleteLibraryObjects(LibFileName: String);
function GetVariable(alist:daisy_alist; VariableName: String;VariableType: Integer;V:Pointer):Boolean;
function SetVariable(alist:daisy_alist; VariableName: String;VariableType: Integer;V:Pointer):Boolean;
function GetVariableAt(alist:daisy_alist;VariableName: String;index : Integer;VariableType: Integer;V:Pointer):Boolean;
function SetVariableAt(alist:daisy_alist;VariableName: String;index : Integer;VariableType: Integer;V:Pointer):Boolean;
procedure DeleteLibraryObject(libname:String;ObjectName:String);
function GetLibraryObject(libname:String;ObjectName:String):daisy_alist;
function GetObjectSyntax(libname:String;ObjectName:String):daisy_syntax;
function CreateColumnAlist:daisy_alist;
function CreateHorizonAlist:daisy_alist;
function CreateWeatherAlist:daisy_alist;
function CreateDummyOutputAlist:daisy_alist;
function GetOutputClone(name:String):daisy_alist;
function IsNameSafe(Name:String;LibName:String;User:Boolean): boolean;
function UsedInLib(Name:String; LibName:String): Integer;
function TimeToString(t:daisy_time):string;





implementation
uses Dialogs;
var
  StrBuf1,StrBuf2,StrBuf3 : Array[0..255] of char;

function GetLibraryObject(libname:String;ObjectName:String):daisy_alist;
var
   lib : daisy_library;
begin
   lib := _daisy_library_find(StrPCopy(StrBuf1,LibName));

   if lib = nil then
      Result := nil
   else
      Result := _daisy_library_alist(lib,StrPCopy(StrBuf1,ObjectName));
end;

function GetObjectSyntax(libname:String;ObjectName:String):daisy_syntax;
var
   lib : daisy_library;
begin
   lib := _daisy_library_find(StrPCopy(StrBuf1,LibName));

   if lib = nil then
      Result := nil
   else
      Result := _daisy_library_syntax (lib,StrPCopy(StrBuf1,ObjectName));
end;

{ RR: Returns 0 if the name is not used in any lib, else returns    }
{ 1 = StdRef, 2 = UserRef, 3 = StdTempl, 4 = UserTempl, 5 = SimSpec }
function UsedInLib(Name:String; LibName:String): Integer;
begin
   Result := 0;
   if FromLibrary(Name, LibName, MainFormVars.StdRefLib) then Result := 1
   else if FromLibrary(Name, LibName, MainFormVars.UserRefLib) then Result := 2
   else if FromLibrary(Name, LibName, MainFormVars.StdTemplLib) then Result := 3
   else if FromLibrary(Name, LibName, MainFormVars.UserTemplLib) then Result := 4
   else if FromLibrary(Name, LibName, MainFormVars.SimSpecLib) then Result := 5;
end;

{ RR: This function has become rather STUPID, but is used for not having to update all places in the code }
function IsNameSafe(Name:String;LibName:String;User:Boolean): boolean;
begin
   if (not User) then begin
      result := (not IsNameUsed(Name, LibName,MainFormVars)) and
                (not FromLibrary(Name, LibName, MainFormVars.UserTemplLib)) and
                (not FromLibrary(Name, LibName, MainFormVars.SimSpecLib));
   end
   else begin
      result := (not IsNameUsed(Name, LibName,MainFormVars)) and
                (not FromLibrary(Name, LibName, MainFormVars.UserTemplLib)) and
                (not FromLibrary(Name, LibName, MainFormVars.SimSpecLib));
   end;
end;

function CreateManagerAlist:daisy_alist;
begin
   result := _daisy_alist_create
end;
function CreateDummyOutputAlist:daisy_alist;
begin
   result := GetLibraryObject('log','Crop Production');
   if result = nil then
      exit
   else
      result := _daisy_alist_clone(result);
end;

function GetOutputClone(name:String):daisy_alist;
begin
   result := GetLibraryObject('log',name);
   if result = nil then
      exit
   else
      result := _daisy_alist_clone(result);
end;

function CreateColumnAlist:daisy_alist;
begin
   result := GetLibraryObject('column','mini');
   if result = nil then
      result := _daisy_alist_create
   else
      result := _daisy_alist_clone(result);
end;
function CreateHorizonAlist:daisy_alist;
begin
   result := GetLibraryObject('horizon','mini');
   if result = nil then
      result := _daisy_alist_create
   else
      result := _daisy_alist_clone(result);
end;
function CreateWeatherAlist:daisy_alist;
var
   TypeString : String;
begin
//   result := GetLibraryObject('weather','mini');
   result := GetLibraryObject('weather','file');
   if result = nil then
      result := _daisy_alist_create
   else begin
      TypeString := 'file';
      result := _daisy_alist_clone(result);
      SetVariable(result,'type', daisy_type_string, @TypeString);
   end;
end;

function GetVariable(alist:daisy_alist; VariableName: String;
                     VariableType: Integer;
                     V:Pointer):Boolean;
var ip: ^Integer;
    np: ^Double;
    sp: ^String;
    pp: ^Pointer;
    cstr : PChar;
begin
   result := _daisy_alist_check(alist,StrPCopy(StrBuf1,VariableName)) <> 0;
   if result then begin
      case VariableType of
         daisy_type_integer:
            begin
               ip := V;
               ip^:= _daisy_alist_get_integer(alist,StrPCopy(StrBuf1,VariableName));
            end;
         daisy_type_number:
            begin
               np := V;
               np^:= _daisy_alist_get_number(alist,StrPCopy(StrBuf1,VariableName));
            end;
         daisy_type_string:
            begin
               sp := V;
               cstr := _daisy_alist_get_string(alist,StrPCopy(StrBuf1,VariableName));
               sp^:= StrPas(cstr);
            end;
         daisy_type_alist:
            begin
               pp  := V;
               pp^ := _daisy_alist_get_alist(alist,StrPCopy(StrBuf1,VariableName));
               if pp^ = nil then
                  result := false;
            end;
         daisy_type_time:
            begin
               pp  := V;
               pp^ := _daisy_alist_get_time(alist,StrPCopy(StrBuf1,VariableName));
            end;
         else
            result := False;
      end;
   end;
end;

function SetVariable(alist:daisy_alist; VariableName: String;
                     VariableType: Integer;
                     V:Pointer):Boolean;

var
   i:Integer;
   n:Double;
   s:String;
   a:Pointer;
begin
   result := True;
   case VariableType of
         daisy_type_integer:
            begin
               i := Integer(V^);
               _daisy_alist_set_integer(alist,StrPCopy(StrBuf1,VariableName),i);
            end;
         daisy_type_number:
            begin
               n := Double(V^);
               _daisy_alist_set_number(alist,StrPCopy(StrBuf1,VariableName),n);
            end;
         daisy_type_string:
            begin
               s := string(V^);
               _daisy_alist_set_string(alist,StrPCopy(StrBuf1,VariableName),StrPCopy(StrBuf2,s));
            end;
         daisy_type_alist:
            begin
              a := Pointer(V^);
              _daisy_alist_set_alist(alist,StrPCopy(StrBuf1,VariableName),a);
            end;
         daisy_type_time:
            begin
              a := Pointer(V^);
              _daisy_alist_set_time(alist,StrPCopy(StrBuf1,VariableName),a);
            end;
         else
            result := False;
   end;
end;

function GetVariableAt(alist:daisy_alist;
                       VariableName: String;
                       index : Integer;
                       VariableType: Integer;
                       V:Pointer):Boolean;
var sz : Integer;
    pp: ^Pointer;
    np: ^Double;
begin
   result := _daisy_alist_check(alist,StrPCopy(StrBuf1,VariableName)) <> 0;
   if result then begin
      case VariableType of
         daisy_type_number:
            begin
               sz := _daisy_alist_size_number(alist,StrPCopy(StrBuf1,VariableName));
               if index < sz then begin
                  np := V;
                  np^:= _daisy_alist_get_number_at(alist,StrPCopy(StrBuf1,VariableName),index);
               end else
                  result := false;
            end;
         daisy_type_alist:
            begin
               sz := _daisy_alist_size_alist(alist,StrPCopy(StrBuf1,VariableName));
               if index < sz then begin
                  pp  := V;
                  pp^ := _daisy_alist_get_alist_at(alist,StrPCopy(StrBuf1,VariableName),index);
               end else
                  result := false;
            end;
         else begin
            result := False;
         end;
      end;
   end;
end;

function SetVariableAt(alist:daisy_alist;
                       VariableName: String;
                       index : Integer;
                       VariableType: Integer;
                       V:Pointer):Boolean;
var
   i:Integer;
   n:Double;
   s:String;
   a:Pointer;
begin
   result := True;
   case VariableType of
         daisy_type_integer:
            begin
               result := False;
               (*
               i := Integer(V^);
               _daisy_alist_set_integer_at(alist,StrPCopy(StrBuf1,VariableName),i,index);
               *)
            end;
         daisy_type_number:
            begin
               n := Double(V^);
               _daisy_alist_set_number_at(alist,StrPCopy(StrBuf1,VariableName),n,index);
            end;
         daisy_type_string:
            begin
               result := false;
               (*
               s := string(V^);
               _daisy_alist_set_string_at(alist,StrPCopy(StrBuf1,VariableName),StrPCopy(StrBuf1,s),index);
               *)
            end;
         daisy_type_alist:
            begin
              a := Pointer(V^);
              _daisy_alist_set_alist_at(alist,StrPCopy(StrBuf1,VariableName),a,index);
            end;
         else
            result := False;
   end;
end;

procedure DeleteLibraryObject(libname:String;ObjectName:String);
var
    lib : daisy_library;
begin
   lib := _daisy_library_find(StrPCopy(StrBuf1,LibName));
   _daisy_library_remove (lib, StrPCopy(StrBuf1,ObjectName))
end;

procedure DeleteLibraryObjects(LibFileName: String);
var ObjectList : TStringList;
    lib        : daisy_library;
    StrBuf1    : array [0..255] of char;
    LibName    : String;
    i          : Integer;
begin
   (* Init *)
   ObjectList := TStringList.Create;
   (* Delete Objects i "action" library *)
   LibName := 'action';
   EnumerateObjects(ObjectList,LibName,LibFileName);
   for i:= 0 to ObjectList.Count-1 do begin
      DeleteLibraryObject(LibName, ObjectList.Strings[i])
   end;

   (* Delete Objects i "horizon" library *)
   ObjectList.Clear; LibName := 'horizon';
   EnumerateObjects(ObjectList,LibName,LibFileName);
   for i:= 0 to ObjectList.Count-1 do begin
      DeleteLibraryObject(LibName, ObjectList.Strings[i])
   end;

   (* Delete Objects i "column" library *)
   ObjectList.Clear; LibName := 'column';
   EnumerateObjects(ObjectList,LibName,LibFileName);
   for i:= 0 to ObjectList.Count-1 do begin
      DeleteLibraryObject(LibName, ObjectList.Strings[i])
   end;

   (* Delete Objects i "weather" library *)
   ObjectList.Clear; LibName := 'weather';
   EnumerateObjects(ObjectList,LibName,LibFileName);
   for i:= 0 to ObjectList.Count-1 do begin
      DeleteLibraryObject(LibName, ObjectList.Strings[i])
   end;

   (* Clean Up *)
   ObjectList.Free;
   ObjectList := nil;
end;

procedure SaveDaisyLibrary(InternalName, ExternalName : String; Alist:daisy_alist);
var printer : daisy_printer;
    StrBuf1 : Array [0..255] of char;
    Org_fn  : String;
    syntax:daisy_syntax;
    parser:daisy_parser;
    d_alist :daisy_alist;

begin
  Org_fn := ExternalName;
  syntax := _daisy_syntax_create;
  if syntax = nil then begin
     ShowMessage('Internal error , no changes saved to ' +  Org_fn);
     exit;
  end;
  d_alist := _daisy_alist_create;
  if d_alist = nil then begin
     ShowMessage('Internal error, no changes saved to ' +  Org_fn);
     exit;
  end;
  _daisy_load (syntax, d_alist);

   if FileExists(Org_fn) then begin
      ExternalName[Length(ExternalName)] := '~';
      RenameFile(Org_fn, ExternalName);
   end;
   printer := _daisy_printer_create_file(StrPCopy(StrBuf1, Org_fn));
   _daisy_printer_comment(printer,StrPCopy(StrBuf1, 'File saved by "small" daisy'));

   _daisy_printer_library_file(printer,StrPCopy(StrBuf1, InternalName));
   if _daisy_printer_good(printer) = 0 then begin
      _daisy_printer_delete(printer);
      RenameFile(ExternalName, Org_fn);
      ShowMessage('Error writing file, no changes saved to ' +  Org_fn);
   end;
   if Alist <> nil then begin
      _daisy_printer_alist(printer, Alist, syntax);
      if _daisy_printer_good(printer) = 0 then begin
         _daisy_printer_delete(printer);
         RenameFile(ExternalName, Org_fn);
         ShowMessage('Error writing file, no changes saved to ' +  Org_fn);
      end;
   end;

   _daisy_printer_delete(printer);
end;

procedure SaveInLibrary(LibName:String; alist :daisy_alist; Name: String; FileName:TFileName);
var lib : daisy_library;
begin
   if FromLibrary(Name,LibName,FileName)then
      DeleteLibraryObject(LibName,Name);
   lib := _daisy_library_find(StrPCopy(StrBuf1,LibName));
   _daisy_library_derive (lib, StrPCopy(StrBuf1,'mini'), alist,
		                    StrPCopy(StrBuf2,Name),
                          StrPCopy(StrBuf3,Filename));
end;

function IsNameUsed(obj:String;LibName: String; SetUp: TMainFormVars):boolean;
begin
   (*
   Names in Std Ref. Lib., Std. Templ. Lib. and in Usr Ref. Lib.
   are reserved !
   *)

   { RR! Return TRUE if found in ONE of the libs... }
   result := FromLibrary(obj,LibName,SetUp.StdRefLib) or
             FromLibrary(obj,LibName,SetUp.UserRefLib) or
             FromLibrary(obj,LibName,SetUp.StdTemplLib);

(*   result := FromLibrary(obj,LibName,SetUp.StdRefLib) and
             FromLibrary(obj,LibName,SetUp.UserRefLib) and
             FromLibrary(obj,LibName,SetUp.StdTemplLib);
*)
end;

function LoadDaisyLibrary(FileName:TFileName) : daisy_alist;
var
  StrBuf1: Array[0..255] of char;
  syntax:daisy_syntax;
  parser:daisy_parser;
  alist :daisy_alist;
begin
  syntax := _daisy_syntax_create;
  if syntax = nil then begin
     result := nil;
     exit;
  end;
  alist := _daisy_alist_create;
  if alist = nil then begin
     result := nil;
     exit;
  end;
  _daisy_load (syntax, alist);
  parser := _daisy_parser_create_file (syntax, StrPCopy(StrBuf1,FileName));
  if parser = nil then begin
     result := nil;
     exit;
  end;
  _daisy_parser_load (parser, alist);
  result := alist;
end;

function FromLibrary(obj:String;LibName: String; FromFile:TFileName):boolean;
var
   lib    : daisy_library;
   sz,i   : Integer;
   s      : PChar;
   ns,str : String;
   StrBuf1: Array[0..255] of char;
begin
   result := false;
   lib := _daisy_library_find(StrPCopy(StrBuf1,LibName));
   if lib <> nil then begin
      sz := _daisy_library_size(lib);
      for i := 0 to sz -1 do begin
         str := StrPas( _daisy_library_name(lib,i));
         if str = obj then begin
             s  := _daisy_library_file(lib,StrPCopy(StrBuf1,str));
             if s <> nil then begin
                ns := StrPas(s);
                if ns = FromFile then begin
                   result := True;
                   exit;
                end;
             end;
          end;
      end;
   end;
end;

function EnumerateObjects(ts:TStringList; LibName: String; FromFile:TFileName):boolean;
var
   lib    : daisy_library;
   sz,i   : Integer;
   s      : PChar;
   ns,str : String;
   StrBuf1: Array[0..255] of char;

begin
   result := false;
   lib := _daisy_library_find(StrPCopy(StrBuf1,LibName));
   if lib <> nil then begin
      result := true;
      sz := _daisy_library_size(lib);
      for i := 0 to sz -1 do begin
         str := StrPas( _daisy_library_name(lib,i));
          s  := _daisy_library_file(lib,StrPCopy(StrBuf1,str));
          if s <> nil then begin
             ns := StrPas(s);
             if ns = FromFile then
                ts.Add(str);
          end;
      end;
   end;
end;

function TimeToString(t:daisy_time):string;
begin
   result := '';
   if t <> nil then begin
      result := '(time ' +
         IntToStr(_daisy_time_get_year(t)) + ' ' +
         IntToStr(_daisy_time_get_month(t)) + ' ' +
         IntToStr(_daisy_time_get_mday(t)) + '  1)';
   end;
end;

initialization
   _daisy_initialize;
   Daisy_DLL_Version := StrPas( _daisy_version );
finalization

end.
