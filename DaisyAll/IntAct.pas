unit IntAct;
{
***************
* Unit IntAct *
***************

Project    : Graphical User Interface for the Daisy simulation package

Purpose    : Internal representation of management actions

Author     : J&R Informatik I/S (Jens Jeppesen)

Date(s)    : 11/2 - 97 Created
             12/2 - 97 Revised (Added Virtual function Specification)
             13/2 - 97 -



            NOTE : WitchList should be read as "Which" List, however the first spelling are used in many places

}

interface
uses classes, Pasdaisy,UseLib;
type
   { Forward needed by time classes}
   TInternAction = class;
   TSowing = class;
   THarvest = class;
   { Class to capture when an action takes place }
   TNewDaisyTime = class(TObject)
      function TimeSpec : string; virtual;
      function print : string; virtual;
      function printNew : string; virtual;
      function printNew_alist : daisy_alist; virtual;
   end;
   { Fixed date }
   TDate = class(TNewDaisyTime)
         year, month, day : integer;
         function ValidateDate(s:String):boolean;
         function Date(show:boolean) : String;
         function DateNew : String;
         function TimeSpec : string; override;
         function print : string; override;
         function printNew : string; override;
         function printNew_alist : daisy_alist; override;
         function Compare(another : TDate):Integer;
   end;
   { Approx. date - same as TDate it's just approx.}
   TCaDate = class(TDate)
         function TimeSpec : string; override;
         function print : string; override;
         function printNew : string; override;
         function printNew_alist : daisy_alist; override;
   end;
   { Relative to other action - possibly displaced n days }
   TRelToAction = class(TNewDaisyTime)
      Action : TInternAction;    { Keep only as reference i.e. need not create }
      Displacement : integer;
      function TimeSpec : string; override;
      function print : string; override;
      function printNew : string; override;
      function printNew_alist : daisy_alist; override;
   end;
   { Relative to development state of a crop from a sowing action }
   { used ONLY by TFuncHarvest }
   TRelToDevelopmentState = class(TNewDaisyTime)
      SowingAction : TSowing; { Keep only as reference i.e. need not create }
      DevStage : Double;
      NumberInYear : Integer; {1=First time, 2 = Second time ....}
      YearNumber : integer;
      Constructor Create;
      function TimeSpec : string; override;
      function print : string; override;
      function printNew : string; override;
      function printNew_alist : daisy_alist; override;
   end;
   { List of all actions }
   TInternActionList = class(TList)
      Dirty : Boolean;
      function DoAdd(i:Integer;Item:TInternAction):integer;
      function Add(Item:TInternAction):Integer;
      procedure DoDelete(ia : TInternAction);
      { Retrieve list of actions of an given type }
      procedure GetList(Actiontype : TClass; res : TList);
      { Action plads i listen + 1 }
      function GetObjIdx(obj:TObject):integer;
      (*
      function SaveToFile(fn:string):boolean;
      function SaveToFile2(fn:string):boolean;
      *)
      function Export_it(fn:string):boolean;
      function Import_it(fn:string):boolean;

      function LoadFromFile(fn:string):boolean;

      procedure SubTree(a:TInternAction; res : TList);
      procedure GetListExSubTree(a:TInternAction;res:TList);
      procedure ClearList;
      procedure NewList;
      function IsDirty:Boolean;
      procedure SetDirty(d:Boolean);
      constructor Create;
      function TopSort(CheckTime:Boolean):Boolean;
      function CheckDates : Boolean;
      function CreateAttributeList: daisy_alist;
   end;
   { Class to capture an action }
   TInternAction = class(TObject)
      { when the atcion takes place }
      when : TNewDaisyTime;
      indegree : Integer; {For TopSort}
      { Witch list object is part of }
      { This is needed because an object}
      { might want to know its index in }
      { the list }
      WitchList : TInternActionList;
      constructor Create;
      procedure Free;
      { Set date of this action }
      function SetDato(D:String):Boolean;
      { Set (approx.) date of this action }
      function SetCaDato(D:String):Boolean;
      { Relate this action to other action }
      procedure RelateToAction(Ac : TInternAction; Disp:integer);
      { Relate this action to development state }
      procedure RelateToDevelopmentState(SowAction:TSowing; St:Double; No,Y:Integer);
      procedure UnRelate;
      function ActionName : String;
      function Specification : String ; virtual;
      function print: String; virtual;
      function params: String; virtual;
      function paramsnew: daisy_alist; virtual;
      procedure Depends(l:TList);
      procedure TimeDepends(l:TList);

   end;
   { Tillage action }
   TTillage = class(TInternAction)
      how : string;
      function Specification : String; override;
      function print : string; override;
      function params: String; override;
      function paramsnew: daisy_alist; override;
   end;
   { Sowing action }
   TSowing = class(TInternAction)
      what : string;
      model : string;
      noHarvActions : Integer;
      noYears:Integer;
      function Specification : String; override;
      constructor Create;
      function print : string; override;
      function params: String; override;
      function paramsnew: daisy_alist; override;
      function GetHarvestAction:THarvest;
      procedure HarvestDepends(l:TList);
   end;
   { Fertilize action }
   TFertilize = class(TInternAction)
      withWhat : string;
      ammount : integer;
      function Specification : String; override;
      function params: String; override;
      function paramsnew: daisy_alist; override;
      function print : string; override;
   end;
   { Irrigate action }
   TIrrigate = class(TInternAction)
      HowMutch : integer;
      UseAir, OverheadIrr:Boolean;
      Temp : Integer;
      constructor Create;
      function Specification : String;override;
      function params: String; override;
      function paramsnew: daisy_alist; override;
      function print : string; override;
   end;
   { Harvest action }
   THarvest = class(TInternAction)
      CropSowed : TSowing;
      SO,Stem,DM,Leaf,Stub:Integer;
      constructor Create;
      function Specification : String; override;
      function params: String; override;
      function paramsnew: daisy_alist; override;
      function print : string; override;
   end;
   TFuncHarvest = class(TInternAction)
      CropSowed : TSowing;
      SO,Stem,DM,Leaf,Stub:Integer;
      constructor Create;
      function Specification : String; override;
      function params: String; override;
      function paramsnew: daisy_alist; override;
      function print : string; override;
   End;
   { Start of simulation "action" }
   TStart = class(TInternAction)
      function print : string; override;
      function params: String; override;
      function paramsnew: daisy_alist; override;
   end;
   { End of simulation "action" }
   TEnd = class(TInternAction)
      function print : string; override;
      function params: String; override;
      function paramsnew: daisy_alist; override;
   end;
   Function IsOurManager(alist : daisy_alist): Boolean;
   Function CheckManager(syntax : daisy_syntax; alist : daisy_alist): Boolean;

implementation
uses SysUtils,Parser, Dialogs;


Function IsOurManagerSyntax(syntax : daisy_syntax; alist : daisy_alist): Boolean;
var
    StrBuf1 : array[0..20] of char;
begin
   result := 0<> _daisy_syntax_check (syntax, alist, StrPCopy(StrBuf1,'daisy'));
end;
Function IsOurManager(alist : daisy_alist): Boolean;
var
   Ms : daisy_syntax;
begin
   Ms:= GetObjectSyntax('action','mini');
   result := IsOurManagerSyntax(Ms,alist);
end;

Function CheckManager(syntax : daisy_syntax; alist : daisy_alist): Boolean;
var AL:TInternActionList;
    StrBuf1 : array[0..20] of char;
begin
   result := IsOurManagerSyntax(syntax,alist);
   if not result then exit;

   AL := TInternActionList.Create;
   result := CreateActionListFromAlist(alist, AL as TList);
   if not result then begin
      AL.Free;
      exit;
   end;
   result := AL.CheckDates;
   if result then
      result := AL.TopSort(true);
   AL.Free;
end;

{ ***** TDaisyTime ***** }

function TNewDaisyTime.TimeSpec : string;
begin
     TimeSpec := '';
end;
function TNewDaisyTime.print : string;
begin
   print := '';
end;

function TNewDaisyTime.printNew : string;
begin
   printnew := '';
end;

function TNewDaisyTime.printNew_alist : daisy_alist;
begin
   result := _daisy_alist_create;

end;

{ ***** TDate ***** }


function TDate.Compare(another:TDate):Integer;
begin
   if Year < another.Year then
      result := -1
   else if Year > another.Year then
      result := 1
   else begin
      if month < another.month then
         result := -1
      else if month > another.month then
         result := 1
      else begin
         if day < another.day then
            result := -1
         else if day > another.day then
            result := 1
         else
            result := 0;
      end
   end;
end;
function TDate.ValidateDate(s:String):boolean;
var i,j:integer;
    ys,ms,ds:string;
    res : boolean;
    code : integer;
    function leap(y:integer):boolean;
    begin
       leap := (y mod 4 = 0) and
               ((y mod 100 <>0) or (y mod 400 = 0));
    end;
begin
   res := true;
   j:=0;
   ys := '';
   ms := '';
   ds := '';
   for i := 1 to Length(s) do begin
      if s[i] = '-' then
         inc(j)
      else begin
         case j of
            0: ys := ys + s[i];
            1: ms := ms + s[i];
            2: ds := ds + s[i]
         else
            res := false;
         end;
      end;
   end;
   if res then begin
      val(ys,year,code);
      if code <> 0 then
         res := false;
   end;
   if res then begin
      val(ms,month,code);
      if code <> 0 then
         res := false;
   end;
   if res then begin
      val(ds,day,code);
      if code <> 0 then
         res := false;
   end;
   if res then begin
      if (year>0)and(month in [1..12])and(day in [1..31]) then begin
         case month of
            1,3,5,7,8,10,12:
               res := day in [1..31];
            4,6,9,11:
               res := day in [1..30];
            2: if leap(year) then
                  res := day in [1..29]
               else
                  res := day in [1..28];
         end;
      end else
         res := false;
   end;
   ValidateDate := res;
end;


// Shit format - day/month fixed size, but not year!!
function TDate.Date(show:boolean):string;
var
   ms, ds, ys: string;
begin
   if (month < 10) and show then
      ms := '0'+IntToStr(month)
   else
      ms := IntToStr(month);
   if (day < 10) and show then
      ds := '0'+IntToStr(day)
   else
      ds := IntToStr(day);
//  Date := IntToStr(year)+'-'+ms+'-'+ds;
// down on: new by RR 121098
   ys := IntToStr(year);
   if (year < 1000) and show then 
      ys := Copy('0000', 1, 4-Length(ys)) + ys;
   Date := ys + '-' + ms + '-' + ds;
end;

function TDate.DateNew:string;
var
   ms, ds : string;
begin
   ms := IntToStr(month);
   ds := IntToStr(day);
   DateNew := IntToStr(year)+' '+ms+' '+ds;
end;

function TDate.TimeSpec : string;
begin
     TimeSpec := Date(True);
end;
function TDate.Print : string;
begin
   Print := '(Abs ' + Date(False) +')';
end;

function TDate.PrintNew : string;
begin
   PrintNew := ' "Abs" (Dato ' + DateNew +')';
end;

function TDate.printNew_alist : daisy_alist;
var
   buf1: Array[0..255] of char;
   buf2: Array[0..255] of char;
   time : daisy_time;
   Str  : String;
begin
   result := _daisy_alist_create;
   Str := 'Abs';
   SetVariable(result, 'Type', daisy_type_string,@Str);
   (* _daisy_alist_set_string(result, StrPCopy(buf1,'Type'), StrPCopy(buf2,'Abs'));*)
   time := _daisy_time_create(year, month, day,1);
   SetVariable(result, 'Dato', daisy_type_time,@time);
   (* _daisy_alist_set_time(result, StrPCopy(buf1,'Dato'), time); *)
end;


function TCaDate.TimeSpec : string;
var
   s : string;
begin
     s := inherited TimeSpec;
     TimeSpec := '>=' + s;
end;

function TCaDate.Print : string;
begin
   Print := '(Ca ' + Date(False) +')';
end;

function TCaDate.PrintNew : string;
begin
   PrintNew := ' "Ca" (Dato ' + DateNew +')';
end;

function TCaDate.printNew_alist : daisy_alist;
var
   buf1: Array[0..255] of char;
   buf2: Array[0..255] of char;
   time : daisy_time;
begin
   result := _daisy_alist_create;
   _daisy_alist_set_string(result, StrPCopy(buf1,'Type'), StrPCopy(buf2,'Ca'));
   time := _daisy_time_create(year, month, day,1);
   _daisy_alist_set_time(result, StrPCopy(buf1,'Dato'), time);
end;


function TRelToAction.TimeSpec:string;
begin
   Result := IntToStr(Action.WitchList.GetObjIdx(Action))+' '+Action.ActionName;
   if Displacement > 1 then
      Result := Result + ' + ' + IntToStr(Displacement) + ' days'
   else if Displacement > 0 then
      Result := Result + ' + ' + IntToStr(Displacement) + ' day';
end;

function TRelToAction.print:string;
begin
   Print := '(Rel ' +IntToStr(Action.WitchList.GetObjIdx(Action))+
            ' '+IntToStr(displacement)+ ')'
end;

function TRelToAction.printNew:string;
begin
   PrintNew := ' "Rel" (Id ' +IntToStr(Action.WitchList.GetObjIdx(Action))+
            ') (Days '+IntToStr(displacement)+ ')'
end;

function TRelToAction.printNew_alist : daisy_alist;
var
   buf1: Array[0..255] of char;
   buf2: Array[0..255] of char;
begin
   result := _daisy_alist_create;
   _daisy_alist_set_string(result, StrPCopy(buf1,'Type'), StrPCopy(buf2,'Rel'));
   _daisy_alist_set_integer(result, StrPCopy(buf1,'Id'), Action.WitchList.GetObjIdx(Action));
   _daisy_alist_set_integer(result, StrPCopy(buf1,'Days'), displacement);
end;


Constructor TRelToDevelopmentState.Create;
begin
   inherited Create;
   YearNumber := 1;
end;

function TRelToDevelopmentState.TimeSpec:string;
begin
   TimeSpec := IntToStr(SowingAction.WitchList.GetObjIdx(SowingAction))+' '+SowingAction.ActionName +'(('+IntToStr(NumberInYear)+')('+IntToStr(YearNumber)+'))';
end;
function TRelToDevelopmentState.print:string;
Var s:String;
begin
   Str(DevStage:8:4,s);
   Print := '(Sow ' +IntToStr(SowingAction.WitchList.GetObjIdx(SowingAction))+
            ' ' + IntToStr(NumberInYear) + ' ' + IntToStr(YearNumber) + ' ' + s +')'

end;

function TRelToDevelopmentState.printnew:string;
Var s:String;
begin
   Str(DevStage:8:4,s);
   PrintNew := ' "Sow" (Id ' +IntToStr(SowingAction.WitchList.GetObjIdx(SowingAction))+
            ') (NoInYear ' + IntToStr(NumberInYear) + ') (Year ' + IntToStr(YearNumber) + ') (DevelopmentStage ' + s +')'

end;

function TRelToDevelopmentState.printNew_alist : daisy_alist;
var
   buf1: Array[0..255] of char;
   buf2: Array[0..255] of char;
begin
   result := _daisy_alist_create;
   _daisy_alist_set_string(result, StrPCopy(buf1,'Type'), StrPCopy(buf2,'Sow'));
   _daisy_alist_set_integer(result, StrPCopy(buf1,'Id'), SowingAction.WitchList.GetObjIdx(SowingAction));
   _daisy_alist_set_integer(result, StrPCopy(buf1,'NoInYear'), NumberInYear);
   _daisy_alist_set_integer(result, StrPCopy(buf1,'Year'), YearNumber);

   _daisy_alist_set_number(result, StrPCopy(buf1,'DevelopmentStage'), DevStage);
end;


{ ***** TInternAction ***** }

constructor TInternAction.Create;
begin
   when := nil;
   inherited Create;
end;

procedure TInternAction.Free;
begin
   if self <> nil then begin
   if when <> nil then
      when.Free;
   end;
   {inherited Free;}
end;

function TInternAction.ActionName : String;
var s:string;
begin
     s := self.ClassName;
     Delete(s,1,1);
     ActionName := s;
end;
function TInternAction.print : String;
begin
   print := ActionName;
end;

function TInternAction.params : String;
begin
   params := ActionName;
end;

function TInternAction.paramsnew: daisy_alist;
begin
   result := _daisy_alist_create;
end;

function TInternAction.Specification : String;
begin
     Specification := ActionName + ' Specification';
end;

function TInternAction.SetDato(D:String):boolean;
begin
   if when <> nil then
      when.free;
   when := TDate.Create;
   with when as TDate do begin
      if ValidateDate(D) then begin
         SetDato := True
      end else begin
         when.Free;
         when := nil;
         SetDato := False;
      end;
   end;
   if WitchList <> nil then
      WitchList.SetDirty(True);
end;

function TInternAction.SetCaDato(D:String):boolean;
begin
   if when <> nil then
      when.free;
   when := TCaDate.Create;
   with when as TCaDate do begin
      if ValidateDate(D) then begin
         SetCaDato := True
      end else begin
         when.Free;
         when := nil;
         SetCaDato := False;
      end;
   end;
   if WitchList <> nil then
      WitchList.SetDirty(True);
end;

procedure TInternAction.UnRelate;
begin
   if when <> nil then begin
      when.free;
      when := nil;
   end;
   if WitchList <> nil then
      WitchList.SetDirty(True);
end;

procedure TInternAction.RelateToAction(Ac : TInternAction; Disp:integer);
begin
   when := TRelToAction.Create;
   with when as TRelToAction do begin
        Action := Ac;
        Displacement := Disp;
   end;
   if WitchList <> nil then
      WitchList.SetDirty(True);
end;

procedure TInternAction.RelateToDevelopmentState(SowAction:TSowing; St:Double; No,Y:Integer);
begin
   when := TRelToDevelopmentState.Create;
   with when as TRelToDevelopmentState do begin
        SowingAction := SowAction;
        DevStage := St;
        NumberInYear := No;
        YearNumber := Y;
   end;
   if WitchList <> nil then
      WitchList.SetDirty(True);
end;

procedure TInternAction.TimeDepends(l:TList);
var obj : TInternAction;
    i   : integer;
begin
   if l <> nil then begin
      with witchlist do begin
         for i:=0 to count-1 do begin
             obj := items[i];
             if obj.when is TRelToAction then with obj.when as TRelToAction do begin
                if Action = self then
                   l.Add(obj);
             end else if obj.when is TRelToDevelopmentState then with obj.when as TRelToDevelopmentState do begin
                if SowingAction = self then
                   l.Add(obj);
             end;
         end;
      end;
   end;
end;
procedure TInternAction.Depends(l:TList);
var obj : TInternAction;
    i   : integer;
begin
   if l <> nil then begin
      TimeDepends(l);
      if Self is TSowing then with self as TSowing do
         HarvestDepends(l);
   end;
end;

{ ***** TInternActionList ***** }


{****** Topological sort ******}
{
IF CheckTime is true ALL actions MUST have a timespecification
Returns False: Topological ordering not possible
        True : Topological ordering possible
}
function TInternActionList.TopSort(CheckTime:Boolean):Boolean;
var i   :Integer;
    ia1,
    ia2 : TInternAction;
    W   : TNewDaisyTime;
    Q,S : TList;
begin
   Q := TList.Create;
   S := TList.Create;
   for i:=0 to Count-1 do begin
       ia1 := Items[i];
       ia1.indegree := 0;
   end;
   for i:= 0 to Count -1 do begin
      ia1 := Items[i];
      W   := ia1.When;
      if W <> nil then begin
         if W is TRelToAction then with W as TRelToAction do begin
            Action.indegree := Action.indegree+1;
         end else if W is TRelToDevelopmentState then with W as TRelToDevelopmentState do begin
            SowingAction.indegree := SowingAction.indegree+1;
         end;
      end else if CheckTime then begin
         S.Free;
         Q.Free;
         Result := False;
         Exit;
      end;

      if ia1 is THarvest then with ia1 as THarvest do begin
         CropSowed.indegree := CropSowed.indegree+1;
      end;
   end;
   for i:=0 to Count-1 do begin
      ia1 := Items[i];
      if ia1.Indegree = 0 then
         Q.Add(ia1);
   end;
   while Q.Count > 0 do begin
      ia1 := Q.Items[0];
      Q.Remove(ia1);
      Q.Pack;
      S.Add(ia1);
      W := ia1.When;
      if W <> nil then begin
         if W is TRelToAction then with W as TRelToAction do begin
            Action.indegree := Action.indegree-1;
            if Action.indegree = 0 then
               Q.Add(Action);
         end else if W is TRelToDevelopmentState then with W as TRelToDevelopmentState do begin
            SowingAction.indegree := SowingAction.indegree-1;
            if SowingAction.indegree = 0 then
               Q.Add(SowingAction);
         end;
      end;
      if ia1 is THarvest then with ia1 as THarvest do begin
         CropSowed.indegree := CropSowed.indegree-1;
         if CropSowed.indegree = 0 then
            Q.Add(CropSowed);
      end;
   end;
   Q.Free;
   S.Free;
   i:= 0; Result := True;
   while (i < Count) and Result do begin
      ia1 := Items[i]; Inc(i);
      Result := ia1.indegree = 0;
   end;
end;

function TInternActionList.CheckDates : boolean;
var i   :Integer;
    ia  : TInternAction;
    StartW, EndW, TempW  : TNewDaisyTime;
    compRes : Integer;
begin
   result := false;
   if Count < 2 then
      exit;
   ia := Items[0];
   if ia = nil then
      exit;
   StartW := ia.When;
   if StartW = nil then
      exit;
   ia := Items[Count-1];
   if ia = nil then
      exit;
   EndW := ia.When;
   if EndW = nil then
      exit;
   if (not(StartW is TDate)) or (not(EndW is TDate)) then
      exit;
   if (StartW as TDate).Compare(EndW as TDate) <> -1 then
      exit;
   for i:=1 to Count-2 do begin
      ia := Items[i];
      TempW := ia.When;
      if TempW = nil then exit;
      if (TempW is TDate) or (TempW is TCaDate) then begin
         if ((StartW as TDate).Compare(TempW as TDate) > 0) or ((EndW as TDate).Compare(TempW as TDate) < 0) then
            exit;
      end;
   end;
   result := true;
end;

procedure TInternActionList.NewList;
begin
   ClearList;
   Add(TStart.Create);
   Add(TEnd.Create);
   SetDirty(False);
end;
procedure TInternActionList.ClearList;
var i:integer;
    o:TObject;
begin
   for i := 0 to count-1 do begin
      o := Items[i];
      o.Free;
   end;
   Clear;
   SetDirty(False);
end;

procedure TInternActionList.GetListExSubTree(a:TInternAction;res:TList);
var l   :TList;
    obj : TInternAction;
    i   : integer;
begin
   if res <> nil then begin
      res.Clear;
      l := TList.Create;
      SubTree(a,l);
      for i := 0 to Count-1 do begin
          if (l.IndexOf(Items[i]) = -1) then begin
             obj := Items[i];
             if not(obj is TEnd) then
                res.Add(obj);
          end;
      end;
      l.Free;
   end;
end;

procedure TInternActionList.DoDelete(ia : TInternAction);
begin
   Delete(IndexOf(ia));
   {ia.WitchList := nil;}
   Pack;
   SetDirty(True);
end;
procedure TInternActionList.GetList(Actiontype : TClass; res : TList);
var
   i : integer;
   obj : TObject;
begin
   if res <> nil then begin
      for i:= 0 to count - 1 do begin
         obj := Items[i];
         {if Actiontype = TInternAction then begin
            if not ((obj is TStart) or (obj is TEnd)) then
               res.Add(Items[i])
         end else} if obj is Actiontype then
            res.Add(Items[i]);
      end;
   end;
end;

function TInternActionList.GetObjIdx(obj:TObject):integer;
begin
   GetObjIdx := IndexOf(obj) + 1;
end;
(*
function TInternActionList.SaveToFile(fn:string):boolean;
var f:TextFile;
    i:integer;
    o:TObject;
begin
   AssignFile(f,fn);
   Rewrite(f);
   writeln(f,'(ActionList');
   for i := 0 to Count-1 do begin
       o := items[i];
      with o as TInternAction do
         writeln(f,'   (',i+1,' '+print+')');
   end;
   writeln(f,')');
   CloseFile(f);
   SetDirty(False);
end;

function TInternActionList.SaveToFile2(fn:string):boolean;
var f:TextFile;
    i:integer;
    o:TObject;
begin
   AssignFile(f,fn);
   Rewrite(f);
   writeln(f,'(manager "mini"');
   for i := 0 to Count-1 do begin
       o := items[i];
      with o as TInternAction do begin
         writeln(f,'   (',i+1,' "',ActionName,'"');
            writeln(f,'       (Params ',params,')');
            if when <> nil then
               writeln(f,'      (Time',when.printnew,')')
            else
               writeln(f,'      (Time)');
         writeln(f,'   )');
      end;
   end;
   writeln(f,')');
   CloseFile(f);
   SetDirty(False);
end;
*)

function TInternActionList.CreateAttributeList: daisy_alist;
var i:integer;
    o:TObject;
    AlistSize: integer;
    manager, action, actiontype ,para, time :daisy_alist ;
    buf : array[0..255] of char;
    buf2 : array[0..255] of char;
begin
   manager       := _daisy_alist_create;
   if manager = nil then begin
      result := manager;
      exit;
   end else begin
      for i := 0 to Count-1 do begin
         o := items[i];
         with o as TInternAction do begin
            action := _daisy_alist_create;
            if action = nil then begin
               _daisy_alist_delete(manager);
               result := action;
               exit;
            end;
            _daisy_alist_set_integer(action,StrPCopy(buf,'Id'),i+1);
            _daisy_alist_set_string(action,StrPCopy(buf,'Type'),StrPCopy(buf2,ActionName));
            para := ParamsNew;
            if para = nil then begin
               _daisy_alist_delete(manager);
               result := para;
               exit;
            end;
            _daisy_alist_set_alist(action,StrPCopy(buf,'Params'),para);
            if when <> nil then begin
               time := when.printnew_alist;
            end else begin
               time := _daisy_alist_create;
               if time <> nil then
                  _daisy_alist_set_string(time,StrPCopy(buf,'Type'),StrPCopy(buf2,'No time specified'));
            end;
            if time = nil then begin
               _daisy_alist_delete(manager);
               result := time;
               exit;
            end;
            _daisy_alist_set_alist(action,StrPCopy(buf,'Time'),time);
         end;
         (* _daisy_alist_set_alist_at(manager,StrPCopy(buf,'Action'),action,i);*)
         SetVariableAt(manager,'Action',i,daisy_type_alist,@action);
      end;
   end;
   result := manager;
end;


function TInternActionList.Export_it(fn:string):boolean;
var i:integer;
    o:TObject;
    lib : daisy_library;
    managersyntax : daisy_syntax;
    manager : daisy_alist ;
    buf : array[0..255] of char;
    buf2 : array[0..255] of char;
begin
   lib           := _daisy_library_find(StrPCopy(buf,'action'));
   if lib = nil then begin
      ShowMessage('Export: Unable to locate manager library');
      result := false;
      exit;
   end;
   managersyntax := _daisy_library_syntax(lib,StrPCopy(buf,'mini'));
   if managersyntax = nil then begin
      ShowMessage('Export: Unable to locate "mini" manager syntax');
      result := false;
      exit;
   end;
   manager       := CreateAttributeList;
   if manager = nil then begin
      ShowMessage('Export: Unable to create AttributeList from Manager actions');
      result := false;
      exit;
   end;
   {if  -1 = _daisy_alist_save (manager, managersyntax, StrPCopy(buf,fn)) then begin
      ShowMessage('Export: Unable to export Manager actions');
      result := false;
      exit;
   end;}
   _daisy_alist_delete(manager);
   result := true;
end;

function TInternActionList.Import_it(fn:string):boolean;
var i:integer;
    o:TObject;
    lib : daisy_library;
    managersyntax : daisy_syntax;
    manager : daisy_alist ;
    buf : array[0..255] of char;
    buf2 : array[0..255] of char;
    parser : daisy_parser;
begin
   lib           := _daisy_library_find(StrPCopy(buf,'action'));
   if lib = nil then begin
      ShowMessage('Import: Unable to locate manager library');
      result := false;
      exit;
   end;
   managersyntax := _daisy_library_syntax(lib,StrPCopy(buf,'mini'));
   if managersyntax = nil then begin
      ShowMessage('Import: Unable to locate "mini" manager syntax');
      result := false;
      exit;
   end;
   parser := _daisy_parser_create_file(managersyntax, StrPCopy(buf,fn));
   if parser = nil then begin
      ShowMessage('Import: Unable to create file parser');
      result := false;
      exit;
   end;

   manager := _daisy_alist_create;
   if manager = nil then begin
      ShowMessage('Import: Unable to create empty AttributeList ');
      result := false;
      exit;
   end;
   _daisy_parser_load(parser, manager);
   (*
      ShowMessage('Import: Error reading file '+ fn);
      _daisy_parser_delete(parser);
      _daisy_alist_delete(manager);
      result := false;
      exit;
   *)
   if not CreateActionListFromAlist(manager, self as TList) then begin
      ShowMessage('Import: Error importing from AttributeList');
      _daisy_parser_delete(parser);
      _daisy_alist_delete(manager);
      result := false;
      exit;
   end;
   _daisy_parser_delete(parser);
   _daisy_alist_delete(manager);
   result := true;
end;


function TInternActionList.LoadFromFile(fn:string):boolean;
begin
{
   ClearList;
   if not ParseActionFile(fn,self) then begin
      NewList;
      Result := False;
   end else begin
      SetDirty(False);
      Result := True;
   end;
}
   result := false;
end;

constructor TInternActionList.Create;
begin
    inherited Create;
    Add(TStart.Create);
    Add(TEnd.Create);
    Dirty := False;
end;

function TInternActionList.IsDirty: Boolean;
begin
   Result := Dirty;
end;
procedure TInternActionList.SetDirty(d:Boolean);
begin
   Dirty := d;
end;

function TInternActionList.DoAdd(i:Integer;Item:TInternAction):integer;
var idx:integer;
begin
   idx := Add(Item);
   for i:= idx downto i+1 do begin
      Exchange(i,i-1);
   end;
end;

function TInternActionList.Add(Item:TInternAction):integer;
begin
   Result := inherited Add(Item);
   Item.WitchList := Self;
   SetDirty(True);
end;

procedure TInternActionList.SubTree(a:TInternAction; res : TList);
var ac : TInternAction;
    i  : integer;
    l  : TList;
begin
   if res <> nil then begin
      res.add(a);
      l := TList.Create;
      a.Depends(l);
      for i := 0 to l.count-1 do begin
         SubTree(l.Items[i],res);
      end;
      l.Free;
   end;
end;

{ ***** TSowing ***** }

constructor TSowing.Create;
begin
     inherited Create;
     noHarvActions := 0;
     noYears:=0;
end;
function TSowing.Specification : String;
begin
     Specification := what;
end;

function TSowing.print : string;
var s:string;
begin
   s := inherited print;
   s := s + ' "'+what+'"'+ ' "'+Model+'" '+IntToStr(NoHarvActions)+' '+IntToStr(NoYears);
   if when <> nil then
      s := s + ' ' + when.Print;
   print := s;
end;

function TSowing.params : string;
var s:string;
begin
   result := '(What "' + what + '")' + '(Model "' + Model + '") (No ' + IntToStr(NoHarvActions) + ') (Years ' + IntToStr(NoYears) + ')';
end;

function TSowing.paramsnew: daisy_alist;
var
   buf1:array[0..255] of char;
   buf2:array[0..255] of char;
begin
   result := _daisy_alist_create;
   _daisy_alist_set_string(result,StrPCopy(buf1,'What'),StrPCopy(buf2,what));
   _daisy_alist_set_string(result,StrPCopy(buf1,'Model'),StrPCopy(buf2,Model));
   _daisy_alist_set_integer(result,StrPCopy(buf1,'No'),NoHarvActions);
   _daisy_alist_set_integer(result,StrPCopy(buf1,'Years'),NoYears);
end;

procedure TSowing.HarvestDepends(l:TList);
var obj : TInternAction;
    i   : integer;
begin
   if l <> nil then begin
      with witchlist do begin
         for i:=0 to count-1 do begin
             obj := items[i];
             if (obj is THarvest) then with obj as THarvest do begin
                if CropSowed = Self then
                   l.Add(obj);
             end else if (self is TSowing) and (obj is TFuncHarvest) then with obj as TFuncHarvest do begin
                if CropSowed = Self then
                   l.Add(obj);
                end;
         end;
      end;
   end;
end;

function TSowing.GetHarvestAction:THarvest;
var i:Integer;
    o:TInternAction;
    l:TList;
    f:boolean;
begin
   l:=TList.Create;
   Depends(l);
   if l.Count > 0 then begin
      i:=0; f:=False;
      while (i<l.Count)and not f do begin
         o := l.items[i];
         if o is THarvest then with o as THarvest do begin
            f := CropSowed = Self;
         end;
         Inc(i);
      end;
      if f then
         Result := o as THarvest
      else
         Result := nil;
   end else
      Result := nil;
end;

{ ***** TFertilize ***** }
function TFertilize.Specification : String;
begin
     Specification := withWhat;
end;

function TFertilize.print : string;
var s:string;
begin
   s := inherited print;
   s := s + ' "'+withWhat+'" '+IntToStr(ammount);
   if when <> nil then
      s := s + ' ' + when.Print;
   print := s;
end;

function TFertilize.params : string;
var s:string;
begin
   result := '(WithWhat "'+withWhat+'") (Ammount '+IntToStr(ammount)+')';
end;

function TFertilize.paramsnew: daisy_alist;
var
   buf1:array[0..255] of char;
   buf2:array[0..255] of char;
begin
   result := _daisy_alist_create;
   _daisy_alist_set_string(result,StrPCopy(buf1,'WithWhat'),StrPCopy(buf2,withWhat));
   _daisy_alist_set_integer(result,StrPCopy(buf1,'Ammount'),ammount);
end;

{ ***** TTillage ***** }
function TTillage.Specification : String;
begin
     Specification := how;
end;

function TTillage.print : string;
var s:string;
begin
   s := inherited print;
   s := s + ' "'+how+'"';
   if when <> nil then
      s := s + ' ' + when.Print;
   print := s;
end;

function TTillage.params : string;
var s:string;
begin
   result := '(How "'+how+'")';
end;

function TTillage.paramsnew: daisy_alist;
var
   buf1:array[0..255] of char;
   buf2:array[0..255] of char;
begin
   result := _daisy_alist_create;
   _daisy_alist_set_string(result,StrPCopy(buf1,'How'),StrPCopy(buf2,how));
end;
{ ***** THarvest ***** }

constructor THarvest.Create;
begin
   inherited Create;
   SO := 100;
   Stem := 100;
   DM := 100;
   Leaf:= 100;
   Stub:= 10; {Might depend on crop/equipment}
end;

function THarvest.Specification : String;
begin
     if CropSowed <> nil then
        Specification := IntToStr(WitchList.GetObjIdx(CropSowed))+ ' '+ CropSowed.Specification
     else  { Should never occur }
        Specification := inherited Specification;
end;

function THarvest.print : string;
var s:string;
begin
   s := inherited print;
   if CropSowed <> nil then
      s := s + ' ' +  IntToStr(WitchList.GetObjIdx(CropSowed))
   else begin end; {MUST Never occur}
   s := s + ' ' + IntToStr(SO)+ ' ' + IntToStr(Stem)+ ' ' +
        IntToStr(DM)+ ' ' + IntToStr(Leaf)+ ' ' +
        IntToStr(Stub);
   if when <> nil then
      s := s + ' ' + when.Print;
   print := s;
end;

function THarvest.params : string;
var s:string;
begin
   s := '';
   if CropSowed <> nil then
      s := s + '(Id ' +  IntToStr(WitchList.GetObjIdx(CropSowed))+')'
   else begin end; {MUST Never occur}
   s := s + ' (SO ' + IntToStr(SO)+ ') (Stem ' + IntToStr(Stem)+ ') (DM ' +
        IntToStr(DM)+ ') (Leaf ' + IntToStr(Leaf)+ ') (Stub ' +
        IntToStr(Stub)+')';
   result := s;
end;

function THarvest.paramsnew: daisy_alist;
var
   buf1:array[0..255] of char;
   buf2:array[0..255] of char;
begin
   result := _daisy_alist_create;
   if CropSowed <> nil then
      _daisy_alist_set_integer(result,StrPCopy(buf1,'Id'),WitchList.GetObjIdx(CropSowed));
  _daisy_alist_set_integer(result,StrPCopy(buf1,'SO'),SO);
  _daisy_alist_set_integer(result,StrPCopy(buf1,'Stem'),Stem);
  _daisy_alist_set_integer(result,StrPCopy(buf1,'DM'),DM);
  _daisy_alist_set_integer(result,StrPCopy(buf1,'Leaf'),Leaf);
  _daisy_alist_set_integer(result,StrPCopy(buf1,'Stub'),Stub);
end;

constructor TFuncHarvest.Create;
begin
   inherited Create;
   SO := 100;
   Stem := 100;
   DM := 100;
   Leaf:= 100;
   Stub:= 10; {Might depend on crop/equipment}
end;

function TFuncHarvest.Specification : String;
begin
     if CropSowed <> nil then
        Specification := IntToStr(WitchList.GetObjIdx(CropSowed))+ ' '+ CropSowed.Specification
     else  { Should never occur }
        Specification := inherited Specification;
end;

function TFuncHarvest.print : string;
var s:string;
begin
   s := inherited print;
   if CropSowed <> nil then
      s := s + ' ' +  IntToStr(WitchList.GetObjIdx(CropSowed))
   else begin end; {MUST Never occur}
   s := s + ' ' + IntToStr(SO)+ ' ' + IntToStr(Stem)+ ' ' +
        IntToStr(DM)+ ' ' + IntToStr(Leaf)+ ' ' +
        IntToStr(Stub);
   if when <> nil then
      s := s + ' ' + when.Print;
   print := s;
end;

function TFuncHarvest.params : string;
var s:string;
begin
   s := '';
   if CropSowed <> nil then
      s := s + '(Id ' +  IntToStr(WitchList.GetObjIdx(CropSowed))+')'
   else begin end; {MUST Never occur}
   s := s + ' (SO ' + IntToStr(SO)+ ') (Stem ' + IntToStr(Stem)+ ') (DM ' +
        IntToStr(DM)+ ') (Leaf ' + IntToStr(Leaf)+ ') (Stub ' +
        IntToStr(Stub)+')';
   result := s;
end;

function TFuncHarvest.paramsnew: daisy_alist;
var
   buf1:array[0..255] of char;
   buf2:array[0..255] of char;
begin
   result := _daisy_alist_create;
   if CropSowed <> nil then
      _daisy_alist_set_integer(result,StrPCopy(buf1,'Id'),WitchList.GetObjIdx(CropSowed));
  _daisy_alist_set_integer(result,StrPCopy(buf1,'SO'),SO);
  _daisy_alist_set_integer(result,StrPCopy(buf1,'Stem'),Stem);
  _daisy_alist_set_integer(result,StrPCopy(buf1,'DM'),DM);
  _daisy_alist_set_integer(result,StrPCopy(buf1,'Leaf'),Leaf);
  _daisy_alist_set_integer(result,StrPCopy(buf1,'Stub'),Stub);
end;

constructor TIrrigate.Create;
begin
   UseAir := True;
end;

function TIrrigate.print : string;
var s:string;
begin
   s := inherited print + ' ' + IntToStr(HowMutch)+ ' ' + IntToStr(Temp);
   if UseAir then
      s := s + ' True'
   else
      s := s + ' False';
   if OverheadIrr then
      s := s + ' True'
   else
      s := s + ' False';
   if when <> nil then
      s := s + ' ' + when.Print;
   print := s;
end;

function TIrrigate.params : string;
var s:string;
begin
   s := '';
   s := '(Temp '+ IntToStr(Temp) + ') (HowMuch ' + IntToStr(HowMutch)+
        ') (UseAir ';
   if UseAir then
      s := s + 'true'
   else
      s := s + 'false';
   s := s + ') (OverheadIrr ';
   if OverheadIrr then
      s := s + 'true'
   else
      s := s + 'false';
   result := s+')';
end;

function TIrrigate.paramsnew: daisy_alist;
var
   buf1:array[0..255] of char;
   buf2:array[0..255] of char;
begin
   result := _daisy_alist_create;

  _daisy_alist_set_integer(result,StrPCopy(buf1,'Temp'),Temp);
  _daisy_alist_set_integer(result,StrPCopy(buf1,'HowMuch'),HowMutch);
  if UseAir then
     _daisy_alist_set_flag(result,StrPCopy(buf1,'UseAir'),1)
  else
     _daisy_alist_set_flag(result,StrPCopy(buf1,'UseAir'),0);
   if OverheadIrr then
     _daisy_alist_set_flag(result,StrPCopy(buf1,'OverheadIrr'),1)
   else
     _daisy_alist_set_flag(result,StrPCopy(buf1,'OverheadIrr'),0);
end;

function TIrrigate.Specification : string;
begin
   Result := '';
   if HowMutch > 0 then
      Result := IntToStr(HowMutch) + ' mm';
   if not UseAir and (Temp > 0) then begin
      if Result = '' then
         Result := 'Temp. ' + IntToStr(Temp) + ' Deg. C.'
      else
         Result := Result + ', Temp. ' + IntToStr(Temp)+' Deg. C.';
   end;
end;

function TEnd.print : string;
begin
   if when <> nil then
      print := inherited print + ' ' + when.Print
   else
      print := inherited print;
end;
function TEnd.params : string;
begin
   result := '';
end;
function TEnd.paramsnew: daisy_alist;
var
   buf1:array[0..255] of char;
   buf2:array[0..255] of char;
begin
   result := _daisy_alist_create;
end;

function TStart.print : string;
begin
   if when <> nil then
      print := inherited print + ' ' + when.Print
   else
      print := inherited print;
end;
function TStart.params : string;
begin
   result := '';
end;
function TStart.paramsnew: daisy_alist;
var
   buf1:array[0..255] of char;
   buf2:array[0..255] of char;
begin
   result := _daisy_alist_create;
end;
end.
