unit ManGlob;
{
***************
* Unit Globals *
***************

Project    : Graphical User Interface for the Daisy simulation package

Purpose    : Global variables shared by all units

Author     : J&R Informatik I/S (Rino Ranheim)

Date(s)    : 1/12 - 96 Created
             20/3 - 97 Revised to load from libary
}

interface

uses
   Classes, WinProcs, SysUtils, IntAct, DaisyLib,DsyTime;
var
   CurrProject: String;
   CurrAction: TInternAction;
   (*
   Lib : TCombinedDaisyLib;
   *)
   Lib : TDaisyLib;
   InsertBefore: Boolean;
   ResourceBuffer: PCHAR;
   InternalActionList : TInternActionList;

   procedure InitActions;
   procedure GlobalInit;
   procedure GlobalTerminate;
   function GetString(i: LongInt): String;
   procedure SetUpTimeEdit(TC:TDaisyTimeComp;Time:TNewDaisyTime);
   procedure EndTimeEdit(TC:TDaisyTimeComp;Action:TInternAction);
   procedure DoGetList(Action:TInternAction;List:TStrings);

implementation

function getstring(i: LongInt): String;
var
   CharsRead: Integer;
begin
   Result := '';
   CharsRead := LoadString(0 , i, ResourceBuffer, 255);
   if CharsRead > 0 then
      Result := StrPas(ResourceBuffer);
end;

procedure InitActions;
begin
end;

procedure GlobalInit;
begin
   InsertBefore := False; { Should be set in options menu }
{   InternalActionList := TInternActionList.Create;}
(*   Lib := TCombinedDaisyLib.Create; *)
   Lib := TDaisyLib.Create;
   GetMem(ResourceBuffer, 256);
end;

procedure GlobalTerminate;
begin
   {InternalActionList.Free;}
   Lib.Free;
   FreeMem(ResourceBuffer, 256);
end;

procedure SetUpTimeEdit(TC:TDaisyTimeComp;Time:TNewDaisyTime);
begin
   TC.ClearTime;
   if Time <> nil then begin
      if Time is TCaDate then begin
         with Time as TCaDate do
            TC.SetCaDate(Date(True))
      end else if Time is TDate then begin
         with Time as TDate do
            TC.SetDate(Date(True))
      end else if Time is TRelToAction then begin
         with Time as TRelToAction do begin
            {Setup := Action;}
            TC.SetRelTo(Displacement);
         end;
      end
   end;
end;

procedure EndTimeEdit(TC:TDaisyTimeComp;Action:TInternAction);
var TimeType,Days:Integer;
    Date_Action:String;
begin
   if TC.Changed then begin
      TC.GetTime(TimeType,Date_Action,Days);
   case TimeType of
      0,3:
         begin
            if not Action.SetDato(Date_Action) then begin
            end;
         end;
      1:
         begin
            if not Action.SetCaDato(Date_Action) then begin
            end;
         end;
      2:
         begin
            Action.UnRelate;
            if TC.TheIndex <> -1 then begin
               CurrAction.RelateToAction(TC.TheObject as TInternAction,Days);
            end;
         end;
   end;
   end;
end;

procedure DoGetList(Action:TInternAction;List:TStrings);
var l:TList;
    o:TInternAction;
    i:Integer;
begin
   l := TList.Create;
   List.Clear;
   InternalActionlist.GetListExSubTree(CurrAction,l);
   for i:= 0 to l.Count-1 do begin
       o := l.Items[i];
       List.Strings[i] := IntToStr(InternalActionlist.GetObjIdx(o))+' '+o.ActionName+' '+o.Specification;
       List.Objects[i] := o;
   end;
   l.Free;
end;
initialization
   GlobalInit;
finalization
   GlobalTerminate;
end.
