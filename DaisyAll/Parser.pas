unit Parser;
{
***************
* Unit Parser *
***************

Project    : Graphical User Interface for the Daisy simulation package

Purpose    : Parsing the actionlist

Author     : J&R Informatik I/S (Jens Jeppesen)

Date(s)    : 6/3 - 97 Created
             19/3 - 97 Grammar
             20/4 - 97 Parsing the actionlist
             28/4 - 97 Minor chages due to changes in Lex
             26/8 - 97 Changes due to change in Grammar
             17/2 - 98 Changed to use Alist's, syntax'es and file_parser's from UNIT PasDaisy.pas

}

interface
uses Classes,PasDaisy,Dialogs;

function GetManagementStartTime(KomponentText: String;var bIsOurs:boolean): daisy_time;

function CreateActionListFromAlist(a:daisy_alist;ResList:TList):Boolean;

implementation
uses globals,IntAct,SysUtils,UseLib;
type
   TA = class(TObject)
      {Timetype :
        0: Abs,
        1: Ca,
        2: RelToAction
        3: DevelopmentState
        4: No time (OK not to specify)
      }
      public
      timetype,
      ACid,
      days,
      no:
      integer;
      DevSta:Real;
      procedure AddTime(a:daisy_alist);

   end;
   TS = class(TA)
      Sow : TSowing;
      Constructor Create;
      Constructor CreateFromAlist(a:daisy_alist);
   end;
   TI = class(TA)
      Irr : TIrrigate;
      Constructor Create;
      Constructor CreateFromAlist(a:daisy_alist);
   end;
   TF = class(TA)
      Fer : TFertilize;
      Constructor Create;
      Constructor CreateFromAlist(a:daisy_alist);
   end;
   TH = class(TA)
      Har : THarvest;
      SowId : Integer;
      Constructor Create;
      Constructor CreateFromAlist(a:daisy_alist);

   end;
   TFH = class(TA)
      FHar : TFuncHarvest;
      SowId : Integer;
      Constructor Create;
      Constructor CreateFromAlist(a:daisy_alist);
   end;
   TT = class(TA)
      Til : TTillage;
      Constructor Create;
      Constructor CreateFromAlist(a:daisy_alist);
   end;
   TE = class(TA)
      E : TEnd;
      Constructor Create;
      Constructor CreateFromAlist(a:daisy_alist);
   end;
   TSS = class(TA)
      S : TStart;
      Constructor Create;
      Constructor CreateFromAlist(a:daisy_alist);
   end;

var _lookahead : integer;
   curA : TA;
   Al : TList;
   y,m,d:integer;
   datostr : string;
   StrBuf1, StrBuf2 : Array[0..255] of char;

(* *)
function GetManagementStartTime(KomponentText: String;var bIsOurs:boolean): daisy_time;
var Manager,
    action    : daisy_alist;
    AlistSize : Integer;
    ActionType: String;
    StartTime : daisy_time;
begin
   result := nil;
   bIsOurs := false;
   Manager := GetLibraryObject('action',KomponentText);
   if Manager = nil then begin
      exit;
   end;
   bIsOurs := IsOurManager(Manager);
   if not bIsOurs then begin
      exit;
   end;

   AlistSize := _daisy_alist_size_alist(Manager,StrPCopy(StrBuf1,'Action'));
   if AlistSize = 0 then begin
      exit;
   end;
   if not GetVariableAt(Manager,'Action',0,daisy_type_alist,@action) then begin
      exit;
   end;
   if (not GetVariable(action,'Type',daisy_type_string, @ActionType)) or
      (ActionType <> 'Start') then begin
      exit;
   end;
   if not GetVariable(action, 'Time',daisy_type_alist, @action) then begin
      exit;
   end;
   if (not GetVariable(action,'Type',daisy_type_string, @ActionType)) or
      (ActionType <> 'Abs') then begin
      exit;
   end;
   if not GetVariable(action, 'Dato',daisy_type_time, @StartTime) then begin
      exit;
   end;
   result := StartTime;
end;

procedure SetAcDatoNew(a:TA;dato:String);
begin
   if a is TT then begin
      with a as TT do
         Til.SetDato(dato);
   end else if a is TS then begin
      with a as TS do
         Sow.SetDato(dato);
   end else if a is TF then begin
      with a as TF do
         Fer.SetDato(dato);
   end else if a is TI then begin
      with a as TI do
         Irr.SetDato(dato);
   end else if a is TSS then begin
      with a as TSS do
         S.SetDato(dato);
   end else if a is TE then begin
      with a as TE do
         E.SetDato(dato);
   end else if a is TH then begin
      with a as TH do
         Har.SetDato(dato);
   end;
end;
{
Set a ca. date in action. This is possible because no "pointers" are nedded
}
procedure SetAcCaDatoNew(a:TA;dato:String);
begin
   if a is TT then begin
      with a as TT do
         Til.SetCaDato(dato);
   end else if a is TS then begin
      with a as TS do
         Sow.SetCaDato(dato);
   end else if a is TF then begin
      with a as TF do
         Fer.SetCaDato(dato);
   end else if a is TI then begin
      with a as TI do
         Irr.SetCaDato(dato);
   end else if a is TE then begin
      with a as TE do
         E.SetCaDato(dato);
   end else if a is TSS then begin
      with a as TSS do
         S.SetCaDato(dato);
   end else if a is TH then begin
      with a as TH do
         Har.SetCaDato(dato);
   end;
end;
(* *)



procedure TA.AddTime(a:daisy_alist);
var
   TimeType_Str : String;
   DatoStr      : String;
   DaisyTime    : daisy_time;
begin
   if a <> nil then begin
      if GetVariable(a,'Type',daisy_type_string,@TimeType_Str) then begin
      (*if (_daisy_alist_check(a,StrPCopy(StrBuf1,'Type')) <> 0) then begin
         TimeType_Str := StrPas(_daisy_alist_get_string(a,StrPCopy(StrBuf1,'Type')));*)
         if          TimeType_Str = 'Abs' then begin
            (* *)
            TimeType  := 0;
            DaisyTime := _daisy_alist_get_time(a,StrPCopy(StrBuf1,'Dato'));
            DatoStr   := IntToStr(_daisy_time_get_year(DaisyTime)) + '-' +
                         IntToStr(_daisy_time_get_month(DaisyTime)) + '-' +
                         IntToStr(_daisy_time_get_mday(DaisyTime));
            SetAcDatoNew(self,DatoStr);
         end else if TimeType_Str = 'Ca' then begin
            (* *)
            TimeType := 1;
            DaisyTime := _daisy_alist_get_time(a,StrPCopy(StrBuf1,'Dato'));
            DatoStr   := IntToStr(_daisy_time_get_year(DaisyTime)) + '-' +
                         IntToStr(_daisy_time_get_month(DaisyTime)) + '-' +
                         IntToStr(_daisy_time_get_mday(DaisyTime));
            SetAcCaDatoNew(self,DatoStr);
         end else if TimeType_Str = 'Rel' then begin
            TimeType := 2;
            (* Just save Time parameters *)
            ACid := _daisy_alist_get_integer(a,StrPCopy(StrBuf1,'Id'));
            days := _daisy_alist_get_integer(a,StrPCopy(StrBuf1,'Days'));
         end else if TimeType_Str = 'Sow' then begin
            TimeType := 3;
            (* Just save Time parameters *)
            ACid   := _daisy_alist_get_integer(a,StrPCopy(StrBuf1,'Id'));
            no     := _daisy_alist_get_integer(a,StrPCopy(StrBuf1,'NoInYear'));
            days   := _daisy_alist_get_integer(a,StrPCopy(StrBuf1,'Year'));
            DevSta := _daisy_alist_get_number(a,StrPCopy(StrBuf1,'DevelopmentStage'));
         end else
            TimeType := 4;
      end else
         timetype := 4;
   end else
      timetype := 4;
end;

Constructor TT.CreateFromAlist(a:daisy_alist);
begin
   inherited Create;
   Til := TTillage.Create;
   with Til do begin
      how := StrPas(_daisy_alist_get_string(a,StrPCopy(StrBuf1,'How')));
   end;
end;



Constructor TH.CreateFromAlist(a:daisy_alist);
begin
   inherited Create;
   Har := THarvest.Create;
   with Har do begin
      SO   := _daisy_alist_get_integer(a,StrPCopy(StrBuf1,'SO'));
      Stem := _daisy_alist_get_integer(a,StrPCopy(StrBuf1,'Stem'));
      DM   := _daisy_alist_get_integer(a,StrPCopy(StrBuf1,'DM'));
      Leaf := _daisy_alist_get_integer(a,StrPCopy(StrBuf1,'Leaf'));
      Stub := _daisy_alist_get_integer(a,StrPCopy(StrBuf1,'Stub'));
   end;
   SowId := _daisy_alist_get_integer(a,StrPCopy(StrBuf1,'Id'));
end;


Constructor TFH.CreateFromAlist(a:daisy_alist);
begin
   inherited Create;
   FHar := TFuncHarvest.Create;
   with FHar do begin
      SO   := _daisy_alist_get_integer(a,StrPCopy(StrBuf1,'SO'));
      Stem := _daisy_alist_get_integer(a,StrPCopy(StrBuf1,'Stem'));
      DM   := _daisy_alist_get_integer(a,StrPCopy(StrBuf1,'DM'));
      Leaf := _daisy_alist_get_integer(a,StrPCopy(StrBuf1,'Leaf'));
      Stub := _daisy_alist_get_integer(a,StrPCopy(StrBuf1,'Stub'));
   end;
   SowId := _daisy_alist_get_integer(a,StrPCopy(StrBuf1,'Id'));
end;



Constructor TF.CreateFromAlist(a:daisy_alist);
begin
   inherited Create;
   Fer := TFertilize.Create;
   with Fer do begin
      ammount  := _daisy_alist_get_integer(a,StrPCopy(StrBuf1,'Ammount'));
      withWhat := StrPas(_daisy_alist_get_string(a,StrPCopy(StrBuf1,'WithWhat')));
   end;
end;


Constructor TI.CreateFromAlist(a:daisy_alist);
begin
   inherited Create;
   Irr := TIrrigate.Create;
   with Irr do begin
      HowMutch    := _daisy_alist_get_integer(a,StrPCopy(StrBuf1,'HowMuch'));
      Temp        := _daisy_alist_get_integer(a,StrPCopy(StrBuf1,'Temp'));
      UseAir      := _daisy_alist_get_flag(a,StrPCopy(StrBuf1,'UseAir')) <> 0;
      OverheadIrr := _daisy_alist_get_flag(a,StrPCopy(StrBuf1,'OverheadIrr')) <> 0;
   end;
end;

Constructor TS.CreateFromAlist(a:daisy_alist);
begin
   inherited Create;
   Sow := TSowing.Create;
   with Sow do begin
      noHarvActions := _daisy_alist_get_integer(a,StrPCopy(StrBuf1,'No'));
      noYears       := _daisy_alist_get_integer(a,StrPCopy(StrBuf1,'Years'));
      what          := StrPas(_daisy_alist_get_string(a,StrPCopy(StrBuf1,'What')));
      model         := StrPas(_daisy_alist_get_string(a,StrPCopy(StrBuf1,'Model')));
   end;
end;

Constructor TSS.CreateFromAlist(a:daisy_alist);
begin
   inherited Create;
   S := TStart.Create;
end;


Constructor TE.CreateFromAlist(a:daisy_alist);
begin
   inherited Create;
   E := TEnd.Create;
end;



function create_action(action:daisy_alist) : TA;
var
   ActionID   : Integer;
   ActionType : String;
   Params     : daisy_alist;
   Time       : daisy_alist;
begin
   if (_daisy_alist_check(action,StrPCopy(StrBuf1,'Id')) <> 0) and
      (_daisy_alist_check(action,StrPCopy(StrBuf1,'Type')) <> 0) then begin

      ActionID   := _daisy_alist_get_integer(action,StrPCopy(StrBuf1,'Id'));
      ActionType := StrPas(_daisy_alist_get_string(action,StrPCopy(StrBuf1,'Type')));
      if (_daisy_alist_check(action,StrPCopy(StrBuf1,'Params')) <> 0) then
         Params     := _daisy_alist_get_alist(action,StrPCopy(StrBuf1,'Params'))
      else
         params := nil;
      if          ActionType = 'Start'       then begin
         (* *)
         result := TSS.CreateFromAlist(Params);
      end else if ActionType = 'End'         then begin
         (* *)
         result := TE.CreateFromAlist(Params);
      end else if ActionType = 'Sowing'      then begin
         if params <> nil then
            result := TS.CreateFromAlist(Params)
         else
            result := nil;
      end else if ActionType = 'Harvest'     then begin
         if params <> nil then
            result := TH.CreateFromAlist(Params)
         else
            result := nil;
      end else if ActionType = 'FuncHarvest' then begin
         if params <> nil then
            result := TFH.CreateFromAlist(Params)
         else
            result := nil;
      end else if ActionType = 'Tillage'     then begin
         if params <> nil then
            result := TT.CreateFromAlist(Params)
         else
            result := nil;
      end else if ActionType = 'Irrigate'    then begin
         if params <> nil then
            result := TI.CreateFromAlist(Params)
         else
            result := nil;
      end else if ActionType = 'Fertilize'   then begin
         if params <> nil then
            result := TF.CreateFromAlist(Params)
         else
            result := nil;
      end else
         result := nil;
      if result <> nil then begin
         if _daisy_alist_check(action,StrPCopy(StrBuf1,'Time')) <> 0 then begin
            Time := _daisy_alist_get_alist(action,StrPCopy(StrBuf1,'Time'));
            result.AddTime(Time);
         end else
            result.TimeType := 4;
      end;
   end else
      result := nil;
end;

function CreateActionList(a:daisy_alist):TList;
var
   TempResult : TList;
   AlistSize,i: Cardinal;
   action : daisy_alist;
   curA   : TA;
   j      : Integer;

begin
   TempResult := TList.Create;
   if 0 = _daisy_alist_check(a,StrPCopy(StrBuf1,'Action')) then begin
      TempResult.Free;
      // ShowMessage('Shit');
      result := nil;
      exit;
   end;

   AlistSize := _daisy_alist_size_alist(a,StrPCopy(StrBuf1,'Action'));
   i := 0;
   while i < AlistSize do begin
      (* action := _daisy_alist_get_alist_at(a,StrPCopy(StrBuf1,'Action'),i); *)
      GetVariableAt(a,'Action',i,daisy_type_alist,@action);
      curA := create_action(action);
      if curA <> nil then
         TempResult.Add(curA)
      else begin
         Result := nil;
         for j := 0 to TempResult.Count - 1 do begin
            curA := TempResult.Items[j];
            curA.Free;
         end;
         TempResult.Free;
         exit;
      end;
      i := i + 1;
   end;
   Result := TempResult;
end;

function BuildActionList(A:TList;AL:TInternActionList;r:Boolean):Boolean;
var i,j:integer;
    res : boolean;
    tmpS : TA;
    obj : TInternAction;
    depList : TList;
    t1,t2 : Pointer;
   function CheckForMultibleHarvest(Sow:TSowing):Boolean;
   var i,j,k:Integer;
       Obj:TInternAction;
   Begin
      with Sow.WitchList do begin
         j:=0;k:=0;
         for i:= 0 to Count-1 do begin
            obj := Items[i];
            if obj is THarvest then with obj as THarvest do begin
               if CropSowed = Sow then
                  Inc(j);
            end else if obj is TFuncHarvest then with obj as TFuncHarvest do begin
               if CropSowed = Sow then
                  Inc(k);
            end;
         end;
      end;
      Result := (j=0)or((j=1)and(k=0));
   End;
begin
   res := r;
   AL.ClearList;
   if res then begin
      {
      Add the actions to InternalAcitonList
      Do not yet add any dependecies except for Sow <- Harvest dependencies
      Idea:
           Check for Tillage specification in lib file
           -        Sowing  -
           -        Irrigate -
           -        Fertilize -

      }
      i := 0; res := True;
      while (i < A.Count) and res do begin
         curA := A.Items[i];
         if curA is TT then begin
            with curA as TT do
               AL.Add(Til);
         end else if curA is TSS then begin
            with curA as TSS do
               AL.Add(S);
         end else if curA is TE then begin
            with curA as TE do
               AL.Add(E);
         end else if curA is TS then begin
            with curA as TS do
               AL.Add(Sow);
         end else if curA is TF then begin
            with curA as TF do
               AL.Add(Fer);
         end else if curA is TI then begin
            with curA as TI do
               AL.Add(Irr);
         end else if curA is TH then begin
            with curA as TH do begin
               AL.Add(Har);
               j := SowId;
               if ((SowId-1) <= A.Count-1) and ((SowId-1) > 0) then begin
                 tmpS := A.Items[SowId-1];
                 if tmpS is TS then begin
                    with tmpS as TS do begin
                       Har.CropSowed := Sow;
                       res := CheckForMultibleHarvest(Sow);
                    end;
                 end else
                    res := false;
               end else
                  res := False;
            end;
         end else if curA is TFH then begin
            with curA as TFH do begin
               AL.Add(FHar);
               j := SowId;
               if ((SowId-1) <= A.Count-1) and ((SowId-1) > 0) then begin
                 tmpS := A.Items[SowId-1];
                 if tmpS is TS then begin
                    with tmpS as TS do begin
                       FHar.CropSowed := Sow;
                    end;
                 end else
                    res := false;
               end else
                  res := False;
            end;
         end;
      Inc(i);
      end;
   end;
   {
   If building the "basic" list was OK now add any dependencies
   }
   if res then begin
      i := 0;
      while (i < A.Count) and res do begin
         curA := A.Items[i];
         case curA.TimeType of
            0,1,4:
               begin
               end;
            2:
               begin
                  if curA is TS then with curA as TS do begin
                     Res := curA.ACid-1 < Al.Count;
                     if Res then
                        Sow.RelateToAction(Al[curA.ACid-1],curA.days);
                  end else if curA is TI then with curA as TI do begin
                     Res := curA.ACid-1 < Al.Count;
                     if res then
                        Irr.RelateToAction(Al[curA.ACid-1],curA.days);
                  end else if curA is TF then with curA as TF do begin
                     Res := curA.ACid-1 < Al.Count;
                     if res then
                        Fer.RelateToAction(Al[curA.ACid-1],curA.days);
                  end else if curA is TH then with curA as TH do begin
                     Res := curA.ACid-1 < Al.Count;
                     if res then
                        Har.RelateToAction(Al[curA.ACid-1],curA.days);
                  end else if curA is TT then with curA as TT do begin
                     Res := curA.ACid-1 < Al.Count;
                     if res then
                        Til.RelateToAction(Al[curA.ACid-1],curA.days);
                  end else if curA is TE then
                     res := False
                  else if curA is TSS then
                     res := False
                  else
                     res := False;
               end;
            3:
               begin
                  if curA is TFH then with curA as TFH do begin
                     Res := curA.ACid-1 < Al.Count;
                     if res then begin
                        Obj :=Al[curA.ACid-1];
                        Res := Res and (Obj is TSowing);
                        if Res then
                           FHar.RelateToDevelopmentState(Obj as TSowing,curA.DevSta,curA.no,curA.days);
                     end;
                  end else
                     res := False;
               end;
         end;
         Inc(i);
      end;
   end;
   if res then
      {
      Actions are not required to have a timespecification, but those who might have
      must not give cycles.
      }
      res := Al.TopSort(False);
   begin
      for i := 0 to A.Count-1 do begin
         curA := A.Items[i];
         {Free the actions if errors}
         if not res then begin
            if curA is TT then begin
               with curA as TT do
                  Til.Free;
            end else if curA is TS then begin
               with curA as TS do
                  Sow.Free;
            end else if curA is TF then begin
               with curA as TF do
                  Fer.Free;
            end else if curA is TI then begin
               with curA as TI do
                  Irr.Free;
            end else if curA is TH then begin
               with curA as TH do
                  Har.Free;
            end;
         end;
         curA.Free;
      end;
   end;
   if res then begin
      obj := Al.Items[0];
      res := res and (obj is TStart);
      obj := Al.Items[Al.Count-1];
      res := res and (obj is TEnd);
   end else
      Al.Clear;

   Result := res;
end;

function CreateActionListFromAlist(a:daisy_alist;ResList:TList):Boolean;
var
   Al : TList;
begin
   Al := CreateActionList(a);
   if al <> nil then
      Result := BuildActionList(Al,ResList as TInternActionList,TRUE)
   else
      Result := false;
end;



Constructor TSS.Create;
begin
   inherited Create;
   S := TStart.Create;
end;
Constructor TE.Create;
begin
   inherited Create;
   E := TEnd.Create;
end;
Constructor TS.Create;
begin
   inherited Create;
   Sow := TSowing.Create;
end;
Constructor TI.Create;
begin
   inherited Create;
   Irr := TIrrigate.Create;
end;
Constructor TF.Create;
begin
   inherited Create;
   Fer := TFertilize.Create;
end;
Constructor TFH.Create;
begin
   inherited Create;
   FHar := TFuncHarvest.Create;
end;
Constructor TH.Create;
begin
   inherited Create;
   Har := THarvest.Create;
end;
Constructor TT.Create;
begin
   inherited Create;
   Til := TTillage.Create;
end;


{
Set date in action. This is possible because no "pointers" are nedded
}
procedure SetAcDato;
begin
   if curA is TT then begin
      with curA as TT do
         Til.SetDato(datostr);
   end else if curA is TS then begin
      with curA as TS do
         Sow.SetDato(datostr);
   end else if curA is TF then begin
      with curA as TF do
         Fer.SetDato(datostr);
   end else if curA is TI then begin
      with curA as TI do
         Irr.SetDato(datostr);
   end else if curA is TSS then begin
      with curA as TSS do
         S.SetDato(datostr);
   end else if curA is TE then begin
      with curA as TE do
         E.SetDato(datostr);
   end else if curA is TH then begin
      with curA as TH do
         Har.SetDato(datostr);
   end;
end;
{
Set a ca. date in action. This is possible because no "pointers" are nedded
}
procedure SetAcCaDato;
begin
   if curA is TT then begin
      with curA as TT do
         Til.SetCaDato(datostr);
   end else if curA is TS then begin
      with curA as TS do
         Sow.SetCaDato(datostr);
   end else if curA is TF then begin
      with curA as TF do
         Fer.SetCaDato(datostr);
   end else if curA is TI then begin
      with curA as TI do
         Irr.SetCaDato(datostr);
   end else if curA is TE then begin
      with curA as TE do
         E.SetCaDato(datostr);
   end else if curA is TSS then begin
      with curA as TSS do
         S.SetCaDato(datostr);
   end else if curA is TH then begin
      with curA as TH do
         Har.SetCaDato(datostr);
   end;
end;

end.
