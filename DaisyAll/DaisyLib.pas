unit DaisyLib;
{
*****************
* Unit DaisyLib *
*****************

Project    : Graphical User Interface for the Daisy simulation package

Purpose    : This is the unit for the library - that is - a default and
             any personal "preferences"

Author     : J&R Informatik I/S

Date(s)    : 9/7 - 1997 Created (JJ)
}

interface
uses Classes,SysUtils;
type
   TLibObject = class(TObject)
      OName : String;
      OFileName : String;
      Property Name:String read OName write OName;
      Property FileName:String read OFileName write OFileName;
   End;
   TCrop = class(TLibObject)
      Multi : boolean;
      Mature : real;
      Constructor Create;
   End;
   TTill = class(TLibObject)
      Constructor Create;
   End;
   TFertilizer = class(TLibObject)
      kgN : real;
      Constructor Create;
   End;
   TModel = class(TLibObject)
      Constructor Create;
   End;
   TSoil = class(TLibObject)
      Constructor Create;
   End;

   TLibObjectList = Class(TList)
      Function NameList:TStringList;
      Function GetLibObject(Name:String):TLibObject;
      Procedure Reload(str,stt,ur,ut : TFileName;ostr,ostt,our,out : TFileName);virtual;
      Procedure DoClear(fn:String);
   End;

   TCropList = Class(TLibObjectList)
      Procedure Reload(str,stt,ur,ut : TFileName;ostr,ostt,our,out : TFileName);virtual;
   End;
   TDaisyLib = class(TObject)
   Private
      Oldstr,Oldstt,Oldur,Oldut : TFileName;
      FertilizerList, ModelList, SoilList,TillList : TLibObjectList;
      CropList : TCropList;
      function PTillageList:boolean;
      function PIrrigationModels:boolean;
      function PFertilizerList:boolean;
      function PParseLibFile(fn:String):boolean;


   Public
      Constructor Create;
      Destructor Free;
      Function GetCropList:TStringList;
      Function GetCrop(CropName : String) : TCrop;
      Function GetTillList:TStringList;
      Function GetTill(TillName : String) : TTill;
      Function GetFertilizerList:TStringList;
      Function GetFertilizer(FertilizerName : String) : TFertilizer;
      Function GetModelList:TStringList;
      Function GetModel(ModelName : String) : TModel;
      Function GetSoilList:TStringList;
      Function GetSoil(SoilName : String) : TSoil;
      Function LoadFromFile(FileName:String):Boolean;
      Function SaveToFile(FileName:String):Boolean;
      Procedure Reload(str,stt,ur,ut : TFileName);virtual;
   End;
implementation
Uses
   UseLib;

Procedure TCropList.Reload(str,stt,ur,ut : TFileName;
                           ostr,ostt,our,out : TFileName);
var ObjectList : TStringList;
    LibName    : String;
    LibFileName: String;
    OldLibFileName: String;
    CP : TCrop;
    procedure DoTheLoad;
    var
       i : Integer;
    begin
       ObjectList.Clear;
       EnumerateObjects(ObjectList,LibName,LibFileName);
       for i:= 0 to ObjectList.Count-1 do begin
          CP := TCrop.Create;
          CP.Mature := 0.0; (* This value should be "gotten" from a call to GetLibraryObject*)
          CP.Multi := False; (* This value should be "gotten" from a call to GetLibraryObject*)
          CP.Name := ObjectList.Strings[i];
          CP.FileName := LibFileName;
          Add(CP);
       end;
    end;
begin
   ObjectList := TStringList.Create;
   LibName := 'crop';
   LibFileName := str;(* Load from StdRefLib*)
   OldLibFileName := ostr;
   if LibFileName <> '' then begin
      if OldLibFileName <> '' then
         DoClear(OldLibFileName);
      DoTheLoad;
   end;
   LibFileName := stt;(* Load from StdTemplLib*)
   OldLibFileName := ostt;
   if LibFileName <> '' then begin
      if OldLibFileName <> '' then
         DoClear(OldLibFileName);
      DoTheLoad;
   end;
   LibFileName := ur;(* Load from UsrRefLib*)
   OldLibFileName := our;
   if LibFileName <> '' then begin
      if OldLibFileName <> '' then
         DoClear(OldLibFileName);
      DoTheLoad;
   end;
   LibFileName := ut; (* Load from UsrTemplLib*)
   OldLibFileName := out;
   if LibFileName <> '' then begin
      if OldLibFileName <> '' then
         DoClear(OldLibFileName);
      DoTheLoad;
   end;
   ObjectList.Free;
end;

Procedure TLibObjectList.Reload(str,stt,ur,ut : TFileName;ostr,ostt,our,out : TFileName);
begin
end;

Function TLibObjectList.NameList:TStringList;
Var i:Integer;
    o:TLibObject;
Begin
   Result := TStringList.Create;
   for i := 0 to count - 1 do begin
      o:= Items[i];
      Result.Add(o.Name);
   end;
End;

Function TLibObjectList.GetLibObject(Name:String):TLibObject;
Var i:Integer;
    Found :Boolean;
Begin
   If Count > 0 then begin
      Found := False; i := 0;
      while (Not Found) and (i < Count) do begin
         Result := Items[i];
         Found := Name = Result.Name;
         Inc(i);
      end;
      if Not Found then
         Result := nil;
   end else
      Result := nil;
End;

Procedure TLibObjectList.DoClear(fn:String);
var Obj : TLibObject;
    i   : Integer;
    tl  : TList;
begin
   if fn = '' then begin (* Clear everything *)
      for i := 0 to Count - 1 do begin
         Obj := Items[i];
         Obj.Free;
      end;
      Clear;
   end else begin(* Clear only those from fn *)
      tl := TList.Create;
      for i := 0 to Count - 1 do begin
         Obj := Items[i];
         if Obj.FileName = fn then
            tl.add(Obj);
      end;
      for i:=0 to tl.Count-1 do begin
         Obj := tl.Items[i];
         Delete(IndexOf(Obj));
         Obj.Free;
      end;
      Pack;
      tl.Free;
   end;
end;


Constructor TDaisyLib.Create;
Begin
   Inherited Create;
   Oldstr:='';
   Oldstt:='';
   Oldur:='';
   Oldut:='';
   CropList := TCropList.Create;
   SoilList := TLibObjectList.Create;
   FertilizerList := TLibObjectList.Create;
   ModelList := TLibObjectList.Create;
   TillList := TLibObjectList.Create;
End;

Destructor TDaisyLib.Free;
Begin
   CropList.DoClear(''); CropList.Free;
   SoilList.Free;
   FertilizerList.Free;
   ModelList.Free;
   TillList.Free;
End;

Function TDaisyLib.LoadFromFile(FileName:String):Boolean;
Begin
   Result := PParseLibFile(FileName);
End;
Function TDaisyLib.SaveToFile(FileName:String):Boolean;
Begin
End;
Function TDaisyLib.GetCropList:TStringList;
Begin
   Result := CropList.NameList;
End;
Function TDaisyLib.GetCrop(CropName : String) : TCrop;
Begin
   Result := CropList.GetLibObject(CropName) as TCrop;
End;

Function TDaisyLib.GetTillList:TStringList;
Begin
   Result := TillList.NameList;
End;
Function TDaisyLib.GetTill(TillName : String) : TTill;
Begin
   Result := TillList.GetLibObject(TillName) as TTill;
End;

Function TDaisyLib.GetFertilizerList:TStringList;
Begin
   Result := FertilizerList.NameList;
End;
Function TDaisyLib.GetFertilizer(FertilizerName : String) : TFertilizer;
Begin
   Result := FertilizerList.GetLibObject(FertilizerName) as TFertilizer;
End;
Function TDaisyLib.GetModelList:TStringList;
Begin
   Result := ModelList.NameList;
End;
Function TDaisyLib.GetModel(ModelName : String) : TModel;
Begin
   Result := ModelList.GetLibObject(ModelName) as TModel;
End;
Function TDaisyLib.GetSoilList:TStringList;
Begin
   Result := SoilList.NameList;
End;
Function TDaisyLib.GetSoil(SoilName : String) : TSoil;
Begin
   Result := SoilList.GetLibObject(SoilName) as TSoil;
End;

Procedure TDaisyLib.Reload(str,stt,ur,ut : TFileName);
begin
   CropList.Reload(str,stt,ur,ut,Oldstr,Oldstt,Oldur,Oldut);
   if str <> '' then Oldstr := str;
   if stt <> '' then Oldstt := stt;
   if ur <> '' then Oldur := ur;
   if ut <> '' then Oldut := ut;
end;


Constructor TTill.Create;
Begin
   Inherited Create;
End;

Constructor TCrop.Create;
Begin
   Inherited Create;
End;
Constructor TModel.Create;
Begin
   Inherited Create;
End;
Constructor TSoil.Create;
Begin
   Inherited Create;
End;
Constructor TFertilizer.Create;
Begin
   Inherited Create;
End;




function TDaisyLib.PTillageList:boolean;
var
    t : TTill;
begin
   Result := True;
   t := TTill.Create;
   t.Name := 'Disk Harrowing';
   TillList.Add(t);
   t := TTill.Create;
   t.Name := 'Rotavation';
   TillList.Add(t);
   t := TTill.Create;
   t.Name := 'Plowing';
   TillList.Add(t);
   t := TTill.Create;
   t.Name := 'Stubble Cultivation';
   TillList.Add(t);
   t := TTill.Create;
   t.Name := 'Seed Bed Preparation';
   TillList.Add(t);
end;


function TDaisyLib.PIrrigationModels:boolean;
var
    m    : TModel;

begin
   Result := true;
      m := TModel.Create;
      m.Name := 'Soil water deficit';
      ModelList.Add(m);

      m := TModel.Create;
      m.Name := 'Soil water Tension';
      ModelList.Add(m);

      m := TModel.Create;
      m.Name := 'Percipitation deficit';
      ModelList.Add(m);
end;

function TDaisyLib.PFertilizerList:boolean;
var FP : TFertilizer;
begin
   result := true;

      FP := TFertilizer.Create;
      FP.kgN := 80.0;
      FP.Name := 'mineral';
      FertilizerList.Add(FP);
(*
      FP := TFertilizer.Create;
      FP.kgN := 80.0;
      FP.Name := 'pig_slurry';
      FertilizerList.Add(FP);

      FP := TFertilizer.Create;
      FP.kgN := 80.0;
      FP.Name := 'cattle_slurry';
      FertilizerList.Add(FP);
*)
      FP := TFertilizer.Create;
      FP.kgN := 250.0;
      FP.Name := 'organic';
      FertilizerList.Add(FP);

end;

function TDaisyLib.PParseLibFile(fn:String):boolean;
begin
   Result := PTillageList and PIrrigationModels and PFertilizerList;
end;
end.


