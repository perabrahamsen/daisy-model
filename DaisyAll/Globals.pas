unit Globals;


{ ************************************************ }
{ Global variables for use in Daisy User Interface }
{ is defined here.                                 }
{ ************************************************ }

interface

uses
   Windows, Messages, SysUtils, Classes, Graphics, Controls
   , Forms, Dialogs, Grids, PasDaisy, IniFiles, TypInfo, DsyTime;

{ Help map files start: }
{$i HelpDsy.inc}
{$i HelpGlob.inc}
{$i HelpMan.inc}
{$i HelpHor.inc}
{$i HelpProf.inc}
{$i HelpSim.inc}
{ Help map files end }

{ Include named constants for resources }
{$I DaisyTxt.inc}

const
   HELP_TAB = 15; { 0x000f }
   Daisy_GUI_Version = '1.0a';

var
   HelpEmptyKey: LongInt;
   HelpEmptyString: String;


type
   TFormGlobals = class(TForm)
   private
     { Private declarations }
   public
     { Public declarations }
   end;

   TMainFormVars = class;
   TProfileVars = class;
   TManagerVars = class;
   TGlobalInfoVars = class;
   THorizonVars = class;
   TStateVars = class;
   TDiscretizationVars = class;

   THorizonVars = class
      public
         HorizonComments: String; { Comments for the specified horizon }
         HorizonHeading1: String; { 2 letter horizon identifier }
         HorizonHeading2: String; { The rest of the horizon identifier }
         TextureOrganicFrac: Integer; { Fractions from 'Texture Composition' }
         TextureClayFrac: Integer;    {    group box.                        }
         TextureSiltFrac: Integer;    {    The values are measured in 1/1000 }
         TextureFineFrac: Integer;    {    and the sum of all five must      }
         TextureCoarseFrac: Integer;  {    therefore equal 1000.             }
         TextureOrganicFracSet: Integer;  { Variables to determine whether something }
         TextureClayFracSet: Integer;     {    is typed in in the editboxes for      }
         TextureSiltFracSet: Integer;     {    'texture composition'                 }
         TextureFineFracSet: Integer;     {    Values will be 0 if the box is empty, }
         TextureCoarseFracSet: Integer;   {    else 1, 2, 4, 8, 16 respectively.     }
         DryBulk: Double;            { Dry Bulk Density }
         HydraulicPropertiesIdx: Integer;       { ...and the selected index }
         SaturatedWaterContent: Double; { Parameters for hydraulic properties }
         ResidualWaterContent: Double;
         VanGenuchtenAlpha: Double;
         VanGenuchtenN: Double;
         SaturatedHydrCond: Double;
         BublingPressure: Double;
         PoreSizeDistIdx: Double;
         TableDaisyV1File: String;  { Filename for Hydraulic properties, Table - Daisy v.1 format }
         SoluteDiffusiveIdx: Integer;        { ...and the selected index }
         ParameterA: Double;  { Parameters for Solute Diffusive Transport }
         ParameterB: Double;
         ThermalPropertiesIdx: Integer;          { ...and the selected index }
         PassiveSlowly: Double; { Passive/Slowly Decomposable Fraction }
         ActiveEasily: Double;  { Active/Easily Decomposable Fraction }
         CNofPassive: Double;   { C/N of Passive/Slowly Decomposable }
         CNofActive: Double;    { C/N of Active/Easily Decomposable }

         Dirty: Boolean;  { Vars saved (FALSE) - or changed since last save (TRUE) }
         CurrentSaveLib     : Integer;  { -1: Not saved yet, 1: User Ref, 3: User Templ }

         constructor Create;    { Allocates neccesary space        }
         destructor Destroy;    { Releases the allocated space     }
         procedure Reset;       { Initializes vars to "not set"    }
         procedure LoadDefault; { Loads default values from mini-object }
         function Load(HorName: string; lib: Integer): Boolean; { Initializes horizon from LIBRARY }
         function Save(HorName: string; lib: Integer; OverWrt: Boolean): Boolean; { Saves horizon to LIBRARY         }
                                                                { lib = 0: User def templ lib      }
                                                                { lib = 1: Sim spec lib      }
                                                                { OverWrt: Overwrite in lib if object already exists }
         procedure Delete;     { Deletes horizon from LIBRARY     }
         procedure Assign(src: THorizonVars); { Assigns values from src to itself }

(*
         { Merges two horizon headers into a library horison name         }
         { Return 0: OK, 1: header1 missing, 2: header2 missing           }
         function MkHorizonName(Header1, Header2: String; var HorName: String): Integer;
*)
(*
         { Splits a library horizon name into two horizon headers         }
         { Return 0: OK, 1: horname not long enough for header1 + header2 }
         function SplitHorizonName(var Header1, Header2: String; HorName: String): Integer;
*)
   end;

   TProfileVars = class
      public
         ProfileComments: String; { Comments for the specified profile }
         ProfileHeading: String; { The profile specified }
         RGSelectIdx: Integer; { Selected Radiobutton for horizon library }
         SGProfileItems: TStringGrid; { Profile definition (Horizons in profile, size & which library they came from }
         NumSGProfileItems: Integer; { Number of items in SGProfileItems (because RowCount can't be 0 ##@$# }
         RootingDepth: Double; { Max allowed rooting depth }
         Dispersivity: Double; { Solute Convective Transport - Dispersivity }
         WaterFlowIdx: Integer;        { ...and the selected index             }
         DepthOfGroundWater: Double; { In case fixed groundwater table is specified }
         FluctuatingGroundwaterFile: String; { In case fluctuating groundwater table is specified (-> filename) }
         LysimeterDepth: Double;  { In case lysimeter depth is specified }
         AbsorptionIsotermIdx: Integer;        { ...and the selected index }
         DiscretizationViewed: Boolean;  { Is the discretization viewed by user since last change? }


         { Variables for subforms }
         DiscVars: TDiscretizationVars;
         {HorVars: THorizonVars;}
         StateVars: TStateVars;

         Dirty: Boolean;  { Vars saved (FALSE) - or changed since last save (TRUE) }
         CurrentSaveLib     : Integer;  { -1: Not saved yet, 1: User Ref, 3: User Templ }

         constructor Create;  { Allocates neccesary space        }
         destructor Destroy;  { Releases the allocated space     }
         procedure Reset;     { Initializes vars to "not set"    }
         function Load(ProfName: string; lib: Integer): Boolean; { Initializes vars from lib }
         function Save(ProfName: string; lib: Integer; OverWrt: Boolean): Boolean; { Saves vars to lib  }
                                                                 { lib = 0: User def templ lib }
                                                                 { lib = 1: Sim spec lib       }
                                                                 { OverWrt: Overwrite in lib if object already exists }
         procedure Assign(src: TProfileVars); { Assigns values from src to itself }
         function GetLowestHor: Double;  { Finds the depth of the deepest horizon in the profile (-1 if no hors in prof)}
   end;

   TDiscretizationVars = class
      public
         Discretization: TStringList; { The discretization }

         UnTouchables: TStringList;   { Listing of which items in the discret- }
                                      { ization that is caused by horizons in  }
                                      { profile, and which that is not!        }
         { ...for user defined discretizations. Untouchables are the depths defined     }
         { for the horizons in the profile. These depths can not be changed or deleted. }
         { '' means the depth on that index can be changed,  'U' means the depth cannot }
         { be changed (i.e. it is untouchable).                                         }
         { A stringlist is used in case it should be a column in the grid later...      }

         constructor Create;  { Allocates neccesary space     }
         destructor Destroy;  { Releases the allocated space  }
         procedure Reset(ProfVars: TProfileVars);     { Initializes vars from profile definition only }
         procedure Load;      { Initializes vars from file    }
         procedure Save;      { Saves vars to file            }
         procedure Assign(src: TDiscretizationVars); { Assigns values from src to itself }
         procedure Sort;      { Sorts the discretization }
         procedure Update(ProfVars: TProfileVars);    { Updates the discretization with new depths from profile }
                              { - Should only be called if IsValid = TRUE !!!           }
         procedure Apply(disctype: Integer);  { Applies a discretization of type disctype }
         // IsValid is NOT sufficient!!! Don't use it for adding depths,
         // while the untouchables can be out of sync if hors are reordered :-(
         function IsValid(lowest: double): Boolean;  { Checks if discretization depths is below lowest }
         function IsHorsInDisc(ProfVars: TProfileVars): Boolean; { checks the discretization in head & ass... }
   end;

   TStateVars = class
      public
         SoilWaterContIdx: Integer;         { ...and the selected index }
         PotentialVsDepth: TStringGrid;
         NumPotentialVsDepthItems: Integer;
         WaterContVsDepth: TStringGrid;
         NumWaterContVsDepthItems:Integer;
         SoilTemperatureIdx: Integer;         { ...and the selected index }
         TemperatureVsDepth: TStringGrid;
         NumTemperatureVsDepthItems:Integer;
         SoilNitrateContIdx: Integer;         { ...and the selected index }
         NitrateVsDepth: TStringGrid;
         NumNitrateVsDepthItems:Integer;
         NitrateConc: Double;
         SoilAmmoniaContIdx: Integer;         { ...and the selected index }
         AmmoniaVsDepth: TStringGrid;
         NumAmmoniaVsDepthItems:Integer;
         AmmoniaContent: Double;

         constructor Create;  { Allocates neccesary space     }
         destructor Destroy;  { Releases the allocated space  }
         procedure Reset;     { Initializes vars to "not set" }
         procedure Load;      { Initializes vars from file    }
         procedure Save;      { Saves vars to file            }
         procedure Assign(src: TStateVars); { Assigns values from src to itself }
   end;

   TGlobalInfoVars = class
      public
         GlobalInfoHeading: String; { Heading (name) for global info }
         Latitude: Double; { Latitude (+ : N.hemisphere, - : S.hemisphere) }
         Elevation: Double;  { Elevation }
         UTM1: Double; { UTM coordinates 1 }
         UTM2: Double; { UTM coordinates 2 }
         AvAnnTemp: Double;  { Average Annual Temperature }
         AvAnnTempAmp: Double;  { Average Annual Temperature Amplitude }
         DateMaxTemp: String;  { Date of Maximum Temperature - Must be blank or 4 long (mmdd)}
         NH4ConcPrec: Double;  { NH4 - Concentration in Precipitation }
         NH4DryDep: Double;  { NH4 - Dry deposition }
         NO3ConcPrec: Double;  { NO3 - Concentration in Precipitation }
         NO3DryDep: Double;  { NO3 - Dry deposition }
         MeteorologicalDataFile: String;  { Meteorological Data File }

         Dirty: Boolean;  { Vars saved (FALSE) - or changed since last save (TRUE) }
         CurrentSaveLib     : Integer;  { -1: Not saved yet, 1: User Ref, 3: User Templ }

         constructor Create;  { Allocates neccesary space     }
         destructor Destroy;  { Releases the allocated space  }
         procedure Reset;  { Initializes vars to "not set" }
         procedure LoadDefault; { Loads default values from mini-object }
         function Load(GlobInfoName: string; lib: Integer): Boolean; { Init vars from lib }
         function Save(GlobInfoName: string; lib: Integer; OverWrt: Boolean): Boolean; { Saves GlobInfo to LIBRARY   }
                                                                     { lib = 0: User def templ lib }
                                                                     { lib = 1: Sim spec lib       }
                                                                     { OverWrt: Overwrite in lib if object already exists }
         procedure Assign(src: TGlobalInfoVars); { Assigns values from src to itself }
   end;

   TMainFormVars = class
      public
         CurrSimulationFile: String; { Filename for current simulation }

         StdRefLib: String;  { Standard reference library }
         UserRefLib: String;  { User defined reference library }
         StdTemplLib: String;  { Standard template library }
         UserTemplLib: String;  { User defined template library }
         { Simulation specific library }
         (*
         The Simulation can have to filenames
         1) Internal name
         2) External name
         The internal name is the filename that's attached to
         all objects i simulation specific lib.
         The external name is the filename in wich all the objects
         in the simulation specific lib is saved.
         Sometimes these names are the same and sometimes they are
         different. And sometimes the external name is not event assigned
         any value, i.e. it's the empty string.
         *)
         SimSpecLib: String;
         ExternalSimSpecLib : String;

         StdRefLibManItems: TStringList;
         StdRefLibGlobItems: TStringList;
         StdRefLibProfItems: TStringList;
         StdRefLibHorItems: TStringList;
        (*JJ:301298*)
         StdRefLibOutputItems : TStringList;
        (*JJ:301298*)
         UserRefLibManItems: TStringList;
         UserRefLibGlobItems: TStringList;
         UserRefLibProfItems: TStringList;
         UserRefLibHorItems: TStringList;
         StdTemplLibManItems: TStringList;
         StdTemplLibGlobItems: TStringList;
         StdTemplLibProfItems: TStringList;
         StdTemplLibHorItems: TStringList;
         UserTemplLibManItems: TStringList;
         UserTemplLibGlobItems: TStringList;
         UserTemplLibProfItems: TStringList;
         UserTemplLibHorItems: TStringList;
         SimSpecLibManItems: TStringList;
         SimSpecLibGlobItems: TStringList;
         SimSpecLibProfItems: TStringList;
         SimSpecLibHorItems: TStringList;


         SimulationHeading: String;  { Simulation Heading (name) }
         InitializationIdx: Integer;        { ...and the selected index }
         RGSelLibIdx: Integer;  { Selected library type }
         RGModuleIdx: Integer;  { Selected module type  }
         SelManType: Integer;      { Selected manager type (stdref, userref ... }
         SelGlobType: Integer;     { Selected global info type (stdref, userref ... }
         SelProfType: Integer;     { Selected profile type (stdref, userref ... }
         SelMan: String;   { Name of selected Manager }
         SelGlob: String;  { Name of selected Global Information }
         SelProf: String;  { Name of selected Profile }
         (*JJ:301298*)
         SelOutput : TStringList;          {List of selected output}
         SimulationStartTime : daisy_time; {Starting time of simulation}
         bStartTimeIsOurs    : boolean;    {Created by minidaisy ?}
         (*JJ:301298*)

(*
         { Variables for subforms... }
         ProfVars: TProfileVars;
         GlobInfoVars: TGlobalInfoVars;
         ManVars: TManagerVars;
*)

         Dirty: Boolean;  { Vars saved (FALSE) - or changed since last save (TRUE) }
         constructor Create;  { Allocates neccesary space     }
         destructor Destroy;  { Releases the allocated space  }
         procedure Reset;     { Initializes vars to "not set" }
         procedure Load;      { Initializes vars from file    }
         procedure Save;      { Saves vars to file            }

         function CreateAlistFromVars : daisy_alist;
         function CreateVarsFromAlist(al:daisy_alist) : boolean;

         procedure Assign(src: TMainFormVars); { Assigns values from src to itself }
         procedure UpdateLib(lib, mdl: Integer); { Update a libvariabel from a daisydll library }
                                                 { lib = 0: Standard Reference }
                                                 { lib = 1: User Defined Reference }
                                                 { lib = 2: Standard Template }
                                                 { lib = 3: User Defined Template }
                                                 { lib = 4: Simulation Specific Reference }
                                                 { mdl = 0: Manager }
                                                 { mdl = 1: Globals }
                                                 { mdl = 2: Profile }
                                                 { mdl = 3: Horizon }
   end;

   TManagerVars = class
      public
         ManagerHeading: String; { Heading (name) for manager }
         TheAlist      : daisy_alist;
         TheAlist_CanDelete : Boolean;
         Dirty              : Boolean;
         CurrentSaveLib     : Integer;  { -1: Not saved yet, 1: User Ref, 3: User Templ }

         constructor Create;  { Allocates neccesary space     }
         destructor Destroy;  { Releases the allocated space  }
         procedure Reset;     { Initializes vars to "not set" }
         procedure Load;      { Initializes vars from file    }
         procedure Save;      { Saves vars to file            }
         procedure Assign(src: TManagerVars); { Assigns values from src to itself }
   end;

   function StrTo3DecimalStr(s: string): string;
   function StrTo1DecPosStr(s: string): string;
   function StrTo1DecPosNotZeroStr(s: string): string;
   function StrTo3DecPosStr(s: string): string;
   function StrTo3DecFracStr(s: string): string;
   function CharToUpper(ch: Char): Char;
   function StrToPosFloatStr(s: string): string;
   function StrToPosNotZeroFloatStr(s: string): string;
   function Month2Days(m: Integer): Integer;
   function mmdd2days(mm, dd: Integer): Integer;
   function Days2Month(d: Integer): Integer;
   function jdays2days(jd: Integer): Integer;
   function MkHorizonName(Header1, Header2: String; var HorName: String): Integer;
   function SplitHorizonName(var Header1, Header2: String; HorName: String): Integer;

   // Set the resources in an instance of the TDaisyTimeComp component
   procedure SetTimeComponentResources(MultiDateMode : Boolean;
                                       var Component : TDaisyTimeComp);

var
   { Variables to hold data for 'Daisy setup file' and 'projekt(simulation) file'}
   MainFormVars: TMainFormVars;
   { + temp vars in case of <cancel> pushed in some subform... }
   TempProfileVars: TProfileVars;
   TempGlobalInfoVars: TGlobalInfoVars;
   TempManagerVars: TManagerVars;
   { ...or in some subsub form... }
   Temp2DiscretizationVars: TDiscretizationVars;
   Temp2HorizonVars: THorizonVars;
   Temp2StateVars: TStateVars;

   FormGlobals: TFormGlobals;

   NewScreenWidth: LongInt; { For adjusting changes in resolution & font size }
   FracErrMsg: String;  { to be removed when resource file is finished... }
   DaisyHomeDir: String;  { Simulation home directory (as on startup) }

const
   NotSet = low(Integer);
   RestOfString = 9999;
   OldScreenWidth: LongInt = 800; { For adjusting changes in resolution & font size }

type
   ENotValidFraction = class (Exception);
   ENotValidFloat = class (Exception);




implementation

uses UseLib,Parser;
{$R *.DFM}



{ *********************************** }
{           THorizonVars              }
{ *********************************** }

constructor THorizonVars.Create;
begin
{   HorizonComments := TStringList.Create;}
{   HydraulicPropertiesItems := TStringList.Create;
   SoluteDiffusiveItems := TStringList.Create;
   ThermalPropertiesItems := TStringList.Create;}
   Dirty := False;
   CurrentSaveLib := -1;  { -1: Not saved yet, 1: User Ref, 3: User Templ }
end;

destructor THorizonVars.Destroy;
begin
{   HorizonComments.Free;}
{   HydraulicPropertiesItems.Free;
   SoluteDiffusiveItems.Free;
  ThermalPropertiesItems.Free;}
end;

procedure THorizonVars.Reset;
begin
{ Reset all horizon variables... }
   HorizonComments := '';
(*
   HorizonHeading1 := '';
   HorizonHeading2 := '';
*)
   TextureOrganicFrac := NotSet;  { If one is set - all must be set and the sum must equal 1000 }
   TextureClayFrac := NotSet;
   TextureSiltFrac := NotSet;
   TextureFineFrac := NotSet;
   TextureCoarseFrac := NotSet;
   TextureOrganicFracSet := 0;
   TextureClayFracSet := 0;
   TextureSiltFracSet := 0;
   TextureFineFracSet := 0;
   TextureCoarseFracSet := 0;
   DryBulk := NotSet;
   HydraulicPropertiesIdx := -1;
   SaturatedWaterContent := NotSet;
   ResidualWaterContent := NotSet;
   VanGenuchtenAlpha := NotSet;
   VanGenuchtenN := NotSet;
   SaturatedHydrCond := NotSet;
   BublingPressure := NotSet;
   PoreSizeDistIdx := -1;
   TableDaisyV1File := '';
   SoluteDiffusiveIdx := -1;
   ParameterA := NotSet;
   ParameterB := NotSet;
   ThermalPropertiesIdx := 0;  { DeVries model is default (but only in MiniDaisy) }
   PassiveSlowly := NotSet;
   ActiveEasily := NotSet;
   CNofPassive := NotSet;
   CNofActive := NotSet;

   Dirty := FALSE;
   CurrentSaveLib := -1;  { -1: Not saved yet, 1: User Ref, 3: User Templ }

end;

procedure THorizonVars.LoadDefault;
begin
   Load('mini', 0); { Loads default values from mini-object }
end;

function THorizonVars.Load(HorName: string; lib: Integer): Boolean;
var
   i, AlistSize, TextureSum, NumTextureSet, RoundError: Integer;
   d: Double;
   Horizon, tortuosity, hydraulic: daisy_alist;
   dLib : daisy_library;
   HorizonSyntax : daisy_syntax;
   TempType: String;
begin
   { Init from HORIZON LIBRARY here }
   Result := TRUE; { be optimistic }

   { First open daisy library and get alist... }
   Horizon := GetLibraryObject('horizon', HorName);
   if Horizon = nil then begin
      Result := FALSE;
      exit;
   end;

   { ...then initialize vars from alist... }

   Reset; // Reset all vars to NotSet (or equal)

(*
  { Heading should come as argument to load() if loaded from lib }
   if lib = 0 then begin
      HorizonHeading1 := '';
      HorizonHeading2 := '';
   end
   else begin
      HorizonHeading1 := Copy(HorName, 1, 2); // Two first letters
      HorizonHeading2 := Copy(HorName, 4, RestOfString); // Skip a space and get the rest
   end;
*)

   // Then get data, if there are any...
   GetVariable(Horizon, 'description', daisy_type_string, @HorizonComments);

   TextureSum := 0; { Normalize if greater than 1 }
   NumTextureSet := 0;
   if GetVariable(Horizon, 'humus', daisy_type_number, @d) then begin
      d := d * 1000; // Trunc() f...s up if this is done as arg to trunc @##$!!
      TextureOrganicFrac := Trunc(d);
      TextureSum := TextureSum + TextureOrganicFrac;
      NumTextureSet := NumTextureSet + 1;
      TextureOrganicFracSet := 1;
   end;
   if GetVariable(Horizon, 'clay', daisy_type_number, @d) then begin
      d := d * 1000;
      TextureClayFrac := Trunc(d);
      TextureSum := TextureSum + TextureClayFrac;
      NumTextureSet := NumTextureSet + 1;
      TextureClayFracSet := 2;
   end;
   if GetVariable(Horizon, 'silt', daisy_type_number, @d) then begin
      d := d * 1000;
      TextureSiltFrac := Trunc(d);
      TextureSum := TextureSum + TextureSiltFrac;
      NumTextureSet := NumTextureSet + 1;
      TextureSiltFracSet := 4;
   end;
   if GetVariable(Horizon, 'fine_sand', daisy_type_number, @d) then begin
      d := d * 1000;
      TextureFineFrac := Trunc(d);
      TextureSum := TextureSum + TextureFineFrac;
      NumTextureSet := NumTextureSet + 1;
      TextureFineFracSet := 8;
   end;
   if GetVariable(Horizon, 'coarse_sand', daisy_type_number, @d) then begin
      d := d * 1000;
      TextureCoarseFrac := Trunc(d);
      TextureSum := TextureSum + TextureCoarseFrac;
      NumTextureSet := NumTextureSet + 1;
      TextureCoarseFracSet := 16;
   end;
   { Normalize... (may be necessary if input not made using user interface) }
   if (TextureSum > 1000) or
      ((TextureSum < 1000) and (NumTextureSet = 5)) then begin
      { Don't do anything if the sum is < 1000 when NOT all fields are set! }
      if TextureOrganicFracSet > 0 then
         TextureOrganicFrac := Round((TextureOrganicFrac * 1000) / TextureSum);
      if TextureClayFracSet > 0 then
         TextureClayFrac := Round((TextureClayFrac * 1000)/ TextureSum);
      if TextureSiltFracSet > 0 then
         TextureSiltFrac := Round((TextureSiltFrac * 1000) / TextureSum);
      if TextureFineFracSet > 0 then
         TextureFineFrac := Round((TextureFineFrac * 1000) / TextureSum);
      if TextureCoarseFracSet > 0 then
         TextureCoarseFrac := Round((TextureCoarseFrac * 1000) / TextureSum);
      RoundError := 1000 - TextureOrganicFrac - TextureClayFrac
                         - TextureSiltFrac - TextureFineFrac
                         - TextureCoarseFrac;
      if RoundError > 0 then begin { Distribute the error (can not be greater than NumTextureSet!)}
         if (RoundError > 0) and (TextureOrganicFracSet > 0)
                              and (TextureOrganicFrac > 0) then begin
            TextureOrganicFrac := TextureOrganicFrac + 1;
            RoundError := RoundError - 1;
         end;
         if (RoundError > 0) and (TextureClayFracSet > 0)
                              and (TextureClayFrac > 0) then begin
            TextureClayFrac := TextureClayFrac + 1;
            RoundError := RoundError - 1;
         end;
         if (RoundError > 0) and (TextureSiltFracSet > 0)
                              and (TextureSiltFrac > 0) then begin
            TextureSiltFrac := TextureSiltFrac + 1;
            RoundError := RoundError - 1;
         end;
         if (RoundError > 0) and (TextureFineFracSet > 0)
                              and (TextureFineFrac > 0) then begin
            TextureFineFrac := TextureFineFrac + 1;
            RoundError := RoundError - 1;
         end;
         if (RoundError > 0) and (TextureCoarseFracSet > 0)
                              and (TextureCoarseFrac > 0) then begin
            TextureCoarseFrac := TextureCoarseFrac + 1;
            RoundError := RoundError - 1;
         end;
      end
      else if RoundError < 0 then begin { Distribute the error (can not be greater than NumTextureSet!)}
         if (RoundError < 0) and (TextureOrganicFracSet > 0)
                              and (TextureOrganicFrac > 0) then begin
            TextureOrganicFrac := TextureOrganicFrac - 1;
            RoundError := RoundError + 1;
         end;
         if (RoundError < 0) and (TextureClayFracSet > 0)
                              and (TextureClayFrac > 0) then begin
            TextureClayFrac := TextureClayFrac - 1;
            RoundError := RoundError + 1;
         end;
         if (RoundError < 0) and (TextureSiltFracSet > 0)
                              and (TextureSiltFrac > 0) then begin
            TextureSiltFrac := TextureSiltFrac - 1;
            RoundError := RoundError + 1;
         end;
         if (RoundError < 0) and (TextureFineFracSet > 0)
                              and (TextureFineFrac > 0) then begin
            TextureFineFrac := TextureFineFrac - 1;
            RoundError := RoundError + 1;
         end;
         if (RoundError < 0) and (TextureCoarseFracSet > 0)
                              and (TextureCoarseFrac > 0) then begin
            TextureCoarseFrac := TextureCoarseFrac - 1;
            RoundError := RoundError + 1;
         end;
      end;
   end;
   { Texture read!!! $@££%&@ }

   GetVariable(Horizon, 'dry_bulk_density', daisy_type_number, @DryBulk);

   if GetVariable(Horizon, 'hydraulic', daisy_type_alist, @hydraulic) then begin
      TempType := '';
      if GetVariable(hydraulic, 'type', daisy_type_string, @TempType) then begin
         if TempType = 'M_vG' then
            HydraulicPropertiesIdx := 0 { van Genuchten/Mualem }
         else if TempType = 'B_vG' then
            HydraulicPropertiesIdx := 1 { van Genuchten/Burdine }
         else if TempType = 'M_BaC' then
            HydraulicPropertiesIdx := 2 { Brooks and Corey/Mualem }
         else if TempType = 'B_BaC' then
            HydraulicPropertiesIdx := 3 { Brooks and Corey/Burdine }
         else if TempType = 'mod_BaC' then
            HydraulicPropertiesIdx := 4 { Modified Brooks and Corey }
         else if TempType = 'old' then
            HydraulicPropertiesIdx := 5; { Table - Daisy v.1 format }
      end;

      case HydraulicPropertiesIdx of
         0: { van Genuchten/Mualem }
            begin
               GetVariable(hydraulic, 'Theta_sat', daisy_type_number, @SaturatedWaterContent);
               GetVariable(hydraulic, 'Theta_res', daisy_type_number, @ResidualWaterContent);
               GetVariable(hydraulic, 'alpha', daisy_type_number, @VanGenuchtenAlpha);
               GetVariable(hydraulic, 'n', daisy_type_number, @VanGenuchtenN);
               GetVariable(hydraulic, 'K_sat', daisy_type_number, @SaturatedHydrCond);
            end;
         1: { van Genuchten/Burdine }
            begin
               GetVariable(hydraulic, 'Theta_sat', daisy_type_number, @SaturatedWaterContent);
               GetVariable(hydraulic, 'Theta_res', daisy_type_number, @ResidualWaterContent);
               GetVariable(hydraulic, 'alpha', daisy_type_number, @VanGenuchtenAlpha);
               GetVariable(hydraulic, 'n', daisy_type_number, @VanGenuchtenN);
               GetVariable(hydraulic, 'K_sat', daisy_type_number, @SaturatedHydrCond);
            end;
         2: { Brooks and Corey/Mualem }
            begin
               GetVariable(hydraulic, 'Theta_sat', daisy_type_number, @SaturatedWaterContent);
               GetVariable(hydraulic, 'Theta_res', daisy_type_number, @ResidualWaterContent);
               GetVariable(hydraulic, 'h_b', daisy_type_number, @BublingPressure);
               GetVariable(hydraulic, 'lambda', daisy_type_number, @PoreSizeDistIdx);
               GetVariable(hydraulic, 'K_sat', daisy_type_number, @SaturatedHydrCond);
            end;
         3: { Brooks and Corey/Burdine }
            begin
               GetVariable(hydraulic, 'Theta_sat', daisy_type_number, @SaturatedWaterContent);
               GetVariable(hydraulic, 'Theta_res', daisy_type_number, @ResidualWaterContent);
               GetVariable(hydraulic, 'h_b', daisy_type_number, @BublingPressure);
               GetVariable(hydraulic, 'lambda', daisy_type_number, @PoreSizeDistIdx);
               GetVariable(hydraulic, 'K_sat', daisy_type_number, @SaturatedHydrCond);
            end;
      4: { Modified Brooks and Corey }
            begin
               GetVariable(hydraulic, 'Theta_sat', daisy_type_number, @SaturatedWaterContent);
               GetVariable(hydraulic, 'Theta_res', daisy_type_number, @ResidualWaterContent);
               GetVariable(hydraulic, 'h_b', daisy_type_number, @BublingPressure);
               GetVariable(hydraulic, 'lambda', daisy_type_number, @PoreSizeDistIdx);
               GetVariable(hydraulic, 'K_sat', daisy_type_number, @SaturatedHydrCond);
            end;
         5: { Table - Daisy v.1 format }
            begin
               GetVariable(hydraulic, 'file', daisy_type_string, @TableDaisyV1File);
            end;
         6: { Pedo-transfer functions }
            { Not implemented yet!!! }
      end;
   end;

   if GetVariable(Horizon, 'tortuosity', daisy_type_alist, @tortuosity) then begin
      TempType := '';
      if GetVariable(tortuosity, 'type', daisy_type_string, @TempType) then begin

         if TempType = 'M_Q' then
            SoluteDiffusiveIdx := 0 { van Genuchten/Mualem }
         else if TempType = 'linear' then
            SoluteDiffusiveIdx := 1; { van Genuchten/Burdine }

         case SoluteDiffusiveIdx of
            0: { Millington Quirke }
               begin
                  // No parameters
               end;
            1: { Linear model (Daisy v.1) }
               begin
                  GetVariable(tortuosity, 'a', daisy_type_number, @ParameterA);
                  GetVariable(tortuosity, 'b', daisy_type_number, @ParameterB);
               end;
         end;
      end;
   end;

//  Not implemented yet...
//   _daisy_alist_get_integer(Horizon, StrPCopy(buf,'ThermalPropertiesIdx')
//                            , ThermalPropertiesIdx);

   GetVariableAt(Horizon, 'SOM_fractions', 0, daisy_type_number, @PassiveSlowly);
   GetVariableAt(Horizon, 'SOM_fractions', 1, daisy_type_number, @ActiveEasily);
   GetVariableAt(Horizon, 'SOM_C_per_N', 0, daisy_type_number, @CNofPassive);
   GetVariableAt(Horizon, 'SOM_C_per_N', 1, daisy_type_number, @CNofActive);
end;


function THorizonVars.Save(HorName: string; lib: Integer; OverWrt: Boolean): Boolean;
var
   SelLibStr, SaveToLib, ExistInLib: string;
   i: Integer;
   d: Double;
   Horizon, tortuosity, hydraulic: daisy_alist;
   HorizonSyntax : daisy_syntax;
   s: String;
begin
   { Save to HORIZON LIBRARY here }
   Result := TRUE;

   { create _daisy_alist... }
   Horizon := CreateHorizonAlist;
   hydraulic := _daisy_alist_create;
   tortuosity := _daisy_alist_create;
   if (Horizon = nil) or (hydraulic = nil) or (tortuosity = nil) then begin
      result := FALSE;
      exit;
   end;

   { ...now input parameters into alist... }
(*
 * Headings should come as argument to save() if saved to lib
 *  - else 'SimSpec' should be argument (temperary saved to lib)

         HorizonHeading1: String; { 2 letter horizon identifier }
         HorizonHeading2: String; { The rest of the horizon identifier }
*)
   if HorizonComments <> '' then
      SetVariable(Horizon, 'description', daisy_type_string, @HorizonComments);

   if TextureOrganicFracSet > 0 then begin
      d := TextureOrganicFrac / 1000;
      SetVariable(Horizon, 'humus', daisy_type_number, @d);
   end;
   if TextureClayFracSet > 0 then begin
      d := TextureClayFrac / 1000;
      SetVariable(Horizon, 'clay', daisy_type_number, @d);
   end;
   if TextureSiltFracSet > 0 then begin
      d := TextureSiltFrac / 1000;
      SetVariable(Horizon, 'silt', daisy_type_number, @d);
   end;
   if TextureFineFracSet > 0 then begin
      d := TextureFineFrac / 1000;
      SetVariable(Horizon, 'fine_sand', daisy_type_number, @d);
   end;
   if TextureCoarseFracSet > 0 then begin
      d := TextureCoarseFrac / 1000;
      SetVariable(Horizon, 'coarse_sand', daisy_type_number, @d);
   end;

   if DryBulk <> NotSet then
      SetVariable(Horizon, 'dry_bulk_density', daisy_type_number, @DryBulk);

   case HydraulicPropertiesIdx of
      0: { van Genuchten/Mualem }
         begin
            s := 'M_vG';
            SetVariable(hydraulic, 'type', daisy_type_string, @s);
            if (SaturatedWaterContent <> NotSet) then
               SetVariable(hydraulic, 'Theta_sat', daisy_type_number, @SaturatedWaterContent);
            if (ResidualWaterContent <> NotSet) then
               SetVariable(hydraulic, 'Theta_res', daisy_type_number, @ResidualWaterContent);
            if (VanGenuchtenAlpha <> NotSet) then
               SetVariable(hydraulic, 'alpha', daisy_type_number, @VanGenuchtenAlpha);
            if (VanGenuchtenN <> NotSet) then
               SetVariable(hydraulic, 'n', daisy_type_number, @VanGenuchtenN);
            if (SaturatedHydrCond <> NotSet) then
               SetVariable(hydraulic, 'K_sat', daisy_type_number, @SaturatedHydrCond);
         end;
      1: { van Genuchten/Burdine }
         begin
            s := 'B_vG';
            SetVariable(hydraulic, 'type', daisy_type_string, @s);
            if (SaturatedWaterContent <> NotSet) then
               SetVariable(hydraulic, 'Theta_sat', daisy_type_number, @SaturatedWaterContent);
            if (ResidualWaterContent <> NotSet) then
               SetVariable(hydraulic, 'Theta_res', daisy_type_number, @ResidualWaterContent);
            if (VanGenuchtenAlpha <> NotSet) then
               SetVariable(hydraulic, 'alpha', daisy_type_number, @VanGenuchtenAlpha);
            if (VanGenuchtenN <> NotSet) then
               SetVariable(hydraulic, 'n', daisy_type_number, @VanGenuchtenN);
            if (SaturatedHydrCond <> NotSet) then
               SetVariable(hydraulic, 'K_sat', daisy_type_number, @SaturatedHydrCond);
         end;
      2: { Brooks and Corey/Mualem }
         begin
            s := 'M_BaC';
            SetVariable(hydraulic, 'type', daisy_type_string, @s);
            if (SaturatedWaterContent <> NotSet) then
               SetVariable(hydraulic, 'Theta_sat', daisy_type_number, @SaturatedWaterContent);
            if (ResidualWaterContent <> NotSet) then
               SetVariable(hydraulic, 'Theta_res', daisy_type_number, @ResidualWaterContent);
            if (BublingPressure <> NotSet) then
               SetVariable(hydraulic, 'h_b', daisy_type_number, @BublingPressure);
            if (PoreSizeDistIdx > -1) then
               SetVariable(hydraulic, 'lambda', daisy_type_number, @PoreSizeDistIdx);
            if (SaturatedHydrCond <> NotSet) then
               SetVariable(hydraulic, 'K_sat', daisy_type_number, @SaturatedHydrCond);
         end;
      3: { Brooks and Corey/Burdine }
         begin
            s := 'B_BaC';
            SetVariable(hydraulic, 'type', daisy_type_string, @s);
            if (SaturatedWaterContent <> NotSet) then
               SetVariable(hydraulic, 'Theta_sat', daisy_type_number, @SaturatedWaterContent);
            if (ResidualWaterContent <> NotSet) then
               SetVariable(hydraulic, 'Theta_res', daisy_type_number, @ResidualWaterContent);
            if (BublingPressure <> NotSet) then
               SetVariable(hydraulic, 'h_b', daisy_type_number, @BublingPressure);
            if (PoreSizeDistIdx > -1) then
               SetVariable(hydraulic, 'lambda', daisy_type_number, @PoreSizeDistIdx);
            if (SaturatedHydrCond <> NotSet) then
               SetVariable(hydraulic, 'K_sat', daisy_type_number, @SaturatedHydrCond);
         end;
      4: { Modified Brooks and Corey }
         begin
            s := 'mod_BaC';
            SetVariable(hydraulic, 'type', daisy_type_string, @s);
            if (SaturatedWaterContent <> NotSet) then
               SetVariable(hydraulic, 'Theta_sat', daisy_type_number, @SaturatedWaterContent);
            if (ResidualWaterContent <> NotSet) then
               SetVariable(hydraulic, 'Theta_res', daisy_type_number, @ResidualWaterContent);
            if (BublingPressure <> NotSet) then
               SetVariable(hydraulic, 'h_b', daisy_type_number, @BublingPressure);
            if (PoreSizeDistIdx > -1) then
               SetVariable(hydraulic, 'lambda', daisy_type_number, @PoreSizeDistIdx);
            if (SaturatedHydrCond <> NotSet) then
               SetVariable(hydraulic, 'K_sat', daisy_type_number, @SaturatedHydrCond);
         end;
      5: { Table - Daisy v.1 format }
         begin
            s := 'old';
            SetVariable(hydraulic, 'type', daisy_type_string, @s);
            if ( TableDaisyV1File <> '') then
               SetVariable(hydraulic, 'file', daisy_type_string, @TableDaisyV1File);
         end;
      6: { Pedo-transfer functions }
         { Not implemented yet!!! }
   end;
   if HydraulicPropertiesIdx > -1 then
      SetVariable(Horizon, 'hydraulic', daisy_type_alist, @hydraulic);

   case SoluteDiffusiveIdx of
      0: { Millington Quirke }
         begin
            s := 'M_Q';
            SetVariable(tortuosity, 'type', daisy_type_string, @s);
         end;
      1: { Linear model (Daisy v.1) }
         begin
            s := 'linear';
            SetVariable(tortuosity, 'type', daisy_type_string, @s);
            if ParameterA <> NotSet then
               SetVariable(tortuosity, 'a', daisy_type_number, @ParameterA);
            if ParameterB <> NotSet then
               SetVariable(tortuosity, 'b', daisy_type_number, @ParameterB);
         end;
   end;
   if SoluteDiffusiveIdx > -1 then
      SetVariable(Horizon, 'tortuosity', daisy_type_alist, @tortuosity);

//  Not implemented yet...
//   _daisy_alist_set_integer(Horizon, StrPCopy(buf,'ThermalPropertiesIdx')
//                            , ThermalPropertiesIdx);


   if PassiveSlowly <> NotSet then
      SetVariableAt(Horizon, 'SOM_fractions', 0, daisy_type_number, @PassiveSlowly);
   if ActiveEasily <> NotSet then
      SetVariableAt(Horizon, 'SOM_fractions', 1, daisy_type_number, @ActiveEasily);
   if CNofPassive <> NotSet then
      SetVariableAt(Horizon, 'SOM_C_per_N', 0, daisy_type_number, @CNofPassive);
   if CNofActive <> NotSet then
      SetVariableAt(Horizon, 'SOM_C_per_N', 1, daisy_type_number, @CNofActive);


{ ...last save the alist to library... }
   case lib of
      0: begin { User defined template- or reference library }
               { , depending on CurrentSaveLib               }
            case CurrentSaveLib of
               1: begin
                     SelLibStr := MainFormVars.UserRefLib;    { User Ref }
                     SaveToLib := LoadStr(RES_MSC_UsrRef);
                  end;
               3: begin
                     SelLibStr := MainFormVars.UserTemplLib;  { User Templ }
                     SaveToLib := LoadStr(RES_MSC_UsrTempl);
                  end;
            else
               CurrentSaveLib := -1;  // Just to be safe...
               Result := False;
               exit;
            end;

            i := UsedInLib(HorName, 'horizon');
            if i = 0 then begin
               SaveInLibrary('horizon', Horizon, HorName, SelLibStr);
               { Commit the changes in CurrentSaveLib }
               SaveDaisyLibrary(SelLibStr, SelLibStr, nil);
               MainFormVars.UpdateLib(CurrentSaveLib, 3);
            end
            else if (i = 4) or (i = 2) then begin
               if (i = 2) then
                  ExistInLib := LoadStr(RES_MSC_UsrRef)
               else // (i=4)
                  ExistInLib := LoadStr(RES_MSC_UsrTempl);
               if not OverWrt then
                  if MessageDlg(LoadStr(RES_MSG_Horizon)
                                 + ' ' + HorName + ' '
                                 + ' allready exists in the ' + ExistInLib + '.'
                                 + CHR(10) + CHR(13) + CHR(13)
                                 + 'Continue saving to ' + SaveToLib + ' and overwrite?'
                                 , mtWarning, [mbYes, mbNo], 0) = mrYes then
                     OverWrt := TRUE;
               if OverWrt then begin
                  DeleteLibraryObject('horizon', HorName); { better safe than sorry ;-) }
                  SaveInLibrary('horizon', Horizon, HorName, SelLibStr);
                  { Commit the changes in CurrentSaveLib }
                  SaveDaisyLibrary(SelLibStr, SelLibStr, nil);
                  MainFormVars.UpdateLib(CurrentSaveLib, 3);
               end
               else begin
                  result := FALSE;
                  exit; { quit saving! }
               end;
            end
            else begin { i <> 0 and i <> 2 and i <> 4 }
               case i of
                  1: s := LoadStr(RES_MSC_StdRef);
//                  2: s := LoadStr(RES_MSC_UsrRef);
                  3: s := LoadStr(RES_MSC_StdTempl);
                  5: s := LoadStr(RES_MSC_SimSpec);
               else
                  s := 'Unknown BUG Library';
               end;
               MessageDlg(LoadStr(RES_MSG_Horizon)
                           + ' ' + HorName + ' '
                           + LoadStr(RES_MSG_Exist_in_Lib2)
                           + ' ' + s + ' '
                           + LoadStr(RES_MSG_Exist_in_Lib3)
                           , mtInformation, [mbOK], 0);
               result := FALSE;
               exit; { quit saving! }
            end;
         end;
      1: begin { Simulation specific library }
            i := UsedInLib(HorName, 'horizon');
            if i = 0 then begin
               SaveInLibrary('horizon', Horizon, HorName, MainFormVars.SimSpecLib);
               // { Commit the changes in Simulation specific library }
               // SaveDaisyLibrary(MainFormVars.SimSpecLib,MainFormVars.SimSpecLib,nil);
               MainFormVars.UpdateLib(4, 3);
            end
            else if i = 5 then begin
               if not OverWrt then
                  if MessageDlg(LoadStr(RES_MSG_Horizon)
                                 + ' ' + HorName + ' '
                                 + LoadStr(RES_ERR_Globals_Exist_in_SimSpec2)
                                 + CHR(10) + CHR(13) + CHR(13)
                                 + LoadStr(RES_MSG_Overwrite)
                                 , mtWarning, [mbYes, mbNo], 0) = mrYes then
                     OverWrt := TRUE;
               if OverWrt then begin
                  DeleteLibraryObject('horizon', HorName); { better safe than sorry ;-) }
                  SaveInLibrary('horizon', Horizon, HorName, MainFormVars.SimSpecLib);
                  // { Commit the changes in simulation specific library }
                  // SaveDaisyLibrary(MainFormVars.SimSpecLib,MainFormVars.SimSpecLib,nil);
                  MainFormVars.UpdateLib(4, 3);
               end
               else begin
                  result := FALSE;
                  exit; { quit saving! }
               end;
            end
            else begin { i <> 0 and i <> 5 }
               case i of
                  1: s := LoadStr(RES_MSC_StdRef);
                  2: s := LoadStr(RES_MSC_UsrRef);
                  3: s := LoadStr(RES_MSC_StdTempl);
                  4: s := LoadStr(RES_MSC_UsrTempl);
               else
                  s := 'Unknown BUG Library';
               end;
               MessageDlg(LoadStr(RES_MSG_Horizon)
                           + ' ' + HorName + ' '
                           + LoadStr(RES_MSG_Exist_in_Lib2)
                           + ' ' + s + ' '
                           + LoadStr(RES_MSG_Exist_in_Lib3)
                           + CHR(10) + CHR(13)
                           + LoadStr(RES_ERR_Globals_Hor_Enter_Diff_Heading)
                           , mtInformation, [mbOK], 0);
               result := FALSE;
               exit; { quit saving! }
            end;
         end;
   end;
end;

procedure THorizonVars.Delete;
begin
   { Delete from HORIZON LIBRARY here }
end;

procedure THorizonVars.Assign(src: THorizonVars);
begin
   HorizonHeading1 := src.HorizonHeading1;
   HorizonHeading2 := src.HorizonHeading2;
   HorizonComments := src.HorizonComments;
   TextureOrganicFrac := src.TextureOrganicFrac;
   TextureClayFrac := src.TextureClayFrac;
   TextureSiltFrac := src.TextureSiltFrac;
   TextureFineFrac := src.TextureFineFrac;
   TextureCoarseFrac := src.TextureCoarseFrac;
   TextureOrganicFracSet := src.TextureOrganicFracSet;
   TextureClayFracSet := src.TextureClayFracSet;
   TextureSiltFracSet := src.TextureSiltFracSet;
   TextureFineFracSet := src.TextureFineFracSet;
   TextureCoarseFracSet := src.TextureCoarseFracSet;
   DryBulk := src.DryBulk;
   HydraulicPropertiesIdx := src.HydraulicPropertiesIdx;
   SaturatedWaterContent := src.SaturatedWaterContent;
   ResidualWaterContent := src.ResidualWaterContent;
   VanGenuchtenAlpha := src.VanGenuchtenAlpha;
   VanGenuchtenN := src.VanGenuchtenN;
   SaturatedHydrCond := src.SaturatedHydrCond;
   BublingPressure := src.BublingPressure;
   PoreSizeDistIdx := src.PoreSizeDistIdx;
   TableDaisyV1File := src.TableDaisyV1File;
   SoluteDiffusiveIdx := src.SoluteDiffusiveIdx;
   ParameterA := src.ParameterA;
   ParameterB := src.ParameterB;
   ThermalPropertiesIdx := src.ThermalPropertiesIdx;
   PassiveSlowly := src.PassiveSlowly;
   ActiveEasily := src.ActiveEasily;
   CNofPassive := src.CNofPassive;
   CNofActive := src.CNofActive;
end;

(*
{ Merges two horizon headers into a library horison name         }
{ Return 0: OK, 1: header1 missing, 2: header2 missing           }
function THorizonVars.MkHorizonName(Header1, Header2: String; var HorName: String): Integer;
var
   s1, s2: String;
begin
   s1 := Trim(Header1); s2 := Trim(Header2);
   if Length(s1) = 0 then Result := 1
   else if Length(s2) = 0 then Result := 2
   else begin
      if Length(s1) = 1 then  { Header2 should start at position 3 in HorName }
         HorName := Copy(s1, 1, 1) + '  ' + s2
      else
         HorName := Copy(s1, 1, 2) + ' ' + s2;
      Result := 0;
   end;
end;
*)

(*
{ Splits a library horizon name into two horizon headers         }
{ Return 0: OK, 1: horname not long enough for header1 + header2 }
function THorizonVars.SplitHorizonName(var Header1, Header2: String; HorName: String): Integer;
var
   s1, s2: String;
begin
   s1 := Trim(Copy(HorName, 1, 2));
   s2 := Trim(Copy(HorName, 4, 9999)); { copy to the end of HorName }
   if (s1 = '') or (s2 = '') then Result := 1
   else begin
      Header1 := s1; Header2 := s2;
      Result := 0;
   end;
end;
*)


{ *********************************** }
{           TProfileVars              }
{ *********************************** }

constructor TProfileVars.Create;
begin
{   ProfileComments := TStringList.Create;}
(*
   SGHorizonsItems := TStringGrid.Create(FormGlobals);
   SGHorizonsItems.FixedRows := 0; { ##@£"# Delphi }
*)
   SGProfileItems := TStringGrid.Create(FormGlobals);
   SGProfileItems.FixedRows := 0;  { ##@£"# Delphi }

   DiscVars := TDiscretizationVars.Create;
{   HorVars := THorizonVars.Create;}
   StateVars := TStateVars.Create;
   Dirty := False;
   CurrentSaveLib := -1;  { -1: Not saved yet, 1: User Ref, 3: User Templ }
end;

destructor TProfileVars.Destroy;
begin
{   ProfileComments.Free;}
(*   SGHorizonsItems.Free;*)
   SGProfileItems.Free;

   DiscVars.Free;
{   HorVars.Free;}
   StateVars.Free;
end;

procedure TProfileVars.Reset;
var
   i: Integer;
begin
{ Reset all profile variables... }
   ProfileComments := '';
(*
   ProfileHeading := '';
*)
   RGSelectIdx := 0;

   SGProfileItems.RowCount := 0; { means := 1  ##@$#       }
   NumSGProfileItems := 0;       { ...so this is neccesary }
   SGProfileItems.ColCount := 3;
   DiscretizationViewed := TRUE; { Since no horizons in profile }

   RootingDepth := NotSet;
   Dispersivity := NotSet;
   WaterFlowIdx := -1;
   DepthOfGroundWater := NotSet;
   FluctuatingGroundwaterFile := '';
   LysimeterDepth := NotSet;
   AbsorptionIsotermIdx := -1;

   DiscVars.Reset(Self);
{   HorVars.Reset;}
   StateVars.Reset;

   Dirty := FALSE;
   CurrentSaveLib := -1;  { -1: Not saved yet, 1: User Ref, 3: User Templ }
end;

function TProfileVars.Load(ProfName: string; lib: Integer): Boolean;
var
   i, j, size, tempint: Integer;
   s, TempType: String;
   d, d2, depth: Double;
   buf: array [0..255] of Char;
   Column, Soil, LBC, horizonsX, horX, initialX : daisy_alist;
   SoilWater, SoilHeat, SoilNO3, SoilNH4, Adsorption  : daisy_alist;
begin
   { Load from PROFILE LIBRARY here }
   Result := TRUE; { be optimistic }

   { First open daisy library and get alist... }
   Column := GetLibraryObject('column', ProfName);
   if Column = nil then begin
      Result := FALSE;
      exit;
   end;

   { ...then initialize vars from alist... }

   Reset; // Reset all vars to NotSet (or equal)

(*
  { Heading should come as argument to load() if loaded from lib }
   if lib = 0 then
      ProfileHeading := ''
   else
      ProfileHeading := ProfName;
*)

   // Then get data, if there are any...
   GetVariable(Column, 'description', daisy_type_string, @ProfileComments);

   if GetVariable(Column, 'Soil', daisy_type_alist, @Soil) then begin
      GetVariable(Soil, 'MaxRootingDepth', daisy_type_number, @RootingDepth);
      GetVariable(Soil, 'dispersivity', daisy_type_number, @Dispersivity);

      if GetVariableAt(Soil, 'horizons', 0, daisy_type_alist, @horizonsX) then begin
         with SGProfileItems do begin
            d2 := 0;
            NumSGProfileItems := _daisy_alist_size_alist(Soil, StrPCopy(buf,'horizons'));
            if NumSGProfileItems > 0 then
               RowCount := NumSGProfileItems;
            for i:=0 to NumSGProfileItems-1 do begin { Remember negative depths... }
               if GetVariableAt(Soil, 'horizons', i, daisy_type_alist, @horizonsX) then begin
                  if GetVariable(horizonsX, 'end', daisy_type_number, @d) then begin
                     Cells[1,i] := StrTo1DecPosNotZeroStr(FloatToStr(d2 - d)); // Negative!!
                     d2 := d; // Previous end
                  end;
                  if GetVariable(horizonsX, 'horizon', daisy_type_alist, @horX) then begin
                     GetVariable(horX, 'type', daisy_type_string, @s);
                     Cells[0,i] := s;
                  end
                  else begin { horizon not in library??? }
                     MessageDlg(LoadStr(RES_ERR_Globals_Prof_Could_Not_Load)
                                 + CHR(10) + CHR(13) + CHR(13)
                                 + LoadStr(RES_ERR_Globals_Prof_Ref_Hor_Not_Avail),
                                 mtError, [mbOK], 0);
                     Result := FALSE;
                     Reset; { Remember to reset the so-far loaded profile }
                     exit;
                  end;
               end;
            end;
         end;
      end;
      // First check if there is a discretization (or else daisydll craps up!)
      if GetVariableAt(Soil, 'zplus', 0, daisy_type_number, @d) then begin
         with DiscVars.Discretization do begin
            size := _daisy_alist_size_number(Soil, StrPCopy(buf,'zplus'));
            for i:=0 to size-1 do begin{ Remember negative values... }
               GetVariableAt(Soil, 'zplus', i, daisy_type_number, @d);
               Add(StrTo1DecPosNotZeroStr(FloatToStr(-d))); // Negative!!
               DiscVars.Untouchables.Add('');
               depth := 0;
               for j:=0 to NumSGProfileItems-1 do begin { check if untouchable }
                  Val(SGProfileItems.Cells[1,j], d2, tempint);
                  depth := depth + d2; // SGProfileItems contains SIZES, not depths
                  if -d = depth then begin // Discretizationvariables are DEPTHS
                     DiscVars.Untouchables[i] := 'U';
                     break;
                  end;
               end;
            end;
         end;
         { Now check that the discretization read is valid for the profile!! }
         if not DiscVars.IsHorsInDisc(Self) then
            DiscVars.Reset(Self);
      end;
   end; { Soil read!! }

   if GetVariable(Column, 'Groundwater', daisy_type_alist, @LBC) then begin
      TempType := '';
      if GetVariable(LBC, 'type', daisy_type_string, @TempType) then begin
         if TempType = 'static' then begin
            if GetVariable(LBC, 'table', daisy_type_number, @d) then begin
               if d = 1.0 then
                  WaterFlowIdx := 0 { van Genuchten/Burdine }
               else begin
                  WaterFlowIdx := 1; { van Genuchten/Mualem }
                  DepthOfGroundWater := d;
               end;
            end;
         end
         else if TempType = 'file' then begin
            if GetVariable(LBC, 'file', daisy_type_string, @FluctuatingGroundwaterFile) then
               WaterFlowIdx := 2; { van Genuchten/Burdine }
         end
         else if TempType = 'Lysimeter' then begin
            if GetVariable(LBC, 'table', daisy_type_number, @d) then begin
               LysimeterDepth := -d; // Negative!!
               WaterFlowIdx := 3; { Brooks and Corey/Mualem }
            end;
         end;
      end;
   end; { LBC Read!! }

   if GetVariable(Column, 'SoilWater', daisy_type_alist, @SoilWater) then begin
//      TempProfileVars.StateVars.SoilWaterContIdx := 1; { default!! } Do this in .Reset
      if GetVariableAt(SoilWater, 'h', 0, daisy_type_number, @d) then begin
         TempProfileVars.StateVars.SoilWaterContIdx := 0; { Field capasity }
      end
      else if GetVariableAt(SoilWater, 'initial_h', 0, daisy_type_alist, @initialX) then begin
         with TempProfileVars.StateVars.PotentialVsDepth do begin
            size := _daisy_alist_size_alist(SoilWater, StrPCopy(buf, 'initial_h'));
            TempProfileVars.StateVars.NumPotentialVsDepthItems := size;
            RowCount := size;
            for i:=0 to size - 1 do begin
               if GetVariableAt(SoilWater, 'initial_h', i, daisy_type_alist, @initialX) then begin
                  GetVariable(initialX, 'end', daisy_type_number, @d);
                  Cells[0,i] := FloatToStr(-d); { All "initial_" depths are negative in StoreDaisy }
                  GetVariable(initialX, 'value', daisy_type_number, @d);
                  Cells[1,i] := FloatToStr(d);
               end;
            end;
         end;
         TempProfileVars.StateVars.SoilWaterContIdx := 2; { Potencial vs. depth }
      end
      else if GetVariableAt(SoilWater, 'initial_Theta', 0, daisy_type_alist, @initialX) then begin
         with TempProfileVars.StateVars.WaterContVsDepth do begin
            size := _daisy_alist_size_alist(SoilWater, StrPCopy(buf, 'initial_Theta'));
            TempProfileVars.StateVars.NumWaterContVsDepthItems := size;
            RowCount := size;
            for i:=0 to size - 1 do begin
               if GetVariableAt(SoilWater, 'initial_Theta', i, daisy_type_alist, @initialX) then begin
                  GetVariable(initialX, 'end', daisy_type_number, @d);
                  Cells[0,i] := FloatToStr(-d); { All "initial_" depths are negative in StoreDaisy }
                  GetVariable(initialX, 'value', daisy_type_number, @d);
                  Cells[1,i] := FloatToStr(d);
               end;
            end;
         end;
         TempProfileVars.StateVars.SoilWaterContIdx := 3; { Water Content vs. depth }
      end;
   end; { SoilWater Read!! }

   if GetVariable(Column, 'SoilHeat', daisy_type_alist, @SoilHeat) then begin
//      TempProfileVars.StateVars.SoilTemperatureIdx := 0;  { default!! }  Do this in .Reset
      if GetVariableAt(SoilHeat, 'initial_T', 0, daisy_type_alist, @initialX) then begin
         with TempProfileVars.StateVars.TemperatureVsDepth do begin
            size := _daisy_alist_size_alist(SoilHeat, StrPCopy(buf, 'initial_T'));
            TempProfileVars.StateVars.NumTemperatureVsDepthItems := size;
            RowCount := size;
            for i:=0 to size - 1 do begin
               if GetVariableAt(SoilHeat, 'initial_T', i, daisy_type_alist, @initialX) then begin
                  GetVariable(initialX, 'end', daisy_type_number, @d);
                  Cells[0,i] := FloatToStr(-d); { All "initial_" depths are negative in StoreDaisy }
                  GetVariable(initialX, 'value', daisy_type_number, @d);
                  Cells[1,i] := FloatToStr(d);
               end;
            end;
         end;
         TempProfileVars.StateVars.SoilTemperatureIdx := 1; { Temperature vs. depth }
      end;
   end; { SoilHeat Read!! }

   if GetVariable(Column, 'SoilNO3', daisy_type_alist, @SoilNO3) then begin
      if GetVariableAt(SoilNO3, 'C', 0, daisy_type_number
                          , @TempProfileVars.StateVars.NitrateConc) then
         TempProfileVars.StateVars.SoilNitrateContIdx := 0 { Uniform profile }
      else if GetVariableAt(SoilNO3, 'initial_C', 0, daisy_type_alist, @initialX) then begin
         with TempProfileVars.StateVars.NitrateVsDepth do begin
            size := _daisy_alist_size_alist(SoilNO3, StrPCopy(buf, 'initial_C'));
            TempProfileVars.StateVars.NumNitrateVsDepthItems := size;
            RowCount := size;
            for i:=0 to size - 1 do begin
               if GetVariableAt(SoilNO3, 'initial_C', i, daisy_type_alist, @initialX) then begin
                  GetVariable(initialX, 'end', daisy_type_number, @d);
                  Cells[0,i] := FloatToStr(-d); { All "initial_" depths are negative in StoreDaisy }
                  GetVariable(initialX, 'value', daisy_type_number, @d);
                  Cells[1,i] := FloatToStr(d);
               end;
            end;
         end;
         TempProfileVars.StateVars.SoilNitrateContIdx := 1; { Nitrate concentration vs. depth }
      end;
   end; { SoilNO3 Read!! }

   if GetVariable(Column, 'SoilNH4', daisy_type_alist, @SoilNH4) then begin
      if GetVariableAt(SoilNH4, 'ppm', 0, daisy_type_number
                          , @TempProfileVars.StateVars.AmmoniaContent) then
         TempProfileVars.StateVars.SoilAmmoniaContIdx := 0 { Uniform profile }
      else if GetVariableAt(SoilNH4, 'initial_ppm', 0, daisy_type_alist, @initialX) then begin
         with TempProfileVars.StateVars.AmmoniaVsDepth do begin
            size := _daisy_alist_size_alist(SoilNH4, StrPCopy(buf, 'initial_ppm'));
            TempProfileVars.StateVars.NumAmmoniaVsDepthItems := size;
            RowCount := size;
            for i:=0 to size - 1 do begin
               if GetVariableAt(SoilNH4, 'initial_ppm', i, daisy_type_alist, @initialX) then begin
                  GetVariable(initialX, 'end', daisy_type_number, @d);
                  Cells[0,i] := FloatToStr(-d); { All "initial_" depths are negative in StoreDaisy }
                  GetVariable(initialX, 'value', daisy_type_number, @d);
                  Cells[1,i] := FloatToStr(d);
               end;
            end;
         end;
         TempProfileVars.StateVars.SoilAmmoniaContIdx := 1; { Nitrate concentration vs. depth }
      end;

      if GetVariable(SoilNH4, 'adsorption', daisy_type_alist, @Adsorption) then begin
         if GetVariable(Adsorption, 'type', daisy_type_string, @TempType) then begin
            if TempType = 'vS_S' then { Daisy v.1 }
               AbsorptionIsotermIdx := 0
            else if TempType = 'Langmuir' then { Langmuir }
               AbsorptionIsotermIdx := 1
            else if TempType = 'mini-Freundlich' then { Freundlich }
               AbsorptionIsotermIdx := 2
            else if TempType = 'linear' then { Linear Freundlich }
               AbsorptionIsotermIdx := 3;
         end;
      end;
   end;

   { SoilNH4 Read!! }

   { Column read!! }
end;

function TProfileVars.Save(ProfName: string; lib: Integer; OverWrt: Boolean): Boolean;
var
   i: Integer;
   SelLibStr, SaveToLib, ExistInLib: string;
   s: String;
   d: Double;
   Bioclimate, Column, Soil, LBC, horizonsX, horX, initialX : daisy_alist;
   SoilWater, SoilHeat, SoilNO3, SoilNH4, Adsorption  : daisy_alist;
   ColumnSet, SoilSet, LBCSet: Boolean;
   SoilWaterSet, SoilHeatSet, SoilNO3Set, SoilNH4Set, AdsorbtionSet  : Boolean;
begin
   { Save to PROFILE LIBRARY here }
   Result := TRUE;
   ColumnSet := FALSE; SoilSet := FALSE; LBCSet := FALSE; SoilWaterSet := FALSE;
   SoilHeatSet := FALSE; SoilNO3Set := FALSE; SoilNH4Set:= FALSE;
   AdsorbtionSet := FALSE;

   { ...then create _daisy_alist... }
   Column := CreateColumnAlist;
    { We reuse the submodel alists from the Column object to get any default
      members set. }

   (* Ugly hack for no good reason *)
   s := 'default';
   Bioclimate := GetLibraryObject ('bioclimate', s);
   Bioclimate :=_daisy_alist_clone (Bioclimate);
   SetVariable (Bioclimate, 'type', daisy_type_string, @s);
   SetVariable(Column, 'Bioclimate', daisy_type_alist, @Bioclimate);

   GetVariable(Column, 'Soil', daisy_type_alist, @Soil);
   Soil := _daisy_alist_clone(Soil);
   GetVariable(Column, 'SoilWater', daisy_type_alist, @SoilWater);
   SoilWater := _daisy_alist_clone(SoilWater);
   GetVariable(Column, 'SoilHeat', daisy_type_alist, @SoilHeat);
   SoilHeat := _daisy_alist_clone(SoilHeat);
   GetVariable(Column, 'SoilNO3', daisy_type_alist, @SoilNO3);
   SoilNO3 := _daisy_alist_clone(SoilNO3);
   GetVariable(Column, 'SoilNH4', daisy_type_alist, @SoilNH4);
   SoilNH4 := _daisy_alist_clone(SoilNH4);
   { We can't initialize the component members (LBC, Adsorbtion) before we
     know what model they should be. }
   if (Column = nil) or (Soil = nil) {or (Adsorption = nil)}
         {or (LBC = nil)} or (SoilWater = nil) or (SoilHeat = nil)         or (SoilNO3 = nil) or (SoilNH4 = nil) then begin
      result := FALSE;
      exit;
   end;

   { ...now input parameters into alist... }
(*
 * Headings should come as argument to save() when saved to lib

         ProfileHeading: String; { The profile identifier }
*)
   if ProfileComments <> '' then begin
      SetVariable(Column, 'description', daisy_type_string, @ProfileComments);
      ColumnSet := TRUE;
   end;
   if RootingDepth <> NotSet then begin
      SetVariable(Soil, 'MaxRootingDepth', daisy_type_number, @RootingDepth);
      SoilSet := TRUE;
   end;
   if Dispersivity <> NotSet then begin
      SetVariable(Soil, 'dispersivity', daisy_type_number, @Dispersivity);
      SoilSet := TRUE;
   end;


   case WaterFlowIdx of
      0: { Grawimetric flow }
         begin
            s := 'static';
            d := 1.0;
            LBC := _daisy_alist_clone (GetLibraryObject ('groundwater', s));
            SetVariable(LBC, 'type', daisy_type_string, @s);
            SetVariable(LBC, 'table', daisy_type_number, @d);
            LBCSet := TRUE;
         end;
      1: { Fixed groundwater table }
         begin
            s := 'static';
            LBC := _daisy_alist_clone (GetLibraryObject ('groundwater', s));            SetVariable(LBC, 'type', daisy_type_string, @s);
            SetVariable(LBC, 'table', daisy_type_number, @DepthOfGroundWater);
            LBCSet := TRUE;
         end;
      2: { Fluctuating groundwater table }
         begin
            s := 'file';
            LBC := _daisy_alist_clone (GetLibraryObject ('groundwater', s));            SetVariable(LBC, 'type', daisy_type_string, @s);
            SetVariable(LBC, 'file', daisy_type_string, @FluctuatingGroundwaterFile);
            LBCSet := TRUE;
         end;
      3: { Lysimeter }
         begin
            s := 'Lysimeter';
            d := -LysimeterDepth;
            LBC := _daisy_alist_clone (GetLibraryObject ('groundwater', s));            SetVariable(LBC, 'type', daisy_type_string, @s);
            SetVariable(LBC, 'table', daisy_type_number, @d);
            LBCSet := TRUE;
         end;
   end;
   if LBCSet then begin
      SetVariable(Column, 'Groundwater', daisy_type_alist, @LBC);
      ColumnSet := TRUE;
   end;

   with DiscVars.Discretization do begin
      for i:=0 to Count - 1 do begin{ Remember negative values... }
         d := -StrToFloat(Strings[i]); // Negative!!
         SetVariableAt(Soil, 'zplus', i, daisy_type_number, @d);
         SoilSet := TRUE;
      end;
   end;

   with SGProfileItems do begin
      d := 0;
      for i:=0 to NumSGProfileItems-1 do begin { Remember negative depths... }
         horizonsX := _daisy_alist_create;
         if (horizonsX = nil) then begin
            Result := FALSE;
            exit;
         end
         else begin
            d := d - StrToFloat(Cells[1,i]); // Negative!! (depths - NOT size here!!)
            SetVariable(horizonsX, 'end', daisy_type_number, @d);
            s := Cells[0,i];
            horX := _daisy_alist_clone (GetLibraryObject ('horizon', s));
            SetVariable(horX, 'type', daisy_type_string, @s);
            SetVariable(horizonsX, 'horizon', daisy_type_alist, @horX);
            SetVariableAt(Soil, 'horizons', i, daisy_type_alist, @horizonsX);
            SoilSet := TRUE;
         end;
      end;
   end;

   case TempProfileVars.StateVars.SoilWaterContIdx of
      0: { Field capasity }
         begin
            d := -100;
            SetVariableAt(SoilWater, 'h', 0, daisy_type_number, @d);
            SoilWaterSet := TRUE;
         end;
      1: { Equilibrium profile }
         begin
            { do nothing -- default! }
         end;
      2: { Potencial vs. depth }
         begin
            with TempProfileVars.StateVars.PotentialVsDepth do begin
               if Trim(Cells[0, 0]) <> '' then begin
                  for i:=0 to TempProfileVars.StateVars.NumPotentialVsDepthItems - 1 do begin
                     initialX := _daisy_alist_create;
                     if initialX = nil then begin
                        Result := FALSE;
                        exit;
                     end
                     else begin
                        d := -StrToFloat(Cells[0,i]); { All "initial_" depths are negative in StoreDaisy }
                        SetVariable(initialX, 'end', daisy_type_number, @d);
                        d := StrToFloat(Cells[1,i]);
                        SetVariable(initialX, 'value', daisy_type_number, @d);
                        SetVariableAt(SoilWater, 'initial_h', i, daisy_type_alist, @initialX);
                        SoilWaterSet := TRUE;
                     end;
                  end;
               end;
            end;
         end;
      3: { Water Content vs. depth }
         begin
            with TempProfileVars.StateVars.WaterContVsDepth do begin
               if Trim(Cells[0, 0]) <> '' then begin
                  for i:=0 to TempProfileVars.StateVars.NumWaterContVsDepthItems - 1 do begin
                     initialX := _daisy_alist_create;
                     if initialX = nil then begin
                        Result := FALSE;
                        exit;
                     end
                     else begin
                        d := -StrToFloat(Cells[0,i]); { All "initial_" depths are negative in StoreDaisy }
                        SetVariable(initialX, 'end', daisy_type_number, @d);
                        d := StrToFloat(Cells[1,i]);
                        SetVariable(initialX, 'value', daisy_type_number, @d);
                        SetVariableAt(SoilWater, 'initial_Theta', i, daisy_type_alist, @initialX);
                        SoilWaterSet := TRUE;
                     end;
                  end;
               end;
            end;
         end;
   end;
   if SoilWaterSet then begin
      SetVariable(Column, 'SoilWater', daisy_type_alist, @SoilWater);
      ColumnSet := TRUE;
   end;

   case TempProfileVars.StateVars.SoilTemperatureIdx of
      0: { Equilibrium profile }
         begin
            { do nothing -- default! }
         end;
      1: { Temerature vs. depth }
         begin
            with TempProfileVars.StateVars.TemperatureVsDepth do begin
               if Trim(Cells[0, 0]) <> '' then begin
                  for i:=0 to TempProfileVars.StateVars.NumTemperatureVsDepthItems - 1 do begin
                     initialX := _daisy_alist_create;
                     if initialX = nil then begin
                        Result := FALSE;
                        exit;
                     end
                     else begin
                        d := -StrToFloat(Cells[0,i]); { All "initial_" depths are negative in StoreDaisy }
                        SetVariable(initialX, 'end', daisy_type_number, @d);
                        d := StrToFloat(Cells[1,i]);
                        SetVariable(initialX, 'value', daisy_type_number, @d);
                        SetVariableAt(SoilHeat, 'initial_T', i, daisy_type_alist, @initialX);
                        SoilHeatSet := TRUE;
                     end;
                  end;
               end;
            end;
         end;
   end;
   if SoilHeatSet then begin
      SetVariable(Column, 'SoilHeat', daisy_type_alist, @SoilHeat);
      ColumnSet := TRUE;
   end;

   case TempProfileVars.StateVars.SoilNitrateContIdx of
      0: { Uniform profile }
         begin
            SetVariableAt(SoilNO3, 'C', 0, daisy_type_number
                          , @TempProfileVars.StateVars.NitrateConc);
            SoilNO3Set := TRUE;
         end;
      1: { Nitrate concentration vs. depth }
         begin
            with TempProfileVars.StateVars.NitrateVsDepth do begin
               if Trim(Cells[0, 0]) <> '' then begin
                  for i:=0 to TempProfileVars.StateVars.NumNitrateVsDepthItems - 1 do begin
                     initialX := _daisy_alist_create;
                     if initialX = nil then begin
                        Result := FALSE;
                        exit;
                     end
                     else begin
                        d := -StrToFloat(Cells[0,i]); { All "initial_" depths are negative in StoreDaisy }
                        SetVariable(initialX, 'end', daisy_type_number, @d);
                        d := StrToFloat(Cells[1,i]);
                        SetVariable(initialX, 'value', daisy_type_number, @d);
                        SetVariableAt(SoilNO3, 'initial_C', i, daisy_type_alist, @initialX);
                        SoilNO3Set := TRUE;
                     end;
                  end;
               end;
            end;
         end;
   end;
   if SoilNO3Set then begin
      SetVariable(Column, 'SoilNO3', daisy_type_alist, @SoilNO3);
      ColumnSet := TRUE;
   end;

   case TempProfileVars.StateVars.SoilAmmoniaContIdx of
      0: { Uniform profile }
         begin
            SetVariableAt(SoilNH4, 'ppm', 0, daisy_type_number
                          , @TempProfileVars.StateVars.AmmoniaContent);
            SoilNH4Set := TRUE;
         end;
      1: { Ammonia Content vs. depth }
         begin
            with TempProfileVars.StateVars.AmmoniaVsDepth do begin
               if Trim(Cells[0, 0]) <> '' then begin
                  for i:=0 to TempProfileVars.StateVars.NumAmmoniaVsDepthItems - 1 do begin
                     initialX := _daisy_alist_create;
                     if initialX = nil then begin
                        Result := FALSE;
                        exit;
                     end
                     else begin
                        d := -StrToFloat(Cells[0,i]); { All "initial_" depths are negative in StoreDaisy }
                        SetVariable(initialX, 'end', daisy_type_number, @d);
                        d := StrToFloat(Cells[1,i]);
                        SetVariable(initialX, 'value', daisy_type_number, @d);
                        SetVariableAt(SoilNH4, 'initial_ppm', i, daisy_type_alist, @initialX);
                        SoilNH4Set := TRUE;
                     end;
                  end;
               end;
            end;
         end;
   end;
   case AbsorptionIsotermIdx of
      0: { Daisy v.1 }
         begin
            s := 'vS_S';
            Adsorption := _daisy_alist_clone (GetLibraryObject ('adsorption', s));            SetVariable(Adsorption, 'type', daisy_type_string, @s);
            AdsorbtionSet := TRUE;
         end;
      1: { Langmuir }
         begin
            s := 'Langmuir';
            Adsorption := _daisy_alist_clone (GetLibraryObject ('adsorption', s));
            SetVariable(Adsorption, 'type', daisy_type_string, @s);
            AdsorbtionSet := TRUE;
         end;
      2: { Freundlich }
         begin
            s := 'mini-Freundlich';
            Adsorption := _daisy_alist_clone (GetLibraryObject ('adsorption', s));
            SetVariable(Adsorption, 'type', daisy_type_string, @s);
            AdsorbtionSet := TRUE;
         end;
      3: { Linear Freundlich }
         begin
            s := 'linear';
            Adsorption := _daisy_alist_clone (GetLibraryObject ('adsorption', s));
            SetVariable(Adsorption, 'type', daisy_type_string, @s);
            AdsorbtionSet := TRUE;
         end;
   end;
   if AdsorbtionSet then begin
      SetVariable(SoilNH4, 'adsorption', daisy_type_alist, @Adsorption);
      SoilNH4Set := TRUE;
   end;

   if SoilNH4Set then begin
      SetVariable(Column, 'SoilNH4', daisy_type_alist, @SoilNH4);
      ColumnSet := TRUE;
   end;

   if SoilSet then
      SetVariable(Column, 'Soil', daisy_type_alist, @Soil);

{ ...last save the alist to library... }
   case lib of
      0: begin { User defined template- or reference library }
               { , depending on CurrentSaveLib               }
            case CurrentSaveLib of
               1: begin
                     SelLibStr := MainFormVars.UserRefLib;    { User Ref }
                     SaveToLib := LoadStr(RES_MSC_UsrRef);
                  end;
               3: begin
                     SelLibStr := MainFormVars.UserTemplLib;  { User Templ }
                     SaveToLib := LoadStr(RES_MSC_UsrTempl);
                  end;
            else
               CurrentSaveLib := -1;  // Just to be safe...
               Result := False;
               exit;
            end;

            i := UsedInLib(ProfName, 'column');
            if i = 0 then begin
               SaveInLibrary('column', Column, ProfName, SelLibStr);
               { Commit the changes in CurrentSaveLib }
               SaveDaisyLibrary(SelLibStr, SelLibStr, nil);
               MainFormVars.UpdateLib(CurrentSaveLib, 2);
            end
            else if (i = 4) or (i = 2) then begin
               if (i = 2) then
                  ExistInLib := LoadStr(RES_MSC_UsrRef)
               else // (i=4)
                  ExistInLib := LoadStr(RES_MSC_UsrTempl);
               if not OverWrt then
                  if MessageDlg(LoadStr(RES_MSG_Profile) + ' ' + ProfName + ' '
                                 + ' allready exists in the ' + ExistInLib + '.'
                                 + CHR(10) + CHR(13) + CHR(13)
                                 + 'Continue saving to ' + SaveToLib + ' and overwrite?',
                                 mtWarning, [mbYes, mbNo], 0) = mrYes then
                     OverWrt := TRUE;
               if OverWrt then begin
                  DeleteLibraryObject('column', ProfName); { better safe than sorry ;-) }
                  SaveInLibrary('column', Column, ProfName, SelLibStr);
                  { Commit the changes in CurrentSaveLib }
                  SaveDaisyLibrary(SelLibStr, SelLibStr, nil);
                  MainFormVars.UpdateLib(CurrentSaveLib, 2);
               end
               else begin
                  result := FALSE;
                  exit; { quit saving! }
               end;
            end
            else begin { i <> 0 and i <> 2 and i <> 4 }
               case i of
                  1: s := LoadStr(RES_MSC_StdRef);
//                  2: s := LoadStr(RES_MSC_UsrRef);
                  3: s := LoadStr(RES_MSC_StdTempl);
                  5: s := LoadStr(RES_MSC_SimSpec);
               else
                  s := 'Unknown BUG Library';
               end;
               MessageDlg(LoadStr(RES_MSG_Profile) + ' ' + ProfName + ' '
                          + LoadStr(RES_MSG_Exist_in_Lib2) + ' ' + s
                          + LoadStr(RES_MSG_Exist_in_Lib3),
                          mtInformation, [mbOK], 0);
               result := FALSE;
               exit; { quit saving! }
            end;
         end;
      1: begin { Simulation specific library }
            i := UsedInLib(ProfName, 'column');
            if i = 0 then begin
               SaveInLibrary('column', Column, ProfName, MainFormVars.SimSpecLib);
               // { Commit the changes in Simulation specific library }
               // SaveDaisyLibrary(MainFormVars.SimSpecLib,MainFormVars.SimSpecLib,nil);
               MainFormVars.UpdateLib(4, 2);
            end
            else if i = 5 then begin
               if not OverWrt then
                  if MessageDlg(LoadStr(RES_MSG_Profile) + ' ' + ProfName
                                 + LoadStr(RES_ERR_Globals_Exist_in_SimSpec2)
                                 + CHR(10) + CHR(13) + CHR(13)
                                 + LoadStr(RES_MSG_Overwrite)
                                 , mtWarning, [mbYes, mbNo], 0) = mrYes then
                     OverWrt := TRUE;
               if OverWrt then begin
                  DeleteLibraryObject('column', ProfName); { better safe than sorry ;-) }
                  SaveInLibrary('column', Column, ProfName, MainFormVars.SimSpecLib);
                  // { Commit the changes in simulation specific library }
                  // SaveDaisyLibrary(MainFormVars.SimSpecLib, MainFormVars.SimSpecLib,nil);
                  MainFormVars.UpdateLib(4, 2);
               end
               else begin
                  result := FALSE;
                  exit; { quit saving! }
               end;
            end
            else begin { i <> 0 and i <> 5 }
               case i of
                  1: s := LoadStr(RES_MSC_StdRef);
                  2: s := LoadStr(RES_MSC_UsrRef);
                  3: s := LoadStr(RES_MSC_StdTempl);
                  4: s := LoadStr(RES_MSC_UsrTempl);
               else
                  s := 'Unknown BUG Library';
               end;
               MessageDlg(LoadStr(RES_MSG_Profile) + ' ' + ProfName + ' '
                           + LoadStr(RES_MSG_Exist_in_Lib2) + s
                           + LoadStr(RES_MSG_Exist_in_Lib3)
                           + CHR(10) + CHR(13)
                           + LoadStr(RES_ERR_Globals_Prof_Enter_Diff_Heading)
                           , mtInformation, [mbOK], 0);
               result := FALSE;
               exit; { quit saving! }
            end;
         end;
   end;
end;

procedure TProfileVars.Assign(src: TProfileVars);
var
   i, j: Integer;
begin
   ProfileComments := src.ProfileComments;
   ProfileHeading := src.ProfileHeading;
   RGSelectIdx := src.RGSelectIdx;

   (*
   SGHorizonsItems.RowCount := src.SGHorizonsItems.RowCount;
   NumSGHorizonsItems := src.NumSGHorizonsItems;
   SGHorizonsItems.ColCount := src.SGHorizonsItems.ColCount;
   if NumSGHorizonsItems > 0 then
      for i := 0 to SGHorizonsItems.RowCount - 1 do
         for j := 0 to SGHorizonsItems.ColCount - 1 do
            SGHorizonsItems.Cells[j, i] := src.SGHorizonsItems.Cells[j, i];
   *)
   SGProfileItems.RowCount := src.SGProfileItems.RowCount;
   NumSGProfileItems := src.NumSGProfileItems;
   SGProfileItems.ColCount := src.SGProfileItems.ColCount;
   if NumSGProfileItems > 0 then
      for i := 0 to SGProfileItems.RowCount - 1 do
         for j := 0 to SGProfileItems.ColCount - 1 do
            SGProfileItems.Cells[j, i] := src.SGProfileItems.Cells[j, i];

   DiscretizationViewed := src.DiscretizationViewed;
   RootingDepth := src.RootingDepth;
   Dispersivity := src.Dispersivity;
   WaterFlowIdx := src.WaterFlowIdx;
   DepthOfGroundWater := src.DepthOfGroundWater;
   FluctuatingGroundwaterFile := src.FluctuatingGroundwaterFile;
   LysimeterDepth := src.LysimeterDepth;
   AbsorptionIsotermIdx := src.AbsorptionIsotermIdx;

   DiscVars.Assign(src.DiscVars);
{   HorVars.Assign(src.HorVars);}
   StateVars.Assign(src.StateVars);
end;

{ GetLowestHor returns -1 if no horizons in profile, else positive depth is returned }
function TProfileVars.GetLowestHor: Double;
var
   i: Integer;
begin
   if (NumSGProfileItems = 0) then
      Result := -1
   else begin
      Result := 0;
      for i:=0 to SGProfileItems.RowCount-1 do
         try
            Result := Result + StrToFloat(SGProfileItems.Cells[1,i]);
         except
            on E: Exception do begin { Bad number (should not occur here!) }
               Result := -1;
               break;
            end;
         end;
   end;
end;



{ *********************************** }
{         TDiscretizationVars         }
{ *********************************** }

constructor TDiscretizationVars.Create;
begin
   Discretization := TStringList.Create;
   UnTouchables := TStringList.Create;
   UnTouchables.Sorted := FALSE;  { to surely prevent automatic sorting of stringlist }
end;

destructor TDiscretizationVars.Destroy;
begin
   Discretization.Free;
   UnTouchables.Free;
end;

{ Resets the discretization after the horizon depths in the profile }
procedure TDiscretizationVars.Reset(ProfVars: TProfileVars);
var
   i: Integer;
   sg: Double;
begin
   Discretization.Clear;
   Untouchables.Clear;

   if (ProfVars.NumSGProfileItems > 0) then begin
      sg := 0;
      for i:=0 to ProfVars.SGProfileItems.RowCount - 1 do begin
         try
            sg := sg + StrToFloat(ProfVars.SGProfileItems.Cells[1, i]);
         except
            on E:Exception do begin
               MessageDlg(LoadStr(RES_ERR_Globals_Disc_Reset_To_Zero)
                           + CHR(10) + CHR(13)
                           + LoadStr(RES_ERR_Developer)
                           , mtError, [mbOK], 0);
               Discretization.Clear;
               Untouchables.Clear;
               break;
            end;
         end;
         Discretization.Add(StrTo1DecPosNotZeroStr(FloatToStr(sg)));
         Untouchables.Add('U');  { Set depth as untouchable }
      end;
   end;
end;

{ BubbleSorts the Discretization variable and keeps the Untouchables    }
{  array up to date                                                     }
{ If elements are invalid numbers, an exception EConvertError is raised }
{ Duplicates are removed without warning!!!                            }
procedure TDiscretizationVars.Sort;
var
   i, j: Integer;
   str, unt: string;
   Tempsl: TStringList;
   TempU: TStringList;
   ElemJ, ElemJ_1: Double;
begin
   Tempsl := TStringList.Create;
   TempU := TStringList.Create;
   try begin
      Tempsl.Clear;
      TempU.Clear;
      Tempsl.Sorted := FALSE;
      TempU.Sorted := FALSE;
      { Make a copy of the original list, in case of bad }
      { elements that raises an exception during sorting }
      Tempsl.AddStrings(Discretization);
      TempU.AddStrings(Untouchables);
      { First sort the lists }
      for i:=1 to Tempsl.Count - 1 do begin
         for j:=i downto 1 do begin { Bubble it up... }
            ElemJ := StrToFloat(Tempsl[j]);
            ElemJ_1 := StrToFloat(Tempsl[j-1]);
            if ElemJ < ElemJ_1 then begin { Swap the strings }
               str := Tempsl[j];
               unt := TempU[j];
               Tempsl[j] := Tempsl[j-1];
               TempU[j] := TempU[j-1];
               Tempsl[j-1] := str;
               TempU[j-1] := unt;
            end
            else { Right index reached }
               break;
         end;
      end;
      { Then delete duplicates }
      for i:=(Tempsl.Count - 1) downto 1 do begin
         if Tempsl[i] = Tempsl[i-1] then  { Duplicate -> delete one of them }
            if TempU[i] = 'U' then begin { Untouchable -> delete element i-1 }
               TempU.Delete(i-1);
               Tempsl.Delete(i-1);
            end
            else begin
               TempU.Delete(i);
               Tempsl.Delete(i);
            end;
      end;

      { Now copy the sorted temp-list into the original }
      Discretization.Clear;
      Untouchables.Clear;
      Discretization.AddStrings(Tempsl);
      Untouchables.AddStrings(TempU);
   end;
   finally
      Tempsl.Free;
      TempU.Free;
   end;
end;

{ Updates the Discretization variable with new depths from the profile     }
{ definition, without deleting the discretization that is already defined. }
{ Should only be called if IsValid = TRUE                                  }
procedure TDiscretizationVars.Update(ProfVars: TProfileVars);
var
   i, j, imax, jmax: Integer;
   cd, sg: Double;
begin
   imax := Discretization.Count;
   jmax := ProfVars.NumSGProfileItems;
   Untouchables.Clear;

   if jmax = 0 then
      Discretization.Clear { Nothing in profile => no discretization! }
   else begin
      Untouchables.AddStrings(Discretization); { ..can't assign to Count #-(   }
      for i:=0 to Untouchables.Count - 1 do  { Old tempuntouchables now just changeable discretization depths }
         Untouchables[i] := '';

      if imax > 0 then
         cd := StrToFloat(Discretization[0])
      else { Make sure that all profile depths will be inserted }
         cd := 1.7 * 10e37; { MaxReal/10 (/10: to be secure from overflow) }
      sg := StrToFloat(ProfVars.SGProfileItems.Cells[1, 0]);

      i:= 0; j := 0;
      { CurrDiscretization is always sorted!!! (property to be used below...) }
      while (i < imax) or (j < jmax) do begin
         if i=imax then
            cd := 1.7 * 10e37;
         if cd < sg then begin
            i := i + 1;
            if i < imax then
               cd := StrToFloat(Discretization[i])
            else { After last element in CurrDiscretizatin, add rest from profile }
               cd := 1.7 * 10e37; { Never go into (if cd < sg) again }
         end
         else if cd = sg then begin
            Untouchables[i] := 'U';
            i := i + 1;
            j := j + 1;
            if i < imax then
               cd := StrToFloat(Discretization[i]);
            if j < jmax then
               sg := sg + StrToFloat(ProfVars.SGProfileItems.Cells[1, j]);
         end

         else begin { cd > sg => insert HorizonDepth into CurrDisc here }
            if i=imax then begin { Add to end of list }
               Discretization.Add( StrTo1DecPosStr(FloatToStr(sg)) );
               Untouchables.Add('U');
            end
            else begin { Insert into list and push the 'big' cd one index forwards }
               Discretization.Insert(i, StrTo1DecPosStr(FloatToStr(sg)) );
               Untouchables.Insert(i, 'U');
            end;
            i := i + 1;
            imax := imax + 1;
            j := j + 1;
            { cd still the same! }
            if j < jmax then
               sg := sg + StrToFloat(ProfVars.SGProfileItems.Cells[1, j]);
         end;
      end;
   end;  { jmax <> 0 }
end;

procedure TDiscretizationVars.Load;
begin
   { Init from file here }
end;

procedure TDiscretizationVars.Save;
begin
   { Save to file here }
end;

procedure TDiscretizationVars.Assign(src: TDiscretizationVars);
begin
   Discretization.Clear;
   Discretization.AddStrings(src.Discretization);
   UnTouchables.Clear;
   UnTouchables.AddStrings(src.UnTouchables);
end;

{ Applies a discretization using the defined depths from the profile }
{ Type of discretization (fine:0, medium:1, coarse:2) as argument    }
procedure TDiscretizationVars.Apply(disctype: Integer);
var
   idx, depth, inc, i, lasti, j, k, numhor: Integer;
   temp, lowest, hd: Double;
   HorDepth: array [0..500] of Double;  { Bad - Needs numhor elements, but how...}
   CalcDepth: array [0..1000] of Double;
begin

   numhor := TempProfileVars.SGProfileItems.RowCount;

   { Initialize arrays }
   for i:=0 to 500 do begin
      HorDepth[i] := -1;
      CalcDepth[i] := -1;
   end;

   { Read all the horizon depths - and sort them }
   hd := 0;
   for i:=0 to numhor-1 do begin
      hd := hd + StrToFloat(TempProfileVars.SGProfileItems.Cells[1,i]);
      HorDepth[i] := hd;
      for j:=i downto 1 do { Bubble-sort - OK here ;-)  }
         if HorDepth[j] < HorDepth[j-1] then begin  { Bubble it up... }
            temp := HorDepth[j-1];
            HorDepth[j-1] := HorDepth[j];
            HorDepth[j] := temp;
         end;
   end;

   { First make discretization as usual }
   idx := 0; depth := 0; inc := 0;

   { Calculate temporary discretization down to 2 times the lowest horizon }
   { Lousy way of taking into concideration that the discretizations may   }
   { shrinc according to the defined horizons.                             }
   lowest := 2 * HorDepth[numhor-1];

   while depth<=lowest do begin
      case disctype of
         0: begin
               case depth of
                  0..2     : inc :=  1;
                  3..9     : inc :=  2;
                  10..17   : inc :=  3;
                  18..35   : inc :=  5;
                  36..130  : inc := 10;
                  131..220 : inc := 20;
                  221..270 : inc := 30;
               else
                  inc := 50;
               end;
            end;
         1: begin
               case depth of
                  0..6     : inc :=  2;
                  7..17    : inc :=  3;
                  18..25   : inc :=  5;
                  26..110  : inc := 10;
                  111..220 : inc := 20;
                  221..270 : inc := 30;
               else
                  inc := 50;
               end;
            end;
         2: begin
               case depth of
                  0..25    : inc :=  5;
                  26..110  : inc := 10;
                  111..220 : inc := 20;
                  221..270 : inc := 30;
               else
                  inc := 50;
               end;
            end;
      end; { case disctype }

      depth := depth + inc;
      CalcDepth[idx] := depth;
      idx := idx + 1;
   end; { while depth<=lowest }

   { Now modify the discretization according to defined horison depths }
   i := 0; j := 0;  { Index for CalcDepths and HorDepths }
   lasti := 0;

   while HorDepth[j] > -1 do begin

      { Find the first calcdepth after the hordepth }
      while CalcDepth[i] < HorDepth[j] do
         i := i + 1;

      if CalcDepth[i] <> HorDepth[j] then begin
         if (i <> lasti) or (lasti = 0) then begin { Just to be secure from div by zero! (if two horizons have the same depth) }

            { Update all the discdepths starting after the first calcdepth }
            { after the horizon depth, and out through the rest of the calcdepths }
            k := i + 1;
            while CalcDepth[k] > -1 do begin
               CalcDepth[k] := CalcDepth[k] - (CalcDepth[i] - HorDepth[j]);
               k := k + 1;
            end;

            { Recalculate all the discdepths up (index.up) to the horizon depth (included) }
            if lasti = 0 then begin
               k := lasti;
               while k <= i do begin
                  CalcDepth[k] := CalcDepth[k] * HorDepth[j] / CalcDepth[i];
                  k := k + 1;
               end { while k <= i }
            end
            else begin
               k := lasti + 1;
               while k <= i do begin
                  CalcDepth[k] := CalcDepth[lasti] + ((CalcDepth[k] - CalcDepth[lasti])
                                                   *  (HorDepth[j] - CalcDepth[lasti]))
                                                   / (CalcDepth[i] - CalcDepth[lasti]);
                  k := k + 1;
               end; { while k <= i }
            end;
         end;  { if i <> lasti }
      end;   { if calcdepth[i] <> hordepth[j] }

      lasti := i; { upper (closer to ground surface!) bound for new horizon }
      j := j + 1; { Next horizon... }
   end;

   { CalcDepth calculated - now put into Discretization variable }
   Discretization.Clear;
   UnTouchables.Clear;
   for k:=0 to i do begin
      Discretization.Add(FormatFloat('0.0', CalcDepth[k]));
      hd := 0;
      for j:=0 to numhor-1 do begin
         hd := hd + StrToFloat(TempProfileVars.SGProfileItems.Cells[1, j]);
         if FormatFloat('0.0', hd) = Discretization[k] then begin
            UnTouchables.Add('U');
            break;
         end;
      end;
      if Discretization.Count > UnTouchables.Count then
         UnTouchables.Add('');
   end;
end;

function TDiscretizationVars.IsValid(lowest: Double): Boolean;
var
   i: Integer;
begin
   Result := TRUE;
   if Discretization.Count > 0 then begin
      for i:=0 to Discretization.Count - 1 do
         if StrToFloat(Discretization[i]) > lowest then
            Result := FALSE;
   end
   else if lowest <= 0 then
      Result := FALSE;
end;


{ IsHorsInDisc checks if all horizon depths are present in the discretization }
{ Also checks that the discretization is valid in any other way :)            }
function TDiscretizationVars.IsHorsInDisc(ProfVars: TProfileVars): Boolean;
var
   i, j, temp, accdepth: Integer;
   found: Boolean;
begin
   Result := FALSE;
   if ProfVars.NumSGProfileItems <= 0 then begin
      if Discretization.Count > 0 then
         exit; { Result = FALSE }
   end
   else begin  { ProfVars.NumSGProfileItems > 0 }

      { Do the elementary checks first... }
      if (Discretization.Count <> UnTouchables.Count)
            or (Discretization.Count < ProfVars.NumSGProfileItems) then
         exit;

      { Now check that the number of untouchables matches the number of horizons... }
      temp := ProfVars.NumSGProfileItems;
      for i:=0 to UnTouchables.Count - 1 do
         if UnTouchables[i] <> '' then { One untouchable found }
            temp := temp - 1;
      if temp <> 0 then
         exit; { Numbers didn't match :( }

      { Check that horizon depths are in discretization, }
      { and that they are the untouchable ones...        }
      { No need to be smart - just do it the hard way... }
      { (can not count on disc to be sorted!)            }
      accdepth := 0;        { one decimal precision => mult. by 10 (delphi "#¤@£2 floats!!)}
      for i:=0 to ProfVars.NumSGProfileItems - 1 do begin
         accdepth := accdepth + Round(StrToFloat(ProfVars.SGProfileItems.Cells[1,i]) * 10); { Add the current hor size }
         { look for each hordepth in the disc... }
         found := FALSE;
         for j:=0 to Discretization.Count - 1 do begin
            if accdepth = Round(StrToFloat(Discretization[j]) * 10) then
               if UnTouchables[j] <> 'U' then
                  exit { this one should have been untouchable! }
               else
                  found := TRUE;
         end;
         if not found then
            exit;
      end;

      { Check that disc does not extend below lowes hor... }
      if not IsValid(accdepth/10) then
         exit;
   end;

   Result := TRUE; { OK - great!! }
end;



{ *********************************** }
{           TStateVars                }
{ *********************************** }

constructor TStateVars.Create;
begin
//   SoilWaterContItems := TStringList.Create;
   PotentialVsDepth := TStringGrid.Create(FormGlobals);
   WaterContVsDepth := TStringGrid.Create(FormGlobals);
//   SoilTemperatureItems := TStringList.Create;
   TemperatureVsDepth := TStringGrid.Create(FormGlobals);
//   SoilNitrateContItems := TStringList.Create;
   NitrateVsDepth := TStringGrid.Create(FormGlobals);
//   SoilAmmoniaContItems := TStringList.Create;
   AmmoniaVsDepth := TStringGrid.Create(FormGlobals);
end;

destructor TStateVars.Destroy;
begin
//   SoilWaterContItems.Free;
   PotentialVsDepth.Free;
   WaterContVsDepth.Free;
//   SoilTemperatureItems.Free;
   TemperatureVsDepth.Free;
//   SoilNitrateContItems.Free;
   NitrateVsDepth.Free;
//   SoilAmmoniaContItems.Free;
   AmmoniaVsDepth.Free;
end;

procedure TStateVars.Reset;
var
   numrows, i: Integer;
begin
   SoilWaterContIdx := 1; { Default!! }
   PotentialVsDepth.RowCount := 1;
   NumPotentialVsDepthItems := 0;
   PotentialVsDepth.ColCount := 2;
   PotentialVsDepth.Rows[0].Clear;
   WaterContVsDepth.RowCount := 1;
   NumWaterContVsDepthItems := 0;
   WaterContVsDepth.ColCount := 2;
   WaterContVsDepth.Rows[0].Clear;

   SoilTemperatureIdx := 0; { Default!! }
   TemperatureVsDepth.RowCount := 1;
   NumTemperatureVsDepthItems := 0;
   TemperatureVsDepth.ColCount := 2;
   TemperatureVsDepth.Rows[0].Clear;

   SoilNitrateContIdx := -1;
   NitrateVsDepth.RowCount := 1;
   NumNitrateVsDepthItems := 0;
   NitrateVsDepth.ColCount := 2;
   NitrateVsDepth.Rows[0].Clear;
   NitrateConc := NotSet;

   SoilAmmoniaContIdx := -1;
   AmmoniaVsDepth.RowCount := 1;
   NumAmmoniaVsDepthItems := 0;
   AmmoniaVsDepth.ColCount := 2;
   AmmoniaVsDepth.Rows[0].Clear;
   AmmoniaContent := NotSet;
end;

procedure TStateVars.Load;
begin
   { Init from file here }
end;

procedure TStateVars.Save;
begin
   { Save to file here }
end;

procedure TStateVars.Assign(src: TStateVars);
var
   i, j: Integer;
begin
//   SoilWaterContItems.Clear;
//   SoilWaterContItems.AddStrings(src.SoilWaterContItems);
   SoilWaterContIdx := src.SoilWaterContIdx;

   NumPotentialVsDepthItems := src.NumPotentialVsDepthItems;
   PotentialVsDepth.RowCount := src.PotentialVsDepth.RowCount;
   PotentialVsDepth.ColCount := src.PotentialVsDepth.ColCount;
   for i := 0 to PotentialVsDepth.RowCount - 1 do
      for j := 0 to PotentialVsDepth.ColCount - 1 do
         PotentialVsDepth.Cells[j, i] := src.PotentialVsDepth.Cells[j, i];

   NumWaterContVsDepthItems := src.NumWaterContVsDepthItems;
   WaterContVsDepth.RowCount := src.WaterContVsDepth.RowCount;
   WaterContVsDepth.ColCount := src.WaterContVsDepth.ColCount;
   for i := 0 to WaterContVsDepth.RowCount - 1 do
      for j := 0 to WaterContVsDepth.ColCount - 1 do
         WaterContVsDepth.Cells[j, i] := src.WaterContVsDepth.Cells[j, i];

//   SoilTemperatureItems.Clear;
//   SoilTemperatureItems.AddStrings(src.SoilTemperatureItems);
   SoilTemperatureIdx := src.SoilTemperatureIdx;

   NumTemperatureVsDepthItems := src.NumTemperatureVsDepthItems;
   TemperatureVsDepth.RowCount := src.TemperatureVsDepth.RowCount;
   TemperatureVsDepth.ColCount := src.TemperatureVsDepth.ColCount;
   for i := 0 to TemperatureVsDepth.RowCount - 1 do
      for j := 0 to TemperatureVsDepth.ColCount - 1 do
         TemperatureVsDepth.Cells[j, i] := src.TemperatureVsDepth.Cells[j, i];

//   SoilNitrateContItems.Clear;
//   SoilNitrateContItems.AddStrings(src.SoilNitrateContItems);
   SoilNitrateContIdx := src.SoilNitrateContIdx;

   NumNitrateVsDepthItems := src.NumNitrateVsDepthItems;
   NitrateVsDepth.RowCount := src.NitrateVsDepth.RowCount;
   NitrateVsDepth.ColCount := src.NitrateVsDepth.ColCount;
   for i := 0 to NitrateVsDepth.RowCount - 1 do
      for j := 0 to NitrateVsDepth.ColCount - 1 do
         NitrateVsDepth.Cells[j, i] := src.NitrateVsDepth.Cells[j, i];
   NitrateConc := src.NitrateConc;

//   SoilAmmoniaContItems.Clear;
//   SoilAmmoniaContItems.AddStrings(src.SoilAmmoniaContItems);
   SoilAmmoniaContIdx := src.SoilAmmoniaContIdx;

   NumAmmoniaVsDepthItems := src.NumAmmoniaVsDepthItems;
   AmmoniaVsDepth.RowCount := src.AmmoniaVsDepth.RowCount;
   AmmoniaVsDepth.ColCount := src.AmmoniaVsDepth.ColCount;
   for i := 0 to AmmoniaVsDepth.RowCount - 1 do
      for j := 0 to AmmoniaVsDepth.ColCount - 1 do
         AmmoniaVsDepth.Cells[j, i] := src.AmmoniaVsDepth.Cells[j, i];
   AmmoniaContent := src.AmmoniaContent;
end;



{ *********************************** }
{           TGlobalInfoVars           }
{ *********************************** }

constructor TGlobalInfoVars.Create;
begin
   Dirty := False;
   CurrentSaveLib := -1;  { -1: Not saved yet, 1: User Ref, 3: User Templ }
end;

destructor TGlobalInfoVars.Destroy;
begin
end;

procedure TGlobalInfoVars.Reset;
begin
(*
   GlobalInfoHeading := '';
*)
   Latitude := NotSet;
   Elevation := NotSet;
   UTM1 := NotSet;
   UTM2 := NotSet;
   AvAnnTemp := NotSet;
   AvAnnTempAmp := NotSet;
   DateMaxTemp := '';
   NH4ConcPrec := NotSet;
   NH4DryDep := NotSet;
   NO3ConcPrec := NotSet;
   NO3DryDep := NotSet;
   MeteorologicalDataFile := '';

   Dirty := FALSE;
   CurrentSaveLib := -1;  { -1: Not saved yet, 1: User Ref, 3: User Templ }
end;

procedure TGlobalInfoVars.LoadDefault;
begin
   Load('mini', 0); { Loads default values from mini-object }
end;

function TGlobalInfoVars.Load(GlobInfoName: string; lib: Integer): Boolean;
   // Convert alist date format to user-interface  format
   function max_Ta_yday2DateMaxTemp(d: Double): String;
   var
      Month, Days: Integer;
      StrMonth, StrDays: String;
   begin
      Days := Trunc(d);
      Month := Days2Month(Days);
      StrMonth := IntToStr(Month);
      if Length(StrMonth) = 1 then
         StrMonth := '0' + StrMonth;
      StrDays := IntToStr(Jdays2Days(Days));
      if Length(StrDays) = 1 then
         StrDays := '0' + StrDays;
      Result := StrMonth + StrDays;
      if Length(Result) <> 4 then  // Respect invariant
         Result := '';
   end;

var
   GlobInfo, DryDeposit, WetDeposit: daisy_alist;
   s: String;
   tempdouble: Double;
begin
   Result := TRUE; { be optimistic }

   { First open daisy library and get alist... }
   GlobInfo := GetLibraryObject('weather', GlobInfoName);
   if GlobInfo = nil then begin
      Result := FALSE;
      exit;
   end;

   { ...then initialize vars from alist... }

(*
   { Heading should come as argument to load() if loaded from lib }
   if lib = 0 then
      GlobalInfoHeading := ''
   else
      GlobalInfoHeading := GlobInfoName;
*)

   // First reset all variables to 'NotSet'
   Reset;
   // Then get data, if there are any...
   GetVariable(GlobInfo, 'Latitude', daisy_type_number, @Latitude);
   GetVariable(GlobInfo, 'Elevation', daisy_type_number, @Elevation);
   GetVariable(GlobInfo, 'UTM_x', daisy_type_number, @UTM1);
   GetVariable(GlobInfo, 'UTM_y', daisy_type_number, @UTM2);
   GetVariable(GlobInfo, 'average', daisy_type_number, @AvAnnTemp);
   GetVariable(GlobInfo, 'amplitude', daisy_type_number, @AvAnnTempAmp);
   if GetVariable(GlobInfo, 'max_Ta_yday', daisy_type_number, @tempdouble) then
      DateMaxTemp := max_Ta_yday2DateMaxTemp(tempdouble);
   if GetVariable(GlobInfo, 'DryDeposit', daisy_type_alist, @DryDeposit) then begin
      GetVariable(DryDeposit, 'NO3', daisy_type_number, @NO3DryDep);
      GetVariable(DryDeposit, 'NH4', daisy_type_number, @NH4DryDep);
   end;
   if GetVariable(GlobInfo, 'WetDeposit', daisy_type_alist, @WetDeposit) then begin
      GetVariable(WetDeposit, 'NO3', daisy_type_number, @NO3ConcPrec);
      GetVariable(WetDeposit, 'NH4', daisy_type_number, @NH4ConcPrec);
   end;
   GetVariable(GlobInfo, 'file', daisy_type_string, @MeteorologicalDataFile);
end;


{ Saves global info vars to lib (0: User def, 1: sim spec) }
function TGlobalInfoVars.Save(GlobInfoName: string; lib: Integer; OverWrt: Boolean): Boolean;
   // Convert user-interface date format to alist format
   function DateMaxTemp2max_Ta_yday(s: String): Double;
   var
      pkt, l: Integer;
   begin
      l := length(s);
      if l <> 4 then
         Result := 0
      else
         Result := mmdd2days(StrToInt(Copy(s, 1, 2))
                             , StrToInt(Copy(s, 3, 2)));
   end;
var
   Temp_String, SelLibStr, SaveToLib, ExistInLib: string;
   i: Integer;
   s: String;
   tempdouble: Double;
   GlobInfo, DryDeposit, WetDeposit: daisy_alist;
   GlobInfoSyntax : daisy_syntax;
begin
   { Save to GLOBAL INFO LIBRARY here... }
   Result := TRUE; { be optimistic }

   { Create _daisy_alist... }
   GlobInfo := CreateWeatherAlist;
   DryDeposit := _daisy_alist_create;
   WetDeposit := _daisy_alist_create;
   if (GlobInfo = nil) or (DryDeposit = nil) or (WetDeposit = nil) then begin
      result := FALSE;
      exit;
   end;

   { ...now input parameters into alist... }
(*
 * Heading should come as argument to save() when saved to library

         GlobalInfoHeading: String; { Global Info identifier }
*)

   if Latitude <> NotSet then
      SetVariable(GlobInfo, 'Latitude', daisy_type_number, @Latitude);
   if Elevation <> NotSet then
      SetVariable(GlobInfo, 'Elevation', daisy_type_number, @Elevation);
   if UTM1 <> NotSet then
      SetVariable(GlobInfo, 'UTM_x', daisy_type_number, @UTM1);
   if UTM2 <> NotSet then
      SetVariable(GlobInfo, 'UTM_y', daisy_type_number, @UTM2);
   if AvAnnTemp <> NotSet then
      SetVariable(GlobInfo, 'average', daisy_type_number, @AvAnnTemp);
   if AvAnnTempAmp <> NotSet then
      SetVariable(GlobInfo, 'amplitude', daisy_type_number, @AvAnnTempAmp);
   if DateMaxTemp <> '' then begin
      tempdouble := DateMaxTemp2max_Ta_yday(DateMaxTemp);
      SetVariable(GlobInfo, 'max_Ta_yday', daisy_type_number, @tempdouble);
   end;

   if NO3DryDep <> NotSet then
      SetVariable(DryDeposit, 'NO3', daisy_type_number, @NO3DryDep);
   if NH4DryDep <> NotSet then
      SetVariable(DryDeposit, 'NH4', daisy_type_number, @NH4DryDep);
   if (NO3DryDep <> NotSet) or (NH4DryDep <> NotSet) then
      SetVariable(GlobInfo, 'DryDeposit', daisy_type_alist, @DryDeposit);

   if NO3ConcPrec <> NotSet then
      SetVariable(WetDeposit, 'NO3', daisy_type_number, @NO3ConcPrec);
   if NH4ConcPrec <> NotSet then
      SetVariable(WetDeposit, 'NH4', daisy_type_number, @NH4ConcPrec);
   if (NO3DryDep <> NotSet) or (NH4DryDep <> NotSet) then
      SetVariable(GlobInfo, 'WetDeposit', daisy_type_alist, @WetDeposit);

   if MeteorologicalDataFile <> '' then
      SetVariable(GlobInfo, 'file', daisy_type_string, @MeteorologicalDataFile);

{ ...last save the alist to library... }
   case lib of
      0: begin { User defined template- or reference library }
               { , depending on CurrentSaveLib               }
            case CurrentSaveLib of
               1: begin
                     SelLibStr := MainFormVars.UserRefLib;    { User Ref }
                     SaveToLib := LoadStr(RES_MSC_UsrRef);
                  end;
               3: begin
                     SelLibStr := MainFormVars.UserTemplLib;  { User Templ }
                     SaveToLib := LoadStr(RES_MSC_UsrTempl);
                  end;
            else
               CurrentSaveLib := -1;  // Just to be safe...
               Result := False;
               exit;
            end;

            i := UsedInLib(GlobInfoName, 'weather');
            if i = 0 then begin
               SaveInLibrary('weather', GlobInfo, GlobInfoName, SelLibStr);
               { Commit the changes in CurrentSaveLib }
               SaveDaisyLibrary(SelLibStr, SelLibStr, nil);
               MainFormVars.UpdateLib(CurrentSaveLib, 1);
            end
            else if (i = 4) or (i = 2) then begin
               if (i = 2) then
                  ExistInLib := LoadStr(RES_MSC_UsrRef)
               else // (i=4)
                  ExistInLib := LoadStr(RES_MSC_UsrTempl);
               if not OverWrt then
                  if MessageDlg(LoadStr(RES_MSG_GlobInfo) + ' ' + GlobInfoName
                                 + ' allready exists in the ' + ExistInLib + '.'
                                 + CHR(10) + CHR(13) + CHR(13)
                                 + 'Continue saving to ' + SaveToLib + ' and overwrite?',
                                 mtWarning, [mbYes, mbNo], 0) = mrYes then
                     OverWrt := TRUE;
               if OverWrt then begin
                  DeleteLibraryObject('weather', GlobInfoName); { better safe than sorry ;-) }
                  SaveInLibrary('weather', GlobInfo, GlobInfoName, SelLibStr);
                  { Commit the changes in CurrentSaveLib }
                  SaveDaisyLibrary(SelLibStr, SelLibStr, nil);
                  MainFormVars.UpdateLib(CurrentSaveLib, 1);
               end
               else begin
                  result := FALSE;
                  exit; { quit saving! }
               end;
            end
            else begin { i <> 0 and i <> 2 and i <> 4 }
               case i of
                  1: s := LoadStr(RES_MSC_StdRef);
//                  2: s := LoadStr(RES_MSC_UsrRef);
                  3: s := LoadStr(RES_MSC_StdTempl);
                  5: s := LoadStr(RES_MSC_SimSpec);
               else
                  s := 'Unknown BUG Library';
               end;
               MessageDlg(LoadStr(RES_MSG_GlobInfo) + ' ' + GlobInfoName + ' '
                           + LoadStr(RES_MSG_Exist_in_Lib2) + ' ' + s
                           + LoadStr(RES_MSG_Exist_in_Lib3)
                           , mtInformation, [mbOK], 0);
               result := FALSE;
               exit; { quit saving! }
            end;
         end;
      1: begin { Simulation specific library }
            i := UsedInLib(GlobInfoName, 'weather');
            if i = 0 then begin
               SaveInLibrary('weather', GlobInfo, GlobInfoName, MainFormVars.SimSpecLib);
               // { Commit the changes in Simulation specific library}
               // SaveDaisyLibrary(MainFormVars.SimSpecLib,MainFormVars.SimSpecLib,nil);
               MainFormVars.UpdateLib(4, 1);
            end
            else if i = 5 then begin
               if not OverWrt then
                  if MessageDlg(LoadStr(RES_MSG_GlobInfo) + ' ' + GlobInfoName + ' '
                                 + LoadStr(RES_ERR_Globals_Exist_in_SimSpec2)
                                 + CHR(10) + CHR(13) + CHR(13)
                                 + LoadStr(RES_MSG_Overwrite)
                                 , mtWarning, [mbYes, mbNo], 0) = mrYes then
                     OverWrt := TRUE;
               if OverWrt then begin
                  DeleteLibraryObject('weather', GlobInfoName); { better safe than sorry ;-) }
                  SaveInLibrary('weather', GlobInfo, GlobInfoName, MainFormVars.SimSpecLib);
                  // { Commit the changes in simulation specific library }
                  // SaveDaisyLibrary(MainFormVars.SimSpecLib,MainFormVars.SimSpecLib,nil);
                  MainFormVars.UpdateLib(4, 1);
               end
               else begin
                  result := FALSE;
                  exit; { quit saving! }
               end;
            end
            else begin { i <> 0 and i <> 5 }
               case i of
                  1: s := LoadStr(RES_MSC_StdRef);
                  2: s := LoadStr(RES_MSC_UsrRef);
                  3: s := LoadStr(RES_MSC_StdTempl);
                  4: s := LoadStr(RES_MSC_UsrTempl);
               else
                  s := 'Unknown BUG Library';
               end;
               MessageDlg(LoadStr(RES_MSG_GlobInfo) + ' ' + GlobInfoName + ' '
                           + LoadStr(RES_MSG_Exist_in_Lib2) + ' ' + s + ' '
                           + LoadStr(RES_MSG_Exist_in_Lib2) + CHR(10) + CHR(13)
                           + LoadStr(RES_ERR_Globals_GlobInfo_Enter_Diff_Heading)
                           , mtInformation, [mbOK], 0);
               result := FALSE;
               exit; { quit saving! }
            end;
         end;
   end;
end;

procedure TGlobalInfoVars.Assign(src: TGlobalInfoVars);
begin
   GlobalInfoHeading := src.GlobalInfoHeading;
   Latitude := src.Latitude;
   Elevation := src.Elevation;
   UTM1 := src.UTM1;
   UTM2 := src.UTM2;
   AvAnnTemp := src.AvAnnTemp;
   AvAnnTempAmp := src.AvAnnTempAmp;
   DateMaxTemp := src.DateMaxTemp;
   NH4ConcPrec := src.NH4ConcPrec;
   NH4DryDep := src.NH4DryDep;
   NO3ConcPrec := src.NO3ConcPrec;
   NO3DryDep := src.NO3DryDep;
   MeteorologicalDataFile := src.MeteorologicalDataFile;
end;



{ *********************************** }
{           TMainFormVars             }
{ *********************************** }

constructor TMainFormVars.Create;
begin
   StdRefLibManItems := TStringList.Create;
   StdRefLibGlobItems := TStringList.Create;
   StdRefLibProfItems := TStringList.Create;
   StdRefLibHorItems := TStringList.Create;
   (*JJ:301298*)
   StdRefLibOutputItems := TStringList.Create;
   (*JJ:301298*)
   UserRefLibManItems := TStringList.Create;
   UserRefLibGlobItems := TStringList.Create;
   UserRefLibProfItems := TStringList.Create;
   UserRefLibHorItems := TStringList.Create;
   StdTemplLibManItems := TStringList.Create;
   StdTemplLibGlobItems := TStringList.Create;
   StdTemplLibProfItems := TStringList.Create;
   StdTemplLibHorItems := TStringList.Create;
   UserTemplLibManItems := TStringList.Create;
   UserTemplLibGlobItems := TStringList.Create;
   UserTemplLibProfItems := TStringList.Create;
   UserTemplLibHorItems := TStringList.Create;
   SimSpecLibManItems := TStringList.Create;
   SimSpecLibGlobItems := TStringList.Create;
   SimSpecLibProfItems := TStringList.Create;
   SimSpecLibHorItems := TStringList.Create;

(*
   ProfVars := TProfileVars.Create;
   GlobInfoVars := TGlobalInfoVars.Create;
   ManVars := TManagerVars.Create;
*)
   (*JJ:301298*)
   SelOutput := TStringList.Create;
   SimulationStartTime := nil;
   bStartTimeIsOurs    := false;
   (*JJ:301298*)
end;

destructor TMainFormVars.Destroy;
begin
   StdRefLibManItems.Free;
   StdRefLibGlobItems.Free;
   StdRefLibProfItems.Free;
   StdRefLibHorItems.Free;
   (*JJ:301298*)
   StdRefLibOutputItems.Free;
   (*JJ:301298*)
   UserRefLibManItems.Free;
   UserRefLibGlobItems.Free;
   UserRefLibProfItems.Free;
   UserRefLibHorItems.Free;
   StdTemplLibManItems.Free;
   StdTemplLibGlobItems.Free;
   StdTemplLibProfItems.Free;
   StdTemplLibHorItems.Free;
   UserTemplLibManItems.Free;
   UserTemplLibGlobItems.Free;
   UserTemplLibProfItems.Free;
   UserTemplLibHorItems.Free;
   SimSpecLibManItems.Free;
   SimSpecLibGlobItems.Free;
   SimSpecLibProfItems.Free;
   SimSpecLibHorItems.Free;
(*
   ProfVars.Destroy;
   GlobInfoVars.Destroy;
   ManVars.Destroy;
*)
  (*JJ:301298*)
  SelOutput.Free;
  (*JJ:301298*)

end;

function TMainFormVars.CreateAlistFromVars : daisy_alist;
var Simulation    : daisy_alist;
    Komponent     : daisy_alist;
    KomponentText : String;
    TempSimulationStartTime : daisy_time;
    bIsOurs       : boolean;
    i             : Integer;
begin
   Simulation := _daisy_alist_create;
   if Simulation = nil then begin
      result := nil;
      exit;
   end;

   KomponentText := SelMan;
   if KomponentText <> '' then begin
      Komponent := _daisy_alist_create;
      if Komponent = nil then begin
         _daisy_alist_delete(Simulation);
         result := nil;
         exit;
      end;
      SetVariable(Komponent,'type', daisy_type_string, @KomponentText);
      SetVariable(Simulation, 'manager', daisy_type_alist, @Komponent);
      TempSimulationStartTime := GetManagementStartTime(KomponentText,bIsOurs);
      if not bIsOurs then begin
         TempSimulationStartTime := SimulationStartTime;
      end;
      if TempSimulationStartTime <> nil then begin
   //    SetVariable(Simulation, 'start', daisy_type_time, @SimulationStartTime);
         SetVariable(Simulation, 'time', daisy_type_time, @TempSimulationStartTime);
      end;
   end;

   KomponentText := SelGlob;
   if KomponentText <> '' then begin
      Komponent := _daisy_alist_create;
      if Komponent = nil then begin
         _daisy_alist_delete(Simulation);
         result := nil;
         exit;
      end;
      SetVariable(Komponent,'type', daisy_type_string, @KomponentText);
      SetVariable(Simulation, 'weather', daisy_type_alist, @Komponent);
   end;
   KomponentText := SelProf;
   if KomponentText <> '' then begin
      Komponent := _daisy_alist_create;
      if Komponent = nil then begin
         _daisy_alist_delete(Simulation);
         result := nil;
         exit;
      end;
      SetVariable(Komponent,'type', daisy_type_string, @KomponentText);
      SetVariableAt(Simulation, 'column',0, daisy_type_alist, @Komponent);
   end;
   KomponentText := SimulationHeading;
   if KomponentText <> '' then begin
      SetVariable(Simulation, 'description', daisy_type_string, @KomponentText);
   end;
   (* JJ:301298 *)
   for i:= 0 to SelOutput.Count-1 do begin
      Komponent := _daisy_alist_create;
      if Komponent = nil then begin
         _daisy_alist_delete(Simulation);
         result := nil;
         exit;
      end;
      KomponentText := SelOutput.Strings[i];
      SetVariable(Komponent,'type', daisy_type_string, @KomponentText);
      SetVariableAt(Simulation, 'output',i, daisy_type_alist, @Komponent);
   end;
   (* JJ:301298 *)
   result := Simulation;
end;

function TMainFormVars.CreateVarsFromAlist(al:daisy_alist) : boolean;
var Komponent : daisy_alist;
    KomponentText : String;
    StringValue   : String;
    KomponentText2 : String;
    i              : integer;
begin
   result := false;
   if al = nil then
      exit;
   KomponentText := 'manager';
   if GetVariable(al,KomponentText,daisy_type_alist,@Komponent) then begin
      KomponentText := 'type';
      if GetVariable(Komponent,KomponentText,daisy_type_string,@StringValue) then begin
         SelMan := StringValue;
         if FromLibrary(SelMan,'action',StdRefLib) then
            SelManType :=  0
         else if FromLibrary(SelMan,'action',UserRefLib)then
            SelManType :=  1
         else if FromLibrary(SelMan,'action',SimSpecLib)then
            SelManType :=  2
      end;
   end;


   KomponentText := 'column';
   if GetVariableAt(al,KomponentText,0,daisy_type_alist,@Komponent) then begin
      KomponentText := 'type';
      if GetVariable(Komponent,KomponentText,daisy_type_string,@StringValue) then begin
         SelProf := StringValue;
         if FromLibrary(SelProf,'column',StdRefLib)then
            SelProfType :=  0
         else if FromLibrary(SelProf,'column',UserRefLib)then
            SelProfType :=  1
         else if FromLibrary(SelProf,'column',SimSpecLib)then
            SelProfType :=  2
      end;
   end;


   KomponentText := 'weather';
   if GetVariable(al,KomponentText,daisy_type_alist,@Komponent) then begin
      KomponentText := 'type';
      if GetVariable(Komponent,KomponentText,daisy_type_string,@StringValue) then begin
         SelGlob := StringValue;
         if FromLibrary(SelGlob,'weather',StdRefLib)then
            SelGlobType :=  0
         else if FromLibrary(SelGlob,'weather',UserRefLib)then
            SelGlobType :=  1
         else if FromLibrary(SelGlob,'weather',SimSpecLib)then
            SelGlobType :=  2
      end;
   end;
   KomponentText := 'description';
   if GetVariable(al,KomponentText,daisy_type_string,@StringValue) then begin
      SimulationHeading := StringValue;
   end;
   (*JJ:301298*)
   KomponentText := 'time';
   GetVariable(al,KomponentText,daisy_type_time, @SimulationStartTime);
   bStartTimeIsOurs := false;
   (*Output*)
   KomponentText := 'output';
   KomponentText2 := 'type';
   SelOutput.Clear;
   i := 0;
   while GetVariableAt(al,KomponentText,i,daisy_type_alist,@Komponent) do begin
      if GetVariable(Komponent,KomponentText2,daisy_type_string,@StringValue) then begin
         if FromLibrary(StringValue,'log',StdRefLib) then
            SelOutput.Add(StringValue);
      end;
      i := i + 1;
   end;
   (*JJ:301298*)

end;


procedure TMainFormVars.Reset;
begin
   CurrSimulationFile := 'Untitled Simulation';

   SimSpecLib := 'FnidderFnadderFnudder$x$y$z';
   ExternalSimSpecLib := '';
(*
   StdRefLibManItems.Clear;
   StdRefLibGlobItems.Clear;
   StdRefLibProfItems.Clear;
   UserRefLibManItems.Clear;
   UserRefLibGlobItems.Clear;
   UserRefLibProfItems.Clear;
   StdTemplLibManItems.Clear;
   StdTemplLibGlobItems.Clear;
   StdTemplLibProfItems.Clear;
   UserTemplLibManItems.Clear;
   UserTemplLibGlobItems.Clear;
   UserTemplLibProfItems.Clear;
   SimSpecLibManItems.Clear;
   SimSpecLibGlobItems.Clear;
   SimSpecLibProfItems.Clear;
   SimSpecLibHorItems.Clear;
 *)
   SimulationHeading := '';
   InitializationIdx := 0; { Default value }
   RGSelLibIdx := 2;       { Default value }
   RGModuleIdx := -1;
   SelManType := -1;
   SelGlobType := -1;
   SelProfType := -1;
   SelMan := '';
   SelGlob := '';
   SelProf := '';
(*
   ProfVars.Reset;
   GlobInfoVars.Reset;
   ManVars.Reset;
*)
   (*JJ:301298*)
   if (SimulationStartTime <> nil) and bStartTimeIsOurs then
      _daisy_time_delete(SimulationStartTime);
   bStartTimeIsOurs := false;
   SimulationStartTime := nil;
   SelOutput.Clear;
   (*JJ:301298*)
   Dirty := FALSE;
end;

procedure TMainFormVars.Load;
begin
   { Init from file here }


end;

procedure TMainFormVars.Save;
begin
   { Save to file here }


end;

procedure TMainFormVars.Assign(src: TMainFormVars);
begin
   CurrSimulationFile := src.CurrSimulationFile;

   StdRefLib := src.StdRefLib;
   UserRefLib := src.UserRefLib;
   StdTemplLib := src.StdTemplLib;
   UserTemplLib := src.UserTemplLib;
   StdRefLibManItems.Clear;
   StdRefLibManItems.AddStrings(src.StdRefLibManItems);
   StdRefLibGlobItems.Clear;
   StdRefLibGlobItems.AddStrings(src.StdRefLibGlobItems);
   StdRefLibProfItems.Clear;
   StdRefLibProfItems.AddStrings(src.StdRefLibProfItems);
   StdRefLibHorItems.Clear;
   StdRefLibHorItems.AddStrings(src.StdRefLibHorItems);
   (*JJ:301298*)
   StdRefLibOutputItems.Clear;
   StdRefLibOutputItems.AddStrings(src.StdRefLibOutputItems);
   (*JJ:301298*)
   UserRefLibManItems.Clear;
   UserRefLibManItems.AddStrings(src.UserRefLibManItems);
   UserRefLibGlobItems.Clear;
   UserRefLibGlobItems.AddStrings(src.UserRefLibGlobItems);
   UserRefLibProfItems.Clear;
   UserRefLibProfItems.AddStrings(src.UserRefLibProfItems);
   UserRefLibHorItems.Clear;
   UserRefLibHorItems.AddStrings(src.UserRefLibHorItems);
   StdTemplLibManItems.Clear;
   StdTemplLibManItems.AddStrings(src.StdTemplLibManItems);
   StdTemplLibGlobItems.Clear;
   StdTemplLibGlobItems.AddStrings(src.StdTemplLibGlobItems);
   StdTemplLibProfItems.Clear;
   StdTemplLibProfItems.AddStrings(src.StdTemplLibProfItems);
   StdTemplLibHorItems.Clear;
   StdTemplLibHorItems.AddStrings(src.StdTemplLibHorItems);
   UserTemplLibManItems.Clear;
   UserTemplLibManItems.AddStrings(src.UserTemplLibManItems);
   UserTemplLibGlobItems.Clear;
   UserTemplLibGlobItems.AddStrings(src.UserTemplLibGlobItems);
   UserTemplLibProfItems.Clear;
   UserTemplLibProfItems.AddStrings(src.UserTemplLibProfItems);
   UserTemplLibHorItems.Clear;
   UserTemplLibHorItems.AddStrings(src.UserTemplLibHorItems);
   SimSpecLibManItems.Clear;
   SimSpecLibManItems.AddStrings(src.SimSpecLibManItems);
   SimSpecLibGlobItems.Clear;
   SimSpecLibGlobItems.AddStrings(src.SimSpecLibGlobItems);
   SimSpecLibProfItems.Clear;
   SimSpecLibProfItems.AddStrings(src.SimSpecLibProfItems);
   SimSpecLibHorItems.Clear;
   SimSpecLibHorItems.AddStrings(src.SimSpecLibHorItems);

   SimulationHeading := src.SimulationHeading;
   InitializationIdx := src.InitializationIdx;
   RGSelLibIdx := src.RGSelLibIdx;
   RGModuleIdx := src.RGModuleIdx;
   SelManType := src.SelManType;
   SelGlobType := src.SelGlobType;
   SelProfType := src.SelProfType;
   SelMan := src.SelMan;
   SelGlob := src.SelGlob;
   SelProf := src.SelProf;
(*
   ProfVars.Assign(src.ProfVars);
   GlobInfoVars.Assign(src.GlobInfoVars);
   ManVars.Assign(src.ManVars);
*)
   (*JJ:301298*)
   if (SimulationStartTime <> nil) and bStartTimeIsOurs then
      _daisy_time_delete(SimulationStartTime);
   bStartTimeIsOurs := src.bStartTimeIsOurs;
   src.bStartTimeIsOurs := false;
   SimulationStartTime := src.SimulationStartTime;
   SelOutput.Clear;
   SelOutput.AddStrings(src.SelOutput);
   (*JJ:301298*)

end;

procedure TMainFormVars.UpdateLib(lib, mdl: Integer);
begin
   { Get library paths from StdRefLib, StdTemplLib, UserRefLib, UserTemplLib and SimSpecLib }

   case lib of
      0: { Standard reference }
         {JJ: Added update of output - not found a reasonable place to put i yet}
         begin
         StdRefLibOutputItems.Clear;
         EnumerateObjects(StdRefLibOutputItems,'log',StdRefLib);
         case mdl of
            0: begin { Manager }
                  StdRefLibManItems.Clear;
                  { Add strings (headings) from StdRefLib in daisydll... }
                  EnumerateObjects(StdRefLibManItems,'action',StdRefLib);
               end;
            1: begin { Global Information }
                  StdRefLibGlobItems.Clear;
                  { Add strings (headings) from StdRefLib in daisydll... }
                  EnumerateObjects(StdRefLibGlobItems,'weather',StdRefLib);
               end;
            2: begin { Profile }
                  StdRefLibProfItems.Clear;
                  { Add strings (headings) from StdRefLib in daisydll... }
                  EnumerateObjects(StdRefLibProfItems,'column',StdRefLib);
               end;
            3: begin { Horizon }
                  StdRefLibHorItems.Clear;
                  { Add strings (headings) from StdRefLib in daisydll... }
                  EnumerateObjects(StdRefLibHorItems,'horizon',StdRefLib);
               end;
         end;
         end;
      1: { User def reference }
         case mdl of
            0: begin { Manager }
                  UserRefLibManItems.Clear;
                  { Add strings (headings) from StdTemplLib in daisydll }
                  EnumerateObjects(UserRefLibManItems,'action',UserRefLib);
               end;
            1: begin { Global Information }
                  UserRefLibGlobItems.Clear;
                  { Add strings (headings) from StdTemplLib in daisydll }
                  EnumerateObjects(UserRefLibGlobItems,'weather',UserRefLib);
               end;
            2: begin { Profile }
                  UserRefLibProfItems.Clear;
                  { Add strings (headings) from StdTemplLib in daisydll }
                  EnumerateObjects(UserRefLibProfItems,'column',UserRefLib);
               end;
            3: begin { Horizon }
                  UserRefLibHorItems.Clear;
                  { Add strings (headings) from StdRefLib in daisydll... }
                  EnumerateObjects(UserRefLibHorItems,'horizon',UserRefLib);
               end;
         end;
      2: { Standard template }
         case mdl of
            0: begin { Manager }
                  StdTemplLibManItems.Clear;
                  { Add strings (headings) from UserRefLib in daisydll }
                  EnumerateObjects(StdTemplLibManItems,'action',StdTemplLib);
               end;
            1: begin { Global Information }
                  StdTemplLibGlobItems.Clear;
                  { Add strings (headings) from UserRefLib in daisydll }
                  EnumerateObjects(StdTemplLibGlobItems,'weather',StdTemplLib);
               end;
            2: begin { Profile }
                  StdTemplLibProfItems.Clear;
                  { Add strings (headings) from UserRefLib in daisydll }
                  EnumerateObjects(StdTemplLibProfItems,'column',StdTemplLib);
               end;
            3: begin { Horizon }
                  StdTemplLibHorItems.Clear;
                  { Add strings (headings) from StdRefLib in daisydll... }
                  EnumerateObjects(StdTemplLibHorItems,'horizon',StdTemplLib);
               end;
         end;
      3: { User def template }
         case mdl of
            0: begin { Manager }
                  UserTemplLibManItems.Clear;
                  { Add strings (headings) from UserTemplLib in daisydll }
                  EnumerateObjects(UserTemplLibManItems,'action',UserTemplLib);
               end;
            1: begin { Global Information }
                  UserTemplLibGlobItems.Clear;
                  { Add strings (headings) from UserTemplLib in daisydll }
                  EnumerateObjects(UserTemplLibGlobItems,'weather',UserTemplLib);
               end;
            2: begin { Profile }
                  UserTemplLibProfItems.Clear;
                  { Add strings (headings) from UserTemplLib in daisydll }
                  EnumerateObjects(UserTemplLibProfItems,'column',UserTemplLib);
               end;
            3: begin { Horizon }
                  UserTemplLibHorItems.Clear;
                  { Add strings (headings) from StdRefLib in daisydll... }
                  EnumerateObjects(UserTemplLibHorItems,'horizon',UserTemplLib);
               end;
         end;
      4: { Simulation Specific }
         case mdl of
            0: begin { Manager }
                  SimSpecLibManItems.Clear;
                  { Add strings (headings) from SimSpecLib in daisydll... }
                  EnumerateObjects(SimSpecLibManItems,'action',SimSpecLib);
               end;
            1: begin { Global Information }
                  SimSpecLibGlobItems.Clear;
                  { Add strings (headings) from SimSpecLib in daisydll... }
                  EnumerateObjects(SimSpecLibGlobItems,'weather',SimSpecLib);
               end;
            2: begin { Profile }
                  SimSpecLibProfItems.Clear;
                  { Add strings (headings) from SimSpecLib in daisydll... }
                  EnumerateObjects(SimSpecLibProfItems,'column',SimSpecLib);
               end;
            3: begin { Horizon }
                  SimSpecLibHorItems.Clear;
                  { Add strings (headings) from SimSpecLib in daisydll... }
                  EnumerateObjects(SimSpecLibHorItems,'horizon',SimSpecLib);
               end;
         end;
   end;
end;

{ *********************************** }
{           TManagerVars              }
{ *********************************** }

constructor TManagerVars.Create;
begin
   ManagerHeading := '';
   TheAlist       := nil;
   TheAlist_CanDelete := False;
   Dirty := False;
   CurrentSaveLib := -1;  { -1: Not saved yet, 1: User Ref, 3: User Templ }
end;

destructor TManagerVars.Destroy;
begin
end;

procedure TManagerVars.Reset;
begin
   ManagerHeading := '';
   TheAlist       := nil;
   TheAlist_CanDelete := False;
   Dirty := False;
   CurrentSaveLib := -1;  { -1: Not saved yet, 1: User Ref, 3: User Templ }
end;

procedure TManagerVars.Load;
{Should be "Create from alist"}
begin
   { Init from file here }
end;

procedure TManagerVars.Save;
{Should be "Make alist from vars"}
begin
   { Save to file here }
end;

procedure TManagerVars.Assign(src: TManagerVars);
begin
   ManagerHeading := src.ManagerHeading;
end;






{ *********************************** }
{        Auxilliary functions         }
{ *********************************** }

function StrTo3DecimalStr(s: string): string;
var
   f: double;
begin
   f := StrToFloat(s);
   Result := FormatFloat('0.000', f);
end;

function StrTo1DecPosStr(s: string): string;
var
   f: double;
begin
   f := StrToFloat(s);
   if f < 0 then
      raise ENotValidFloat.Create(LoadStr(RES_ERR_Globals_Num_Must_Be_Pos));
   Result := FormatFloat('0.0', f);
end;

function StrTo1DecPosNotZeroStr(s: string): string;
var
   f: double;
begin
   f := StrToFloat(s);
   if f <= 0 then
      raise ENotValidFloat.Create(LoadStr(RES_ERR_Globals_Num_Must_Be_Pos_Or_Zero));
   Result := FormatFloat('0.0', f);
end;

function StrTo3DecPosStr(s: string): string;
var
   f: double;
begin
   f := StrToFloat(s);
   if f < 0 then
      raise ENotValidFloat.Create(LoadStr(RES_ERR_Globals_Num_Must_Be_Pos));
   Result := FormatFloat('0.000', f);
end;

function StrToPosFloatStr(s: string): string;
var
   f: double;
begin
   f := StrToFloat(s);
   if f < 0 then
      raise ENotValidFloat.Create(LoadStr(RES_ERR_Globals_Num_Must_Be_Pos));
   Result := s;
end;

function StrToPosNotZeroFloatStr(s: string): string;
var
   f: double;
begin
   f := StrToFloat(s);
   if f <= 0 then
      raise ENotValidFloat.Create(LoadStr(RES_ERR_Globals_Num_Must_Be_Pos_Or_Zero));
   Result := s;
end;

function StrTo3DecFracStr(s: string): string;
var
   f: double;
begin
   f := StrToFloat(s); // Raises EConvertError if not valid float
   // Now remember the 'pentium numbers' returned by StrToFloat!
   if (f < 0.0) or (f > 1.0001) then
      raise ENotValidFraction.Create(LoadStr(RES_ERR_Globals_Fraction_Valid_Interval));
   Result := FormatFloat('0.000', f);
end;

function CharToUpper(ch: Char): Char;
var
   c: Integer;
begin
   c := Ord(ch);
   if (c > 96) and (c < 123) then
      Result := Chr(c - 32)
   else
      Result := ch;
end;

{ Returns nmber of days in a month }
function Month2Days(m: Integer): Integer;
begin
   case m of
      1, 3, 5, 7, 8, 10, 12 : Result := 31;
      2                     : Result := 28;
      4, 6, 9, 11           : Result := 30;
   else
      Result := 0; { error - unnormal year!! }
   end;
end;

{ Converts month number and days to julian days }
function mmdd2days(mm, dd: Integer): Integer;
var
   i: Integer;
begin
   Result := 0;
   for i:=1 to mm-1 do
      Result := Result + Month2Days(i);
   Result := Result + dd;
//   if dd > Months2Days(mm) then Result := 0; { not that many days in this month!! }
end;

{ Converts julian days to days in the last month (jdays mod months) (ex. jd=41 gives 10)}
function jdays2days(jd: Integer): Integer;
begin
   Result := jd - mmdd2days(Days2Month(jd), 0);
end;

{ Returns the month containing the day (ex. d=41 gives 2)}
function Days2Month(d: Integer): Integer;
var
   i, daysused: Integer;
begin
   if d = 0 then d := 1  // Day 0 not valid!
   else if d < 0 then d := -d;  // Negative days => convert to positive!
   d := ((d-1) mod 365) + 1;  // Stay within a year (day 1 through day 365)!
   daysused := 0;
   Result := 12;
   for i:=1 to 12 do begin
      daysused := daysused + Month2Days(i);
      if daysused > d then begin
         Result := i;
         exit;
      end;
   end;
end;

{ Merges two horizon headers into a library horison name         }
{ Return 0: OK, 1: header1 missing, 2: header2 missing           }
function MkHorizonName(Header1, Header2: String; var HorName: String): Integer;
var
   s1, s2: String;
begin
   s1 := Trim(Header1); s2 := Trim(Header2);
   if Length(s1) = 0 then Result := 1
   else if Length(s2) = 0 then Result := 2
   else begin
      if Length(s1) = 1 then  { Header2 should start at position 3 in HorName }
         HorName := Copy(s1, 1, 1) + '  ' + s2
      else
         HorName := Copy(s1, 1, 2) + ' ' + s2;
      Result := 0;
   end;
end;

{ Splits a library horizon name into two horizon headers         }
{ Return 0: OK, 1: horname not long enough for header1 + header2 }
function SplitHorizonName(var Header1, Header2: String; HorName: String): Integer;
var
   s1, s2: String;
begin
   s1 := Trim(Copy(HorName, 1, 2));
   s2 := Trim(Copy(HorName, 4, 9999)); { copy to the end of HorName }
   if (s1 = '') or (s2 = '') then Result := 1
   else begin
      Header1 := s1; Header2 := s2;
      Result := 0;
   end;
end;


// Set the resources in an instance of the TDaisyTimeComp component
procedure SetTimeComponentResources(MultiDateMode : Boolean;
                                    var Component : TDaisyTimeComp);
begin
   Component.Caption                   := LoadStr(RES_FRM_DsyTime);
   Component.TimeTypeLabel.Caption     := LoadStr(RES_LBL_DsyTime_Time_Type);
   Component.DisplacementLabel.Caption := LoadStr(RES_LBL_DsyTime_Displacement);
   Component.ActionsLabel.Caption      := LoadStr(RES_LBL_DsyTime_Actions);

   // If we are in multi-type-date mode then we must use the following 3 properties
   if MultiDateMode then
      begin
         Component.TimeTypeDate     := LoadStr(RES_LBL_DsyTime_TimeType_Date);
         Component.TimeTypeApprox   := LoadStr(RES_LBL_DsyTime_TimeType_Approx);
         Component.TimeTypeRelative := LoadStr(RES_LBL_DsyTime_TimeType_Relative);
      end
   else
      // else we are in One-Date-Only mode, and we must use the following property
      Component.DateLabel := LoadStr(RES_LBL_DsyTime_Date);

   // Set the error messages
   Component.ErrorMessageHead    := LoadStr(RES_ERR_DsyTime_Not_A_Valid_Date1);
   Component.ErrorMessageBlank   := LoadStr(RES_ERR_DsyTime_Not_A_Valid_Date2);
   Component.ErrorMessageInvalid := LoadStr(RES_ERR_DsyTime_Not_A_Valid_Date3);
end;







initialization
   { Allocate global variables here }
   { for FormMainForm }
   MainFormVars := TMainFormVars.Create;
      { for Manager (subform to mainform) }
   TempManagerVars := TManagerVars.Create;
      { for FormGlobalInfo (subform to mainform) }
   TempGlobalInfoVars := TGlobalInfoVars.Create;
      { for FormProfile (subform to mainform) }
   TempProfileVars := TProfileVars.Create;
         { for FormDiscretization (subform to formprofile) }
   Temp2DiscretizationVars := TDiscretizationVars.Create;
         { for FormHorizon (subform to formprofile) }
   Temp2HorizonVars := THorizonVars.Create;
         { for FormStateInit (subform to formprofile) }
   Temp2StateVars := TStateVars.Create;

   { Init Help Empty Key ( used for displaying the second help tab!! ) }
   HelpEmptyString := '';
   HelpEmptyKey := LongInt(@HelpEmptyString);

   { Set current home directory (OpenDialog changes this "@£$¤) }
   DaisyHomeDir := GetCurrentDir;


   { *** To be removed when proper resource file... }  {...What???}
   FracErrMsg := LoadStr(RES_ERR_Globals_Invalid_Fraction) + CHR(10) + CHR(13)
                 + LoadStr(RES_ERR_Globals_Frac_Valid_Int_Dec_Point);

finalization
   { Destroy global variables here }
   MainFormVars.Destroy;
   TempManagerVars.Destroy;
   TempGlobalInfoVars.Destroy;
   TempProfileVars.Destroy;
   Temp2DiscretizationVars.Destroy;
   Temp2HorizonVars.Destroy;
   Temp2StateVars.Destroy;
end.
