unit pasdaisy;

(* Unit pasdaisy derived from @ cdaisy.h --- C interface to daisy.
 *
 * This file describes the Delphi interface to the Daisy soil/crop
 * simulation model.
 *)

interface
Uses
  WinTypes;

(* @ Typeless pointers.
 *
 * In the Delphi interface, the C++ classes are represented
 * as typeless pointers, with accessors corresponding to the
 * member functions in C++.
 *)

type
   daisy_syntax  = pointer;
   daisy_alist   = pointer;
   daisy_library = pointer;
   daisy_daisy   = pointer;
   daisy_parser  = pointer;
   daisy_time    = pointer;
   daisy_column  = pointer;
   daisy_weather = pointer;
   daisy_printer = pointer;

   (* @ Utilities.
    *
    * These types aren't really a part of the Daisy model, but useful in
    * the description of the model nonetheless.
    *)

   (* @@ The daisy_bool Type.
    *
    * The daisy_bool type indicate that a variable only can have two
    * different states.  Since C doesn't have a bool type, we use an int
    * instead.
    *)

   daisy_bool = Integer;
   double_array = ^double;

   (* @ The daisy_syntax Type.
    *
    * A syntax describes what attributes that are associated with an
    * object.
    *)

   (* Create an empty syntax object. *)
   function _daisy_syntax_create: daisy_syntax; cdecl;

   (* Delete syntax object. *)
   procedure _daisy_syntax_delete(syntax: daisy_syntax); cdecl;

   (* Check that alist match the syntax. *)
   function _daisy_syntax_check (syntax: daisy_syntax;
	                        alist : daisy_alist;
		                name  : PChar ): daisy_bool; cdecl;

   (* Elements in the syntax table have the following properties.

      NAME: A string giving the name of the entry.
      CATEGORY: Specifies whether it is a constant parameter, a state
      variable, a log variable, or an optional parameter.
      TYPE: What kind of values this element will hold in the alist.
      SIZE: Specifies whether it is an array, and if so its size.
   *)

   (* Add element to syntax table. *)
   procedure _daisy_syntax_add (syntax: daisy_syntax;
                                name: PChar;
                                cat: Integer;
                                _type: Integer;
                                size: Integer); cdecl;



   (* Add alist type to syntax table. *)
   procedure _daisy_syntax_add_alist (syntax: daisy_syntax; name:PChar; cat: Integer;
                                      nested:daisy_syntax;size: Integer);cdecl;

   (* The following functions return "magic" values used by the `cat',
      `type', and `size' arguments to `daisy_syntax_add'.

      These will not change within a simulation, so you can safely cache
      the values.  The values may change in future versions of the daisy
      library, so don't hardcode them. *)

   (* The currently valid categories are "Const", "State", "Optional",
      and "LogOnly".  Each have an associated number.  Use the following
      functions to switch between number and name. *)

   (* Number used for specific category. *)
   function _daisy_category_number (name: PChar): Integer; cdecl;

   (* Name used for specific category. *)
   function _daisy_category_name (number: Integer): PChar; cdecl;

   (* Some negative sizes have "magic" meanings for the `size'
      parameter. Positive numbers indicate a fixed size array. *)

   (* An array of unknown size. *)
   function _daisy_size_sequence: Integer; cdecl;

   (* Not an array. *)
   function _daisy_size_singleton: Integer; cdecl;

   (* The following types are currently supported: "Number", "AList",
      "CSMP", "Boolean", "String", "Date", "Integer", "Object",
      "Library", and "Error". *)

   (* Number used for specific type. *)
   function _daisy_type_number (name: PChar): Integer; cdecl;

   (* Name used for a specific type. *)
   function _daisy_type_name (number: Integer): PChar; cdecl;

   (* @ The daisy_alist Type.
    *
    * An alist contains the attribute values read by the parser.
    *)

   (* Create an empty alist object. *)
   function _daisy_alist_create: daisy_alist; cdecl;

   (* Delete alist object. *)
   procedure _daisy_alist_delete(alist: daisy_alist); cdecl;
   function _daisy_alist_clone (alist: daisy_alist) : daisy_alist; cdecl;

{   (* Pretty-print alist to file. Return -1 on errors. *)

   function _daisy_alist_save (alist: daisy_alist; syntax: daisy_syntax; filename: PChar): Integer; cdecl;
}
   (* Check that NAME is defined in ALIST. *)
   function _daisy_alist_check(alist: daisy_alist; name: PChar): daisy_bool; cdecl;

   (* The following functions are for manipulating individual members of
      an array.   It is an error to call them if the member is an
      array. *)

   (* Get integer NAME from ALIST. *)
   function _daisy_alist_get_integer (alist: daisy_alist; name: PChar): Integer; cdecl;

   (* Get double NAME from ALIST. *)
   function _daisy_alist_get_number (alist: daisy_alist; name: PChar): double; cdecl;

   (* Get char* NAME from ALIST. *)
   function _daisy_alist_get_string (alist: daisy_alist; name: PChar): PChar; cdecl;

   (* Get bool NAME from ALIST. *)
   function _daisy_alist_get_flag (alist: daisy_alist; name: PChar): daisy_bool; cdecl;

   function _daisy_alist_get_time (alist: daisy_alist; name: PChar): daisy_time; cdecl;

   (* Get alist NAME from ALIST. *)
   function _daisy_alist_get_alist (alist: daisy_alist; name: PChar): daisy_alist; cdecl;

   (* Set integer NAME from ALIST to VALUE. *)
   procedure _daisy_alist_set_integer (alist: daisy_alist; name: PChar;
			 value: Integer) cdecl;

   (* Set double NAME from ALIST to VALUE. *)
   procedure _daisy_alist_set_number (alist: daisy_alist; name: PChar;
			value: double) cdecl;

   (* Set char* NAME from ALIST to VALUE. *)
   procedure _daisy_alist_set_string (alist: daisy_alist; name: PChar;
			value: PChar) cdecl;

   (* Set bool NAME from ALIST to VALUE. *)
   procedure _daisy_alist_set_flag (alist: daisy_alist; name: PChar;
		      value: daisy_bool) cdecl;

   procedure _daisy_alist_set_time (alist: daisy_alist; name: PChar;
		      value: daisy_time) cdecl;
   (* Set alist NAME from ALIST to VALUE. *)
   procedure _daisy_alist_set_alist (alist: daisy_alist; name: PChar;
		       value: daisy_alist)cdecl;

   (* The following functions are for manipulating array members of an alist.
      It is an error to call them if the member is not an array.  The
      array will grow automatically if you `set' values outside its upper
      bound. The lower array bound is zero. *)

   (* Size of integer array. *)
{   function _daisy_alist_size_integer (alist: daisy_alist; name: PChar): Cardinal; cdecl;
}
   (* Size of number array. *)
   function _daisy_alist_size_number (alist: daisy_alist; name: PChar): Cardinal; cdecl;

   (* Size of string array. *)
{   function _daisy_alist_size_string (alist: daisy_alist; name: PChar): Cardinal; cdecl;

   (* Size of flag array. *)
   function _daisy_alist_size_flag (alist: daisy_alist; name: PChar): Cardinal; cdecl;
}
   (* Size of alist array. *)
   function _daisy_alist_size_alist (alist: daisy_alist; name: PChar): Cardinal; cdecl;

   (* Get integer NAME[INDEX] from ALIST. *)
(*   function _daisy_alist_get_integer_at (alist: daisy_alist; name: PChar;
			    index: Cardinal): Cardinal; cdecl; *)

   (* Get double NAME[INDEX] from ALIST. *)
   function _daisy_alist_get_number_at (alist: daisy_alist; name: PChar;
			               index: Cardinal): double; cdecl;

   (* Get char* NAME[INDEX] from ALIST. *)
{   function _daisy_alist_get_string_at (alist: daisy_alist; name: PChar;
			   index: Cardinal): PChar; cdecl;

   (* Get bool NAME[INDEX] from ALIST. *)
   function _daisy_alist_get_flag_at (alist: daisy_alist; name: PChar;
			 index: Cardinal): daisy_bool; cdecl;

}   (* Get alist NAME[INDEX] from ALIST. *)
   function _daisy_alist_get_alist_at (alist: daisy_alist; name: PChar;
			  index: Cardinal): daisy_alist; cdecl;

   (* Set integer NAME[INDEX] from ALIST to VALUE. *)
  { procedure _daisy_alist_set_integer_at (alist: daisy_alist; name: PChar;
			    value: Integer; index: Cardinal); cdecl;

   (* Set double NAME[INDEX] from ALIST to VALUE. *)
  } procedure _daisy_alist_set_number_at (alist: daisy_alist; name: PChar;
			    value: double; index: Cardinal); cdecl;

   (* Set char* NAME[INDEX] from ALIST to VALUE. *)
 {  procedure _daisy_alist_set_string_at (alist: daisy_alist; name: PChar;
			   value: PChar; index: Cardinal); cdecl;

   (* Set bool NAME[INDEX] from ALIST to VALUE. *)
   procedure _daisy_alist_set_flag_at (alist: daisy_alist; name: PChar;
			  value: daisy_bool; index: Cardinal); cdecl;

   (* Set alist NAME[INDEX] from ALIST to VALUE. *)
}   procedure _daisy_alist_set_alist_at (alist: daisy_alist; name: PChar;
			    value: daisy_alist; index: Cardinal); cdecl;

   (* @ The daisy_library Type.
    *
    * A library contains a collection of objects, each containing a
    * constructor, a syntax, an alist, an origin, and a name.
    *)

   (* Return the library named NAME. *)
   function _daisy_library_find (name: PChar): daisy_library; cdecl;

   (* Number of objects in LIBRARY. *)
   function _daisy_library_size (lib: daisy_library): Integer; cdecl;

   (* Name of object number INDEX in LIBRARY. *)
   function _daisy_library_name (lib: daisy_library; index: Cardinal): PChar; cdecl;

   (* Syntax for object NAME in LIBRARY. *)
   function _daisy_library_syntax (lib: daisy_library; name: PChar): daisy_syntax; cdecl;

   (* Alist for object NAME in LIBRARY. *)
   function _daisy_library_alist (lib: daisy_library; name: PChar): daisy_alist; cdecl;

   (* File associated with object NAME in
				   LIBRARY, or NULL if none. *)
   function _daisy_library_file (lib: daisy_library; name: PChar): PChar; cdecl;

   (* Add new element to library.  SUPER is the name of an existing
      element, from which to inherit the constructor and syntax.  ALIST
      is the default attributes for the new object.  NAME is the name of
      the new object in the library.  FILENAME is the name of the file to
      eventually save the object.

      Currently, only the Horizon and Column libraries are supported.
   *)
   procedure _daisy_library_derive (lib: daisy_library;
		                    super: PChar; alist: daisy_alist;
		                    name: PChar; filename: PChar); cdecl;

   (* Remove object NAME from LIBRARY *)
   procedure _daisy_library_remove (lib: daisy_library; name: PChar); cdecl;

   (* Save all elements in all libraries that are associated with FILE.
   Return -1 on errors. *)
   function _daisy_library_save_file (filename: PChar): Integer; cdecl;

   (* @ The daisy_parser Type.
    *
    * A parser fills an alist based on a syntax.
    *)

   (* Create a file parser. *)
   function _daisy_parser_create_file (syntax: daisy_syntax; filename: PChar): daisy_parser; cdecl;

   (* Delete parser object. *)
   procedure _daisy_parser_delete (parser: daisy_parser); cdecl;

   (* Load file. *)
   procedure _daisy_parser_load (parser: daisy_parser; alist: daisy_alist); cdecl;


   (* @ The daisy_printer Type.
    *
    * A printer pretty print the content of alists and library objects.
    *)

   (* Print to FILENAME. *)
   function _daisy_printer_create_file (filename: PChar): daisy_printer; cdecl;

   (* Print COMMENT *)
   procedure _daisy_printer_comment (printer:daisy_printer; comment: PChar); cdecl;

   (* Print ALIST. *)
   procedure _daisy_printer_alist (printer: daisy_printer;alist:daisy_alist;syntax: daisy_syntax); cdecl;

   (* Save all elements in all libraries that are associated with FILE. *)
   procedure _daisy_printer_library_file (printer:daisy_printer; filename: PChar);cdecl;

   (* Return false iff errors have occured. *)
   function _daisy_printer_good (printer:daisy_printer):daisy_bool; cdecl;

   (* Delete the PRINTER object. *)
   procedure _daisy_printer_delete (printer:daisy_printer); cdecl;


   (* @ The daisy_daisy Type.
    *
    * The daisy_daisy object contains the entire simulation.
    *)

   (* Create the daisy object. *)
   function _daisy_daisy_create (syntax: daisy_syntax; alist: daisy_alist): daisy_daisy; cdecl;

   (* Delete the daisy object. *)
   procedure _daisy_daisy_delete (daisy: daisy_daisy); cdecl;

   (* Check context. *)
   function _daisy_daisy_check (daisy: daisy_daisy; syntax: daisy_syntax): daisy_bool; cdecl;

   (* @@ Running the simulation.
    *
    * There are three basic ways to run the simulation.  Run the entire
    * simulation to end, run a the entire simulation for a single time
    * step, or manually run each component of the simulation.  Running
    * the `action', `weather', `groundwater', `columns', `logs', and
    * `time' tick functions in that sequence is equivalent to running the
    * main daisy `tick' function.
    *)

   (* Run the Daisy simulation to the end. *)
   procedure _daisy_daisy_run (daisy: daisy_daisy); cdecl;

   (* Start the simulation. *)
   procedure _daisy_daisy_start (daisy: daisy_daisy); cdecl;

   (* Run all processes a single time step. *)
   procedure _daisy_daisy_tick (daisy: daisy_daisy); cdecl;

   (* Run manager a single time step. *)
   procedure _daisy_daisy_tick_action (daisy: daisy_daisy); cdecl;

   (* Run weather a single time step. *)
   procedure _daisy_daisy_tick_weather (daisy: daisy_daisy); cdecl;

   (* Run groundwater a single time step. *)
   procedure _daisy_daisy_tick_groundwater (daisy: daisy_daisy); cdecl;

   (* Run all columns a single time step. *)
   procedure _daisy_daisy_tick_columns (daisy: daisy_daisy); cdecl;

   (* Run column #col a single time step. *)
   procedure _daisy_daisy_tick_column (daisy: daisy_daisy; col: Integer); cdecl;

   (* Write all log files for this time step. *)
   procedure _daisy_daisy_tick_logs (daisy: daisy_daisy); cdecl;

   (* Run time a single time step. *)
   procedure _daisy_daisy_tick_time (daisy: daisy_daisy); cdecl;

   (* Check if simulation is still active. *)
   function _daisy_daisy_is_running (daisy: daisy_daisy): daisy_bool; cdecl;

   (* @@ Manipulating the simulation.
    *
    * These functions allows you to inspect and manipulate the individual
    * parts of the daisy simulation.
    *)

   (* Extract time. *)
   function _daisy_daisy_get_time (daisy: daisy_daisy): daisy_time; cdecl;

   (* Extract weather. *)
   function _daisy_daisy_get_weather (daisy: daisy_daisy): daisy_weather; cdecl;

   (* Count the number of columns in daisy. *)
   function _daisy_daisy_count_columns (daisy: daisy_daisy): Integer; cdecl;

   (* Extract a column, [0 <= col < size]. *)
   function _daisy_daisy_get_column (daisy: daisy_daisy; col: Integer): daisy_column; cdecl;

   (* Append an extra column to the simulation. *)
   procedure _daisy_daisy_append_column (daisy: daisy_daisy; column: daisy_column); cdecl;

   (* Remove column from simulation. *)
   procedure _daisy_daisy_remove_column (daisy: daisy_daisy; column: daisy_column); cdecl;

   (* @ The daisy_time Type.
    *
    * The time type keeps track of time in the simulation.
    *)

   (* Hour of day, starting with 0. *)
   function _daisy_time_get_hour (time: daisy_time): Integer; cdecl;

   (* Day of month, starting with 1. *)
   function _daisy_time_get_mday (time: daisy_time): Integer; cdecl;

   (* Month of year, starting with 1. *)
   function _daisy_time_get_month (time: daisy_time): Integer; cdecl;

   (* Year, four digits. *)
   function _daisy_time_get_year (time: daisy_time): Integer; cdecl;

   function _daisy_time_create(aa,mm,dd,hh:Integer): daisy_time; cdecl;
   procedure _daisy_time_delete(d:daisy_time); cdecl;

   (* @ The daisy_weather Type. *)

   (* [mm/d] *)
   procedure _daisy_weather_put_precipitation (column: daisy_weather; prec: double); cdecl;

   (* [degree C] *)
   procedure _daisy_weather_put_air_temperature (column: daisy_weather; T: double ); cdecl;

   (* [mm/d] *)
   procedure _daisy_weather_put_reference_evapotranspiration (column: daisy_weather;
						              ref: double); cdecl;

   (* @ The daisy_column Type.
    *
    * The daisy_column type keeps track of all information within a single
    * column in the simulation.
    *)

   (* @@ Cloning and merging columns.
    *
    * The general idea is to save calculations.  If you know that two
    * columns have an identical start, you can limit the calculations to
    * just one of them until the point where some action separates them.
    * At that point you clone the column, giving you an identical copy
    * (except for the name) of the column.  Similarly, you can merge two
    * columns when you judge that the difference between them is
    * sufficiently small.  The result is an "average" column.  If one
    * daisy column actually represent a larger field area than the other,
    * you can specify this by giving a weight.  A weight of `0.9' means
    * that the first column represents the conditions on 90% of the area,
    * and the second column the remaining 10%.
    *)

   (* Create new column by cloning. *)
   function _daisy_column_clone (column: daisy_column; name: PChar): daisy_column; cdecl;

   (* Merge `other' into `column'. *)
   procedure _daisy_column_merge (column: daisy_column; other: daisy_column;
	                    	  weight: double); cdecl;

   (* @@ Manipulating the column state.
    *
    * The idea behind these functions is that an external model can both
    * query and replace the state within a column.
    *)

   (* The name of the column. *)
   function _daisy_column_get_name (column: daisy_column): PChar; cdecl;

   (* @@@ Soil Geometry.
    *
    * The numeric layers used in the soil.
    *)

   (* The number of numeric layers. *)
   function _daisy_column_count_layers (column: daisy_column): Integer; cdecl;

   (* Heigh of numeric lay `lay' in cm. *)
   function _daisy_column_get_dz (column: daisy_column; lay: Integer): double; cdecl;

   (* @@@ Soil Water.
    *
    * Water content of the soil.
    *)

   (* [cm] *)
   procedure _daisy_column_put_water_pressure (column: daisy_column; h: double_array); cdecl;

   (* [cm^3/cm^3/h] *)
   procedure _daisy_column_get_water_sink (column: daisy_column; sink: double_array); cdecl;

   (* @@@ Soil Nitrate.
    *
    * Nitrate solution in the soil.
    *)

   (* [g/cm^3] *)
   procedure _daisy_column_put_no3_m (M: double_array); cdecl;

   (* [g/cm^3] *)
   procedure _daisy_column_get_no3_m (M: double_array); cdecl;

   (* @@@ Bioclimate.
    *
    * What happens in the canopy?
    *)

   (* [mm/h] *)
   function _daisy_column_get_evap_interception (column: daisy_column): double; cdecl;

   (* [mm] *)
   function _daisy_column_get_intercepted_water (column: daisy_column): double; cdecl;

   (* [mm/h] *)
   function _daisy_column_get_net_precipitation (column: daisy_column): double; cdecl;

   (* @@@ Surface.
    *
    * The surface manages anything that lies on top of the soil.
    *)

   (* [mm/h] *)
   function _daisy_column_get_evap_soil_surface (column: daisy_column): double; cdecl;

   (* [mm/h] *)
   function _daisy_column_get_evap_pond (column: daisy_column): double; cdecl;

   (* [mm] *)
   procedure _daisy_column_put_ponding (column: daisy_column; pond: double); cdecl;

   (* [g/cm^2] *)
   procedure _daisy_column_put_surface_no3 (column: daisy_column; no3: double); cdecl;

   (* [g/cm^2] *)
   function _daisy_column_get_surface_no3 (column: daisy_column): double; cdecl;

   (* [mm] *)
   function _daisy_column_get_snow_height (column: daisy_column): double; cdecl;

   (* @ Miscellaneous.
    *
    * Other functions which doesn't fit nicely into the above categories.
    *)

   (* Initialize syntax and alist for daisy. *)
   procedure _daisy_load (syntax: daisy_syntax; alist: daisy_syntax); cdecl;

   (* Initialize the Daisy subsystem. *)
   procedure _daisy_initialize; cdecl;

   function _daisy_version: PChar; cdecl;

Implementation
   (* cdecl == external 'daisy.dll' *)

   (* @ The daisy_syntax Type.
    *
    * A syntax describes what attributes that are associated with an
    * object.
    *)

   (* Create an empty syntax object. *)
   function _daisy_syntax_create: daisy_syntax; external 'daisy.dll';

   (* Delete syntax object. *)
   procedure _daisy_syntax_delete(syntax: daisy_syntax); external 'daisy.dll';

   (* Check that alist match the syntax. *)
   function _daisy_syntax_check (syntax: daisy_syntax;
	                        alist : daisy_alist;
		                name  : PChar ): daisy_bool; external 'daisy.dll';

   (* Elements in the syntax table have the following properties.

      NAME: A string giving the name of the entry.
      CATEGORY: Specifies whether it is a constant parameter, a state
      variable, a log variable, or an optional parameter.
      TYPE: What kind of values this element will hold in the alist.
      SIZE: Specifies whether it is an array, and if so its size.
   *)

   (* Add element to syntax table. *)
   procedure _daisy_syntax_add (syntax: daisy_syntax;
                                name: PChar;
                                cat: Integer;
                                _type: Integer;
                                size: Integer); external 'daisy.dll';

   (* Add alist type to syntax table. *)
   procedure _daisy_syntax_add_alist (syntax: daisy_syntax; name:PChar; cat: Integer;
                                      nested:daisy_syntax;size: Integer);external 'daisy.dll';


   (* The following functions return "magic" values used by the `cat',
      `type', and `size' arguments to `daisy_syntax_add'.

      These will not change within a simulation, so you can safely cache
      the values.  The values may change in future versions of the daisy
      library, so don't hardcode them. *)

   (* The currently valid categories are "Const", "State", "Optional",
      and "LogOnly".  Each have an associated number.  Use the following
      functions to switch between number and name. *)

   (* Number used for specific category. *)
   function _daisy_category_number (name: PChar): Integer; external 'daisy.dll';

   (* Name used for specific category. *)
   function _daisy_category_name (number: Integer): PChar; external 'daisy.dll';

   (* Some negative sizes have "magic" meanings for the `size'
      parameter. Positive numbers indicate a fixed size array. *)

   (* An array of unknown size. *)
   function _daisy_size_sequence: Integer; external 'daisy.dll';

   (* Not an array. *)
   function _daisy_size_singleton: Integer; external 'daisy.dll';

   (* The following types are currently supported: "Number", "AList",
      "CSMP", "Boolean", "String", "Date", "Integer", "Object",
      "Library", and "Error". *)

   (* Number used for specific type. *)
   function _daisy_type_number (name: PChar): Integer; external 'daisy.dll';

   (* Name used for a specific type. *)
   function _daisy_type_name (number: Integer): PChar; external 'daisy.dll';

   (* @ The daisy_alist Type.
    *
    * An alist contains the attribute values read by the parser.
    *)

   (* Create an empty alist object. *)
   function _daisy_alist_create: daisy_alist; external 'daisy.dll';

   (* Delete alist object. *)
   procedure _daisy_alist_delete(alist: daisy_alist); external 'daisy.dll';
   function _daisy_alist_clone (alist: daisy_alist) : daisy_alist;  external 'daisy.dll';

   (* Pretty-print alist to file. Return -1 on errors. *)
{
   function _daisy_alist_save (alist: daisy_alist; syntax: daisy_syntax; filename: PChar): Integer; external 'daisy.dll';
}
   (* Check that NAME is defined in ALIST. *)
   function _daisy_alist_check(alist: daisy_alist; name: PChar): daisy_bool; external 'daisy.dll';

   (* The following functions are for manipulating individual members of
      an array.   It is an error to call them if the member is an
      array. *)

   (* Get integer NAME from ALIST. *)
   function _daisy_alist_get_integer (alist: daisy_alist; name: PChar): Integer; external 'daisy.dll';

   (* Get double NAME from ALIST. *)
   function _daisy_alist_get_number (alist: daisy_alist; name: PChar): double; external 'daisy.dll';

   (* Get char* NAME from ALIST. *)
   function _daisy_alist_get_string (alist: daisy_alist; name: PChar): PChar; external 'daisy.dll';

   (* Get bool NAME from ALIST. *)
   function _daisy_alist_get_flag (alist: daisy_alist; name: PChar): daisy_bool; external 'daisy.dll';

   function _daisy_alist_get_time (alist: daisy_alist; name: PChar): daisy_time; external 'daisy.dll';

   (* Get alist NAME from ALIST. *)
   function _daisy_alist_get_alist (alist: daisy_alist; name: PChar): daisy_alist; external 'daisy.dll';

   (* Set integer NAME from ALIST to VALUE. *)
   procedure _daisy_alist_set_integer (alist: daisy_alist; name: PChar;
			 value: Integer); external 'daisy.dll';

   (* Set double NAME from ALIST to VALUE. *)
   procedure _daisy_alist_set_number (alist: daisy_alist; name: PChar;
			value: double); external 'daisy.dll';

   (* Set char* NAME from ALIST to VALUE. *)
   procedure _daisy_alist_set_string (alist: daisy_alist; name: PChar;
			value: PChar); external 'daisy.dll';

   (* Set bool NAME from ALIST to VALUE. *)
   procedure _daisy_alist_set_flag (alist: daisy_alist; name: PChar;
		      value: daisy_bool); external 'daisy.dll';
   procedure _daisy_alist_set_time (alist: daisy_alist; name: PChar;
		      value: daisy_time); external 'daisy.dll';

   (* Set alist NAME from ALIST to VALUE. *)
   procedure _daisy_alist_set_alist (alist: daisy_alist; name: PChar;
		       value: daisy_alist);external 'daisy.dll';

   (* The following functions are for manipulating array members of an alist.
      It is an error to call them if the member is not an array.  The
      array will grow automatically if you `set' values outside its upper
      bound. The lower array bound is zero. *)

   (* Size of integer array. *)
{   function _daisy_alist_size_integer (alist: daisy_alist; name: PChar): Cardinal; external 'daisy.dll';
}
   (* Size of number array. *)
   function _daisy_alist_size_number (alist: daisy_alist; name: PChar): Cardinal; external 'daisy.dll';

   (* Size of string array. *)
{   function _daisy_alist_size_string (alist: daisy_alist; name: PChar): Cardinal; external 'daisy.dll';

   (* Size of flag array. *)
   function _daisy_alist_size_flag (alist: daisy_alist; name: PChar): Cardinal; external 'daisy.dll';
}
   (* Size of alist array. *)
   function _daisy_alist_size_alist (alist: daisy_alist; name: PChar): Cardinal; external 'daisy.dll';

   (* Get integer NAME[INDEX] from ALIST. *)
(*   function _daisy_alist_get_integer_at (alist: daisy_alist; name: PChar;
			    index: Cardinal): Cardinal; external 'daisy.dll'; *)

   (* Get double NAME[INDEX] from ALIST. *)
   function _daisy_alist_get_number_at (alist: daisy_alist; name: PChar;
			               index: Cardinal): double; external 'daisy.dll';

   (* Get char* NAME[INDEX] from ALIST. *)
{   function _daisy_alist_get_string_at (alist: daisy_alist; name: PChar;
			   index: Cardinal): PChar; external 'daisy.dll';

   (* Get bool NAME[INDEX] from ALIST. *)
   function _daisy_alist_get_flag_at (alist: daisy_alist; name: PChar;
			 index: Cardinal): daisy_bool; external 'daisy.dll';

}   (* Get alist NAME[INDEX] from ALIST. *)
   function _daisy_alist_get_alist_at (alist: daisy_alist; name: PChar;
			  index: Cardinal): daisy_alist; external 'daisy.dll';

   (* Set integer NAME[INDEX] from ALIST to VALUE. *)
  { procedure _daisy_alist_set_integer_at (alist: daisy_alist; name: PChar;
			    value: Integer; index: Cardinal); external 'daisy.dll';

   (* Set double NAME[INDEX] from ALIST to VALUE. *)
  } procedure _daisy_alist_set_number_at (alist: daisy_alist; name: PChar;
			    value: double; index: Cardinal); external 'daisy.dll';

   (* Set char* NAME[INDEX] from ALIST to VALUE. *)
 {  procedure _daisy_alist_set_string_at (alist: daisy_alist; name: PChar;
			   value: PChar; index: Cardinal); external 'daisy.dll';

   (* Set bool NAME[INDEX] from ALIST to VALUE. *)
   procedure _daisy_alist_set_flag_at (alist: daisy_alist; name: PChar;
			  value: daisy_bool; index: Cardinal); external 'daisy.dll';

   (* Set alist NAME[INDEX] from ALIST to VALUE. *)
}   procedure _daisy_alist_set_alist_at (alist: daisy_alist; name: PChar;
			    value: daisy_alist; index: Cardinal); external 'daisy.dll';

   (* @ The daisy_library Type.
    *
    * A library contains a collection of objects, each containing a
    * constructor, a syntax, an alist, an origin, and a name.
    *)

   (* Return the library named NAME. *)
   function _daisy_library_find (name: PChar): daisy_library; external 'daisy.dll';

   (* Number of objects in LIBRARY. *)
   function _daisy_library_size (lib: daisy_library): Integer; external 'daisy.dll';

   (* Name of object number INDEX in LIBRARY. *)
   function _daisy_library_name (lib: daisy_library; index: Cardinal): PChar; external 'daisy.dll';

   (* Syntax for object NAME in LIBRARY. *)
   function _daisy_library_syntax (lib: daisy_library; name: PChar): daisy_syntax; external 'daisy.dll';

   (* Alist for object NAME in LIBRARY. *)
   function _daisy_library_alist (lib: daisy_library; name: PChar): daisy_alist; external 'daisy.dll';

   (* File associated with object NAME in
				   LIBRARY, or NULL if none. *)
   function _daisy_library_file (lib: daisy_library; name: PChar): PChar; external 'daisy.dll';

   (* Add new element to library.  SUPER is the name of an existing
      element, from which to inherit the constructor and syntax.  ALIST
      is the default attributes for the new object.  NAME is the name of
      the new object in the library.  FILENAME is the name of the file to
      eventually save the object.

      Currently, only the Horizon and Column libraries are supported.
   *)
   procedure _daisy_library_derive (lib: daisy_library;
		                    super: PChar; alist: daisy_alist;
		                    name: PChar; filename: PChar); external 'daisy.dll';

   (* Remove object NAME from LIBRARY *)
   procedure _daisy_library_remove (lib: daisy_library; name: PChar); external 'daisy.dll';

   (* Save all elements in all libraries that are associated with FILE.
   Return -1 on errors. *)
   function _daisy_library_save_file (filename: PChar): Integer; external 'daisy.dll';

   (* @ The daisy_parser Type.
    *
    * A parser fills an alist based on a syntax.
    *)

   (* Create a file parser. *)
   function _daisy_parser_create_file (syntax: daisy_syntax; filename: PChar): daisy_parser; external 'daisy.dll';

   (* Delete parser object. *)
   procedure _daisy_parser_delete (parser: daisy_parser); external 'daisy.dll';

   (* Load file. *)
   procedure _daisy_parser_load (parser: daisy_parser; alist: daisy_alist); external 'daisy.dll';
      (* @ The daisy_printer Type.
    *
    * A printer pretty print the content of alists and library objects.
    *)

   (* Print to FILENAME. *)
   function _daisy_printer_create_file (filename: PChar): daisy_printer; external 'daisy.dll';

   (* Print COMMENT *)
   procedure _daisy_printer_comment (printer:daisy_printer; comment: PChar); external 'daisy.dll';

   (* Print ALIST. *)
   procedure _daisy_printer_alist (printer: daisy_printer;alist:daisy_alist;syntax: daisy_syntax); external 'daisy.dll';

   (* Save all elements in all libraries that are associated with FILE. *)
   procedure _daisy_printer_library_file (printer:daisy_printer; filename: PChar);external 'daisy.dll';

   (* Return false iff errors have occured. *)
   function _daisy_printer_good (printer:daisy_printer):daisy_bool; external 'daisy.dll';

   (* Delete the PRINTER object. *)
   procedure _daisy_printer_delete (printer:daisy_printer); external 'daisy.dll';



   (* @ The daisy_daisy Type.
    *
    * The daisy_daisy object contains the entire simulation.
    *)

   (* Create the daisy object. *)
   function _daisy_daisy_create (syntax: daisy_syntax; alist: daisy_alist): daisy_daisy; external 'daisy.dll';

   (* Delete the daisy object. *)
   procedure _daisy_daisy_delete (daisy: daisy_daisy); external 'daisy.dll';

   (* Check context. *)
   function _daisy_daisy_check (daisy: daisy_daisy; syntax: daisy_syntax): daisy_bool; external 'daisy.dll';

   (* @@ Running the simulation.
    *
    * There are three basic ways to run the simulation.  Run the entire
    * simulation to end, run a the entire simulation for a single time
    * step, or manually run each component of the simulation.  Running
    * the `action', `weather', `groundwater', `columns', `logs', and
    * `time' tick functions in that sequence is equivalent to running the
    * main daisy `tick' function.
    *)

   (* Run the Daisy simulation to the end. *)
   procedure _daisy_daisy_run (daisy: daisy_daisy); external 'daisy.dll';

   (* Start the simulation. *)
   procedure _daisy_daisy_start (daisy: daisy_daisy); external 'daisy.dll';

   (* Run all processes a single time step. *)
   procedure _daisy_daisy_tick (daisy: daisy_daisy); external 'daisy.dll';

   (* Run manager a single time step. *)
   procedure _daisy_daisy_tick_action (daisy: daisy_daisy); external 'daisy.dll';

   (* Run weather a single time step. *)
   procedure _daisy_daisy_tick_weather (daisy: daisy_daisy); external 'daisy.dll';

   (* Run groundwater a single time step. *)
   procedure _daisy_daisy_tick_groundwater (daisy: daisy_daisy); external 'daisy.dll';

   (* Run all columns a single time step. *)
   procedure _daisy_daisy_tick_columns (daisy: daisy_daisy); external 'daisy.dll';

   (* Run column #col a single time step. *)
   procedure _daisy_daisy_tick_column (daisy: daisy_daisy; col: Integer); external 'daisy.dll';

   (* Write all log files for this time step. *)
   procedure _daisy_daisy_tick_logs (daisy: daisy_daisy); external 'daisy.dll';

   (* Run time a single time step. *)
   procedure _daisy_daisy_tick_time (daisy: daisy_daisy); external 'daisy.dll';

   (* Check if simulation is still active. *)
   function _daisy_daisy_is_running (daisy: daisy_daisy): daisy_bool; external 'daisy.dll';

   (* @@ Manipulating the simulation.
    *
    * These functions allows you to inspect and manipulate the individual
    * parts of the daisy simulation.
    *)

   (* Extract time. *)
   function _daisy_daisy_get_time (daisy: daisy_daisy): daisy_time; external 'daisy.dll';

   (* Extract weather. *)
   function _daisy_daisy_get_weather (daisy: daisy_daisy): daisy_weather; external 'daisy.dll';

   (* Count the number of columns in daisy. *)
   function _daisy_daisy_count_columns (daisy: daisy_daisy): Integer; external 'daisy.dll';

   (* Extract a column, [0 <= col < size]. *)
   function _daisy_daisy_get_column (daisy: daisy_daisy; col: Integer): daisy_column; external 'daisy.dll';

   (* Append an extra column to the simulation. *)
   procedure _daisy_daisy_append_column (daisy: daisy_daisy; column: daisy_column); external 'daisy.dll';

   (* Remove column from simulation. *)
   procedure _daisy_daisy_remove_column (daisy: daisy_daisy; column: daisy_column); external 'daisy.dll';

   (* @ The daisy_time Type.
    *
    * The time type keeps track of time in the simulation.
    *)

   (* Hour of day, starting with 0. *)
   function _daisy_time_get_hour (time: daisy_time): Integer; external 'daisy.dll';

   (* Day of month, starting with 1. *)
   function _daisy_time_get_mday (time: daisy_time): Integer; external 'daisy.dll';

   (* Month of year, starting with 1. *)
   function _daisy_time_get_month (time: daisy_time): Integer; external 'daisy.dll';

   (* Year, four digits. *)
   function _daisy_time_get_year (time: daisy_time): Integer; external 'daisy.dll';

   function _daisy_time_create(aa,mm,dd,hh:Integer): daisy_time; external 'daisy.dll';
   procedure _daisy_time_delete(d:daisy_time); external 'daisy.dll';

   (* @ The daisy_weather Type. *)

   (* [mm/d] *)
   procedure _daisy_weather_put_precipitation (column: daisy_weather; prec: double); external 'daisy.dll';

   (* [degree C] *)
   procedure _daisy_weather_put_air_temperature (column: daisy_weather; T: double ); external 'daisy.dll';

   (* [mm/d] *)
   procedure _daisy_weather_put_reference_evapotranspiration (column: daisy_weather;
						              ref: double); external 'daisy.dll';

   (* @ The daisy_column Type.
    *
    * The daisy_column type keeps track of all information within a single
    * column in the simulation.
    *)

   (* @@ Cloning and merging columns.
    *
    * The general idea is to save calculations.  If you know that two
    * columns have an identical start, you can limit the calculations to
    * just one of them until the point where some action separates them.
    * At that point you clone the column, giving you an identical copy
    * (except for the name) of the column.  Similarly, you can merge two
    * columns when you judge that the difference between them is
    * sufficiently small.  The result is an "average" column.  If one
    * daisy column actually represent a larger field area than the other,
    * you can specify this by giving a weight.  A weight of `0.9' means
    * that the first column represents the conditions on 90% of the area,
    * and the second column the remaining 10%.
    *)

   (* Create new column by cloning. *)
   function _daisy_column_clone (column: daisy_column; name: PChar): daisy_column; external 'daisy.dll';

   (* Merge `other' into `column'. *)
   procedure _daisy_column_merge (column: daisy_column; other: daisy_column;
	                    	  weight: double); external 'daisy.dll';

   (* @@ Manipulating the column state.
    *
    * The idea behind these functions is that an external model can both
    * query and replace the state within a column.
    *)

   (* The name of the column. *)
   function _daisy_column_get_name (column: daisy_column): PChar; external 'daisy.dll';

   (* @@@ Soil Geometry.
    *
    * The numeric layers used in the soil.
    *)

   (* The number of numeric layers. *)
   function _daisy_column_count_layers (column: daisy_column): Integer; external 'daisy.dll';

   (* Heigh of numeric lay `lay' in cm. *)
   function _daisy_column_get_dz (column: daisy_column; lay: Integer): double; external 'daisy.dll';

   (* @@@ Soil Water.
    *
    * Water content of the soil.
    *)

   (* [cm] *)
   procedure _daisy_column_put_water_pressure (column: daisy_column; h: double_array); external 'daisy.dll';

   (* [cm^3/cm^3/h] *)
   procedure _daisy_column_get_water_sink (column: daisy_column; sink: double_array); external 'daisy.dll';

   (* @@@ Soil Nitrate.
    *
    * Nitrate solution in the soil.
    *)

   (* [g/cm^3] *)
   procedure _daisy_column_put_no3_m (M: double_array); external 'daisy.dll';

   (* [g/cm^3] *)
   procedure _daisy_column_get_no3_m (M: double_array); external 'daisy.dll';

   (* @@@ Bioclimate.
    *
    * What happens in the canopy?
    *)

   (* [mm/h] *)
   function _daisy_column_get_evap_interception (column: daisy_column): double; external 'daisy.dll';

   (* [mm] *)
   function _daisy_column_get_intercepted_water (column: daisy_column): double; external 'daisy.dll';

   (* [mm/h] *)
   function _daisy_column_get_net_precipitation (column: daisy_column): double; external 'daisy.dll';

   (* @@@ Surface.
    *
    * The surface manages anything that lies on top of the soil.
    *)

   (* [mm/h] *)
   function _daisy_column_get_evap_soil_surface (column: daisy_column): double; external 'daisy.dll';

   (* [mm/h] *)
   function _daisy_column_get_evap_pond (column: daisy_column): double; external 'daisy.dll';

   (* [mm] *)
   procedure _daisy_column_put_ponding (column: daisy_column; pond: double); external 'daisy.dll';

   (* [g/cm^2] *)
   procedure _daisy_column_put_surface_no3 (column: daisy_column; no3: double); external 'daisy.dll';

   (* [g/cm^2] *)
   function _daisy_column_get_surface_no3 (column: daisy_column): double; external 'daisy.dll';

   (* [mm] *)
   function _daisy_column_get_snow_height (column: daisy_column): double; external 'daisy.dll';

   (* @ Miscellaneous.
    *
    * Other functions which doesn't fit nicely into the above categories.
    *)

   (* Initialize syntax and alist for daisy. *)
   procedure _daisy_load (syntax: daisy_syntax; alist: daisy_syntax); external 'daisy.dll';

   (* Initialize the Daisy subsystem. *)
   procedure _daisy_initialize; external 'daisy.dll';
   function _daisy_version:PChar; external 'daisy.dll';

End.
