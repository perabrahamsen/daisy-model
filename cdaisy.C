// @ cdaisy.C --- C interface to daisy.
//
// See `cdaisy.h' for more documentation.

#include "syntax.h"
#include "alist.h"
#include "daisy.h"
#include "parser_file.h"
#include "time.h"
#include "column.h"
#include "weather.h"
#include "common.h"
#include "action.h"
#include "groundwater.h"

typedef int daisy_bool;

// @ The daisy_syntax Type.

extern "C" Syntax*
daisy_syntax_create ()
{ return new Syntax (); }

extern "C" void				
daisy_syntax_delete (Syntax* syntax)
{ delete syntax; }

extern "C" int
daisy_syntax_check (const Syntax* syntax, const AttributeList* alist, 
		    const char* name)
{ return syntax->check (*alist, name); }

// @ The daisy_alist Type.

extern "C" AttributeList*
daisy_alist_create ()
{ return new AttributeList (); }

extern "C" void				// Delete alist object.
daisy_alist_delete (AttributeList* alist)
{ delete alist; }

// @ The daisy_parser Type.

extern "C" Parser*
daisy_parser_create_file (const Syntax* syntax, const char* filename)
{ return new ParserFile (*syntax, filename); }

extern "C" void
daisy_parser_delete (Parser* parser)
{ delete parser; }

extern "C" void
daisy_parser_load (Parser* parser, AttributeList* alist)
{ parser->load (*alist); }

// @ The daisy_daisy Type.

extern "C" Daisy*
daisy_daisy_create (const AttributeList* alist)
{ return new Daisy (*alist); }

extern "C" void				// Delete the daisy object.
daisy_daisy_delete (Daisy* daisy)
{ delete daisy; }

extern "C" daisy_bool			// Check context.
daisy_daisy_check (Daisy* daisy, const Syntax* syntax)
{ return daisy->check (*syntax); }

// @@ Running the simulation.

extern "C" void	
daisy_daisy_run (Daisy* daisy)
{ daisy->run (); }

extern "C" void
daisy_daisy_start (Daisy* daisy)
{ daisy->running = true; }

extern "C" daisy_bool
daisy_daisy_is_running (Daisy* daisy)
{ return daisy->running; }

extern "C" void
daisy_daisy_tick (Daisy* daisy)
{ daisy->tick (); }

extern "C" void
daisy_daisy_tick_action (Daisy* daisy)
{ daisy->action.doIt (*daisy); }

extern "C" void
daisy_daisy_tick_weather (Daisy* daisy)
{ daisy->weather.tick (daisy->time); }

void
daisy_daisy_tick_groundwater (Daisy* daisy)
{ daisy->groundwater.tick (daisy->time); }

extern "C" void
daisy_daisy_tick_columns (Daisy* daisy)
{
  daisy->tick_columns ();
}

extern "C" void
daisy_daisy_tick_column (Daisy* daisy, int col)
{ 
  daisy->columns[col]->tick (daisy->time, 
			     daisy->weather,
			     daisy->groundwater); 
}

extern "C" void
daisy_daisy_tick_logs (Daisy* daisy)
{ daisy->tick_logs (); }

extern "C" void				// Run time a single time step.
daisy_daisy_tick_time (Daisy* daisy)
{ daisy->time.tick_hour (); }

extern "C" daisy_bool			// Check if simulation is still active.
daisy_daisy_is_running (Daisy* daisy);

// @@ Manipulating the simulation.

extern "C" Time*
daisy_daisy_get_time (Daisy* daisy)
{ return &daisy->time; }

extern "C" Weather*
daisy_daisy_get_weather (Daisy* daisy)
{ return &daisy->weather; }

extern "C" int
daisy_daisy_count_columns (const Daisy* daisy)
{ return daisy->columns.size (); }

extern "C" Column*
daisy_daisy_get_column (Daisy* daisy, int col)
{ return daisy->columns[col]; }

extern "C" void
daisy_daisy_append_column (Daisy* daisy, Column* column);

extern "C" void
daisy_daisy_remove_column (Daisy* daisy, Column* column);

// @ The daisy_time Type.

extern "C" int
daisy_time_get_hour (Time* time)
{ return time->hour (); }

extern "C" int
daisy_time_get_mday (Time* time)
{ return time->mday (); }

extern "C" int
daisy_time_get_month (Time* time)
{ return time->month (); }

extern "C" int
daisy_time_get_year (Time* time)
{ return time->year (); }

// @ The daisy_weather Type.

extern "C" void
daisy_weather_put_precipitation (Weather* column, double prec);

extern "C" void
daisy_weather_put_air_temperature (Weather* column, double T);

extern "C" void
daisy_weather_put_reference_evapotranspiration (Weather* column, double ref);

// @ The daisy_column Type.

// @@ Cloning and merging columns.

extern "C" Column*

daisy_column_clone (const Column* column, const char* name);

extern "C" void
daisy_column_merge (Column* column, const Column* other, double weight);

// @@ Manipulating the column state.

extern "C" const char*
daisy_column_get_name (const Column* column)
{ return column->name.c_str (); }

// @@@ Soil Geometry.

extern "C" int
daisy_column_count_layers (const Column* column);

extern "C" double				// Heigh of numeric lay `lay' in cm.
daisy_column_get_dz (const Column* column, int lay);

// @@@ Soil Water. 
//
// Water content of the soil.

extern "C" void				// [cm]
daisy_column_put_water_pressure (Column* column, const double h[]);

extern "C" void				// [cm^3/cm^3/h]
daisy_column_get_water_sink (const Column* column, double sink[]);

// @@@ Soil Nitrate. 
// 
// Nitrate solution in the soil.

extern "C" void				// [g/cm^3]
daisy_column_put_no3_m (const double M[]);

extern "C" void				// [g/cm^3]
daisy_column_get_no3_m (double M[]);

// @@@ Bioclimate. 
//
// What happens in the canopy?

extern "C" double				// [mm/h]
daisy_column_get_evap_interception (const Column* column);

extern "C" double				// [mm]
daisy_column_get_intercepted_water (const Column* column);

extern "C" double				// [mm/h]
daisy_column_get_net_precipitation (const Column* column);

// @@@ Surface.
// 
// The surface manages anything that lies on top of the soil.

extern "C" double				// [mm/h]
daisy_column_get_evap_soil_surface (const Column* column);

extern "C" double				// [mm/h]
daisy_column_get_evap_pond (const Column* column);

extern "C" void				// [mm]
daisy_column_put_ponding (Column* column, double pond);

extern "C" void				// [g/cm^2]
daisy_column_put_surface_no3 (Column* column, double no3);

extern "C" double				// [g/cm^2]
daisy_column_get_surface_no3 (const Column* column);

extern "C" double				// [mm]
daisy_column_get_snow_height (const Column* column);

// @ Miscellaneous.
//
// Other functions which doesn't fit nicely into the above categories.

extern "C" void
daisy_load (Syntax* syntax, AttributeList* alist)
{ Daisy::load_syntax (*syntax, *alist); }

extern "C" void
daisy_initialize ()
{
#ifdef __unix
  // We should do the appropriate magic on Unix.
#else
  // We should load the DLL here on MSDOS.
#endif
}
