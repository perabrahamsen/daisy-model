// @ cdaisy.C --- C interface to daisy.
//
// See `cdaisy.h' for more documentation.

#include "syntax.h"
#include "alist.h"
#include "daisy.h"
#include "parser_file.h"
#include "time.h"
#include "field.h"
#include "column.h"
#include "weather.h"
#include "common.h"
#include "action.h"
#include "horizon.h"
#include "printer_file.h"
#include "version.h"
#include "chemical.h"
#include "log_extern.h"

#include <fstream.h>

typedef int daisy_bool;

// @ The daisy_syntax Type.

extern "C" Syntax* EXPORT
daisy_syntax_create ()
{ return new Syntax (); }

extern "C" void EXPORT		
daisy_syntax_delete (Syntax* syntax)
{ delete syntax; }

extern "C" int EXPORT
daisy_syntax_check (const Syntax* syntax, const AttributeList* alist, 
		    const char* name)
{ return syntax->check (*alist, CERR, name); }

extern "C" void EXPORT
daisy_syntax_add (Syntax* syntax, const char* name,
		  Syntax::category cat, Syntax::type type, int size)
{ syntax->add (name, type, cat, size, "added from C API"); }

extern "C" void EXPORT
daisy_syntax_add_alist (Syntax* syntax, const char* name,
			Syntax::category cat, Syntax* nested, int size)
{ syntax->add (name, *nested, cat, size, "added from C API"); }

extern "C" int EXPORT
daisy_category_number (const char* name)
{ return Syntax::category_number (name); }

extern "C" const char* EXPORT
daisy_category_name (Syntax::category number)
{ return Syntax::category_name (number); }

extern "C" int EXPORT
daisy_size_sequence ()
{ return Syntax::Sequence; }

extern "C" int EXPORT
daisy_size_singleton ()
{ return Syntax::Singleton; }

extern "C" int EXPORT
daisy_type_number (const char* name)
{ return Syntax::type_number (name); }

extern "C" const char* EXPORT
daisy_type_name (Syntax::type number)
{ return Syntax::type_name (number); }

// @ The daisy_alist Type.

extern "C" AttributeList* EXPORT
daisy_alist_create ()
{ return new AttributeList (); }


extern "C" AttributeList* EXPORT
daisy_alist_clone (const AttributeList* alist)
{ return new AttributeList (*alist); }

extern "C" void EXPORT
daisy_alist_delete (AttributeList* alist)
{ delete alist; }

extern "C" daisy_bool EXPORT
daisy_alist_check (const AttributeList* alist, const char* name)
{ 
  return alist->check (name);
}

extern "C" int EXPORT
daisy_alist_get_integer (const AttributeList* alist, const char* name)
{ 
  return alist->integer (name);
}

extern "C" double EXPORT
daisy_alist_get_number (const AttributeList* alist, const char* name)
{ 
  return alist->number (name);
}

extern "C" const char* EXPORT
daisy_alist_get_string (const AttributeList* alist, const char* name)
{ 
  return alist->name (name).c_str ();
}

extern "C" daisy_bool EXPORT
daisy_alist_get_flag (const AttributeList* alist, const char* name)
{ 
  return alist->flag (name);
}

extern "C" const Time* EXPORT
daisy_alist_get_time (const AttributeList* alist, const char* name)
{
  return &alist->time(name);
}

extern "C" const AttributeList* EXPORT
daisy_alist_get_alist (const AttributeList* alist, const char* name)
{ 
  return &alist->alist (name);
}

extern "C" void EXPORT
daisy_alist_set_integer (AttributeList* alist, const char* name,
			 int value)
{ 
  alist->add (name, value);
}

extern "C" void EXPORT
daisy_alist_set_number (AttributeList* alist, const char* name,
			double value)
{
  alist->add (name, value);
}

extern "C" void EXPORT
daisy_alist_set_string (AttributeList* alist, const char* name,
			const char* value)
{ 
  alist->add (name, value);
}

extern "C" void EXPORT
daisy_alist_set_flag (AttributeList* alist, const char* name,
		      daisy_bool value)
{ 
  alist->add (name, bool (value));
}

extern "C" void EXPORT
daisy_alist_set_time (AttributeList* alist, const char* name,
		      Time* value)
{
  alist->add (name, *value);
}

extern "C" void EXPORT
daisy_alist_set_alist (AttributeList* alist, const char* name,
		       AttributeList* value)
{ 
  alist->add (name, *value);
}

#ifdef UNINPLEMENTED
extern "C" unsigned int EXPORT
daisy_alist_size_integer (const AttributeList* alist, const char* name)
{ return alist->integer_sequence (name).size (); }

extern "C" unsigned int EXPORT
daisy_alist_size_string (const AttributeList* alist, const char* name)
{ return alist->name_sequence (name).size (); }

extern "C" unsigned int EXPORT
daisy_alist_size_flag (const AttributeList* alist, const char* name)
{ return alist->flag_sequence (name).size (); }
#endif

extern "C" unsigned int EXPORT
daisy_alist_size_number (const AttributeList* alist, const char* name)
{ return alist->number_sequence (name).size (); }

extern "C" unsigned int EXPORT
daisy_alist_size_alist (const AttributeList* alist, const char* name)
{
  // KLUDGE: Work around the use of non-sequence value as the default
  // for each element in the sequence.
  if (alist->size (name) == Syntax::Singleton)
    return 0;
  
  return alist->alist_sequence (name).size (); }

#ifdef UNINPLEMENTED
extern "C" int EXPORT
daisy_alist_get_integer_at (const AttributeList* alist, const char* name,
			    unsigned int index)
{ return alist->integer_sequence (name)[index]; }

extern "C" const char* EXPORT
daisy_alist_get_string_at (const AttributeList* alist, const char* name,
			    unsigned int index)
{ return alist->name_sequence (name)[index].c_str (); }

extern "C" daisy_bool EXPORT
daisy_alist_get_flag_at (const AttributeList* alist, const char* name,
			    unsigned int index)
{ return alist->flag_sequence (name)[index]; }
#endif

extern "C" double EXPORT
daisy_alist_get_number_at (const AttributeList* alist, const char* name,
			    unsigned int index)
{ return alist->number_sequence (name)[index]; }

extern "C" AttributeList* EXPORT
daisy_alist_get_alist_at (const AttributeList* alist, const char* name,
			  unsigned int index)
{ return alist->alist_sequence (name)[index]; }

#ifdef UNINPLEMENTED
extern "C" void EXPORT
daisy_alist_set_integer_at (AttributeList* alist, const char* name,
			    int value, unsigned int index)
{ 
  vector<int>& v = alist->check (name)
    ? *new vector<int> (alist->integer_sequence (name))
    : *new vector<int>;
  if (v.size () <= index)
    while (v.size () <= index)
      v.push_back (value);
  else
    v[index] = value;
  alist->add (name, v);
}
#endif

extern "C" void EXPORT
daisy_alist_set_string_at (AttributeList* alist, const char* name,
			   const char* value, unsigned int index)
{
  vector<string>& v = alist->check (name)
    ? *new vector<string> (alist->name_sequence (name))
    : *new vector<string>;
  if (v.size () <= index)
    while (v.size () <= index)
      v.push_back (value);
  else
    v[index] = value;
  alist->add (name, v);
}

#ifdef UNINPLEMENTED
extern "C" void EXPORT
daisy_alist_set_flag_at (AttributeList* alist, const char* name,
			 daisy_bool value, unsigned int index)
{ 
  vector<bool>& v = alist->check (name)
    ? *new vector<bool> (alist->flag_sequence (name))
    : *new vector<bool>;
  if (v.size () <= index)
    while (v.size () <= index)
      v.push_back (value);
  else
    v[index] = value;
  alist->add (name, v);
}
#endif

extern "C" void EXPORT
daisy_alist_set_number_at (AttributeList* alist, const char* name,
			   double value, unsigned int index)
{
  vector<double>& v= alist->check (name)
    ? *new vector<double> (alist->number_sequence (name))
    : *new vector<double>;
  if (v.size () <= index)
    while (v.size () <= index)
      v.push_back (value);
  else
    v[index] = value;
  alist->add (name, v);
}

extern "C" void EXPORT
daisy_alist_set_alist_at (AttributeList* alist, const char* name,
			  AttributeList* value, unsigned int index)
{ 
  vector<AttributeList*>& v = alist->check (name)
    ? *new vector<AttributeList*> (alist->alist_sequence (name))
    : *new vector<AttributeList*>;
  if (v.size () <= index)
    while (v.size () <= index)
      v.push_back (value);
  else
    {
#if 0
      // BUG: Might be duplicate, so we can't delete.
      delete v[index];
#endif
      v[index] = value;
    }
  alist->add (name, v);
}

/* @ The daisy_library Type.
 * 
 * A library contains a collection of objects, each containing a
 * constructor, a syntax, an alist, an origin, and a name.
 */

extern "C" Library* EXPORT
daisy_library_find (const char* name)
{ return &Library::find (name); }

extern "C" int EXPORT
daisy_library_size (const Library* library)
{
  vector<string> entries;
  library->entries (entries);
  return entries.size ();
}

extern "C" const char* EXPORT
daisy_library_name (const Library* library, const unsigned int index)
{
  vector<string> entries;
  library->entries (entries);
  return entries[index].c_str ();
}

extern "C" const Syntax* EXPORT
daisy_library_syntax (const Library* library, const char* name)
{ return &library->syntax (name); }

extern "C" const AttributeList* EXPORT
daisy_library_alist (const Library* library, const char* name)
{ return &library->lookup (name); }

extern "C" const char* EXPORT
daisy_library_file (const Library* library, const char* name)
{ 
  const AttributeList& alist = library->lookup (name);
  if (alist.check ("parsed_from_file"))
    return alist.name ("parsed_from_file").c_str ();
  
  return NULL;
}

extern "C" void EXPORT
daisy_library_derive (Library* library, 
		      const char* super, AttributeList* alist, 
		      const char* name, const char* filename)
{ 
  if (filename)
    {
      alist->add ("parsed_from_file", filename);
      alist->add ("parsed_sequence", Library::get_sequence ());
      // alist->add ("type", super);
    }
  library->add_derived (name, *alist, super);
}

extern "C" void EXPORT
daisy_library_remove (Library* library, const char* name)
{ library->remove (name); }

// @ The daisy_parser Type.

extern "C" Parser* EXPORT
daisy_parser_create_file (const Syntax* syntax, const char* filename)
{ return new ParserFile (*syntax, filename, CERR); }

extern "C" void EXPORT
daisy_parser_delete (Parser* parser)
{ delete parser; }

extern "C" void EXPORT
daisy_parser_load (Parser* parser, AttributeList* alist)
{ parser->load (*alist); }

extern "C" unsigned int EXPORT
daisy_parser_error_count (Parser* parser)
{ return parser->error_count (); }

// @ The daisy_printer Type.

extern "C" Printer* EXPORT
daisy_printer_create_file (const char* filename)
{ return new PrinterFile (filename); }

extern "C" void EXPORT
daisy_printer_comment (Printer* printer, const char* comment)
{ printer->print_comment (comment); }

extern "C" void EXPORT
daisy_printer_alist (Printer* printer, 
		     const AttributeList* alist, const Syntax* syntax)
{ printer->print_alist (*alist, *syntax); }

extern "C" void EXPORT
daisy_printer_library_file (Printer* printer, const char* filename)
{ printer->print_library_file (filename); }

extern "C" void EXPORT
daisy_printer_delete (Printer* printer)
{ delete printer; }

extern "C" bool EXPORT
daisy_printer_good (Printer* printer)
{ return printer->good (); }

// @ The daisy_daisy Type.

extern "C" Daisy* EXPORT
daisy_daisy_create (const Syntax* syntax, const AttributeList* alist)
{ 
  Daisy* daisy =  new Daisy (*alist); 
  daisy->initialize (*syntax);
  return daisy;
}

extern "C" void EXPORT
daisy_daisy_delete (Daisy* daisy)
{ delete daisy; }

extern "C" daisy_bool EXPORT	// Check context.
daisy_daisy_check (Daisy* daisy)
{ return daisy->check (CERR); }

// @@ Running the simulation.

extern "C" void EXPORT	
daisy_daisy_run (Daisy* daisy)
{
  try
    {
      daisy->run (); 
    }
  catch (const char* error)
    {
      CERR << "Exception: " << error << "\n";
      exit (1);
    }
  catch (...)
    {
      CERR << "Unhandled exception\n";
      exit (1);
    }
}

extern "C" void EXPORT
daisy_daisy_start (Daisy* daisy)
{ daisy->running = true; }

extern "C" daisy_bool EXPORT
daisy_daisy_is_running (Daisy* daisy)
{ return daisy->running; }

extern "C" void EXPORT
daisy_daisy_tick (Daisy* daisy)
{
  try
    {
      daisy->tick (); 
    }
  catch (const char* error)
    {
      CERR << "Exception: " << error << "\n";
      exit (1);
    }
  catch (...)
    {
      CERR << "Unhandled exception\n";
      exit (1);
    }
}

extern "C" void EXPORT
daisy_daisy_tick_action (Daisy* daisy)
{
  try
    {
      daisy->action.doIt (*daisy);
    }
  catch (const char* error)
    {
      CERR << "Exception: " << error << "\n";
      exit (1);
    }
  catch (...)
    {
      CERR << "Unhandled exception\n";
      exit (1);
    }
}

extern "C" void EXPORT
daisy_daisy_tick_weather (Daisy* daisy)
{
  if (!daisy->weather)
    return;

  try
    {
      daisy->weather->tick (daisy->time); 
    }
  catch (const char* error)
    {
      CERR << "Exception: " << error << "\n";
      exit (1);
    }
  catch (...)
    {
      CERR << "Unhandled exception\n";
      exit (1);
    }
}

extern "C" void EXPORT
daisy_daisy_tick_columns (Daisy* daisy)
{
  try
    {
      daisy->tick_columns ();
    }
  catch (const char* error)
    {
      CERR << "Exception: " << error << "\n";
      exit (1);
    }
  catch (...)
    {
      CERR << "Unhandled exception\n";
      exit (1);
    }
}

extern "C" void EXPORT
daisy_daisy_tick_column (Daisy* daisy, int col)
{
  try
    {
      daisy->field.find (col)->tick (daisy->time, daisy->weather);
    }
  catch (const char* error)
    {
      CERR << "Exception: " << error << "\n";
      exit (1);
    }
  catch (...)
    {
      CERR << "Unhandled exception\n";
      exit (1);
    }
}

extern "C" void EXPORT
daisy_daisy_tick_logs (Daisy* daisy)
{
  try
    {
      daisy->tick_logs (); 
    }
  catch (const char* error)
    {
      CERR << "Exception: " << error << "\n";
      exit (1);
    }
  catch (...)
    {
      CERR << "Unhandled exception\n";
      exit (1);
    }
}

extern "C" void EXPORT		// Run time a single time step.
daisy_daisy_tick_time (Daisy* daisy)
{
  try
    {
      daisy->time.tick_hour (); 
    }
  catch (const char* error)
    {
      CERR << "Exception: " << error << "\n";
      exit (1);
    }
  catch (...)
    {
      CERR << "Unhandled exception\n";
      exit (1);
    }
}

// @@ Manipulating the simulation.

extern "C" Time* EXPORT
daisy_daisy_get_time (Daisy* daisy)
{ return &daisy->time; }

extern "C" Weather* EXPORT
daisy_daisy_get_weather (Daisy* daisy)
{ return daisy->weather; }

extern "C" unsigned int EXPORT
daisy_daisy_count_columns (const Daisy* daisy)
{ return daisy->field.size (); }

extern "C" Column* EXPORT
daisy_daisy_get_column (Daisy* daisy, int col)
{ 
  assert (daisy);
  assert (col >= 0 && col < daisy->field.size ()); 
  return daisy->field.find (col); 
}

extern "C" void EXPORT
daisy_daisy_append_column (Daisy* /* daisy */, Column* /*column*/)
{ /* BUG: unimplemented */ }

extern "C" void EXPORT
daisy_daisy_remove_column (Daisy* /*daisy*/, Column* /*column*/)
{ /* BUG: unimplemented */ }

// @ The daisy_time Type.

extern "C" Time* EXPORT
daisy_time_create (int year, int month, int mday, int hour)
{ return new Time (year, month, mday, hour); }

extern "C" void EXPORT 
daisy_time_delete (Time* time)
{ delete time; }

extern "C" int EXPORT
daisy_time_get_hour (Time* time)
{ return time->hour (); }

extern "C" int EXPORT
daisy_time_get_mday (Time* time)
{ return time->mday (); }

extern "C" int EXPORT
daisy_time_get_month (Time* time)
{ return time->month (); }

extern "C" int EXPORT
daisy_time_get_year (Time* time)
{ return time->year (); }

// @ The daisy_weather Type.

extern "C" void EXPORT
daisy_weather_put_precipitation (Weather* weather, double prec)
{ weather->put_precipitation (prec); }

extern "C" void EXPORT
daisy_weather_put_air_temperature (Weather* weather, double T)
{ weather->put_air_temperature (T); }

extern "C" void EXPORT
daisy_weather_put_reference_evapotranspiration (Weather* weather, double ref)
{ weather->put_reference_evapotranspiration (ref); }

extern "C" void EXPORT
daisy_weather_put_global_radiation (Weather* weather, double radiation)
{ weather->put_global_radiation (radiation); }

// @ The daisy_column Type.

// @@ Cloning and merging columns.

extern "C" Column* EXPORT
daisy_column_clone (const Column* /*column*/, const char* /*name*/)
{ /* BUG: unimplemented */ return NULL; }

extern "C" void EXPORT
daisy_column_merge (Column* /*column*/, const Column* /*other*/, double /*weight*/)
{ /* BUG: unimplemented */ }

// @@ Manipulating the column state.

extern "C" const char* EXPORT
daisy_column_get_name (const Column* column)
{ return column->name.c_str (); }

// @@@ Soil Geometry.

extern "C" unsigned int EXPORT
daisy_column_count_layers (const Column* column)
{ return column->count_layers (); }

extern "C" double EXPORT	// Heigh of numeric lay `lay' in cm.
daisy_column_get_dz (const Column* column, int lay)
{ return column->get_dz (lay); }

// @@@ Soil Water. 
//
// Water content of the soil.

extern "C" void EXPORT		// [cm]
daisy_column_put_water_pressure (Column* column, const double h[])
{ 
  assert (h);

  // Convert to vector.
  vector<double> v;
  unsigned int size = column->count_layers ();
  for (unsigned int i = 0; i < size; i++)
    v.push_back (h[i]);
  
  column->put_water_pressure (v); 
}

extern "C" void EXPORT		// [cm^3/cm^3/h]
daisy_column_get_water_sink (const Column* column, double sink[])
{ 
  // Get sink.
  vector<double> v;
  column->get_water_sink (v); 
  unsigned int size = column->count_layers ();
  assert (v.size () <= size);

  // Store v in sink..
  unsigned int i = 0;
  for (; i < v.size (); i++)
    sink[i] = v[i];
  for (; i < size; i++)
    sink[i] = 0.0;
}

extern "C" double EXPORT	// [cm^3/cm^3]
daisy_column_get_water_content_at (const Column* column, 
				   unsigned int index)
{ return column->get_water_content_at (index); }

// @@@ Soil Nitrate. 
// 
// Nitrate solution in the soil.

extern "C" void EXPORT		// [g/cm^3]
daisy_column_put_no3_m (Column* column, const double M[])
{ 
  // Convert to vector.
  vector<double> v;
  unsigned int size = column->count_layers ();
  for (unsigned int i = 0; i < size; i++)
    v.push_back (M[i]);
  
  column->put_no3_m (v); 
}

extern "C" void EXPORT		// [g/cm^3]
daisy_column_get_no3_m (Column* column, double M[])
{ 
  // Get NO3.
  vector<double> v;
  column->get_no3_m (v); 
  unsigned int size = column->count_layers ();
  assert (v.size () <= size);

  // Store NO3 in M.
  unsigned int i = 0;
  for (; i < v.size (); i++)
    M[i] = v[i];
  for (; i < size; i++)
    M[i] = 0.0;
}

// @@@ Bioclimate. 
//
// What happens in the canopy?

extern "C" double EXPORT	// [mm/h]
daisy_column_get_evap_interception (const Column* column)
{ return column->get_evap_interception (); }

extern "C" double EXPORT	// [mm]
daisy_column_get_intercepted_water (const Column* column)
{ return column->get_intercepted_water (); }

extern "C" double EXPORT	// [mm/h]
daisy_column_get_net_throughfall (const Column* column)
{ return column->get_net_throughfall (); }

// @@@ Surface.
// 
// The surface manages anything that lies on top of the soil.

extern "C" double EXPORT	// [mm/h]
daisy_column_get_evap_soil_surface (const Column* column)
{ return column->get_evap_soil_surface (); }

extern "C" double EXPORT	// [mm/h]
daisy_column_get_exfiltration (const Column* column)
{ return column->get_exfiltration (); }

extern "C" void EXPORT		// [mm]
daisy_column_put_ponding (Column* column, double pond)
{ column->put_ponding (pond); }

extern "C" void	 EXPORT		// [g/cm^2]
daisy_column_put_surface_no3 (Column* column, double no3)
{ column->put_surface_no3  (no3); }

extern "C" double EXPORT	// [g/cm^2]
daisy_column_get_surface_no3 (const Column* column)
{ return column->get_surface_no3 (); }

extern "C" double EXPORT	// [mm]
daisy_column_get_snow_storage (const Column* column)
{ return column->get_snow_storage (); }

extern "C" void EXPORT		// [g/cm^2]
daisy_column_put_surface_chemical (Column* column, 
				   const char* name, double amount)
{ column->put_surface_chemical (name, amount); }

extern "C" double EXPORT	// [g/cm^2]
daisy_column_get_surface_chemical (const Column* column,
				   const char* name)
{ return column->get_surface_chemical (name); }


// @@@ Organic Matter.
// 
// The organic content of the soil.

extern "C" double EXPORT	// [g C/cm³]
daisy_column_get_smb_c_at (Column* column, unsigned int index)
{ return column->get_smb_c_at (index); }

extern "C" double EXPORT	// [g C/cm³]
daisy_column_get_co2_at (Column* column, unsigned int index)
{ return column->get_co2_production_at (index); }

// @@@ Soil Heat.
// 
// Temperature of soil.

extern "C" double EXPORT	// [°C]
daisy_column_get_temperature_at (Column* column, unsigned int index)
{ return column->get_temperature_at (index); }


// @@@ Crops.
//
// What grows in the column.

extern "C" double EXPORT	// [cm³ H2O/cm³/h]
daisy_column_get_crop_h2o_uptake_at (Column* column, unsigned int index)
{ return column->get_crop_h2o_uptake_at (index); }

// @ The daisy_chemical Type.
//
// Contains information about chemicals known by Daisy.

extern "C" Chemical* EXPORT		// Return the chemical named NAME.
daisy_chemical_find (const char* name)
{
  const Library& chemlib = Library::find ("chemical");
  if (chemlib.check (name))
    return &Librarian<Chemical>::create (chemlib.lookup (name));
  return NULL;
}

extern "C" double EXPORT	// The crop uptake reflection factor.
daisy_chemical_reflection_factor (const Chemical* chemical)
{ return chemical->crop_uptake_reflection_factor (); }

// @ The daisy_log Type.
//
// Extract information from the `extern' log model.

extern "C" LogExternSource::type EXPORT
daisy_log_lookup (const char* log, const char* tag)
{ return LogExternSource::find (log).lookup (tag); }

extern "C" double  EXPORT
daisy_log_get_number (const char* log, const char* tag)
{ return LogExternSource::find (log).number (tag); }

extern "C" const char*  EXPORT
daisy_log_get_name (const char* log, const char* tag)
{ return LogExternSource::find (log).name (tag).c_str (); }

extern "C" void EXPORT
daisy_log_get_array (const char* log, const char* tag, double value[])
{ 
  const vector<double>& array =  LogExternSource::find (log).array (tag); 
  
  copy (array.begin (), array.end (), value);
}

// @ Miscellaneous.
//
// Other functions which doesn't fit nicely into the above categories.

extern "C" void EXPORT
daisy_load (Syntax* syntax, AttributeList* alist)
{ 
  Daisy::load_syntax (*syntax, *alist); 
  Library::load_syntax (*syntax, *alist); 
}

extern "C" void EXPORT
daisy_initialize ()
{
#ifdef __unix
  // We should do the appropriate magic on Unix.
#else
  // We should load the DLL here on MSDOS.
#endif
}

extern "C" const char* EXPORT
daisy_version ()
{ return version; }
