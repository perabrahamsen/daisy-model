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
#include "horizon.h"

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
{ return syntax->check (*alist, name); }

extern "C" void EXPORT
daisy_syntax_add (Syntax* syntax, const char* name,
		  Syntax::category cat, Syntax::type type, int size)
{ syntax->add (name, type, cat, size); }

extern "C" void EXPORT
daisy_syntax_add_alist (Syntax* syntax, const char* name,
			Syntax::category cat, Syntax* nested, int size)
{ syntax->add (name, *nested, cat, size); }

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

extern "C" void EXPORT
daisy_alist_delete (AttributeList* alist)
{ delete alist; }


extern "C" int EXPORT
daisy_alist_save (const AttributeList* alist, const Syntax* syntax,
		  const char* file)
{ 
  ofstream out (file);
  alist->dump (out, *syntax);
  return out.good ();
}

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
{ return alist->alist_sequence (name).size (); }

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
  if (v.size () < index)
    while (v.size () < index)
      v.push_back (value);
  else
    v[index] = value;
  alist->add (name, v);
}

extern "C" void EXPORT
daisy_alist_set_string_at (AttributeList* alist, const char* name,
			   const char* value, unsigned int index)
{
  vector<string>& v = alist->check (name)
    ? *new vector<string> (alist->name_sequence (name))
    : *new vector<string>;
  if (v.size () < index)
    while (v.size () < index)
      v.push_back (value);
  else
    v[index] = value;
  alist->add (name, v);
}

extern "C" void EXPORT
daisy_alist_set_flag_at (AttributeList* alist, const char* name,
			 daisy_bool value, unsigned int index)
{ 
  vector<bool>& v = alist->check (name)
    ? *new vector<bool> (alist->flag_sequence (name))
    : *new vector<bool>;
  if (v.size () < index)
    while (v.size () < index)
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
  if (v.size () < index)
    while (v.size () < index)
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
    alist->add ("parsed_from_file", filename);

  if (library == &Librarian<Horizon>::library ())
    Librarian<Horizon>::derive_type (name, *alist, super);
  else if (library == &Librarian<Column>::library ())
    Librarian<Column>::derive_type (name, *alist, super);
  else if (library == &Action::library ())
    Action::derive_type (name, *alist, super);
  else if (library == &Librarian<Weather>::library ())
    Librarian<Weather>::derive_type (name, *alist, super);
  else 
    abort ();
}

extern "C" void EXPORT
daisy_library_remove (Library* library, const char* name)
{ library->remove (name); }

extern "C" int EXPORT
daisy_library_save_file (const char* filename)
{
  ofstream out (filename);

  if (!out.good ())
    return -1;
  
  out << ";; Automated dump of daisy libraries to " << filename << "\n";

  vector<string> all;
  Library::all (all);
  const string def ("def");

  for (unsigned int i = 0; i < all.size (); i++)
    {
      Library& library = Library::find (all[i]);
      vector<string> elements;
      library.entries (elements);
      
      for (unsigned int j = 0; j < elements.size (); j++)
	{
	  const AttributeList& alist = library.lookup (elements[j]);

	  if (alist.check ("parsed_from_file") 
	      && alist.name ("parsed_from_file") == filename)
	    {
	      out << "\n(def" << all[i] << " " << elements[j] << "\n  ";
	      alist.dump (out, library.syntax (elements[j]), 2);
	      out << ")\n";
	    }
	}
    }

  if (!out.good ())
    return -1;

  return 0;
}

// @ The daisy_parser Type.

extern "C" Parser* EXPORT
daisy_parser_create_file (const Syntax* syntax, const char* filename)
{ return new ParserFile (*syntax, filename); }

extern "C" void EXPORT
daisy_parser_delete (Parser* parser)
{ delete parser; }

extern "C" void EXPORT
daisy_parser_load (Parser* parser, AttributeList* alist)
{ parser->load (*alist); }

// @ The daisy_daisy Type.

extern "C" Daisy* EXPORT
daisy_daisy_create (const AttributeList* alist)
{ return new Daisy (*alist); }

extern "C" void EXPORT
daisy_daisy_delete (Daisy* daisy)
{ delete daisy; }

extern "C" daisy_bool EXPORT	// Check context.
daisy_daisy_check (Daisy* daisy, const Syntax* syntax)
{ return daisy->check (*syntax); }

// @@ Running the simulation.

extern "C" void EXPORT	
daisy_daisy_run (Daisy* daisy)
{ daisy->run (); }

extern "C" void EXPORT
daisy_daisy_start (Daisy* daisy)
{ daisy->running = true; }

extern "C" daisy_bool EXPORT
daisy_daisy_is_running (Daisy* daisy)
{ return daisy->running; }

extern "C" void EXPORT
daisy_daisy_tick (Daisy* daisy)
{ daisy->tick (); }

extern "C" void EXPORT
daisy_daisy_tick_action (Daisy* daisy)
{ daisy->action.doIt (*daisy); }

extern "C" void EXPORT
daisy_daisy_tick_weather (Daisy* daisy)
{ daisy->weather.tick (daisy->time); }

extern "C" void EXPORT
daisy_daisy_tick_columns (Daisy* daisy)
{
  daisy->tick_columns ();
}

extern "C" void EXPORT
daisy_daisy_tick_column (Daisy* daisy, int col)
{ 
  daisy->columns[col]->tick (daisy->time, 
			     daisy->weather); 
}

extern "C" void EXPORT
daisy_daisy_tick_logs (Daisy* daisy)
{ daisy->tick_logs (); }

extern "C" void EXPORT		// Run time a single time step.
daisy_daisy_tick_time (Daisy* daisy)
{ daisy->time.tick_hour (); }

// @@ Manipulating the simulation.

extern "C" Time* EXPORT
daisy_daisy_get_time (Daisy* daisy)
{ return &daisy->time; }

extern "C" Weather* EXPORT
daisy_daisy_get_weather (Daisy* daisy)
{ return &daisy->weather; }

extern "C" int EXPORT
daisy_daisy_count_columns (const Daisy* daisy)
{ return daisy->columns.size (); }

extern "C" Column* EXPORT
daisy_daisy_get_column (Daisy* daisy, int col)
{ return daisy->columns[col]; }

extern "C" void EXPORT
daisy_daisy_append_column (Daisy* daisy, Column* column);

extern "C" void EXPORT
daisy_daisy_remove_column (Daisy* daisy, Column* column);

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
daisy_weather_put_precipitation (Weather* column, double prec);

extern "C" void EXPORT
daisy_weather_put_air_temperature (Weather* column, double T);

extern "C" void EXPORT
daisy_weather_put_reference_evapotranspiration (Weather* column, double ref);

// @ The daisy_column Type.

// @@ Cloning and merging columns.

extern "C" Column* EXPORT
daisy_column_clone (const Column* column, const char* name);

extern "C" void EXPORT
daisy_column_merge (Column* column, const Column* other, double weight);

// @@ Manipulating the column state.

extern "C" const char* EXPORT
daisy_column_get_name (const Column* column)
{ return column->name.c_str (); }

// @@@ Soil Geometry.

extern "C" int EXPORT
daisy_column_count_layers (const Column* column);

extern "C" double EXPORT	// Heigh of numeric lay `lay' in cm.
daisy_column_get_dz (const Column* column, int lay);

// @@@ Soil Water. 
//
// Water content of the soil.

extern "C" void EXPORT		// [cm]
daisy_column_put_water_pressure (Column* column, const double h[]);

extern "C" void EXPORT		// [cm^3/cm^3/h]
daisy_column_get_water_sink (const Column* column, double sink[]);

// @@@ Soil Nitrate. 
// 
// Nitrate solution in the soil.

extern "C" void EXPORT		// [g/cm^3]
daisy_column_put_no3_m (const double M[]);

extern "C" void EXPORT		// [g/cm^3]
daisy_column_get_no3_m (double M[]);

// @@@ Bioclimate. 
//
// What happens in the canopy?

extern "C" double EXPORT	// [mm/h]
daisy_column_get_evap_interception (const Column* column);

extern "C" double EXPORT	// [mm]
daisy_column_get_intercepted_water (const Column* column);

extern "C" double EXPORT	// [mm/h]
daisy_column_get_net_precipitation (const Column* column);

// @@@ Surface.
// 
// The surface manages anything that lies on top of the soil.

extern "C" double EXPORT	// [mm/h]
daisy_column_get_evap_soil_surface (const Column* column);

extern "C" double EXPORT	// [mm/h]
daisy_column_get_evap_pond (const Column* column);

extern "C" void EXPORT		// [mm]
daisy_column_put_ponding (Column* column, double pond);

extern "C" void	 EXPORT		// [g/cm^2]
daisy_column_put_surface_no3 (Column* column, double no3);

extern "C" double EXPORT	// [g/cm^2]
daisy_column_get_surface_no3 (const Column* column);

extern "C" double EXPORT	// [mm]
daisy_column_get_snow_height (const Column* column);

// @ Miscellaneous.
//
// Other functions which doesn't fit nicely into the above categories.

extern "C" void EXPORT
daisy_load (Syntax* syntax, AttributeList* alist)
{ Daisy::load_syntax (*syntax, *alist); }

extern "C" void EXPORT
daisy_initialize ()
{
#ifdef __unix
  // We should do the appropriate magic on Unix.
#else
  // We should load the DLL here on MSDOS.
#endif
}
