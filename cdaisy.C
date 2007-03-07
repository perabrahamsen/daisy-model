// @ cdaisy.C --- C interface to daisy.
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
//
// This file is part of Daisy.
// 
// Daisy is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser Public License as published by
// the Free Software Foundation; either version 2.1 of the License, or
// (at your option) any later version.
// 
// Daisy is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser Public License for more details.
// 
// You should have received a copy of the GNU Lesser Public License
// along with Daisy; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

//
// See 'cdaisy.h' for more documentation.

#include "scope.h"
#include "block.h"
#include "library.h"
#include "syntax.h"
#include "alist.h"
#include "daisy.h"
#include "toplevel.h"
#include "parser_file.h"
#include "time.h"
#include "field.h"
#include "column.h"
#include "weather.h"
#include "action.h"
#include "horizon.h"
#include "printer_file.h"
#include "version.h"
#include "chemical.h"
#include "log_extern.h"
#include "treelog_stream.h"
#include <fstream>
#include <iostream>
#include <string>

using namespace std;
/*bgj
#ifdef __BORLANDC__
#define EXPORT _export
#define IMPORT _import
#else
#define EXPORT
#define IMPORT
#endif
*/

#ifdef MINGW
#ifdef BUILD_DLL
/* DLL export */
#define EXPORT __declspec(dllexport)
#else
/* EXE import */
#define EXPORT __declspec(dllimport)
#endif
#else // !MINGW
#define EXPORT
#endif

typedef int daisy_bool;

// @ The daisy_toplevel Type.

extern "C" EXPORT Toplevel*
daisy_toplevel_create_with_log (const char* logname)
{
  try 
    { return new Toplevel (logname); }
  catch (...)
    { return NULL; }
}

extern "C" EXPORT void
daisy_toplevel_parse_command_line (Toplevel* toplevel,
                                   int argc, char** argv)
{
  try
    { toplevel->command_line (argc, argv); }
  catch (int i)
    { 
      if (i != EXIT_SUCCESS)
        toplevel->error ("Command line parsing failure");
    }
  catch (...)
    { toplevel->error ("Command line parsing failed"); }
}

extern "C" EXPORT void
daisy_toplevel_parse_file (Toplevel* toplevel, char* filename)
{
  try 
    { toplevel->parse_file (filename); }
  catch (...)
    { toplevel->error ("Error while parsing '" + string (filename) + "'"); }
}

extern "C" EXPORT const Syntax*
daisy_toplevel_get_program_syntax (Toplevel* toplevel)
{ return &toplevel->program_syntax (); }

extern "C" EXPORT const AttributeList*
daisy_toplevel_get_program_alist (Toplevel* toplevel)
{ return &toplevel->program_alist (); }

extern "C" EXPORT void
daisy_toplevel_initialize (Toplevel* toplevel)
{ 
  try 
    { toplevel->initialize (); }
  catch (...)
    { toplevel->error ("Error while initializing"); }
}

extern "C" EXPORT Daisy*
daisy_toplevel_get_daisy (Toplevel* toplevel)
{ return dynamic_cast<Daisy*> (&toplevel->program ()); }

extern "C" EXPORT void
daisy_toplevel_run (Toplevel* toplevel)
{
  try 
    { toplevel->run (); }
  catch (...)
    { toplevel->error ("Error while running program"); }
}

extern "C" EXPORT void
daisy_toplevel_error (Toplevel* toplevel, char* message)
{ toplevel->error (message); }

extern "C" EXPORT bool
daisy_toplevel_ok (Toplevel* toplevel)
{ return toplevel->state () != Toplevel::is_error; }

extern "C" EXPORT bool
daisy_toplevel_done (Toplevel* toplevel)
{ return toplevel->state () == Toplevel::is_done; }

extern "C" EXPORT void     
daisy_toplevel_delete (Toplevel* toplevel)
{ delete toplevel; }

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
{ 
  TreelogStream treelog (cerr);
  Treelog::Open nest (treelog, name);
  return syntax->check (*alist, treelog); 
}

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
{ return Syntax::category_name (number).c_str (); }

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
{ return Syntax::type_name (number).c_str (); }

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
{ return alist->identifier_sequence (name).size (); }

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
{ return alist->identifier_sequence (name)[index].name ().c_str (); }

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
  vector<symbol> v;
  if (alist->check (name))
    v = alist->identifier_sequence (name);
  if (v.size () <= index)
    while (v.size () <= index)
      v.push_back (symbol (value));
  else
    v[index] = symbol (value);
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
{ return &Library::find (symbol (name)); }

extern "C" int EXPORT
daisy_library_size (const Library* library)
{
  vector<symbol> entries;
  library->entries (entries);
  return entries.size ();
}

extern "C" const char* EXPORT
daisy_library_name (const Library* library, const unsigned int index)
{
  vector<symbol> entries;
  library->entries (entries);
  return entries[index].name ().c_str ();
}

extern "C" const Syntax* EXPORT
daisy_library_syntax (const Library* library, const char* name)
{ return &library->syntax (symbol (name)); }

extern "C" const AttributeList* EXPORT
daisy_library_alist (const Library* library, const char* name)
{ return &library->lookup (symbol (name)); }

extern "C" const char* EXPORT
daisy_library_file (const Library* library, const char* name)
{ 
  const AttributeList& alist = library->lookup (symbol (name));
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
    }
  library->add_derived (symbol (name), *alist, symbol (super));
}

extern "C" void EXPORT
daisy_library_remove (Library* library, const char* name)
{ library->remove (symbol (name)); }

// @ The daisy_parser Type.

extern "C" Parser* EXPORT
daisy_parser_create_file (Syntax* syntax, const char* filename)
{ 
  static TreelogStream treelog (cerr);
  static Treelog::Open nest (treelog, "parser");
  return new ParserFile (*syntax, filename, treelog); 
}

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

#if 0
extern "C" void EXPORT
daisy_printer_alist (Printer* printer, 
		     const AttributeList* alist, const Syntax* syntax,
		     const AttributeList* super)
{ printer->print_alist (*alist, *syntax, *super); }
#endif

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
  static TreelogStream treelog (cerr);
  static Treelog::Open nest (treelog, "daisy");
  Block block (*syntax, *alist, treelog, "Daisy");
  Daisy* daisy = new Daisy (block); 
  daisy_assert (block.ok ());
  daisy_assert (daisy);
  daisy->initialize (syntax, alist, treelog);
  return daisy;
}

extern "C" void EXPORT
daisy_daisy_delete (Daisy* daisy)
{ delete daisy; }

extern "C" daisy_bool EXPORT	// Check context.
daisy_daisy_check (Daisy* daisy)
{ 
  TreelogStream treelog (cerr);
  Treelog::Open nest (treelog, "Daisy");
  return daisy->check (treelog); 
}

// @@ Running the simulation.

extern "C" void EXPORT	
daisy_daisy_run (Daisy* daisy)
{
  try
    {
      TreelogStream treelog (cerr);
      daisy->run (treelog); 
    }
  catch (const char* error)
    {
      cerr << "Exception: " << error << "\n";
      exit (1);
    }
  catch (...)
    {
      cerr << "Unhandled exception\n";
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
      TreelogStream treelog (cerr);
      daisy->tick (treelog); 
    }
  catch (const char* error)
    {
      cerr << "Exception: " << error << "\n";
      exit (1);
    }
  catch (...)
    {
      cerr << "Unhandled exception\n";
      exit (1);
    }
}

extern "C" void EXPORT
daisy_daisy_tick_before (Daisy* daisy)
{
  try
    {
      TreelogStream msg (cerr);
      daisy->tick_before (msg);
    }
  catch (const char* error)
    {
      cerr << "Exception: " << error << "\n";
      exit (1);
    }
  catch (...)
    {
      cerr << "Unhandled exception\n";
      exit (1);
    }
}

extern "C" void EXPORT
daisy_daisy_tick_columns (Daisy* daisy)
{
  try
    {
      TreelogStream treelog (cerr);
      daisy->tick_columns (treelog);
    }
  catch (const char* error)
    {
      cerr << "Exception: " << error << "\n";
      exit (1);
    }
  catch (...)
    {
      cerr << "Unhandled exception\n";
      exit (1);
    }
}

extern "C" void EXPORT
daisy_daisy_tick_column (Daisy* daisy, int col)
{
  try
    {
      TreelogStream msg (cerr);
      daisy->tick_column (col, msg);
    }
  catch (const char* error)
    {
      cerr << "Exception: " << error << "\n";
      exit (1);
    }
  catch (...)
    {
      cerr << "Unhandled exception\n";
      exit (1);
    }
}

extern "C" void EXPORT
daisy_daisy_tick_after (Daisy* daisy)
{
  try
    {
      TreelogStream treelog (cerr);
      daisy->tick_after (treelog);
    }
  catch (const char* error)
    {
      cerr << "Exception: " << error << "\n";
      exit (1);
    }
  catch (...)
    {
      cerr << "Unhandled exception\n";
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
daisy_daisy_get_column (Daisy* daisy, const int col)
{ 
  daisy_assert (daisy);
  daisy_assert (col >= 0 && col < daisy->field.size ()); 
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
{ return column->name.name ().c_str (); }

// @@@ Soil Geometry.

#ifdef EXCOM

extern "C" unsigned int EXPORT
daisy_column_count_layers (const Column* column)
{ return column->count_layers (); }

extern "C" double EXPORT	// Heigh of numeric lay 'lay' in cm.
daisy_column_get_dz (const Column* column, int lay)
{ return column->get_dz (lay); }

// @@@ Soil Water. 
//
// Water content of the soil.

extern "C" void EXPORT		// [cm]
daisy_column_put_water_pressure (Column* column, const double h[])
{ 
  daisy_assert (h);

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
  daisy_assert (v.size () <= size);

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
  daisy_assert (v.size () <= size);

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
{ column->put_surface_chemical (symbol (name), amount); }

extern "C" double EXPORT	// [g/cm^2]
daisy_column_get_surface_chemical (const Column* column,
				   const char* name)
{ return column->get_surface_chemical (symbol (name)); }


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

#endif // EXCOM

// @ The daisy_chemical Type.
//
// Contains information about chemicals known by Daisy.

extern "C" Chemical* EXPORT		// Return the chemical named NAME.
daisy_chemical_find (const char* name)
{
  const Library& chemlib = Library::find (symbol ("chemical"));
  const symbol sym (name);
  if (chemlib.check (sym))
    {
      Syntax parent_syntax;
      AttributeList parent_alist;
      Block block (parent_syntax, parent_alist, Treelog::null (), "chemical");
      return Librarian<Chemical>::build_alist (block, chemlib.lookup (sym), 
					       name);
    }
  return NULL;
}

extern "C" double EXPORT	// The crop uptake reflection factor.
daisy_chemical_reflection_factor (const Chemical* chemical)
{ return chemical->crop_uptake_reflection_factor (); }

// @ The daisy_scope Type.
//
// Extract information from the 'extern' log model.
extern "C" unsigned int EXPORT  // Return number of extern scopes
daisy_scope_extern_size ()
{
  return extern_scope_size ();
}

extern "C" const Scope* EXPORT  // Return extern scope INDEX.
daisy_scope_extern_get (const unsigned int index)
{
  return extern_scope_get (index);
}

extern "C" unsigned int EXPORT // Number of numbers in SCOPE.
daisy_scope_number_size (const Scope *const scope)
{ return scope->all_numbers ().size (); }

extern const char* EXPORT       // Name of number INDEX in SCOPE.
daisy_scope_number_name (const Scope *const scope, const unsigned int index)
{ return scope->all_numbers ()[index].name ().c_str (); }

extern "C" const int EXPORT	// check if NAME is defined in SCOPE.
daisy_scope_has_number (const Scope* scope, const char* name)
{ 
  if (scope->has_number (symbol (name)))
    return 1;
  else 
    return 0; 
}

extern "C" const double EXPORT	// Return numeric value of NAME in SCOPE.
daisy_scope_number (const Scope* scope, const char* name)
{ 
  return (scope->number (symbol (name)));
}

extern "C" const char* EXPORT	// Return UNITS of NAME defined in SCOPE.
daisy_scope_dimension (const Scope* scope, const char* name)
{ 
  return scope->dimension (symbol (name)).name ().c_str ();
}

extern "C" const int EXPORT	// check if NAME is defined in SCOPE.
daisy_scope_has_string (const Scope* scope, const char* name)
{ 
  if (scope->has_identifier (symbol (name)))
    return 1;
  else 
    return 0; 
}

extern "C" const char* EXPORT	// Return string value of NAME in SCOPE.
daisy_scope_string (const Scope* scope, const char* name)
{ 
  return scope->identifier (symbol (name)).name ().c_str ();
}

extern "C" const char* EXPORT	// Return UNITS of NAME defined in SCOPE.
daisy_scope_description (const Scope* scope, const char* name)
{ return scope->get_description (symbol (name)).name().c_str (); }

// @ Miscellaneous.
//
// Other functions which doesn't fit nicely into the above categories.

extern "C" void EXPORT
daisy_load (Syntax* syntax, AttributeList* alist)
{ 
  Daisy::load_syntax (*syntax, *alist); 
  alist->add ("type", "Daisy");
  Library::load_syntax (*syntax, *alist); 
  syntax->add ("directory", Syntax::String, Syntax::OptionalConst,
	      "Run program in this directory.\n\
This can affect both where input files are found and where log files\n\
are generated.");
  syntax->add ("path", Syntax::String,
	      Syntax::OptionalConst, Syntax::Sequence,
	      "List of directories to search for input files in.\n\
The special value \".\" means the current directory.");
  syntax->add ("input", Librarian<Parser>::library (),
	      Syntax::OptionalConst, Syntax::Singleton,
	      "Command to add more information about the simulation.");
  syntax->add ("run", Librarian<Program>::library (), 
	      Syntax::OptionalState, Syntax::Singleton, 
	      "Program to run.\n\
\n\
If this option is specified, all the 'Daisy' specific top-level attributes\n\
will be ignored.  If unspecified, run 'Daisy' on the current top-level\n\
attributes.");
}

extern "C" void EXPORT
daisy_initialize ()
{
  // Options::initialize_path ();

#ifdef __unix
  // We should do the appropriate magic on Unix.
#else
  // We should load the DLL here on MSDOS.
#endif
}

extern "C" const char* EXPORT
daisy_version ()
{ return version; }
