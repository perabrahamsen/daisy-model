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
#include "output.h"
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
#include "scope.h"
#include "assertion.h"
#include <string>
#include <typeinfo>

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

#define DAISY_CATCH_BLOCK(toplevel) \
  catch (const char* error) \
    { toplevel->error ("Exception: " + std::string (error)); } \
  catch (const std::string& error) \
    { toplevel->error ("Exception raised: " + error); } \
  catch (const std::exception& e) \
    { toplevel->error (std::string ("Standard exception: ") \
                      + typeid (e).name () + ": " + e.what ()); } \
  catch (...) \
    { toplevel->error ("Unhandled exception"); }

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
		    const char* name, Toplevel* toplevel)
{ 
  Treelog::Open nest (*toplevel->msg, name);
  return syntax->check (toplevel->metalib (), *alist, *toplevel->msg); 
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
{ return alist->check (name); }

extern "C" int EXPORT
daisy_alist_get_integer (const AttributeList* alist, const char* name)
{ return alist->integer (name); }

extern "C" double EXPORT
daisy_alist_get_number (const AttributeList* alist, const char* name)
{ return alist->number (name); }

extern "C" const char* EXPORT
daisy_alist_get_string (const AttributeList* alist, const char* name)
{ return alist->name (name).c_str (); }

extern "C" daisy_bool EXPORT
daisy_alist_get_flag (const AttributeList* alist, const char* name)
{ return alist->flag (name); }

extern "C" const AttributeList* EXPORT
daisy_alist_get_alist (const AttributeList* alist, const char* name)
{ return &alist->alist (name); }

extern "C" void EXPORT
daisy_alist_set_integer (AttributeList* alist, const char* name, int value)
{ alist->add (name, value); }

extern "C" void EXPORT
daisy_alist_set_number (AttributeList* alist, const char* name, double value)
{ alist->add (name, value); }

extern "C" void EXPORT
daisy_alist_set_string (AttributeList* alist, const char* name, 
                        const char* value)
{ alist->add (name, value); }

extern "C" void EXPORT
daisy_alist_set_flag (AttributeList* alist, const char* name, daisy_bool value)
{ alist->add (name, bool (value)); }

extern "C" void EXPORT
daisy_alist_set_alist (AttributeList* alist, const char* name,
		       AttributeList* value)
{ alist->add (name, *value); }

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
  std::vector<symbol> v;
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
  std::vector<double>& v= alist->check (name)
    ? *new std::vector<double> (alist->number_sequence (name))
    : *new std::vector<double>;
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
  std::vector<AttributeList*>& v = alist->check (name)
    ? *new std::vector<AttributeList*> (alist->alist_sequence (name))
    : *new std::vector<AttributeList*>;
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
daisy_library_find (Toplevel *const toplevel, const char *const name)
{ return &toplevel->metalib ().library (symbol (name)); }

extern "C" int EXPORT
daisy_library_size (const Library* library)
{
  std::vector<symbol> entries;
  library->entries (entries);
  return entries.size ();
}

extern "C" const char* EXPORT
daisy_library_name (const Library* library, const unsigned int index)
{
  std::vector<symbol> entries;
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
daisy_library_derive (Toplevel *const toplevel, Library* library, 
		      const char* super, AttributeList* alist, 
		      const char* name, const char* filename)
{ 
  if (filename)
    {
      alist->add ("parsed_from_file", filename);
      alist->add ("parsed_sequence", toplevel->metalib ().get_sequence ());
    }
  library->add_derived (symbol (name), *alist, symbol (super));
}

extern "C" void EXPORT
daisy_library_remove (Library* library, const char* name)
{ library->remove (symbol (name)); }

// @ The daisy_printer Type.

extern "C" Printer* EXPORT
daisy_printer_create_file (Toplevel *const toplevel,
                           const char *const filename)
{ return new PrinterFile (toplevel->metalib (), filename); }

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

// @@ Building the environment.

extern "C" EXPORT Toplevel*
daisy_daisy_create_with_log (const char* logname)
{
  try 
    { return new Toplevel (logname); }
  catch (...)
    { return NULL; }
}

extern "C" EXPORT void
daisy_daisy_parse_command_line (Toplevel* toplevel,
                                   int argc, char** argv)
{
  try
    { toplevel->command_line (argc, argv); }
  catch (int i)
    { 
      if (i != EXIT_SUCCESS)
        toplevel->error ("Command line parsing failure");
    }
  DAISY_CATCH_BLOCK(toplevel);
}

extern "C" EXPORT void
daisy_daisy_parse_file (Toplevel* toplevel, char* filename)
{
  try 
    {
      toplevel->parse_file (filename); 
      return;
    }
  DAISY_CATCH_BLOCK(toplevel);
  toplevel->error ("While parsing '" + std::string (filename) + "'");
}

extern "C" EXPORT const Syntax*
daisy_daisy_get_program_syntax (Toplevel* toplevel)
{ return &toplevel->program_syntax (); }

extern "C" EXPORT const AttributeList*
daisy_daisy_get_program_alist (Toplevel* toplevel)
{ return &toplevel->program_alist (); }

extern "C" EXPORT void
daisy_daisy_initialize (Toplevel* toplevel)
{ 
  try 
    { toplevel->initialize (); }
  catch (...)
    { toplevel->error ("Error while initializing"); }
}

extern "C" EXPORT int
daisy_daisy_is_daisy (Toplevel* toplevel)
{ return dynamic_cast<Daisy*> (&toplevel->program ()) != NULL; }

extern "C" EXPORT void
daisy_daisy_run (Toplevel* toplevel)
{
  try 
    { toplevel->run (); }
  catch (...)
    { toplevel->error ("Error while running program"); }
}

extern "C" EXPORT void
daisy_daisy_error (Toplevel* toplevel, char* message)
{ toplevel->error (message); }

extern "C" EXPORT bool
daisy_daisy_ok (Toplevel* toplevel)
{ return toplevel->state () != Toplevel::is_error; }

extern "C" EXPORT bool
daisy_daisy_done (Toplevel* toplevel)
{ return toplevel->state () == Toplevel::is_done; }

extern "C" EXPORT void     
daisy_daisy_delete (Toplevel* toplevel)
{ delete toplevel; }

// @@ Running the simulation.

extern "C" void EXPORT
daisy_daisy_start (Toplevel* toplevel)
{
  try
    {
      Daisy& daisy = dynamic_cast<Daisy&> (toplevel->program ());
      daisy.running = true; 
    }
  DAISY_CATCH_BLOCK(toplevel);
}

extern "C" daisy_bool EXPORT
daisy_daisy_is_running (Toplevel* toplevel)
{ 
  try
    {
      Daisy& daisy = dynamic_cast<Daisy&> (toplevel->program ());
      return daisy.running; 
    }
  DAISY_CATCH_BLOCK(toplevel);
  return false;
}

extern "C" void EXPORT
daisy_daisy_tick (Toplevel* toplevel)
{
  try
    {
      Daisy& daisy = dynamic_cast<Daisy&> (toplevel->program ());
      Treelog::Open nest (*toplevel->msg, daisy.time.print ());
      daisy.tick (*toplevel->msg); 
    }
  DAISY_CATCH_BLOCK(toplevel);
}

extern "C" void EXPORT
daisy_daisy_tick_before (Toplevel* toplevel)
{
  try
    {
      Daisy& daisy = dynamic_cast<Daisy&> (toplevel->program ());
      daisy.tick_before (*toplevel->msg);
    }
  DAISY_CATCH_BLOCK(toplevel);
}

extern "C" void EXPORT
daisy_daisy_tick_columns (Toplevel* toplevel)
{
  try
    {
      Daisy& daisy = dynamic_cast<Daisy&> (toplevel->program ());
      daisy.tick_columns (*toplevel->msg);
    }
  DAISY_CATCH_BLOCK(toplevel);
}

extern "C" void EXPORT
daisy_daisy_tick_column (Toplevel* toplevel, int col)
{
  try
    {
      Daisy& daisy = dynamic_cast<Daisy&> (toplevel->program ());
      daisy.tick_column (col, *toplevel->msg);
    }
  DAISY_CATCH_BLOCK(toplevel);
}

extern "C" void EXPORT
daisy_daisy_tick_after (Toplevel* toplevel)
{
  try
    {
      Daisy& daisy = dynamic_cast<Daisy&> (toplevel->program ());
      daisy.tick_after (*toplevel->msg);
    }
  DAISY_CATCH_BLOCK(toplevel);
}

// @@ Manipulating the simulation.

extern "C" Time* EXPORT
daisy_daisy_get_time (Toplevel* toplevel)
{
  try
    {
      Daisy& daisy = dynamic_cast<Daisy&> (toplevel->program ());
      return &daisy.time; 
    }
  DAISY_CATCH_BLOCK(toplevel);
  return NULL;
}

extern "C" unsigned int EXPORT
daisy_daisy_count_columns (Toplevel *const toplevel)
{ 
  try
    {
      Daisy& daisy = dynamic_cast<Daisy&> (toplevel->program ());
      return daisy.field->size (); 
    }
  DAISY_CATCH_BLOCK(toplevel);
  return 0;
}

extern "C" Column* EXPORT
daisy_daisy_get_column (Toplevel* toplevel, const int col)
{ 
  try
    {
      Daisy& daisy = dynamic_cast<Daisy&> (toplevel->program ());
      daisy_assert (col >= 0 && col < daisy.field->size ()); 
      return daisy.field->find (col); 
    }
  DAISY_CATCH_BLOCK(toplevel)
  return NULL;
}

extern "C" const char* EXPORT
daisy_column_get_name (const Column* column)
{ return column->name.name ().c_str (); }

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

// @ The daisy_scope Type.
//
// Extract information from the 'extern' log model.
extern "C" unsigned int EXPORT  // Return number of extern scopes
daisy_daisy_scope_extern_size (Toplevel *const toplevel)
{
  try
    {
      Daisy& daisy = dynamic_cast<Daisy&> (toplevel->program ());
      return daisy.output_log->scope_size ();
    }
  DAISY_CATCH_BLOCK(toplevel);
  return 0;
}

extern "C" Scope* EXPORT  // Return extern scope INDEX.
daisy_daisy_scope_extern_get (Toplevel *const toplevel,
                              const unsigned int index)
{
  try
    {
      Daisy& daisy = dynamic_cast<Daisy&> (toplevel->program ());
      return &daisy.output_log->scope (index); 
    }
  DAISY_CATCH_BLOCK(toplevel);
  return NULL;
}

extern "C" unsigned int EXPORT // Number of numbers in SCOPE.
daisy_scope_number_size (const Scope *const scope)
{ return scope->all_numbers ().size (); }

extern "C" const char* EXPORT       // Name of number INDEX in SCOPE.
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

extern "C" int EXPORT           // True, iff SCOPE is writable.
daisy_scope_writable (Scope* scope)
{ 
  if (dynamic_cast<WScope*> (scope))
    return 1; 

  return 0;
}

extern "C" void EXPORT          // In SCOPE, set NAME to VALUE.
daisy_scope_set_number (Scope* scope, 
                        const char *const name, const double value)
{ 
  WScope* wscope = dynamic_cast<WScope*> (scope);
  daisy_assert (wscope);
  wscope->set_number (symbol (name), value);
}

// @ Miscellaneous.

extern "C" void EXPORT
daisy_initialize ()
{ }

extern "C" const char* EXPORT
daisy_version ()
{ return symbol (version).name ().c_str (); }
