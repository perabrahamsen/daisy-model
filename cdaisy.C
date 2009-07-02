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

#define BUILD_DLL

#include "scope.h"
#include "block.h"
#include "metalib.h"
#include "library.h"
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
#include "treelog.h"
#include "frame_model.h"
#include "frame_submodel.h"
#include "filepos.h"
#include <string>
#include <typeinfo>

#ifdef MINGW
#ifdef __unix
#error "Unix?"
#define EXPORT /* Nothing */
#elif defined (BUILD_DLL)
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

extern "C" int EXPORT
daisy_category_number (const char* name)
{ return Value::category_number (name); }

extern "C" const char* EXPORT
daisy_category_name (Value::category number)
{ return Value::category_name (number).name ().c_str (); }

extern "C" int EXPORT
daisy_size_sequence ()
{ return Value::Variable; }

extern "C" int EXPORT
daisy_size_singleton ()
{ return Value::Singleton; }

extern "C" int EXPORT
daisy_type_number (const char* name)
{ return Value::type_number (name); }

extern "C" const char* EXPORT
daisy_type_name (Value::type number)
{ return Value::type_name (number).name ().c_str (); }

// @ The daisy_frame Type.

#if 0
extern "C" Frame* EXPORT
daisy_frame_clone (const Frame* frame)
{ return &frame->clone (); }
#endif

extern "C" void EXPORT
daisy_frame_delete (Frame* frame)
{ delete frame; }

extern "C" const char* EXPORT
daisy_frame_type_name (const Frame* frame)
{ return frame->type_name ().name ().c_str (); }

extern "C" const char* EXPORT
daisy_frame_base_name (const Frame* frame)
{ return frame->base_name ().name ().c_str (); }

extern "C" const char* EXPORT
daisy_frame_description (const Frame* frame)
{ return frame->description ().name ().c_str (); }

extern "C" daisy_bool EXPORT
daisy_frame_check (const Frame* frame, const char* name)
{ return frame->check (name); }

extern "C" int EXPORT
daisy_frame_get_integer (const Frame* frame, const char* name)
{ return frame->integer (name); }

extern "C" double EXPORT
daisy_frame_get_number (const Frame* frame, const char* name)
{ return frame->number (name); }

extern "C" const char* EXPORT
daisy_frame_get_string (const Frame* frame, const char* name)
{ return frame->name (name).name ().c_str (); }

extern "C" daisy_bool EXPORT
daisy_frame_get_flag (const Frame* frame, const char* name)
{ return frame->flag (name); }

extern "C" const Frame* EXPORT
daisy_frame_get_frame (const Frame* frame, const char* name)
{ 
  if (frame->lookup (name) == Value::Object)
    return &frame->model (name); 
  else
    return &frame->submodel (name); 
}

extern "C" void EXPORT
daisy_frame_set_integer (Frame* frame, const char* name, int value)
{ frame->set (name, value); }

extern "C" void EXPORT
daisy_frame_set_number (Frame* frame, const char* name, double value)
{ frame->set (name, value); }

extern "C" void EXPORT
daisy_frame_set_string (Frame* frame, const char* name, 
                        const char* value)
{ frame->set (name, value); }

extern "C" void EXPORT
daisy_frame_set_flag (Frame* frame, const char* name, daisy_bool value)
{ frame->set (name, bool (value)); }

extern "C" void EXPORT
daisy_frame_set_frame (Frame* frame, const char* name,
		       Frame* value)
{ 
  if (frame->lookup (name) == Value::AList)
    frame->set (name, dynamic_cast<const FrameSubmodel&> (*value)); 
  else
    frame->set (name, dynamic_cast<const FrameModel&> (*value)); 
}

#ifdef UNINPLEMENTED
extern "C" unsigned int EXPORT
daisy_frame_size_integer (const Frame* frame, const char* name)
{ return frame->integer_sequence (name).size (); }

extern "C" unsigned int EXPORT
daisy_frame_size_string (const Frame* frame, const char* name)
{ return frame->name_sequence (name).size (); }

extern "C" unsigned int EXPORT
daisy_frame_size_flag (const Frame* frame, const char* name)
{ return frame->flag_sequence (name).size (); }
#endif

extern "C" unsigned int EXPORT
daisy_frame_size_number (const Frame* frame, const char* name)
{ return frame->number_sequence (name).size (); }

extern "C" unsigned int EXPORT
daisy_frame_size_frame (const Frame* frame, const char* name)
{
  // KLUDGE: Work around the use of non-sequence value as the default
  // for each element in the sequence.
  if (frame->type_size (name) == Value::Singleton)
    return 0;
  
  return frame->value_size (name); 
}

#ifdef UNINPLEMENTED
extern "C" int EXPORT
daisy_frame_get_integer_at (const Frame* frame, const char* name,
			    unsigned int index)
{ return frame->integer_sequence (name)[index]; }

extern "C" const char* EXPORT
daisy_frame_get_string_at (const Frame* frame, const char* name,
			    unsigned int index)
{ return frame->name_sequence (name)[index].name ().c_str (); }

extern "C" daisy_bool EXPORT
daisy_frame_get_flag_at (const Frame* frame, const char* name,
			    unsigned int index)
{ return frame->flag_sequence (name)[index]; }
#endif

extern "C" double EXPORT
daisy_frame_get_number_at (const Frame* frame, const char* name,
			    unsigned int index)
{ return frame->number_sequence (name)[index]; }

extern "C" const Frame* EXPORT
daisy_frame_get_frame_at (const Frame* frame, const char* name,
			  unsigned int index)
{ 
  if (frame->lookup (name) == Value::Object)
    return frame->model_sequence (name)[index]; 
  else
    return frame->submodel_sequence (name)[index]; 
}

#ifdef UNINPLEMENTED
extern "C" void EXPORT
daisy_frame_set_integer_at (Frame* frame, const char* name,
			    int value, unsigned int index)
{ 
  vector<int>& v = frame->check (name)
    ? *new vector<int> (frame->integer_sequence (name))
    : *new vector<int>;
  if (v.size () <= index)
    while (v.size () <= index)
      v.push_back (value);
  else
    v[index] = value;
  frame->set (name, v);
}
#endif

extern "C" void EXPORT
daisy_frame_set_string_at (Frame* frame, const char* name,
			   const char* value, unsigned int index)
{
  std::vector<symbol> v;
  if (frame->check (name))
    v = frame->name_sequence (name);
  if (v.size () <= index)
    while (v.size () <= index)
      v.push_back (symbol (value));
  else
    v[index] = symbol (value);
  frame->set (name, v);
}

#ifdef UNINPLEMENTED
extern "C" void EXPORT
daisy_frame_set_flag_at (Frame* frame, const char* name,
			 daisy_bool value, unsigned int index)
{ 
  vector<bool>& v = frame->check (name)
    ? *new vector<bool> (frame->flag_sequence (name))
    : *new vector<bool>;
  if (v.size () <= index)
    while (v.size () <= index)
      v.push_back (value);
  else
    v[index] = value;
  frame->set (name, v);
}
#endif

extern "C" void EXPORT
daisy_frame_set_number_at (Frame* frame, const char* name,
			   double value, unsigned int index)
{
  std::vector<double>& v= frame->check (name)
    ? *new std::vector<double> (frame->number_sequence (name))
    : *new std::vector<double>;
  if (v.size () <= index)
    while (v.size () <= index)
      v.push_back (value);
  else
    v[index] = value;
  frame->set (name, v);
}

/* @ The daisy_library Type.
 * 
 * A library contains a collection of objects, each containing a
 * constructor, a frame, an origin, and a name.
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

extern "C" const Frame* EXPORT
daisy_library_frame (const Library* library, const char* name)
{ return &library->model (symbol (name)); }

extern "C" const char* EXPORT
daisy_library_file (const Library* library, const char* name)
{ 
  const Frame& frame = library->model (name);
  if (frame.own_position () != Filepos::none ())
    return frame.own_position ().filename ().name ().c_str ();
  
  return NULL;
}

extern "C" void EXPORT
daisy_library_remove (Library* library, const char* name)
{ library->remove (symbol (name)); }

// @ The daisy_printer Type.

#include <fstream>

struct PrinterFileOwned : public PrinterFile
{
  std::ofstream out;
  
  PrinterFileOwned (Metalib& metalib, const char *const filename)
    : PrinterFile (metalib, out),
      out (filename)
  { }
};

extern "C" Printer* EXPORT
daisy_printer_create_file (Toplevel *const toplevel,
                           const char *const filename)
{ return new PrinterFileOwned (toplevel->metalib (), filename); }

extern "C" void EXPORT
daisy_printer_comment (Printer* printer, const char* comment)
{ printer->print_comment (comment); }

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
daisy_daisy_create ()
{
  try 
    { return new Toplevel ("none"); }
  catch (...)
    { return NULL; }
}

extern "C" EXPORT void
daisy_daisy_parse_command_line (Toplevel* toplevel, int argc, char** argv)
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

extern "C" EXPORT const Frame*
daisy_daisy_get_program_frame (Toplevel* toplevel)
{ return &toplevel->program_frame (); }

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
      Treelog::Open nest (toplevel->msg (), daisy.time.print ());
      daisy.tick (toplevel->msg ()); 
    }
  DAISY_CATCH_BLOCK(toplevel);
}

extern "C" void EXPORT
daisy_daisy_tick_before (Toplevel* toplevel)
{
  try
    {
      Daisy& daisy = dynamic_cast<Daisy&> (toplevel->program ());
      daisy.tick_before (toplevel->msg ());
    }
  DAISY_CATCH_BLOCK(toplevel);
}

extern "C" void EXPORT
daisy_daisy_tick_columns (Toplevel* toplevel)
{
  try
    {
      Daisy& daisy = dynamic_cast<Daisy&> (toplevel->program ());
      daisy.tick_columns (toplevel->msg ());
    }
  DAISY_CATCH_BLOCK(toplevel);
}

extern "C" void EXPORT
daisy_daisy_tick_column (Toplevel* toplevel, int col)
{
  try
    {
      Daisy& daisy = dynamic_cast<Daisy&> (toplevel->program ());
      daisy.tick_column (col, toplevel->msg ());
    }
  DAISY_CATCH_BLOCK(toplevel);
}

extern "C" void EXPORT
daisy_daisy_tick_after (Toplevel* toplevel)
{
  try
    {
      Daisy& daisy = dynamic_cast<Daisy&> (toplevel->program ());
      daisy.tick_after (toplevel->msg ());
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

extern "C" const char* EXPORT
daisy_column_get_description (const Column* column)
{ 
  const FrameModel& frame = column->frame (); 
  const symbol d = frame.description ();
  if (d != Value::None ())
    return d.name ().c_str ();
  return "";
}

extern "C" unsigned int EXPORT
daisy_column_location_size (const Column* column)
{ return column->location ().size (); }

extern "C" double EXPORT
daisy_column_location_x (const Column* column, unsigned int index)
{ return column->location ()[index]->x; }

extern "C" double EXPORT
daisy_column_location_y (const Column* column, unsigned int index)
{ return column->location ()[index]->y; }

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
{ 
  std::set<symbol> all;
  scope->entries (all);
  size_t count = 0;
  for (std::set<symbol>::const_iterator i = all.begin (); i != all.end (); i++)
    if (scope->lookup (*i) == Value::Number)
      count++;
  return count; 
}

extern "C" const char* EXPORT       // Name of number INDEX in SCOPE.
daisy_scope_number_name (const Scope *const scope, const unsigned int index)
{
  std::set<symbol> all;
  scope->entries (all);
  size_t count = 0;
  for (std::set<symbol>::const_iterator i = all.begin (); i != all.end (); i++)
    {
      const symbol name = *i;
      if (scope->lookup (name) == Value::Number)
        if (count == index)
          return name.name ().c_str ();
        else
          count++;
    }
  daisy_notreached ();
}

extern "C" const int EXPORT	// check if NAME is defined in SCOPE.
daisy_scope_has_number (const Scope* scope, const char* name)
{ 
  if (scope->lookup (name) == Value::Number 
      && scope->type_size (name) == Value::Singleton
      && scope->check (name))
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
  if (scope->lookup (name) == Value::String 
      && scope->type_size (name) == Value::Singleton
      && scope->check (name))
    return 1;
  else 
    return 0; 
}

extern "C" const char* EXPORT	// Return string value of NAME in SCOPE.
daisy_scope_string (const Scope* scope, const char* name)
{ 
  return scope->name (name).name ().c_str ();
}

extern "C" const char* EXPORT	// Return UNITS of NAME defined in SCOPE.
daisy_scope_description (const Scope* scope, const char* name)
{ return scope->description (name).name().c_str (); }

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
  wscope->set (symbol (name), value);
}

// @ Miscellaneous.

extern "C" void EXPORT
daisy_initialize ()
{ }

extern "C" const char* EXPORT
daisy_version ()
{ return symbol (version).name ().c_str (); }

// cdaisy.C ends here.
