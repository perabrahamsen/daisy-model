// log_dlf.C -- Log selected data in tabular format.
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

#define BUILD_DLL

#include "log_dlf.h"
#include "select.h"
#include "geometry.h"
#include "assertion.h"
#include "daisy.h"
#include "block.h"
#include "frame.h"
#include "treelog.h"
#include "summary.h"
#include "scope_block.h"
#include "filepos.h"
#include "librarian.h"
#include <sstream>

const char *const LogDLF::default_description = "\
Shared base class for log models generating Daisy Log File formatted results.";

void
LogDLF::common_match (const Daisy& daisy, Treelog&)
{ print_header.finish (out, daisy); }

void 
LogDLF::common_done (const std::vector<Time::component_t>& time_columns,
                       const Time& time, const double dt)
{ 
  if (print_tags)
    {
      if (std_time_columns)
        for (size_t i = 0; i < time_columns.size (); i++)
          out << Time::component_name (time_columns[i]) << field_separator;

      // Print the entry names in the first line of the log file..
      for (unsigned int i = 0; i < entries.size (); i++)
        {
          if (i != 0)
            out << field_separator;

          const Geometry *const geo = entries[i]->geometry ();
          const int size = entries[i]->size ();
          const symbol tag = entries[i]->tag ();
          static const symbol empty_symbol ("");

          if (geo && size >= 0)
            {
              if (geo->cell_size () == size)
                {
                  // Content.
                  for (unsigned j = 0; j < size; j++)
                    {
                      if (j != 0)
                        out << array_separator;
                      if (tag != empty_symbol)
                        out << tag << " @ ";
                      out << geo->cell_name (j);
                    }
                }
              else if (geo->edge_size () == size)
                {
                  // Flux
                  for (unsigned j = 0; j < size; j++)
                    {
                      if (j != 0)
                        out << array_separator;
                      if (tag != empty_symbol)
                        out << tag << " @ ";
                      out << geo->edge_name (j);
                    }
                }
              else
                {
                  // Other arrays (buggy if other array have same size as geo.)
                  for (unsigned j = 0; j < size; j++)
                    {
                      if (j != 0)
                        out << array_separator;
                      out << tag << "[" << j << "]";
                    }
                }
            }
          else
            out << entries[i]->tag ();
        }
      out << record_separator;
      print_tags = false;
    }
  if (print_dimension)
    {
      if (std_time_columns)
        for (size_t i = 0; i < time_columns.size (); i++)
          out << field_separator;

      // Print the entry names in the first line of the log file..
      for (unsigned int i = 0; i < entries.size (); i++)
        {
          if (i != 0)
            out << field_separator;

          const int size = entries[i]->size ();
          symbol dimension = entries[i]->dimension ().name ();
          if (dimension == Value::None () 
              || dimension == Value::Unknown ()
              || dimension == Value::Fraction ())
            dimension = "";

          if (size >= 0)
            {
              for (unsigned j = 0; j < size; j++)
                {
                  if (j != 0)
                    out << array_separator;
                  out << dimension;
                }
            }
          else
            out << dimension;
        }
      out << record_separator;
      print_dimension = false;
    }

  if (std_time_columns)
    for (size_t i = 0; i < time_columns.size (); i++)
      out << time.component_value (time_columns[i]) << field_separator;

  for (size_t i = 0; i < entries.size (); i++)
    {
      if (i != 0)
        out << field_separator;
      
      entries[i]->done (dt);

      process_entry (i);
    }
  out << record_separator;
  if (flush)
    out.flush ();
}

bool 
LogDLF::match (const Daisy& daisy, Treelog& msg)
{
  common_match (daisy, msg);
  return LogSelect::match (daisy, msg);
}
void 
LogDLF::done (const std::vector<Time::component_t>& time_columns,
                const Time& time, const double dt)
{ 
  LogSelect::done (time_columns, time, dt);

  if (!is_printing)
    return;

  for (unsigned int i = 0; i < entries.size (); i++)
    if (entries[i]->prevent_printing ())
      return;

  common_done (time_columns, time, dt);
  end = time;
}
bool 
LogDLF::initial_match (const Daisy& daisy, Treelog& msg)
{
  begin = daisy.time;

  common_match (daisy, msg);
  return LogSelect::initial_match (daisy, msg);
}
void 
LogDLF::initial_done (const std::vector<Time::component_t>& time_columns,
                        const Time& time, const double dt)
{ 
  LogSelect::initial_done (time_columns, time, dt);

  bool prevent_printing = !print_initial;
  for (unsigned int i = 0; i < entries.size (); i++)
    if (entries[i]->prevent_printing ())
      prevent_printing = true;

  if (prevent_printing)
    {
      for (unsigned int i = 0; i < entries.size (); i++)
        entries[i]->done (dt);
      return;
    }
      
  common_done (time_columns, time, dt);
  begin = time;
}

bool LogDLF::check (const Border& border, Treelog& msg) const
{ 
  Treelog::Open nest (msg, name);
  bool ok = LogSelect::check (border, msg);
  if (!out.good ())
    {
      std::ostringstream tmp;
      tmp << "Write error for '" << file << "'";
      msg.error (tmp.str ());
      ok = false;
    }
  return ok; 
}

bool 
LogDLF::contain_time_columns (const std::vector<Select*>& entries)
{
  static const symbol time ("time");
  for (unsigned int i = 0; i < entries.size (); i++)
    if (entries[i]->path[0] == time)
      return true;
  return false;
}

void
LogDLF::initialize (Treelog& msg)
{
  out.open (file.name ().c_str ());

  print_header.start (out, name, file, parsed_from_file);

  for (size_t i = 0; i < parameters.size (); i++)
    print_header.parameter (out, parameters[i].first, parameters[i].second);

  print_header.interval (out, *volume);
  if (description != default_description)
    print_header.log_description (out, description);

  out.flush ();
}

std::vector<std::pair<symbol, symbol>/**/>
LogDLF::build_parameters (Block& al)
{
  std::vector<std::pair<symbol, symbol>/**/> result;
  ScopeBlock scope_block (al);
  std::vector<symbol> pars = al.name_sequence ("parameter_names");
  for (size_t i = 0; i < pars.size (); i++)
    {
      const symbol key = pars[i];
      if (scope_block.has_name (key))
        {
          const symbol value = scope_block.name (key);
          std::string id = key.name ();
          std::transform (id.begin (), id.end (), id.begin (), ::toupper);
          result.push_back (std::pair<symbol, symbol> (symbol (id), value));
        }
      else
        al.msg ().warning ("Parameter name '" + key + "' not found"); 
    }
  return result;
}

LogDLF::LogDLF (Block& al)
  : LogSelect (al),
    parsed_from_file (al.frame ().inherited_position ().filename ()),
    file (al.name ("where")),
    flush (al.flag ("flush")),
    record_separator (al.name ("record_separator")),
    field_separator (al.name ("field_separator")),
    error_string (al.name ("error_string")),
    missing_value (al.name ("missing_value")),
    array_separator (al.name ("array_separator")),
    print_header (al.name ("print_header")),
    parameters (build_parameters (al)),
    print_tags (al.flag ("print_tags")),
    print_dimension (al.flag ("print_dimension")),
    print_initial (al.flag ("print_initial")),
    std_time_columns (al.ok () && !contain_time_columns (entries)),
    begin (1, 1, 1, 1),
    end (1, 1, 1, 1)
{ }

LogDLF::~LogDLF ()
{
  if (!out.good ())
    Assertion::error ("Problems writing to '" + file + "'");
}

static struct LogDLFSyntax : public DeclareBase
{
  LogDLFSyntax ()
    : DeclareBase (Log::component, "dlf", "select", 
                   LogDLF::default_description)
  { }
  void load_frame (Frame& frame) const
  { 
    frame.declare ("parameter_names", Value::String, 
                   Value::Const, Value::Variable, "\
List of string parameters to print to the table header.\n\
\n\
For example, if you have defined 'column' and 'crop' parameters for\n\
this table log parameterization, you can print them to the log file\n\
header by specifying '(names column crop)'.");
    frame.set_empty ("parameter_names");
    frame.declare ("where", Value::String, Value::Const,
                   "Name of the log file to create.");
    frame.declare ("print_header", Value::String, Value::Const,
                   "If this is set to 'false', no header is printed.\n\
If this is set to 'true', a full header is printer.\n\
If this is set to 'fixed', a small fixed size header is printed.");
    static VCheck::Enum check_header ("false", "true", "fixed");
    frame.set_check ("print_header", check_header);
    frame.set ("print_header", "true");
    frame.declare ("print_tags", Value::Boolean, Value::Const,
                   "Print a tag line in the file.");
    frame.set ("print_tags", true);
    frame.declare ("print_dimension", Value::Boolean, Value::Const,
                   "Print a line with units after the tag line.");
    frame.set ("print_dimension", true);
    frame.declare ("print_initial", Value::Boolean, Value::Const,
                   "Print a line with initial values when logging starts.");
    frame.set ("print_initial", true);
    frame.declare ("flush", Value::Boolean, Value::Const,
                   "Flush to disk after each entry (for debugging).");
    frame.set ("flush", false);
    frame.declare ("record_separator", Value::String, Value::Const, "\
String to print between records (time steps).");
    frame.set ("record_separator", "\n");
    frame.declare ("field_separator", Value::String, Value::Const, "\
String to print between fields.");
    frame.set ("field_separator", "\t");
    frame.declare ("error_string", Value::String, Value::Const, "\
String to print when errors are encountered.");
    frame.set ("error_string", "!");
    frame.declare ("missing_value", Value::String, Value::Const, "\
String to print when the path doesn't match anything.\n\
This can be relevant for example if you are logging a crop, and there are\n\
no crops on the field.");
    frame.set ("missing_value", "00.00");
    frame.declare ("array_separator", Value::String, Value::Const, "\
String to print between array entries.");
    frame.set ("array_separator", "\t");
    Librarian::add_doc_fun (LogSelect::component, 
                            LogSelect::document_entries);
  }
} LogDLF_syntax;

// log_dlf.C ends here.
