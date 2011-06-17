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

#include "log_select.h"
#include "dlf.h"
#include "summary.h"
#include "symbol.h"
#include "select.h"
#include "geometry.h"
#include "assertion.h"
#include "daisy.h"
#include "block_model.h"
#include "frame_model.h"
#include "treelog.h"
#include "filepos.h"
#include "librarian.h"
#include "metalib.h"
#include "library.h"
#include <sstream>
#include <fstream>
#include <vector>

class DestinationTable : public Destination
{
  // destination Content.
  enum { Error, Missing, Number, Name, Array } type;
  double dest_number;
  symbol dest_name;
  const std::vector<double>* dest_array;

  // Select::Destination
  void error ();
  void missing ();
  void add (const std::vector<double>& value);
  void add (const double value);
  void add (const symbol value);

public:
  void process_entry (std::ostream& out, 
                      const char *const error_string,
                      const char *const missing_value,
                      const char *const array_separator);

  DestinationTable ()
    : type (Error),
      dest_number (-42.42e42),
      dest_name ("Daisy bug"),
      dest_array (NULL)
  { }
};

void 
DestinationTable::process_entry (std::ostream& out, 
                                 const char *const error_string,
                                 const char *const missing_value,
                                 const char *const array_separator)
{ 
  switch (type)
    {
    case Error:
      out << error_string;
      break;
    case Missing: 
      out << missing_value;
      break;
    case Number:
      out << dest_number;
      break;
    case Name: 
      out << dest_name;
      break;
    case Array:
      {
        daisy_assert (dest_array);
        const std::vector<double> array = *dest_array;
        for (unsigned int i = 0; i < array.size (); i++)
          {
            if (i != 0)
              out << array_separator;
            out << array[i];
          }
      }
      break;
    default:
      daisy_notreached ();
    }
}

void 
DestinationTable::error ()
{ 
  type = Error;
}

void 
DestinationTable::missing ()
{ 
  type = Missing;
}

void 
DestinationTable::add (const std::vector<double>& value)
{ 
  type = Array;
  dest_array = &value;
}

void 
DestinationTable::add (const double value)
{ 
  type = Number;
  dest_number = value;
}

void 
DestinationTable::add (const symbol value)
{ 
  type = Name;
  dest_name = value;
}



struct LogDLF : public LogSelect
{
  // File Content.
  const symbol parsed_from_file; // Defined in...
  const symbol file;       // Filename.
  std::ofstream out;            // Output stream.
  const bool flush;             // Flush after each time step.
  // char* faster than symbol for output.
  const char *const record_separator; // String to print on records (time steps)
  const char *const field_separator; // String to print between fields.
  const char *const error_string; // String to print on errors.
  const char *const missing_value; // String to print for missing values.
  const char *const array_separator; // String to print between array entries.
  DLF print_header;             // How much header should be printed?
  std::vector<std::pair<symbol, symbol>/**/> parameters;      // Par vals.
  bool print_tags;              // Set if tags should be printed.
  bool print_dimension;         // Set if dimensions should be printed.
  const bool print_initial;     // Set if initial values should be printed.
  const bool std_time_columns;  // Add year, month, day and hour columns.
  Time begin;                   // First log entry.
  Time end;                     // Last log entry.

  // Data ends up here.
  const auto_vector<Summary*> summary;
  DestinationTable destination;

  // Log.
  void common_match (const Daisy& daisy, Treelog& out);
  void common_done (const std::vector<Time::component_t>& time_columns,
                    const Time& time, Treelog&);

  // Log.
  bool match (const Daisy& daisy, Treelog& out);
  void done (const std::vector<Time::component_t>& time_columns,
             const Time&, double dt, Treelog&);

  // Initial line.
  bool initial_match (const Daisy&, const Time& previous, Treelog&);
  void initial_done (const std::vector<Time::component_t>& time_columns,
                     const Time&, Treelog&);

  // Create and destroy.
  bool check (const Border&, Treelog& msg) const;
  static bool contain_time_columns (const std::vector<Select*>& entries);
  void initialize (const symbol log_dir, Treelog&);
  void summarize (Treelog& msg);
  static std::vector<std::pair<symbol, symbol>/**/>
  /**/ build_parameters (const BlockModel& al);
  explicit LogDLF (const BlockModel& al);
  ~LogDLF ();
};

void
LogDLF::common_match (const Daisy& daisy, Treelog&)
{ print_header.finish (out, metalib (), daisy.frame ()); }

void 
LogDLF::common_done (const std::vector<Time::component_t>& time_columns,
                     const Time& time, Treelog&)
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
          if (dimension == Attribute::None () 
              || dimension == Attribute::Unknown ()
              || dimension == Attribute::Fraction ())
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
      
      entries[i]->done_print ();

      destination.process_entry (out, 
                                 error_string, missing_value, array_separator); 
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
              const Time& time, const double dt, Treelog& msg)
{ 
  LogSelect::done (time_columns, time, dt, msg);

  if (!is_printing)
    return;

  for (unsigned int i = 0; i < entries.size (); i++)
    if (entries[i]->prevent_printing ())
      return;

  common_done (time_columns, time, msg);
  end = time;
}

bool 
LogDLF::initial_match (const Daisy& daisy, const Time& previous, Treelog& msg)
{
  for (unsigned int i = 0; i < summary.size (); i++)
    summary[i]->clear ();

  begin = daisy.time ();

  common_match (daisy, msg);
  return LogSelect::initial_match (daisy, previous, msg);
}

void 
LogDLF::initial_done (const std::vector<Time::component_t>& time_columns,
                      const Time& time, Treelog& msg)
{ 
  LogSelect::initial_done (time_columns, time, msg);

  bool prevent_printing = !print_initial;
  for (unsigned int i = 0; i < entries.size (); i++)
    if (entries[i]->prevent_printing ())
      prevent_printing = true;

  if (prevent_printing)
    {
      for (unsigned int i = 0; i < entries.size (); i++)
        entries[i]->done_print ();
      return;
    }
      
  common_done (time_columns, time, msg);
  begin = time;
}

bool 
LogDLF::check (const Border& border, Treelog& msg) const
{ 
  TREELOG_MODEL (msg);
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
  static const symbol previous ("previous");
  for (unsigned int i = 0; i < entries.size (); i++)
    if (entries[i]->path[0] == time || (entries[i]->path[0] == previous))
      return true;
  return false;
}

void
LogDLF::initialize (const symbol log_dir, Treelog& msg)
{
  const std::string fn = log_dir.name () + file.name ();
  out.open (fn.c_str ());

  print_header.start (out, objid, file, parsed_from_file);

  for (size_t i = 0; i < parameters.size (); i++)
    print_header.parameter (out, parameters[i].first, parameters[i].second);

  print_header.interval (out, *volume);
  print_header.log_description (out, description);

  out.flush ();

  TREELOG_MODEL (msg);
  for (unsigned int i = 0; i < summary.size (); i++)
    summary[i]->initialize (entries, msg);
}

void
LogDLF::summarize (Treelog& msg)
{
  if (summary.size () > 0)
    {
      TREELOG_MODEL (msg);
      std::ostringstream tmp;

      tmp << "LOGFILE: " << file  << "\n";
      tmp << "VOLUME: " << volume->one_line_description () << "\n";
      tmp << "TIME: " << begin.print () << " to " << end.print ();
      for (size_t i = 0; i < parameters.size (); i++)
        if (parameters[i].first != "*")
          tmp << "\n" << parameters[i].first << ": " << parameters[i].second;

      msg.message (tmp.str ());
      for (size_t i = 0; i < summary.size (); i++)
        summary[i]->summarize (msg);
    }
}

std::vector<std::pair<symbol, symbol>/**/>
LogDLF::build_parameters (const BlockModel& al)
{
  daisy_assert (al.check ("parameter_names"));
  const std::vector<symbol> pars = al.name_sequence ("parameter_names");
  std::vector<std::pair<symbol, symbol>/**/> result;
  for (size_t i = 0; i < pars.size (); i++)
    {
      const symbol key = pars[i];
      if (al.can_extract_as (key, Attribute::String))
        {
          const symbol value = al.name (key);
          std::string id = key.name ();
          std::transform (id.begin (), id.end (), id.begin (), ::toupper);
          result.push_back (std::pair<symbol, symbol> (symbol (id), value));
        }
      else
        al.msg ().warning ("Parameter name '" + key + "' not found"); 
    }
  return result;
}

LogDLF::LogDLF (const BlockModel& al)
  : LogSelect (al),
    parsed_from_file (al.frame ().inherited_position ().filename ()),
    file (al.name ("where")),
    flush (al.flag ("flush")),
    record_separator (al.name ("record_separator").name ().c_str ()),
    field_separator (al.name ("field_separator").name ().c_str ()),
    error_string (al.name ("error_string").name ().c_str ()),
    missing_value (al.name ("missing_value").name ().c_str ()),
    array_separator (al.name ("array_separator").name ().c_str ()),
    print_header (al.name ("print_header")),
    parameters (build_parameters (al)),
    print_tags (al.flag ("print_tags")),
    print_dimension (al.flag ("print_dimension")),
    print_initial (al.flag ("print_initial")),
    std_time_columns (al.ok () && !contain_time_columns (entries)),
    begin (1, 1, 1, 1),
    end (1, 1, 1, 1),
    summary (Librarian::build_vector<Summary> (al, "summary"))
{ 
  if (!al.ok ())
    return;

  for (unsigned int i = 0; i < entries.size (); i++)
    entries[i]->add_dest (&destination);
}

LogDLF::~LogDLF ()
{
  if (!out.good ())
    Assertion::error ("Problems writing to '" + file + "'");
}

static struct LogDLFSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new LogDLF (al); }

  LogDLFSyntax ()
    : DeclareModel (Log::component, "DLF", "select", "\
Shared base class for log models generating Daisy Log File formatted results.")
  { }
  void load_frame (Frame& frame) const
  { 
    frame.declare_string ("parameter_names", 
                   Attribute::Const, Attribute::Variable, "\
List of string parameters to print to the table header.\n\
\n\
For example, if you have defined 'column' and 'crop' parameters for\n\
this table log parameterization, you can print them to the log file\n\
header by specifying '(names column crop)'.");
    frame.set_empty ("parameter_names");
    frame.declare_string ("where", Attribute::Const,
                   "Name of the log file to create.");
    DLF::add_syntax (frame, "print_header");
    frame.declare_boolean ("print_tags", Attribute::Const,
                   "Print a tag line in the file.");
    frame.set ("print_tags", true);
    frame.declare_boolean ("print_dimension", Attribute::Const,
                   "Print a line with units after the tag line.");
    frame.set ("print_dimension", true);
    frame.declare_boolean ("print_initial", Attribute::Const,
                   "Print a line with initial values when logging starts.");
    frame.set ("print_initial", true);
    frame.declare_boolean ("flush", Attribute::Const,
                   "Flush to disk after each entry (for debugging).");
    frame.set ("flush", false);
    frame.declare_string ("record_separator", Attribute::Const, "\
String to print between records (time steps).");
    frame.set ("record_separator", "\n");
    frame.declare_string ("field_separator", Attribute::Const, "\
String to print between fields.");
    frame.set ("field_separator", "\t");
    frame.declare_string ("error_string", Attribute::Const, "\
String to print when errors are encountered.");
    frame.set ("error_string", "!");
    frame.declare_string ("missing_value", Attribute::Const, "\
String to print when the path doesn't match anything.\n\
This can be relevant for example if you are logging a crop, and there are\n\
no crops on the field.");
    frame.set ("missing_value", "00.00");
    frame.declare_string ("array_separator", Attribute::Const, "\
String to print between array entries.");
    frame.set ("array_separator", "\t");
    frame.declare_object ("summary", Summary::component,
                          Attribute::Const, Attribute::Variable,
                          "Summaries for this log file.");
    frame.set_empty ("summary");
    Librarian::add_doc_fun (Log::component, 
                            LogSelect::document_entries);
  }
} LogDLF_syntax;

static struct LogTableSyntax : public DeclareParam
{
  LogTableSyntax ()
    : DeclareParam (Log::component, "table", "DLF", "\
Each selected variable is represented by a column in the specified log file.")
  { }
  void load_frame (Frame&) const
  { }
} LogTable_syntax;

// log_dlf.C ends here.
