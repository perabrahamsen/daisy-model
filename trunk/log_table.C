// log_table.C -- Log selected data in tabular format.
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
#include "library.h"
#include "block.h"
#include "select.h"
#include "summary.h"
#include "geometry.h"
#include "dlf.h"
#include "daisy.h"
#include "timestep.h"
#include "vcheck.h"
#include "memutils.h"
#include "librarian.h"
#include "scope_block.h"
#include <sstream>
#include <fstream>

struct LogTable : public LogSelect, public Destination
{
  static const char *const default_description;

  // File Content.
  const std::string parsed_from_file; // Defined in...
  const std::string file;       // Filename.
  std::ofstream out;            // Output stream.
  const bool flush;		// Flush after each time step.
  const std::string record_separator; // String to print on records (time steps).
  const std::string field_separator; // String to print between fields.
  const std::string error_string; // String to print on errors.
  const std::string missing_value; // String to print for missing values.
  const std::string array_separator; // String to print between array entries.
  DLF print_header;             // How much header should be printed?
  std::vector<std::pair<symbol, symbol>/**/> parameters;      // Par vals.
  bool print_tags;		// Set if tags should be printed.
  bool print_dimension;		// Set if dimensions should be printed.
  const bool print_initial;     // Set if initial values should be printed.
  const bool std_time_columns;	// Add year, month, day and hour columns.
  const std::vector<Summary*> summary; // Summarize this log file.
  Time begin;			// First log entry.
  Time end;			// Last log entry.

  // destination Content.
  enum { Error, Missing, Number, Name, Array } type;
  double dest_number;
  symbol dest_name;
  const std::vector<double>* dest_array;
  
  // Log.
  void common_match (const Daisy& daisy, Treelog& out);
  void common_done (const std::vector<Time::component_t>& time_columns,
		    const Time& time, double dt);

  // Log.
  bool match (const Daisy& daisy, Treelog& out);
  void done (const std::vector<Time::component_t>& time_columns,
	     const Time& time, double dt);

  // Initial line.
  bool initial_match (const Daisy&, Treelog&);
  void initial_done (const std::vector<Time::component_t>& time_columns,
		     const Time& time, const double dt);

  // Select::Destination
  void error ();
  void missing ();
  void add (const std::vector<double>& value);
  void add (const double value);
  void add (const symbol value);

  // Create and destroy.
  bool check (const Border&, Treelog& msg) const;
  static bool contain_time_columns (const std::vector<Select*>& entries);
  void initialize (Treelog&);
  static std::vector<std::pair<symbol, symbol>/**/>
  /**/ build_parameters (Block& al);
  explicit LogTable (Block& al);
  void summarize (Treelog&);
  ~LogTable ();
};

const char *const LogTable::default_description = "\
Each selected variable is represented by a column in the specified log file.";

void
LogTable::common_match (const Daisy& daisy, Treelog&)
{ print_header.finish (out, daisy); }

void 
LogTable::common_done (const std::vector<Time::component_t>& time_columns,
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
	  std::string dimension = entries[i]->dimension ().name ();
	  if (dimension == Syntax::None () 
	      || dimension == Syntax::Unknown ()
	      || dimension == Syntax::Fraction ())
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
  out << record_separator;
  if (flush)
    out.flush ();
}

bool 
LogTable::match (const Daisy& daisy, Treelog& msg)
{
  common_match (daisy, msg);
  return LogSelect::match (daisy, msg);
}
void 
LogTable::done (const std::vector<Time::component_t>& time_columns,
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
LogTable::initial_match (const Daisy& daisy, Treelog& msg)
{
  begin = daisy.time;

  for (unsigned int i = 0; i < summary.size (); i++)
    summary[i]->clear ();

  common_match (daisy, msg);
  return LogSelect::initial_match (daisy, msg);
}
void 
LogTable::initial_done (const std::vector<Time::component_t>& time_columns,
			const Time& time, const double dt)
{ 
  LogSelect::initial_done (time_columns, time, dt);

  if (!print_initial)
    {
      for (unsigned int i = 0; i < entries.size (); i++)
        entries[i]->done (dt);
      return;
    }

  for (unsigned int i = 0; i < entries.size (); i++)
    if (entries[i]->prevent_printing ())
      return;

  common_done (time_columns, time, dt);
  begin = time;
}

void 
LogTable::error ()
{ 
  type = Error;
}

void 
LogTable::missing ()
{ 
  type = Missing;
}

void 
LogTable::add (const std::vector<double>& value)
{ 
  type = Array;
  dest_array = &value;
}

void 
LogTable::add (const double value)
{ 
  type = Number;
  dest_number = value;
}

void 
LogTable::add (const symbol value)
{ 
  type = Name;
  dest_name = value;
}

bool LogTable::check (const Border& border, Treelog& msg) const
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
LogTable::contain_time_columns (const std::vector<Select*>& entries)
{
  static const symbol time ("time");
  for (unsigned int i = 0; i < entries.size (); i++)
    if (entries[i]->path[0] == time)
      return true;
  return false;
}

void
LogTable::initialize (Treelog& msg)
{
  out.open (file.c_str ());

  print_header.start (out, name, file, parsed_from_file);

  for (size_t i = 0; i < parameters.size (); i++)
    print_header.parameter (out, parameters[i].first, parameters[i].second);

  print_header.interval (out, *volume);
  if (description != default_description)
    print_header.log_description (out, description);

  out.flush ();

  Treelog::Open nest (msg, name);
  for (unsigned int i = 0; i < summary.size (); i++)
    summary[i]->initialize (entries, msg);
}

std::vector<std::pair<symbol, symbol>/**/>
LogTable::build_parameters (Block& al)
{
  std::vector<std::pair<symbol, symbol>/**/> result;
  ScopeBlock scope_block (al);
  std::vector<symbol> pars = al.identifier_sequence ("parameter_names");
  for (size_t i = 0; i < pars.size (); i++)
    {
      const symbol key = pars[i];
      if (scope_block.has_identifier (key))
        {
          const symbol value = scope_block.identifier (key);
          std::string id = key.name ();
          std::transform (id.begin (), id.end (), id.begin (), ::toupper);
          result.push_back (std::pair<symbol, symbol> (symbol (id), value));
        }
      else
        al.msg ().warning ("Parameter name '" + key + "' not found"); 
    }
  return result;
}

LogTable::LogTable (Block& al)
  : LogSelect (al),
    parsed_from_file (al.name ("parsed_from_file", "")),
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
    summary (Librarian::build_vector<Summary> (al, "summary")),
    begin (1, 1, 1, 1),
    end (1, 1, 1, 1),
    type (Error),
    dest_number (-42.42e42),
    dest_name ("Daisy bug"),
    dest_array (NULL)
{
  if (!al.ok ())
    return;
  for (unsigned int i = 0; i < entries.size (); i++)
    entries[i]->add_dest (this);
}

void
LogTable::summarize (Treelog& msg)
{
  if (summary.size () > 0)
    {
      Treelog::Open nest (msg, name);
      std::ostringstream tmp;

      tmp << "LOGFILE: " << file  << "\n";
      tmp << "VOLUME: " << volume->one_line_description () << "\n";
      tmp << "TIME: " << begin.print () << " to " << end.print ();
      msg.message (tmp.str ());
      const Timestep step = end - begin;
      for (size_t i = 0; i < summary.size (); i++)
	summary[i]->summarize (Time::hours_between (begin, end), msg);
    }
}

LogTable::~LogTable ()
{
  sequence_delete (summary.begin (), summary.end ());
  if (!out.good ())
    Assertion::error ("Problems writing to '" + file + "'");
}

static struct LogTableSyntax
{
  static Model& make (Block& al)
  { return *new LogTable (al); }

  LogTableSyntax ()
    { 
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      LogSelect::load_syntax (syntax, alist);
      alist.add ("description", LogTable::default_description);
      syntax.add ("parameter_names", Syntax::String, 
                  Syntax::Const, Syntax::Sequence, "\
List of string parameters to print to the table header.\n\
\n\
For example, if you have defined 'column' and 'crop' parameters for\n\
this table log parameterization, you can print them to the log file\n\
header by specifying '(names column crop)'.");
      alist.add ("parameter_names", std::vector<symbol> ());
      syntax.add ("where", Syntax::String, Syntax::Const,
		  "Name of the log file to create.");
      syntax.add ("print_header", Syntax::String, Syntax::Const,
		  "If this is set to 'false', no header is printed.\n\
If this is set to 'true', a full header is printer.\n\
If this is set to 'fixed', a small fixed size header is printed.");
      static VCheck::Enum check_header ("false", "true", "fixed");
      syntax.add_check ("print_header", check_header);
      alist.add ("print_header", "true");
      syntax.add ("print_tags", Syntax::Boolean, Syntax::Const,
		  "Print a tag line in the file.");
      alist.add ("print_tags", true);
      syntax.add ("print_dimension", Syntax::Boolean, Syntax::Const,
		  "Print a line with units after the tag line.");
      alist.add ("print_dimension", true);
      syntax.add ("print_initial", Syntax::Boolean, Syntax::Const,
		  "Print a line with initial values when logging starts.");
      alist.add ("print_initial", true);
      syntax.add ("flush", Syntax::Boolean, Syntax::Const,
		  "Flush to disk after each entry (for debugging).");
      alist.add ("flush", false);
      syntax.add ("record_separator", Syntax::String, Syntax::Const, "\
String to print between records (time steps).");
      alist.add ("record_separator", "\n");
      syntax.add ("field_separator", Syntax::String, Syntax::Const, "\
String to print between fields.");
      alist.add ("field_separator", "\t");
      syntax.add ("error_string", Syntax::String, Syntax::Const, "\
String to print when errors are encountered.");
      alist.add ("error_string", "!");
      syntax.add ("missing_value", Syntax::String, Syntax::Const, "\
String to print when the path doesn't match anything.\n\
This can be relevant for example if you are logging a crop, and there are\n\
no crops on the field.");
      alist.add ("missing_value", "00.00");
      syntax.add ("array_separator", Syntax::String, Syntax::Const, "\
String to print between array entries.");
      alist.add ("array_separator", "\t");
      syntax.add_object ("summary", Summary::component,
                         Syntax::Const, Syntax::Sequence,
                         "Summaries for this log file.");
      alist.add ("summary", std::vector<const AttributeList*> ());
      Librarian::add_type (Log::component, "table", alist, syntax, &make);
      Librarian::add_doc_fun (LogSelect::component, 
                              LogSelect::document_entries);
    }
} LogTable_syntax;

// log_table.C ends here.
