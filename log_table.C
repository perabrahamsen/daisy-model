// log_table.C
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


#include "log_select.h"
#include "select.h"
#include "summary.h"
#include "geometry.h"
#include "version.h"
#include "daisy.h"
#include "tmpstream.h"
#include <fstream>
#include <time.h>

struct LogTable : public LogSelect, public Destination
{
  static const char *const default_description;

  // File Content.
  string file;			// Filename.
  ofstream out;			// Output stream.
  const bool flush;		// Flush after each time step.
  const string record_separator; // String to print on records (time steps).
  const string field_separator;	// String to print between fields.
  const string error_string;	// String to print on errors.
  const string missing_value;	// String to print for missing values.
  const string array_separator;	// String to print between array entries.
  bool print_header;		// Set if header should be printed.
  bool print_tags;		// Set if tags should be printed.
  bool print_dimension;		// Set if dimensions should be printed.
  const bool time_columns;	// Add year, month, day and hour columns.
  const vector<Summary*> summary; // Summarize this log file.
  Time begin;			// First log entry.
  Time end;			// Last log entry.

  // destination Content.
  enum { Error, Missing, Number, Name, Array } type;
  double dest_number;
  symbol dest_name;
  const vector<double>* dest_array;
  
  // Log.
  void common_match (const Daisy& daisy, Treelog& out);
  void common_done (const Time& time);

  // Log.
  bool match (const Daisy& daisy, Treelog& out);
  void done (const Time& time);

  // Initial line.
  bool initial_match (const Daisy&, Treelog&);
  void initial_done (const Time& time);

  // Select::Destination
  void error ();
  void missing ();
  void add (const vector<double>& value);
  void add (double value);
  void add (symbol value);

  // Create and destroy.
  bool check (const Syntax&, Treelog& msg) const;
  static bool contain_time_columns (const vector<Select*>& entries);
  void initialize (Treelog&);
  LogTable (const AttributeList& al);
  void summarize (Treelog&);
  ~LogTable ();
};

const char *const LogTable::default_description = "\
Each selected variable is represented by a column in the specified log file.";

void
LogTable::common_match (const Daisy& daisy, Treelog&)
{
  if (print_header)
    {
      print_dlf_header (out, daisy.alist);
      print_header = false;
    }
}

void 
LogTable::common_done (const Time& time)
{ 
  if (print_tags)
    {
      if (time_columns)
	out << "year\tmonth\tmday\thour\t";

      // Print the entry names in the first line of the log file..
      for (unsigned int i = 0; i < entries.size (); i++)
	{
	  if (i != 0)
	    out << field_separator;

	  const Geometry* geometry = entries[i]->geometry ();
	  const int size = entries[i]->size ();
	  const symbol tag = entries[i]->tag ();
	  static const symbol empty_symbol ("");

	  if (geometry && size >= 0)
	    {
	      if (geometry->size () == size)
		{
		  // Content.
		  for (unsigned j = 0; j < size; j++)
		    {
		      if (j != 0)
			out << array_separator;
		      if (tag != empty_symbol)
			out << tag << " @ ";
		      out << geometry->z (j);
		    }
		}
	      else if (geometry->size () + 1 == size)
		{
		  // Flux
		  double last = 0.0;
		  
		  for (unsigned j = 0; j < size; j++)
		    {
		      if (j != 0)
			out << array_separator;
		      if (tag != empty_symbol)
			out << tag << " @ ";
		      out << last;
		      if (j <  geometry->size ())
			last = geometry->zplus (j);
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
      if (time_columns)
	out << "\t\t\t\t";

      // Print the entry names in the first line of the log file..
      for (unsigned int i = 0; i < entries.size (); i++)
	{
	  if (i != 0)
	    out << field_separator;

	  const Geometry* geometry = entries[i]->geometry ();
	  const int size = entries[i]->size ();
	  string dimension = entries[i]->dimension ();
	  if (dimension == Syntax::None () 
	      || dimension == Syntax::Unknown ()
	      || dimension == Syntax::Fraction ())
	    dimension = "";

	  if (geometry && size >= 0)
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

  if (time_columns)
    out << time.year () << "\t" << time.month () << "\t" 
	<< time.mday () << "\t" << time.hour () << "\t";

  for (unsigned int i = 0; i < entries.size (); i++)
    {
      if (i != 0)
	out << field_separator;
      
      entries[i]->done ();

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
	    const vector<double> array = *dest_array;
	    for (unsigned int i = 0; i < array.size (); i++)
	      {
		if (i != 0)
		  out << array_separator;
		out << array[i];
	      }
	  }
	  break;
	default:
	  daisy_assert (false);
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
LogTable::done (const Time& time)
{ 
  LogSelect::done (time);

  if (!is_printing)
    return;

  for (unsigned int i = 0; i < entries.size (); i++)
    if (entries[i]->prevent_printing ())
      return;

  common_done (time);
  end = time;
}
bool 
LogTable::initial_match (const Daisy& daisy, Treelog& msg)
{
  common_match (daisy, msg);
  return LogSelect::initial_match (daisy, msg);
}
void 
LogTable::initial_done (const Time& time)
{ 
  for (unsigned int i = 0; i < summary.size (); i++)
    summary[i]->clear ();

  LogSelect::initial_done (time);

  for (unsigned int i = 0; i < entries.size (); i++)
    if (entries[i]->prevent_printing ())
      return;

  common_done (time);
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
LogTable::add (const vector<double>& value)
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

bool LogTable::check (const Syntax&, Treelog& msg) const
{ 
  Treelog::Open nest (msg, name);
  bool ok = true;
  if (!out.good ())
    {
      TmpStream tmp;
      tmp () << "Write error for '" << file << "'";
      msg.error (tmp.str ());
      ok = false;
    }
  return ok; 
}

bool 
LogTable::contain_time_columns (const vector<Select*>& entries)
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
  Treelog::Open nest (msg, name);
  for (unsigned int i = 0; i < summary.size (); i++)
    summary[i]->initialize (entries, msg);
}

LogTable::LogTable (const AttributeList& al)
  : LogSelect (al),
    file (al.name ("where")),
    out (file.c_str ()),
    flush (al.flag ("flush")),
    record_separator (al.name ("record_separator")),
    field_separator (al.name ("field_separator")),
    error_string (al.name ("error_string")),
    missing_value (al.name ("missing_value")),
    array_separator (al.name ("array_separator")),
    print_header (al.flag ("print_header")),
    print_tags (al.flag ("print_tags")),
    print_dimension (al.flag ("print_dimension")),
    time_columns (!contain_time_columns (entries)),
    summary (map_create<Summary> (al.alist_sequence ("summary"))),
    begin (1, 1, 1, 1),
    end (1, 1, 1, 1),
    type (Error)
{
  for (unsigned int i = 0; i < entries.size (); i++)
    entries[i]->add_dest (this);

  if (print_header)
    {
      out << "dlf-0.0 -- " << name;
      if (al.check ("parsed_from_file"))
	out << " (defined in '" << al.name ("parsed_from_file") << "').";
      out << "\n";
      out << "\n";
      out << "VERSION: " << version  << "\n";
      out << "LOGFILE: " << file  << "\n";
      time_t now = time (NULL);
      out << "RUN: " << ctime (&now);
      if (to < from)
	out << "INTERVAL: [" << from << ";" << to << "]\n";
      for (unsigned int i = 0; i < conv_vector.size (); i += 2)
	out << "SET: " << conv_vector[i] << " = " << conv_vector[i+1] << "\n";
      if (description != default_description)
	{
	  out << "\nLOG: ";
	  for (unsigned int i = 0; i < description.size (); i++)
	    if (description[i] != '\n')
	      out << description[i];
	    else
	      out << "\nLOG: ";
	  out << "\n";
	}
      out << "\n";
    }
  out.flush ();
}

void
LogTable::summarize (Treelog& msg)
{
  if (summary.size () > 0)
    {
      Treelog::Open nest (msg, name);
      TmpStream tmp;

      tmp () << "LOGFILE: " << file  << "\n";
      if (to < from)
	tmp () << "INTERVAL: [" << from << ";" << to << "]\n";
      tmp () << "TIME: " 
	     << begin.year () << "-" << begin.month () << "-" << begin.mday () 
	     << ":" << begin.hour () << " to "
	     << end.year () << "-" << end.month () << "-" << end.mday () 
	     << ":" << end.hour ();
      for (unsigned int i = 0; i < conv_vector.size (); i += 2)
	tmp () << "\nSET: " << conv_vector[i] << " = "
	       << conv_vector[i+1];
      msg.message (tmp.str ());
      
      for (unsigned int i = 0; i < summary.size (); i++)
	summary[i]->summarize (Time::hours_between (begin, end), msg);
    }
}

LogTable::~LogTable ()
{
  if (!out.good ())
    throw (string ("Problems writing to '") + file + "'");
}

static struct LogTableSyntax
{
  static Log& make (const AttributeList& al)
    { return *new LogTable (al); }

  LogTableSyntax ()
    { 
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      LogSelect::load_syntax (syntax, alist);
      alist.add ("description", LogTable::default_description);
      syntax.add ("where", Syntax::String, Syntax::Const,
		  "Name of the log file to create.");
      syntax.add ("print_header", Syntax::Boolean, Syntax::Const,
		  "Print header section of the file.");
      alist.add ("print_header", true);
      syntax.add ("print_tags", Syntax::Boolean, Syntax::Const,
		  "Print a tag line in the file.");
      alist.add ("print_tags", true);
      syntax.add ("print_dimension", Syntax::Boolean, Syntax::Const,
		  "Print a line with units after the tag line.");
      alist.add ("print_dimension", true);
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
      syntax.add ("summary", Librarian<Summary>::library (),
		  Syntax::Const, Syntax::Sequence,
		  "Summaries for this log file.");
      alist.add ("summary", vector<AttributeList*> ());
      Librarian<Log>::add_type ("table", alist, syntax, &make);
    }
} LogTable_syntax;
