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
#include "geometry.h"
#include "version.h"
#include "daisy.h"
#include <fstream.h>
#include <time.h>

struct LogTable : public LogSelect, public Select::Destination
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

  // Destination Content.
  enum { Error, Missing, Number, Name, Array } type;
  double dest_number;
  string dest_name;
  const vector<double>* dest_array;
  
  // Log.
  bool match (const Daisy& daisy, Treelog& out);
  void done ();

  // Select::Destination
  void error (const string& tag);
  void missing (const string& tag);
  void add (const string& tag, const vector<double>& value);
  void add (const string& tag, double value);
  void add (const string& tag, const string& value);

  // Create and destroy.
  LogTable (const AttributeList& al);
  ~LogTable ();
};

const char *const LogTable::default_description = "\
Each selected variable is represented by a column in the specified log file.";

bool 
LogTable::match (const Daisy& daisy, Treelog& msg)
{
  if (print_header)
    {
      print_dlf_header (out, daisy.alist);
      print_header = false;
    }
  return LogSelect::match (daisy, msg);
}
void 
LogTable::done ()
{ 
  if (!is_printing)
    return;

  for (unsigned int i = 0; i < entries.size (); i++)
    if (entries[i]->prevent_printing ())
      return;

  if (print_tags)
    {
      // Print the entry names in the first line of the log file..
      for (unsigned int i = 0; i < entries.size (); i++)
	{
	  if (i != 0)
	    out << field_separator;

	  const Geometry* geometry = entries[i]->geometry ();
	  const int size = entries[i]->size ();
	  const string tag = entries[i]->tag ();

	  if (geometry && size >= 0)
	    {
	      if (geometry->size () == size)
		{
		  // Content.
		  for (unsigned j = 0; j < size; j++)
		    {
		      if (j != 0)
			out << array_separator;
		      if (tag != "")
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
		      if (tag != "")
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

  for (unsigned int i = 0; i < entries.size (); i++)
    {
      if (i != 0)
	out << field_separator;
      
      entries[i]->done (*this);

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
	  assert (false);
	}
    }
  out << record_separator;
  if (flush)
    out.flush ();
}

void 
LogTable::error (const string&)
{ 
  type = Error;
}

void 
LogTable::missing (const string&)
{ 
  type = Missing;
}

void 
LogTable::add (const string&, const vector<double>& value)
{ 
  type = Array;
  dest_array = &value;
}

void 
LogTable::add (const string&, double value)
{ 
  type = Number;
  dest_number = value;
}

void 
LogTable::add (const string&, const string& value)
{ 
  type = Name;
  dest_name = value;
}

LogTable::LogTable (const AttributeList& al)
  : LogSelect (al),
    file (al.name ("where")),
#ifdef BORLAND_PERMISSIONS
    out (file.c_str (), ios::out|ios::trunc, 0666),
#else
    out (file.c_str ()),
#endif
    flush (al.flag ("flush")),
    record_separator (al.name ("record_separator")),
    field_separator (al.name ("field_separator")),
    error_string (al.name ("error_string")),
    missing_value (al.name ("missing_value")),
    array_separator (al.name ("array_separator")),
    print_header (al.flag ("print_header")),
    print_tags (al.flag ("print_tags")),
    print_dimension (al.flag ("print_dimension")),
    type (Error)
{
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
      const double from  = al.number ("from");
      const double to = al.number ("to");
      if (to < from)
	out << "INTERVAL: [" << from << ";" << to << "]\n";
      const vector<string>& conv_vector = al.name_sequence ("set");
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
      Librarian<Log>::add_type ("table", alist, syntax, &make);
    }
} LogTable_syntax;
