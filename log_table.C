// log_table.C

#include "log_select.h"
#include "select.h"
#include "geometry.h"
#include <fstream.h>

struct LogTable : public LogSelect, public Select::Destination
{
  // File Content.
  string file;			// Filename.
  ofstream out;			// Output stream.
  const bool flush;		// Flush after each time step.
  const string record_separator; // String to print on records (time steps).
  const string field_separator;	// String to print between fields.
  const string error_string;	// String to print on errors.
  const string missing_value;	// String to print for missing values.
  const string array_separator;	// String to print between array entries.
  bool print_tags;		// Set if tags should be printed.
  bool print_dimension;		// Set if dimensions should be printed.

  // Destination Content.
  enum { Error, Missing, Number, Name, Array } type;
  double dest_number;
  string dest_name;
  const vector<double>* dest_array;
  
  // Log.
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

void 
LogTable::done ()
{ 
  if (!is_printing)
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
	  const string tag = entries[i]->tag;

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
	    out << entries[i]->tag;
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
	  const string dimension = entries[i]->dimension;

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
	    out << entries[i]->dimension;
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
    print_tags (al.flag ("print_tags")),
    print_dimension (al.flag ("print_dimension")),
    
    type (Error)
{
#if 0      
  if (description.size () > 0)
    // Print description in start of file.
    out << description << "\n";
#endif
}

LogTable::~LogTable ()
{
  if (!out.good ())
    CERR << "Problems writing to `" << file << "'\n";
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
      syntax.add ("where", Syntax::String, Syntax::Const,
		  "Name of the log file to create.");
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

