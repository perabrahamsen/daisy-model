// log_table1.C
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


#include "log.h"
#include "condition.h"
#include "time.h"
#include "geometry.h"
#include <fstream.h>
#include <numeric>
#include <set>
#include <map>

typedef map<string, string, less<string>/**/> string_map;

// Entries
struct LogEntry
{
  // Types.
  typedef set<string, less<string>/**/> string_set;

  // Content.
  Condition* condition;		// Should we accumulate now?
  vector<string> path;		// Content of this entry.
  string tag;			// Name of this entry.
  const string dimension;	// Physical dimension of this entry.
  const string description;	// Description of this entry.
  const string missing_value;	// What to print on missing values.

  // Calculation parameters.
  const int start_year;		// For gnuplot time.
  const double factor;		// Convert value.
  const double offset;		// - || -
  /* const */ double from;	// Restrict interval of array.
  /* const */ double to;
  const double content_at;	// Specific position in array for content.
  const double flux_at;		// Specific position in array for fluxes.
  const bool accumulate;	// Accumulate numbers over time.
  const bool full;		// Print out total array.

  // Permanent state.
  double value;		        // Total accumulated value.
  int count;			// Number of accumulated values.
  bool error;			// If an error occured.
  vector<double> full_value;	// Total array.
  string_set names;		// For logging strings.

  // Intermediate state.
  unsigned int current_path_index;// How nested in open's we are.
  unsigned int last_valid_path_index;	// Remember the last valid level.
  bool is_active;		// Should we be accumulating now?

  bool valid ()		// If the current path index is valid.
    { 
      return (current_path_index == last_valid_path_index
	      && current_path_index < path.size ()); 
    }

  void open_group (const string& name) // Open one group level.
    {
      if (valid () && (path[current_path_index] == "*" 
		       || name == path[current_path_index]))
	{
	  if (is_active && last_valid_path_index == path.size () -1)
	    {
	      names.insert (name);
	      count++;
	    }
	  last_valid_path_index++;
	}
      current_path_index++;
    }

  void open (const string& name) // Open one leaf level.
    {
      if (valid () && (path[current_path_index] == "*" 
		       || name == path[current_path_index]))
	last_valid_path_index++;
      current_path_index++;
    }

  void close ()		// Close one level.
    {
      if (current_path_index == last_valid_path_index)
	last_valid_path_index--;
      current_path_index--;
    }

  // Output routines.
  void output (const string& name, const Time& time)
    {
      if (!is_active)
	return;

      open (name);
      if (valid ())
	{
	  const string& type = path[current_path_index];

	  if (type == "year")
	    value += time.year ();
	  else if (type == "month")
	    value += time.month ();
	  else if (type == "mday")
	    value += time.mday ();
	  else if (type == "yday")
	    value += time.yday ();
	  else if (type == "hour")
	    value += time.hour ();
	  else if (type == "week")
	    value += time.week ();
	  else if (type == "wday")
	    value += time.wday ();
	  else
	    error = true;

	  count++;
	}
      else if (current_path_index == last_valid_path_index)
	{
	  double year = time.year () - start_year;
	  double month = time.month ();
	  double mday = time.mday ();
	  double hour = time.hour ();
	  double mlen = Time::month_length (time.year (), time.month ());

	  value += year * 12.0 + month + (mday - 1.0 + hour / 24.0) / mlen;
	  count++;
	}
      close ();
    }

  void output (const string& name, const double number)
    { 
      if (!is_active)
	return;

      open (name);
      if (current_path_index == last_valid_path_index)
	{
	  value += number;
	  count++;
	}
      close ();
    }

  void output (const string& name, const int integer)
    { 
      if (!is_active)
	return;

      open (name);
      if (current_path_index == last_valid_path_index)
	{
	  value += integer;
	  count++;
	}
      close ();
    }

  void output (const string& name, const string& a_name)
    {
      if (!is_active)
	return;

      open (name);
      if (current_path_index == last_valid_path_index)
	{
	  names.insert (a_name);
	  count++;
	}
      close ();
    }

  void output (const string& name, const vector<double>& array,
	       const Geometry* geometry)
    { 
      if (!is_active)
	return;

      open (name);
      if (current_path_index == last_valid_path_index)
	{
	  if (full)
	    {
	      if (count == 0)
		full_value = array;
	      else 
		{
		  if (array.size () > full_value.size ())
		    full_value.insert (full_value.end (), 
				       full_value.size () - array.size (),
				       0.0);
		  for (unsigned int i = 0; i < array.size (); i++)
		    full_value[i] += array[i];
		}
	    }
	  else if (geometry)
	    {
	      if (content_at <= 0)
		value += array[geometry->interval_plus (content_at)];
	      else if (flux_at <= 0)
		value += array[geometry->interval_border (flux_at)];
	      else if (to > from)
		value += geometry->total (array);
	      else
		value += geometry->total (array, from, to);
	    }
	  else
	    {
#ifdef BORLAND_TEMPLATES
	      for (unsigned int i = 0; i < array.size (); i++)
		value += array[i];
#else
	      // Global fun hidden by LogEntry::accumulate.
	      value += ::accumulate (array.begin (), array.end (), 0.0);
#endif
	    }
	  count++;
	}
      close ();
    }

  // Reset at start of time step.
  bool match (const Daisy& daisy, Treelog& out, bool is_printing)
    {
      daisy_assert (current_path_index == 0U);
      daisy_assert (last_valid_path_index == 0U);

      if (condition)
	{
	  condition->tick (daisy, out);
	  is_active = condition->match (daisy);
	}
      else
	is_active = is_printing;
      return is_active;
    }

  // Print result at end of time step.
  void done (ostream& out)
    {
      daisy_assert (current_path_index == 0U);
      daisy_assert (last_valid_path_index == 0U);

      if (error)
	out << "!";
      else if (count == 0)
	out << missing_value;
      else if (full)
	{
	  for (unsigned int i = 0; i < full_value.size (); i++)
	    {
	      if (i != 0)
		out << " ";
	      out << (full_value[i] * factor + offset);
	    }
	  if (!accumulate)
	    fill (full_value.begin (), full_value.end (), 0.0);
	}
      else if (names.size () > 0)
	{
	  for (string_set::const_iterator i = names.begin ();
		  i != names.end ();
		  i++)
	    {
	      if (i != names.begin ())
		out << " + ";
	      out << (*i);
	    }
  	  names.erase (names.begin (), names.end ());
	}
      else
	{
	  out << (value * factor + offset);
	  if (!accumulate)
	    value = 0.0;
	}
      count = 0;
      error = false;
    }
  // Create and Destroy.
  void initialize (const string_map conv)
    {
      // Convert path according to mapping in 'conv'.
      for (unsigned int i = 0; i < path.size (); i++)
	{
	  string_map::const_iterator entry = conv.find (path[i]);
	  if (entry != conv.end ())
	    path[i] = (*entry).second;
	  else if (path[i].size () > 0 && path[i][0] == '$')
	    path[i] = "*";
	}
    }
  LogEntry (const AttributeList& al)
    : condition (al.check ("when") 
		 ? &Librarian<Condition>::create (al.alist ("when"))
		 : NULL),
      path (al.name_sequence ("path")),
      dimension (al.name ("dimension")),
      description (al.name ("description")),
      missing_value (al.name ("missing_value")),
      start_year (al.integer ("start_year")),
      factor (al.number ("factor")),
      offset (al.number ("offset")),
      from (al.number ("from")),
      to (al.number ("to")),
      content_at (al.number ("content_at")),
      flux_at (al.number ("flux_at")),
      accumulate (al.flag ("accumulate")),
      full (al.flag ("full")),
      value (al.number ("value")),
      count (al.integer ("count")),
      error (false),
      current_path_index (0U),
      last_valid_path_index (0U),
      is_active (false)
    { 
      if (al.check ("tag"))
	tag = al.name ("tag");
      else if (path.size () > 0)
	tag = path[path.size () - 1];
      else 
	tag = "<none>";
    }
  ~LogEntry ()
  { delete condition; }
};

struct LogTable1 : public Log
{
  // State. 
  bool is_printing;		// True iff this time step should be logged.
  bool is_active;		// True iff we need values for this time step.

  // Filter functions.
  bool check (const string&) const
    { return is_active; }
  bool check_derived (const string&, const string&, const Library&) const
    { return is_active; }
  const string description;	// Description of table.
  string file;			// Filename.
  ofstream out;			// Output stream.
  Condition& condition;	// Should we print a log now?

  vector<LogEntry*> entries;

  // Display.
  const bool print_tags;	// Show tags on first line?
  const bool print_dimension;	// Show dimensions in next line?
  const bool flush;		// Flush after each time step.

  // Default Range.
  const double from;		// Sum arrays from here...
  const double to;		// to here.

  // Checking to see if we should log this time step.
  bool match (const Daisy& daisy, Treelog& out)
    {
      condition.tick (daisy, out);
      is_printing = condition.match (daisy);
      is_active = is_printing;

      for (unsigned int i = 0; i < entries.size (); i++)
	if (entries[i]->match (daisy, out, is_printing))
	  is_active = true;
      
      return is_active;
    }

  void done ()
    {
      if (!is_printing)
	return;
      // Print each entry.
      bool first = true;
      for (unsigned int i = 0; i < entries.size (); i++)
	{
	  if (first)
	    first = false;
	  else
	    out << "\t";
	  entries[i]->done (out);
	}
      out << "\n";
      if (flush)
	out.flush ();
    }

  // Normal items.
  void open (const string& name)
    { 
      for (unsigned int i = 0; i < entries.size (); i++)
	entries[i]->open_group (name);
    }
  void close ()
    { 
      for (unsigned int i = 0; i < entries.size (); i++)
	entries[i]->close ();
    }

  // Unnamed items.
  void open_unnamed ()
    { }
  void close_unnamed ()
    { }

  // Derived items.
  void open_derived (const string& field, const string& type)
    { open (field); open (type); }
  void close_derived ()
    { close (); close (); }

  // Derived items in a list.
  void open_entry (const string& type, const AttributeList&)
    { open (type); }
  void close_entry ()
    { close (); }

  // Named derived items in a list.
  void open_named_entry (const string& name, const string&,
			 const AttributeList&)
    { open (name); }
  void close_named_entry ()
    { close (); }

  void output (const string& name, const Time& value)
    { 
      if (is_active)
	for (unsigned int i = 0; i < entries.size (); i++)
	  entries[i]->output (name, value);
    }
  void output (const string&, const bool)
    { }
  void output (const string& name, const double value)
    { 
      if (is_active)
	for (unsigned int i = 0; i < entries.size (); i++)
	  entries[i]->output (name, value);
    }
  void output (const string& name, const int value)
    { 
      if (is_active)
	for (unsigned int i = 0; i < entries.size (); i++)
	  entries[i]->output (name, value);
    }
  void output (const string& name, const string& value)
    { 
      if (is_active)
	for (unsigned int i = 0; i < entries.size (); i++)
	  entries[i]->output (name, value);
    }
  void output (const string& name, const vector<double>& value)
    { 
      if (is_active)
	for (unsigned int i = 0; i < entries.size (); i++)
	  entries[i]->output (name, value, geometry ());
    }
  void output (const string&, const PLF&)
    { }

  // Create and Destroy.
  bool check (const Syntax&, Treelog&) const
    { return true; }

  LogTable1 (const AttributeList& al)
    : Log (al),
      description (al.name ("description")),
      file (al.name ("where")),
#ifdef BORLAND_PERMISSIONS
      out (file.c_str (), ios::out|ios::trunc, 0666),
#else
      out (file.c_str ()),
#endif
      condition (Librarian<Condition>::create (al.alist ("when"))),
      entries (map_construct<LogEntry> (al.alist_sequence ("entries"))),
      print_tags (al.flag ("print_tags")),
      print_dimension (al.flag ("print_dimension")),
      flush (al.flag ("flush")),
      from (al.number ("from")),
      to (al.number ("to"))
    {
      // Create path convertion map.
      const vector<string>& conv_vector = al.name_sequence ("set");
      string_map conv_map;
      for (unsigned int i = 0; i < conv_vector.size (); i += 2)
	{
	  daisy_assert (i+1 < conv_vector.size ());
	  conv_map[conv_vector[i]] = conv_vector[i+1];
	}
      for (unsigned int i = 0; i < entries.size (); i++)
	entries[i]->initialize (conv_map);

      // You can set the print range from here.
      for (unsigned int i = 0; i < entries.size (); i++)
	{
	  if (from <= 0.0 && entries[i]->from > 0.0)
	    entries[i]->from = from;
	  if (to <= 0.0 && entries[i]->to > 0.0)
	    entries[i]->to = to;
	}
#if 0      
      if (description.size () > 0)
	// Print description in start of file.
	out << description << "\n";
#endif
      if (print_tags)
	{
	  // Print the entry names in the first line of the log file..
	  bool first = true;
	  for (unsigned int i = 0; i < entries.size (); i++)
	    {
	      if (first)
		first = false;
	      else
		out << "\t";
	      out << entries[i]->tag;
	    }
	  out << "\n";
	}
      if (print_dimension)
	{
	  // Print the entry names in the first line of the log file..
	  bool first = true;
	  for (unsigned int i = 0; i < entries.size (); i++)
	    {
	      if (first)
		first = false;
	      else
		out << "\t";
	      out << entries[i]->dimension;
	    }
	  out << "\n";
	}
	
    }

  ~LogTable1 ()
    {
      if (!out.good ())
	throw (string ("Problems writing to '") + file + "'");
      delete &condition;
      sequence_delete (entries.begin (), entries.end ());
    }
};

static struct LogTable1Syntax
{
  static Log& make (const AttributeList& al)
  { return *new LogTable1 (al); }

  static bool check_alist (const AttributeList& al, Treelog& err)
  {
    bool ok = true;

    if ((al.size ("set") % 2) == 1)
      {
	err.entry ("'set' should contain an even number of arguments");
	ok = false;
      }

    return ok;
  }
  static bool check_entry_alist (const AttributeList& al, Treelog& err)
    {
      bool ok = true;

      bool specify_content = (al.number ("content_at") <= 0.0);
      bool specify_flux = (al.number ("flux_at") <= 0.0);
      bool specify_interval = !(al.number ("to") > al.number ("from"));
      

      if (specify_content && specify_flux)
	{
	  err.entry ("You cannot specify both 'content_at' and 'flux_at'");
	  ok = false;
	}
      if ((specify_content || specify_flux) && specify_interval)
	{
	  err.entry ("You cannot specify both position and interval");
	  ok = false;
	}

      return ok;
    }

  LogTable1Syntax ()
    { 
      Syntax& syntax = *new Syntax ();
      syntax.add_check (check_alist);
      AttributeList& alist = *new AttributeList ();
      syntax.add ("description", Syntax::String, Syntax::Const,
		  "Description of this log file format.");
      alist.add ("description", "\
Each selected variable is represented by a column in the log file.");
      syntax.add ("where", Syntax::String, Syntax::Const,
		  "Name of the log file to create.");
      syntax.add ("when", Librarian<Condition>::library (), 
		  "Add entries to the log file when this condition is true.");
      
      Syntax& entry_syntax = *new Syntax ();
      entry_syntax.add_check (check_entry_alist);
      AttributeList entry_alist;
      entry_syntax.add ("tag", Syntax::String, Syntax::OptionalConst,
			"Tag to identify the column.\n\
These will be printed in the first line of the log file.\n\
The default tag is the last element in the path.");
      entry_syntax.add ("dimension", Syntax::String, Syntax::Const,
			"The unit for numbers in this column.\n\
These will be printed in the second line of the log file.");
      entry_alist.add ("dimension", "");
      entry_syntax.add ("description", Syntax::String, Syntax::Const,
			"A description of this column.");
      entry_alist.add ("description", "\
Each entry represents one column in the log file.");
      entry_syntax.add ("path", Syntax::String, Syntax::Const, 
			Syntax::Sequence, "\
Sequence of attribute names leading to the variable you want to log in\n\
this column.  The first name should be one of the attributes of the\n\
daisy component itself.  What to specify as the next name depends on\n\
the type of the attribute you selected before.\n\
\n\
If the value of that attribute itself is a fixed component, you should\n\
specify the name of an attribute in that component as the second name.\n\
\n\
If the value is a library component, you should specify the name of\n\
the model or parameterization you are interested in, and then the name\n\
of the attribute inside the model you want to log.\n\
\n\
If the attribute is a date, you should specify 'year', 'month',\n\
'mday', or 'hour'.  These are all integer values.  If you don't specify\n\
any of these, a special ever increasing 'gnuplot' value will be calculated.\n\
\n\
The last attribute in the patch should be a number, a number sequence,\n\
a string, or an integer.  These are the only values which can be\n\
logged by this model.\n\
\n\
You can use the special value \"*\" to match everything at a given\n\
level, for example all crops.  This way the path can specify multiple\n\
values, they will be added before they are printed in the log file.\n\
All values that start with a \"$\" will work like \"*\".  They are intended\n\
to be mapped with the 'set' attribute in the 'table' log model.");
      entry_syntax.add ("missing_value", Syntax::String, Syntax::Const, "\
String to print when the path doesn't match anything.\n\
This can be relevant for example if you are logging a crop, and there are\n\
no crops on the field.");
      entry_alist.add ("missing_value", "00.00");
      entry_syntax.add ("when", 
			Librarian<Condition>::library (),
			Syntax::OptionalConst, Syntax::Singleton,
			"\
When to calculate the values in this column.\n\
By default, the values will be calculated once, when the a new log entry\n\
is written.  If you calculate the values more often, they will be\n\
accumulated.  This is useful if you for example want to summarize the\n\
hourly percolation into a daily log.");
      entry_syntax.add ("start_year", Syntax::Integer, Syntax::Const,
			"Start year for gnuplot dates.");
      entry_alist.add ("start_year", 0);
      entry_syntax.add ("factor", Syntax::None (), Syntax::Const, "\
Factor to multiply the calculated value with, before logging.");
      entry_alist.add ("factor", 1.0);
      entry_syntax.add ("offset", Syntax::Unknown (), Syntax::Const, "\
Offset to add to the calculated value, before logging.");
      entry_alist.add ("offset", 0.0);
      entry_syntax.add ("from", "cm", Syntax::Const, "\
Summarize soil content of numeric array between 'from' and 'to' with\n\
regard to the current geomerty.  A negative number, or '0.0' at the\n\
surface.");
      entry_alist.add ("from", 0.0);
      entry_syntax.add ("to", "cm", Syntax::Const, "\
Summarize soil content of numeric array between 'from' and 'to' with\n\
regard to the current geomerty.  Should be below 'from' to indicate the\n\
of the layer, or above to summarize over the entire soil.");
      entry_alist.add ("to", 1.0);
      entry_syntax.add ("content_at", "cm", Syntax::Const, "\
Give the content of interval at the specified depth, if negative.");
      entry_alist.add ("content_at", 1.0);
      entry_syntax.add ("flux_at", "cm", Syntax::Const,
			"Specify position to measure flux. \n\
The closest interval border will be used.  Ignore if not-negative.");
      entry_alist.add ("flux_at", 1.0);
      entry_syntax.add ("accumulate", Syntax::Boolean, Syntax::Const,
			"Log accumulated values.");
      entry_alist.add ("accumulate", false);
      entry_syntax.add ("full", Syntax::Boolean, Syntax::Const,
			"Log the full content of the array.\n\
The entries are separated with space.");
      entry_alist.add ("full", false);
      entry_syntax.add ("value", Syntax::Unknown (), Syntax::State,
			"The current accumulated value.");
      entry_alist.add ("value", 0.0);
      entry_syntax.add ("count", Syntax::Integer, Syntax::State, "\
Number of times the path has matched a variable since the last log entry.");
      entry_alist.add ("count", 0);
      syntax.add ("entries", entry_syntax, entry_alist, Syntax::State,
		  Syntax::Sequence, "What to log in each column.");
      syntax.add ("set", Syntax::String, Syntax::Const, Syntax::Sequence, 
		  "Map path names in the entries.\n\
The first entry in the sequence is a symbol from the paths (e.g. $crop),\n\
and the second is the value to replace the symbol with (e.g. Grass).\n\
The third entry is another symbol to replace, and the fourth is another\n\
value to replace it with.  And so forth.");
      const vector<string> empty_string_vector;
      alist.add ("set", empty_string_vector);
      syntax.add ("print_tags", Syntax::Boolean, Syntax::Const,
		  "Print a tag line in the file.");
      alist.add ("print_tags", true);
      syntax.add ("print_dimension", Syntax::Boolean, Syntax::Const,
		  "Print a line with units after the tag line.");
      alist.add ("print_dimension", true);
      syntax.add ("flush", Syntax::Boolean, Syntax::Const,
		  "Flush to disk after each entry (for debugging).");
      alist.add ("flush", false);
      syntax.add ("from", "cm", Syntax::Const,
		  "Default 'from' value for all entries.");
      alist.add ("from", 0.0);
      syntax.add ("to", "cm", Syntax::Const,
		  "Default 'to' value for all entries.");
      alist.add ("to", 1.0);

      Librarian<Log>::add_type ("table1", alist, syntax, &make);
    }
} LogTable1_syntax;
