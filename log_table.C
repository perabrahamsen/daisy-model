// log_table.C

#include "log.h"
#include "condition.h"
#include "time.h"
#include "frame.h"
#include "geometry.h"
#include <fstream.h>
#include <numeric>
#include <algorithm>

// Entries
struct LogEntry
{
  // Content.
  Condition* condition;		// Should we accumulate now?
  const vector<string> path;	// Content of this entry.
  string tag;			// Name of this entry.
  const string missing_value;	// What to print on missing values.

  // Calculation parameters.
  const int start_year;		// For gnuplot time.
  const double factor;		// Convert value.
  const double offset;		// - || -
  const double from;		// Restrict interval of array.
  const double to;
  const double at;		// Specific position in array.
  const bool accumulate;	// Accumulate numbers over time.

  // Permanent state.
  double value;		// Total accumulated value.
  int count;			// Number of accumulated values.
  bool error;			// If an error occured.

  // Intermediate state.
  unsigned int current_path_index;// How nested in open's we are.
  unsigned int last_valid_path_index;	// Remember the last valid level.
  bool is_active;		// Should we be accumulating now?

  bool valid ()		// If the current path index is valid.
    { 
      return (current_path_index == last_valid_path_index
	      && current_path_index < path.size ()); 
    }

  void open (const string& name) // Open one level.
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

  void output (const string& name, const vector<double>& array,
	       const Geometry* geometry)
    { 
      if (!is_active)
	return;

      open (name);
      if (current_path_index == last_valid_path_index)
	{
	  if (geometry)
	    {
	      if (at <= 0)
		value += array[geometry->interval_plus (at)];
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
	      // Bug: Stupid Filer::accumulate hides this.
	      value += ::accumulate (array.begin (), array.end (), 0.0);
#endif
	    }
	  count++;
	}
      close ();
    }

  // Reset at start of time step.
  bool match (const Frame& frame, const Daisy& daisy, bool is_printing)
    {
      assert (current_path_index == 0U);
      assert (last_valid_path_index == 0U);

      if (condition)
	is_active = condition->match (frame, daisy);
      else
	is_active = is_printing;
      return is_active;
    }

  // Print result at end of time step.
  void done (ostream& out)
    {
      assert (current_path_index == 0U);
      assert (last_valid_path_index == 0U);

      if (error)
	out << "!";
      else if (count == 0)
	out << missing_value;
      else
	out << (value * factor + offset);

      if (!accumulate)
	value = 0.0;
      count = 0;
      error = false;
    }
  // Create and Destroy.
  LogEntry (const AttributeList& al)
    : condition (al.check ("when") 
		 ? &Librarian<Condition>::create (al.alist ("when"))
		 : NULL),
      path (al.name_sequence ("path")),
      missing_value (al.name ("missing_value")),
      start_year (al.integer ("start_year")),
      factor (al.number ("factor")),
      offset (al.number ("offset")),
      from (al.number ("from")),
      to (al.number ("to")),
      at (al.number ("at")),
      accumulate (al.flag ("accumulate")),
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
    { delete &condition; }
};

struct LogTable : public Log, public Filter
{
  // State. 
  bool is_printing;		// True iff this time step should be logged.
  bool is_active;		// True iff we need values for this time step.

  // Filter functions.
  bool check (const string&, bool) const
    { return is_active; }
  Filter& lookup (const string&) const
    { 
      // Bug: We should get rid of the filter all together.
      return const_cast<LogTable&> (*this); 
    }

  // Content.
  string file;			// Filename.
  ofstream out;			// Output stream.
  Condition& condition;	// Should we print a log now?

  vector<LogEntry*> entries;

  // Display.
  bool print_tags;		// Show tags on first line?
  bool flush;			// Flush after each time step.

  // Checking to see if we should log this time step.
  Filter& match (const Frame& frame, const Daisy& daisy)
    {
      is_printing = condition.match (frame, daisy);
      is_active = is_printing;

      for (unsigned int i = 0; i < entries.size (); i++)
	if (entries[i]->match (frame, daisy, is_printing))
	  is_active = true;
      
      return *this;
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

  // Open normal items.
  void open (const string& name)
    { 
      for (unsigned int i = 0; i < entries.size (); i++)
	entries[i]->open (name);
    }
  void close ()
    { 
      for (unsigned int i = 0; i < entries.size (); i++)
	entries[i]->close ();
    }

  // Ignore unnamed items.
  void open_unnamed ()
    { }
  void close_unnamed ()
    { }

  // Open derived items two steps a time.
  void open_derived (const string& field, const string& type)
    { open (field); open (type); }
  void close_derived ()
    { close (); close (); }

  // Open derived items in list normally.
  void open_entry (const string& type)
    { open (type); }
  void close_entry ()
    { close (); }

  void output (const string& name, Filter&, const Time& value, bool)
    { 
      if (is_active)
	for (unsigned int i = 0; i < entries.size (); i++)
	  entries[i]->output (name, value);
    }
  void output (const string&, Filter&, const bool, bool)
    { }
  void output (const string& name, Filter&, const double value, bool)
    { 
      if (is_active)
	for (unsigned int i = 0; i < entries.size (); i++)
	  entries[i]->output (name, value);
    }
  void output (const string& name, Filter&, const int value, bool)
    { 
      if (is_active)
	for (unsigned int i = 0; i < entries.size (); i++)
	  entries[i]->output (name, value);
    }
  void output (const string&, Filter&, const string&, bool)
    { }
  void output (const string& name, Filter&, const vector<double>& value, bool)
    { 
      if (is_active)
	for (unsigned int i = 0; i < entries.size (); i++)
	  entries[i]->output (name, value, geometry ());
    }
  void output (const string&, Filter&, const CSMP&, bool)
    { }

  // Create and Destroy.
  bool check (const Syntax&) const
    { return true; }

  LogTable (const AttributeList& al)
    : Log (),
      file (al.name ("where")),
#ifdef BORLAND_PERMISSIONS
      out (file.c_str (), ios::out|ios::trunc, 0666),
#else
      out (file.c_str ()),
#endif
      condition (Librarian<Condition>::create (al.alist ("when"))),
      entries (map_construct<LogEntry> (al.alist_sequence ("entries"))),
      print_tags (al.flag ("print_tags")),
      flush (al.flag ("flush"))
    {
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
    }

  ~LogTable ()
    {
#ifdef CONST_DELETE
      if (!out.good ())
	cerr << "Problems writing to `" << file << "'\n";
      delete &condition;
      sequence_delete (entries.begin (), entries.end ());
#endif
    }
};

static struct LogTableSyntax
{
  static Log& make (const AttributeList& al)
    { return *new LogTable (al); }

  LogTableSyntax ()
    { 
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      syntax.add ("where", Syntax::String, Syntax::Const);
      syntax.add ("when", Librarian<Condition>::library (), Syntax::Const);
      
      Syntax& entry_syntax = *new Syntax ();
      AttributeList& entry_alist = *new AttributeList ();
      entry_syntax.add ("tag", Syntax::String, Syntax::Optional);
      entry_syntax.add ("path", Syntax::String, Syntax::Const, 
			Syntax::Sequence);
      entry_syntax.add ("missing_value", Syntax::String, Syntax::Const);
      entry_alist.add ("missing_value", "00.00");
      entry_syntax.add ("when", 
			Librarian<Condition>::library (), Syntax::Optional);
      entry_syntax.add ("start_year", Syntax::Integer, Syntax::Const);
      entry_alist.add ("start_year", 0);
      entry_syntax.add ("factor", Syntax::Number, Syntax::Const);
      entry_alist.add ("factor", 1.0);
      entry_syntax.add ("offset", Syntax::Number, Syntax::Const);
      entry_alist.add ("offset", 0.0);
      entry_syntax.add ("from", Syntax::Number, Syntax::Const);
      entry_alist.add ("from", 0.0);
      entry_syntax.add ("to", Syntax::Number, Syntax::Const);
      entry_alist.add ("to", 1.0);
      entry_syntax.add ("at", Syntax::Number, Syntax::Const);
      entry_alist.add ("at", 1.0);
      entry_syntax.add ("accumulate", Syntax::Boolean, Syntax::Const);
      entry_alist.add ("accumulate", false);
      entry_syntax.add ("value", Syntax::Number, Syntax::State);
      entry_alist.add ("value", 0.0);
      entry_syntax.add ("count", Syntax::Integer, Syntax::State);
      entry_alist.add ("count", 0);
      syntax.add ("entries", entry_syntax, Syntax::Const, Syntax::Sequence);
      alist.add ("entries", entry_alist);
      
      syntax.add ("print_tags", Syntax::Boolean, Syntax::Const);
      alist.add ("print_tags", true);
      syntax.add ("flush", Syntax::Boolean, Syntax::Const);
      alist.add ("flush", false);

      Librarian<Log>::add_type ("table1", alist, syntax, &make);
    }
} LogTable_syntax;

