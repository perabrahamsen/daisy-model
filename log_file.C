// log_file.C

#include "log.h"
#include "condition.h"
#include "filter.h"
#include "csmp.h"
#include "time.h"
#include "alist.h"
#include "syntax.h"
#include "common.h"
#include <list>
#include <fstream.h>

class LogFile : public Log
{
  // Content.
private:
  string name;
  ostream* stream;
  list<int> offsets;
  bool opening;
  int column;
  const Condition& condition;
  Filter& filter;
  const bool compact;
  const bool accumulating;
  bool matching;
  
  // Printing.
  void force_open (const string& = "");
  void force_close ();
  void print (const char*);
  void print (const string&);
  void print (double);
  void print (int);
  void print (bool);
  void newline ();

  bool printing () const;

  // Log.
public:
  Filter& match (const Daisy&);

  // Open normal items.
  void open (const string&);
  void close ();

  // Open unnamed alists.
  void open_unnamed ();
  void close_unnamed ();

  // Open derived items.
  void open_derived (const string& field, const string& type);
  void close_derived ();

  // Open derived items in list.
  void open_entry (const string& type, const AttributeList&);
  void close_entry ();

  void output (const string&, Filter&, const Time&, bool log_only = false);
  void output (const string&, Filter&, const bool, bool log_only = false);
  void output (const string&, Filter&, const double, bool log_only = false);
  void output (const string&, Filter&, const int, bool log_only = false);
  void output (const string&, Filter&, const string&, bool log_only = false);
  void output (const string&, Filter&, const vector<double>&, bool log_only = false);
  void output (const string&, Filter&, const CSMP&, bool log_only = false);

  // Create and Destroy.
private:
  LogFile (const AttributeList&);
public:
  static Log& make (const AttributeList&);
  bool check (const Syntax& syntax) const;
  ~LogFile ();
};

void 
LogFile::force_open (const string& name)
{
  if (!matching)
    return;
  if (compact)
    {
      column = offsets.back ();
      newline ();
      opening = true;
      if (name == "")
	print (string ("("));
      else 
	print (string ("(") + name + " ");
      offsets.push_back (column);
      return;
    }
  if (!opening)
    {
      newline ();
      opening = true;
    }
  offsets.push_back (column);
  if (name == "")
    print (string ("("));
  else 
    print (string ("(") + name + " ");
}

void 
LogFile::force_close ()
{ 
  if (!matching)
    return;

  print (")");
  opening = false;
  column = offsets.back ();
  offsets.pop_back ();
}

void 
LogFile::open (const string& name)
{
  if (!compact)
    force_open (name);
}

void 
LogFile::close ()
{ 
  if (!compact)
    force_close ();
}

void 
LogFile::open_unnamed ()
{
  if (!compact)
    force_open ("");
}

void 
LogFile::close_unnamed ()
{ 
  if (!compact)
    force_close ();
}

void
LogFile::print (const string& s)
{
  if (!matching)
    return;
  if (!stream)
    {
#ifdef BORLAND_PERMISSIONS
      stream = new ofstream (name.c_str (), ios::out|ios::trunc, 0666);
#else
      stream = new ofstream (name.c_str ());
#endif
      if (!*stream)
	CERR << "Failed to open `" << name << "'\n";
    }
  if (compact && !opening)
    {
      column = offsets.back ();
      newline ();
      opening = true;
    }
  *stream << s;
  column += s.length ();
}

void
LogFile::newline ()
{
  if (!matching)
    return;

  *stream << "\n" << string (column / 8, '\t') << string (column % 8, ' ');
}

Filter&
LogFile::match (const Daisy& daisy)
{
  matching = condition.match (daisy);
  
  if (matching || accumulating)
    return filter;

  static Filter* none = NULL;
  if (!none)
    {
      AttributeList none_alist;
      none_alist.add ("type", "none");
      none = &Librarian<Filter>::create (none_alist);
    }
  return *none;
}

void 
LogFile::open_derived (const string& field, const string& type)
{
  if (!compact)
    {
      open (field);
      print (type);
      print (" ");
    }
}

void
LogFile::close_derived ()
{
  close ();
}

void 
LogFile::open_entry (const string& type, const AttributeList&)
{
  force_open (type);
}

void 
LogFile::close_entry ()
{
  force_close ();
}
      
void
LogFile::print (const char* s)
{
  print (string (s));
}

void
LogFile::print (double v)
{
  ostrstream scratch;
  scratch << v << '\0';
  const char* s = scratch.str ();
  print (s);
  delete [] s;
}

void
LogFile::print (int v)
{
  ostrstream scratch;
  scratch << v << '\0';
  const char* s = scratch.str ();
  print (s);
  delete [] s;
}

void
LogFile::print (bool v)
{
  ostrstream scratch;
  scratch << v << '\0';
  const char* s = scratch.str ();
  print (s);
  delete [] s;
}

void
LogFile::output (const string& name, Filter& filter, const Time& value, bool log_only)
{
  if (filter.check (name, log_only))
    {
      open (name);
      print (value.year ());
      print (" ");
      print (value.month ());
      print (" ");
      print (value.mday ());
      print (" ");
      print (value.hour ());
      close ();
    }
}

void
LogFile::output (const string& name, Filter& filter, const bool value, bool log_only)
{
  if (filter.check (name, log_only))
    {
      open (name);
      print (value);
      close ();
    }
}

void
LogFile::output (const string& name, Filter& filter, const double value, bool log_only)
{
  if (filter.check (name, log_only))
    {
      open (name);
      print (value);
      close ();
    }
}

void
LogFile::output (const string& name, Filter& filter, const int value, bool log_only)
{
  if (filter.check (name, log_only))
    {
      open (name);
      print (0.0 + value);
      close ();
    }
}

void
LogFile::output (const string& name, Filter& filter, const string& value, bool log_only)
{
  if (filter.check (name, log_only))
    {
      open (name);
      print ("\"");
      print (value);		// BUG: We should escape " and \ here.
      print ("\"");
      close ();
    }
}

void
LogFile::output (const string& name, Filter& filter, const vector<double>& value, bool log_only)
{
  if (filter.check (name, log_only))
    {
      Filter& f = filter.lookup (name);
      const Geometry* g = geometry ();

      if (printing ())
	{
	  vector<double> val;
	  
	  if (g != NULL)
	    val = f.select (*g, value);
	  else
	    val = value;
      
	  open (name);
	  bool first = true;
	  for (const double* p = val.begin (); p != val.end (); p++)
	    {
	      if (first)
		first = false;
	      else
		print (" ");
	      print (*p);
	    }
	  close ();
	}
      else
	f.accumulate (*g, value);
    }
}

void
LogFile::output (const string& name, Filter& filter, const CSMP& csmp, bool log_only)
{
  if (filter.check (name, log_only))
    {
      open (name);
      for (unsigned int i = 0; i < csmp.size (); i++)
	{
	  open_unnamed ();
	  print (csmp.x (i));
	  print (" ");
	  print (csmp.y (i));
	  close_unnamed ();
	} 
      close ();
    }
}

bool
LogFile::printing () const
{ return matching; }
  
bool
LogFile::check (const Syntax& syntax) const
{
  bool ok = filter.check (syntax, Syntax::Singleton);
  
  if (!ok)
    CERR << "in log file `" << name << "'\n";
  
  return ok;
}

LogFile::LogFile (const AttributeList& al)
  : name (al.name ("where")),
    stream (0),
    opening (true),
    column (0),
    condition (Librarian<Condition>::create (al.alist ("when"))), 
    filter (Librarian<Filter>::create (al.alist ("what"))),
    compact (al.flag ("compact")),
    accumulating (filter.accumulating ()),
    matching (false)
{ 
  offsets.push_back (0);
}

LogFile::~LogFile ()
{
#ifdef CONST_DELETE
  delete &condition;
  delete &filter;
#endif
  assert (offsets.size () == 1);
  if (stream)
    {
      *stream << "\n";
      if (stream->bad ())
	CERR << "There were problems writing to `" << name << "'\n";
      delete stream;
    }
}

// Add the LogFile syntax to the syntax table.
Log&
LogFile::make (const AttributeList& al)
{
  return *new LogFile (al);
}

static struct LogFileSyntax
{
  LogFileSyntax ();
} LogFile_syntax;

LogFileSyntax::LogFileSyntax ()
{ 
  Syntax& syntax = *new Syntax ();
  AttributeList& alist = *new AttributeList ();
  alist.add ("description", "Create a log file with lots of parentheses.");
  syntax.add ("where", Syntax::String, Syntax::Const,
	      "Name of log file to generate.");
  syntax.add ("when", Librarian<Condition>::library (), "\
Generate an entry in the log file when this condition is true.");
  syntax.add ("what", Librarian<Filter>::library (), "\
Specify what subset of the state should be logged.");
  syntax.order ("where", "when", "what");
  syntax.add ("compact", Syntax::Boolean, Syntax::Const, "\
generate a more compact log file, by leaving out the attribute names.");
  alist.add ("compact", false);
  Librarian<Log>::add_type ("file", alist, syntax, &LogFile::make);
}
