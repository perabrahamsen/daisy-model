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
#include <strstream.h>

class LogFile : public Log, public Filter
{
  // Content.
private:
  string name;
  ostream* stream;
  list<int> offsets;
  bool opening;
  int column;
  struct Entry
  {
    bool active;
    const Condition* condition;
    const Filter& filter;
    Entry (const AttributeList& al)
      : active (false),
	condition (&Condition::create (al.list ("when"))), 
	filter (al.filter ("what"))
    { }
    ~Entry ()
    { delete condition; }
  };
  typedef list<Entry*> EntryList;
  EntryList entries;
  void print (const char*);
  void print (string);
  void print (double);
  void print (int);
  void print (bool);
  void newline ();

  // Log.
public:
  const Filter& match (const Daisy&);

  void open (string = "");
  void open (string field, string type);
  void close ();
  void output (string, const Filter&, const Time&, bool log_only = false);
  void output (string, const Filter&, const bool, bool log_only = false);
  void output (string, const Filter&, const double, bool log_only = false);
  void output (string, const Filter&, const int, bool log_only = false);
  void output (string, const Filter&, const string, bool log_only = false);
  void output (string, const Filter&, const vector<double>&, bool log_only = false);
  void output (string, const Filter&, const CSMP&, bool log_only = false);
  void output_point (double x, double y);

  // Filter.
  bool check (string, bool log_only = false) const;
  const Filter& lookup (string) const;

  // Create and Destroy.
private:
  friend class LogFileSyntax;
  static Log& make (const AttributeList&);
  LogFile (const AttributeList&);
public:
  ~LogFile ();
};

void 
LogFile::open (string name)
{
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
LogFile::close ()
{ 
  print (")");
  opening = false;
  column = offsets.back ();
  offsets.pop_back ();
}

void
LogFile::print (string s)
{
  if (!stream)
    {
      stream = new ofstream (name.c_str ());
      if (!*stream)
	cerr << "Failed to open `" << name << "'\n";
    }
  *stream << s;
  column += s.length ();
}

void
LogFile::newline ()
{
  *stream << "\n" << string (column / 8, '\t') << string (column % 8, ' ');
}

const Filter&
LogFile::match (const Daisy& daisy)
{
  bool found = false;
  for (EntryList::const_iterator i = entries.begin ();
       i != entries.end ();
       i++)
    {
      if ((*i)->condition->match (daisy))
	{
	  found = true;
	  (*i)->active = true;
	}
      else
	(*i)->active = false;
    }
  if (found)
    return *this;
  return *Filter::none;
}

void 
LogFile::open (string field, string type)
{
  open (field);
  print (type);
  print (" ");
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
  delete s;
}

void
LogFile::print (int v)
{
  ostrstream scratch;
  scratch << v << '\0';
  const char* s = scratch.str ();
  print (s);
  delete s;
}

void
LogFile::print (bool v)
{
  ostrstream scratch;
  scratch << v << '\0';
  const char* s = scratch.str ();
  print (s);
  delete s;
}

void
LogFile::output (string name, const Filter& filter, const Time& value, bool log_only)
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
LogFile::output (string name, const Filter& filter, const bool value, bool log_only)
{
  if (filter.check (name, log_only))
    {
      open (name);
      print (value);
      close ();
    }
}

void
LogFile::output (string name, const Filter& filter, const double value, bool log_only)
{
  if (filter.check (name, log_only))
    {
      open (name);
      print (value);
      close ();
    }
}

void
LogFile::output (string name, const Filter& filter, const int value, bool log_only)
{
  if (filter.check (name, log_only))
    {
      open (name);
      print (0.0 + value);
      close ();
    }
}

void
LogFile::output (string name, const Filter& filter, const string value, bool log_only)
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
LogFile::output (string name, const Filter& filter, const vector<double>& value, bool log_only)
{
  if (filter.check (name, log_only))
    {
      open (name);
      bool first = true;
      for (const double* p = value.begin (); p != value.end (); p++)
	{
	  if (first)
	    first = false;
	  else
	    print (" ");
	  print (*p);
	}
      close ();
    }
}

void
LogFile::output (string name, const Filter& filter, const CSMP& csmp, bool log_only)
{
  if (filter.check (name, log_only))
    {
      open (name);
      csmp.output (*this);
      close ();
    }
}

void
LogFile::output_point (double x, double y)
{
  open ();
  print (x);
  print (" ");
  print (y);
  close ();
}

bool
LogFile::check (string s, bool log_only) const
{
  for (EntryList::const_iterator i = entries.begin ();
       i != entries.end ();
       i++)
    {
      if ((*i)->active && (*i)->filter.check (s, log_only))
	return true;
    }
  return false;
}
 
const Filter& 
LogFile::lookup (string s) const
{
  for (EntryList::const_iterator i = entries.begin ();
       i != entries.end ();
       i++)
    {
      if ((*i)->active && (*i)->filter.check (s))
	return (*i)->filter.lookup (s);
    }
  assert (false);
  return *Filter::none;	// Shut up!
}

LogFile::LogFile (const AttributeList& av)
  : name (av.name ("where")),
    stream (0),
    opening (true),
    column (0)
{ 
  const vector<const AttributeList*>& ml = av.list_sequence ("matches");
  
  for (vector<const AttributeList*>::const_iterator i = ml.begin ();
       i != ml.end ();
       i++)
    {
      entries.push_back (new Entry (**i));
    }
}					    

LogFile::~LogFile ()
{
  sequence_delete (entries.begin (), entries.end ());
  assert (offsets.size () == 0);
  if (stream)
    {
      *stream << "\n";
      if (stream->bad ())
	cerr << "There were problems writing to `" << name << "'\n";
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
  Syntax& match = *new Syntax ();
  match.add ("when", Condition::library (), Syntax::Const);
  assert (Log::global_syntax_table != NULL);
  match.add_filter ("what", *Log::global_syntax_table, Syntax::Const);
  match.order ("when", "what");
  syntax.add ("where", Syntax::String, Syntax::Const);
  syntax.add ("matches", match, Syntax::Const, Syntax::Sequence);
  syntax.order ("where", "matches");
  Log::add_type ("file", alist, syntax, &LogFile::make);
}
