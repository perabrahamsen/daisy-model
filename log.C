// log.C

#include "log.h"
#include "condition.h"
#include "filter.h"
#include "csmp.h"
#include "daisy.h"
#include <list.h>
#include <fstream.h>
#include <strstream.h>

struct Log::Implementation
{
  ostream& err;
  struct Entry
  {
    string name;
    ostream* stream;
    const Condition* condition;
    const Filter* filter;
    list<int> offsets;
    bool opening;
    int column;
    void open (string);
    void close ();
    void print (string);
    void newline ();
    Entry (string n , const Condition* c, const Filter* f);
    Entry ();
    ~Entry ();
  };
  // BUG: Should not user pointer, but g++ generated bad code otherwise.
  typedef list<Entry*> EntryList;
  EntryList entries;
  Entry* current;
  void select (EntryList::iterator);
  void deselect ();
  Implementation (ostream& s);
  ~Implementation ();
};

void
Log::Implementation::select (EntryList::iterator i)
{
  current = *i;
}

void
Log::Implementation::deselect ()
{
  current = 0;
}

void 
Log::Implementation::Entry::open (string name)
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
Log::Implementation::Entry::close ()
{ 
  print (")");
  opening = false;
  column = offsets.back ();
  offsets.pop_back ();
}

void
Log::Implementation::Entry::print (string s)
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
Log::Implementation::Entry::newline ()
{
  *stream << "\n" << string (column / 8, '\t') << string (column % 8, ' ');
}

Log::Implementation::Implementation (ostream& s)
  : err (s),
    current (0)
{ }

Log::Implementation::~Implementation ()
{
  for (EntryList::iterator i = entries.begin ();
       i != entries.end ();
       i++)
    delete *i;
}

Log::Implementation::Entry::Entry (string n,
				   const Condition* c, const Filter* f)
  : name (n), stream (0), condition (c), filter (f),
    opening (true), column (0)
{ }

Log::Implementation::Entry::Entry ()
  : name (), stream (0), condition (0), filter (0),
    opening (true), column (0)
{ }

Log::Implementation::Entry::~Entry ()
{
  assert (offsets.size () == 0);
  if (stream)
    {
      *stream << "\n";
      if (stream->bad ())
	cerr << "There were problems writing to `" << name << "'\n";
      delete stream;
    }
  delete condition;
  delete filter;
}

void 
Log::tick (const Daisy& daisy)
{    
  for (Implementation::EntryList::iterator i = impl.entries.begin ();
       i != impl.entries.end ();
       i++)
    {
      if (daisy.match ((*i)->condition))
	{
	  impl.select (i);
	  daisy.output (*this, (*i)->filter);
	}
    }
  impl.deselect ();
}

void 
Log::open (string field, string type)
{
  impl.current->open (field);
  print (type);
  print (" ");
}

void 
Log::open (string name)
{
  impl.current->open (name);
}

void 
Log::close ()
{ 
  impl.current->close ();
}

void
Log::print (string s)
{
  impl.current->print (s);
}

void
Log::print (const char* s)
{
  print (string (s));
}

void
Log::print (double v)
{
  ostrstream scratch;
  scratch << v << '\0';
  const char* s = scratch.str ();
  print (s);
  delete s;
}

void
Log::print (int v)
{
  ostrstream scratch;
  scratch << v << '\0';
  const char* s = scratch.str ();
  print (s);
  delete s;
}

void
Log::print (bool v)
{
  ostrstream scratch;
  scratch << v << '\0';
  const char* s = scratch.str ();
  print (s);
  delete s;
}

void
Log::output (string name, const Filter* filter, const Time& value, bool log_only)
{
  if (filter->check (name, log_only))
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
Log::output (string name, const Filter* filter, const bool value, bool log_only)
{
  if (filter->check (name, log_only))
    {
      open (name);
      print (value);
      close ();
    }
}

void
Log::output (string name, const Filter* filter, const double value, bool log_only)
{
  if (filter->check (name, log_only))
    {
      open (name);
      print (value);
      close ();
    }
}

void
Log::output (string name, const Filter* filter, const int value, bool log_only)
{
  if (filter->check (name, log_only))
    {
      open (name);
      print (0.0 + value);
      close ();
    }
}

void
Log::output (string name, const Filter* filter, const vector<double>& value, bool log_only)
{
  if (filter->check (name, log_only))
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
Log::output (string name, const Filter* filter, const CSMP& csmp, bool log_only)
{
  if (filter->check (name, log_only))
    {
      open (name);
      csmp.output (*this);
      close ();
    }
}

void
Log::output_point (double x, double y)
{
  open ();
  print (x);
  print (" ");
  print (y);
  close ();
}

ostream& 
Log::err () const
{
  return impl.err;
}

void
Log::add (string n, const Condition* c, const Filter* f)
{
  impl.entries.push_back (new Implementation::Entry (n, c, f));
}

Log::Log (ostream& s)
  : impl (*new Implementation (s))
{ }

Log::~Log ()
{ delete &impl; }
