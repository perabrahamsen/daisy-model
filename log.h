// log.h

#ifndef LOG_H
#define LOG_H

#include "filter.h"
#include "librarian.h"

class Daisy;
class Time;
class CSMP;
class Geometry;

class Log
{
  // Content.
private:
  struct Implementation;
  Implementation& impl;

  // Use.  
public:
  // Called at the start of each time step.
  virtual Filter& match (const Daisy&) = 0;
  // Called at the end of each time step.
  virtual void done ();

  // Normal items.
  virtual void open (const string&) = 0;
  virtual void close () = 0;

  // Lists.
  virtual void open_unnamed () = 0;
  virtual void close_unnamed () = 0;

  // Derived objects.
  virtual void open_derived (const string& field, const string& type) = 0;
  virtual void close_derived () = 0;

  // Derived objects in a variable length list.
  virtual void open_entry (const string& type) = 0;
  virtual void close_entry () = 0;

  virtual void output (const string&, Filter&, const Time&,
		       bool log_only = false) = 0;
  virtual void output (const string&, Filter&, const bool,
		       bool log_only = false) = 0;
  virtual void output (const string&, Filter&, const double,
		       bool log_only = false) = 0;
  virtual void output (const string&, Filter&, const int,
		       bool log_only = false) = 0;
  virtual void output (const string&, Filter&, const string&,
		       bool log_only = false) = 0;
  virtual void output (const string&, Filter&, const vector<double>&,
		       bool log_only = false) = 0;
  virtual void output (const string&, Filter&, const CSMP&,
		       bool log_only = false) = 0;

  void open_geometry (const Geometry&);
  void close_geometry ();
  const Geometry* geometry ();

  // Create and Destroy.
protected:
  Log ();
public:
  virtual bool check (const Syntax&) const = 0;
  virtual ~Log ();
};

static Librarian<Log> Log_init ("log");

// Output an alist.

template <class T> void
output_submodule (const T& submodule,
                  const char* name, Log& log, Filter& filter, 
		  bool log_only = false)
{
  if (filter.check (name, log_only))
    {
      log.open (name);
      submodule.output (log, filter.lookup (name));
      log.close ();
    }
}

// Output an object.
 
template <class T> void
output_derived (const T& submodule,
		const char* name,
		Log& log, Filter& filter)
{
  if (filter.check (name))
    {
      Filter& f1 = filter.lookup (name);
      const Library& library = Librarian<T>::library ();

      if (f1.check_derived (submodule.name, library))
	{
	  log.open_derived (name, submodule.name);
	  submodule.output (log, f1.lookup_derived (submodule.name, library));
	  log.close_derived ();
	}
    }
}

// Output a list of objects.

template <class T> void
output_list (T const& items,
	     const char* name, Log& log, Filter& filter, 
	     const Library& library)
{
  if (filter.check (name))
    {
      Filter& f = filter.lookup (name);
      log.open (name);
      for (typename T::const_iterator item = items.begin(); 
	   item != items.end();
	   item++)
	{
	  if (f.check_derived ((*item)->name, library))
	    {
	      log.open_entry ((*item)->name);
	      (*item)->output (log, f.lookup_derived ((*item)->name, library));
	      log.close_entry ();
	    }
	}
      log.close ();
    }
}

// Output a list of unnamed alists.

template <class T> void
output_vector (T const& items,
	       const char* name, Log& log, Filter& filter)
{
  if (filter.check (name))
    {
      Filter& f1 = filter.lookup (name);
      log.open (name);
      for (typename T::const_iterator item = items.begin();
	   item != items.end();
	   item++)
	{
	  log.open_unnamed ();
	  (*item)->output (log, f1);
	  log.close_unnamed ();
	}
      log.close ();
    }
}

#endif LOG_H
