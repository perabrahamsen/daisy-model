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
public:
  struct Implementation;
  Implementation& impl;

  // Use.
public:
  virtual Filter& match (const Daisy&) = 0;

  // Normal items.
  virtual void open (string) = 0;
  virtual void close () = 0;

  // Derived objects.
  virtual void open (string field, string type) = 0;

  // Derived objects in a variable length list.
  virtual void open_entry (string type) = 0;
  virtual void close_entry () = 0;

  virtual void output (string, Filter&, const Time&,
		       bool log_only = false) = 0;
  virtual void output (string, Filter&, const bool,
		       bool log_only = false) = 0;
  virtual void output (string, Filter&, const double,
		       bool log_only = false) = 0;
  virtual void output (string, Filter&, const int,
		       bool log_only = false) = 0;
  virtual void output (string, Filter&, const string,
		       bool log_only = false) = 0;
  virtual void output (string, Filter&, const vector<double>&,
		       bool log_only = false) = 0;
  virtual void output (string, Filter&, const CSMP&,
		       bool log_only = false) = 0;

  void open_geometry (const Geometry&);
  void close_geometry ();
  const Geometry* geometry ();

  virtual bool printing () const = 0;

  // Used by CSMP.
public:
  virtual void output_point (double x, double y) = 0;

  // Create and Destroy.
protected:
  Log ();
public:
  virtual bool check (const Syntax&) const = 0;
  virtual ~Log ();
};

static Librarian<Log> Log_init ("log");

template <class T> void
output_submodule (const T& submodule,
                  const char* name, Log& log, Filter& filter)
{
  if (filter.check (name))
    {
      log.open (name);
      submodule.output (log, filter.lookup (name));
      log.close ();
    }
}
 
template <class T> void
output_derived (const T& submodule,
		const char* name,
		Log& log, Filter& filter)
{
  if (filter.check (name))
    {
      Filter& f1 = filter.lookup (name);
      if (f1.check (submodule.name))
	{
	  log.open (name, submodule.name);
	  submodule.output (log, f1.lookup (submodule.name));
	  log.close ();
	}
    }
}

template <class T> void
output_list (T const& items,
	     const char* name, Log& log, Filter& filter)
{
  if (filter.check (name))
    {
      Filter& f = filter.lookup (name);
      log.open (name);
      for (typename T::const_iterator item = items.begin(); 
	   item != items.end();
	   item++)
	{
	  if (f.check ((*item)->name))
	    {
	      log.open_entry ((*item)->name);
	      (*item)->output (log, f.lookup ((*item)->name));
	      log.close_entry ();
	    }
	}
      log.close ();
    }
}

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
	  log.open ("");
	  (*item)->output (log, f1);
	  log.close ();
	}
      log.close ();
    }
}

#endif LOG_H
