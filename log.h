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
  virtual const Filter& match (const Daisy&) = 0;

  virtual void open (string) = 0;
  virtual void open (string field, string type) = 0;
  virtual void close () = 0;
  virtual void output (string, const Filter&, const Time&,
		       bool log_only = false) = 0;
  virtual void output (string, const Filter&, const bool,
		       bool log_only = false) = 0;
  virtual void output (string, const Filter&, const double,
		       bool log_only = false) = 0;
  virtual void output (string, const Filter&, const int,
		       bool log_only = false) = 0;
  virtual void output (string, const Filter&, const string,
		       bool log_only = false) = 0;
  virtual void output (string, const Filter&, const vector<double>&,
		       bool log_only = false) = 0;
  virtual void output (string, const Filter&, const CSMP&,
		       bool log_only = false) = 0;

  void open_geometry (const Geometry&);
  void close_geometry ();
  const Geometry* geometry ();

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
                  const char* name, Log& log, const Filter& filter)
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
		Log& log, const Filter& filter)
{
  if (filter.check (name))
    {
      const Filter& f1 = filter.lookup (name);
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
	     const char* name, Log& log, Filter const& filter)
{
  if (filter.check (name))
    {
      const Filter& f = filter.lookup (name);
      log.open (name);
      for (typename T::const_iterator item = items.begin(); 
	   item != items.end();
	   item++)
	{
	  if (f.check ((*item)->name))
	    {
	      log.open ((*item)->name);
	      (*item)->output (log, f.lookup ((*item)->name));
	      log.close ();
	    }
	}
      log.close ();
    }
}

template <class T> void
output_vector (T const& items,
	       const char* name, Log& log, Filter const& filter)
{
  if (filter.check (name))
    {
      Filter const& f1 = filter.lookup (name);
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
