// log.h

#ifndef LOG_H
#define LOG_H

#include <string>
#include <vector>

class Filter;
class Daisy;
class Time;
class CSMP;
class AttributeList;
class Library;
class Syntax;

class Log
{
  // Content.
public:
  static Syntax* global_syntax_table;

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

  // Used by CSMP.
public:
  virtual void output_point (double x, double y) = 0;

  // Library.
public:
  static const Library& library ();
  static Log& create (const AttributeList&);
  typedef Log& (*constructor) (const AttributeList&);
  static void add_type (const string, const AttributeList&, const Syntax&,
			constructor);
  static void derive_type (const string, const AttributeList&, string super);
 
  // Create and Destroy.
protected:
  Log ();
public:
  virtual ~Log ();
};

// Ensure the Log library is initialized.
// See TC++PL, 2ed, 10.5.1, for an explanation.
static class Log_init
{
  static int count;
public:
  Log_init ();
  ~Log_init ();
} Log_init;

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
output_list (T const& items,
	     const char* name, Log& log, Filter const& filter)
{
  if (filter.check (name))
    {
      const Filter& f = filter.lookup ("name");
      log.open (name);
      for (T::const_iterator item = items.begin(); 
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
      log.open (name);
      for (T::const_iterator item = items.begin();
	   item != items.end();
	   item++)
	{
	  log.open ("");
	  (*item)->output (log, filter.lookup (name));
	  log.close ();
	}
      log.close ();
    }
}

#endif LOG_H
