// log.h

#ifndef LOG_H
#define LOG_H

#include "librarian.h"

class Daisy;
class Time;
class PLF;
class Geometry;

class Log
{
  // Content.
private:
  struct Implementation;
  Implementation& impl;
public:
  static const char *const description;

  // Filter
public:
  virtual bool check (const string&) const = 0;
  virtual bool check_entry (const string&, const Library& library) const;
  virtual bool check_derived (const string& field, const string& name, 
			      const Library& library) const = 0;

  // Use.  
public:
  // Called at the start of each time step.
  virtual bool match (const Daisy&) = 0;
  // Called at the end of each time step.
  virtual void done ();

  // Conditionals.
public:
  struct Maybe
  {
  private:
    Log& ll;
  public:
    Maybe (Log& l, const string& value)
      : ll (l)
    { ll.open_maybe (value); }
    ~Maybe ()
    { ll.close_maybe (); }
  };
private:
  virtual void open_maybe (const string& value);
  virtual void close_maybe ();
  friend struct Log::Maybe;

  // Normal items.
public:
  virtual void open (const string&) = 0;
  virtual void close () = 0;

  // Lists.
  virtual void open_unnamed () = 0;
  virtual void close_unnamed () = 0;

  // AList singletons variant.
  virtual void open_alist (const string& name, const AttributeList& alist);
  virtual void close_alist ();

  // Derived objects.
  virtual void open_derived (const string& field, const string& type) = 0;
  virtual void close_derived () = 0;

  // Derived objects in a variable length list.
  virtual void open_entry (const string& type, const AttributeList&) = 0;
  virtual void close_entry () = 0;

  virtual void output (const string&, const Time&) = 0;
  virtual void output (const string&, const bool) = 0;
  virtual void output (const string&, const double) = 0;
  virtual void output (const string&, const int) = 0;
  virtual void output (const string&, const string&) = 0;
  virtual void output (const string&, const vector<double>&) = 0;
  virtual void output (const string&, const PLF&) = 0;

  void open_geometry (const Geometry&);
  void close_geometry ();
  const Geometry* geometry ();

  // Create and Destroy.
public:
  virtual bool check (const Syntax&) const = 0;
protected:
  Log ();
public:
  virtual ~Log ();
};

static Librarian<Log> Log_init ("log");

// Output an alist.

template <class T> void
output_submodule (const T& submodule, 
		  const char* name, Log& log)
{
  if (log.check (name))
    {
      log.open (name);
      submodule.output (log);
      log.close ();
    }
}

// Output an object.
 
template <class T> void
output_derived (const T& submodule, const char* name, Log& log)
{
  const Library& library = Librarian<T>::library ();

  if (log.check_derived (name, submodule.name, library))
    {
      log.open_derived (name, submodule.name);
      submodule.output (log);
      log.close_derived ();
    }
}

// Output a list of objects.

template <class T> void
output_list (T const& items, const char* name, Log& log, 
	     const Library& library)
{
  if (log.check (name))
    {
      log.open (name);
      for (typename T::const_iterator item = items.begin(); 
	   item != items.end();
	   item++)
	{
	  if (log.check_entry ((*item)->name, library))
	    {
	      log.open_entry ((*item)->name, (*item)->alist);
	      (*item)->output (log);
	      log.close_entry ();
	    }
	}
      log.close ();
    }
}

// Output a list of unnamed alists.

template <class T> void
output_vector (T const& items, const char* name, Log& log)
{
  if (log.check (name))
    {
      log.open (name);
      for (typename T::const_iterator item = items.begin ();
	   item != items.end ();
	   item++)
	{
	  log.open_unnamed ();
	  (*item)->output (log);
	  log.close_unnamed ();
	}
      log.close ();
    }
}

#endif LOG_H
