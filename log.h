// log.h

#ifndef LOG_H
#define LOG_H

#include <std/string.h>
#include <vector.h>

class Filter;
class Condition;
class Daisy;
class Column;
class Crop;
class Time;
class CSMP;

class Log
{
  // Use.
public:
  void tick (const Daisy&);

  void open (string = "");
  void open (string field, string type);
  void close ();
  void output (string, const Filter*, const Time&, bool log_only = false);
  void output (string, const Filter*, const bool, bool log_only = false);
  void output (string, const Filter*, const double, bool log_only = false);
  void output (string, const Filter*, const int, bool log_only = false);
  void output (string, const Filter*, const vector<double>&, bool log_only = false);
  void output (string, const Filter*, const CSMP&, bool log_only = false);
  ostream& err () const;

  // Used by CSMP.
public:
  void output_point (double x, double y);

private:
  void print (const char*);
  void print (string);
  void print (double);
  void print (int);
  void print (bool);

    // Content.
private:
  struct Implementation;
  Implementation& impl;

  // Create and Destroy.
private:
  friend class Parser; // Only create from Input.
  void add (string, const Condition*, const Filter*);
public:
  Log (ostream&);
  ~Log ();
};

template <class T> void
output_submodule (const T& submodule, const char* name, Log& log, const Filter* filter)
{
  if (filter->check (name))
    {
      log.open (name);
      submodule.output (log, filter->lookup (name));
      log.close ();
    }
}

#endif LOG_H
