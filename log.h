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
  void close ();
  void output (string, const Filter*, const Time&);
  void output (string, const Filter*, const bool&);
  void output (string, const Filter*, const double&);
  void output (string, const Filter*, const vector<double>&);
  void output (string, const Filter*, const CSMP&);
  void output_point (double x, double y);
  ostream& err ();

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

#endif LOG_H
