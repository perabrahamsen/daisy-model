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
  friend class Input; // Only create from Input.
  void add (string, const Condition*, const Filter*);
  Log ();
public:
  ~Log ();
};

#endif LOG_H
