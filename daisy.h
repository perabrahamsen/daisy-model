// daisy.h

#ifndef DAISY_H
#define DAISY_H

#include "time.h"

class Manager;
class Weather;
class Groundwater;
class Log;
class Filter;
class ColumnList;
class Syntax;
class AttributeList;

class Daisy
{
  // Content.
  struct Implementation;
  Implementation& impl;
public:
  Log& log;
  Time time;
  Manager& manager;
  Weather& weather;
  Groundwater& groundwater;
  ColumnList& columns;

  // Simulation.
public:
  void run();
  void output (Log&, const Filter*) const;
  void output_field (Log&, const Filter*) const;

  // Create and Destroy.
public:
  static void load_syntax (Syntax&);
  Daisy (Log&, const AttributeList&);
  ~Daisy ();
};

#endif DAISY_H

