// daisy.h

#ifndef DAISY_H
#define DAISY_H

#include "time.h"
#include <vector.h>

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
public:
  const vector<Log*>& logs;
  Time time;
  Manager& manager;
  Weather& weather;
  Groundwater& groundwater;
  ColumnList& columns;

  // Simulation.
public:
  void run();

  // Create and Destroy.
public:
  static void load_syntax (Syntax&);
  Daisy (const AttributeList&);
  ~Daisy ();
};

#endif DAISY_H

