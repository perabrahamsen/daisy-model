// daisy.h

#ifndef DAISY_H
#define DAISY_H

#include "time.h"
#include <vector>

class Action;
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
  bool running;
  const vector<Log*>& logs;
  Time time;
  Action& action;
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

