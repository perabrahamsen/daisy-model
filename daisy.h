// daisy.h

#ifndef DAISY_H
#define DAISY_H

#include "common.h"
#include "time.h"
#include <vector>

class Action;
class Harvest;
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
  vector<const Harvest*>& harvest;

#ifdef HANDLE_EXCEPTIONS
  // Exceptions.
public:
  struct Initialization : public runtime_error
  {
    Initialization (const char* n) : runtime_error (n) { }
  };
#endif

  // Simulation.
public:
  void run();
  bool check (const Syntax&);

  // Create and Destroy.
public:
  static void load_syntax (Syntax&, AttributeList&);
  Daisy (const AttributeList&);
  ~Daisy ();
};

#endif DAISY_H

