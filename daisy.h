// daisy.h

#ifndef DAISY_H
#define DAISY_H

#include "time.h"
#include <vector>

class Action;
class Harvest;
class Weather;
class Log;
class Field;
class Syntax;
class AttributeList;
class Condition;

class Daisy
{
private:
  static const char *const default_description;

  // Initial content.
public:
  const Syntax* syntax;
  const AttributeList& alist;

  // Content.
public:
  bool running;
  const vector<Log*>& logs;
  Condition& activate_output;
  Time time;
  Action& action;
  Weather* weather;
  Field& field;
  vector<const Harvest*>& harvest;

  // Simulation.
public:
  void tick_columns ();
  void tick_logs ();
  void tick ();
  void run ();
  bool check ();

  // Create and Destroy.
public:
  void initialize (const Syntax&);
  static void load_syntax (Syntax&, AttributeList&);
  Daisy (const AttributeList&);
  ~Daisy ();
};

#endif DAISY_H

