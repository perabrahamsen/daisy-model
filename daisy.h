// daisy.h
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
//
// This file is part of Daisy.
// 
// Daisy is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser Public License as published by
// the Free Software Foundation; either version 2.1 of the License, or
// (at your option) any later version.
// 
// Daisy is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser Public License for more details.
// 
// You should have received a copy of the GNU Lesser Public License
// along with Daisy; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA


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
class Treelog;
class LogAll;

class Daisy
{
  // Initial content.
public:
  static const char *const default_description;
  const Syntax* syntax;
  const AttributeList& alist;

  // Content.
public:
  bool running;
  bool logging;
  const std::vector<Log*> logs;
  LogAll& log_all;
  static const std::vector<Log*> 
  /**/ find_active_logs (const std::vector<Log*>& logs, LogAll& log_all);
  const std::vector<Log*> active_logs;
  Condition& activate_output;
  Condition& print_time;
  Time time;
  Action& action;
  Weather* weather;
  Field& field;
  std::vector<const Harvest*> harvest;

  // Simulation.
public:
  void tick_columns (Treelog&);
  void initial_logs (Treelog&);
  void tick_logs (Treelog&);
  void tick (Treelog&);
  void run (Treelog&);
  bool check (Treelog& err);

  // Create and Destroy.
public:
  void initialize (const Syntax&, Treelog& err);
  static void load_syntax (Syntax&, AttributeList&);
  Daisy (const AttributeList&);
  ~Daisy ();
};

#endif // DAISY_H

