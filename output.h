// output.h -- Handle output from a Daisy simulation.
// 
// Copyright 2007 Per Abrahamsen and KVL.
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


#ifndef OUTPUT_H
#define OUTPUT_H

#include "condition.h"
#include "memutils.h"
#include "time.h"
#include <vector>

class Log;
class LogAll;
class Scope;
class MScope;
class Border;
class Block;
class Metalib;
class Daisy;

class Output
{
  // Content.
private:
  bool logging;
  const auto_vector<MScope*> exchanges;
  const auto_vector<Log*> logs;
  const std::unique_ptr<LogAll> log_all;
  std::vector<Log*> active_logs;
  const std::vector<const Scope*> my_scopes;
  const std::unique_ptr<Condition> activate_output;
  const std::vector<Time::component_t> time_columns;
  const symbol log_prefix;
  const symbol log_suffix;

  // Use.
public:
  void initial_logs (const Daisy&, const Time& previous, Treelog&);
  void tick (const Daisy&, const Time& time, double dt, Treelog&);
  void summarize (Treelog&) const;
  size_t scope_size () const;
  const Scope& scope (size_t) const;
  const std::vector<const Scope*>& scopes () const;

  // Create and Destroy.
public:
  bool check (const Border& field, Treelog& msg);
  void initialize (const Metalib&, Treelog&);
  void add_log (Log*);
private:
  static const std::vector<Log*> 
  /**/ find_active_logs (const std::vector<Log*>& logs, LogAll& log_all);
  static const std::vector<const Scope*> 
  /**/ find_extern_logs (const std::vector<Log*>& logs, 
                         const std::vector<MScope*>& exchanges);
public:
  static void load_syntax (Frame& frame);
  explicit Output (const BlockModel&);
  ~Output ();
  Output ();
private:
  Output (const Output&);
};

#endif // OUTPUT_H

