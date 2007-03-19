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
#include <vector>

struct Log;
struct LogAll;
struct Scope;
struct Border;
struct Block;
struct Daisy;

class Output
{
  // Content.
private:
  bool logging;
  const auto_vector<Scope*> exchanges;
  const auto_vector<Log*> logs;
  const std::auto_ptr<LogAll> log_all;
  const std::vector<Log*> active_logs;
  const std::vector<Scope*> scopes;
  const std::auto_ptr<Condition> activate_output;

  // Use.
public:
  void initial_logs (const Daisy&, Treelog&);
  void tick (const Daisy&, Treelog&);
  void summarize (Treelog&) const;
  size_t scope_size () const;
  Scope& scope (size_t) const;

  // Create and Destroy.
public:
  bool check (const Border& field, Treelog& msg);
  void initialize (Treelog&);
private:
  static const std::vector<Log*> 
  /**/ find_active_logs (const std::vector<Log*>& logs, LogAll& log_all);
  static const std::vector<Scope*> 
  /**/ find_extern_logs (const std::vector<Log*>& logs, 
                         const std::vector<Scope*>& exchanges);
public:
  static void load_syntax (Syntax&, AttributeList&);
  explicit Output (Block&);
  ~Output ();
  Output ();
private:
  Output (const Output&);
};

#endif // OUTPUT_H

