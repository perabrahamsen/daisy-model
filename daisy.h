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

#include "program.h"
#include "time.h"
#include "timestep.h"
#include "memutils.h"
#include <boost/scoped_ptr.hpp>
#include <vector>
#include <memory>

class Action;
class Harvest;
class Weather;
class Log;
class Field;
class Treelog;
class Output;
class Condition;
class Timestep;
class Metalib;
class Scopesel;
class Scope;
class Frame;
class FrameModel;

class Daisy : public Program
{
public:
  static const char *const default_description;

  // Initial content.
private:
  class Implementation;
  const boost::scoped_ptr<Implementation> impl;
public:
  const FrameModel& frame () const;
  const Scope* find_scope (const Scopesel&, Treelog&) const;
  Scope& find_scope (size_t index) const;
  size_t scope_size () const;
  const Time& time () const;
  const Timestep& timestep () const;
public:
  const Metalib& metalib;
  const Units& units () const;

  // Content.
  bool running;
public:
  std::auto_ptr<Field> field;
  auto_vector<const Harvest*> harvest;

  // UI.
public:
  void attach_ui (Run* run, const std::vector<Log*>& logs);

  // Simulation.
public:
  bool run (Treelog&);
  void tick (Treelog&);
  void tick_before (Treelog&);
  void tick_columns (Treelog&);
  void tick_column (size_t, Treelog&);
  void tick_after (Treelog&);
  void output (Log&) const;

  // Create and Destroy.
public:
  void initialize (Metalib&, Block&);
  bool check (Treelog& err);
  static void load_syntax (Frame&);
  explicit Daisy (const BlockModel&);
private:
  void summarize (Treelog&) const;
  ~Daisy ();
};

#endif // DAISY_H

