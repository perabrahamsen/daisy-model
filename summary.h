// summary.h
// 
// Copyright 2003 Per Abrahamsen and KVL.
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


#ifndef SUMMARY_H
#define SUMMARY_H

#include "model.h"
#include "symbol.h"
#include <vector>

class Select;
class Treelog;
class BlockModel;
class Time;
class Scope;

class Summary : public Model
{
  // Content.
public:
  static const char *const component;
  const symbol objid;
  symbol library_id () const;
  virtual void find_scopes (std::vector<const Scope*>&) const;

  // Simulation.
  virtual void tick (const Time&);

  // Create and Destroy.
public:
  virtual void clear () = 0;
  virtual void initialize (std::vector<Select*>&, Treelog&) = 0;
  virtual bool check (Treelog&) const;
protected:
  Summary (const BlockModel& al);
public:
  virtual void summarize (Treelog&) const = 0;
  ~Summary ();
};

#endif // SUMMARY_H
