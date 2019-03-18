// drain.h --- Lateral transport of water.
// 
// Copyright 2008, 2010 Per Abrahamsen and KU.
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
//
// URL: <http://code.google.com/p/daisy-model/wiki/ComponentDrain>

#ifndef DRAIN_H
#define DRAIN_H

#include "model_derived.h"
#include <map>

class BlockModel;
class Geometry;
class Soil;
class SoilWater;
class SoilHeat;
class Surface;
class Treelog;
class Log;
class Time;
class Scope;

class Drain : public ModelDerived
{
  // Identity.
public:
  static const char *const component;
  symbol library_id () const;

  // Simulation.
public:
  virtual void tick (const Time&, const Scope&, 
                     const Geometry&, const Soil&, const SoilHeat&,
                     const Surface&, SoilWater&, Treelog&) = 0;

public:
  virtual void output (Log&) const = 0;

  // Create and Destroy.
public:
  virtual void initialize (const Time&, const Scope&, 
                           const Geometry&, Treelog&) = 0;
  virtual bool check (const Scope&, Treelog&) const = 0;
protected:
  explicit Drain (const BlockModel& al);
public:
  ~Drain ();
};

#endif // DRAIN_H
