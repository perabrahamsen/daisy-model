// groundwater.h
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


#ifndef GROUNDWATER_H
#define GROUNDWATER_H

#include "librarian.h"

class Log;
class Time;
class Treelog;
class Geometry;
class Soil;
class SoilWater;
class SoilHeat;
class Output;

class Groundwater : public Model
{
  // Content.
public:
  const symbol name;
  static const char *const description;
  static const char *const component;

  // Lower boundary.
  enum bottom_t { pressure, lysimeter, forced_flux, free_drainage };
  virtual bottom_t bottom_type () const = 0;
  virtual double q_bottom () const = 0;
  virtual void accept_bottom (double amount, const Geometry&, size_t edge);

  // Groundwater.
  virtual bool is_pipe () const;
  virtual double pipe_height () const;
  virtual double Z_aquitard () const;
  virtual double K_aquitard () const;
  virtual void set_Z_aquitard (double);

  // Simulation.
public:
  virtual void tick (const Geometry& geo,
                     const Soil&, SoilWater&, double h_surface /* [cm] */,
		     const SoilHeat&, const Time&, Treelog&) = 0;
  virtual void output (Log&) const;

  // Accessors.
public:
  virtual double table () const = 0;

    // Create and Destroy.
public:
  static void load_syntax (Syntax&, AttributeList&);
  virtual void initialize (const Output&, 
                           const Geometry&, const Time& time, Treelog&) = 0;
  virtual bool check (Treelog&) const;
protected:
  Groundwater (Block& al);
public:
  ~Groundwater ();
};

static Librarian<Groundwater> Groundwater_init;

#endif // GROUNDWATER_H
