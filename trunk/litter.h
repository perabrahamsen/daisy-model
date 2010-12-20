// litter.h -- Litter lay below permanent vegetation.
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


#ifndef LITTER_H
#define LITTER_H

#include "model.h"
#include "symbol.h"

class BlockModel;

class Litter : public Model
{
  // Content.
public:
  static const char *const component;
  symbol library_id () const;

  // Simulation.
public:
  virtual void update (double top_DM /* [kg DM/m^2] */) = 0;
  virtual double cover () const = 0; // Fraction of surface covered [0-1]
  virtual double vapor_flux_factor () const = 0; // Affect on soil evap. []
  virtual double water_capacity () const = 0;    // Max water content [mm]
  virtual double albedo () const = 0;  // Light reflection factor []

  // Create and Destroy.
public:
  virtual void initialize (double top_DM /* [kg DM/m^2] */) = 0;
protected:
  Litter ();
public:
  ~Litter ();
};

#endif // LITTER_H
