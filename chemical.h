// chemical.h
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
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


#ifndef CHEMICAL_H
#define CHEMICAL_H

#include "model.h"
#include "solute.h"
#include "alist.h"

class SoilHeat;
class OrganicMatter;

class Chemical : public Model, public Solute
{
  // Content.
public:
  const symbol name;
  const AttributeList alist;
  static const char *const component;

  // Management.
public:
  virtual void spray (double amount, double dt) = 0;
  virtual void harvest (double removed, double surface, double dt) = 0;

  // Simulation.
public:
  virtual void tick_top (double snow_leak_rate /* [h^-1] */,
                         double cover /* [] */,
                         double canopy_leak_rate /* [h^-1] */,
                         double surface_runoff_rate /* [h^-1] */,
                         double dt /* [h] */) = 0;
  virtual void mixture (const Geometry& geo,
                        const double pond /* [mm] */, 
                        const double rate /* [h/mm] */,
                        const double dt /* [h]*/) = 0;
  virtual void infiltrate (const double rate, const double dt) = 0;
  virtual double down () = 0;     // [g/m^2/h]
  virtual void uptake (const Soil&, const SoilWater&, double dt) = 0;
  virtual void decompose (const Geometry& geo,
                          const Soil&, const SoilWater&, const SoilHeat&, 
                          const OrganicMatter&, double dt) = 0;
  virtual void output (Log&) const = 0;

  // Create and Destroy.
private:
  Chemical ();
  explicit Chemical (const Chemical&);
  Chemical& operator= (const Chemical&);
protected:
  explicit Chemical (Block&);
public:
  ~Chemical ();
};

#endif // CHEMICAL_H
