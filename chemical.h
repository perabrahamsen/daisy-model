// chemical.h
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


#ifndef CHEMICAL_H
#define CHEMICAL_H

#include "librarian.h"

class Chemical : public Model
{
  // Content.
public:
  const symbol name;
  static const char *const description;
  static const char *const component;

  // Queries.
public:
  virtual double crop_uptake_reflection_factor () const	= 0; // [0-1]
  virtual double canopy_dissipation_rate () const = 0; // [h^-1]
  virtual double canopy_washoff_coefficient () const = 0; // [mm]
  virtual double diffusion_coefficient () const = 0; // in free solu. [cm² / h]
  virtual const AttributeList& solute_alist () const = 0;
  virtual double decompose_rate () const = 0; // [h^-1]
  virtual double decompose_heat_factor (double T) const = 0; // [dg C ->]
  virtual double decompose_water_factor (double h) const = 0 ; // [cm ->]
  virtual double decompose_CO2_factor (double CO2) const = 0; // [g C/cm^3 ->]
  virtual double decompose_conc_factor (double conc) const = 0; // [g X/cm^3 H2O->]
  virtual double decompose_depth_factor (double depth) const = 0; // [cm->]
  virtual double decompose_lag_increment (double conc) const = 0; // [g X/cm^3 H2O->]

  // Create and Destroy.
protected:
  Chemical (const AttributeList&);
public:
  ~Chemical ();
};

static Librarian<Chemical> Chemical_init;

#endif // CHEMICAL_H
