// svat.h --- Soil, Vegetation and ATmostphere.
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


#ifndef SVAT_H
#define SVAT_H

#include "librarian.h"

class Soil;
class SoilHeat;
class SoilWater;
class Weather;
class Vegetation;
class Surface;
class Pet;

class SVAT
{
  // Content.
public:
  const string name;
  static const char *const description;

  // Simulation.
public:
  virtual void tick (const Weather&, const Vegetation&,
		     const Surface&, const Soil&, const SoilHeat&, 
		     const SoilWater&, const Pet&,
		     double canopy_ea, double snow_ea,
		     double pond_ea, double soil_ea, double crop_ea,
                     double crop_ep) = 0;
  virtual void output (Log&) const;
  virtual double production_stress () const = 0; // []

  // Create and Destroy.
  static void load_syntax (Syntax&, AttributeList&);
protected:
  SVAT (const AttributeList&);
public:
  virtual ~SVAT ();
};

#if !defined (__BORLANDC__)
EMPTY_TEMPLATE
Librarian<SVAT>::Content* Librarian<SVAT>::content;
#endif

static Librarian<SVAT> SVAT_init ("svat");

#endif // SVAT_H

// svat.h ends here.
