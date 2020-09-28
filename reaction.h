// reaction.h --- Tranformation between soil chemicals.
// 
// Copyright 2004 Per Abrahamsen and KVL.
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


#ifndef REACTION_H
#define REACTION_H

#include "model_framed.h"

class Log;
class Geometry;
class Soil;
class SoilWater;
class SoilHeat;
class Surface;
class OrganicMatter;
class Chemistry;
class Treelog;
class BlockModel;
class Vegetation;
class Bioclimate;

class Reaction : public ModelFramed
{
  // Content.
public:
  static const char *const component;
  symbol library_id () const;

  // Simulation.
public:
  virtual void tick_top (const Vegetation&, const Bioclimate&,
			 const double tillage_age /* [d] */,
                         const double total_rain, const double h_pond,
			 OrganicMatter& organic, Chemistry& chemistry,
			 const double dt, Treelog&);
  virtual void tick_surface (const Geometry&, 
                             const Soil&, const SoilWater&, const SoilHeat&,
                             const Surface&,
			     OrganicMatter&, Chemistry&, const double dt,
                             Treelog& msg);
  virtual void tick_soil (const Geometry&, const Soil&, const SoilWater&,
                          const SoilHeat&, OrganicMatter&, Chemistry&,
                          const double dt, Treelog&);
  virtual void output (Log&) const = 0;

  // Create and Destroy.
public:
  virtual void initialize (const Geometry& geo,
                           const Soil&, const SoilWater&, const SoilHeat&, 
                           const OrganicMatter&, const Surface&, Treelog&) = 0;
  virtual bool check (const Geometry& geo,
                      const Soil&, const SoilWater&, const SoilHeat&,
		      const OrganicMatter&, const Chemistry&,
		      Treelog&) const = 0;
protected:
  Reaction (const BlockModel& al);
public:
  ~Reaction ();
};

#endif // REACTION_H
