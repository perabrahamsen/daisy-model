// reaction_colgen.h --- Base model for coloid generation.
// 
// Copyright 2009 Per Abrahamsen and KVL.
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


#ifndef REACTION_COLGEN_H
#define REACTION_COLGEN_H

#include "reaction.h"
#include "ponddamp.h"
#include <memory>

class ReactionColgen : public Reaction
{
  // Parameters.
protected:
  const symbol colloid_name;
  const std::unique_ptr<Ponddamp> ponddamp;

  // Log variable.
protected:
  double dds;                   // Median droplet size [mm]
  double KH;                    // Ponding factor []
  double D;                     // Depletion [g/cm^2/h]
  double surface_release;       // Fraction of available colloids released []

  // Simulation.
protected:
  static double find_surface_soil (const Geometry&, const Soil&, const Surface&)
    ; // Soil in mixing layer. [g/cm^2] 
  void tick_colgen (const double total_rain, const double h_pond);
  void output_colgen (Log&) const;

  // Create and Destroy.
protected:
  bool check (const Geometry&, const Soil&, const SoilWater&, const SoilHeat&,
	      const OrganicMatter&, const Chemistry&, Treelog&) const;
  ReactionColgen (const BlockModel&);
  ~ReactionColgen ();
};

#endif // REACTION_COLGEN_H
