// seed.h -- Initial crop growth.
// 
// Copyright 2008 Per Abrahamsen and KVL.
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

#ifndef SEED_H
#define SEED_H

#include "model.h"
#include <vector>

class Seed : public ModelLogable
{
  // Content.
public:
  static const char *const component;
  symbol library_id () const;

  // Simulation.
public:
  virtual double assimilate (const double ABA_xylem, const double rel_hum, 
			     const double CO2_atm, double Ta, double Tl, 
			     const double cropN,
                             const std::vector<double>& PAR,
                             const std::vector<double>& PAR_Height,
                             double PAR_LAI, 
			     const std::vector<double>& fraction,
                             double dt,
                             CanopyStandard& canopy,
                             Phenology& development, Treelog&) = 0;
  virtual void clear ();
  virtual void output (Log&) const = 0;

  // Create and Destroy.
public:
  static const AttributeList& default_model ();
  Seed (Block&);
  ~Seed ();
};

#endif // SEED_H
