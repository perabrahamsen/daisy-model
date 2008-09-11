// colloids.h -- Calculating amount of colloids.
// 
// Copyright 2008 Birgitte Gjettermann, Per Abrahamsen, Søren Hansen and KU
//
// This file is part of Daisy.
// 
// Daisy is free softw%are; you can redistribute it and/or modify
// it under the terms of the GNU Lesser Public License as published by
// the Free Software Foundation; either version 2.1 of the License, or
// (at your option) any later version.
// 
// Daisy is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser Public License for more details.// 
// You should have received a copy of the GNU Lesser Public License
// along with Daisy; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA


#ifndef COLLOIDS_H
#define COLLOIDS_H

#include "model.h"
#include <vector>

class Log;
class Treelog;
class Block;

class Colloids : public ModelLogable
{
  // Content.
public:
  static const char *const component;
  symbol library_id () const;

  // Simulation.
public:
  virtual void output (Log&) const = 0;
  virtual void initialize (const double Rain_intensity /*[]*/, Treelog&) = 0;
  double colloid_generation (const double zi, const double bulk_density, 
                             const double Rain_intensity /*[]*/, Treelog&);
  double colloid_filtration (const double Rain_intensity /*[]*/, Treelog&);

  // Create and Destroy.
protected:
  Colloids (Block&);

public:
  static const AttributeList& default_model ();
  ~Colloids ();
};

#endif // COLLOIDS_H
