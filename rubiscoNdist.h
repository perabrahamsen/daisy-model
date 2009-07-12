// rubiscoNdist.h -- Rubisco N distribution in canopy
// 
// Copyright 2006 Birgitte Gjettermann, Per Abrahamsen and KVL
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


#ifndef RUBISCONDIST_H
#define RUBISCONDIST_H

#include "model_logable.h"
#include <vector>

class Log;
class Treelog;
class BlockModel;
class Units;

class RubiscoNdist : public ModelLogable
{
  // Content.
public:
  static const char *const component;
  symbol library_id () const;

  // Simulation.
public:
  virtual void output (Log&) const = 0;
  virtual void rubiscoN_distribution (const Units& units,
                                      const std::vector <double>& PAR_height, 
				      const double LAI, const double DS,
				      std::vector <double>& rubiscoNdist, 
				      const double cropN, Treelog& msg)=0;
  // Create and Destroy.
protected:
  RubiscoNdist (const BlockModel&);

public:
  ~RubiscoNdist ();
};

#endif // RUBISCONDIST_H
