// stomatacon.h -- Calculating stomata conductance.
// 
// Copyright 2008 Birgitte Gjettermann, Per Abrahamsen and KU
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
// GNU Lesser Public License for more details.// 
// You should have received a copy of the GNU Lesser Public License
// along with Daisy; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA


#ifndef STOMATACON_H
#define STOMATACON_H

#include "model.h"
#include <vector>

class Log;
class Treelog;
class Block;

class StomataCon : public ModelLogable
{
  // Content.
public:
  static const char *const component;
  symbol library_id () const;

  // Simulation.
public:
  virtual void output (Log&) const = 0;
  virtual double stomata_con (const double wsf /*[]*/, const double m /*[]*/,
                              const double hs /*[]*/, 
                              const double pz /*[mol/m²leaf/s]*/,
                              const double Ptot /*[Pa]*/, const double cs /*[Pa]*/,
                              const double Gamma /*[Pa]*/, 
                              const double intercept /*[mol/m²leaf/s]*/, 
                              const double CO2_atm /*[Pa]*/, const double Ds /*[Pa]*/,
                              Treelog&) = 0;//[]
  // Create and Destroy.
protected:
  StomataCon (Block&);

public:
  static const AttributeList& default_model ();
  ~StomataCon ();
};

#endif // STOMATACON_H
