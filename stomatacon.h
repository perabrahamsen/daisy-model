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

#include "model_logable.h"
#include <vector>

class Log;
class Treelog;
class BlockModel;

class StomataCon : public ModelLogable
{
  // Content.
public:
  static const char *const component;
  symbol library_id () const;

  // Simulation.
public:
  virtual void output (Log&) const = 0;
  virtual double minimum () const = 0;
  virtual double stomata_con (const double ABA,  // [g/cm^3]
                              const double h_x,  // [MPa]
                              const double hs /*[]*/, 
                              const double pz /*[mol/m²leaf/s]*/,
                              const double Ptot /*[Pa]*/,
                              const double cs /*[Pa]*/,
                              const double Gamma /*[Pa]*/, 
                              const double Ds /*[Pa]*/,
                              Treelog&) = 0;//[]
  // Create and Destroy.
protected:
  StomataCon (const BlockModel&);

public:
  ~StomataCon ();
};

struct StomataCon_WSF_base : public StomataCon
{
  // Parameters.
  const double beta;  // ABA coefficient.
  const double ABA_min;         // Unstressed ABA.
  const double delta; // Psi coefficient.

  double wsf (const double ABA /* g/cm^3 */, const double h_x /* MPa */) const;

  // Create.
  StomataCon_WSF_base (const BlockModel&);
};

#endif // STOMATACON_H
