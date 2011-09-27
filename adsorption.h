// adsorption.h
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


#ifndef ADSORPTION_H
#define ADSORPTION_H

#include "model_derived.h"

class Log;
class Soil;
class BlockModel;

class Adsorption : public ModelDerived
{
  // Content.
public:
  static const char *const component;
  symbol library_id () const;

  // Simulation.
public:
  virtual bool full () const;
  virtual void output (Log&) const;
private:
  virtual double C_to_M (const Soil&, double Theta, int i, double C, double sf) const = 0;
  virtual double M_to_C (const Soil&, double Theta, int i, double M, double sf) const = 0;
public:
  double C_to_M_total (const Soil&, double Theta, int i, double C) const;
  double M_to_C_total (const Soil&, double Theta, int i, double M) const;
  double C_to_M1 (const Soil&, double Theta, int i, double C) const;
  double M_to_C1 (const Soil&, double Theta, int i, double M) const;
  double C_to_M2 (const Soil&, double Theta, int i, double C) const;
  double M_to_C2 (const Soil&, double Theta, int i, double M) const;

  // Create and Destroy.
public:
  static const Adsorption& none ();
protected:
  Adsorption (const char* name);
  Adsorption (const BlockModel& al);
public:
  ~Adsorption ();
};

// Linear adsorption is treated specially in some solute transport models.
struct AdsorptionLinear : public Adsorption
{
  virtual double K (const Soil& soil, size_t c) const = 0;
protected:
  AdsorptionLinear (const BlockModel&);
};

#endif // ADSORPTION_H
