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

#include "librarian.h"

class Soil;

class Adsorption
{
  // Content.
public:
  static const char *const description;
  const string name;

  // Simulation.
public:
  virtual void output (Log&) const;
  virtual double C_to_M (const Soil&, double Theta, int i, double C) const = 0;
  virtual double M_to_C (const Soil&, double Theta, int i, double M) const = 0;

  // Create and Destroy.
protected:
  Adsorption (const string& name);
public:
  virtual ~Adsorption ();
};

static Librarian<Adsorption> Adsorption_init ("adsorption");

#endif // ADSORPTION_H
