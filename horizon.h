// horizon.h
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


#ifndef HORIZON_H
#define HORIZON_H

#include "librarian.h"

class Hydraulic;
class Tortuosity;
class Treelog;

// Weigth of mineral particles. [g / cm³]
GLOBAL_CONSTANT const double rho_mineral = 2.65;	
// Weight of humus. [g / cm³]
GLOBAL_CONSTANT const double rho_humus = 1.3; 
GLOBAL_CONSTANT const double rho_water = 1.0; // [g/cm^3]
GLOBAL_CONSTANT const double rho_ice = 0.917; // [g/cm^3]
GLOBAL_CONSTANT const double c_fraction_in_humus = 0.587;

class Horizon 
{
  // Content.
private:
  struct Implementation;
  Implementation& impl;
public:
  const string name;
  static const char *const description;

  // Water.
public:
  Hydraulic& hydraulic;
  double heat_conductivity (double Theta, double Ice) const; // [erg/cm/h/dg C]
  double heat_capacity (double Theta, double Ice) const; // [erg/cm^3/dg C]
  
  // Texture.
public:
  Tortuosity& tortuosity;
  double dry_bulk_density () const;
  double clay () const;
  double silt () const;
  double sand () const;
  double humus () const;
  double SOM_C (unsigned int pool) const;
  double SOM_C_per_N (unsigned int pool) const;

  // Chemistry.
public:
  bool has_attribute (const string& name) const;
  double get_attribute (const string& name) const;

  // Simulation.
public:
  void output (Log&) const;

  // Create and Destroy.
public:
  static void load_syntax (Syntax&, AttributeList&);
  Horizon (const AttributeList&);
  void initialize (bool top_soil, Treelog&);
  virtual ~Horizon ();
};

EMPTY_TEMPLATE
Librarian<Horizon>::Content* Librarian<Horizon>::content;

static Librarian<Horizon> Horizon_init ("horizon");

#endif // HORIZON_H
