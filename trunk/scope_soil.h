// scope_soil.h --- Look up values in the soil.
// 
// Copyright 2007 Per Abrahamsen and KVL.
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


#ifndef SCOPE_SOIL_H
#define SCOPE_SOIL_H

#include "scope.h"
#include <vector>

class Geometry;
class Soil;
class SoilWater;
class SoilHeat;
class Chemical;

class ScopeSoil : public Scope
{
  // Utilities.
public:
  static const symbol rho_b;
  static const symbol rho_b_unit;
  static const symbol clay;
  static const symbol humus;
  static const symbol h;
  static const symbol Theta;
  static const symbol T;

  // Types.
public:
  enum domain_t { primary, secondary, matrix };

  // Content.
private:
  const Geometry& geo;
  const Soil& soil;
  const SoilWater& soil_water;
  const SoilHeat& soil_heat;
  const std::vector<symbol> all_numbers_;

  // State.
private:
  bool old_water;
  domain_t domain;
  double dry_bulk_density;
  double Theta_extra;

  int cell;			// Current cell.
public:
  void set_cell (int);
  void set_old_water (bool old);
  void set_domain (domain_t);
  void set_dry_bulk_density (double rho_b); // Replace rho_b.
  void set_extra_water (double Theta_extra);

  // Scope Interface.
public:
  void entries (std::set<symbol>&) const;
  Attribute::type lookup (symbol tag) const;
  bool check (symbol tag) const;
  double number (symbol tag) const;
  symbol dimension (symbol tag) const;
  symbol description (symbol tag) const;

  // Create and Destroy.
private:
  static std::vector<symbol> 
  /**/ find_numbers (const Soil&);
public:
  explicit ScopeSoil (const Geometry&, 
                      const Soil&, const SoilWater&, const SoilHeat&);
  ~ScopeSoil ();
};

#endif // SCOPE_SOIL_H
