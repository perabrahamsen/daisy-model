// horheat.C --- Heat capicity and conductivity for horizons.
// 
// Copyright 1996-2004 Per Abrahamsen and Søren Hansen
// Copyright 2000-2004 KVL.
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


#ifndef HORHEAT_H
#define HORHEAT_H

struct AttributeList;
struct Syntax;
struct Treelog;
struct Texture;
struct Hydraulic;

#include <vector>

struct HorHeat
{
  // Parameters.
  const double quarts_form_factor;
  const double mineral_form_factor;

  // Heat Capacity and Conductivity.
  double C_soil;
  std::vector<double> K_water;
  std::vector<double> K_ice;

  // Use.
  double heat_conductivity (double Theta, double Ice) const; // [erg/cm/h/dg C]
  double heat_capacity (double Theta, double Ice) const; // [erg/cm^3/dg C]

  // Create and Destroy.
  static void load_syntax (Syntax& syntax, AttributeList& alist);
  // Note:  These variables are really not used after initialization.
  enum constituents
  { 
    Water, Ice, Air, Quarts, Minerals, Organic_Matter,
    Constituents_End,
    Constituents_Start = Water
  };
  double content[Constituents_End];
  double Theta_pF_high;
  void initialize (const Hydraulic&, const Texture& texture, 
                   double quarts, Treelog& msg);
  double HeatCapacity ();
  double DepolationsFactor (const Hydraulic&, 
			    const constituents medium, const double alfa);
  double ThermalConductivity (const Hydraulic&, constituents medium);
  const int intervals;

  static const double heat_capacity_table[Constituents_End];

  HorHeat (const AttributeList& al);
  ~HorHeat ();
};

#endif // HORHEAT_H
