// soil_heat.C
// 
// Copyright 2006 Per Abrahamsen and KVL.
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


#include "soil_heat.h"
#include "alist.h"
#include "geometry.h"
#include "soil.h"
#include "soil_water.h"
#include "syntax.h"
#include "log.h"
#include <sstream>

double
SoilHeat::energy (const Geometry& geo, const Soil& soil,
                  const SoilWater& soil_water,
                  const double from, const double to) const
{
  const size_t cell_size = geo.cell_size ();
  double amount = 0.0;

  for (size_t i = 0; i < cell_size; i++)
    {
      const double f = geo.fraction_in_z_interval (i, from, to);
      if (f > 1e-10)
        {
          const double Theta = soil_water.Theta (i);
          const double X_ice = soil_water.X_ice (i);
          const double C = soil.heat_capacity (i, Theta, X_ice);
          const double V = geo.volume (i) * f;
          amount += C * T (i) * V;
        }
    }
  return amount;
}

void
SoilHeat::set_energy (const Geometry& geo, 
                      const Soil& soil, const SoilWater& soil_water, 
                      const double from, const double to, const double amount)
{
  const size_t cell_size = geo.cell_size ();

  // Find total energy capacity and volume.
  double capacity = 0.0;
  double volume = 0.0;

  for (size_t i = 0; i < cell_size; i++)
    {
      const double f = geo.fraction_in_z_interval (i, from, to);
      if (f > 1e-10)
        {
          const double Theta = soil_water.Theta (i);
          const double X_ice = soil_water.X_ice (i);
          const double C = soil.heat_capacity (i, Theta, X_ice);
          const double V = geo.volume (i) * f;
          capacity += C * V;
          volume += V;
        }
    }
  
  // Distribute temperature evenly.
  const double average = amount / capacity;

  for (size_t i = 0; i < cell_size; i++)
    {
      const double f = geo.fraction_in_z_interval (i, from, to);
      if (f > 1e-10)
        T_[i] = f * average + (1 - f) * T_[i];
    }
}

void
SoilHeat::swap (const Geometry& geo,
                const double from, const double middle, const double to)
{
  // This will only work right if the water is also swaped.
  // There *might* be a small error on the top and bottom cells, but I
  // believe it should work as long as the energy is directly
  // proportional with the water content.
  geo.swap (T_, from, middle, to);
}
  
void
SoilHeat::tick_after (const size_t cell_size, 
                      const Soil& soil, const SoilWater& soil_water, 
                      Treelog&)
{
  for (size_t i = 0; i < cell_size; i++)
    {
      conductivity_[i]
        = soil.heat_conductivity (i,
                                  soil_water.Theta (i), soil_water.X_ice (i));
      capacity_[i]
        = soil.heat_capacity (i, soil_water.Theta (i), soil_water.X_ice (i));
    }
}

void
SoilHeat::output_base (Log& log) const
{
  output_value (T_, "T", log); 
  output_value (capacity_, "capacity", log); 
  output_value (conductivity_, "conductivity", log); 
}

bool
SoilHeat::check (const size_t n, Treelog& err) const
{
  bool ok = true;
  if (T_.size () != n)
    {
      std::ostringstream tmp;
      tmp << "You have " << n << " intervals but " 
          << T_.size () << " T values";
      err.entry (tmp.str ());
      ok = false;
    }
  return ok;
}

void
SoilHeat::load_base (Syntax& syntax, AttributeList&)
{ 
  Geometry::add_layer (syntax, Syntax::OptionalState, "T", "dg C",
                       "Soil temperature.");
  syntax.add ("S", "erg/cm^3/h", Syntax::OptionalState, 
              "External heat source, by default zero.");
  syntax.add ("conductivity", "erg/cm^3/dg C/h", Syntax::LogOnly, 
              "Heat conductivity.");
  syntax.add ("capacity", "erg/cm^3/dg C", Syntax::LogOnly, 
              "Heat capacity.");
}

SoilHeat::SoilHeat (const Block& al)
{
  if (al.check ("S"))
    S = al.number_sequence ("S");
}

void
SoilHeat::initialize_base (const AttributeList& al, 
                           const Geometry& geo,
                           Treelog& out)
{
  // Fetch initial T.
  geo.initialize_layer (T_, al, "T", out);
  const size_t cell_size = geo.cell_size ();
  while (S.size () < cell_size)
    S.push_back (0.0);
  capacity_.insert (capacity_.begin (), cell_size, -42.42e42);
  conductivity_.insert (conductivity_.begin (), cell_size, -42.42e42);
}

SoilHeat::~SoilHeat ()
{ }
