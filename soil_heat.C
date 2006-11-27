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
#include "block.h"
#include "alist.h"
#include "geometry.h"
#include "soil.h"
#include "soil_water.h"
#include "weather.h"
#include "timestep.h"
#include "syntax.h"
#include "log.h"
#include "submodel.h"
#include <sstream>

static const double rho_water = 1.0; // [g/cm^3]
static const double rho_ice = 0.917; // [g/cm^3]
static const double gravity = 982.; // [cm/s^2]

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

double 
SoilHeat::capacity (const Soil& soil, 
                    const SoilWater& soil_water,
                    const size_t i) const
{ 
  const double Theta = soil_water.Theta (i);
  const double X_ice = soil_water.X_ice (i);
  return soil.heat_capacity (i, Theta, X_ice); 
}

double 
SoilHeat::capacity_apparent
/**/ (const Soil& soil, const SoilWater& soil_water, const size_t i) const
{ 
  const double h = soil_water.h (i);
  const double h_ice = soil_water.h_ice (i);
  double cap = capacity (soil, soil_water, i);

  switch (state[i])
    {
    case freezing:
      cap += (latent_heat_of_fussion * latent_heat_of_fussion
              * rho_water * soil.Cw2 (i, h)
              / (273. * gravity));
      break;
    case thawing:
      cap += (latent_heat_of_fussion * latent_heat_of_fussion
              * rho_water * soil.Cw2 (i, h_ice)
              / (273. * gravity));
      break;
    case liquid:
    case frozen:
      break;
    }
  return cap;
}

void
SoilHeat::update_freezing_points (const Soil& soil,
                                  const SoilWater& soil_water)
{
  for (unsigned int i = 0; i < soil.size (); i++)
    {
      const double Theta = soil_water.Theta (i);
      const double h = soil_water.h (i);
      const double h_ice = soil_water.h_ice (i);
      const double h_melt = std::max (h_ice, h);

      T_thawing[i] 
        = std::min (0.0, 273. *  h_melt / (latent_heat_of_fussion /gravity - h_melt));
      T_freezing[i] 
        = std::min (T_thawing[i] - 0.01, 273. *  h / (latent_heat_of_fussion / gravity - h));
      daisy_assert (T_freezing[i] <= T_thawing[i]);

      switch (state[i])
        { 
        case liquid:
          break;
        case freezing:
          if (Theta < soil.Theta (i, h_frozen, 0.0))
            state[i] = frozen;
          break;
        case frozen:
          if (Theta > soil.Theta (i, h_frozen + 1000.0, 0.0))
            state[i] = freezing;
          break;
        case thawing:
          break;
        }
    }
} 

bool
SoilHeat::update_state (const Geometry& geo,
                        const Soil& soil, 
                        const SoilWater& soil_water, 
                        std::vector<double>& T)
{
  bool changed = false;

  for (unsigned int i = 0; i < soil.size (); i++)
    {
      switch (state[i])
        {
        case freezing:
          if (T[i] < T_freezing[i])
            {
              // Find freezing rate.
              freezing_rate[i] = calculate_freezing_rate (geo, 
                                                          soil, soil_water, 
                                                          i, T);

              if (freezing_rate[i] < 0.0)
                freezing_rate[i] = 0.0;

              // Check if there are sufficient water.
              const double Theta_min = soil.Theta (i, h_frozen - 1000.0, 0.0);
              daisy_assert (Theta_min >= soil.Theta_res (i));
              const double available_water = soil_water.Theta (i) - Theta_min;
              if (freezing_rate[i] * dt > available_water)
                freezing_rate[i] = std::max (0.0, available_water / dt);
	      
              // We have used the energy.
              T[i] = T_freezing[i];
            }
          else if (T[i] > T_thawing[i])
            {
              if (soil_water.X_ice_total (i) > 0.0)
                state[i] = thawing;
              else
                state[i] = liquid;

              changed = true;
              freezing_rate[i] = 0.0;
            }
          else
            freezing_rate[i] = 0.0;
          daisy_assert (-freezing_rate[i] * rho_water / rho_ice
                        <= soil_water.X_ice_total (i));
          break;
        case frozen:
          freezing_rate[i] = 0.0;
          if (T[i] > T_thawing[i])
            {
              if (soil_water.X_ice_total (i) > 0.0)
                state[i] = thawing;
              else
                state[i] = liquid;
              changed = true;
            }
          break;
        case thawing:
          if (T[i] > T_thawing[i])
            {
              freezing_rate[i] = calculate_freezing_rate (geo, 
                                                          soil, soil_water, 
                                                          i, T);
              if (freezing_rate[i] > 0.0)
                freezing_rate[i] = 0.0;

              const double X_ice_total = soil_water.X_ice_total (i);
              const double ice_water = X_ice_total * rho_ice / rho_water;
              if (-freezing_rate[i] * dt >= ice_water)
                {
                  freezing_rate[i] = -ice_water / dt;
                  daisy_assert (freezing_rate[i] <= 0.0);

                  state[i] = liquid;
                }
              // We have used the energy.
              T[i] = T_thawing[i];

            }
          else if (T[i] < T_freezing[i])
            {
              state[i] = freezing;
              changed = true;
              freezing_rate[i] = 0.0;
            }
          else
            freezing_rate[i] = 0.0;
          daisy_assert (-freezing_rate[i] * rho_water / rho_ice
                        <= soil_water.X_ice_total (i) * 1.0001);
          break;
        case liquid:
          freezing_rate[i] = 0.0;
          if (T[i] < T_freezing[i])
            {
              state[i] = freezing;
              changed = true;
            }
          break;
        }
      daisy_assert (-freezing_rate[i] * rho_water / rho_ice
                    <= soil_water.X_ice_total (i) * 1.0001);
    }
  return changed;
}

double
SoilHeat::calculate_freezing_rate (const Geometry& geo,
                                   const Soil& soil,
                                   const SoilWater& soil_water,
                                   unsigned int i, 
                                   const std::vector<double>& T)
{
  const double T_mean = (T[i] + T_old[i]) / 2.0;
  const double dT = T[i] - T_old[i];
  const double dq = q[i] - q[i+1];
  const double vol = geo.volume (i);
  const double S 
    = soil_water.S_sum (i) - soil_water.S_ice (i) * rho_ice / rho_water;
  const double Sh
    = water_heat_capacity * rho_water * S * T_mean;
  const double cap = capacity (soil, soil_water, i);
  return (1.0 / (latent_heat_of_fussion * rho_ice))
    * (cap * dT / dt + dq / vol + Sh);
}

bool
SoilHeat::check_state (const Soil& soil, 
                       const std::vector<double>& T) const
{
  for (unsigned int i = 0; i < soil.size (); i++)
    {
      switch (state[i])
        {
        case freezing:
          if (T[i] > T_freezing[i])
            return true;
          break;
        case frozen:
          if (T[i] > T_thawing[i])
            return true;
          break;
        case thawing:
          if (T[i] < T_thawing[i])
            return true;
          break;
        case liquid:
          if (T[i] < T_freezing[i])
            return true;
          break;
        }
    }
  return false;
}

void
SoilHeat::force_state (std::vector<double>& T)
{
  for (unsigned int i = 0; T.size (); i++)
    {
      switch (state[i])
        {
        case freezing:
          T[i] = T_freezing[i];
          break;
        case frozen:
          if (T[i] > T_thawing[i])
            T[i] = T_thawing[i];
          break;
        case thawing:
          T[i] = T_thawing[i];
          break;
        case liquid:
          if (T[i] < T_freezing[i])
            T[i] = T_freezing[i];
          break;
        }
    }
}

double 
SoilHeat::top_flux (const Geometry& geo,
                      const Soil& soil, const SoilWater& soil_water) const
{
  // BUG: Geometry1D only.
  const double k 
    = soil.heat_conductivity (0, soil_water.Theta (0), soil_water.X_ice (0))
    * 1e-7 * 1e4 / 3600.0;	// erg/h/ cm/ K -> W/m^2/K
  return k * (T (0) - T (1)) / (geo.z (0) - geo.z (1));
}

void
SoilHeat::output (Log& log) const
{
  output_value (T_, "T", log); 
  output_value (capacity_, "capacity", log); 
  output_value (conductivity_, "conductivity", log); 
  output_value (T_top, "T_top", log);
  output_value (T_freezing, "T_freezing", log);
  output_value (T_thawing, "T_thawing", log);
  output_value (q, "q", log);
  static const symbol state_symbol ("state");
  if (log.check_leaf (state_symbol))
    {
      std::vector<double> tmp (state.size (), -1.0);
      for (unsigned int i = 0; i < state.size (); i++)
        tmp[i] = static_cast<double> (state[i]);
      output_value (tmp, "state", log);
    }
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
  if (S.size () != n)
    {
      std::ostringstream tmp;
      tmp << "You have " << n << " intervals but " 
          << S.size () << " sink values";
      err.entry (tmp.str ());
      ok = false;
    }
  return ok;
}

void
SoilHeat::load_syntax (Syntax& syntax, AttributeList& alist)
{ 
  alist.add ("submodel", "SoilHeat");
  alist.add ("description", "Temperature and heat flux in soil.");
  Geometry::add_layer (syntax, Syntax::OptionalState, "T", "dg C",
                       "Soil temperature.");
  syntax.add ("S", "erg/cm^3/h", Syntax::OptionalState, 
              "External heat source, by default zero.");
  syntax.add ("conductivity", "erg/cm^3/dg C/h", Syntax::LogOnly, 
              "Heat conductivity.");
  syntax.add ("capacity", "erg/cm^3/dg C", Syntax::LogOnly, 
              "Heat capacity.");
  syntax.add ("h_frozen", "cm^-1", Syntax::Const,
              "Pressure below which no more water will freeze.");
  alist.add ("h_frozen", -15000.0);
  syntax.add ("enable_ice", Syntax::Boolean, Syntax::Const,
              "Disable this to prevent water from freezing.");
  alist.add ("enable_ice", false);
  syntax.add ("T_top", "dg C", Syntax::OptionalState, 
              "Surface temperature at previous time step.");
  syntax.add ("T_freezing", "dg C", Syntax::LogOnly, Syntax::Sequence,
              "Freezing point depression for freezing.");
  syntax.add ("T_thawing", "dg C", Syntax::LogOnly, Syntax::Sequence,
              "Freezing point depression for thawing.");
  syntax.add ("q", "erg/cm^2/h", Syntax::LogOnly, Syntax::Sequence,
              "Heat flux (positive upward).");
  syntax.add ("state", Syntax::Unknown (), Syntax::LogOnly, Syntax::Sequence,
              "Current freezing/melting state.");
}

SoilHeat::SoilHeat (const Block& al)
  : h_frozen (al.number ("h_frozen")),
    enable_ice (al.flag ("enable_ice")),
    T_top (al.number ("T_top", -500.0))
{
  if (al.check ("S"))
    S = al.number_sequence ("S");
}

void
SoilHeat::initialize (const AttributeList& al, 
                      const Geometry& geo,
                      const std::vector<double>& default_T,
                      Treelog& out)
{
  // Fetch initial T.
  geo.initialize_layer (T_, al, "T", out);
  const size_t cell_size = geo.cell_size ();
  const size_t edge_size = geo.edge_size ();
  while (S.size () < cell_size)
    S.push_back (0.0);
  capacity_.insert (capacity_.begin (), cell_size, -42.42e42);
  conductivity_.insert (conductivity_.begin (), cell_size, -42.42e42);

  // Freezing point.
  T_freezing.insert (T_freezing.end (), cell_size, 0.0);
  T_thawing.insert (T_thawing.end (), cell_size, 0.0);
  state.insert (state.end (), cell_size, liquid);
  freezing_rate.insert (freezing_rate.end (), cell_size, 0.0);
  q.insert (q.end (), edge_size, 0.0);

  for (unsigned int i = 0; i < cell_size; i++)
    if (T_.size () <= i)
      T_.push_back (default_T[i]);

  // May be larger if user initialized it wrongly.  Checked in "check".
  daisy_assert (T_.size () >= cell_size);
}

SoilHeat::~SoilHeat ()
{ }

static Submodel::Register 
soil_heat_submodel ("SoilHeat", SoilHeat::load_syntax);

// soil_heat.C ends here.
