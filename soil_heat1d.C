// soil_heat1d.C
// 
// Copyright 1996-2001,2006 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001,2006 KVL.
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


#include "soil_heat1d.h"
#include "alist.h"
#include "surface.h"
#include "weather.h"
#include "soil_water.h"
#include "soil.h"
#include "geometry.h"
#include "syntax.h"
#include "time.h"
#include "mathlib.h"
#include "log.h"
#include <sstream>
#include "submodel.h"

static const double water_heat_capacity = 4.2e7; // [erg/cm^3/dg C]
static const double rho_water = 1.0; // [g/cm^3]
static const double rho_ice = 0.917; // [g/cm^3]

struct SoilHeat1D::Implementation
{
  // Parameters
  const double h_frozen;
  const bool enable_ice;

  // State
  std::vector<double> T_old;
  double T_top;
  double T_top_old;
  double T_bottom;
  std::vector<double> T_freezing;
  std::vector<double> T_thawing;
  std::vector<double> freezing_rate;
  std::vector<state_t> state;
  std::vector<double> q;
  std::vector<double> S;

  /* const */ double delay;	// Period delay [ cm/rad ??? ]

  

  // Simulation.
  double capacity (const Soil&, const SoilWater&, size_t i) const;
  double capacity_apparent (const Soil&, const SoilWater&, size_t i) const;
  void tick (const Time&, const Geometry& geo,
             const Soil&, SoilWater&, 
             const Surface&, const Weather&, 
             std::vector<double>& T);
  void update_freezing_points (const Soil& soil,
                               const SoilWater& soil_water);
  bool update_state (const Geometry& geo,
                     const Soil& soil, const SoilWater& soil_water, 
                     std::vector<double>& T);
  double calculate_freezing_rate (const Geometry& geo,
                                  const Soil& soil,
                                  const SoilWater& soil_water,
                                  unsigned int i, 
                                  const std::vector<double>& T);
  bool check_state (const Soil& soil, 
                    const std::vector<double>& T) const;
  void force_state (std::vector<double>& T);
  void solve (const Time&, const Geometry& geo,
              const Soil&, const SoilWater&, 
              const Surface&, const Weather&, 
              std::vector<double>& T);
  void calculate_heat_flux (const Geometry& geo,
                            const Soil&, const SoilWater&, 
                            const std::vector<double>& T);
  double energy (const Geometry& geo,
                 const Soil&, const SoilWater&, double from, double to, 
                 const std::vector<double>& T) const;
  void set_energy (const Geometry& geo,
                   const Soil&, const SoilWater&, 
                   double from, double to, double energy, 
                   std::vector<double>& T);
  double bottom (const Time&, const Weather& weather) const;

  // Create & Destroy.
  void initialize (const Geometry& geo,
                   const Soil& soil, const Time& time, const Weather& weather, 
                   std::vector<double>& T,
                   Treelog&);
  Implementation (const AttributeList&);
};

static const double latent_heat_of_fussion = 3.35e9; // [erg/g]
static const double gravity = 982.; // [cm/s^2]

double 
SoilHeat1D::Implementation::capacity (const Soil& soil, 
                                      const SoilWater& soil_water,
                                      const size_t i) const
{ 
  const double Theta = soil_water.Theta (i);
  const double X_ice = soil_water.X_ice (i);
  return soil.heat_capacity (i, Theta, X_ice); 
}

double 
SoilHeat1D::Implementation::capacity_apparent
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
SoilHeat1D::Implementation::tick (const Time& time,
                                  const Geometry& geo,
                                  const Soil& soil,
                                  SoilWater& soil_water,
                                  const Surface& surface,
                                  const Weather& weather, 
                                  std::vector<double>& T)
{
  // Update freezing and melting points.
  update_freezing_points (soil, soil_water);

  // Solve with old state.
  T_old = T;
  solve (time, geo, soil, soil_water, surface, weather, T);

  // Check if ice is enabled.
  if (!enable_ice)
    return;

  // Update state according to new temperatures.
  const bool changed = update_state (geo, soil, soil_water, T);

  if (changed)
    {
      // Solve again with new state.
      T = T_old;
      solve (time, geo, soil, soil_water, surface, weather, T);

      // Check if state match new temperatures.
      const bool changed_again = check_state (soil, T);
      
      if (changed_again)
        {
          // Force temperatures to match state.
          force_state (T);
        }
    }
  // Update ice in water.
  soil_water.freeze (soil, freezing_rate);
}
  
void
SoilHeat1D::Implementation::update_freezing_points (const Soil& soil,
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
SoilHeat1D::Implementation::update_state (const Geometry& geo,
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
SoilHeat1D::Implementation::calculate_freezing_rate (const Geometry& geo,
                                                     const Soil& soil,
                                                     const SoilWater& soil_water,
                                                     unsigned int i, 
                                                     const std::vector<double>& T)
{
  const double T_mean = (T[i] + T_old[i]) / 2.0;
  const double dT = T[i] - T_old[i];
  const double dq = q[i] - q[i+1];
  const double dz = geo.dz (i);
  const double S 
    = soil_water.S_sum (i) - soil_water.S_ice (i) * rho_ice / rho_water;
  const double Sh
    = water_heat_capacity * rho_water * S * T_mean;
  const double cap = capacity (soil, soil_water, i);
  return (1.0 / (latent_heat_of_fussion * rho_ice))
    * (cap * dT / dt + dq / dz + Sh);
}

bool
SoilHeat1D::Implementation::check_state (const Soil& soil, 
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
SoilHeat1D::Implementation::force_state (std::vector<double>& T)
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

void
SoilHeat1D::Implementation::solve (const Time& time,
                                   const Geometry& geo,
                                   const Soil& soil,
                                   const SoilWater& soil_water,
                                   const Surface& surface,
                                   const Weather& weather, 
                                   std::vector<double>& T)
{
  // Border conditions.
  T_bottom = bottom (time, weather); // BUGLET: Should be time - 1 hour.
		     
  const double T_top_new = surface.temperature ();
  
  if (T_top < -400.0)
    T_top = T_top_new;

  int size = soil.size ();

  // Tridiagonal matrix.
  std::vector<double> a (size, 0.0);
  std::vector<double> b (size, 0.0);
  std::vector<double> c (size, 0.0);
  std::vector<double> d (size, 0.0);

  // Inner nodes.
  for (int i = 0; i < size; i++)
    {
      // Soil Water
      const double Theta = soil_water.Theta (i);
      const double X_ice = soil_water.X_ice (i);

      const int prev = i - 1;
      const int next = i + 1;

      // Calculate average heat capacity and conductivity.
      const double conductivity = soil.heat_conductivity (i, Theta, X_ice);

      // Calculate distances.
      const double dz_next 
        = (i == size - 1)
        ? geo.z (i) - geo.z (prev)
        : geo.z (next) - geo.z (i);
      const double dz_prev 
        = (i == 0)
        ? geo.z (i) - 0.0
        : geo.z (i) - geo.z (prev);
      const double dz_both = dz_prev + dz_next;

      // Calculate temperature differences.
      const double dT_next = ((i == size - 1)
                              ? T_bottom - T[i] 
                              : T[next] - T[i]);
      const double dT_prev = (i == 0) ? T[i] - T_top : T[i] - T[prev];
      const double dT_both = dT_prev + dT_next;
      
      // Calculate conductivity gradient.
      double gradient;
      if (i == 0)
        gradient = 0.0;
      else if (i == size - 1)
        gradient 
          = (soil.heat_conductivity (i, 
                                     soil_water.Theta (i),
                                     soil_water.X_ice (i))
             - soil.heat_conductivity (prev, 
                                       soil_water.Theta (prev),
                                       soil_water.X_ice (prev)))
          / dz_prev;
      else
        gradient 
          = (soil.heat_conductivity (next, 
                                     soil_water.Theta (next),
                                     soil_water.X_ice (next))
             - soil.heat_conductivity (prev, 
                                       soil_water.Theta (prev),
                                       soil_water.X_ice (prev)))
          / dz_both;
      
      // Computational,
      const double Cx = gradient
        + water_heat_capacity
        * (soil_water.q (i) + soil_water.q (next)) / 2.0;

      // Heat capacity including thawing/freezing.
      const double capacity = capacity_apparent (soil, soil_water, i);

      // Setup tridiagonal matrix.
      a[i] = - conductivity / dz_both / dz_prev + Cx / 2.0 / dz_both;
      b[i] = capacity / dt
        + conductivity / dz_both * (1.0 / dz_next + 1.0 / dz_prev);
      c[i] = - conductivity / dz_both / dz_next - Cx / 2.0 / dz_both;
      const double x2 = dT_next / dz_next - dT_prev/ dz_prev;
      if (i == 0)
        d[i] = T[i] * capacity / dt
          + conductivity / geo.z (1) * (x2 + T_top_new / geo.z (0))
          + Cx * (T[1] - T_top + T_top_new) / (2.0 * geo.z (1));
      else
        d[i] = T[i] * capacity / dt + (conductivity / dz_both) * x2
          + Cx * dT_both / dz_both / 2.0;
      
      if (state[i] == freezing || state[i] == thawing)
        d[i] -= latent_heat_of_fussion * rho_water
          * (soil_water.q (i) - soil_water.q (next)) / geo.dz (i) / dt;

      // External heat source.
      d[i] += S[i];
    }
  d[size - 1] = d[size - 1] - c[size - 1] * T_bottom;
  tridia (0, size, a, b, c, d, T.begin ());
  T_top_old = T_top;
  T_top = T_top_new;
  daisy_assert (T[0] < 50.0);

  calculate_heat_flux (geo, soil, soil_water, T);
}

void
SoilHeat1D::Implementation::calculate_heat_flux (const Geometry& geo,
                                                 const Soil& soil,
                                                 const SoilWater& soil_water, 
                                                 const std::vector<double>& T)
{
  // Top and inner nodes.
  double T_prev = (T_top + T_top_old) / 2.0;
  double z_prev = 0.0;
  for (unsigned int i = 0; i < soil.size (); i++)
    {
      const double Theta = soil_water.Theta (i);
      const double X_ice = soil_water.X_ice (i);
      const double K = soil.heat_conductivity (i, Theta, X_ice);
      const double T_next = (T[i] + T_old[i]) / 2.0;
      const double dT = T_prev - T_next;
      const double dz = z_prev - geo.z (i);
      const double q_water = soil_water.q (i);
      const double T = (T_prev + T_next) / 2.0;

      q[i] = - K * dT/dz + water_heat_capacity * rho_water *  q_water * T;
      T_prev = T_next;
      z_prev = geo.z (i);
    }
  // Lower boundary.
  const unsigned int i = soil.size ();
  const unsigned int prev = i - 1U;
  const double Theta = soil_water.Theta (prev);
  const double X_ice = soil_water.X_ice (prev);
  const double K = soil.heat_conductivity (prev, Theta, X_ice);
  const double T_next = T_bottom;
  const double dT = T_prev - T_next;
  const double dz = geo.z (prev-1U) - geo.z (prev);
  const double q_water = soil_water.q (i);
  const double my_T = (T_prev + T_next) / 2.0;

  q[i] = - K * dT/dz - water_heat_capacity * rho_water *  q_water * my_T;
}
 

double 
SoilHeat1D::Implementation::energy (const Geometry& geo,
                                    const Soil& soil,
                                    const SoilWater& soil_water,
                                    double from, double to, 
                                    const std::vector<double>& T) const
{
  double amount = 0.0;
  double old = 0.0;

  for (unsigned int i = 0; i < soil.size () && old > to ; i++)
    {
      if (geo.zplus (i) < from)
        {
          const double height = (std::min (old, from) - std::max (geo.zplus (i), to));
          const double C = soil.heat_capacity (i, soil_water.Theta (i), soil_water.X_ice (i));
          amount += C * T[i] * height;
        }
      old = geo.zplus (i);
    }
  return amount;
}

void
SoilHeat1D::Implementation::set_energy (const Geometry& geo,
                                        const Soil& soil,
                                        const SoilWater& soil_water, 
                                        double from, double to, double energy, 
                                        std::vector<double>& T)
{
  // Find total energy capacity.
  double capacity = 0.0;
  double old = 0.0;

  for (unsigned int i = 0; i < soil.size () && old > to ; i++)
    {
      if (geo.zplus (i) < from)
        {
          const double height = (std::min (old, from) - std::max (geo.zplus (i), to));
          capacity += soil.heat_capacity (i, soil_water.Theta (i), 
                                          soil_water.X_ice (i)) * height;
        }
      old = geo.zplus (i);
    }
  
  // Distribute temperature evenly.
  const double average = energy / capacity / (to - from);
  old = 0.0;

  for (unsigned int i = 0; i < soil.size () && old > to ; i++)
    {
      if (geo.zplus (i) < from)
        {
          const double height = (std::min (old, from) - std::max (geo.zplus (i), to));
          T[i] = (height * average + (geo.dz (i) - height)* T[i]) 
            / geo.dz (i);
        }
      old = geo.zplus (i);
    }
}

double 
SoilHeat1D::Implementation::bottom (const Time& time, 
                                    const Weather& weather) const 
{
  return weather.T_normal (time, delay);
}

SoilHeat1D::Implementation::Implementation (const AttributeList& al)
  : h_frozen (al.number ("h_frozen")),
    enable_ice (al.flag ("enable_ice")),
    T_top (al.number ("T_top", -500.0))
{ 
  if (al.check ("S"))
    S = al.number_sequence ("S");
}

void
SoilHeat1D::Implementation::initialize (const Geometry& geo,
                                        const Soil& soil, 
                                        const Time& time, 
                                        const Weather& weather, 
                                        std::vector<double>& T,
                                        Treelog&)
{
  // Freezing point.
  T_freezing.insert (T_freezing.end (), soil.size (), 0.0);
  T_thawing.insert (T_thawing.end (), soil.size (), 0.0);
  state.insert (state.end (), soil.size (), liquid);
  freezing_rate.insert (freezing_rate.end (), soil.size (), 0.0);
  q.insert (q.end (), soil.size () + 1U, 0.0);
  while (S.size () < soil.size ())
    S.push_back (0.0);

  // Fetch average temperatur.
  const double rad_per_day = 2.0 * M_PI / 365.0;

  // Calculate delay.
  const double pF_2_0 = -100.0;
  double k = 0;
  double C = 0;
  
  for (unsigned int i = 0; i < soil.size (); i++)
    {
      const double Theta_pF_2_0 = soil.Theta (i, pF_2_0, 0.0);
      k += geo.dz (i) * soil.heat_conductivity (i, Theta_pF_2_0, 0.0);
      C += geo.dz (i) * soil.heat_capacity (i, Theta_pF_2_0, 0.0);
      const double a = k / C;
      delay = geo.zplus (i) / sqrt (24.0 * 2.0 * a / rad_per_day);

      // Fill out T if necessary.
      if (T.size () <= i)
        T.push_back (bottom (time, weather));
    }

  // We check for this in SoilHeat1D::check ().
  // daisy_assert (T.size () == soil.size ());
}

SoilHeat1D::state_t 
SoilHeat1D::state (const size_t i) const
{ return impl.state[i]; }

double 
SoilHeat1D::capacity (const Soil& soil, const SoilWater& soil_water, 
                      const size_t i) const
{ return impl.capacity (soil, soil_water, i); }

double 
SoilHeat1D::capacity_apparent (const Soil& soil,
                               const SoilWater& soil_water,
                               const size_t i) const
{ return impl.capacity_apparent (soil, soil_water, i); }

double 
SoilHeat1D::top_flux (const Geometry& geo,
                      const Soil& soil, const SoilWater& soil_water) const
{
  const double k 
    = soil.heat_conductivity (0, soil_water.Theta (0), soil_water.X_ice (0))
    * 1e-7 * 1e4 / 3600.0;	// erg/h/ cm/ K -> W/m^2/K
  return k * (T (0) - T (1)) / (geo.z (0) - geo.z (1));
}

void 
SoilHeat1D::tick (const Time& time, 
                  const Geometry& geo,
                  const Soil& soil,
                  SoilWater& soil_water,
                  const Surface& surface,
                  const Weather& weather)
{
  impl.tick (time, geo, soil, soil_water, surface, weather, T_);
}

double 
SoilHeat1D::energy (const Geometry& geo,
                    const Soil& soil,
                    const SoilWater& soil_water,
                    double from, double to) const
{
  return impl.energy (geo, soil, soil_water, from, to, T_);
}

void
SoilHeat1D::set_energy (const Geometry& geo,
                        const Soil& soil,
                        const SoilWater& soil_water, 
                        double from, double to, double energy)
{
  impl.set_energy (geo, soil, soil_water, from, to, energy, T_);
}

void
SoilHeat1D::swap (const Geometry& geo,
                  double from, double middle, double to)
{
  // This will only work right if the water is also swaped.
  // There *might* be a small error on the top and bottom nodes, but I
  // believe it should work as long as the energy is directly
  // proportional with the water content.
  geo.swap (T_, from, middle, to);
}
  
double 
SoilHeat1D::source (const size_t i) const
{ return impl.S[i]; }

void
SoilHeat1D::set_source (const size_t i, const double value)
{ impl.S[i] = value; }


void
SoilHeat1D::output (Log& log) const
{
  SoilHeat::output_base (log);
  output_value (impl.T_top, "T_top", log);
  output_value (impl.T_freezing, "T_freezing", log);
  output_value (impl.T_thawing, "T_thawing", log);
  output_value (impl.q, "q", log);
  static const symbol state_symbol ("state");
  if (log.check_leaf (state_symbol))
    {
      std::vector<double> tmp (impl.state.size (), -1.0);
      for (unsigned int i = 0; i < impl.state.size (); i++)
        tmp[i] = static_cast<double> (impl.state[i]);
      output_value (tmp, "state", log);
    }
}

void
SoilHeat1D::load_syntax (Syntax& syntax, AttributeList& alist)
{ 
  alist.add ("submodel", "SoilHeat");
  alist.add ("description", "Temperature and heat flux in soil.");
  SoilHeat::load_base (syntax, alist);
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
  syntax.add ("S", "erg/cm^3/h", Syntax::OptionalState, 
              "External heat source, by default zero.");
}

SoilHeat1D::SoilHeat1D (const AttributeList& al)
  : SoilHeat (al),
    impl (*new Implementation (al))
{ }


void
SoilHeat1D::initialize (const AttributeList& al, 
                        const Geometry& geo,
                        const Soil& soil, const Time& time, 
                        const Weather& weather, Treelog& out)
{ 
  SoilHeat::initialize_base (al, geo, out);
  impl.initialize (geo, soil, time, weather, T_, out); 
}

SoilHeat1D::~SoilHeat1D ()
{
  delete &impl;
}

static Submodel::Register 
soil_heat_submodel ("SoilHeat1D", SoilHeat1D::load_syntax);
