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

#define BUILD_DLL

#include "soil_heat.h"
#include "block.h"
#include "geometry.h"
#include "soil.h"
#include "soil_water.h"
#include "movement.h"
#include "weather.h"
#include "frame_submodel.h"
#include "log.h"
#include "treelog.h"
#include "assertion.h"
#include "librarian.h"
#include "vcheck.h"
#include "mathlib.h"
#include "check.h"
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
          const double V = geo.cell_volume (i) * f;
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
          const double V = geo.cell_volume (i) * f;
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

double 
SoilHeat::source (const Geometry& geo, const SoilWater& soil_water,
		  size_t c) const
{
  double v = 0.0;

  if (state[c] == SoilHeat::freezing || state[c] == SoilHeat::thawing)
    {
      const std::vector<size_t>& cell_edges = geo.cell_edges (c);
      for (size_t e = 0; e < cell_edges.size (); e++)
	{
	  const size_t edge = cell_edges[e];
	  const double in_sign = (geo.edge_to (edge) == c)
	    ? 1.0 : -1.0;				// []
	  const double area = geo.edge_area (edge);	// [cm^2]
	  const double flux = soil_water.q_matrix (edge); // [cm/h]
	  const double in = flux * area;		// [cm^3/h]
	  const double vol = geo.cell_volume (c);	// [cm^3]
	  v += latent_heat_of_fussion * rho_water * in_sign * in / vol;
	  // [erg/cm^3/h] += [erg/g] * [g/cm^3] * [] * [cm^3/h] / [cm^3]
	}
    }

  return v;
}

double 
SoilHeat::T_pseudo (const Geometry& geo,
		    const int cell,
		    const int other,
		    const std::vector<double>& T_old,
		    const double T_top,
		    const std::vector<double>& T,
		    const double T_bottom)
{
  if (geo.cell_is_internal (cell))
    return (T_old[cell] + T[cell]) / 2.0;
  
  switch (cell)
    {
    case Geometry::cell_above:
      return T_top;
    case Geometry::cell_below:
      return T_bottom;
    default:
      daisy_assert (geo.cell_is_internal (other));
      return (T_old[other] + T[other]) / 2.0;
    }
}

double 
SoilHeat::K_pseudo (const Geometry& geo, const int cell, const int other,
		    const Soil& soil, const SoilWater& soil_water)
{
  if (geo.cell_is_internal (cell))
    return soil.heat_conductivity (cell, 
				   soil_water.Theta (cell),
				   soil_water.X_ice (cell));

  daisy_assert (geo.cell_is_internal (other));
  return soil.heat_conductivity (other, 
				 soil_water.Theta (other),
				 soil_water.X_ice (other));
}

void 
SoilHeat::calculate_heat_flux (const Geometry& geo,
			       const Soil& soil,
			       const SoilWater& soil_water,
			       const std::vector<double>& T_old,
			       const double T_top,
			       const std::vector<double>& T_new,
			       const double T_bottom,
			       std::vector<double>& q)
{
  const size_t edge_size = geo.edge_size ();

  for (size_t e = 0; e < edge_size; e++)
    {
      const int from = geo.edge_from (e);
      const int to = geo.edge_to (e);
      const double l = geo.edge_length (e);	 // [cm]
      const double q_water = soil_water.q_matrix (e); // [cm/h]
      const double T_from			 // [dg C]
	= T_pseudo (geo, from, to, T_old, T_top, T_new, T_bottom);
      const double T_to	                         // [dg C]
	= T_pseudo (geo, to, from, T_old, T_top, T_new, T_bottom);
      const double dT = T_from - T_to;           // [dg C]
      const double T = (T_from + T_to) / 2.0;	 // [dg C]
      const double K_from			 // [erg/cm/h/dg C]
	= K_pseudo (geo, from, to, soil, soil_water);
      const double K_to		                 // [erg/cm/h/dg C]
	= K_pseudo (geo, from, to, soil, soil_water);
      const double K = (K_from + K_to) / 2.0;    // [erg/cm/h/dg C]
      q[e] = K * dT/l + water_heat_capacity * rho_water *  q_water * T;
      // [erg/cm^2/h] = [erg/cm/h/dg C] * [dg C] / [cm] 
      //              + [erg/g/dg C] * [g/cm^3] * [cm/h] * [dg C]
    }
}

double
SoilHeat::suggest_dt (const double T_air) const
{
  if (!enable_ice)
    return NAN;
  const size_t cell_size = T_.size ();
  if (T_air <= 0.0)
    return ice_dt;
  for (size_t c = 0; c < cell_size; c++)
    if (T_[c] <= 0.0)
      return ice_dt;

  return NAN;
}

double
SoilHeat::exptected_T_z0 (const Geometry& geo, const Soil& soil,
			  const SoilWater& soil_water,
			  const double T_bottom,
			  const Movement& movement, const double T_surface, 
			  const double dt, Treelog& msg) const
{
  


  const size_t cell_size = geo.cell_size ();
  const size_t edge_size = geo.edge_size ();

  // Borders.
  double T_top_old = T_limited;
  const double T_top_new = T_surface;
  if (T_top_old < -400.0)
    T_top_old = T_top_new;

  // Edges.
  std::vector<double> q_water (edge_size);
  for (size_t e = 0; e < edge_size; e++)
    q_water[e] = soil_water.q_matrix (e);

  // Cells;
  std::vector<double> conductivity (cell_size);
  std::vector<double> S_water (cell_size);
  std::vector<double> T (cell_size);
  std::vector<double> capacity (cell_size);
  std::vector<double> S_heat (cell_size);
  for (size_t c = 0; c < cell_size; c++)
    {
      // Fixed.
      const double Theta = soil_water.Theta (c);
      const double X_ice = soil_water.X_ice (c);
      conductivity[c] = soil.heat_conductivity (c, Theta, X_ice);
#if 1
      daisy_assert (iszero (soil_water.S_ice_water (c)));
      S_water[c] = soil_water.S_sum (c) - soil_water.S_ice_water (c);
#else
      S_water[c] = 0.0;
#endif
      // Changes with ice state.
      T[c] = this->T (c);
      capacity[c] = this->capacity_apparent (soil, soil_water, c);
      S_heat[c] = this->source (geo, soil_water, c);
    }

  // Solve with old state.
  const std::vector<double> T_old = T;
  movement.heat (q_water, S_water, S_heat, 
		 capacity_, conductivity,
		 T_top_old, T_top_new, T_bottom, T, dt, msg);

  // Calculate flux.
  std::vector<double> q (edge_size, 0.0);
  const double T_prev = (T_top_old + T_top_new) / 2.0;
  calculate_heat_flux (geo, soil, soil_water, T_old, T_prev, T, T_bottom, q);

  return geo.content_hood (T, Geometry::cell_above); // [dg C]
}

void 
SoilHeat::tick (const Geometry& geo, const Soil& soil, SoilWater& soil_water,
		const double T_bottom,
                const Movement& movement, const double T_surface, 
		const double dt, Treelog& msg)
{
  


  const size_t cell_size = geo.cell_size ();
  const size_t edge_size = geo.edge_size ();

  // Update freezing and melting points.
  this->update_freezing_points (soil, soil_water);

  // Borders.
  double T_top_old = T_limited;
  T_limited = limit_T_top (geo, soil, soil_water, T_surface);
  const double T_top_new = T_limited;
  if (T_top_old < -400.0)
    T_top_old = T_top_new;

  // Edges.
  std::vector<double> q_water (edge_size);
  for (size_t e = 0; e < edge_size; e++)
    q_water[e] = soil_water.q_matrix (e);

  // Cells;
  std::vector<double> conductivity (cell_size);
  std::vector<double> S_water (cell_size);
  std::vector<double> T (cell_size);
  std::vector<double> S_heat (cell_size);
  for (size_t c = 0; c < cell_size; c++)
    {
      // Fixed.
      const double Theta = soil_water.Theta (c);
      const double X_ice = soil_water.X_ice (c);
      conductivity[c] = soil.heat_conductivity (c, Theta, X_ice);
#if 1
      daisy_assert (iszero (soil_water.S_ice_water (c)));
      S_water[c] = soil_water.S_sum (c) - soil_water.S_ice_water (c);
#else
      S_water[c] = 0.0;
#endif
      // Changes with ice state.
      T[c] = this->T (c);
      capacity_apparent_[c] = this->capacity_apparent (soil, soil_water, c);
      S_heat[c] = this->source (geo, soil_water, c);
    }

  // Solve with old state.
  const std::vector<double> T_old = T;
  movement.heat (q_water, S_water, S_heat, 
		 capacity_apparent_, conductivity,
		 T_top_old, T_top_new, T_bottom, T, dt, msg);

  // Update ice state according to new temperatures.
  if (this->enable_ice)
    {
      const bool changed 
	= this->update_state (geo, soil, soil_water, T_old, T, dt);

      if (changed)
	{
	  // Solve again with new state.
	  for (size_t c = 0; c < cell_size; c++)
	    {
	      capacity_apparent_[c]
		= this->capacity_apparent (soil, soil_water, c);
	      S_heat[c] = this->source (geo, soil_water, c);
	    }
	  T = T_old;
	  movement.heat (q_water, S_water, S_heat,
			 capacity_apparent_, conductivity,
			 T_top_old, T_top_new, T_bottom, T, dt, msg);

	  // Check if state match new temperatures.
	  const bool changed_again = this->check_state (soil, T);

	  if (changed_again)
	    {
	      // Force temperatures to match state.
	      this->force_state (T);
	    }
	}

      // Update ice in water.
      for (size_t c = 0; c < cell_size; c++)
	soil_water.freeze (soil, c, this->freezing_rate (c));
    }

  // Calculate flux.
  std::vector<double> q (edge_size, 0.0);
  const double T_prev = (T_top_old + T_top_new) / 2.0;
  calculate_heat_flux (geo, soil, soil_water, T_old, T_prev, T, T_bottom, q);

  // Update soil heat.
  for (size_t c = 0; c < cell_size; c++)
    this->set_temperature (c, T[c]);
  for (size_t e = 0; e < edge_size; e++)
    this->set_flux (e, q[e]);
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
SoilHeat::set_temperature (const size_t c, const double value)
{ T_[c] = value; }

void 
SoilHeat::set_flux (const size_t e, const double value)
{ q[e] = value; }

double 
SoilHeat::capacity_new (const Soil& soil, 
                        const SoilWater& soil_water,
                        const size_t i) const
{ 
  const double Theta = soil_water.Theta (i);
  const double X_ice = soil_water.X_ice (i);
  return soil.heat_capacity (i, Theta, X_ice); 
}

double 
SoilHeat::capacity_old (const Soil& soil, 
                        const SoilWater& soil_water,
                        const size_t i) const
{ 
  const double Theta = soil_water.Theta_old (i);
  const double X_ice = soil_water.X_ice_old (i);
  return soil.heat_capacity (i, Theta, X_ice); 
}

double 
SoilHeat::capacity_apparent
/**/ (const Soil& soil, const SoilWater& soil_water, const size_t i) const
{ 
  const double h = soil_water.h (i);
  const double h_ice = soil_water.h_ice (i);
  double cap = capacity_new (soil, soil_water, i);

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
        = std::min (T_thawing[i] - T_thawing_epsilon, 
                    273. *  h / (latent_heat_of_fussion / gravity - h));
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
                        const std::vector<double>& T_old,
                        std::vector<double>& T,
                        const double dt)
{
  // If we don't use ice, there is nothing to do here.
  if (!enable_ice)
    return false;

  const size_t cell_size = T.size ();
  daisy_assert (T_freezing.size () == cell_size);
  daisy_assert (T_thawing.size () == cell_size);
  daisy_assert (freezing_rate_.size () == cell_size);
  daisy_assert (soil.size () == cell_size);
  daisy_assert (state.size () == cell_size);

  bool changed = false;

  for (size_t i = 0; i < cell_size; i++)
    {
      switch (state[i])
        {
        case freezing:
          if (T[i] < T_freezing[i])
            {
              // Find freezing rate.
              freezing_rate_[i] = calculate_freezing_rate (geo, 
							   soil, soil_water, 
							   i, T_old, T, dt);

              if (freezing_rate_[i] < 0.0)
                freezing_rate_[i] = 0.0;

              // Check if there are sufficient water.
              const double Theta_min = soil.Theta (i, h_frozen - 1000.0, 0.0);
#ifdef THETA_RES
              daisy_assert (Theta_min >= soil.Theta_res (i));
#endif
              const double available_water = soil_water.Theta (i) - Theta_min;
              if (freezing_rate_[i] * dt > available_water)
                freezing_rate_[i] = std::max (0.0, available_water / dt);
	      
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
              freezing_rate_[i] = 0.0;
            }
          else
            freezing_rate_[i] = 0.0;
          daisy_assert (-freezing_rate_[i] * dt * rho_water / rho_ice
                        <= soil_water.X_ice_total (i));
          break;
        case frozen:
          freezing_rate_[i] = 0.0;
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
              freezing_rate_[i] = calculate_freezing_rate (geo, 
							   soil, soil_water, 
							   i, T_old, T, dt);
              if (freezing_rate_[i] > 0.0)
                freezing_rate_[i] = 0.0;

              const double X_ice_total = soil_water.X_ice_total (i);
              const double ice_water = X_ice_total * rho_ice / rho_water;
              if (-freezing_rate_[i] * dt >= ice_water)
                {
                  freezing_rate_[i] = -ice_water / dt;
                  daisy_assert (freezing_rate_[i] <= 0.0);

                  state[i] = liquid;
                }
              // We have used the energy.
              T[i] = T_thawing[i];

            }
          else if (T[i] < T_freezing[i])
            {
              state[i] = freezing;
              changed = true;
              freezing_rate_[i] = 0.0;
            }
          else
            freezing_rate_[i] = 0.0;
          daisy_assert (-freezing_rate_[i] * dt * rho_water / rho_ice
                        <= soil_water.X_ice_total (i) * 1.0001);
          break;
        case liquid:
          freezing_rate_[i] = 0.0;
          if (T[i] < T_freezing[i])
            {
              state[i] = freezing;
              changed = true;
            }
          break;
        }
      daisy_assert (-freezing_rate_[i] * dt * rho_water / rho_ice
                    <= soil_water.X_ice_total (i) * 1.0001);
    }
  return changed;
}

double
SoilHeat::calculate_freezing_rate (const Geometry& geo,
                                   const Soil& soil,
                                   const SoilWater& soil_water,
                                   unsigned int i, 
                                   const std::vector<double>& T_old,
                                   const std::vector<double>& T,
                                   const double dt)
{
  const size_t cell_size = geo.cell_size ();
  daisy_assert (i < cell_size);
  daisy_assert (T.size () == cell_size);
  daisy_assert (T_old.size () == cell_size);
  const double T_mean = (T[i] + T_old[i]) / 2.0;
  const double dT = T[i] - T_old[i];

  const size_t edge_size = geo.edge_size ();
  daisy_assert (q.size () == edge_size);

  double dq = 0.0;              // Net flux out.
  const std::vector<size_t>& cell_edges = geo.cell_edges (i);
  for (size_t j = 0; j < cell_edges.size (); j++)
    { 
      const size_t edge = cell_edges[j];
      const int from = geo.edge_from (edge);
      const double area = geo.edge_area (edge);
      if (i == from)
        dq += q[edge] * area;
      else
        dq -= q[edge] * area;
    }
  
  const double vol = geo.cell_volume (i);

  daisy_assert (iszero (soil_water.S_ice_water (i)));
  const double S 
    = soil_water.S_sum (i) - soil_water.S_ice_water (i);
  const double Sh
    = water_heat_capacity * rho_water * S * T_mean;
  const double cap_new = capacity_new (soil, soil_water, i);
  const double cap_old = capacity_old (soil, soil_water, i);
  const double cap = 0.5 * (cap_new + cap_old);
  const double dC = (cap_new - cap_old);
  return (1.0 / (latent_heat_of_fussion * rho_ice))
    * (cap * dT / dt + (dC / dt) * T_mean + dq / vol + Sh);

}

bool
SoilHeat::check_state (const Soil&, 
                       const std::vector<double>& T) const
{
  const size_t cell_size = T.size ();
  daisy_assert (T_freezing.size () == cell_size);
  daisy_assert (T_thawing.size () == cell_size);

  for (size_t i = 0; i < cell_size; i++)
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
  const size_t cell_size = T.size ();
  daisy_assert (T_freezing.size () == cell_size);
  daisy_assert (T_thawing.size () == cell_size);
  for (size_t i = 0; i < T.size (); i++)
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
SoilHeat::freezing_rate (const size_t c) const
{ return freezing_rate_[c]; }

double 
SoilHeat::top_flux (const Geometry& geo,
                    const Soil& soil, const SoilWater& soil_water) const
{
  // BUG: Geometry1D only.
  const double k 
    = soil.heat_conductivity (0, soil_water.Theta (0), soil_water.X_ice (0))
    * 1e-7 * 1e4 / 3600.0;	// erg/h/ cm/ K -> W/m^2/K/cm
  return k * (T (0) - T (1)) / (geo.cell_z (0) - geo.cell_z (1));
}

double 
SoilHeat::top_flux (const Geometry& geo) const
{
  const std::vector<size_t>& edge_above = geo.cell_edges (Geometry::cell_above);
  const size_t edge_above_size = edge_above.size ();
  daisy_assert (edge_above_size > 0U);
  double total_area = 0.0;
  double total_q = 0.0;
  for (size_t i = 0; i < edge_above_size; i++)
    {
      const size_t edge = edge_above[i];
      const double area = geo.edge_area (edge);
      total_area += area;
      daisy_assert (q.size () > edge);
      total_q += area * q[edge];
    }
  total_q /= total_area;
  total_q *= 2.778e-7;          // [erg/cm^2/h] -> [W/m^2]
  return -total_q;		// Positive up.
}

double 
SoilHeat::limit_T_top (const Geometry& geo,
                       const Soil& soil, const SoilWater& soil_water,
                       const double T_surface) const
{
  if (q_lim <= 0.0)
    return T_surface;

  const std::vector<size_t>& edge_above = geo.cell_edges (Geometry::cell_above);
  const size_t edge_above_size = edge_above.size ();
  daisy_assert (edge_above_size > 0U);
  double total_area = 0.0;
  double total_l = 0.0;
  double total_T = 0.0;
  double total_K = 0.0;
  for (size_t i = 0; i < edge_above_size; i++)
    {
      const size_t edge = edge_above[i];
      const int cell = geo.edge_other (edge, Geometry::cell_above);
      const double area = geo.edge_area (edge);
      total_area += area;
      total_l += - area * geo.cell_z (cell);
      daisy_assert (T_.size () > cell);
      total_T += area * T_[cell];
      total_K += area * soil.heat_conductivity (cell, 
                                                soil_water.Theta (cell),
                                                soil_water.X_ice (cell));
    }
  daisy_assert (total_area > 0.0);
  const double dz = total_l / total_area;
  const double T_soil = total_T / total_area;
  const double K = total_K / total_area;
  const double q = K * (T_soil - T_surface) / dz;
#if 0
  std::ostringstream tmp;
  tmp << "q = " << q << "; q_lim = " << q_lim
      << "; dz = " << dz
      << "; T_soil  = " << T_soil
      << "; K = " << K
      << "; T_surface = " << T_surface;
  Assertion::message (tmp.str ());
#endif
  if (std::fabs (q) < q_lim)
    return T_surface;

  const double sign = q / std::fabs (q);
  daisy_assert (std::isfinite (sign));
  daisy_approximate (std::fabs (sign), 1.0);
  const double T_limited = T_soil - sign * q_lim * dz / K;
  const double q_limited = K * (T_soil - T_limited) / dz;
  daisy_approximate (std::fabs (q_limited), q_lim);
  return T_limited;
}

void
SoilHeat::output (Log& log) const
{
  output_value (T_, "T", log); 
  output_value (capacity_, "capacity", log); 
  output_value (capacity_apparent_, "capacity_apparent", log); 
  output_value (conductivity_, "conductivity", log); 
  output_variable (T_limited, log);
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
  return ok;
}

static void
load_T (Frame& frame)
{ Geometry::add_layer (frame, "dg C", Check::none (),
		       Attribute::Const, "Soil temperature."); }

void
SoilHeat::load_syntax (Frame& frame)
{ 
  frame.declare ("q_lim", "erg/cm^2/h", Attribute::Const, "\
Limit surface flux to this value.\n\
Negative for no limit.");
  frame.set ("q_lim",
             75.0 /* [W/m^2] */ / 2.778e-7 /* erg/cm^2/h] -> [W/m^2] */);
  Geometry::add_layer (frame, Attribute::OptionalState, "T", load_T);
  frame.declare ("conductivity", "erg/cm/dg C/h", Attribute::LogOnly, 
                 "Heat conductivity.");
  frame.declare ("capacity", "erg/cm^3/dg C", Attribute::LogOnly, 
                 "Heat capacity.");
  frame.declare ("capacity_apparent", "erg/cm^3/dg C", Attribute::LogOnly, 
                 "Apparent heat capacity, including freezing.");
  frame.declare ("h_frozen", "cm^-1", Attribute::Const,
              "Pressure below which no more water will freeze.");
  frame.set ("h_frozen", -15000.0);
  frame.declare_string ("enable_ice", Attribute::Const,
                        "Set this to 'false' or 'true'");
  static VCheck::Enum ice_check ("false", "true");
  frame.set_check ("enable_ice", ice_check);
  frame.set ("enable_ice", "false");
  frame.declare ("ice_dt", "h", Attribute::Const, 
                 "Timestep to use when ice is enabled.\n\
This applies when there are subzero temperatures in the profile.");
  frame.set ("ice_dt", 3.0 / 60.0);
  frame.declare ("T_limited", "dg C", Attribute::OptionalState, 
                 "Surface temperature limited by 'q_lim'.");
  frame.declare ("T_freezing", "dg C", Attribute::LogOnly, Attribute::SoilCells,
              "Freezing point depression for freezing.");
  frame.declare ("T_thawing", "dg C", Attribute::LogOnly, Attribute::SoilCells,
              "Freezing point depression for thawing.");
  frame.declare ("T_thawing_epsilon", "dg C", Attribute::Const, 
                 "Subtract this from T_thawing for stability.");
  frame.set ("T_thawing_epsilon", 0.001);
  frame.declare ("q", "erg/cm^2/h", Attribute::LogOnly, Attribute::SoilEdges,
              "Heat flux.");
  frame.declare ("state", Attribute::Unknown (), Attribute::LogOnly, Attribute::SoilCells,
              "Current freezing/melting state.");
}

SoilHeat::SoilHeat (const Block& al)
  : q_lim (al.number ("q_lim", -42.42e42)),
    h_frozen (al.number ("h_frozen")),
    enable_ice (al.name ("enable_ice") != symbol ("false")),
    ice_dt (al.number ("ice_dt")),
    T_thawing_epsilon (al.number ("T_thawing_epsilon")),
    T_limited (al.number ("T_limited", -500.0))
{ }

void 
SoilHeat::initialize (const FrameSubmodel& al, const Geometry& geo,
                      const std::vector<double>& default_T,
                      Treelog& msg)
{
  // Fetch initial T.
  geo.initialize_layer (T_, al, "T", msg);

  const size_t cell_size = geo.cell_size ();
  const size_t edge_size = geo.edge_size ();
  capacity_.insert (capacity_.begin (), cell_size, -42.42e42);
  capacity_apparent_.insert (capacity_apparent_.begin (), cell_size, -42.42e42);
  conductivity_.insert (conductivity_.begin (), cell_size, -42.42e42);

  // Freezing point.
  T_freezing.insert (T_freezing.end (), cell_size, 0.0);
  T_thawing.insert (T_thawing.end (), cell_size, 0.0);
  state.insert (state.end (), cell_size, liquid);
  freezing_rate_.insert (freezing_rate_.end (), cell_size, 0.0);
  q.insert (q.end (), edge_size, 0.0);

  for (unsigned int i = 0; i < cell_size; i++)
    {
      if (T_.size () <= i)
        T_.push_back (default_T[i]);

      daisy_assert (T_[i] > -100.0);
      daisy_assert (T_[i] < 50.0);
    }

  // May be larger if user initialized it wrongly.  Checked in "check".
  daisy_assert (T_.size () >= cell_size);

  if (T_limited < -400)
    T_limited = geo.content_hood (*this, &SoilHeat::T, Geometry::cell_above);
}

SoilHeat::~SoilHeat ()
{ }

static DeclareSubmodel 
soil_heat_submodel (SoilHeat::load_syntax, "SoilHeat", "\
Temperature and heat flux in soil.");

// soil_heat.C ends here.
