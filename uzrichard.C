// uzrichard.C --- Using Richard's Equation to calculate water flow.
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

#define BUILD_DLL

#include "uzmodel.h"
#include "block_model.h"
#include "groundwater.h"
#include "surface.h"
#include "geometry_vert.h"
#include "soil.h"
#include "soil_heat.h"
#include "mathlib.h"
#include "frame.h"
#include "log.h"
#include "average.h"
#include "librarian.h"
#include "treelog.h"
#include <sstream>
#include <memory>

class UZRichard : public UZmodel
{
  // Parameters.
  const int debug;
  const int max_time_step_reductions;
  const int time_step_reduction;
  const int max_iterations;
  const int max_number_of_small_time_steps;
  const int msg_number_of_small_time_steps;
  const bool require_positive_convergence;
  const double max_absolute_difference;
  const double max_relative_difference;
  std::unique_ptr<const Average> K_average;

  // Simulate.
private:
  bool richard (Treelog&, const GeometryVert& geo,
                const Soil& soil, const SoilHeat& soil_heat,
		int first, const Surface& top, size_t top_edge,
		int last, const Groundwater& bottom, size_t bottom_edge,
		const std::vector<double>& S,
		const std::vector<double>& h_old,
		const std::vector<double>& Theta_old,
		const std::vector<double>& h_ice,
		std::vector<double>& h,
		std::vector<double>& Theta,
                size_t q_offset,
		std::vector<double>& q, double dt);
  bool converges (const std::vector<double>& previous,
		  const std::vector<double>& current) const;
  void internode (const Soil&, const SoilHeat&,
		  int first, int last,
		  const std::vector<double>& h_ice,
		  const std::vector<double>& K,
		  std::vector<double>& Kplus) const;
  void q_darcy (const GeometryVert& geo,
		int first, int last,
		const std::vector<double>& h_previous,
		const std::vector<double>& h,
		const std::vector<double>& Theta_previous,
		const std::vector<double>& Theta,
		const std::vector<double>& Kplus,
		const std::vector<double>& S,
		double ddt,
		double *const q);
public:
  void tick (Treelog&, const GeometryVert& geo,
             const Soil& soil, const SoilHeat&,
	     unsigned int first, const Surface& top,
             size_t top_edge, 
	     unsigned int last, const Groundwater& bottom,
             const size_t bottom_edge,
	     const std::vector<double>& S,
	     const std::vector<double>& h_old,
	     const std::vector<double>& Theta_old,
	     const std::vector<double>& h_ice,
	     std::vector<double>& h,
	     std::vector<double>& Theta,
             size_t q_offset, 
	     std::vector<double>& q_base, 
             double dt);
  
  // Create and Destroy.
public:
  void has_macropores (bool); // Tell UZ that there is macropores.
  UZRichard (const BlockModel& par);
  ~UZRichard ();
};

bool
UZRichard::richard (Treelog& msg,
		    const GeometryVert& geo,
                    const Soil& soil,
		    const SoilHeat& soil_heat,
		    /* const */ int first, const Surface& top,
                    const size_t top_edge,
		    const int last, const Groundwater& bottom,
                    const size_t bottom_edge,
		    const std::vector<double>& S,
		    const std::vector<double>& h_old,
		    const std::vector<double>& Theta_old,
		    const std::vector<double>& h_ice,
		    std::vector<double>& h_new,
		    std::vector<double>& Theta_new,
                    const size_t q_offset,
		    std::vector<double>& q_base,
                    const double dt)
{
  std::ostringstream tmp;
  tmp << "UZ Richard: " << first << " to " << last;
  Treelog::Open nest (msg, tmp.str ());
  // Input variables for solving a tridiagonal matrix.
  const unsigned int size = last - first + 1;
  const double h_top = top.h_top (geo, top_edge);
  daisy_assert (h_top < 1000.0);
  daisy_assert (h_top > -1000.0);
  const double q_top = top.q_top (geo, top_edge, dt);
  daisy_approximate (h_top, -q_top * dt);
  const Groundwater::bottom_t bottom_type = bottom.bottom_type ();
  const double q_bottom_forced = (bottom_type == Groundwater::forced_flux)
    ? bottom.q_bottom (bottom_edge) : -42.42e42;

  // Find relevant fluxes.
  daisy_assert (geo.edge_to (top_edge) == Geometry::cell_above);
  daisy_assert (first + q_offset >= top_edge);
  daisy_assert (last + q_offset + 1 < geo.edge_size ());
  daisy_assert (q_base.size () > q_offset + last + 1);
  double *const q = &q_base[q_offset];

  if (size < 2)
    throw ("Richard's equation need at least two numerical layers");
  std::vector<double> a (size);
  std::vector<double> b (size);
  std::vector<double> c (size);
  std::vector<double> d (size);

  // Intermeditate results.
  std::vector<double> h (size);
  std::vector<double> h_previous (size);
  std::vector<double> h_conv (size);
  std::vector<double> Theta_previous (size);
  std::vector<double> Theta (size);
  std::vector<double> Ksum (size);
  std::vector<double> Kold (size);
  std::vector<double> K (size + 1);
  std::vector<double> Kplus (size);

  // For lysimeter bottom.
  const double h_lim = geo.zplus (last) - geo.cell_z (last);
  daisy_assert (h_lim < 0.0);

  // Keep track of water going to the top.
  double top_water = 0.0;
  double available_water = h_top;

  // First guess is the old value.
  std::copy (h_old.begin () + first, h_old.begin () + last + 1, h.begin ());
  std::copy (Theta_old.begin () + first, Theta_old.begin () + last + 1,
	     Theta.begin ());

  // If top node is unsaturated, we start with a flux boundary.
  bool flux = h[0] < 0.0;

#if 0
      if (available_water
	  > soil.K (first, 0.0, h_ice[first], soil_heat.T (first)) * dt
	  + (soil.Theta (first, 0.0, h_ice[first]) - Theta_old[first])
	  * geo.dz (first))
	flux = false;
#endif

  bool switched_top = false;  // Switched top this timestep?
  double time_left = dt;	// How much of the large time step left.
  double ddt = dt;		// We start with small == large time step.
  int number_of_time_step_reductions = 0;
  int iterations_with_this_time_step = 0;
  int n_small_time_steps = 0;

  // We switch to pressure top of flux top leave top node saturated. Once.
  bool allow_saturated_flux = false;
  
  while (time_left > 0.0)
    {
      // Initialization for each small time step.
      int iterations_used = 0;
      if (ddt > time_left)
	ddt = time_left;

      std::unique_ptr<Treelog::Open> nest;

      if (n_small_time_steps > 0
          && (n_small_time_steps%msg_number_of_small_time_steps) == 0)
        {
          std::ostringstream tmp_ddt;
          tmp_ddt << "Time t = " << (dt - time_left) 
                  << "; ddt = " << ddt
                  << "; steps " << n_small_time_steps 
                  << "; time left = " << time_left;
          nest.reset (new Treelog::Open (msg, tmp_ddt.str ()));
          msg.touch ();
          msg.flush ();
        }
      
      n_small_time_steps++;
      if (n_small_time_steps > max_number_of_small_time_steps) 
        {
          msg.debug ("Too many small timesteps");
          throw "Too many small timesteps";
        }

      for (unsigned int i = 0; i < size; i++)
	{
	  Ksum[i] = 0.0;
	  Kold[i] = soil.K (first + i, h[i], h_ice[first + i], 
			    soil_heat.T (first + i));
	}
      h_previous = h;
      Theta_previous = Theta;

      if (!flux)
	h[0] = h_top - geo.cell_z (first) + top_water;

      // Amount of water we put into the top this small time step.
      double delta_top_water = 88.0e88;

      // Bottom flux.
      double q_bottom = 42.42e42;
      daisy_assert (size > 0);
      switch (bottom_type)
        {
        case Groundwater::lysimeter:
          if (h[size - 1] > h_lim)
            q_bottom = -Kold[size-1];
          else 
            q_bottom = 0.0;
          break;
        case Groundwater::forced_flux:
          q_bottom = q_bottom_forced;
          break;
        case Groundwater::free_drainage:
          q_bottom = - Kold[size - 1];
          break;
        case Groundwater::pressure:
          break;
        default:
          daisy_panic ("Unknown bottom type");
        }

      do
	{
          if (++iterations_used > max_iterations)
            {
              if (debug > 1)
                {
		  std::ostringstream tmp;
                  tmp << "Too many iterations: "
		      << "ddt = " << ddt
		      << ", available_water = " << available_water 
		      << ", time left = " << time_left;
                  if (debug > 2)
                    {
                      tmp << "\nh_previous =";
                      for (size_t c = 0; c < size; c++)
                        tmp << "\t" << h_previous[c];
                      tmp << "\nh =";
                      for (size_t c = 0; c < size; c++)
                        tmp << "\t" << h[c];
                      tmp << "\nh_conv =";
                      for (size_t c = 0; c < size; c++)
                        tmp << "\t" << h_conv[c];
                      tmp << "\nS =";
                      for (size_t c = 0; c < size; c++)
                        tmp << "\t" << S[first + c];
                      tmp << "\nK =";
                      for (size_t c = 0; c < size; c++)
                        tmp << "\t" << K[c];
                      tmp << "\nTheta =";
                      for (size_t c = 0; c < size; c++)
                        tmp << "\t" << Theta[c];
                    }
		  msg.message (tmp.str ());
                }
              goto failure;
            }

	  h_conv = h;

	  // Calculate parameters.
	  for (unsigned int i = 0; i < size; i++)
	    {

	      Ksum[i] += soil.K (first + i, h[i], h_ice[first + i], 
				 soil_heat.T (first + i));
	      K[i] = (Ksum[i] / iterations_used + Kold[i]) / 2.0;
	    }
          K[size] = K[size - 1];
	  internode (soil, soil_heat, first, last, h_ice, K, Kplus);

	  // Calculate cells.
	  for (unsigned int i = 0; i < size; i++)
	    {
	      const double Cw1 = soil.Cw1 (first + i, h[i], h_ice[first + i]);
	      // const double Cw2 = max (1e-5, soil.Cw2 (first + i, h[i]));
	      const double Cw2 = soil.Cw2 (first + i, h[i]);
	      const double dz = geo.dz (first + i);
	      const double z = geo.cell_z (first + i);

	      if (i == 0)
		{
		  if (flux)
		    {
		      // Calculate upper boundary.
		      const double dz_plus = z - geo.cell_z (first + i + 1);

		      b[i] = Cw2 + (ddt / dz) * (Kplus[i] / dz_plus);
		      d[i] = Theta[i] - Cw1 - ddt * S[first + i]
			+ (ddt / dz)
			* (available_water / time_left - Kplus[i]);

		      // Same as pressure boudnary.
		      a[i] = 0.0;
		      c[i] = - (ddt / dz) * (Kplus[i] / dz_plus);
		    }
		}
	      else if (i == 1 && !flux)
		{
		  // Calculate upper boundary.
		  const double dz_plus = z - geo.cell_z (first + i + 1);
		  const double dz_minus = geo.cell_z (first + i - 1) - z;

		  double h_above = h[0];
		  daisy_assert (approximate (h_above, 
					     h_top
					     - geo.cell_z (first)
					     + top_water));
		  b[i] = Cw2
		    + (ddt / dz) * (Kplus[i - 1] / dz_minus
                                    + Kplus[i] / dz_plus);
		  d[i] = Theta[i] - Cw1 - ddt * S[first + i]
		    + (ddt / dz)
		    * (Kplus[i - 1] * (1 + h_above / dz_minus) - Kplus[i]);
		  a[i] = 0.0;
		  c[i] = - (ddt / dz) * (Kplus[i] / dz_plus);
		}
	      else if (i == size - 1)
		{
		  // Calculate lower boundary
		  const double dz_minus = geo.cell_z (first + i - 1) - z;

		  if (bottom_type == Groundwater::pressure)
		    {
                      const double z_bottom = geo.zplus (first + i);
		      const double dz_plus = z - z_bottom;
		      const double bottom_pressure = bottom.table () - z_bottom;
		      b[i] = Cw2 + (ddt / dz) * (  Kplus[i - 1] / dz_minus
						+ Kplus[i] / dz_plus);
		      d[i] = Theta[i] - Cw1 - ddt * S[first + i]
			+ (ddt / dz)
			* (Kplus[i - 1]
			   - Kplus[i] * (1.0 -  bottom_pressure/ dz_plus));
		    }
                  else if (bottom_type == Groundwater::lysimeter
                           && std::isnormal (q_bottom))
                    {
                      // Active lysimeter, use fake pressure bottom.
		      const double dz_plus = z - geo.zplus (first + i);
                      const double K_sat = soil.K (first + i, 0.0, 
                                                   h_ice[first + i], 
                                                   soil_heat.T (first + i));
                      b[i] = Cw2 + (ddt / dz) * (  Kplus[i - 1] / dz_minus
						+ K_sat / dz_plus);
		      d[i] = Theta[i] - Cw1 - ddt * S[first + i]
			+ (ddt / dz) * (Kplus[i - 1] - K_sat);
                    }
                  else
                    {
                      // Flux bottom.
		      b[i] = Cw2 + (ddt / dz) * (Kplus[i - 1] / dz_minus);
		      d[i] = Theta[i] - Cw1 - ddt * S[first + i]
			+ (ddt / dz) * (Kplus[i - 1] + q_bottom);
		    }

		  a[i] = - (ddt / dz) * (Kplus[i - 1] / dz_minus);
		  c[i] = 0.0;
		}
	      else
		{
		  // Calculate intermediate cells.
		  const double dz_minus = geo.cell_z (first + i - 1) - z;
		  const double dz_plus = z - geo.cell_z (first + i + 1);

		  a[i] = - (ddt / dz) * (Kplus[i - 1] / dz_minus);
		  b[i] = Cw2 + (ddt / dz) * (  Kplus[i - 1] / dz_minus
					    + Kplus[i] / dz_plus);
		  c[i] = - (ddt / dz) * (Kplus[i] / dz_plus);
		  d[i] = Theta[i] - Cw1 - ddt * S[first + i]
		    +  (ddt / dz) * (Kplus[i - 1] - Kplus[i] );
		}
	    }
	  tridia (flux ? 0 : 1, size, a, b, c, d, h.begin ());

          daisy_assert (h.size () > 1);
	  if (h[0] < -1e9 || h[1] < -1e9 || h[size-1] < -1e9)
	    {
              if (debug > 0)
                {
                  std::ostringstream tmp;
                  tmp << "ABSURD: h[0] = " << h[0] << " h[1] = " << h[1] 
                      << " h[" << (size-1) << "] = " << h[size-1];
                  msg.message (tmp.str ());
                }
              goto failure;
	    }
	}
      while (!converges (h_conv, h));
      
      // Calculate new water content.
      for (unsigned int i = 0; i < size; i++)
        Theta[i] = soil.Theta (first + i, h[i], h_ice[first + i]);

      if (flux)
        {
          if (h[0] > 0.0)
            {
              if (debug > 1)
                msg.message ("Flux top with saturated soil");

	      if (switched_top)
		{
		  if (debug > 1)
		    msg.message ("Already tried pressure top, live with it");
		  allow_saturated_flux = true;
		}
	      else if (!allow_saturated_flux)
		goto switch_top;
            }

          // Yeah, it worked!
          delta_top_water = -(available_water / time_left) * ddt;
        }
      else 
        {
          daisy_assert (!flux);

          // Find flux.
          switch (bottom_type)
            {
            case Groundwater::forced_flux:
            case Groundwater::free_drainage:
              q[last + 1] = q_bottom;
              for (int i = last; i >= first; i--)
                q[i] = - (((Theta[i - first] 
                            - Theta_previous[i-first]) / ddt) + S[i])
                  * geo.dz (i) + q[i + 1];
              break;
            case Groundwater::pressure:
            case Groundwater::lysimeter:
              q_darcy (geo, first, last, h_previous, h, 
                       Theta_previous, Theta, Kplus, S, ddt, q);
              break;
            default:
              daisy_panic ("Unknown bottom type");
            }

          // We take water from flux pond first.
          delta_top_water = q[first] * ddt;

          if (available_water + delta_top_water < -1e-30)
            // We don't have more water in the pressure top.
            {
              if (debug > 1)
                {
                  std::ostringstream tmp;
                  tmp << "Couldn't accept top flux: "
                      << "available_water = " << available_water 
                      << ", delta_top_water = " << delta_top_water
                      << ", time left = " << time_left
                      << ", ddt = " << ddt;
                  if (debug > 2)
                    {
                      tmp << "\nh =";
                      for (size_t c = 0; c < size; c++)
                        tmp << "\t" << h[c];
                      tmp << "\nTheta =";
                      for (size_t c = 0; c < size; c++)
                        tmp << "\t" << Theta[c];
                      tmp << "\nS =";
                      for (size_t c = 0; c < size; c++)
                        tmp << "\t" << S[first + c];
                      tmp << "\nK =";
                      for (size_t c = 0; c < size; c++)
                        tmp << "\t" << K[c];
                    }
                  msg.message (tmp.str ());
                }
              goto switch_top;
            }
          // Yeah, it works!
        }

      // Acceptance mode.
      top_water += delta_top_water;
      available_water += delta_top_water;
      time_left -= ddt;
      switched_top = false;
      iterations_with_this_time_step++;

      if (debug > 1)
	{
	  std::ostringstream tmp;
	  tmp << "Succes; ddt = " << ddt << "; time left " << time_left
	      << "; type = " << (flux ? "flux" : "pressure")
	      << "; iterations = " << iterations_used;
	  msg.message (tmp.str ());
	}
      
      if (iterations_with_this_time_step > time_step_reduction)
        {
          number_of_time_step_reductions--;
          iterations_with_this_time_step = 0;
          ddt *= time_step_reduction;
	  if (debug > 1)
	    {
	      std::ostringstream tmp;
	      tmp << "Increasing timestep to " << ddt;
	      msg.message (tmp.str ());
	    }
        }
      continue;

      // Convergence problems.
    failure:
      if (switched_top)
	goto reduce_timestep;
      if (flux && available_water <= 0.0)
	goto reduce_timestep;
      
      // Switch top mode.
    switch_top:
      if (switched_top)
	goto reduce_timestep;

      flux = !flux;
      switched_top = true;

      if (debug > 1)
	{
	  std::ostringstream tmp;
	  if (flux)
	    tmp << "Trying flux top";
	  else
	    tmp << "Trying pressure top";
	  if (debug > 2)
	    {
	      tmp << "\nh =";
	      for (size_t c = 0; c < size; c++)
		tmp << "\t" << h_previous[c];
	      tmp << "\nTheta =";
	      for (size_t c = 0; c < size; c++)
		tmp << "\t" << Theta[c];
	      tmp << "\nS =";
	      for (size_t c = 0; c < size; c++)
		tmp << "\t" << S[first + c];
	      tmp << "\nK =";
	      for (size_t c = 0; c < size; c++)
		tmp << "\t" << K[c];
	    }
	  msg.message (tmp.str ());
	}
      goto recover_state;
      
      // Reduce timestep.
    reduce_timestep:
      if (++number_of_time_step_reductions > max_time_step_reductions)
        {
	  if (debug > 1)
	    msg.message ("Too many timestep reductions");

	  return false;
        }
      ddt /= time_step_reduction;
      switched_top = false;
      
      if (debug > 1)
	{
	  std::ostringstream tmp;
	  tmp << "Reducing timestep to " << ddt;
	  msg.message (tmp.str ());
	}
      
    recover_state:
      Theta = Theta_previous;
      h = h_previous;
    }

  // Make it official.
  daisy_assert (h_new.size () >= first + size);
  copy (h.begin (), h.end (), &h_new[first]);
  daisy_assert (Theta_new.size () >= first + size);
  copy (Theta.begin (), Theta.end (), &Theta_new[first]);

  // Check upper boundary.
  daisy_assert (h_top < 1000.0);
  daisy_assert (h_top > -1000.0);

  if (!balance (h_top, available_water, top_water))
    {
      std::ostringstream tmp;
      tmp << "h_top = " << h_top << ", available_water = " << available_water
          << ", top_water = " << top_water;
      msg.error (tmp.str ());
    }

  // We know flux on upper border, use mass preservation to
  // calculate flux below given the change in water content.

  q[first] = top_water / dt;
  for (int i = first; i <= last; i++)
    {
      q[i + 1] = (((Theta_new[i] - Theta_old[i]) / dt) + S[i])
	* geo.dz (i) + q[i];
    }

  for (unsigned int i = 0; i < size; i++)
    if (!std::isfinite (h[i]))
      {
        std::ostringstream tmp;
        tmp << "h[" << i << "] = " << h[i];
        msg.error (tmp.str ());
        return false;
      }

  return true;
}

bool
UZRichard::converges (const std::vector<double>& previous,
		      const std::vector<double>& current) const
{
  size_t size = previous.size ();
  daisy_assert (current.size () == size);

  for (unsigned int i = 0; i < size; i++)
    {
      const double absolute_difference = std::fabs (current[i] - previous[i]);
      if (absolute_difference < max_absolute_difference)
	// Small absolute difference, OK.
	continue;

      const double abs_previous = fabs (previous[i]);
      if (abs_previous < 0.1)
	// Pressure close to zero, OK.
	continue;

      const double relative_difference = absolute_difference / abs_previous;
      if (relative_difference < max_relative_difference)
	// Small relative difference, OK.
	continue;

      if (require_positive_convergence)
	// We have no exception for positive pressure, NOT OK.
	return false;

      if (previous[i] >= 0.0 && current[i] >= 0.0)
	// Positive always OK.
	continue;

      return false;
    }
  return true;
}

void 
UZRichard::internode (const Soil& soil, const SoilHeat& soil_heat,
		      int first, int last,
		      const std::vector<double>& h_ice,
		      const std::vector<double>& K, 
		      std::vector<double>& Kplus) const
{
  int size = last - first + 1;
  daisy_assert (K_average.get ());
  for (int i = 0; i < size; i++)
    Kplus[i] = (*K_average)(K[i], K[i + 1]);
  
  for (int i = 0; i < size; i++)
    {
      double Ksat = soil.K (first + i, 0.0, h_ice[first + i], 
			    soil_heat.T (first + i));
      Kplus[i] = std::min (Ksat, Kplus[i]);
      if (i > 0)
	Kplus[i - 1] = std::min (Ksat, Kplus[i - 1]);
    }
}

void
UZRichard::q_darcy (const GeometryVert& geo,
		    const int first, const int last,
		    const std::vector<double>& /* h_previous */,
		    const std::vector<double>& h,
		    const std::vector<double>& Theta_previous,
		    const std::vector<double>& Theta,
		    const std::vector<double>& Kplus,
		    const std::vector<double>& S,
		    const double ddt,
		    double *const q)
{
  // Find an unsaturated area.
  // Start looking 3/4 towards the bottom.
  const double start_pos = geo.cell_z (first) 
    + (geo.cell_z (last) - geo.cell_z (first)) * 3.0 / 4.0;
  int start = first; 
  while (start < last && geo.zplus (start + 1) > start_pos)
    start++;
  if (!(start < last - 2))
    {
      std::ostringstream tmp;
      tmp << "We need at least 2 numeric cells below 3/4 depth for \
calculating flow with pressure top.\n";
      tmp << "3/4 depth is " << start_pos << " [cm]\n"
          << "cell " << start << " ends at " << geo.zplus (start) << " [cm]\n"
          << "first " << first << " ends at " << geo.zplus (first) << " [cm]\n"
          << "last " << last << " ends at " << geo.zplus (last) << " [cm]";
      throw (tmp.str ());
    }
  if (!(start > first + 1))
    {
      std::ostringstream tmp;
      tmp << "We need at least 1 numeric cell above 3/4 depth for \
calculating flow with pressure top.\n";
      tmp << "3/4 depth is " << start_pos << " [cm]\n"
          << "cell " << start << " ends at " << geo.zplus (start) << " [cm]\n"
          << "first " << first << " ends at " << geo.zplus (first) << " [cm]\n"
          << "last " << last << " ends at " << geo.zplus (last) << " [cm]";
      throw (tmp.str ());
    }
  for (; start > first; start--)
    {
      if (h[start - first] < 0.0 && h[start + 1 - first] < 0.0)
	break;
    }
#ifdef REQUIRE_UNSATURATED_DARCY
  if (start == first)
    throw ("We couldn't find an unsaturated area.");
#endif // REQUIRE_UNSATURATED_DARCY

  // Use Darcy equation to find flux here.
  q[start + 1] = -Kplus[start - first] 
    * (  (  (h[start - first] - h[start + 1 - first])
	  / (geo.cell_z (start) - geo.cell_z (start + 1)))
       + 1);
  // Use mass preservation to find flux below and above.
  for (int i = start + 1; i <= last; i++)
    {
      q[i + 1] = (((Theta[i - first] - Theta_previous[i-first]) / ddt) + S[i])
	* geo.dz (i) + q[i];
    }
  for (int i = start; i >= first; i--)
    {
      q[i] = - (((Theta[i - first] - Theta_previous[i-first]) / ddt) + S[i])
	* geo.dz (i) + q[i + 1];
    }
}

void
UZRichard::tick (Treelog& msg, const GeometryVert& geo,
                 const Soil& soil, const SoilHeat& soil_heat,
                 unsigned int first, const Surface& top, 
                 const size_t top_edge,
		 const unsigned int last, const Groundwater& bottom, 
                 const size_t bottom_edge,
		 const std::vector<double>& S,
		 const std::vector<double>& h_old,
		 const std::vector<double>& Theta_old,
		 const std::vector<double>& h_ice,
		 std::vector<double>& h,
		 std::vector<double>& Theta,
                 const size_t q_offset,
		 std::vector<double>& q_base,
                 const double dt)
{
  if (!richard (msg, geo, soil, soil_heat,
                first, top, top_edge, last, bottom, bottom_edge,
		S, h_old, Theta_old, h_ice, h, Theta, q_offset, q_base, dt))
    throw "Richard's equation doesn't converge";
}

void
UZRichard::has_macropores (const bool has_them)
{ 
  if (K_average.get ())
    return;

  if (has_them)
    K_average = Average::build_geometric ();
  else
    K_average = Average::build_arithmetic ();

  daisy_assert (K_average.get ());
}

UZRichard::UZRichard (const BlockModel& al)
  : UZmodel (al),
    // Parameters.
    debug (al.integer ("debug")),
    max_time_step_reductions (al.integer ("max_time_step_reductions")),
    time_step_reduction (al.integer ("time_step_reduction")),
    max_iterations (al.integer ("max_iterations")),
    max_number_of_small_time_steps (al.integer ("max_number_of_small_time_steps")),
    msg_number_of_small_time_steps (al.integer ("msg_number_of_small_time_steps")),
    require_positive_convergence (al.flag ("require_positive_convergence")),
    max_absolute_difference (al.number ("max_absolute_difference")),
    max_relative_difference (al.number ("max_relative_difference")),
    K_average (al.check ("K_average")
	       ? Librarian::build_item<Average> (al, "K_average")
	       : NULL)
{ }

UZRichard::~UZRichard ()
{ }

// Add the UZRichard syntax to the syntax table.
static struct UZRichardSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new UZRichard (al); }

  UZRichardSyntax ()
    : DeclareModel (UZmodel::component, "richards", "\
A numerical solution to Richard's Equation.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare_integer ("debug", Attribute::Const, "\
Print additional debug messages, higher numbers means more messages.");
    frame.set ("debug", 0);
    frame.declare_integer ("max_time_step_reductions", Attribute::Const, "\
Number of times we may reduce the time step before giving up");
    frame.set ("max_time_step_reductions", 16);
    frame.declare_integer ("time_step_reduction", Attribute::Const, 
               "Divide the time step with this at each reduction.");
    frame.set ("time_step_reduction", 4);
    frame.declare_integer ("max_iterations", Attribute::Const, "\
Maximum number of iterations when seeking convergence before reducing\n\
the time step.");
    frame.set ("max_iterations", 25);
    frame.declare_integer ("max_number_of_small_time_steps", Attribute::Const, "\
Maximum number of small time steps in a large time step.");
    frame.set ("max_number_of_small_time_steps", 200000);  
    frame.declare_integer ("msg_number_of_small_time_steps", Attribute::Const, "\
Number of small time steps in a large time step between message.");
    frame.set ("msg_number_of_small_time_steps", 5000);
    frame.declare_boolean ("require_positive_convergence", Attribute::Const, "\
True if convergense criteria is also used for saturated cells.\n\
Setting this to false may speed up simulation, but give worse estimate\n\
of pressure and flow in saturated soil.");
    frame.set ("require_positive_convergence", true);
    frame.declare ("max_absolute_difference", "cm", Attribute::Const, "\
Maximum absolute difference in 'h' values for convergence.");
    frame.set ("max_absolute_difference", 0.02);
    frame.declare ("max_relative_difference", Attribute::None (), Attribute::Const, "\
Maximum relative difference in 'h' values for convergence.");
    frame.set ("max_relative_difference", 0.001);
    frame.declare_object ("K_average", Average::component,
                      Attribute::OptionalConst, Attribute::Singleton,
                      "Model for calculating average K between cells.\n\
The default model is 'geometric' if there are macropores, and\n\
'arithmetic' otherwise.");
  }
} UZRichard_syntax;

// uzrichards.C ends here.
