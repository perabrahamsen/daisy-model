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


#include "uzmodel.h"
#include "groundwater.h"
#include "surface.h"
#include "geometry_vert.h"
#include "soil.h"
#include "soil_heat.h"
#include "mathlib.h"
#include "alist.h"
#include "syntax.h"
#include "timestep.h"
#include "log.h"
#include "average.h"
#include <sstream>

class UZRichard : public UZmodel
{
  // Parameters.
  const int max_time_step_reductions;
  const int time_step_reduction;
  const int max_iterations;
  const double max_absolute_difference;
  const double max_relative_difference;
  /* const */ Average* K_average;

  // Simulate.
private:
  bool richard (Treelog&, const GeometryVert& geo,
                const Soil& soil, const SoilHeat& soil_heat,
		int first, const Surface& top,
                size_t top_edge,
		int last, const Groundwater& bottom,
		const std::vector<double>& S,
		const std::vector<double>& h_old,
		const std::vector<double>& Theta_old,
		const std::vector<double>& h_ice,
		std::vector<double>& h,
		std::vector<double>& Theta,
                size_t q_offset,
		std::vector<double>& q);
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
  bool tick (Treelog&, const GeometryVert& geo,
             const Soil& soil, const SoilHeat&,
	     unsigned int first, const Surface& top,
             size_t top_edge, 
	     unsigned int last, const Groundwater& bottom,
	     const std::vector<double>& S,
	     const std::vector<double>& h_old,
	     const std::vector<double>& Theta_old,
	     const std::vector<double>& h_ice,
	     std::vector<double>& h,
	     std::vector<double>& Theta,
             size_t q_offset, 
	     std::vector<double>& q_base);
  
  // Create and Destroy.
public:
  void has_macropores (bool); // Tell UZ that there is macropores.
  UZRichard (Block& par);
  ~UZRichard ();
  static void load_syntax (Syntax& syntax, AttributeList& alist);
};

bool
UZRichard::richard (Treelog& msg,
		    const GeometryVert& geo,
                    const Soil& soil,
		    const SoilHeat& soil_heat,
		    int first, const Surface& top,
                    const size_t top_edge,
		    const int last, const Groundwater& bottom,
		    const std::vector<double>& S,
		    const std::vector<double>& h_old,
		    const std::vector<double>& Theta_old,
		    const std::vector<double>& h_ice,
		    std::vector<double>& h_new,
		    std::vector<double>& Theta_new,
                    const size_t q_offset,
		    std::vector<double>& q_base)
{
  std::ostringstream tmp;
  tmp << "UZ Richard: " << first << " to " << last;
  Treelog::Open nest (msg, tmp.str ());
  // Input variables for solving a tridiagonal matrix.
  const unsigned int size = last - first + 1;
  const Surface::top_t top_type = top.top_type (geo, top_edge);
  const double h_top = top.h_top (geo, top_edge);
  const double q_top = top.q_top (geo, top_edge);
  const Groundwater::bottom_t bottom_type = bottom.bottom_type ();
  const double q_bottom_forced = (bottom_type == Groundwater::forced_flux)
    ? bottom.q_bottom () : -42.42e42;

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

  // For h bottom.
  if (bottom_type == Groundwater::pressure)
    {
      daisy_assert (last + 1 < geo.cell_size ());
      K[size] = soil.K (last + 1, 0.0, h_ice[last + 1], 
			soil_heat.T (last + 1));
    }
  
  // For lysimeter bottom.
  const double h_lim = geo.zplus (last) - geo.z (last);
  daisy_assert (h_lim < 0.0);

  // Check when we last switched top.
  double switched_top_last = dt * 2.0;

  // Keep track of water going to the top.
  double top_water = 0.0;
  double available_water;
  bool flux = true;

  if (top_type == Surface::soil)
    {
      available_water
	= (Theta_old[first] 
	   - soil.Theta (first, -20000.0, h_ice[first])) * geo.dz (first);
    }
  else
    {
      available_water = h_top;

      if (available_water
	  > soil.K (first, 0.0, h_ice[first], soil_heat.T (first)) * dt
	  + (soil.Theta (first, 0.0, h_ice[first]) - Theta_old[first])
	  * geo.dz (first))
	flux = false;
    }

  // First guess is the old value.
  copy (&h_old[first], &h_old[last + 1], h.begin ());
  copy (&Theta_old[first], &Theta_old[last + 1], Theta.begin ());

  double time_left = dt;	// How much of the large time step left.
  double ddt = dt;		// We start with small == large time step.
  int number_of_time_step_reductions = 0;
  int iterations_with_this_time_step = 0;

  while (time_left > 0.0)
    {
      // Initialization for each small time step.
      int iterations_used = 0;
      if (ddt > time_left)
	ddt = time_left;

      for (unsigned int i = 0; i < size; i++)
	{
	  Ksum[i] = 0.0;
	  Kold[i] = soil.K (first + i, h[i], h_ice[first + i], 
			    soil_heat.T (first + i));
	}
      h_previous = h;
      Theta_previous = Theta;

      if (!flux && top_type != Surface::soil)
	h[0] = h_top - geo.z (first) + top_water;

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
	  h_conv = h;
	  iterations_used++;

	  // Calculate parameters.
	  for (unsigned int i = 0; i < size; i++)
	    {

	      Ksum[i] += soil.K (first + i, h[i], h_ice[first + i], 
				 soil_heat.T (first + i));
	      K[i] = (Ksum[i] / iterations_used + Kold[i]) / 2.0;
	    }
	  if (bottom_type != Groundwater::pressure)
	    K[size] = K[size - 1];

	  internode (soil, soil_heat, first, last, h_ice, K, Kplus);

	  // Calcualte cells.
	  for (unsigned int i = 0; i < size; i++)
	    {
	      const double Cw1 = soil.Cw1 (first + i, h[i], h_ice[first + i]);
	      // const double Cw2 = max (1e-5, soil.Cw2 (first + i, h[i]));
	      const double Cw2 = soil.Cw2 (first + i, h[i]);
	      const double dz = geo.dz (first + i);
	      const double z = geo.z (first + i);

	      if (i == 0)
		{
		  if (flux)
		    {
		      // Calculate upper boundary.
		      const double dz_plus = z - geo.z (first + i + 1);

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
		  const double dz_plus = z - geo.z (first + i + 1);
		  const double dz_minus = geo.z (first + i - 1) - z;

		  double h_above;
		  if (top_type == Surface::soil)
		    {
		      const double Theta_ddt = Theta_old[first]
			+ top_water / geo.dz (first);
		      h_above = soil.h (first, Theta_ddt);
		    }
		  else
		    {
		      h_above = h[0];
                      daisy_assert (approximate (h_above, 
                                                 h_top
                                                 - geo.z (first)
                                                 + top_water));
		    }
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
		  const double dz_minus = geo.z (first + i - 1) - z;

		  if (bottom_type == Groundwater::pressure)
		    {
		      const double dz_plus = z - geo.z (first + i + 1);
		      //const double bottom_pressure = h[i + 1];
		      const double bottom_pressure = h_old[first + i + 1];
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
		  const double dz_minus = geo.z (first + i - 1) - z;
		  const double dz_plus = z - geo.z (first + i + 1);

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
	      std::ostringstream tmp;
	      tmp << "ABSURD: h[0] = " << h[0] << " h[1] = " << h[1] 
		     << " h[" << (size-1) << "] = " << h[size-1]
		     << " stepping down";
	      msg.error (tmp.str ());
	      iterations_used = max_iterations + 42;
	      break;
	    }
	}
      while (   !converges (h_conv, h)
	     && iterations_used <= max_iterations);

      if (iterations_used > max_iterations)
	{
        try_again:
	  number_of_time_step_reductions++;

	  if (number_of_time_step_reductions > max_time_step_reductions)
	    {
	      if (approximate (switched_top_last, time_left))
		return false;
	      else 
                flux = !flux;
		
	      switched_top_last = time_left;
	      ddt = time_left;
	      number_of_time_step_reductions = 0;
	    }
	  else
	    ddt /= time_step_reduction;
	  h = h_previous;
	}
      else
	{
	  // Calculate new water content.
	  for (unsigned int i = 0; i < size; i++)
	    Theta[i] = soil.Theta(first + i, h[i], h_ice[first + i]);

	  bool accepted = true;	// Could the top accept the results?
	  // Amount of water we put into the top this small time step.
	  double delta_top_water = 88.0e88;
	  if (!flux)
	    {
	      // Find flux.
	      if (bottom_type == Groundwater::forced_flux)
		{
		  q[last + 1] = bottom.q_bottom ();
		  for (int i = last; i >= first; i--)
		    q[i] = - (((Theta[i - first] 
				- Theta_previous[i-first]) / ddt) + S[i])
		      * geo.dz (i) + q[i + 1];
		}
	      else
		q_darcy (geo, first, last, h_previous, h, 
			 Theta_previous, Theta, Kplus, S, ddt, q);

	      // We take water from flux pond first.
	      delta_top_water = q[first] * ddt;

	      if (available_water + delta_top_water < -1e-30)
		// We don't have more water in the pressure top.
		{
		  if (approximate (switched_top_last, time_left))
		    {
		      std::ostringstream tmp;
		      tmp << "available_water = " << available_water 
			     << ", delta_top_water = " << delta_top_water
			     << ", time left = " << time_left;
		      msg.warning (tmp.str ());
                      msg.warning ("Couldn't accept top flux");
                      Theta = Theta_previous;
                      goto try_again;
		    }
		  else
		    {
		      flux = true;
		      accepted = false;
		    }
		}
	    }
	  else if (h[0] <= 0)
	    // We have a flux top, and unsaturated soil.
	    {
              delta_top_water = -(available_water / time_left) * ddt;
	    }
	  else if (q_top > 0.0)
	    {
	      // We have a saturated soil, with an upward flux.
	      throw ("Saturated soil with an upward flux");
	    }
	  else if (switched_top_last < time_left + ddt / 2.0)
            {
              std::ostringstream tmp;
              tmp << "last: " << switched_top_last 
                  << "; time left: " << time_left << "; h[" << first 
                  << "] = " << h[first] <<"; q = " << q_top;
              msg.warning (tmp.str ());
              msg.warning ("Couldn't drain top flux");
	      Theta = Theta_previous;
              goto try_again;
            }
	  else
	    // We have saturated soil, make it a pressure top.
	    {
	      flux = false;
	      accepted = false;
	    }

	  if (accepted)
	    {
	      top_water += delta_top_water;
              available_water += delta_top_water;
	      time_left -= ddt;
	      iterations_with_this_time_step++;

	      if (iterations_with_this_time_step > time_step_reduction)
		{
		  number_of_time_step_reductions--;
		  iterations_with_this_time_step = 0;
		  ddt *= time_step_reduction;
		}
	    }
	  else
	    {
	      switched_top_last = time_left;
	      Theta = Theta_previous;
	      h = h_previous;
	    }
	}
    }

  // Make it official.
  daisy_assert (h_new.size () >= first + size);
  copy (h.begin (), h.end (), &h_new[first]);
  daisy_assert (Theta_new.size () >= first + size);
  copy (Theta.begin (), Theta.end (), &Theta_new[first]);

  // Check upper boundary.
  daisy_assert (top_type == Surface::soil
                || approximate (h_top,
                                available_water - top_water));

  // We know flux on upper border, use mass preservation to
  // calculate flux below given the change in water content.

  if (top_type == Surface::soil)
    first++;

  q[first] = top_water / dt;
  for (int i = first; i <= last; i++)
    {
      q[i + 1] = (((Theta_new[i] - Theta_old[i]) / dt) + S[i])
	* geo.dz (i) + q[i];
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
      if (   fabs (current[i] - previous[i]) > max_absolute_difference
	  && (   !std::isnormal (previous[i])
              || !std::isnormal (current[i])
	      || (  fabs ((current[i] - previous[i]) / previous[i])
		  > max_relative_difference)))
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
  daisy_assert (K_average != NULL);
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
  const double start_pos = geo.z (first) 
    + (geo.z (last) - geo.z (first)) * 3.0 / 4.0;
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

  for (; start > 0; start--)
    {
      if (h[start - first] < 0.0 && h[start + 1 - first] < 0.0)
	break;
    }
  if (start == 0)
    throw ("We couldn't find an unsaturated area.");
  // Use Darcy equation to find flux here.
  q[start + 1] = -Kplus[start - first] 
    * (  (  (h[start - first] - h[start + 1 - first])
	  / (geo.z (start) - geo.z (start + 1)))
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

bool
UZRichard::tick (Treelog& msg, const GeometryVert& geo,
                 const Soil& soil, const SoilHeat& soil_heat,
		 const unsigned int first, const Surface& top, 
                 const size_t top_edge,
		 const unsigned int last, const Groundwater& bottom, 
		 const std::vector<double>& S,
		 const std::vector<double>& h_old,
		 const std::vector<double>& Theta_old,
		 const std::vector<double>& h_ice,
		 std::vector<double>& h,
		 std::vector<double>& Theta,
                 const size_t q_offset,
		 std::vector<double>& q_base)
{
  if (!richard (msg, geo, soil, soil_heat, first, top, top_edge, last, bottom, 
		S, h_old, Theta_old, h_ice, h, Theta, q_offset, q_base))
    throw ("Richard's equation doesn't converge");

  return true;
}

void
UZRichard::has_macropores (bool has_them)
{ 
  if (K_average == NULL)
    {
      if (has_them)
	{
	  static AttributeList geometric;
	  if (!geometric.check ("type"))
	    geometric.add ("type", "geometric");
	  K_average = Librarian<Average>::build_free (Treelog::null (),
						      geometric, "has macro");
	}
      else
	{
	  static AttributeList arithmetic;
	  if (!arithmetic.check ("type"))
	    arithmetic.add ("type", "arithmetic");
	  K_average = Librarian<Average>::build_free (Treelog::null (),
						      arithmetic, "no macro");
	}
    }
}

UZRichard::UZRichard (Block& al)
  : UZmodel (al),
    // Parameters.
    max_time_step_reductions (al.integer ("max_time_step_reductions")),
    time_step_reduction (al.integer ("time_step_reduction")),
    max_iterations (al.integer ("max_iterations")),
    max_absolute_difference (al.number ("max_absolute_difference")),
    max_relative_difference (al.number ("max_relative_difference")),
    K_average (al.check ("K_average")
	       ? Librarian<Average>::build_item (al, "K_average")
	       : NULL)
{ }

UZRichard::~UZRichard ()
{ 
  delete K_average;
}

void 
UZRichard::load_syntax (Syntax& syntax, AttributeList& alist)
{
  syntax.add ("max_time_step_reductions",
              Syntax::Integer, Syntax::Const, "\
Number of times we may reduce the time step before giving up");
  alist.add ("max_time_step_reductions", 4);
  syntax.add ("time_step_reduction", Syntax::Integer, Syntax::Const, 
              "Divide the time step with this at each reduction.");
  alist.add ("time_step_reduction", 4);
  syntax.add ("max_iterations", Syntax::Integer, Syntax::Const, "\
Maximum number of iterations when seeking convergence before reducing\n\
the time step.");
  alist.add ("max_iterations", 25);
  syntax.add ("max_absolute_difference", "cm", Syntax::Const, "\
Maximum absolute difference in 'h' values for convergence.");
  alist.add ("max_absolute_difference", 0.02);
  syntax.add ("max_relative_difference", Syntax::None (), Syntax::Const, "\
Maximum relative difference in 'h' values for convergence.");
  alist.add ("max_relative_difference", 0.001);
  syntax.add ("K_average", Librarian<Average>::library (),
              Syntax::OptionalConst, Syntax::Singleton,
              "Model for calculating average K between cells.\n\
The default model is 'geometric' if there are macropores, and\n\
'arithmetic' otherwise.");
}

const AttributeList& 
UZmodel::default_model ()
{
  static AttributeList alist;
  
  if (!alist.check ("type"))
    {
      Syntax dummy;
      UZRichard::load_syntax (dummy, alist);
      alist.add ("type", "richards");

    }
  return alist;
}

// Add the UZRichard syntax to the syntax table.
static struct UZRichardSyntax
{
  static UZmodel& make (Block& al)
    {
      return *new UZRichard (al);
    }

  UZRichardSyntax ()
    {
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      alist.add ("description", "A numerical solution to Richard's Equation.");
      UZRichard::load_syntax (syntax, alist);
      Librarian<UZmodel>::add_type ("richards", alist, syntax, &make);
    }
} UZRichard_syntax;


