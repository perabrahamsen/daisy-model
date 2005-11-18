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
#include "soil.h"
#include "soil_heat.h"
#include "mathlib.h"
#include "alist.h"
#include "syntax.h"
#include "timestep.h"
#include "log.h"
#include "average.h"
#include <sstream>

using namespace std;

class UZRichard : public UZmodel
{
  // Variables.
private:
  double q_up;
  double q_down;
  int iterations;

  // Parameters.
  int max_time_step_reductions;
  int time_step_reduction;
  int max_iterations;
  double max_absolute_difference;
  double max_relative_difference;
  /* const */ Average* K_average;

  // UZmodel.
public:
  bool flux_top () const
    { return true; };
  double q () const
    { return q_down; }
  void flux_top_on () const
    { }
  void flux_top_off () const
    { }
  bool accept_top (Treelog&, double)
    { return true; };
  bottom_t bottom_type () const
    { return free_drainage; };
  bool accept_bottom (double)
    { return true; };
  void output (Log&) const;

  // Simulate.
private:
  bool richard (Treelog&, const Soil& soil, const SoilHeat& soil_heat,
		int first, const UZtop& top,
		int last, const UZbottom& bottom,
		const vector<double>& S,
		const vector<double>& h_old,
		const vector<double>& Theta_old,
		const vector<double>& h_ice,
		vector<double>& h,
		vector<double>& Theta,
		vector<double>& q);
  bool converges (const vector<double>& previous,
		  const vector<double>& current) const;
  void internode (const Soil&, const SoilHeat&,
		  int first, int last,
		  const vector<double>& h_ice,
		  const vector<double>& K,
		  vector<double>& Kplus) const;
  void q_darcy (const Soil& soil,
		int first, int last,
		const vector<double>& h_previous,
		const vector<double>& h,
		const vector<double>& Theta_previous,
		const vector<double>& Theta,
		const vector<double>& Kplus,
		const vector<double>& S,
		double ddt,
		vector<double>& q);
public:
  bool tick (Treelog&, const Soil& soil, const SoilHeat&,
	     unsigned int first, const UZtop& top,
	     unsigned int last, const UZbottom& bottom,
	     const vector<double>& S,
	     const vector<double>& h_old,
	     const vector<double>& Theta_old,
	     const vector<double>& h_ice,
	     vector<double>& h,
	     vector<double>& Theta,
	     vector<double>& q);

  // Create and Destroy.
public:
  void has_macropores (bool); // Tell UZ that there is macropores.
  UZRichard (Block& par);
  ~UZRichard ();
};

bool
UZRichard::richard (Treelog& msg,
		    const Soil& soil,
		    const SoilHeat& soil_heat,
		    int first, const UZtop& top,
		    const int last, const UZbottom& bottom,
		    const vector<double>& S,
		    const vector<double>& h_old,
		    const vector<double>& Theta_old,
		    const vector<double>& h_ice,
		    vector<double>& h_new,
		    vector<double>& Theta_new,
		    vector<double>& q)
{
  Treelog::Open nest (msg, "UZ Richard");
  // Input variables for solving a tridiagonal matrix.
  const unsigned int size = last - first + 1;
  if (size < 2)
    throw ("Richard's equation need at least two numerical layers");
  vector<double> a (size);
  vector<double> b (size);
  vector<double> c (size);
  vector<double> d (size);

  // Intermeditate results.
  vector<double> h (size);
  vector<double> h_previous (size);
  vector<double> h_conv (size);
  vector<double> Theta_previous (size);
  vector<double> Theta (size);
  vector<double> Ksum (size);
  vector<double> Kold (size);
  vector<double> K (size + 1);
  vector<double> Kplus (size);

  // For h bottom.
  if (bottom.bottom_type () == UZbottom::pressure)
    {
      daisy_assert (last + 1 < soil.size ());
      K[size] = soil.K (last + 1, 0.0, h_ice[last + 1], 
			soil_heat.T (last + 1));
    }
  
  // For lysimeter bottom.
  daisy_assert (q.size () > last);
  const double h_lim = soil.zplus (last) - soil.z (last);
  daisy_assert (h_lim < 0.0);

  // Check when we last switched top.
  double switched_top_last = dt * 2.0;

  // Keep track of water going to the top.
  double top_water = 0.0;
  double available_water;

  if (top.soil_top ())
    {
      available_water
	= (Theta_old[first] 
	   - soil.Theta (first, -20000.0, h_ice[first])) * soil.dz (first);
    }
  else
    {
      available_water = top.h ();

      if (available_water
	  > soil.K (first, 0.0, h_ice[first], soil_heat.T (first)) * dt
	  + (soil.Theta (first, 0.0, h_ice[first]) - Theta_old[first])
	  * soil.dz (first))
	top.flux_top_off ();
    }


  // First guess is the old value.
  copy (h_old.begin () + first, h_old.begin () + last + 1, h.begin ());
  copy (Theta_old.begin () + first, Theta_old.begin () + last + 1,
	Theta.begin ());

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

      if (!top.flux_top () && !top.soil_top ())
	h[first] = top.h () - soil.z (first) + top_water;

      // Bottom flux.
      double q_bottom = 42.42e42;
      daisy_assert (size > 0);
      if (bottom.bottom_type () == UZbottom::lysimeter)
        { 
          if (h[size - 1] > h_lim)
            {
              q_bottom = -Kold[size-1];

#if 0
              const double Theta_diff 
                = soil.Theta (last, h[size - 1], h_ice[last])
                - soil.Theta (last, h_lim - 1.0, h_ice[last]);
              daisy_assert (Theta_diff > 0);
              const double dz = soil.dz (last);
              const double max_K = Theta_diff * dz / ddt;

              if (Kold[size-1] > max_K)
                ddt = max (1e-5, Theta_diff * dz / Kold[size-1]);
#endif
            }
          else 
            q_bottom = 0.0;
        }
      else if (bottom.bottom_type () == UZbottom::forced_flux)
        q_bottom = bottom.q_bottom ();
      else if (bottom.bottom_type () == UZbottom::free_drainage)
        q_bottom = - Kold[size - 1];
      else
        daisy_assert (bottom.bottom_type () == UZbottom::pressure);

      do
	{
	  h_conv = h;
	  iterations_used++;
	  iterations++;

	  // Calculate parameters.
	  for (unsigned int i = 0; i < size; i++)
	    {

	      Ksum[i] += soil.K (first + i, h[i], h_ice[first + i], 
				 soil_heat.T (first + i));
	      K[i] = (Ksum[i] / iterations_used + Kold[i]) / 2.0;
	    }
	  if (bottom.bottom_type () != UZbottom::pressure)
	    K[size] = K[size - 1];

	  internode (soil, soil_heat, first, last, h_ice, K, Kplus);

	  // Calcualte nodes.
	  for (unsigned int i = 0; i < size; i++)
	    {
	      const double Cw1 = soil.Cw1 (first + i, h[i], h_ice[first + i]);
	      // const double Cw2 = max (1e-5, soil.Cw2 (first + i, h[i]));
	      const double Cw2 = soil.Cw2 (first + i, h[i]);
	      const double dz = soil.dz (first + i);
	      const double z = soil.z (first + i);

	      if (i == 0)
		{
		  if (top.flux_top ())
		    {
		      // Calculate upper boundary.
		      const double dz_plus = z - soil.z (first + i + 1);

		      b[i] = Cw2 + (ddt / dz) * (Kplus[i] / dz_plus);
		      d[i] = Theta[i] - Cw1 - ddt * S[first + i]
			+ (ddt / dz)
			* (available_water / time_left - Kplus[i]);

		      // Same as pressure boudnary.
		      a[i] = 0.0;
		      c[i] = - (ddt / dz) * (Kplus[i] / dz_plus);
		    }
		}
	      else if (i == 1 && !top.flux_top ())
		{
		  // Calculate upper boundary.
		  const double dz_plus = z - soil.z (first + i + 1);
		  const double dz_minus = soil.z (first + i - 1) - z;

		  double h_above;
		  if (top.soil_top ())
		    {
		      const double Theta_ddt = Theta_old[first]
			+ top_water / soil.dz (first);
		      h_above = soil.h (first, Theta_ddt);
		    }
		  else
		    {
		      h_above = h[first];
                      daisy_assert (approximate (h_above, 
                                                 top.h () - soil.z (first)
                                                 + top_water));
#if 0
		      if (top.h () < 0.0)
			{
			  std::ostringstream tmp;
			  tmp << "TOP H = " << top.h () << ", H ABOVE = " 
				 << h_above;
			  msg.error (tmp.str ());
			}
#endif
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
		  const double dz_minus = soil.z (first + i - 1) - z;

		  if (bottom.bottom_type () == pressure)
		    {
		      const double dz_plus = z - soil.z (first + i + 1);
		      //const double bottom_pressure = h[i + 1];
		      const double bottom_pressure = h_old[first + i + 1];
		      b[i] = Cw2 + (ddt / dz) * (  Kplus[i - 1] / dz_minus
						+ Kplus[i] / dz_plus);
		      d[i] = Theta[i] - Cw1 - ddt * S[first + i]
			+ (ddt / dz)
			* (Kplus[i - 1]
			   - Kplus[i] * (1.0 -  bottom_pressure/ dz_plus));
		    }
                  else if (bottom.bottom_type () == UZbottom::lysimeter
                           && isnormal (q_bottom))
                    {
                      // Active lysimeter, use fake pressure bottom.
		      const double dz_plus = z - soil.zplus (first + i);
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
		  // Calculate intermediate nodes.
		  const double dz_minus = soil.z (first + i - 1) - z;
		  const double dz_plus = z - soil.z (first + i + 1);

		  a[i] = - (ddt / dz) * (Kplus[i - 1] / dz_minus);
		  b[i] = Cw2 + (ddt / dz) * (  Kplus[i - 1] / dz_minus
					    + Kplus[i] / dz_plus);
		  c[i] = - (ddt / dz) * (Kplus[i] / dz_plus);
		  d[i] = Theta[i] - Cw1 - ddt * S[first + i]
		    +  (ddt / dz) * (Kplus[i - 1] - Kplus[i] );
		}
	    }
	  tridia (top.flux_top () ? 0 : 1, size, a, b, c, d, h.begin ());

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
	      else if (top.flux_top ())
		top.flux_top_off ();
	      else
		top.flux_top_on ();
		
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
	  if (!top.flux_top ())
	    {
	      // Find flux.
	      if (bottom.bottom_type () == UZbottom::forced_flux)
		{
		  q[last + 1] = bottom.q_bottom ();
		  for (int i = last; i >= first; i--)
		    q[i] = - (((Theta[i - first] 
				- Theta_previous[i-first]) / ddt) + S[i])
		      * soil.dz (i) + q[i + 1];
		}
	      else
		q_darcy (soil, first, last, h_previous, h, 
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
		      msg.debug (tmp.str ());
#if 0
		      throw ("Couldn't accept top flux");
#else
                      msg.warning ("Couldn't accept top flux");
                      Theta = Theta_previous;
                      goto try_again;
#endif
		    }
		  else
		    {
		      top.flux_top_on ();
		      accepted = false;
		    }
		}
	    }
	  else if (h[first] <= 0)
	    // We have a flux top, and unsaturated soil.
	    {
              delta_top_water = -(available_water / time_left) * ddt;
	    }
	  else if (top.q () > 0.0)
	    {
	      // We have a saturated soil, with an upward flux.
#if 0
	      throw ("Saturated soil with an upward flux");
#else
              msg.warning ("Saturated soil with an upward flux");
	      Theta = Theta_previous;
              goto try_again;
#endif
              
	    }
	  else if (switched_top_last < time_left + ddt / 2.0)
            {
              std::ostringstream tmp;
              tmp << "last: " << switched_top_last 
                     << "; time left: " << time_left << "; h[" << first 
                     << "] = " << h[first] <<"; q = " << top.q ();
              msg.debug (tmp.str ());
#if 0
              throw ("Couldn't drain top flux");
#else
              msg.warning ("Couldn't drain top flux");
	      Theta = Theta_previous;
              goto try_again;
#endif
            }
	  else
	    // We have saturated soil, make it a pressure top.
	    {
	      top.flux_top_off ();
	      accepted = false;
	    }

	  if (accepted)
	    {

#if 0
	      // This code checks that darcy and the mass preservation
	      // code gives the same results.
	      // Disabled because this is not the case when ice is present.
	      {
		bool error_found = false;
                q[first] = delta_top_water / ddt;
		for (int i = first; i < last; i++)
		  {
		    // Mass preservation.
		    q[i + 1] = (((Theta[i - first] - Theta_previous[i - first]) / ddt)
				+ S[i] * ddt) * soil.dz (i) + q[i];
		    if (h[i - first] >= 0.0 && h[i + 1 - first] >= 0.0)
		      continue;
		    const double darcy
		      = -Kplus[i - first]
		      * ((  (h[i - first] - h[i + 1 - first])
			  / (soil.z (i) - soil.z (i + 1)))
			 + 1);
		    if ((fabs (darcy) > 1.0e-30
			 && fabs (q[i+1] / darcy - 1.0) > 0.10)
                        && fabs (q[i+1] - darcy) > 0.01 / ddt
                        && i<2)
		      {
			error_found = true;
			std::ostringstream tmp;
			tmp << "q[" << (i + 1) << "] = " << q[i+1]
			       << ", darcy = " << darcy
			       << "delta_top_water = " << delta_top_water
			       << " ddt = " << ddt
			       << " top.flux() = " << top.flux_top();
			msg.error (tmp.str ());
		      }
		  }
		if (error_found)
		    throw ("\
Richard eq. mass balance flux is different than darcy flux");
	      }
#endif
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
  copy (h.begin (), h.end (), h_new.begin () + first);
  daisy_assert (Theta_new.size () >= first + size);
  copy (Theta.begin (), Theta.end (), Theta_new.begin () + first);

  // Check upper boundary.
  daisy_assert (top.soil_top () 
	  || approximate (top.h (), available_water - top_water));

#if 0
  q_darcy (soil, first, last, h_old, h_new, Theta_old, Theta_new,
	   Kplus, S, dt, q);
#else
  // We know flux on upper border, use mass preservation to
  // calculate flux below given the change in water content.

  if (top.soil_top ())
    first++;

  q[first] = top_water / dt;
  for (int i = first; i <= last; i++)
    {
      q[i + 1] = (((Theta_new[i] - Theta_old[i]) / dt) + S[i])
	* soil.dz (i) + q[i];
    }
#endif
  return true;
}

bool
UZRichard::converges (const vector<double>& previous,
		      const vector<double>& current) const
{
  size_t size = previous.size ();
  daisy_assert (current.size () == size);

  for (unsigned int i = 0; i < size; i++)
    {
      if (   fabs (current[i] - previous[i]) > max_absolute_difference
	  && (   previous[i] == 0.0
	      || current[i] == 0.0
	      || (  fabs ((current[i] - previous[i]) / previous[i])
		  > max_relative_difference)))
	return false;
    }
  return true;
}

void 
UZRichard::internode (const Soil& soil, const SoilHeat& soil_heat,
		      int first, int last,
		      const vector<double>& h_ice,
		      const vector<double>& K, 
		      vector<double>& Kplus) const
{
  int size = last - first + 1;
  daisy_assert (K_average != NULL);
  for (int i = 0; i < size; i++)
    Kplus[i] = (*K_average)(K[i], K[i + 1]);
  
  for (int i = 0; i < size; i++)
    {
      double Ksat = soil.K (first + i, 0.0, h_ice[first + i], 
			    soil_heat.T (first + i));
      Kplus[i] = min (Ksat, Kplus[i]);
      if (i > 0)
	Kplus[i - 1] = min (Ksat, Kplus[i - 1]);
    }
}

void
UZRichard::q_darcy (const Soil& soil, 
		    const int first, const int last,
		    const vector<double>& /* h_previous */,
		    const vector<double>& h,
		    const vector<double>& Theta_previous,
		    const vector<double>& Theta,
		    const vector<double>& Kplus,
		    const vector<double>& S,
		    const double ddt,
		    vector<double>& q)
{
  // Find an unsaturated area.
  // Start looking 3/4 towards the bottom.
  const double start_pos = (soil.z (first) + soil.z (last) * 3.0) / 4.0;
  int start = soil.interval_plus (start_pos) - 1;
  if (!(start < last - 2))
    {
      std::ostringstream tmp;
      tmp << "We need at least 2 numeric nodes below 3/4 depth for \
calculating flow with pressure top.\n";
      tmp << "3/4 depth is " << start_pos << " [cm]\n"
             << "node " << start << " ends at " << soil.zplus (start) << " [cm]\n"
             << "last " << last << " ends at " << soil.zplus (last) << " [cm]";
      throw (string (tmp.str ()));
    }
  if (!(start > first + 1))
    throw ("We need at least 1 numeric node above 3/4 depth for \
calculating flow with pressure top.");

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
	  / (soil.z (start) - soil.z (start + 1)))
       + 1);
  // Use mass preservation to find flux below and above.
  for (int i = start + 1; i <= last; i++)
    {
      q[i + 1] = (((Theta[i - first] - Theta_previous[i-first]) / ddt) + S[i])
	* soil.dz (i) + q[i];
    }
  for (int i = start; i >= first; i--)
    {
      q[i] = - (((Theta[i - first] - Theta_previous[i-first]) / ddt) + S[i])
	* soil.dz (i) + q[i + 1];
    }
}

bool
UZRichard::tick (Treelog& msg, const Soil& soil, const SoilHeat& soil_heat,
		 unsigned int first, const UZtop& top, 
		 unsigned int last, const UZbottom& bottom, 
		 const vector<double>& S,
		 const vector<double>& h_old,
		 const vector<double>& Theta_old,
		 const vector<double>& h_ice,
		 vector<double>& h,
		 vector<double>& Theta,
		 vector<double>& q)
{
  iterations = 0;
  if (!richard (msg, soil, soil_heat, first, top, last, bottom, 
		S, h_old, Theta_old, h_ice, h, Theta, q))
    throw ("Richard's Equation doesn't converge");

  q_up = q[first];
  q_down = q[last + 1];
  return true;
}

void
UZRichard::output (Log& log) const
{
  output_variable (q_up, log);
  output_variable (q_down, log);
  output_variable (iterations, log);
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
    // Variables.
    q_up (0.0),
    q_down (0.0),
    iterations (0),
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
      syntax.add ("q_up", "mm/h", Syntax::LogOnly, 
		  "Flux up through the surface.");
      syntax.add ("q_down", "mm/h", Syntax::LogOnly,
		  "Flux up through the bottom of the last node.");
      syntax.add ("iterations", Syntax::Integer, Syntax::LogOnly,
		  "Number of iterations used,");
      syntax.add ("K_average", Librarian<Average>::library (),
		  Syntax::OptionalConst, Syntax::Singleton,
		  "Model for calculating average K between nodes.\n\
The default model is 'geometric' if there are macropores, and\n\
'arithmetic' otherwise.");

      Librarian<UZmodel>::add_type ("richards", alist, syntax, &make);
    }
} UZRichard_syntax;


