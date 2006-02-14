// uzlr.C --- using linear reservoirs to calculate water flow.
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
#include "surface.h"
#include "groundwater.h"
#include "soil.h"
#include "soil_heat.h"
#include "mathlib.h"
#include <sstream>

using namespace std;

class UZlr : public UZmodel
{
  // Parameters.
private:
  const double h_fc;		// Field Capacity. [cm]
  const double z_top;		// Depth of layer with upw. water movement [cm]

  // Simulate.
public:
  bool tick (Treelog&, const Soil& soil, const SoilHeat& soil_heat,
	     unsigned int first, const Surface& top, 
	     unsigned int last, const Groundwater& bottom, 
	     const vector<double>& S,
	     const vector<double>& h_old,
	     const vector<double>& Theta_old,
	     const vector<double>& h_ice,
	     vector<double>& h,
	     vector<double>& Theta,
	     vector<double>& q);
  void output (Log&) const
  { }

  // Create and Destroy.
  void has_macropores (bool)
  { }
public:
  UZlr (Block& par);
  ~UZlr ();
};

bool
UZlr::tick (Treelog& msg, const Soil& soil, const SoilHeat& soil_heat,
	    unsigned int first, const Surface& top, 
	    unsigned int last, const Groundwater& bottom, 
	    const vector<double>& S,
	    const vector<double>& h_old,
	    const vector<double>& Theta_old,
	    const vector<double>& h_ice,
	    vector<double>& h,
	    vector<double>& Theta,
	    vector<double>& q)
{
  double q_up = 0.0;
  double q_down = 0.0;

  if (top.soil_top ())
    {
      daisy_assert (!top.flux_top ());
      // We have a forced pressure top, in the form of a ridge system.
      // Since LR only works with flux top, we use Darcy to simulate a
      // flux top between the first node (with a forced pressure) and
      // the second node, and then continue calculating with a flux
      // top from the second node.
      const double dz = soil.z (first) - soil.z (first+1);
      const double dh = (h_old[first] - h_old[first+1]);
      const double K = min (soil.K (first, h_old[first], h_ice[first],
				    soil_heat.T (first)),
			    soil.K (first, h_old[first+1], h_ice[first+1],
				    soil_heat.T (first+1)));
      q_up = -K * (dh/dz + 1.0);

      // We can safely ignore S[first], since the ridge system has
      // already incorporated it.
      first++;

      // New upper limit.
      q[first] = q_up;
    }
  else
    {
      // Limit flux by soil capacity.
      const double K_sat = soil.K (first, 0.0, h_ice[first], 
				   soil_heat.T (first));
      daisy_assert (K_sat > 0.0);

      // Make sure it is a flux top.
      top.flux_top_on ();

      if (!top.flux_top ())
	// It refuses to be a flux, must be a lake.
	{
	  const double dz = 0.0 - soil.z (first);
	  const double dh = top.h () - h_old[first];
	  q_up = q[first] = -K_sat * (dh/dz + 1.0);
	}
      else
	q_up = q[first] = max (top.q (), -K_sat);
    }

  //  Use darcy for upward movement in the top.
  const bool use_darcy = (h_old[first] < h_fc) && (q_up > 0.0);
  const int to_darcy = max (soil.interval_plus (z_top), first + 5);

  // Intermediate nodes.
  for (int i = first; i <= last; i++)
    {
      const double dz = soil.dz (i);
      const double Theta_sat = soil.Theta (i, 0.0, h_ice[i]);
      const double Theta_res = soil.Theta_res (i);
      const double Theta_new = Theta_old[i] - q[i] * dt / dz - S[i] * dt;
      daisy_assert (Theta_new >= Theta_res);
      // daisy_assert (Theta_new <= Theta_sat);
      const double h_new = soil.h (i, Theta_new);
      double K_new = soil.K (i, h_new, h_ice[i], soil_heat.T (i));

      // If we have free drainage bottom, we go for field capacity all
      // the way.  Otherwise, we assume groundwater start at the
      // bottom of the last node, and attempt equilibrium from there.
      // This asumption is correct for lysimeter bottom, adequate for
      // pressure bottom (where groundwater table is in the last
      // node), and wrong for forced flux (= pipe drained soil) where
      // the groundwater is usually much higher.  Still, it is better
      // than using h_fc.
      const double h_lim = (bottom.bottom_type ()
                            == Groundwater::free_drainage) 
        ? h_fc
        : max (soil.zplus (last) - soil.z (i), h_fc);
      daisy_assert (h_lim < 0.0);

      if (use_darcy && i < to_darcy)
	// Dry earth, near top.  Use darcy to move water up.
	{
	  const double dist = soil.z (i) - soil.z (i+1);
	  q[i+1] = max (K_new * ((h_old[i+1] - h_new) / dist - 1.0), 0.0);

	  if (Theta_new + q[i+1] * dt / dz > Theta_sat)
	    {
	      q[i+1] = (Theta_sat -  Theta_new) * dz / dt;
	      Theta[i] = Theta_sat;
	      h[i] = 0.0;
	    }
	  else
	    {
	      Theta[i] = Theta_new + q[i+1] * dt / dz;
	      h[i] = soil.h (i, Theta[i]);
	    }
	}
      else if (h_new <= h_lim)
	// Dry earth, no water movement.
	{
	  daisy_assert (Theta_new <= Theta_sat);
	  q[i+1] = 0.0;
	  Theta[i] = Theta_new;
	  h[i] = h_new;
	}
      else
	// Gravitational water movement.
	{
	  if (i + 1 < soil.size ())
	    {
	      // Geometric average K.
	      if (h_ice[i+1] < h_fc) // Blocked by ice.
		K_new = 0.0;
	      else
		K_new = sqrt (K_new * soil.K (i+1, h_old[i+1], h_ice[i+1],
					      soil_heat.T (i+1)));
	    }
	  else if (bottom.bottom_type () == Groundwater::forced_flux)
	    K_new = -bottom.q_bottom ();
          
	  const double Theta_lim = soil.Theta (i, h_lim, h_ice[i]);
	  const double Theta_next = Theta_new - K_new * dt / dz;

	  if (Theta_next < Theta_lim)
	    {
              if (Theta_lim < Theta_new)
                {
                  q[i+1] = (Theta_lim - Theta_new) * dz / dt;
                  Theta[i] = Theta_lim;
                  h[i] = h_lim;
                }
              else
                {
                  std::ostringstream tmp;
                  tmp << "BUG: h_new = " << h_new << " h_lim = " << h_lim;
                  msg.error (tmp.str ());
                  q[i+1] = 0.0;
                  Theta[i] = Theta_new;
                  h[i] = h_new;
                }
	    }
	  else if (Theta_next >= Theta_sat)
	    {
	      q[i+1] = (Theta_sat - Theta_new) * dz / dt;
	      Theta[i] = Theta_sat;
	      h[i] = 0.0;
	    }
	  else
	    {
	      q[i+1] = -K_new;
	      Theta[i] = Theta_next;
	      h[i] = soil.h (i, Theta[i]);
	    }
	}
      daisy_assert (isfinite (h[i]));
      daisy_assert (isfinite (Theta[i]));
      daisy_assert (isfinite (q[i+1]));
      daisy_assert (Theta[i] <= Theta_sat);
      daisy_assert (Theta[i] >= Theta_res);
    }

  // Lower border.
  q_down = q[last + 1];

#if 1
  if (bottom.bottom_type () == Groundwater::forced_flux
      && !approximate (q_down, bottom.q_bottom ()))
    {
      // Ensure forced bottom.
      double extra_water = (bottom.q_bottom () - q_down) * dt;
      for (int i = last; isnormal (extra_water); i--)
	{
	  q[i+1] += extra_water / dt;
	  if (i < 0)
	    // Take it from ponding.
	    break;
	  const double dz = soil.dz (i);
	  const double Theta_sat = soil.Theta (i, 0.0, h_ice[i]);
	  Theta[i] += extra_water / dz;
	  if (Theta[i] <= Theta_sat)
	    {
	      extra_water = 0;
	      h[i] = soil.h (i, Theta[i]);
	    }
	  else
	    {
	      extra_water = (Theta[i] - Theta_sat) * dz;
	      Theta[i] = Theta_sat;
	      h[i] = 0.0;
	    }
	}
      q_up = q[first] + extra_water / dt;
      q_down = q[last + 1];
    }
#else
  double extra_water = 0.0;
  if (bottom.type () == Groundwater::forced_flux
      && !approximate (q_down, bottom.q_bottom ()))
      // Ensure forced bottom.
      extra_water = (bottom.q_bottom () - q_down) * dt;

  for (int i = last; i >= int (first); i--)
    {
      const double dz = soil.dz (i);

      if (isnormal (extra_water))
	{
	  // Add extra water from bottom to this layer.
	  q[i+1] += extra_water / dt;
      
	  // Does it fit?
	  const double Theta_sat = soil.Theta (i, 0.0, h_ice[i]);
	  Theta[i] += extra_water / dz;
	  if (Theta[i] <= Theta_sat)
	    {
	      extra_water = 0;
	      h[i] = soil.h (i, Theta[i]);
	    }
	  else
	    {
	      extra_water = (Theta[i] - Theta_sat) * dz;
	      Theta[i] = Theta_sat;
	      h[i] = 0.0;
	    }
	  daisy_assert (extra_water >= 0.0);
	}      
#if 1
      // Can it pass down this fast?
      const double K_this = soil.K (i, 0.0, h_ice[i], soil_heat.T (i));
      const double K_above = (i > 0) 
	? soil.K (i-1, 0.0, h_ice[i-1], soil_heat.T (i-1))
	: K_this;
      const double K = max (K_this, K_above);
      const double conductivity_extra_water = (q[i] < -K * 1.01)
	? -(q[i] + K) * dt 
	: 0.0;
      daisy_assert (conductivity_extra_water >= 0.0);

      // Check that extra water is enough.
      if (extra_water < conductivity_extra_water)
	{
	  // We get less water from above.
	  Theta[i] -= (conductivity_extra_water - extra_water) / dz;
	  extra_water = conductivity_extra_water;

	  // Did we get too little water?
	  const double Theta_res = soil.Theta_res (i);
	  if (Theta[i] < Theta_res)
	    {
	      // We substracted to much, get it from above.
	      extra_water = conductivity_extra_water 
		- (Theta_res - Theta[i]) * dz;
	      Theta[i] = Theta_res;
	    }
	  h[i] = soil.h (i, Theta[i]);
	}
      daisy_assert (extra_water >= 0.0);
#endif
    }
  q_up = q[first] + extra_water / dt;
  q_down = q[last + 1];
#endif
  // Check mass conservation.
  double total_old = 0.0;
  double total_new = 0.0;
  double total_S = 0.0;
  for (unsigned int i = first; i <= last; i++)
    {
      total_old += soil.dz (i) * Theta_old[i];
      total_new += soil.dz (i) * Theta[i];
      total_S += soil.dz (i) * S[i] * dt;
    }
  daisy_assert (approximate (total_old + (-q_up + q_down - total_S) * dt, 
			     total_new));
  return true;
}

UZlr::UZlr (Block& al)
  : UZmodel (al),
    h_fc (al.number ("h_fc")),
    z_top (al.number ("z_top"))
{ }

UZlr::~UZlr ()
{ }

// Add the UZlr syntax to the syntax table.
static struct UZlrSyntax
{
  static UZmodel& make (Block& al)
    {
      return *new UZlr (al);
    }

  UZlrSyntax ()
    {
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      alist.add ("description", "\
Use gravitational water movement for wet soil, where h > h_fc.\n\
There are no water movement when h < h_fc, except at the layers down\n\
to z_top, where there can be darcy movement.");

      // Parameters.
      syntax.add ("h_fc", "cm", Syntax::Const, "Field capacity.");
      alist.add ("h_fc", -100.0);
      syntax.add ("z_top", "cm", Syntax::Const, 
		  "Depth of layer where upward water movement is possible.");
      alist.add ("z_top", -10.0);

      Librarian<UZmodel>::add_type ("lr", alist, syntax, &make);
    }
} UZlr_syntax;


