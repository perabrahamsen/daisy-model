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
#include "soil.h"
#include "soil_heat.h"
#include "log.h"
#include "mathlib.h"

class UZlr : public UZmodel
{
  // Parameters.
private:
  const double h_fc;		// Field Capacity. [cm]
  const double z_top;		// Depth of layer with upwrd water movemnt [cm]

  // Variables.
private:
  double q_up;
  double q_down;

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
  bool flux_bottom () const
    { return true; };
  bool accept_bottom (double)
    { return true; };
  void output (Log&) const;

  // Simulate.
public:
  bool tick (Treelog&, const Soil& soil, const SoilHeat& soil_heat,
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
  UZlr (const AttributeList& par);
  ~UZlr ();
};

bool
UZlr::tick (Treelog&, const Soil& soil, const SoilHeat& soil_heat,
	    unsigned int first, const UZtop& top, 
	    unsigned int last, const UZbottom& /* bottom */, 
	    const vector<double>& S,
	    const vector<double>& h_old,
	    const vector<double>& Theta_old,
	    const vector<double>& h_ice,
	    vector<double>& h,
	    vector<double>& Theta,
	    vector<double>& q)
{
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
	q_up = q[first] = -K_sat;
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

      if (use_darcy && i < to_darcy)
	// Dry earth, near top.  Use darcy to move water up.
	{
	  const double dist = soil.z (i) - soil.z (i+1);
	  q[i+1] = max (K_new * ((h_old[i+1] - h_new) / dist - 1.0), 0.0);
	  if (Theta_new + q[i+1] * dt / dz + 0.00001 > Theta_sat)
	    q[i+1] = (Theta_sat -  Theta_new - 0.00001) * dz / dt;
	  Theta[i] = Theta_new + q[i+1] * dt / dz;
	  h[i] = soil.h (i, Theta[i]);
	}
      else if (h_new < h_fc)
	// Dry earth, no water movement.
	{
	  if (Theta_new > Theta_sat)
	    {
	      q[i+1] = (Theta_sat - Theta_new) * dz / dt;
	      Theta[i] = Theta_sat;
	      h[i] = 0.0;
	    }
	  else
	    {
	      q[i+1] = 0.0;
	      Theta[i] = Theta_new;
	      h[i] = h_new;
	    }
	}
      else
	// Gravitational water movement.
	{
	  // Geometric average K.
	  if (i + 1 < soil.size ())
	    {
	      if (h_ice[i+1] < h_fc) // Blocked by ice.
		K_new = 0.0;
	      else
		K_new = sqrt (K_new * soil.K (i+1, h[i+1], h_ice[i+1],
					      soil_heat.T (i+1)));
	    }

	  daisy_assert (isfinite (h_new));
	  const double Theta_fc = soil.Theta (i, h_fc, h_ice[i]);
	  const double Theta_next = Theta_new - K_new * dt / dz;
	  
	  if (Theta_next < Theta_fc)
	    {
	      q[i+1] = (Theta_fc - Theta_new) * dz / dt;
	      Theta[i] = Theta_fc;
	      h[i] = h_fc;
	    }
	  else if (Theta_next > Theta_sat)
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
	  daisy_assert (q[i+1] < 1e-10);
	}
      daisy_assert (isfinite (h[i]));
      daisy_assert (isfinite (Theta[i]));
      daisy_assert (isfinite (q[i+1]));
      daisy_assert (Theta[i] <= Theta_sat);
      daisy_assert (Theta[i] >= Theta_res);
    }

  // Lower border.
  q_down = q[last + 1];

  // Check mass conservation.
#ifndef _NDEBUG
  double total_old = 0.0;
  double total_new = 0.0;
  double total_S = 0.0;
  for (unsigned int i = first; i <= last; i++)
    {
      total_old += soil.dz (i) * Theta_old[i];
      total_new += soil.dz (i) * Theta[i];
      total_S += soil.dz (i) * S[i] * dt;
    }
  daisy_assert (approximate (total_old - q_up * dt + q_down * dt - total_S * dt, 
		       total_new));
#endif
  return true;
}

void
UZlr::output (Log& log) const
{
  output_variable (q_up, log);
  output_variable (q_down, log);
}

UZlr::UZlr (const AttributeList& al)
  : UZmodel (al),
    h_fc (al.number ("h_fc")),
    z_top (al.number ("z_top")),
    q_up (0.0),
    q_down (0.0)
{ }

UZlr::~UZlr ()
{ }

// Add the UZlr syntax to the syntax table.
static struct UZlrSyntax
{
  static UZmodel& make (const AttributeList& al)
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
      
      // Variables.
      syntax.add ("q_up", "mm/h", Syntax::LogOnly, 
		  "Flux up through the surface.");
      syntax.add ("q_down", "mm/h", Syntax::LogOnly,
		  "Flux up through the bottom of the last node.");

      // Parameters.
      syntax.add ("h_fc", "cm", Syntax::Const, "Field capacity.");
      alist.add ("h_fc", -100.0);
      syntax.add ("z_top", "cm", Syntax::Const, 
		  "Depth of layer where upward water movement is possible.");
      alist.add ("z_top", -10.0);

      Librarian<UZmodel>::add_type ("lr", alist, syntax, &make);
    }
} UZlr_syntax;


