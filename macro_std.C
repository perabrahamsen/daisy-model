// macro_std.C -- Standard preferential flow model.
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


#include "macro.h"
#include "soil.h"
#include "plf.h"
#include "mathlib.h"
#include "log.h"
#include "uzmodel.h"
#include "check.h"
#include "tmpstream.h"

struct MacroStandard : public Macro
{
  // Parameters.
  const double height_start;	// Height macropores start [cm]
  const double height_end;	// Height macropores end [cm]
  const PLF distribution;		// Where they end [cm ->]
  const double pressure_initiate; // Pressure needed to init pref.flow [cm]
  const double pressure_end;	// Pressure after pref.flow has been init [cm]
  const double pond_max;	// Pond height before activating pref.flow [mm]

  // Simulation.
 void tick (const Soil& soil, unsigned int first, unsigned int last,
	    UZtop& surface,
	    const vector<double>& h_ice,
	    const vector<double>& h,
	    const vector<double>& Theta,
	    vector<double>& S_m,
	    vector<double>& S_p,
	    vector<double>& q_p, Treelog&);
  void output (Log&) const
    { }

  // Create and Destroy.
  MacroStandard (const AttributeList& al)
    : Macro (al),
      height_start (al.number ("height_start")),
      height_end (al.number ("height_end")),
      distribution (al.plf ("distribution")),
      pressure_initiate (al.number ("pressure_initiate")),
      pressure_end (al.number ("pressure_end")),
      pond_max (al.number ("pond_max"))
    { }
  ~MacroStandard ()
    { }
};

void 
MacroStandard::tick (const Soil& soil, 
		     const unsigned int first, const unsigned int last,
		     UZtop& surface,
		     const vector<double>& h_ice,
		     const vector<double>& h,
		     const vector<double>& Theta,
		     vector<double>& S_m,
		     vector<double>& S_p,
		     vector<double>& q_p,
		     Treelog& msg)
{ 
  // Check input.
  assert (last > first);
  assert (last < soil.size ()); 
  assert (h.size () == soil.size ());
  assert (Theta.size () == soil.size ());
  assert (S_m.size () == soil.size ());
  assert (S_p.size () == soil.size ());
  assert (q_p.size () == soil.size () + 1U);

  // Start and end of macro intervals.
  const unsigned int from = max (((int) soil.interval_plus (height_start)) - 1,
				 /* not unsigned, or -1 fails */
				 (int) first);
  const unsigned int to = min (soil.interval_plus (height_end), last);

  // Check if macropores reach surface, and there is ponding there.
  double q_top = 0.0;
  if (height_start >= 0.0  && !surface.flux_top () && surface.h () > 0.0)
    {
      // Empty it.
      surface.flux_top_on ();
      if (-surface.q () * 10.0 * dt > pond_max)
	{
	  q_top = surface.q () + pond_max / 10.0 / dt;
	  assert (q_top < 0.0);
	  assert (q_p[0] == 0.0);
	  assert (from == 0);
	  q_p[0] = q_top;
	  const bool accepted = surface.accept_top (msg, q_p[0]);
	  assert (accepted);
	}
    }

  // End point of layer above.
  double previous_end = (from == 0) ? 0.0 : soil.zplus (from - 1);
  const double last_end = soil.zplus (to);

  // Do the preferential flow.
  for (unsigned int i = from; i < to; i++)
    {
      // The size of the layer.
      const double dz = soil.dz (i); // [cm]
      // The flow into the layer from above.
      double flow = -q_p[i];	// [cm/h] (downwards)

      if (h[i] > pressure_initiate)
	// Do we activate a macropore here?
	{
	  // Add change in water to macropore flow.
	  flow += (Theta[i] - soil.Theta (i, pressure_end, h_ice[i]))
	    * dz / dt;
	}
      else if (flow > 0.0)
	// We might end a macropore here.
	{
	  // Find fraction ending in this layer.
	  const double this_layer 
	    = distribution (soil.zplus (i)) - distribution (previous_end);
	  const double rest
	    = distribution (last_end) - distribution (previous_end);
	  assert (rest > 0.0);
	  assert (this_layer >= 0.0);
	  assert (rest >= this_layer);
	  const double fraction = this_layer / rest;

	  // Sutract it from the flow.
	  flow *= (1.0 - fraction);
	}

      // The flow out through the bottom of the layer.
      q_p[i+1] = -flow;// [cm/h] (upwards)

      // Update sink.
      S_p[i] = (q_p[i] - q_p[i+1]) / dz;
      
      // Update end point of layer above.
      previous_end = soil.zplus (i);
    }

  // Put any remaining preferential flow in the last node.
  if (q_p[to] < 0.0)		// Flow downward.
    {
      S_p[to] = q_p[to] / soil.dz (to);
    }
  q_p[to+1] = 0.0;		// No more flow.

  // Check that the sink terms add up.
  if (fabs (soil.total (S_p) - q_top) > 1.0e-11)
    {
      TmpStream tmp;
      tmp () << __FILE__ << ":" <<  __LINE__
	     << ": BUG: Total S_p = '" << (soil.total (S_p) - q_top)
	     << "' first pass";
      msg.error (tmp.str ());
    }

  // Now check for saturated conditions.
  double extra_water = 0.0;	// [cm]
  for ( /* not unsigned, or the >= fails */ int i = to; i >= (int) from; i--)
    {
      // The size of the layer.
      const double dz = soil.dz (i); // [cm]
      // Saturated water.
      const double Theta_sat = soil.Theta (i, 0.0, h_ice[i]);
      // Expected water content.
      const double Theta_new = Theta[i] - (S_m[i] + S_p[i]) * dt;

      if (Theta_new > Theta_sat)
	// Check that we doesn't oversaturate the sol.
	{
	  // Find the extra water in this layer.
	  const double delta_water = Theta_new - Theta_sat;
	  assert (delta_water > 0.0);

	  // Add extra water to sink (thus removing it from the soil).
	  S_p[i] += delta_water / dt;
	  extra_water += delta_water * dz;

	  // Check that we got it right.
	  assert (approximate (Theta[i] - (S_m[i] + S_p[i]) * dt, Theta_sat));
	}
      else if (extra_water > 0.0)
	// Try to get rid of the extra water.
	{
	  // Unused water storage capacity in this layer.
	  const double delta_water = Theta_sat - Theta_new;

	  if (extra_water < delta_water * dz)
	    // It all fits within this layer.
	    {
	      // Remove extra water from sink (thus adding it to the soil).
	      S_p[i] -= extra_water / dz / dt;
	      extra_water = 0.0;
	      
	      // Check that we got it right.
	      assert (Theta[i] - (S_m[i] + S_p[i]) * dt <= Theta_sat);
	    }
	  else
	    // Otherwise, fill it up.
	    {
	      // Remove delta water from sink (thus adding it to the soil).
	      S_p[i] -= delta_water / dt;
	      extra_water -= delta_water * dz;

	      // Check that we got it right.
	      assert (approximate (Theta[i] - (S_m[i] + S_p[i]) * dt,
				   Theta_sat));
	    }
	}

      // Move the extra water back up, through the macropore.
      q_p[i] += extra_water;
    }
  // Update matrix sink.
  for (unsigned int i = from; i <= to; i++)
    S_m[i] += S_p[i];

  // Check that we got all the extra water stored somewhere.
  if (extra_water != 0.0)
    if (!surface.accept_top (msg, extra_water))
      {
	TmpStream tmp;
	tmp () << "BUG: Surface would not accept " << extra_water 
	       << "mm overflowing water (layer " << from << ")";
	msg.error (tmp.str ());
      }

  // Check that the sink terms add up.
  if (fabs (soil.total (S_p) - q_top) > 1.0e-11)
    {
      TmpStream tmp;
      tmp () << __FILE__ << ":" <<  __LINE__
	     << ": BUG: Total S_p = '" << (soil.total (S_p) - q_top) 
	     << "' second pass";
      msg.error (tmp.str ());
    }
}

static struct MacroStandardSyntax
{
  static Macro&
  make (const AttributeList& al)
    { return *new MacroStandard (al); }
  MacroStandardSyntax ()
    {
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      alist.add ("description", "\
The area between 'height_start' and 'height_end' contains macropores,\n\
which are initiated when the water potential reach 'pressure_initiate',\n\
and then immediately emptied down to 'pressure_end'.  The water entering\n\
the macropore is distributed in soil below as a source term, according\n\
to the 'distribution' parameter.");

      syntax.add ("height_start", "cm", Check::non_positive (), Syntax::Const, 
		  "Macropores starts at this depth (a negative number)");
      syntax.add ("height_end", "cm", Check::non_positive (), Syntax::Const, 
		  "Macropores ends at this depth (a negative number)");
      syntax.add ("distribution", "cm", Syntax::Fraction (), Syntax::Const, "\
Distribution of macropore end points as a function of height.\n\
The function should start with '1' at 'height_end', and then decrease to\n\
'0' at 'height_start'.  It can be constant, but may never increase.\n\
The value indicates the fraction of macropores which ends at the given\n\
where all macropores is assumed to start at the top.");
      syntax.add ("pressure_initiate", "cm", Syntax::Const, 
		  "Pressure needed to init pref.flow");
      syntax.add ("pressure_end", "cm", Syntax::Const, 
		  "Pressure after pref.flow has been init");
      syntax.add ("S_p", "h-1", Syntax::LogOnly,
		  "Macropore sink term.");
      syntax.add ("pond_max", "mm", Check::non_negative (), Syntax::Const, "\
Maximum height of ponding before spilling into macropores.\n\
After macropores are activated pond will havethis height.");
      alist.add ("pond_max", 0.5);
      Librarian<Macro>::add_type ("default", alist, syntax, &make);
    }
} MacroStandard_syntax;
