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
#include "block.h"
#include "geometry1d.h"
#include "soil.h"
#include "surface.h"
#include "plf.h"
#include "mathlib.h"
#include "log.h"
#include "uzmodel.h"
#include "check.h"
#include "vcheck.h"
#include <sstream>

using namespace std;

struct MacroStandard : public Macro
{
  // Parameters.
  const PLF distribution;		// Where they end [cm ->]
  const double height_start;	// Height macropores start [cm]
  const double height_end;	// Height macropores end [cm]
  const double pressure_initiate; // Pressure needed to init pref.flow [cm]
  const double pressure_end;	// Pressure after pref.flow has been init [cm]
  const double pond_max;	// Pond height before activating pref.flow [mm]

  // Simulation.
 void tick (const Geometry1D& geo,
            const Soil& soil, unsigned int first, unsigned int last,
	    Surface& surface,
	    const vector<double>& h_ice,
	    const vector<double>& h,
	    const vector<double>& Theta,
	    vector<double>& S_m,
	    vector<double>& S_p,
	    vector<double>& q_p, double dt, Treelog&);
  void output (Log&) const
    { }

  // Create and Destroy.
  static bool check_alist (const AttributeList& al, Treelog& err);
  static void load_syntax (Syntax& syntax, AttributeList& alist);
  MacroStandard (Block& al)
    : Macro (al),
      distribution (al.plf ("distribution")),
      height_start (al.check ("height_start") 
		    ? al.number ("height_start")
		    : distribution.x (distribution.size () - 1)),
      height_end (al.check ("height_end")
		  ? al.number ("height_end")
		  : distribution.x (0)),
      pressure_initiate (al.number ("pressure_initiate")),
      pressure_end (al.number ("pressure_end")),
      pond_max (al.number ("pond_max"))
    { }
  ~MacroStandard ()
    { }
};

void 
MacroStandard::tick (const Geometry1D& geo,
                     const Soil& soil, 
		     const unsigned int first, const unsigned int last,
		     Surface& surface,
		     const vector<double>& h_ice,
		     const vector<double>& h,
		     const vector<double>& Theta,
		     vector<double>& S_m,
		     vector<double>& S_p,
		     vector<double>& q_p,
		     const double dt, 
                     Treelog& msg)
{ 
  // Check input.
  daisy_assert (last > first);
  daisy_assert (last < geo.cell_size ()); 
  daisy_assert (h.size () == geo.cell_size ());
  daisy_assert (Theta.size () == geo.cell_size ());
  daisy_assert (S_m.size () == geo.cell_size ());
  daisy_assert (S_p.size () == geo.cell_size ());
  daisy_assert (q_p.size () == geo.edge_size ());

  // Check for macropores outside our soil.
  if (height_start < geo.z (last))
    return;
  const double soil_end = geo.zplus (geo.cell_size () - 1);

  // Start and end of macro intervals.
  const unsigned int from = max (double2int (geo.interval_plus 
					     (height_start)) - 1,
				 /* not unsigned, or -1 fails */
				 double2int (first));
  const unsigned int to 
    = min (geo.interval_plus (max (height_end, soil_end)), last);

  // Check if macropores reach surface, and there is ponding there.
  double q_top = 0.0;
  if (height_start >= 0.0
      && surface.top_type (geo, 0U) == Surface::limited_water)
    {
      const double surface_q = surface.q_top (geo, 0U, dt);
      // Empty it.
      if (-surface_q * 10.0 * dt > pond_max)
	{
	  q_top = surface_q + pond_max / 10.0 / dt;
	  daisy_assert (q_top < 0.0);
	  daisy_assert (iszero (q_p[0]));
	  daisy_assert (from == 0);
	  q_p[0] = q_top;
	  surface.accept_top (q_p[0], geo, 0U, dt, msg);
	}
    }

  // End point of layer above.
  double previous_end = (from == 0) ? 0.0 : geo.zplus (from - 1);
  const double last_end = geo.zplus (to);

  // Do the preferential flow.
  for (unsigned int i = from; i < to; i++)
    {
      // The size of the layer.
      const double dz = geo.dz (i); // [cm]
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
	  /* const */ double this_layer 
	    = distribution (geo.zplus (i)) - distribution (previous_end);
	  const double rest
	    = distribution (last_end) - distribution (previous_end);
	  daisy_assert (rest > 0.0);
	  daisy_assert (this_layer >= 0.0);
	  daisy_assert (rest >= this_layer);
	  const double fraction = this_layer / rest;
	  // Sutract it from the flow.
	  flow *= (1.0 - fraction);
	}

      // The flow out through the bottom of the layer.
      q_p[i+1] = -flow;// [cm/h] (upwards)

      // Update sink.
      S_p[i] = (q_p[i] - q_p[i+1]) / dz;
      
      // Update end point of layer above.
      previous_end = geo.zplus (i);
    }

  // Put any remaining preferential flow in the last cell.
  if (q_p[to] < 0.0)		// Flow downward.
    {
      S_p[to] = q_p[to] / geo.dz (to);
    }
  q_p[to+1] = 0.0;		// No more flow.

  // Check that the sink terms add up.
  if (fabs (geo.total_surface (S_p) - q_top) > 1.0e-11)
    {
      std::ostringstream tmp;
      tmp << __FILE__ << ":" <<  __LINE__
	     << ": BUG: Total S_p = '" << (geo.total_surface (S_p) - q_top)
	     << "' first pass";
      msg.error (tmp.str ());
    }

  // Now check for saturated conditions.
  double extra_water = 0.0;	// [cm]
  for ( /* not unsigned, or the >= fails */ int i = to; 
					    i >= double2int (from);
					    i--)
    {
      // The size of the layer.
      const double dz = geo.dz (i); // [cm]
      // Saturated water.
      const double Theta_sat = soil.Theta (i, 0.0, h_ice[i]);
      // Expected water content.
      const double Theta_new = Theta[i] - (S_m[i] + S_p[i]) * dt;

      if (Theta_new > Theta_sat)
	// Check that we doesn't oversaturate the sol.
	{
	  // Find the extra water in this layer.
	  const double delta_water = Theta_new - Theta_sat;
	  daisy_assert (delta_water > 0.0);

	  // Add extra water to sink (thus removing it from the soil).
	  S_p[i] += delta_water / dt;
	  extra_water += delta_water * dz;

	  // Check that we got it right.
	  daisy_assert (approximate (Theta[i] - (S_m[i] + S_p[i]) * dt, Theta_sat));
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
	      daisy_assert (Theta[i] - (S_m[i] + S_p[i]) * dt <= Theta_sat);
	    }
	  else
	    // Otherwise, fill it up.
	    {
	      // Remove delta water from sink (thus adding it to the soil).
	      S_p[i] -= delta_water / dt;
	      extra_water -= delta_water * dz;

	      // Check that we got it right.
	      daisy_assert (approximate (Theta[i] - (S_m[i] + S_p[i]) * dt,
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
  if (std::isnormal (extra_water))
    surface.accept_top (extra_water, geo, 0U, dt, msg);

  // Check that the sink terms add up.
  if (fabs (geo.total_surface (S_p) - q_top - extra_water) > 1.0e-11)
    {
      std::ostringstream tmp;
      tmp << __FILE__ << ":" <<  __LINE__
          << ": BUG: Total S_p = " << geo.total_surface (S_p) 
          << ", q_top = " 
          << q_top << ", extra_water = " << extra_water;
      msg.error (tmp.str ());
    }
}

bool 
MacroStandard::check_alist (const AttributeList& al, Treelog& err)
{
  bool ok = true;
  const PLF& distribution = al.plf ("distribution");
  const int size = distribution.size ();

  if (size < 2)
    {
      err.error ("You must specify at least two points in distribution");
      return false;
    }

  double height_start;
  if (al.check ("height_start"))
    {
      height_start = al.number ("height_start");
      if (std::isnormal (distribution (height_start)))
	{
	  err.error ("distribution (height_start) should be 0.0");
	  ok = false;
	}
    }
  else
    height_start = distribution.x (size - 1);

  double height_end;
  if (al.check ("height_end"))
    {
      height_end = al.number ("height_end");
      if (std::isnormal (distribution (height_end)))
	{
	  err.error ("distribution (height_end) should be 1.0");
	  ok = false;
	}
    }
  else
    height_end = distribution.x (0);

  if (height_end >= height_start)
    {
      err.error ("height_end should be below height_start");
      ok = false;
    }

  if (al.number ("pressure_end") >= al.number ("pressure_initiate"))
    {
      err.error ("pressure_end must be lower than pressure_initiate");
      ok = false;
    }

  return ok;
}

void 
MacroStandard::load_syntax (Syntax& syntax, AttributeList& alist)
{ 
  syntax.add_check (check_alist);
  alist.add ("description", "\
The area between 'height_start' and 'height_end' contains macropores,\n\
which are initiated when the water potential reach 'pressure_initiate',\n\
and then immediately emptied down to 'pressure_end'.  The water entering\n\
the macropore is distributed in soil below as a source term, according\n\
to the 'distribution' parameter.");

  syntax.add ("height_start", "cm", Check::non_positive (), 
	      Syntax::OptionalConst, 
	      "Macropores starts at this depth (a negative number).\n\
If not specified, use the last point in 'distribution'.");
  syntax.add ("height_end", "cm", Check::non_positive (),
	      Syntax::OptionalConst, 
	      "Macropores ends at this depth (a negative number).\n\
If not specified, use the first point in 'distribution'.");
  syntax.add ("distribution", "cm", Syntax::Fraction (), Syntax::Const, "\
Distribution of macropore end points as a function of height.\n\
The function should start with '1' at 'height_end', and then decrease to\n\
'0' at 'height_start'.  It can be constant, but may never increase.\n\
The value indicates the fraction of macropores which ends at the given\n\
where all macropores is assumed to start at the top.");
  static VCheck::StartValue start (1.0);
  static VCheck::EndValue end (0.0);
  static VCheck::FixedPoint fixpoint (0.0, 0.0);
  static VCheck::All distcheck (start, end, fixpoint, 
				VCheck::non_increasing ());
  syntax.add_check ("distribution", distcheck);
  syntax.add ("pressure_initiate", "cm", Syntax::Const, 
	      "Pressure needed to init pref.flow");
  alist.add ("pressure_initiate", -3.0);
  syntax.add ("pressure_end", "cm", Syntax::Const, 
	      "Pressure after pref.flow has been init");
  alist.add ("pressure_end", -30.0);
  syntax.add ("S_p", "h-1", Syntax::LogOnly,
	      "Macropore sink term.");
  syntax.add ("pond_max", "mm", Check::non_negative (), Syntax::Const, "\
Maximum height of ponding before spilling into macropores.\n\
After macropores are activated pond will havethis height.");
  alist.add ("pond_max", 0.5);

}

std::auto_ptr<Macro> 
Macro::create (double depth)
{ 
  daisy_assert (depth < 0.0);

  Syntax syntax;
  AttributeList alist;
  MacroStandard::load_syntax (syntax, alist);
  alist.add ("type", "default");

  PLF distribution;
  distribution.add (depth, 1.0);
  if (depth < -80.0)
    distribution.add (-80.0, 0.0);
  else if (depth < -1.0)
    distribution.add (depth + 1, 0.0);
  else
    distribution.add (depth * 0.99, 0.0);
  distribution.add (0.0, 0.0);
  alist.add ("distribution", distribution);

  daisy_assert (syntax.check (alist, Treelog::null ()));
  Block block (syntax, alist, Treelog::null (), "macro");

  return std::auto_ptr<Macro> (new MacroStandard (block)); 
}


static struct MacroStandardSyntax
{
  static Macro& make (Block& al)
  { return *new MacroStandard (al); }

  MacroStandardSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    MacroStandard::load_syntax (syntax, alist);
    Librarian<Macro>::add_type ("default", alist, syntax, &make);
  }
} MacroStandard_syntax;
