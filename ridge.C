// ridge.C --- Simulate a ridge system.
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


#include "ridge.h"
#include "soil.h"
#include "plf.h"
#include "submodel.h"
#include "syntax.h"
#include "submodel.h"
#include "mathlib.h"
#include "log.h"
#include "soil.h"
#include "soil_water.h"

struct Ridge::Implementation
{
  // Parameters.
  const PLF z;			// Ridge geometry function. [->cm]
  const double lowest;		// Lowest point of ridge. [cm]
  const double highest;		// Highest point of ridge. [cm]
  const double height;		// Height from lowest to highest point. [cm]
  const PLF x;			// Inverste ridge geometry function. [cm->]
  const double z_switch;	// Height where we switch between regimes. [cm]
  const double x_switch;	// Location where we switch between regimes. []
  const double R_crust;		// Resistance in crust. [h]
  /*const*/ int last_node ; // Last node in ridge system.
  /*const*/ double dz;		// Depth of first node below ridge. [cm]
  /*const*/ double K_sat_below; // Saturated conductivity below ridge. [cm/h]
  // Variables.
  double Theta;			// Water in ridge. [cm^3/cm^3]
  double Theta_pre;             // Water in ridge before transport. [cm^3/cm^3]
  double h;			// Pressure in ridge. [cm]
  double K;			// Saturated conductivity in ridge [cm/h]
  double z_pond;		// Internal free water level. [cm]
  double x_pond;		// Water to soil point. []
  double internal_ponding;	// Distance from ridge bot. to water surf [cm]
  double R_bottom;		// Resistance in ridge bottom [h]
  double R_wall;		// Resistance in ridge wall [h]
  double I_bottom;		// Infiltration through ridge bottom [cm/h]
  double I_wall;		// Infiltration through ridge wall [cm/h]
  double I;			// Total infiltration [cm/h]
  
  // Simulation.
  void tick (const Soil&, const SoilWater&, 
	     double external_ponding /* [cm] */);
  void update_water (const Soil&, const vector<double>& S_,
		     vector<double>& h_, vector<double>& Theta_,
		     vector<double>& q, const vector<double>& q_p);
  void output (Log& log) const;

  // Create and Destroy.
  static void load_syntax (Syntax&, AttributeList&);
  void initialize (const Soil&, const SoilWater&);
  static PLF normalize (PLF plf);
  Implementation (const AttributeList&);
  ~Implementation ();
};

static const double x_width = 1.0;

void 
Ridge::tick (const Soil& soil, const SoilWater& soil_water,
	     double external_ponding)
{ impl.tick (soil, soil_water, external_ponding * 0.1 /* mm -> cm */); }

void 
Ridge::Implementation::tick (const Soil& soil, const SoilWater& soil_water,
			     double external_ponding)
{
  // First, we need to find the internal ponding height. 
  // The external ponding assumes a flat surface, we need to find the
  // point (x_pond, z_pond) in the ridge geometry where air, soil and
  // surface water all meet.
  if (external_ponding < 1.0e-5)
    {
      // No ponding.
      x_pond = 0.0;
      z_pond = lowest;
    }
  else if (external_ponding > highest - 1.0e-5)
    {
      // Above top of ridge (ridge geometry doesn't matter then). 
      x_pond = 1.0;
      z_pond = external_ponding;
    }
  else
    {
      // Somewhere in between.  
      //
      // For a given point (x, z), x * z is the amount of space
      // available had all the soil from the ridge been removed.  If
      // you remove the space occupied by the ridge soil, the
      // remaining space is available to free water and air.  So we
      // need to find the point where x * z minus the ridge soil is
      // equal to the amount of water in the pond.  
      //
      // We can't solve this analytically in general, but we can solve
      // it when the ridge geometry is a straight line.  Since the
      // geometry is represented by a PLF, we can find the relevant
      // piece and solve it there.

      // Note that x * z can be negative because our zero point is at
      // the original soil surface, not the bottom of the ridge, this
      // doesn't matter since we are only interested in differences,
      // not absolute number.  

      // Ridge soil until now.
      double integral = 0.0;
      // Last point in the PLF.
      double x0 = 0.0;
      double z0 = lowest;
      
      for (unsigned int i = 1; ; i++)
	{
	  // We already know the answer must lie between two points.
	  assert (i < z.size ());
	  // New point in the PLF.
	  const double x1 = z.x (i);
	  const double z1 = z.y (i);
	  // Difference.
	  const double delta_z = z1 - z0;
	  const double delta_x = x1 - x0;
	  // Average z height.
	  const double average_z = 0.5 * (z0 + z1);
	  // Ridge soil for this step.
	  const double this_step = delta_x * average_z;
	  // Total space for this step.
	  const double total = x1 * z1;

	  // Check if this interval can contain all the ponded water.
	  if (total - (integral + this_step) >= external_ponding)
	    {
	      // Find slant in this interval.
	      const double slant = delta_z / delta_x;

	      // We now need to find the point (x_p, z_p) where 
	      //     total - (integral + this_step) = external_ponding)
	      // where
	      //     total = x_p * z_p
	      //     this_step = 0.5 * (x_p - x0) * (z0 + z_p)
	      //
	      // Substitute 
	      //     z_p = z0 + slant * (x_p - x0)
	      // and we get
	      //     x_p * (z0 + slant * (x_p - x0))
	      //     - (integral 
	      //        + 0.5 * (x_p - x0) * (z0 + z0 + slant * (x_p - x0))
	      //     = external_ponding
	      // <=>
	      //     x_p * z0 + slant * x_p^2 - slant * x0 * x_p
	      //     - integral
	      //     - (x_p - x0) * ( z0 + 0.5 * slant * (x_p - x0))
	      //     - external_ponding
	      //     = 0
	      // <=>
	      //     x_p * z0 + slant * x_p^2 - slant * x0 * x_p
	      //     - integral 
	      //     - x_p * z0
	      //     - 0.5 * x_p * slant * (x_p - x0)
	      //     + x0 * z0
	      //     + 0.5 * x0 * slant * (x_p - x0)
	      //     - external_ponding
	      //     = 0
	      // <=> 
	      //     x_p * z0 + slant * x_p^2 - slant * x0 * x_p
	      //     - integral 
	      //     - x_p * z0
	      //     - 0.5 * x_p * slant * x_p
	      //     + 0.5 * x_p * slant * x0
	      //     + x0 * z0
	      //     + 0.5 * x0 * slant * x_p
	      //     - 0.5 * x0 * slant * x0
	      //     - external_ponding
	      //     = 0
	      // <=>
	      //     (slant - 0.5 * slant) * x_p^2
	      //     (z0 - slant * x0 - z0 + 0.5 * slant * x0 
	      //      + 0.5 * slant * x0) * x_p
	      //     - integral + x0 * z0 - 0.5 * x0 * slant * x0
	      //     - external_ponding
	      //     = 0
	      // <=>
	      //     0.5 * slant * x_p^2
	      //     = integral - x0 * z0 + 0.5 * x0 * slant * x0
	      //     + external_ponding
	      // <=>
	      //     x_p^2 = 2 * (integral - x0 * z0 + external_ponding)
	      //             / slant + x0 * x0 
	      // <=>
	      //     x_p = sqrt (2.0 * (integral - x0 * z0 
	      //                        + external_ponding) / slant
	      //                 + x0 * x0)
	      x_pond = sqrt (2.0 * (integral - x0 * z0 + external_ponding) 
			     / slant + x0 * x0);
	      z_pond = z (x_pond);

	      // Check that we got the right result.
	      assert (x_pond > 0.0);
	      assert (x_pond < 1.0);
	      const double total = x_pond * z_pond;
	      const double this_step = 0.5 * (x_pond - x0) * (z0 + z_pond);
	      assert (approximate (total - (integral + this_step), 
				   external_ponding));
	      break;
	    }
	  
	  // Prepare next step
	  integral += this_step;
	  x0 = x1;
	  z0 = z1;
	}
    }
  internal_ponding = z_pond - lowest;

  if (external_ponding < 0.0)
    // Exfiltration
    {
      R_bottom = -42.42e42;
      I_bottom = external_ponding;
      // R_wall meaningless.
      I_wall = 0.0;
      R_wall = -42.42e42;
    }
  else
    {
      // Find maximal infiltration.
      const double Theta_sat = soil.Theta (0, 0.0, 0.0);
      const double available_space = (Theta_sat - Theta) * dz;
      assert (available_space > 0.0);
      const double I_max = min (external_ponding, 
				available_space - 1.0e-8) / dt;

      // Find resistance and infiltration for bottom regime.
      const double x_bottom = min (x_pond, x_switch);
      const double bottom_width = x_bottom - 0.0;
      if (bottom_width > 0.0)
	{
	  const double K_bottom = min (K, K_sat_below);
	  const double dz_bottom
	    = z.integrate (0.0, x_bottom) / bottom_width + dz;
#if 0
	  CERR << "dz_bottom = " << dz_bottom << ", x_bottom = " 
	       << x_bottom << ", bottom_width = " << bottom_width
	       << ", dz = " << dz << "\n";
#endif
	  R_bottom
	    = (x_width / bottom_width) * (dz_bottom /  K_bottom + R_crust);
	  I_bottom = min (internal_ponding / R_bottom, I_max);
	}
      else
	{
	  R_bottom = -42.42e42;
	  I_bottom = 0.0;
	}

      // Find resistance and infiltration for wall regime.
      if (z_pond > z_switch + 1e-5)
	{
	  const double wall_width = x_pond - x_switch;
	  assert (wall_width > 0.0);
	  const double dz_wall
	    = z.integrate (x_switch, x_pond) / wall_width + dz;
#if 0 
	  CERR << "dz_wall = " << dz_wall << ", wall_with = " 
	       << wall_width << "\n";
#endif
	  R_wall = (x_width / wall_width) * (dz_wall / K);
#if 0
	  CERR << "R_wall = " << R_wall << ", x_width = " <<x_width
	       << ", dz_wall = " <<dz_wall << ", K = " << K << "\n";
#endif
	  I_wall = min ((z_pond - z_switch) / R_wall, I_max - I_bottom);
	}
      else
	{
	  // R_wall meaningless.
	  I_wall = 0.0;
	  R_wall = -42.42e42;
	}
    }
  // Total infiltration.
  I = I_bottom + I_wall;
  assert (I < external_ponding + 1.0e-8);

#if 0
  CERR << "switch = (" << x_switch << ", " << z_switch << "), pond = ("
       << x_pond << ", " << z_pond << ") I = " << I 
       << " (bottom = " << I_bottom << ", wall = " << I_wall 
       << "), internal ponding = " << internal_ponding 
       << ",external ponding = " << external_ponding << "\n";
#endif

  // Update water.
  Theta = I * dt;
  for (int i = 0; i <= last_node; i++)
    Theta += soil_water.Theta (i) * soil.dz (i);
  Theta /= dz;
  Theta_pre = Theta;
  assert (Theta < soil.Theta (0, 0.0, 0.0));
  h = soil.h (0, Theta);
}

void
Ridge::update_water (const Soil& soil,
		     const vector<double>& S_,
		     vector<double>& h_,
		     vector<double>& Theta_,
		     vector<double>& q,
		     const vector<double>& q_p)
{ impl.update_water (soil, S_, h_, Theta_, q, q_p); }

void
Ridge::Implementation::update_water (const Soil& soil,
				     const vector<double>& S_,
				     vector<double>& h_,
				     vector<double>& Theta_,
				     vector<double>& q,
				     const vector<double>& q_p)
{
  const double E = -(q[last_node + 1] + q_p[last_node + 1]);
  Theta = (I - E) * dt;
  for (int i = 0; i <= last_node; i++)
    Theta += (Theta_[i] - S_[i] * dt) * soil.dz (i);
  Theta /= dz;
  const double Theta_sat = soil.Theta (0, 0.0, 0.0);
  assert (Theta < Theta_sat);
  h = soil.h (0, Theta);

  q[0] = -I;
  for (int i = 0; i <= last_node; i++)
    {
      q[i+1] = q[i] + soil.dz (i) * (S_[i] + (Theta - Theta_[i]) / dt ) 
	- q_p[i+1];
      Theta_[i] = Theta;
      h_[i] = h;
      assert (approximate (soil.h (i, Theta), h));
    }
  assert (approximate (E, -(q[last_node + 1] + q_p[last_node + 1])));
}

void 
Ridge::output (Log& log) const
{ impl.output (log); }

void 
Ridge::Implementation::output (Log& log) const
{
  log.output ("Theta", Theta);
  log.output ("Theta_pre", Theta_pre);
  log.output ("h", h);
  log.output ("z_pond", z_pond);
  log.output ("x_pond", x_pond);
  log.output ("internal_ponding", internal_ponding);
  if (R_bottom >= 0.0)
  log.output ("R_bottom", R_bottom);
  if (R_wall >= 0.0)
    log.output ("R_wall", R_wall);
  log.output ("I_bottom", I_bottom);
  log.output ("I_wall", I_wall);
  log.output ("I", I);
}

int 
Ridge::last_node () const
{ return impl.last_node; }

double
Ridge::h () const
{ return impl.h; }

double 
Ridge::exfiltration () const
{ return -impl.I * 10; }

void 
Ridge::initialize (const Soil& soil, const SoilWater& soil_water)
{ impl.initialize (soil, soil_water); }

void 
Ridge::Implementation::initialize (const Soil& soil, const SoilWater& soil_water)
{
  // Find values depending on soil numerics.
  last_node = soil.interval_plus (lowest);
  assert (last_node+1 < soil.size ());
  dz = 0 - soil.zplus (last_node);
  K_sat_below = soil.K (last_node+1, 0.0, 0.0);

  // Initialize water content.
  Theta = 0.0;
  for (int i = 0; i <= last_node; i++)
    Theta += soil_water.Theta (i) * soil.dz (i);
  Theta /= dz;
  Theta_pre = Theta;
  assert (Theta < soil.Theta (0, 0.0, 0.0));
  h = soil.h (0, Theta);
  K = soil.K (0, 0.0, 0.0);
}

void
Ridge::load_syntax (Syntax& syntax, AttributeList& alist)
{
  alist.add ("submodel", "Ridge");
  alist.add ("description", "Surface model after ridging.");
  
  // Parameters.
  syntax.add ("z", Syntax::Fraction (), "cm", Syntax::Const, "\
The basic ridge parameter is the height, formulated as z (x),\n\
where x is the relative distance from the middle of the ridge.\n\
x = 0.0 is in the middle of a ridge, while x = 1.0 is at the\n\
maximal distance.  The ridge is assumed to be symmetric.\n\
z (x) is measured in centimeter above the unridged soil surface, which\n\
means it is in the same reference system as the rest of the model.");
  syntax.add ("R_crust", "h", Syntax::Const, "Resistance in crust.");
  syntax.add ("switch", Syntax::Fraction (), Syntax::Const,
	      "\
Fraction of ridge height where we switch from bottom regime to wall regime.");
  alist.add ("switch", 1.0/3.0);

  // Content.
  syntax.add ("Theta", "cm^3/cm^3", Syntax::LogOnly, "Soil water content.");
  syntax.add ("Theta_pre", "cm^3/cm^3", Syntax::LogOnly, 
	      "Soil water content before transport.");
  syntax.add ("h", "cm", Syntax::LogOnly, "Soil water pressure.");
  syntax.add ("z_pond", "cm", Syntax::LogOnly, "Internal free water height.");
  syntax.add ("x_pond", "", Syntax::LogOnly, "Water to soil point.");
  syntax.add ("internal_ponding", "cm", Syntax::LogOnly, 
	      "Distance from ridge bottom to water surface.");
  syntax.add ("R_bottom", "h", Syntax::LogOnly, "Resistance in ridge bottom.");
  syntax.add ("R_wall", "h", Syntax::LogOnly, "Resistance in ridge wall.");
  syntax.add ("I_bottom", "cm/h", Syntax::LogOnly, 
	      "Infiltration through ridge bottom.");
  syntax.add ("I_wall", "cm/h", Syntax::LogOnly,
	      "Infiltration through ridge wall.");
  syntax.add ("I", "cm/h", Syntax::LogOnly, "Total infiltration.");
}

PLF 
Ridge::Implementation::normalize (PLF plf)
{
  plf.offset (-plf.integrate (0.0, 1.0));
  assert (fabs (plf.integrate (0.0, 1.0)) < 1e-10);
  return plf;
}

Ridge::Ridge (const AttributeList& al)
  : impl (*new Implementation (al))
{ }

Ridge::Implementation::Implementation (const AttributeList& al)
  // The input may be given with an arbitrary origin, typically it
  // will be measured from the bottom of the ridge.  We make sure the
  // integrated value is zero, meaning that the total soil volume (or
  // bulk density) hasn't changed by the ridging operation.
  : z (normalize (al.plf ("z"))),
    // Structure.
    lowest (z (0.0)),
    highest (z (1.0)),
    height (highest - lowest),
    x (z.inverse ()),
    // Find the point where we switch regimes.
    z_switch (lowest + height * al.number ("switch")),
    x_switch (x (z_switch)),
    R_crust (al.number ("R_crust")),
    // Utilities depending on soil...
    last_node (42424242),
    dz (-42.42e42),
    // Log variables.
    Theta (-42.42e42),
    h (-42.42e42),
    K (-42.42e42),
    z_pond (-42.42e42),
    x_pond (-42.42e42),
    internal_ponding (-42.42e42),
    R_bottom (-42.42e42),
    R_wall (-42.42e42),
    I_bottom (0.0),
    I_wall (0.0),
    I (0.0)
{ }

Ridge::~Ridge ()
{ delete &impl; }

Ridge::Implementation::~Implementation ()
{ }

static Submodel::Register 
soil_submodel ("Ridge", Ridge::load_syntax);
