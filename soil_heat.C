// soil_heat.C

#include "soil_heat.h"
#include "alist.h"
#include "surface.h"
#include "groundwater.h"
#include "soil_water.h"
#include "soil.h"
#include "syntax.h"
#include "time.h"
#include "mathlib.h"
#include "log.h"

struct SoilHeat::Implementation
{
  // State
  vector<double> T;
  double T_top;

  // BUG:  These should move to groundwater module.
  // Parameters.
  const double average;		// Average temperature at bottom [C]
  const double amplitude;	// Variation in bottom temperature [C]
  const double omega;		// Period length for above [ rad / day]
  const double omega_offset;	// Period start for above [rad]
  /* const */ double delay;	// Period delay [ cm/rad ??? ]

  // Calculate season delay [C/cm] at pF 2.
  static double calculate_delay (const Soil& soil, double omega);

  // Simulation.
  void tick (const Time&, const Soil&, const SoilWater&, 
	     const Surface&, const Groundwater&);
  double energy (const Soil&, const SoilWater&, double from, double to) const;
  void set_energy (const Soil&, const SoilWater&, 
		   double from, double to, double energy);
  double bottom (const Time&, const double depth) const;

  // Create & Destroy.
  bool check (unsigned n) const;
  void initialize (const AttributeList& al, 
		   const Soil& soil, const Time& time);
  Implementation (const AttributeList&);
};

void
SoilHeat::Implementation::tick (const Time& time,
				const Soil& soil,
				const SoilWater& soil_water,
				const Surface& surface,
				const Groundwater& groundwater)
{
#ifdef WATER_FLUX_HEAT
  const double water_heat_capacity = 4.2; // [J/cm³/K]
#endif

  // Border conditions.
  const double T_bottom = bottom (time, // BUGLET: Should be time - 1 hour.
				  -soil.zplus (soil.size () - 1));
  const double T_top_new = surface.temperature ();
  
  if (T_top < -400.0)
    T_top = T_top_new;

  // Limit for groundwater table.
  int size = soil.size ();
  if (!groundwater.flux_bottom ())
    {
      if (groundwater.table () < soil.z (size - 1))
	THROW (runtime_error ("Groundwater table below lowest node."));
      size = soil.interval (groundwater.table ()) + 1;
    }

  // Tridiagonal matrix.
  vector<double> a (size, 0.0);
  vector<double> b (size, 0.0);
  vector<double> c (size, 0.0);
  vector<double> d (size, 0.0);

  // Inner nodes.
  for (int i = 0; i < size; i++)
    {
      const int prev = i - 1;
      const int next = i + 1;

      // Calculate average heat capacity and conductivity.
      const double capacity
	= (soil.heat_capacity (i, soil_water.Theta (i))
	   + soil.heat_capacity (i, soil_water.Theta_old (i))) / 2.0;
      const double conductivity 
	= (soil.heat_conductivity (i, soil_water.Theta (i))
	   + soil.heat_conductivity (i, soil_water.Theta_old (i))) / 2.0;

      // Calculate distances.
      const double dz_next 
	= (i == size - 1)
	? soil.z (i) - soil.z (prev)
	: soil.z (next) - soil.z (i);
      const double dz_prev 
	= (i == 0)
	? soil.z (i) - 0.0
	: soil.z (i) - soil.z (prev);
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
	  = (((soil.heat_conductivity (i, soil_water.Theta_old (i))
	       - soil.heat_conductivity (prev, soil_water.Theta_old (prev)))
	      + (soil.heat_conductivity (i, soil_water.Theta (i))
		 - soil.heat_conductivity (prev, soil_water.Theta (prev))))
	     / 2.0) / dz_prev;
      else
	gradient 
	  = (((soil.heat_conductivity (next, soil_water.Theta_old (next))
	       - soil.heat_conductivity (prev, soil_water.Theta_old (prev)))
	      + (soil.heat_conductivity (next, soil_water.Theta (next))
		 - soil.heat_conductivity (prev, soil_water.Theta (prev))))
	     / 2.0) / dz_both;
      
      
      // Computational,
      const double Cx = gradient
#ifndef WATER_FLUX_HEAT
	// BUG!
	;
#else
	+ water_heat_capacity
	* (soil_water.q (i) + soil_water.q (next)) / 2.0;
#endif
      // Setup tridiagonal matrix.
      a[i] = - conductivity / dz_both / dz_prev + Cx / 2.0 / dz_both;
      b[i] = capacity / dt
	+ conductivity / dz_both * (1.0 / dz_next + 1.0 / dz_prev);
      c[i] = - conductivity / dz_both / dz_next - Cx / 2.0 / dz_both;
      const double x2 = dT_next / dz_next - dT_prev/ dz_prev;
      if (i == 0)
	d[i] = T[i] * capacity / dt
	  + conductivity / soil.z (1) * (x2 + T_top_new / soil.z (0))
	  + Cx * (T[1] - T_top + T_top_new) / (2.0 * soil.z (1));
      else
	d[i] = T[i] * capacity / dt + (conductivity / dz_both) * x2
	  + Cx * dT_both / dz_both / 2.0;
    }
  d[size - 1] = d[size - 1] - c[size - 1] * T_bottom;
  tridia (0, size, a, b, c, d, T.begin ());
  T_top = T_top_new;
  assert (T[0] < 50.0);

  // Temperature is constant in the groundwater.
  for (unsigned int i = size; i < soil.size (); i++)
    T[i] = T_bottom;
}

double 
SoilHeat::Implementation::energy (const Soil& soil,
				  const SoilWater& soil_water,
				  double from, double to) const
{
  double amount = 0.0;
  double old = 0.0;

  for (unsigned int i = 0; i < soil.size () && old > to ; i++)
    {
      if (soil.zplus (i) < from)
	{
	  const double height = (min (old, from) - max (soil.zplus (i), to));
	  const double C = soil.heat_capacity (i, soil_water.Theta (i));
	  amount += C * T[i] * height;
	}
      old = soil.zplus (i);
    }
  return amount;
}

void
SoilHeat::Implementation::set_energy (const Soil& soil,
				      const SoilWater& soil_water, 
				      double from, double to, double energy)
{
  // Find total energy capacity.
  double capacity = 0.0;
  double old = 0.0;

  for (unsigned int i = 0; i < soil.size () && old > to ; i++)
    {
      if (soil.zplus (i) < from)
	{
	  const double height = (min (old, from) - max (soil.zplus (i), to));
	  capacity += soil.heat_capacity (i, soil_water.Theta (i)) * height;
	}
      old = soil.zplus (i);
    }
  
  // Distribute temperature evenly.
  const double average = energy / capacity / (to - from);
  old = 0.0;

  for (unsigned int i = 0; i < soil.size () && old > to ; i++)
    {
      if (soil.zplus (i) < from)
	{
	  const double height = (min (old, from) - max (soil.zplus (i), to));
	  T[i] = (height * average + (soil.dz (i) - height)* T[i]) 
	    / soil.dz (i);
	}
      old = soil.zplus (i);
    }
}

double
SoilHeat::Implementation::calculate_delay (const Soil& soil, double omega)
{
  // BUG: When this is calculated it gives an average delay for the
  // whole profile.  The numbers are thus only correct at the bottom
  // of the profile.

  const double pF_2_0 = -100.0;
  double k = 0;
  double C = 0;
  
  for (unsigned int i = 0; i < soil.size (); i++)
    {
      const double Theta_pF_2_0 = soil.Theta (i, pF_2_0);
      k += soil.dz (i) * soil.heat_conductivity (i, Theta_pF_2_0);
      C += soil.dz (i) * soil.heat_capacity (i, Theta_pF_2_0);
    }
  const double a = k / C;
  return sqrt (24.0 * 2.0 * a / omega);
}

double 
SoilHeat::Implementation::bottom (const Time& time, double depth) const 
{
  return average 
    + amplitude 
    * exp (-depth / delay)
    * cos (omega * time.yday () 
	   + omega_offset 
	   - depth / delay);
}

bool
SoilHeat::Implementation::check (unsigned n) const
{
  bool ok = true;
  if (T.size () > n)
    {
      cerr << "You have " << n << " intervals but " 
	   << T.size () << " T values\n";
      ok = false;
    }
  return ok;
}

SoilHeat::Implementation::Implementation (const AttributeList& al)
  : T_top (al.number ("T_top")),
    average (al.number ("average")),
    amplitude (al.number ("amplitude")),
    omega (al.number ("omega")),
    omega_offset (al.number ("omega_offset"))
{ }

void
SoilHeat::Implementation::initialize (const AttributeList& al, 
				      const Soil& soil, const Time& time)
{
  delay  = calculate_delay (soil, omega);
  soil.initialize_layer (T, al, "T");
  // Fill out with T calculated numbers if necessary.
  for (unsigned int i = T.size (); i < soil.size (); i++)
    T.push_back (bottom (time, -soil.zplus (i)));

  assert (T.size () == soil.size ());
}


void 
SoilHeat::tick (const Time& time, 
		const Soil& soil,
		const SoilWater& soil_water,
		const Surface& surface,
		const Groundwater& groundwater)
{
  impl.tick (time, soil, soil_water, surface, groundwater);
}

double 
SoilHeat::energy (const Soil& soil,
		  const SoilWater& soil_water,
		  double from, double to) const
{
  return impl.energy (soil, soil_water, from, to);
}

void
SoilHeat::set_energy (const Soil& soil,
		      const SoilWater& soil_water, 
		      double from, double to, double energy)
{
  impl.set_energy (soil, soil_water, from, to, energy);
}

void
SoilHeat::swap (const Soil& soil, double from, double middle, double to)
{
  // This will only work right if the water is also swaped.
  // There *might* be a small error on the top and bottom nodes, but I
  // believe it should work as long as the energy is directly
  // proportional with the water content.
  soil.swap (impl.T, from, middle, to);
}
  
double
SoilHeat::T (int i) const
{
  return impl.T[i]; 
}

void
SoilHeat::output (Log& log, Filter& filter) const
{
  log.output ("T", filter, impl.T);
}

bool
SoilHeat::check (unsigned n) const
{
  return impl.check (n);
}

void
SoilHeat::load_syntax (Syntax& syntax, AttributeList& alist)
{ 
  Geometry::add_layer (syntax, "T");
  syntax.add ("T_top", Syntax::Number, Syntax::State);
  alist.add ("T_top", -500.0);	// Use surface temperature.
  syntax.add ("average", Syntax::Number, Syntax::Const);
  alist.add ("average", 7.8);
  syntax.add ("amplitude", Syntax::Number, Syntax::Const);
  alist.add ("amplitude", 8.5);
  syntax.add ("omega", Syntax::Number, Syntax::Const);
  alist.add ("omega", 2.0 * M_PI / 365.0);
  syntax.add ("omega_offset", Syntax::Number, Syntax::Const);
  alist.add ("omega_offset", -209.0);
}

SoilHeat::SoilHeat (const AttributeList& al)
  : impl (*new Implementation (al))
{ }


void
SoilHeat::initialize (const AttributeList& al, 
		      const Soil& soil, const Time& time)
{ impl.initialize (al, soil, time); }

SoilHeat::~SoilHeat ()
{
  delete &impl;
}
