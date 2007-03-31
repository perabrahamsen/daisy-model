// chemical_std.C
// 
// Copyright 1996-2002 Per Abrahamsen and Søren Hansen
// Copyright 2000-2002 KVL.
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

#include "chemical.h"
#include "organic_matter.h"
#include "soil_heat.h"
#include "soil_water.h"
#include "soil.h"
#include "geometry.h"
#include "log.h"
#include "block.h"
#include "alist.h"
#include "mathlib.h"
#include "plf.h"
#include "check.h"
#include "librarian.h"

struct ChemicalStandard : public Chemical
{
  // Parameters.
  const double crop_uptake_reflection_factor;
  const double canopy_dissipation_rate;
  const double canopy_washoff_coefficient;
  const double diffusion_coefficient_; 
  const double decompose_rate;
  const PLF decompose_heat_factor_;
  const PLF decompose_water_factor_;
  const PLF decompose_CO2_factor;
  const PLF decompose_conc_factor;
  const PLF decompose_depth_factor;
  const PLF decompose_lag_increment;

  // Management.
  double spray_;
  double harvest_;
  double residuals;

  // Surface state and log.
  double snow_storage;
  double snow_in;
  double snow_out;
  
  double canopy_storage;
  double canopy_in;
  double canopy_dissipate;
  double canopy_harvest;
  double canopy_out;

  double surface_storage;
  double surface_in;
  double surface_out;
  double surface_mixture;
  double surface_runoff;

  // Soil state and log.
  std::vector<double> decomposed;
  std::vector<double> uptaken;
  std::vector<double> lag;

  // Utlities.
  static double heat_turnover_factor (double T);
  static double water_turnover_factor (double h);
  double decompose_heat_factor (const double T) const;
  double decompose_water_factor (const double h) const;

  // Solute.
  double diffusion_coefficient () const;

  // Management.
  void spray (const double amount, const double dt); // [g/m^2]
  void harvest (const double removed, const double surface, const double dt);
  
  // Simulation.
  void tick_top (const double snow_leak_rate, // [h^-1]
                 const double cover, // [],
                 const double canopy_leak_rate, // [h^-1]
                 const double surface_runoff_rate, // [h^-1]
                 const double dt); // [h]
  void mixture (const Geometry& geo,
                const double pond /* [mm] */, 
                const double rate /* [h/mm] */,
                const double dt /* [h]*/);
  void infiltrate (const double rate, const double dt);
  double down ();                 // [g/m^2/h]
  void uptake (const Soil&, const SoilWater&, double dt);
  void decompose (const Geometry& geo,
                  const Soil&, const SoilWater&, const SoilHeat&, 
                  const OrganicMatter&, double dt);
  void output (Log&) const;

  // Create.
  void initialize (const AttributeList&, const Geometry&,
                   const Soil&, const SoilWater&, Treelog&);
  ChemicalStandard (Block&);
};


double
ChemicalStandard::heat_turnover_factor (const double T)
{
  if (T < 0.0)
    return 0.0;
  if (T < 20.0)
    return 0.1 * T;
  if (T < 37.0)
    return exp (0.47 - 0.027 * T + 0.00193 * T *T);

  if (T < 60.0)
    {
      // J.A. van Veen and M.J.Frissel.
      const double max_val = exp (0.47 - 0.027 * T + 0.00193 * T * T);
      return max_val * (1.0 - (T - 37.0) / (60.0 - 37.0));
    }
  return 0.0;
}

double
ChemicalStandard::water_turnover_factor (const double h)
{
  if (h >= 0.0)
    return 0.6;

  const double pF = h2pF (h);

  if (pF <= 0.0)
    return 0.6;
  if (pF <= 1.5)
    return 0.6 + (1.0 - 0.6) * pF / 1.5;
  if (pF <= 2.5)
    return 1.0;
  if (pF <= 6.5)
    return 1.0 - (pF - 2.5) / (6.5 - 2.5);

  return 0;
}

double
ChemicalStandard::decompose_heat_factor (const double T) const
{
  if (decompose_heat_factor_.size () < 1)
    return heat_turnover_factor (T);
  else
    return decompose_heat_factor_ (T);
}
double 

ChemicalStandard::decompose_water_factor (const double h) const
{
  if (decompose_water_factor_.size () < 1)
    return water_turnover_factor (h);
  else
    return decompose_water_factor_ (h);
}

double
ChemicalStandard::diffusion_coefficient () const
{ return diffusion_coefficient_; }

void 
ChemicalStandard::spray (const double amount, const double dt) // [g/m^2]
{ spray_ += amount / dt; }

void 
ChemicalStandard::harvest (const double removed, const double surface,
                           const double dt)
{ 
  const double new_storage 
    = canopy_storage + (spray_ - harvest_ - residuals) * dt;
  const double gone_rate = new_storage * removed / dt;
  harvest_ += gone_rate * (1.0 - surface); 
  residuals += gone_rate * surface;
}

void 
ChemicalStandard::tick_top (const double snow_leak_rate, // [h^-1]
                            const double cover, // [],
                            const double canopy_leak_rate, // [h^-1]
                            const double surface_runoff_rate, // [h^-1]
                            const double dt) // [h]
{
  const double old_storage = snow_storage + canopy_storage;

  // Snow pack
  snow_in = spray_;
  snow_storage += snow_in * dt;

  snow_out = snow_storage * snow_leak_rate;
  snow_storage -= snow_out * dt;

  // Canopy.
  canopy_in = snow_out * cover;
  canopy_harvest = harvest_;
  canopy_storage += (canopy_in - canopy_harvest - residuals) * dt;

  const double old_canopy = canopy_storage;
  const double washoff_rate 
    = canopy_washoff_coefficient * canopy_leak_rate;
  canopy_storage // Implicit solution: new = old - new * rate =>
    = old_canopy / (1.0 + dt * (canopy_dissipation_rate + washoff_rate));
  canopy_dissipate = canopy_dissipation_rate * canopy_storage;
  canopy_out = canopy_storage * washoff_rate + residuals;
  daisy_assert (approximate (canopy_storage - old_canopy, 
                             - dt * (canopy_out + canopy_dissipate)));
  if (canopy_storage < 1e-18)
    {
      canopy_out += canopy_storage / dt;
      canopy_storage = 0.0;
    }

  // Surface
  surface_in = canopy_out + (snow_out - canopy_in);
  surface_runoff = surface_storage * surface_runoff_rate; 
  surface_storage += (surface_in - surface_runoff) * dt;
      
  // Reset manangement information.
  spray_ = 0.0;
  harvest_ = 0.0;
  residuals = 0.0;

  // Mass balance.
  const double new_storage = snow_storage + canopy_storage;
  const double input = snow_in;
  const double output = surface_in + canopy_harvest + canopy_dissipate;
  daisy_assert (approximate (new_storage - old_storage, 
                             (input - output) * dt));
}

void 
ChemicalStandard::mixture (const Geometry& geo,
                           const double pond /* [mm] */, 
                           const double rate /* [h/mm] */,
                           const double dt /* [h]*/)
{
  // Make sure we have something to mix.
  if (pond < 1e-6 || rate < 1e-99)
    {
      surface_mixture = 0.0;
      return;
    }

  // Mix them.
  const double soil_conc
    = geo.content_at (static_cast<const Solute&> (*this), &Solute::C, 0.0)
    * (100.0 * 100.0) / 10.0; // [g/cm^3/] -> [g/m^2/mm]
  const double storage_conc = surface_storage / pond;// [g/m^2/mm]
  
  surface_mixture // [g/m^2/h]
    = std::min (surface_storage / dt, (storage_conc - soil_conc) / rate);
  surface_storage -= surface_mixture * dt;
}

void 
ChemicalStandard::infiltrate (const double rate, const double dt)
{
  surface_out = surface_storage * rate;
  surface_storage -= surface_out * dt;
}

double
ChemicalStandard::down ()                 // [g/m^2/h]
{ return surface_out; }

void 
ChemicalStandard::uptake (const Soil& soil, 
                          const SoilWater& soil_water,
                          const double dt)
{
  daisy_assert (uptaken.size () == soil.size ());

  const double rate = 1.0 - crop_uptake_reflection_factor;
  
  for (unsigned int i = 0; i < soil.size (); i++)
    uptaken[i] = C (i) * soil_water.S_root (i) * rate;
  
  add_to_root_sink (uptaken, dt);
}

void 
ChemicalStandard::decompose (const Geometry& geo,
                             const Soil& soil, 
                             const SoilWater& soil_water,
                             const SoilHeat& soil_heat,
                             const OrganicMatter& organic_matter,
                             const double dt)
{
  daisy_assert (decomposed.size () == soil.size ());

  unsigned int size = soil.size ();

  // Update lag time.
  bool found = false;
  for (unsigned int i = 0; i < size; i++)
    {
      lag[i] += this->decompose_lag_increment (C_[i]) * dt;
      
      if (lag[i] >= 1.0)
	{
	  lag[i] = 1.0;
	  found = true;
	}
      else if (lag[i] < 0.0)
	{
	  lag[i] = 0.0;
	}
    }

  // No decomposition.
  if (!found)
    size = 0;

  for (unsigned int i = 0; i < size; i++)
    {
      const double heat_factor 
	= this->decompose_heat_factor (soil_heat.T (i));
      const double water_factor 
	= this->decompose_water_factor (soil_water.h (i));
      const double CO2_factor 
	= this->decompose_CO2_factor (organic_matter.CO2 (i));
      const double conc_factor
	= this->decompose_conc_factor (C_[i]);
      const double depth_factor
	= this->decompose_depth_factor (geo.z (i));
      const double rate
	= decompose_rate * heat_factor * water_factor * CO2_factor
	* conc_factor * depth_factor;
      decomposed[i] = M_left (i, dt) * rate;
    }
  for (unsigned int i = size; i < soil.size (); i++)
    decomposed[i] = 0.0;

  add_to_sink (decomposed, dt);
}

void
ChemicalStandard::output (Log& log) const
{
  Solute::output (log);

  // Surface.
  output_variable (snow_storage, log);
  output_variable (snow_in, log);
  output_variable (snow_out, log);
  output_variable (canopy_storage, log);
  output_variable (canopy_in, log);
  output_variable (canopy_dissipate, log);
  output_variable (canopy_harvest, log);
  output_variable (canopy_out, log);
  output_variable (surface_storage, log);
  output_variable (surface_in, log);
  output_variable (surface_runoff, log);
  output_variable (surface_mixture, log);
  output_variable (surface_out, log);

  // Soil.
  output_variable (uptaken, log);
  output_variable (decomposed, log);
}

void
ChemicalStandard::initialize (const AttributeList& al,
                              const Geometry& geo,
                              const Soil& soil, const SoilWater& soil_water, 
                              Treelog& out)
{
  Solute::initialize (al, geo, soil, soil_water, out);
  uptaken.insert (uptaken.begin (), soil.size (), 0.0);
  decomposed.insert (decomposed.begin (), soil.size (), 0.0);
  lag.insert (lag.end (), soil.size () - lag.size (), 0.0);
}

ChemicalStandard::ChemicalStandard (Block& al)
  : Chemical (al),
    crop_uptake_reflection_factor 
  /**/ (al.number ("crop_uptake_reflection_factor")),
    canopy_dissipation_rate 
  /**/ (al.check ("canopy_dissipation_rate")
        ? al.number ("canopy_dissipation_rate")
        : (al.check ("canopy_dissipation_halftime")
           ? halftime_to_rate (al.number ("canopy_dissipation_halftime"))
           : al.number ("canopy_dissipation_rate_coefficient"))),
    canopy_washoff_coefficient (al.number ("canopy_washoff_coefficient")),
    diffusion_coefficient_ (al.number ("diffusion_coefficient") * 3600.0),
    decompose_rate (al.check ("decompose_rate")
                    ? al.number ("decompose_rate")
                    : halftime_to_rate (al.number ("decompose_halftime"))),
    decompose_heat_factor_ (al.plf ("decompose_heat_factor")),
    decompose_water_factor_ (al.plf ("decompose_water_factor")),
    decompose_CO2_factor (al.plf ("decompose_CO2_factor")),
    decompose_conc_factor (al.plf ("decompose_conc_factor")),
    decompose_depth_factor (al.plf ("decompose_depth_factor")),
    decompose_lag_increment (al.plf ("decompose_lag_increment")),
    spray_ (0.0),
    harvest_ (0.0),
    residuals (0.0),
    snow_storage (al.number ("snow_storage")),
    snow_in (0.0),
    snow_out (0.0),
    canopy_storage (al.number ("canopy_storage")),
    canopy_in (0.0),
    canopy_dissipate (0.0),
    canopy_harvest (0.0),
    canopy_out (0.0),
    surface_in (0.0),
    lag (al.check ("lag") 
         ? al.number_sequence ("lag") 
         : std::vector<double> ())
{ }

static struct ChemicalStandardSyntax
{
  static Model& make (Block& al)
  { return *new ChemicalStandard (al); }

  static bool check_alist (const AttributeList& al, Treelog& err)
  { 
    bool ok = true;

    static bool warned = false;
    if (al.check ("canopy_dissipation_rate_coefficient") && !warned)
      {
	err.entry ("OBSOLETE: Use 'canopy_dissipation_rate' instead "
		   "of 'canopy_dissipation_rate_coefficient'");
	warned = true;
      }

    if (!al.check ("canopy_dissipation_rate")
	&& !al.check ("canopy_dissipation_halftime")
	&& !al.check ("canopy_dissipation_rate_coefficient"))
      {
	err.entry ("\
You must specify 'canopy_dissipation_rate' or 'canopy_dissipation_halftime'");
	ok = false;
      }
    if (al.check ("canopy_dissipation_rate") 
	&& al.check ("canopy_dissipation_halftime"))
      {
	err.entry ("\
You may not specify both 'canopy_dissipation_rate' and \
'canopy_dissipation_halftime'");
	ok = false;
      }

    if (!al.check ("decompose_rate") && !al.check ("decompose_halftime"))
      {
	err.entry ("\
You must specify 'decompose_rate' or 'decompose_halftime'");
	ok = false;
	
      }
    if (al.check ("decompose_rate") && al.check ("decompose_halftime"))
      {
	err.entry ("\
You may not specify both 'decompose_rate' and 'decompose_halftime'");
	ok = false;
      }
    return ok;
  }

  ChemicalStandardSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    syntax.add_check (check_alist);

#if 0
    syntax.add_submodule ("solute", alist, Syntax::Const,
			  "Description of chemical in soil.",
                          Solute::load_syntax);
#endif
    Solute::load_syntax (syntax, alist);
    
    syntax.add ("description", Syntax::String, Syntax::OptionalConst,
		"Description of this parameterization."); 
    alist.add ("description", "\
Read chemical properties as normal Daisy parameters.");

    // Parameters.
    syntax.add_fraction ("crop_uptake_reflection_factor", Syntax::Const, "\
How much of the chemical is reflected at crop uptake.");
    alist.add ("crop_uptake_reflection_factor", 1.0);
    syntax.add ("canopy_dissipation_rate", "h^-1", 
		Check::fraction (), Syntax::OptionalConst,
		"How fast does the chemical dissipate on canopy.\n\
You must specify it with either 'canopy_dissipation_halftime' or\n\
'canopy_dissipation_rate'.");
    syntax.add ("canopy_dissipation_halftime", "h", 
		Check::positive (), Syntax::OptionalConst,
		"How fast does the chemical dissipate on canopy.\n\
You must specify it with either 'canopy_dissipation_halftime' or\n\
'canopy_dissipation_rate'.");
    syntax.add ("canopy_dissipation_rate_coefficient", "h^-1", 
		Check::fraction (), Syntax::OptionalConst,
		"Obsolete alias for 'canopy_dissipation_rate'.");
    syntax.add_fraction ("canopy_washoff_coefficient", Syntax::Const, "\
Fraction of the chemical that follows the water off the canopy.");
    syntax.add ("diffusion_coefficient", "cm^2/s", Check::positive (),
		Syntax::Const, "Diffusion coefficient.");
    syntax.add ("decompose_rate", "h^-1", Check::fraction (),
		Syntax::OptionalConst,
		"How fast the solute is being decomposed in the soil.\n\
You must specify it with either 'decompose_rate' or 'decompose_halftime'.");
    syntax.add ("decompose_halftime", "h", Check::positive (),
		Syntax::OptionalConst,
		"How fast the solute is being decomposed in the soil.\n\
You must specify it with either 'decompose_rate' or 'decompose_halftime'.");
    syntax.add ("decompose_heat_factor", "dg C", Syntax::None (),
		Syntax::Const, "Heat factor on decomposition.");
    alist.add ("decompose_heat_factor", PLF::empty ());
    syntax.add ("decompose_water_factor", "cm", Syntax::None (),
		Syntax::Const,
		"Water potential factor on decomposition.");
    alist.add ("decompose_water_factor", PLF::empty ());
    syntax.add ("decompose_CO2_factor", "g CO2-C/cm^3", Syntax::None (),
		Syntax::Const,
		"CO2 development factor on decomposition.");
    PLF no_factor;
    no_factor.add (0.0, 1.0);
    no_factor.add (1.0, 1.0);
    alist.add ("decompose_CO2_factor", no_factor);
    syntax.add ("decompose_conc_factor", "g/cm^3 H2O", Syntax::None (),
		Syntax::Const,
		"Concentration development factor on decomposition.");
    alist.add ("decompose_conc_factor", no_factor);
    syntax.add ("decompose_depth_factor", "cm", Syntax::None (),
		Syntax::Const,
		"Depth influence on decomposition.");
    alist.add ("decompose_depth_factor", no_factor);
    syntax.add ("decompose_lag_increment", 
		"g/cm^3/h", Syntax::Fraction (), Syntax::Const,
		"Increment lag with the value of this PLF for the current\n\
concentration each hour.  When lag in any cell reaches 1.0,\n\
decomposition begins.  It can never be more than 1.0 or less than 0.0.");
    alist.add ("decompose_lag_increment", no_factor);
    
    // Surface variables.
    syntax.add ("snow_storage", "g/m^2", Syntax::State, 
                "Stored in the snow pack.");
    alist.add ("snow_storage", 0.0);
    syntax.add ("snow_in", "g/m^2/h", Syntax::LogOnly, 
                "Entering snow pack..");
    syntax.add ("snow_out", "g/m^2/h", Syntax::LogOnly, 
                "Leaking from snow pack.");

    syntax.add ("canopy_storage", "g/m^2", Syntax::State, 
                "Stored on the canopy.");
    alist.add ("canopy_storage", 0.0);
    syntax.add ("canopy_in", "g/m^2/h", Syntax::LogOnly, 
                "Entering canopy.");
    syntax.add ("canopy_dissipate", "g/m^2/h", Syntax::LogOnly, 
                "Dissipating from canopy.");
    syntax.add ("canopy_out", "g/m^2/h", Syntax::LogOnly, 
                "Falling through or off the canopy.");
    syntax.add ("canopy_harvest", "g/m^2/h", Syntax::LogOnly, 
                "Amount removed with crop harvest.");

    syntax.add ("surface_storage", "g/m^2", Syntax::State, 
                "Stored on the soil surface.");
    alist.add ("surface_storage", 0.0);
    syntax.add ("surface_in", "g/m^2/h", Syntax::LogOnly, 
                "Falling on the bare soil surface.");
    syntax.add ("surface_runoff", "g/m^2/h", Syntax::LogOnly, 
                "Removed through lateral movement on the soil.");
    syntax.add ("surface_mixture", "g/m^2/h", Syntax::LogOnly, 
                "Entering the soil through mixture with ponded water.");
    syntax.add ("surface_out", "g/m^2/h", Syntax::LogOnly, 
                "Entering the soil with water infiltration.");

    // Soil solute.
    syntax.add ("uptaken", "g/cm^3/h", Syntax::LogOnly, Syntax::Sequence,
                "Amount uptaken by crops in this time step.");
    syntax.add ("decomposed", "g/cm^3/h", Syntax::LogOnly, Syntax::Sequence,
                "Amount decomposed in this time step.");
    syntax.add ("lag", Syntax::None (), Syntax::OptionalState,
                "This state variable grows with lag_increment (C) each hour.\n\
When it reached 1.0, decomposition begins.");
    // Use "none" adsorption by default.
    AttributeList none;
    none.add ("type", "none");
    alist.add ("adsorption", none);

    Librarian::add_type (Chemical::component, "default", alist, syntax, &make);
  }
} ChemicalStandard_syntax;
