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
#include "mathlib.h"
#include "plf.h"
#include "soil_chemical.h"
#include "check.h"

static double
heat_turnover_factor (double T)
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

static double
water_turnover_factor (double h)
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

class ChemicalStandard : public Chemical
{
  // Parameters.
private:
  const double crop_uptake_reflection_factor_;
  const double canopy_dissipation_rate_;
  const double canopy_washoff_coefficient_;
  const double diffusion_coefficient_; // [cm^2/h]
  const AttributeList solute_alist_;
  const double decompose_rate_;
  const PLF decompose_heat_factor_;
  const PLF decompose_water_factor_;
  const PLF decompose_CO2_factor_;
  const PLF decompose_conc_factor_;
  const PLF decompose_depth_factor_;
  const PLF decompose_lag_increment_;
  const bool active_groundwater_;

  // Queries.
public:
  double crop_uptake_reflection_factor () const	// [0-1]
    { return crop_uptake_reflection_factor_; }
  double canopy_dissipation_rate () const	// [h^-1]
    { return canopy_dissipation_rate_; }
  double canopy_washoff_coefficient () const	// [mm]
    { return canopy_washoff_coefficient_; }
  double diffusion_coefficient () const	// [cm^2/h]
    { return diffusion_coefficient_; }
  const AttributeList& solute_alist () const
    { return solute_alist_; }
  double decompose_rate () const// [h^-1]
    { return decompose_rate_; }
  double decompose_heat_factor (double T) const
    {
      if (decompose_heat_factor_.size () < 1)
	return heat_turnover_factor (T);
      else
	return decompose_heat_factor_ (T);
    }
  double decompose_water_factor (double h) const
    {
      if (decompose_water_factor_.size () < 1)
	return water_turnover_factor (h);
      else
	return decompose_water_factor_ (h);
    }
  double decompose_CO2_factor (double CO2) const
    { return decompose_CO2_factor_ (CO2); }
  double decompose_conc_factor (double conc) const
    { return decompose_conc_factor_ (conc); }
  double decompose_depth_factor (double depth) const
    { return decompose_depth_factor_ (depth); }
  double decompose_lag_increment (double conc) const
    { return decompose_lag_increment_ (conc); }
  bool active_groundwater () const
    { return active_groundwater_; }

  // Create.
public:
  ChemicalStandard (const AttributeList&);
};


ChemicalStandard::ChemicalStandard (const AttributeList& al)
  : Chemical (al),
    crop_uptake_reflection_factor_ 
  (al.number ("crop_uptake_reflection_factor")),
    canopy_dissipation_rate_ 
  (al.check ("canopy_dissipation_rate")
   ? al.number ("canopy_dissipation_rate")
   : (al.check ("canopy_dissipation_halftime")
      ? halftime_to_rate (al.number ("canopy_dissipation_halftime"))
      : al.number ("canopy_dissipation_rate_coefficient"))),
    canopy_washoff_coefficient_ (al.number ("canopy_washoff_coefficient")),
    diffusion_coefficient_ (al.number ("diffusion_coefficient") * 3600.0),
    solute_alist_ (al.alist ("solute")),
    decompose_rate_ (al.check ("decompose_rate")
	     ? al.number ("decompose_rate")
		     : halftime_to_rate (al.number ("decompose_halftime"))),
    decompose_heat_factor_ (al.plf ("decompose_heat_factor")),
    decompose_water_factor_ (al.plf ("decompose_water_factor")),
    decompose_CO2_factor_ (al.plf ("decompose_CO2_factor")),
    decompose_conc_factor_ (al.plf ("decompose_conc_factor")),
    decompose_depth_factor_ (al.plf ("decompose_depth_factor")),
    decompose_lag_increment_ (al.plf ("decompose_lag_increment")),
    active_groundwater_ (al.flag ("active_groundwater"))
{ }

static struct ChemicalStandardSyntax
{
  static Chemical&
  make (const AttributeList& al)
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
    syntax.add ("description", Syntax::String, Syntax::OptionalConst,
		"Description of this parameterization."); 
    alist.add ("description", "\
Read chemical properties as normal Daisy parameters.");
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
Fracxftion of the chemical that follows the water off the canopy.");
    syntax.add ("diffusion_coefficient", "cm^2/s", Check::positive (),
		Syntax::Const, "Diffusion coefficient.");
    syntax.add_submodule ("solute", alist, Syntax::Const,
			  "Description of chemical in soil.",
			  SoilChemical::load_syntax);
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
concentration each hour.  When lag in any node reaches 1.0,\n\
decomposition begins.  It can never be more than 1.0 or less than 0.0.");
    alist.add ("decompose_lag_increment", no_factor);
    syntax.add ("active_groundwater", Syntax::Boolean, Syntax::Const, "\
Clear this flag to turn off decomposition in groundwater.");
    alist.add ("active_groundwater", true);
    Librarian<Chemical>::add_type ("default", alist, syntax, &make);
  }
} ChemicalStandard_syntax;
