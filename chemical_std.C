// chemical_std.C
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


#include "chemical.h"
#include "mathlib.h"
#include "plf.h"
#include "soil_chemical.h"

static double
heat_turnover_factor (double T)
{
  if (T < 0.0)
    return 0.0;
  if (T < 20.0)
    return 0.1 * T;

  return exp (0.47 - 0.027 * T + 0.00193 * T *T);
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
  const double canopy_dissipation_rate_coefficient_;
  const double canopy_washoff_coefficient_;
  const double diffusion_coefficient_; // [cm^2/h]
  const AttributeList solute_alist_;
  const double decompose_rate_;
  const PLF decompose_heat_factor_;
  const PLF decompose_water_factor_;
  const PLF decompose_CO2_factor_;
  const PLF decompose_conc_factor_;
  const PLF decompose_depth_factor_;
  const bool active_groundwater_;

  // Queries.
public:
  double crop_uptake_reflection_factor () const	// [0-1]
    { return crop_uptake_reflection_factor_; }
  double canopy_dissipation_rate_coefficient () const	// [h^-1]
    { return canopy_dissipation_rate_coefficient_; }
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
  bool active_groundwater () const
    { return active_groundwater_; }

  // Create.
public:
  ChemicalStandard (const AttributeList&);
};


ChemicalStandard::ChemicalStandard (const AttributeList& al)
  : Chemical (al),
    crop_uptake_reflection_factor_ (al.number ("\
crop_uptake_reflection_factor")),
    canopy_dissipation_rate_coefficient_ (al.number ("\
canopy_dissipation_rate_coefficient")),
    canopy_washoff_coefficient_ (al.number ("canopy_washoff_coefficient")),
    diffusion_coefficient_ (al.number ("diffusion_coefficient") * 3600.0),
    solute_alist_ (al.alist ("solute")),
    decompose_rate_ (al.number ("decompose_rate")),
    decompose_heat_factor_ (al.plf ("decompose_heat_factor")),
    decompose_water_factor_ (al.plf ("decompose_water_factor")),
    decompose_CO2_factor_ (al.plf ("decompose_CO2_factor")),
    decompose_conc_factor_ (al.plf ("decompose_conc_factor")),
    decompose_depth_factor_ (al.plf ("decompose_depth_factor")),
    active_groundwater_ (al.flag ("active_groundwater"))
{ }

static struct ChemicalStandardSyntax
{
  static Chemical&
  make (const AttributeList& al)
    { return *new ChemicalStandard (al); }
  ChemicalStandardSyntax ()
    {
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      alist.add ("description", "\
Read chemical properties as normal Daisy parameters.");
      syntax.add ("crop_uptake_reflection_factor",
		  Syntax::Fraction (), Syntax::Const,
		  "How much of the chemical is reflected at crop uptake.");
      alist.add ("crop_uptake_reflection_factor", 1.0);
      syntax.add ("canopy_dissipation_rate_coefficient", "h^-1", Syntax::Const,
		  "How fast does the chemical dissipate on canopy.");
      syntax.add ("canopy_washoff_coefficient", "mm", Syntax::Const,
		  "How fast is the chemical washed off the canopy.");
      syntax.add ("diffusion_coefficient", "cm^2/s", Syntax::Const,
		  "Diffusion coefficient.");
      syntax.add_submodule ("solute", alist, Syntax::Const,
			    "Description of chemical in soil.",
			    SoilChemical::load_syntax);
      syntax.add ("decompose_rate", "h^-1", Syntax::Const,
		  "Fraction of solute being decomposed each hour.");
      PLF empty;
      syntax.add ("decompose_heat_factor", "dg C", Syntax::None (),
		  Syntax::Const, "Heat factor on decomposition.");
      alist.add ("decompose_heat_factor", empty);
      syntax.add ("decompose_water_factor", "cm", Syntax::None (),
		  Syntax::Const,
		  "Water potential factor on decomposition.");
      alist.add ("decompose_water_factor", empty);
      syntax.add ("decompose_CO2_factor", "g C/cm^3", Syntax::None (),
		  Syntax::Const,
		  "CO2 development factor on decomposition.");
      PLF no_factor;
      no_factor.add (0.0, 1.0);
      no_factor.add (1.0, 1.0);
      alist.add ("decompose_CO2_factor", no_factor);
      syntax.add ("decompose_conc_factor", "g X/cm^3 H2O", Syntax::None (),
		  Syntax::Const,
		  "Concentration development factor on decomposition.");
      alist.add ("decompose_conc_factor", no_factor);
      syntax.add ("decompose_depth_factor", "cm", Syntax::None (),
		  Syntax::Const,
		  "Depth influence on decomposition.");
      alist.add ("decompose_depth_factor", no_factor);
      syntax.add ("active_groundwater", Syntax::Boolean, Syntax::Const, "\
Clear this flag to turn off decomposition in groundwater.");
      alist.add ("active_groundwater", true);
      Librarian<Chemical>::add_type ("default", alist, syntax, &make);
    }
} ChemicalStandard_syntax;
