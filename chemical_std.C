// chemical_std.C

#include "chemical.h"
#include "mathlib.h"
#include "csmp.h"
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
  const CSMP decompose_heat_factor_;
  const CSMP decompose_water_factor_;
  const CSMP decompose_CO2_factor_;
  const CSMP decompose_conc_factor_;
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
    decompose_heat_factor_ (al.csmp ("decompose_heat_factor")),
    decompose_water_factor_ (al.csmp ("decompose_water_factor")),
    decompose_CO2_factor_ (al.csmp ("decompose_CO2_factor")),
    decompose_conc_factor_ (al.csmp ("decompose_conc_factor")),
    active_groundwater_ (al.flag ("active_groundwater"))
{ }

#ifdef BORLAND_TEMPLATES
template class add_submodule<SoilChemical>;
#endif BORLAND_TEMPLATES

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
      add_submodule<SoilChemical> ("solute", syntax, alist, 
				   Syntax::Const,
				   "Description of chemical in soil.");
      syntax.add ("decompose_rate", "h^-1", Syntax::Const,
		  "Fraction of solute being decomposed each hour.");
      CSMP empty;
      syntax.add ("decompose_heat_factor", Syntax::CSMP, Syntax::Const,
		  "Heat factor on decomposition [dg C ->].");
      alist.add ("decompose_heat_factor", empty);
      syntax.add ("decompose_water_factor", Syntax::CSMP, Syntax::Const,
		  "Water potential factor on decomposition [cm ->].");
      alist.add ("decompose_water_factor", empty);
      syntax.add ("decompose_CO2_factor", Syntax::CSMP, Syntax::Const,
		  "CO2 development factor on decomposition [g C/cm^3 ->].");
      CSMP no_factor;
      no_factor.add (0.0, 1.0);
      no_factor.add (1.0, 1.0);
      alist.add ("decompose_CO2_factor", no_factor);
      syntax.add ("decompose_conc_factor", Syntax::CSMP, Syntax::Const,
		  "\
Concentration development factor on decomposition [g X/cm^3 H2O ->].");
      alist.add ("decompose_conc_factor", no_factor);
      syntax.add ("active_groundwater", Syntax::Boolean, Syntax::Const, "\
Clear this flag to turn off decomposition in groundwater.");
      alist.add ("active_groundwater", true);
      Librarian<Chemical>::add_type ("default", alist, syntax, &make);
    }
} ChemicalStandard_syntax;
