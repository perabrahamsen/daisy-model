// chemical_std.C

#include "chemical.h"

class ChemicalStandard : public Chemical
{
  // Parameters.
private: 
  const double crop_uptake_reflection_factor_;
  const double canopy_dissipation_rate_coefficient_;
  const double canopy_washoff_coefficient_;

  // Queries.
public:
  double crop_uptake_reflection_factor () const	// [0-1]
    { return crop_uptake_reflection_factor_; }
  double canopy_dissipation_rate_coefficient () const	// [h^-1]
    { return canopy_dissipation_rate_coefficient_; }
  double canopy_washoff_coefficient () const	// [mm]
    { return canopy_washoff_coefficient_; }

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
    canopy_washoff_coefficient_ (al.number ("\
canopy_washoff_coefficient"))
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
      syntax.add ("crop_uptake_reflection_factor", "[0-1]", Syntax::Const,
		  "How much of the chemical is reflexted at crop uptake.");
      syntax.add ("canopy_dissipation_rate_coefficient", "h^-1", Syntax::Const,
		  "How fast does the chemical dissipate on canopy.");
      syntax.add ("canopy_washoff_coefficient", "mm", Syntax::Const,
		  "How fast is the chemical washed off the canopy.");
      Librarian<Chemical>::add_type ("default", alist, syntax, &make);
    }
} ChemicalStandard_syntax;
