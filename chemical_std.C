// chemical_std.C

#include "chemical.h"

class ChemicalStandard : public Chemical
{
  // Parameters.
private: 
  const double crop_uptake_reflection_factor_;

  // Queries.
public:
  double crop_uptake_reflection_factor () const	// [0-1]
    { return crop_uptake_reflection_factor_; }

  // Create.
public:
  ChemicalStandard (const AttributeList&);
};


ChemicalStandard::ChemicalStandard (const AttributeList& al)
  : Chemical (al),
    crop_uptake_reflection_factor_ (al.number 
				    ("crop_uptake_reflection_factor"))
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
      syntax.add ("crop_uptake_reflection_factor",
		  Syntax::Number, Syntax::Const);
      Librarian<Chemical>::add_type ("default", alist, syntax, &make);
    }
} ChemicalStandard_syntax;
