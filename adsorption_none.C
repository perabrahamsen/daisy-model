// adsorption_none.C

#include "adsorption.h"

class AdsorptionNone : public Adsorption
{
  // Simulation.
public:
  double C_to_M (const Soil&, double Theta, int, double C) const
    { return C * Theta; }
  double M_to_C (const Soil&, double Theta, int, double M) const
    { return M / Theta; }

  // Create.
public:
  AdsorptionNone (const AttributeList& al)
    : Adsorption (al.name ("type"))
    { }
};

static struct AdsorptionNoneSyntax
{
  static Adsorption& make (const AttributeList& al)
  {
    return *new AdsorptionNone (al);
  }

  AdsorptionNoneSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "No adsorption.\n\
Used for solutes that are not adsorped to the soil.");
    Librarian<Adsorption>::add_type ("none", alist, syntax, &make);
  }
} AdsorptionNone_syntax;
