// adsorption_linear.C -- Lininear Freundlich adsorption.

#include "adsorption.h"
#include "soil.h"

class AdsorptionLinear : public Adsorption
{
  // Parameters.
  const double K_clay;

  // Simulation.
public:
  double C_to_M (const Soil& soil, double Theta, int i, double C) const
    {
      const double porosity = soil.Theta (i, 0.0);
      const double clay = soil.clay (i);
      const double W_clay = clay * rho_mineral * (1.0 - porosity);
      return C * (K_clay * W_clay + Theta);
    }
  double M_to_C (const Soil& soil, double Theta, int i, double M) const
    {
      const double porosity = soil.Theta (i, 0.0);
      const double clay = soil.clay (i);
      const double W_clay = clay * rho_mineral * (1.0 - porosity);
      return M / (Theta + K_clay * W_clay);
    }
  // Create.
public:
  AdsorptionLinear (const AttributeList& al)
    : Adsorption (al.name ("type")),
      K_clay (al.number ("K_clay"))
    { }
};

static struct AdsorptionLinearSyntax
{
  static Adsorption& make (const AttributeList& al)
  {
    return *new AdsorptionLinear (al);
  }

  AdsorptionLinearSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    syntax.add ("K_clay", Syntax::Number, Syntax::Const);

    Librarian<Adsorption>::add_type ("linear", alist, syntax, &make);
  }
} AdsorptionLinear_syntax;
