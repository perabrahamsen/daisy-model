// adsorbtion_linear.C -- Lininear Freundlich adsorbtion.

#include "adsorbtion.h"
#include "soil.h"

class AdsorbtionLinear : public Adsorbtion
{
  // Parameters.
  const double K_clay;

  // Simulation.
public:
  double C_to_M (const Soil& soil, double Theta, int i, double C) const
    {
      const double S = K_clay * soil.clay (i) * C;
      return rho_mineral * S + Theta * C;
    }
  double M_to_C (const Soil& soil, double Theta, int i, double M) const
    {
      return M / (Theta + soil.clay (i) * K_clay * rho_mineral);
    }
  // Create.
public:
  AdsorbtionLinear (const AttributeList& al)
    : Adsorbtion (al.name ("type")),
      K_clay (al.number ("K_clay"))
    { }
};

static struct AdsorbtionLinearSyntax
{
  static Adsorbtion& make (const AttributeList& al)
  {
    return *new AdsorbtionLinear (al);
  }

  AdsorbtionLinearSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    syntax.add ("K_clay", Syntax::Number, Syntax::Const);
    AttributeList& alist = *new AttributeList ();
    Librarian<Adsorbtion>::add_type ("linear", alist, syntax, &make);
  }
} AdsorbtionLinear_syntax;
