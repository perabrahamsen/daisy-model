// adsorption_langmuir.C

#include "adsorption.h"
#include "soil.h"
#include "mathlib.h"

class AdsorptionLangmuir : public Adsorption
{
  // Parameters.
  const double K_clay;
  const double my_max;

  // Simulation.
public:
  double C_to_M (const Soil& soil, double Theta, int i, double C) const
    {
      const double K = K_clay * soil.clay (i);
      const double S = (my_max * C) / (K + C);
      return soil.dry_bulk_density (i) * S + Theta * C;
    }
  double M_to_C (const Soil& soil, double Theta, int i, double M) const
    {
      // We need to solve the following equation w.r.t. C.
      //
      //     M = rho (my_max C) / (K + C) + Theta C
      // ==>
      //     M (K + C) = rho my_max C + Theta C (K + C)
      // ==> 
      //     0 = Theta C^2 + (rho my_max + Theta K - M) C - M K
      //
      // So we get a square equation.  We use the positive solution.
      
      const double K = K_clay * soil.clay (i);

      const double a = Theta;
      const double b = soil.dry_bulk_density (i) * my_max + Theta * K - M;
      const double c = - M * K;

      return single_positive_root_of_square_equation (a, b, c);
    }
  // Create.
public:
  AdsorptionLangmuir (const AttributeList& al)
    : Adsorption (al.name ("type")),
      K_clay (al.number ("K_clay")),
      my_max (al.number ("my_max"))
    { }
};

static struct AdsorptionLangmuirSyntax
{
  static Adsorption& make (const AttributeList& al)
  {
    return *new AdsorptionLangmuir (al);
  }

  AdsorptionLangmuirSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    syntax.add ("K_clay", Syntax::Number, Syntax::Const);
    syntax.add ("my_max", Syntax::Number, Syntax::Const);
    AttributeList& alist = *new AttributeList ();
    Librarian<Adsorption>::add_type ("Langmuir", alist, syntax, &make);
  }
} AdsorptionLangmuir_syntax;
