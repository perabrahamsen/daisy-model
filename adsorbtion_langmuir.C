// adsorbtion_langmuir.C

#include "adsorbtion.h"
#include "soil.h"
#include "mathlib.h"

class AdsorbtionLangmuir : public Adsorbtion
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
      return rho_mineral * S + Theta * C;
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
      const double b = rho_mineral * my_max + Theta * K - M;
      const double c = - M * K;

      return single_positive_root_of_square_equation (a, b, c);
    }
  // Create.
public:
  AdsorbtionLangmuir (const AttributeList& al)
    : Adsorbtion (al.name ("type")),
      K_clay (al.number ("K_clay")),
      my_max (al.number ("my_max"))
    { }
};

static struct AdsorbtionLangmuirSyntax
{
  static Adsorbtion& make (const AttributeList& al)
  {
    return *new AdsorbtionLangmuir (al);
  }

  AdsorbtionLangmuirSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    syntax.add ("K_clay", Syntax::Number, Syntax::Const);
    syntax.add ("my_max", Syntax::Number, Syntax::Const);
    AttributeList& alist = *new AttributeList ();
    Librarian<Adsorbtion>::add_type ("Langmuir", alist, syntax, &make);
  }
} AdsorbtionLangmuir_syntax;
