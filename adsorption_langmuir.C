// adsorption_langmuir.C

#include "adsorption.h"
#include "soil.h"
#include "mathlib.h"

class AdsorptionLangmuir : public Adsorption
{
  // Parameters.
  const double K;
  const double my_max_clay;
  const double my_max_humus;

  // Simulation.
public:
  double C_to_M (const Soil& soil, double Theta, int i, double C) const
    {
      const double my_max 
	= my_max_clay * soil.clay (i) + my_max_humus * soil.humus (i);
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
      
      const double my_max = my_max_clay * soil.clay (i);

      const double a = Theta;
      const double b = soil.dry_bulk_density (i) * my_max + Theta * K - M;
      const double c = - M * K;

      return single_positive_root_of_square_equation (a, b, c);
    }
  // Create.
public:
  AdsorptionLangmuir (const AttributeList& al)
    : Adsorption (al.name ("type")),
      K (al.number ("K")),
      my_max_clay (al.check ("my_max_clay") ? al.number ("my_max_clay") : 0.0),
      my_max_humus (al.check ("my_max_humus") 
		    ? al.number ("my_max_humus") 
		    : al.number ("my_max_clay"))
    { }
};

static struct AdsorptionLangmuirSyntax
{
  static Adsorption& make (const AttributeList& al)
  {
    return *new AdsorptionLangmuir (al);
  }

  static bool check_alist (const AttributeList& al)
    {
      bool ok = true;

      const bool has_my_max_clay = al.check ("my_max_clay");
      const bool has_my_max_humus = al.check ("my_max_humus");
      
      if (!has_my_max_clay && !has_my_max_humus)
	{
	  CERR << "You must specify either `my_max_clay' or `my_max_humus'\n";
	  ok = false;
	}
      if (has_my_max_clay)
	non_negative (al.number ("my_max_clay"), "my_max_clay", ok);
      if (has_my_max_humus)
	non_negative (al.number ("my_max_humus"), "my_max_humus", ok);

      non_negative (al.number ("K"), "K", ok);

      if (!ok)
	CERR << "in `linear' adsorption\n";

      return ok;
    }
  AdsorptionLangmuirSyntax ()
  {
    Syntax& syntax = *new Syntax (check_alist);
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "M = rho (my_max C) / (K + C) + Theta C");
    syntax.add ("K", "g/cm^3", Syntax::Const, "Half saturation constant.");
    syntax.add ("my_max_clay", "g/cm^3", Syntax::OptionalConst,
		"Max adsorption capacity (clay).\n\
It is multiplied with the soil clay fraction to get the clay part of\n\
`my_max'.  If `my_max_humus' is specified, `my_max_clay' defaults to 0.");
    syntax.add ("my_max_humus", "g/cm^3", Syntax::OptionalConst,
		"Max adsorption capacity (humus).\n\
It is multiplied with the soil humus fraction to get the humus part\n\
of `my_max'.  By default, `my_max_humus' is equal to `my_max_clay'.");
    Librarian<Adsorption>::add_type ("Langmuir", alist, syntax, &make);
  }
} AdsorptionLangmuir_syntax;
