// adsorption_linear.C -- Lininear Freundlich adsorption.

#include "adsorption.h"
#include "soil.h"

class AdsorptionLinear : public Adsorption
{
  // Parameters.
  const double K_clay;
  const double K_humus;

  // Simulation.
public:
  double C_to_M (const Soil& soil, double Theta, int i, double C) const
    {
      const double K = soil.clay (i) * K_clay + soil.humus (i) * K_humus;
      const double rho = soil.dry_bulk_density (i);
      return C * (K * rho + Theta);
    }
  double M_to_C (const Soil& soil, double Theta, int i, double M) const
    {
      const double K = soil.clay (i) * K_clay + soil.humus (i) * K_humus;
      const double rho = soil.dry_bulk_density (i);
      return M / (Theta + K * rho);
    }
  // Create.
public:
  AdsorptionLinear (const AttributeList& al)
    : Adsorption (al.name ("type")),
      K_clay (al.check ("K_clay") ? al.number ("K_clay") : 0.0),
      K_humus (al.check ("K_humus") 
	       ? al.number ("K_humus") 
	       : al.number ("K_clay"))
    { }
};

static struct AdsorptionLinearSyntax
{
  static Adsorption& make (const AttributeList& al)
  {
    return *new AdsorptionLinear (al);
  }

  static bool check_alist (const AttributeList& al)
    {
      bool ok = true;

      const bool has_K_clay = al.check ("K_clay");
      const bool has_K_humus = al.check ("K_humus");
      
      if (!has_K_clay && !has_K_humus)
	{
	  CERR << "You must specify either `K_clay' or `K_humus'\n";
	  ok = false;
	}
      if (has_K_clay)
	non_negative (al.number ("K_clay"), "K_clay", ok);
      if (has_K_humus)
	non_negative (al.number ("K_humus"), "K_humus", ok);
      
      if (!ok)
	CERR << "in `linear' adsorption\n";

      return ok;
    }
  AdsorptionLinearSyntax ()
  {
    Syntax& syntax = *new Syntax (check_alist);
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "M = rho K C + Theta C");
    syntax.add ("K_clay", "g/cm^3", Syntax::OptionalConst, 
		"Clay dependent distribution parameter.\n\
It is multiplied with the soil clay fraction to get the clay part of\n\
the `K' factor.  If `K_humus' is specified, `K_clay' defaults to 0.");
    syntax.add ("K_humus", "g/cm^3", Syntax::OptionalConst, 
		"Humus dependent distribution parameter.\n\
It is multiplied with the soil humus fraction to get the humus part\n\
of the `K' factor.  By default, `K_humus' is equal to `K_clay'.");

    Librarian<Adsorption>::add_type ("linear", alist, syntax, &make);
  }
} AdsorptionLinear_syntax;
