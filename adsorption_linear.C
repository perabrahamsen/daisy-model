// adsorption_linear.C -- Lininear Freundlich adsorption.

#include "adsorption.h"
#include "soil.h"

class AdsorptionLinear : public Adsorption
{
  // Parameters.
  const double K_clay;
  const double K_OC;

  // Simulation.
public:
  double C_to_M (const Soil& soil, double Theta, int i, double C) const
    {
      const double K = soil.clay (i) * K_clay
	+ soil.humus (i) * c_fraction_in_humus * K_OC;
      const double rho = soil.dry_bulk_density (i);
      return C * (K * rho + Theta);
    }
  double M_to_C (const Soil& soil, double Theta, int i, double M) const
    {
      const double K = soil.clay (i) * K_clay 
	+ soil.humus (i) * c_fraction_in_humus * K_OC;
      const double rho = soil.dry_bulk_density (i);
      return M / (Theta + K * rho);
    }
  // Create.
public:
  AdsorptionLinear (const AttributeList& al)
    : Adsorption (al.name ("type")),
      K_clay (al.check ("K_clay") ? al.number ("K_clay") : 0.0),
      K_OC (al.check ("K_OC") ? al.number ("K_OC") : al.number ("K_clay"))
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
      const bool has_K_OC = al.check ("K_OC");
      
      if (!has_K_clay && !has_K_OC)
	{
	  CERR << "You must specify either `K_clay' or `K_OC'\n";
	  ok = false;
	}
      if (has_K_clay)
	non_negative (al.number ("K_clay"), "K_clay", ok);
      if (has_K_OC)
	non_negative (al.number ("K_OC"), "K_OC", ok);
      
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
the `K' factor.  If `K_OC' is specified, `K_clay' defaults to 0.");
    syntax.add ("K_OC", "g/cm^3", Syntax::OptionalConst, 
		"Humus dependent distribution parameter.\n\
It is multiplied with the soil organic carbon fraction to get the\n\
carbon part of the `K' factor.  By default, `K_OC' is equal to `K_clay'.");

    Librarian<Adsorption>::add_type ("linear", alist, syntax, &make);
  }
} AdsorptionLinear_syntax;
