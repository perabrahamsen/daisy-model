// adsorption_freundlich.C

#include "adsorption.h"
#include "soil.h"
#include "mathlib.h"

class AdsorptionFreundlich : public Adsorption
{
  // Parameters.
  const double K_clay;
  const double K_OC;
  const double m;

  // Simulation.
public:
  double C_to_M (const Soil&, double Theta, int, double C) const;
  double M_to_C (const Soil&, double Theta, int, double M) const;

  // Create.
public:
  AdsorptionFreundlich (const AttributeList& al)
    : Adsorption (al.name ("type")),
      K_clay (al.check ("K_clay") ? al.number ("K_clay") : 0.0),
      K_OC (al.check ("K_OC") 
	       ? al.number ("K_OC") 
	       : al.number ("K_clay")),
      m (al.number ("m"))
    { }
};

double 
AdsorptionFreundlich::C_to_M (const Soil& soil,
			      double Theta, int i, double C) const
{
  const double K = soil.clay (i) * K_clay 
    + soil.humus (i) * c_fraction_in_humus * K_OC;
  const double rho = soil.dry_bulk_density (i);
  const double S = K * pow (C, m);
  return rho * S + Theta * C;
}

double 
AdsorptionFreundlich::M_to_C (const Soil& soil,
			      double Theta, int i, double M) const
{
  // Guess start boundary.
  double min_C = 0.0;
  double min_M = C_to_M (soil, Theta, i, min_C);
  double max_C = 1.0;
  double max_M = C_to_M (soil, Theta, i, max_C);

  // Find upper boundary by doubling repeatedly.
  while (max_M < M)
    {
      max_C *= 2;
      max_M = C_to_M (soil, Theta, i, max_C);
    }

  // Guess by middling the C value.
  int count = 0;
  while (!approximate (min_M, max_M))
    {
      assert (count++ < 100);	// 100 iterations should be enough.
      
      const double new_C = (min_C + max_C) / 2.0;
      const double new_M = C_to_M (soil, Theta, i, new_C);
      if (new_M < M)
	{
	  min_C = new_C;
	  min_M = new_M;
	}
      else
	{
	  max_C = new_C;
	  max_M = new_M;
	}
    }
  return (min_C + max_C) / 2.0;
}

static struct AdsorptionFreundlichSyntax
{
  static Adsorption& make (const AttributeList& al)
  {
    return *new AdsorptionFreundlich (al);
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

      non_negative (al.number ("m"), "m", ok);
      
      if (!ok)
	CERR << "in `Freundlich' adsorption\n";

      return ok;
    }

  AdsorptionFreundlichSyntax ()
  {
    Syntax& syntax = *new Syntax (check_alist);
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "M = rho K C^m + Theta C");
    syntax.add ("K_clay", "(g/cm^3)^-m", Syntax::OptionalConst, 
		"Clay dependent distribution parameter.\n\
It is multiplied with the soil clay fraction to get the clay part of\n\
the `K' factor.  If `K_OC' is specified, `K_clay' defaults to 0.\n\
The dimension depends on the `m' parameter.");
    syntax.add ("K_OC", "(g/cm^3)^-m", Syntax::OptionalConst, 
		"Humus dependent distribution parameter.\n\
It is multiplied with the soil organic carbon fraction to get the\n\
carbon part of the `K' factor.  By default, `K_OC' is equal to `K_clay'.\n\
The dimension depends on the `m' parameter.");
    syntax.add ("m", Syntax::None (), Syntax::Const,
		"Freundlich parameter");
    Librarian<Adsorption>::add_type ("Freundlich", alist, syntax, &make);
  }
} AdsorptionFreundlich_syntax;
