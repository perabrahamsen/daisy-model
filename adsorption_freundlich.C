// adsorption_freundlich.C

#include "adsorption.h"
#include "soil.h"
#include "mathlib.h"

class AdsorptionFreundlich : public Adsorption
{
  // Parameters.
  const double K_clay;
  const double m;

  // Simulation.
public:
  double C_to_M (const Soil&, double Theta, int, double C) const;
  double M_to_C (const Soil&, double Theta, int, double M) const;

  // Create.
public:
  AdsorptionFreundlich (const AttributeList& al)
    : Adsorption (al.name ("type")),
      K_clay (al.number ("K_clay")),
      m (al.number ("m"))
    { }
};

double 
AdsorptionFreundlich::C_to_M (const Soil& soil,
			      double Theta, int i, double C) const
{
  const double S = K_clay * soil.clay (i) * pow (C, m);
  return rho_mineral * S + Theta * C;
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

  AdsorptionFreundlichSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    syntax.add ("K_clay", Syntax::Number, Syntax::Const);
    syntax.add ("m", Syntax::Number, Syntax::Const);
    AttributeList& alist = *new AttributeList ();
    Librarian<Adsorption>::add_type ("Freundlich", alist, syntax, &make);
  }
} AdsorptionFreundlich_syntax;
