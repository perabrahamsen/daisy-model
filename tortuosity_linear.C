// tortuosity_linear.C --- Linear Impedance factor

#include "tortuosity.h"
#include "hydraulic.h"

static double h_wp = -15000;

class TortuosityLinear : public Tortuosity
{
  const double a_maybe;
  const double b;

  // Simulation.
public:
  double factor (const Hydraulic& hydraulic, double Theta) const
    {
      double a = a_maybe;
      if (a < 0.0)
	a = hydraulic.Theta (h_wp);
      
      return max (1.0e-6, a + b * Theta);
    }

  // Create.
public:
  TortuosityLinear (const AttributeList& al)
    : Tortuosity (al.name ("type")),
      a_maybe (al.check ("a") ? al.number ("a") : -42.0),
      b (al.number ("b"))
    { }
};

static struct TortuosityLinearSyntax
{
  static Tortuosity& make (const AttributeList& al)
  {
    return *new TortuosityLinear (al);
  }

  TortuosityLinearSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "Linear Impedance factor.  a + b Theta.");
    syntax.add ("a", "cm^3/cm^3", Syntax::OptionalConst, "\
Theta offset.  By default, this corresponds to the wilting point.");
    syntax.add ("b", Syntax::None (), Syntax::Const, "Theta factor.");
    alist.add ("b", 2.0);
    Librarian<Tortuosity>::add_type ("linear", alist, syntax, &make);
  }
} TortuosityLinear_syntax;

