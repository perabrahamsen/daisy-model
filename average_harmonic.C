// average_harmonic.C -- Harmonic average.

#include "average.h"

struct AverageHarmonic : public Average
{
  // Simulation.
  double operator()(double a, double b) const
    { return 2.0 * a * b / (a + b); }
  // Create and Destroy.
  AverageHarmonic (const AttributeList& al)
    : Average (al)
    { }
  ~AverageHarmonic ()
    { }
};

static struct AverageHarmonicSyntax
{
  static Average&
  make (const AttributeList& al)
    { return *new AverageHarmonic (al); }
  AverageHarmonicSyntax ()
    {
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      alist.add ("description", "Harmonic average `2ab/(a+b)'.");
      Librarian<Average>::add_type ("harmonic", alist, syntax, &make);
    }
} AverageHarmonic_syntax;
