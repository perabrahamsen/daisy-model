// average_arithmetic.C -- Arithmetic average.

#include "average.h"

struct AverageArithmetic : public Average
{
  // Simulation.
  double operator()(double a, double b) const
    { return (a + b) / 2.0; }
  // Create and Destroy.
  AverageArithmetic (const AttributeList& al)
    : Average (al)
    { }
  ~AverageArithmetic ()
    { }
};

static struct AverageArithmeticSyntax
{
  static Average&
  make (const AttributeList& al)
    { return *new AverageArithmetic (al); }
  AverageArithmeticSyntax ()
    {
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      alist.add ("description", "Arithmetic average '(a+b)/2'.");
      Librarian<Average>::add_type ("arithmetic", alist, syntax, &make);
    }
} AverageArithmetic_syntax;
