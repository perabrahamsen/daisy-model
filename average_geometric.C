// average_geometric.C -- Geometric average.

#include "average.h"

struct AverageGeometric : public Average
{
  // Simulation.
  double operator()(double a, double b) const
    { return sqrt (a * b); }
  // Create and Destroy.
  AverageGeometric (const AttributeList& al)
    : Average (al)
    { }
  ~AverageGeometric ()
    { }
};

static struct AverageGeometricSyntax
{
  static Average&
  make (const AttributeList& al)
    { return *new AverageGeometric (al); }
  AverageGeometricSyntax ()
    {
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      alist.add ("description", "Geometric average `sqrt(a*b)'.");
      Librarian<Average>::add_type ("geometric", alist, syntax, &make);
    }
} AverageGeometric_syntax;
