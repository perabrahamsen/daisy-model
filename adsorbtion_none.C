// adsorbtion_none.C

#include "adsorbtion.h"

class AdsorbtionNone : public Adsorbtion
{
  // Simulation.
public:
  double C_to_M (const Soil&, double Theta, int, double C) const
    { return C * Theta; }
  double M_to_C (const Soil&, double Theta, int, double M) const
    { return M / Theta; }

  // Create.
public:
  AdsorbtionNone (const AttributeList& al)
    : Adsorbtion (al.name ("type"))
    { }
};

static struct AdsorbtionNoneSyntax
{
  static Adsorbtion& make (const AttributeList& al)
  {
    return *new AdsorbtionNone (al);
  }

  AdsorbtionNoneSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    Librarian<Adsorbtion>::add_type ("none", alist, syntax, &make);
  }
} AdsorbtionNone_syntax;
