// tortuosity_M_Q.C --- Millington-Quirk

#include "tortuosity.h"
#include "hydraulic.h"

class TortuosityM_Q : public Tortuosity
{
  // Simulation.
public:
  double factor (const Hydraulic& hydraulic, double Theta) const
    {
      const double n = hydraulic.Theta (0.0);
      return pow (Theta, 7.0 / 3.0) / (n * n); // Tortuosity factor []
    }

  // Create.
public:
  TortuosityM_Q (const AttributeList& al)
    : Tortuosity (al.name ("type"))
    { }
};

static struct TortuosityM_QSyntax
{
  static Tortuosity& make (const AttributeList& al)
  {
    return *new TortuosityM_Q (al);
  }

  TortuosityM_QSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    Librarian<Tortuosity>::add_type ("M_Q", alist, syntax, &make);
  }
} TortuosityM_Q_syntax;

