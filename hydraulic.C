// hydraulic.C

#include "hydraulic.h"
#include "library.h"
#include "alist.h"
#include "syntax.h"
#include "csmp.h"
#include <vector>
#include <map>

bool 
Hydraulic::compact () const
{
  return false;
}

void
Hydraulic::K_to_M (CSMP& csmp, const int intervals) const
{
  static const double h0 = -20000.0;
  const double Ksat = K (0.0);
  const double max_change = pow (Ksat / K (h0), 1.0 / intervals);
  double step = (0 - h0) / 4.0;

  double h = h0;
  double sum = 0.0;
  while (h < 0)
    {
      csmp.add (h, sum);
      step *= 2;
      while (K (h + step) / K (h) > max_change)
	step /= 2;
      sum += step * (K (h) + K (h + step)) / 2;
      h += step;
    }
  csmp.add (h, sum);
}

void
Hydraulic::load_syntax (Syntax& syntax, AttributeList& alist)
{ 
  syntax.add ("Theta_sat", Syntax::Number, Syntax::Const);
  syntax.add ("Theta_res", Syntax::Number, Syntax::Const);
  alist.add ("Theta_res", 0.0);
}

Hydraulic::Hydraulic (const AttributeList& al)
  : Theta_sat (al.number ("Theta_sat")),
    Theta_res (al.number ("Theta_res"))
{ }

Hydraulic::~Hydraulic ()
{ }

Librarian<Hydraulic>::Content* Librarian<Hydraulic>::content = NULL;
