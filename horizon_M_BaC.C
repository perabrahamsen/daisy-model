// horizon_M_BaC.C

#include "horizon_M_BaC.h"
#include "syntax.h"
#include "alist.h"

#define exception BUG_exception
#include <math.h>
#undef exception

double 
HorizonM_BaC::Theta (const double h) const
{
  return Se (h) * (Theta_sat - Theta_rel) + Theta_rel;
}

double 
HorizonM_BaC::K (const double h) const
{
  return K_sat * pow (Se (h), (2 + 2.5 * lambda) /lambda);
}

double 
HorizonM_BaC::Cw2 (const double h) const
{
  if (h < h_b)
    return -(lambda*(pow(h_b / h, lambda - 1)*h_b) / pow(h, 2));
  else
    return 0.0;
}

double 
HorizonM_BaC::h (const double Theta) const
{
  if (Theta < Theta_sat)
    return h_b / pow((Theta_rel - Theta) / (Theta_rel - Theta_sat), 1 / lambda);
  else
    return h_b;
}

double 
HorizonM_BaC::Se (double h) const
{
  if (h < h_b)
    return pow (h_b / h, lambda);
  else
    return 1;
}

HorizonM_BaC::HorizonM_BaC (const AttributeList& al)
     : Theta_sat (al.number ("Theta_sat")),
       Theta_rel (al.number ("Theta_rel")),
       lambda (al.number ("lambda")),
       h_b (al.number ("h_b")),
       K_sat (al.number ("K_sat"))
{ }

HorizonM_BaC::~HorizonM_BaC ()
{ }

// Add the HorizonM_BaC syntax to the syntax table.

Horizon&
HorizonM_BaC::make (AttributeList& al)
{
  return *new HorizonM_BaC (al);
}

static struct HorizonM_BaCSyntax
{
  HorizonM_BaCSyntax ();
} horizonM_BaC_syntax;

HorizonM_BaCSyntax::HorizonM_BaCSyntax ()
{ 
  Syntax& syntax = *new Syntax ();
  AttributeList& alist = *new AttributeList ();
  syntax.add ("Theta_sat", Syntax::Number);
  syntax.add ("Theta_rel", Syntax::Number);
  syntax.add ("lambda", Syntax::Number);
  syntax.add ("h_b", Syntax::Number);
  syntax.add ("K_sat", Syntax::Number);

  Horizon::add_type ("M_BaC", alist, syntax, &HorizonM_BaC::make);
}
