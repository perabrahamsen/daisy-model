// horizon_M_vG.C

#include "horizon_M_vG.h"
#include "syntax.h"
#include "alist.h"

#define exception BUG_exception
#include <math.h>
#undef exception

double 
HorizonM_vG::Theta (const double h) const
{
  return Se (h) * (Theta_sat - Theta_ref) + Theta_ref;
}

double 
HorizonM_vG::K (const double h) const
{
  if (h < 0)
    {
      const double Se_h = Se (h);
      return K_sat * sqrt (Se_h) * pow (1 - pow (1 - pow (Se_h, 1/m), m), 2);
    }
  else
    return K_sat;
}

double 
HorizonM_vG::Cw2 (const double h) const
{
  if (h < 0)
    return - (  (Theta_sat - Theta_ref)
	      * (m * (  pow (1 / (1 + pow (a * h, n)), m - 1)
		      * (n * (pow (a * h, n - 1) * a))))
	      / pow (1 + pow(a * h, n), 2));
  else
    return 0.0;
}

double 
HorizonM_vG::h (const double Theta) const
{
  if (Theta < Theta_sat)
    return pow(pow(Theta_ref / (Theta_ref - Theta_sat) 
		   + Theta / (Theta_sat - Theta_ref), -1 / m) - 1, 1 / n) / a;
  else
    return 0.0;
}

double 
HorizonM_vG::Se (double h) const
{
  return pow (1 / (1 + pow (a * h, n)), m);
}

HorizonM_vG::HorizonM_vG (const AttributeList& al)
     : Theta_sat (al.number ("Theta_sat")),
       Theta_ref (al.number ("Theta_ref")),
       alpha (al.number ("alpha")),
       a (-alpha),
       n (al.number ("n")),
       m (1 - 1 / n),
       K_sat (al.number ("K_sat"))
{ }

HorizonM_vG::~HorizonM_vG ()
{ }

// Add the HorizonM_vG syntax to the syntax table.

Horizon&
HorizonM_vG::make (AttributeList& al)
{
  return *new HorizonM_vG (al);
}

static struct HorizonM_vGSyntax
{
  HorizonM_vGSyntax ();
} horizonM_vG_syntax;

HorizonM_vGSyntax::HorizonM_vGSyntax ()
{ 
  Syntax& syntax = *new Syntax ();
  AttributeList& alist = *new AttributeList ();
  syntax.add ("Theta_sat", Syntax::Number);
  syntax.add ("Theta_ref", Syntax::Number);
  syntax.add ("alpha", Syntax::Number);
  syntax.add ("n", Syntax::Number);
  syntax.add ("K_sat", Syntax::Number);

  Horizon::add_type ("M_vG", alist, syntax, &HorizonM_vG::make);
}
