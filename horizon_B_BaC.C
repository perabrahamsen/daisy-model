// horizon_B_BaC.C

#include "horizon.h"
#include "syntax.h"
#include "alist.h"
#include "common.h"

class HorizonB_BaC : public Horizon
{
  // Content.
  const double Theta_sat;
  const double Theta_rel;
  const double lambda;
  const double h_b;
  const double K_sat;

  // Use.
public:
  double Theta (double h) const;
  double K (double h) const;
  double Cw2 (double h) const;
  double h (double Theta) const;
private:
  double Se (double h) const;
  
  // Create and Destroy.
private:
  friend class HorizonB_BaCSyntax;
  static Horizon& make (AttributeList& al);
  HorizonB_BaC (const AttributeList&);
public:
  ~HorizonB_BaC ();
};

double 
HorizonB_BaC::Theta (const double h) const
{
  return Se (h) * (Theta_sat - Theta_rel) + Theta_rel;
}

double 
HorizonB_BaC::K (const double h) const
{
  return K_sat * pow (Se (h), (2 + 3 * lambda) / lambda);
}

double 
HorizonB_BaC::Cw2 (const double h) const
{
  if (h < h_b)
    return -(lambda*(pow(h_b / h, lambda - 1)*h_b) / pow(h, 2));
  else
    return 0.0;
}

double 
HorizonB_BaC::h (const double Theta) const
{
  if (Theta < Theta_sat)
    return h_b / pow((Theta_rel - Theta) / (Theta_rel - Theta_sat), 1 / lambda);
  else
    return h_b;
}

double 
HorizonB_BaC::Se (double h) const
{
  if (h < h_b)
    return pow (h_b / h, lambda);
  else
    return 1;
}

HorizonB_BaC::HorizonB_BaC (const AttributeList& al)
     : Theta_sat (al.number ("Theta_sat")),
       Theta_rel (al.number ("Theta_rel")),
       lambda (al.number ("lambda")),
       h_b (al.number ("h_b")),
       K_sat (al.number ("K_sat"))
{ }

HorizonB_BaC::~HorizonB_BaC ()
{ }

// Add the HorizonB_BaC syntax to the syntax table.

Horizon&
HorizonB_BaC::make (AttributeList& al)
{
  return *new HorizonB_BaC (al);
}

static struct HorizonB_BaCSyntax
{
  HorizonB_BaCSyntax ();
} horizonB_BaC_syntax;

HorizonB_BaCSyntax::HorizonB_BaCSyntax ()
{ 
  Syntax& syntax = *new Syntax ();
  AttributeList& alist = *new AttributeList ();
  syntax.add ("Theta_sat", Syntax::Number);
  syntax.add ("Theta_rel", Syntax::Number);
  syntax.add ("lambda", Syntax::Number);
  syntax.add ("h_b", Syntax::Number);
  syntax.add ("K_sat", Syntax::Number);

  Horizon::add_type ("B_BaC", alist, syntax, &HorizonB_BaC::make);
}
