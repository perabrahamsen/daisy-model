// horizon_B_C.C

#include "horizon.h"
#include "syntax.h"
#include "alist.h"
#include "common.h"

class HorizonB_C : public Horizon
{
  // Content.
  const double Theta_sat;
  const double h_b;
  const double b;
  const double K_sat;

  // Use.
public:
  double Theta (double h) const;
  double K (double h) const;
  double Cw2 (double h) const;
  double h (double Theta) const;
  double M (double h) const;
private:
  double Sr (double h) const;
  
  // Create and Destroy.
private:
  friend class HorizonB_CSyntax;
  static Horizon& make (AttributeList& al);
  HorizonB_C (const AttributeList&);
public:
  ~HorizonB_C ();
};

double 
HorizonB_C::Theta (const double h) const
{
  return Sr (h) * Theta_sat;
}

double 
HorizonB_C::K (const double h) const
{
  return K_sat * pow (Sr (h), (2 + 3/b) * b);
}

double 
HorizonB_C::Cw2 (const double h) const
{
  if (h < h_b)
    return -(Theta_sat*(pow(h_b / h, 1 / b - 1)*h_b) / (pow(h, 2)*b));
  else
    return 0.0;
}

double 
HorizonB_C::h (const double Theta) const
{
  if (Theta < Theta_sat)
    return h_b / pow(Theta / Theta_sat, b);
  else
    return h_b;
}

double 
HorizonB_C::M (double h) const
{
  if (h < h_b)
    return K_sat * (-h_b / (1 + 3/b)) * pow (h_b / h, 1 + 3/b);
  else
    return K_sat * h;
}

double 
HorizonB_C::Sr (double h) const
{
  if (h < h_b)
    return pow (h_b / h, 1 / b);
  else
    return 1;
}

HorizonB_C::HorizonB_C (const AttributeList& al)
     : Theta_sat (al.number ("Theta_sat")),
       h_b (al.number ("h_b")),
       b (al.number ("b")),
       K_sat (al.number ("K_sat"))
{ }

HorizonB_C::~HorizonB_C ()
{ }

// Add the HorizonB_C syntax to the syntax table.

Horizon&
HorizonB_C::make (AttributeList& al)
{
  return *new HorizonB_C (al);
}

static struct HorizonB_CSyntax
{
  HorizonB_CSyntax ();
} horizonB_C_syntax;

HorizonB_CSyntax::HorizonB_CSyntax ()
{ 
  Syntax& syntax = *new Syntax ();
  AttributeList& alist = *new AttributeList ();
  syntax.add ("Theta_sat", Syntax::Number);
  syntax.add ("h_b", Syntax::Number);
  syntax.add ("b", Syntax::Number);
  syntax.add ("K_sat", Syntax::Number);

  Horizon::add_type ("B_C", alist, syntax, &HorizonB_C::make);
}
