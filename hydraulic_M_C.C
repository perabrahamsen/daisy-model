// hydraulic_M_C.C
//
// Campbell retention curve model with Mualem theory.

#include "hydraulic.h"
#include "syntax.h"
#include "alist.h"
#include "common.h"

class HydraulicM_C : public Hydraulic
{
  // Content.
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
  friend class HydraulicM_CSyntax;
  static Hydraulic& make (const AttributeList& al);
  HydraulicM_C (const AttributeList&);
public:
  ~HydraulicM_C ();
};

double 
HydraulicM_C::Theta (const double h) const
{
  return Sr (h) * Theta_sat;
}

double 
HydraulicM_C::K (const double h) const
{
  return K_sat * pow (Sr (h), (2 + 2.5/b) * b);
}

double 
HydraulicM_C::Cw2 (const double h) const
{
  if (h < h_b)
    return -(Theta_sat*(pow(h_b / h, 1 / b - 1)*h_b) / (pow(h, 2)*b));
  else
    return 0.0;
}

double 
HydraulicM_C::h (const double Theta) const
{
  if (Theta < Theta_sat)
    return h_b / pow(Theta / Theta_sat, b);
  else
    return h_b;
}

double 
HydraulicM_C::M (double h) const
{
  if (h < h_b)
    return K_sat * (-h_b / (1 + 2.5/b)) * pow (h_b / h, 1 + 2.5/b);
  else
    return K_sat * h;
}

double 
HydraulicM_C::Sr (double h) const
{
  if (h < h_b)
    return pow (h_b / h, 1 / b);
  else
    return 1;
}

HydraulicM_C::HydraulicM_C (const AttributeList& al)
  : Hydraulic (al),
    h_b (al.number ("h_b")),
    b (al.number ("b")),
    K_sat (al.number ("K_sat"))
{ }

HydraulicM_C::~HydraulicM_C ()
{ }

// Add the HydraulicM_C syntax to the syntax table.

Hydraulic&
HydraulicM_C::make (const AttributeList& al)
{
  return *new HydraulicM_C (al);
}

static struct HydraulicM_CSyntax
{
  HydraulicM_CSyntax ();
} hydraulicM_C_syntax;

HydraulicM_CSyntax::HydraulicM_CSyntax ()
{ 
  Syntax& syntax = *new Syntax ();
  AttributeList& alist = *new AttributeList ();
  Hydraulic::load_syntax (syntax, alist);
  syntax.add ("h_b", Syntax::Number, Syntax::Const);
  syntax.add ("b", Syntax::Number, Syntax::Const);
  syntax.add ("K_sat", Syntax::Number, Syntax::Const);

  Hydraulic::add_type ("M_C", alist, syntax, &HydraulicM_C::make);
}
