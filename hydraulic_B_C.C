// hydraulic_B_C.C
//
// Campbell retention curve model with Burdine theory.

#include "hydraulic.h"
#include "syntax.h"
#include "alist.h"
#include "common.h"

class HydraulicB_C : public Hydraulic
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
  friend class HydraulicB_CSyntax;
  static Hydraulic& make (AttributeList& al);
  HydraulicB_C (const AttributeList&);
public:
  ~HydraulicB_C ();
};

double 
HydraulicB_C::Theta (const double h) const
{
  return Sr (h) * Theta_sat;
}

double 
HydraulicB_C::K (const double h) const
{
  return K_sat * pow (Sr (h), (2 + 3/b) * b);
}

double 
HydraulicB_C::Cw2 (const double h) const
{
  if (h < h_b)
    return -(Theta_sat*(pow(h_b / h, 1 / b - 1)*h_b) / (pow(h, 2)*b));
  else
    return 0.0;
}

double 
HydraulicB_C::h (const double Theta) const
{
  if (Theta < Theta_sat)
    return h_b / pow(Theta / Theta_sat, b);
  else
    return h_b;
}

double 
HydraulicB_C::M (double h) const
{
  if (h < h_b)
    return K_sat * (-h_b / (1 + 3/b)) * pow (h_b / h, 1 + 3/b);
  else
    return K_sat * h;
}

double 
HydraulicB_C::Sr (double h) const
{
  if (h < h_b)
    return pow (h_b / h, 1 / b);
  else
    return 1;
}

HydraulicB_C::HydraulicB_C (const AttributeList& al)
  : Hydraulic (al),
    h_b (al.number ("h_b")),
    b (al.number ("b")),
    K_sat (al.number ("K_sat"))
{ }

HydraulicB_C::~HydraulicB_C ()
{ }

// Add the HydraulicB_C syntax to the syntax table.

Hydraulic&
HydraulicB_C::make (AttributeList& al)
{
  return *new HydraulicB_C (al);
}

static struct HydraulicB_CSyntax
{
  HydraulicB_CSyntax ();
} hydraulicB_C_syntax;

HydraulicB_CSyntax::HydraulicB_CSyntax ()
{ 
  Syntax& syntax = *new Syntax ();
  AttributeList& alist = *new AttributeList ();
  Hydraulic::load_syntax (syntax, alist);
  syntax.add ("h_b", Syntax::Number, Syntax::Const);
  syntax.add ("b", Syntax::Number, Syntax::Const);
  syntax.add ("K_sat", Syntax::Number, Syntax::Const);

  Hydraulic::add_type ("B_C", alist, syntax, &HydraulicB_C::make);
}
