// hydraulic_M_BaC.C
//
// Brooks and Corey retention curve model with Mualem theory.


#include "hydraulic.h"

class HydraulicM_BaC : public Hydraulic
{
  // Content.
  const double h_b;
  const double K_sat;

  // Use.
public:
  double Theta (double h) const;
  double K (double h) const;
  double Cw2 (double h) const;
  double h (double Theta) const;
  double M (double h) const;
private:
  double Se (double h) const;
  
  // Create and Destroy.
private:
  friend class HydraulicM_BaCSyntax;
  static Hydraulic& make (const AttributeList& al);
  HydraulicM_BaC (const AttributeList&);
public:
  ~HydraulicM_BaC ();
};

double 
HydraulicM_BaC::Theta (const double h) const
{
  return Se (h) * (Theta_sat - Theta_res) + Theta_res;
}

double 
HydraulicM_BaC::K (const double h) const
{
  return K_sat * pow (Se (h), (2 + 2.5 * lambda) /lambda);
}

double 
HydraulicM_BaC::Cw2 (const double h) const
{
  if (h < h_b)
    return -(lambda*(pow(h_b / h, lambda - 1)*h_b) / pow(h, 2));
  else
    return 0.0;
}

double 
HydraulicM_BaC::h (const double Theta) const
{
  if (Theta < Theta_sat)
    return h_b / pow((Theta_res - Theta) / (Theta_res - Theta_sat), 1 / lambda);
  else
    return h_b;
}

double 
HydraulicM_BaC::M (double h) const
{
  if (h < h_b)
    return K_sat * (-h_b / (1 + 2.5*lambda)) * pow (h_b / h, 1 + 2.5*lambda);
  else
    return K_sat * h;
}

double 
HydraulicM_BaC::Se (double h) const
{
  if (h < h_b)
    return pow (h_b / h, lambda);
  else
    return 1;
}

HydraulicM_BaC::HydraulicM_BaC (const AttributeList& al)
  : Hydraulic (al),
    h_b (al.number ("h_b")),
    K_sat (al.number ("K_sat"))
{ }

HydraulicM_BaC::~HydraulicM_BaC ()
{ }

// Add the HydraulicM_BaC syntax to the syntax table.

Hydraulic&
HydraulicM_BaC::make (const AttributeList& al)
{
  return *new HydraulicM_BaC (al);
}

static struct HydraulicM_BaCSyntax
{
  HydraulicM_BaCSyntax ();
} hydraulicM_BaC_syntax;

HydraulicM_BaCSyntax::HydraulicM_BaCSyntax ()
{ 
  Syntax& syntax = *new Syntax ();
  AttributeList& alist = *new AttributeList ();
  Hydraulic::load_syntax (syntax, alist);
  syntax.add ("h_b", Syntax::Number, Syntax::Const);
  syntax.add ("K_sat", Syntax::Number, Syntax::Const);

  Librarian<Hydraulic>::add_type ("M_BaC", alist, syntax, 
				  &HydraulicM_BaC::make);
}
