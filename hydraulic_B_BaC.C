// hydraulic_B_BaC.C
//
// Brooks and Corey retention curve model with Burdine theory.

#include "hydraulic.h"

class HydraulicB_BaC : public Hydraulic
{
  // Content.
  const double lambda;
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
  friend class HydraulicB_BaCSyntax;
  static Hydraulic& make (const AttributeList& al);
  HydraulicB_BaC (const AttributeList&);
public:
  ~HydraulicB_BaC ();
};

double 
HydraulicB_BaC::Theta (const double h) const
{
  return Se (h) * (Theta_sat - Theta_res) + Theta_res;
}

double 
HydraulicB_BaC::K (const double h) const
{
  return K_sat * pow (Se (h), (2 + 3 * lambda) / lambda);
}

double 
HydraulicB_BaC::Cw2 (const double h) const
{
  if (h < h_b)
    return (Theta_sat - Theta_res)
      * lambda * pow (h_b / h, lambda + 1) / -h_b;
  else
    return 0.0;
}

double 
HydraulicB_BaC::h (const double Theta) const
{
  if (Theta < Theta_sat)
    return h_b / pow((Theta_res - Theta) / (Theta_res - Theta_sat), 1 / lambda);
  else
    return h_b;
}

double 
HydraulicB_BaC::M (double h) const
{
  if (h <= h_b)
    return K_sat * (-h_b / (1 + 3*lambda)) * pow (h_b / h, 1 + 3*lambda);
  else
    return M (h_b) + K_sat * (h - h_b);
}

double 
HydraulicB_BaC::Se (double h) const
{
  if (h < h_b)
    return pow (h_b / h, lambda);
  else
    return 1;
}

HydraulicB_BaC::HydraulicB_BaC (const AttributeList& al)
  : Hydraulic (al),
    lambda (al.number ("lambda")),
    h_b (al.number ("h_b")),
    K_sat (al.number ("K_sat"))
{ }

HydraulicB_BaC::~HydraulicB_BaC ()
{ }

// Add the HydraulicB_BaC syntax to the syntax table.

Hydraulic&
HydraulicB_BaC::make (const AttributeList& al)
{
  return *new HydraulicB_BaC (al);
}

static struct HydraulicB_BaCSyntax
{
  HydraulicB_BaCSyntax ();
} hydraulicB_BaC_syntax;

HydraulicB_BaCSyntax::HydraulicB_BaCSyntax ()
{ 
  Syntax& syntax = *new Syntax ();
  AttributeList& alist = *new AttributeList ();
  Hydraulic::load_syntax (syntax, alist);
  syntax.add ("lambda", Syntax::Number, Syntax::Const);
  syntax.add ("h_b", Syntax::Number, Syntax::Const);
  syntax.add ("K_sat", Syntax::Number, Syntax::Const);

  Librarian<Hydraulic>::add_type ("B_BaC", alist, syntax, 
				  &HydraulicB_BaC::make);
}
