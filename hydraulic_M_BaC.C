// hydraulic_M_BaC.C
//
// Brooks and Corey retention curve model with Mualem theory.


#include "hydraulic.h"

class HydraulicM_BaC : public Hydraulic
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
    return (Theta_sat - Theta_res)
      * lambda * pow (h_b / h, lambda + 1) / -h_b;
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
  if (h <= h_b)
    return K_sat * (-h_b / (1 + 2.5*lambda)) * pow (h_b / h, 1 + 2.5*lambda);
  else
    return M (h_b) + K_sat * (h - h_b);
}

double 
HydraulicM_BaC::Se (double h) const
{
  double result;

  if (h < h_b)
    result = pow (h_b / h, lambda);
  else
    result = 1;
  
  if (result <= 0.0)
    {
      CERR << "Se (" << h << ") = " << result << "; lambda = " << lambda
	   << "; h_b = " << h_b << "\n;";
    }
  return result;
}

HydraulicM_BaC::HydraulicM_BaC (const AttributeList& al)
  : Hydraulic (al),
    lambda (al.number ("lambda")),
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
  alist.add ("description", 
	     "Brooks and Corey retention curve model with Mualem theory.");
  Hydraulic::load_syntax (syntax, alist);
  syntax.add ("lambda", Syntax::None (), Syntax::Const,
	      "Pore size index.");
  syntax.add ("h_b", "cm", Syntax::Const,
	      "Bubbling pressure.");
  syntax.add ("K_sat", "cm/h", Syntax::Const,
	      "Water conductivity of saturated soil.");

  Librarian<Hydraulic>::add_type ("M_BaC", alist, syntax, 
				  &HydraulicM_BaC::make);
}
