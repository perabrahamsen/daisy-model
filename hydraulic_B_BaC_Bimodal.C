// hydraulic_B_BaC_Bimodal.C
//
// Brooks and Corey retention curve model with Burdine theory.
// Bimodal hydraulic conductivity curve.

#include "hydraulic.h"
#include "mathlib.h"

class HydraulicB_BaC_Bimodal : public Hydraulic
{
  // Content.
  const double lambda;
  const double h_b;
  const double Theta_b;
  const double K_b;
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
  friend class HydraulicB_BaC_BimodalSyntax;
  static Hydraulic& make (const AttributeList& al);
  HydraulicB_BaC_Bimodal (const AttributeList&);
public:
  ~HydraulicB_BaC_Bimodal ();
};

double
HydraulicB_BaC_Bimodal::Theta (const double h) const
{
  if (h < h_b)
    return Se (h) * (Theta_b - Theta_res) + Theta_res;
  else if (h < 0.0)
    return (Theta_sat - Theta_b) / (-h_b) * (h - h_b) + Theta_b;
  else
    return Theta_sat;
}

double
HydraulicB_BaC_Bimodal::K (const double h) const
{
  if (h < h_b)
    return K_b * pow (Se (h), (2 + 3 * lambda) / lambda);
  else
    return (K_sat - K_b) / (-h_b) * (h - h_b) + K_b;
}

double
HydraulicB_BaC_Bimodal::Cw2 (const double h) const
{
  if (h < h_b)
    return (Theta_b - Theta_res)
      * lambda * pow (h_b / h, lambda + 1) / -h_b;
  else if (h < 0)
    return (Theta_sat - Theta_b) / (-h_b);
  else
    return 0.0;
}

double
HydraulicB_BaC_Bimodal::h (const double Theta) const
{
  assert (Theta > Theta_res);
  if (Theta < Theta_b)
    return h_b / pow((Theta - Theta_res) / (Theta_b - Theta_res), 1.0 / lambda);
  else if (Theta < Theta_sat)
    return h_b * ( 1 - (Theta - Theta_b) / (Theta_sat - Theta_b));
  else
    return 0;
}

double
HydraulicB_BaC_Bimodal::M (double h) const
{
  if (h <= h_b)
    return K_b * (-h_b / (1 + 3*lambda)) * pow (h_b / h, 1 + 3*lambda);
  else
    return M (h_b) + K_b * (h - h_b);
}

double
HydraulicB_BaC_Bimodal::Se (double h) const
{
  if (h < h_b)
    return pow (h_b / h, lambda);
  else
    return 1;
}

HydraulicB_BaC_Bimodal::HydraulicB_BaC_Bimodal (const AttributeList& al)
  : Hydraulic (al),
    lambda (al.number ("lambda")),
    h_b (al.number ("h_b")),
    Theta_b (al.number ("Theta_b")),
    K_b (al.number ("K_b")),
    K_sat (al.number ("K_sat"))
{ }

HydraulicB_BaC_Bimodal::~HydraulicB_BaC_Bimodal ()
{ }

// Add the HydraulicB_BaC_Bimodal syntax to the syntax table.

Hydraulic&
HydraulicB_BaC_Bimodal::make (const AttributeList& al)
{
  return *new HydraulicB_BaC_Bimodal (al);
}

static struct HydraulicB_BaC_BimodalSyntax
{
  HydraulicB_BaC_BimodalSyntax ();
} HydraulicB_BaC_Bimodal_syntax;

HydraulicB_BaC_BimodalSyntax::HydraulicB_BaC_BimodalSyntax ()
{
  Syntax& syntax = *new Syntax ();
  AttributeList& alist = *new AttributeList ();
  alist.add ("description", 
	     "Brooks and Corey retention curve model with Burdine theory.\n\
Bimodal hydraulic conductivity curve.");
  Hydraulic::load_syntax (syntax, alist);
  syntax.add ("lambda", Syntax::None (), Syntax::Const,
	      "Pore size index.");
  syntax.add ("h_b", "cm", Syntax::Const,
	      "Bubbling pressure.");
  syntax.add ("Theta_b", Syntax::None (), Syntax::Const,
	      "Water content at `h_b'.");
  syntax.add ("K_b", "cm/h", Syntax::Const,
	      "Water conductivity at `h_b'.");
  syntax.add ("K_sat", "cm/h", Syntax::Const,
	      "Water conductivity of saturated soil.");

  Librarian<Hydraulic>::add_type ("B_BaC_Bimodal", alist, syntax,
				  &HydraulicB_BaC_Bimodal::make);
}
