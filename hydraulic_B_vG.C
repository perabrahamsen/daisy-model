// hydraulic_B_vG.C
//
// van Genuchten retention curve model with Burdine theory.

#include "hydraulic.h"
#include "csmp.h"

class HydraulicB_vG : public Hydraulic
{
  // Content.
  const double alpha;
  const double a;		// - alpha
  const double n;
  const double m;		// 1 - 2/n
  const double K_sat;

  // Use.
public:
  double Theta (double h) const;
  double K (double h) const;
  double Cw2 (double h) const;
  double h (double Theta) const;
  double M (double Theta) const;
private:
  double Se (double h) const;
  
  // Create and Destroy.
private:
  friend class HydraulicB_vGSyntax;
  static Hydraulic& make (const AttributeList& al);
  HydraulicB_vG (const AttributeList&);
public:
  ~HydraulicB_vG ();
};

double 
HydraulicB_vG::Theta (const double h) const
{
  return Se (h) * (Theta_sat - Theta_res) + Theta_res;
}

double 
HydraulicB_vG::K (const double h) const
{
  if (h < 0)
    {
      const double Se_h = Se (h);
      return K_sat * pow (Se_h, 2) * (1 - pow (1 - pow (Se_h, 1/m), m));
    }
  else
    return K_sat;
}

double 
HydraulicB_vG::Cw2 (const double h) const
{
  if (h < 0)
    return - ((Theta_sat - Theta_res)
	      * (m * (pow (1.0 / (1 + pow (a * h, n)), m - 1)
		      * (n * (pow (a * h, n - 1) * a))))
	      / pow (1 + pow(a * h, n), 2));
  else
    return 0.0;
}

double 
HydraulicB_vG::h (const double Theta) const
{
  if (Theta < Theta_sat)
    return pow(pow(Theta_res / (Theta_res - Theta_sat) 
		   + Theta / (Theta_sat - Theta_res), -1 / m) - 1, 1 / n) / a;
  else
    return 0.0;
}

double 
HydraulicB_vG::M (double h) const
{
  // Use.
  static CSMP csmp;
  static bool initialized = false;
  if (!initialized)
    {
      K_to_M (csmp, 500);
      initialized = true;
    }
  return csmp (h);
}

double 
HydraulicB_vG::Se (double h) const
{
  return pow (1 / (1 + pow (a * h, n)), m);
}

HydraulicB_vG::HydraulicB_vG (const AttributeList& al)
  : Hydraulic (al),
    alpha (al.number ("alpha")),
    a (-alpha),
    n (al.number ("n")),
    m (1 - 2 / n),
    K_sat (al.number ("K_sat"))
{ }

HydraulicB_vG::~HydraulicB_vG ()
{ }

// Add the HydraulicB_vG syntax to the syntax table.

Hydraulic&
HydraulicB_vG::make (const AttributeList& al)
{
  return *new HydraulicB_vG (al);
}

static struct HydraulicB_vGSyntax
{
  HydraulicB_vGSyntax ();
} hydraulicB_vG_syntax;

HydraulicB_vGSyntax::HydraulicB_vGSyntax ()
{ 
  Syntax& syntax = *new Syntax ();
  AttributeList& alist = *new AttributeList ();
  alist.add ("description", 
	     "van Genuchten retention curve model with Burdine theory.");
  Hydraulic::load_syntax (syntax, alist);
  syntax.add ("alpha", "cm^-1", Syntax::Const,
	      "van Genuchten alpha.");
  syntax.add ("n", Syntax::None (), Syntax::Const,
	      "van Genuchten n.");
  syntax.add ("K_sat", "cm/h", Syntax::Const,
	      "Water conductivity of saturated soil.");

  Librarian<Hydraulic>::add_type ("B_vG", alist, syntax, &HydraulicB_vG::make);
}
