// hydraulic_M_vG.C
//
// van Genuchten retention curve model with Mualem theory.

#include "hydraulic.h"
#include "plf.h"

class HydraulicM_vG : public Hydraulic
{
  // Content.
  const double alpha;
  const double a;		// - alpha
  const double n;
  const double m;		// 1 - 1/n
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
  friend class HydraulicM_vGSyntax;
  static Hydraulic& make (const AttributeList& al);
  HydraulicM_vG (const AttributeList&);
public:
  ~HydraulicM_vG ();
};

double 
HydraulicM_vG::Theta (const double h) const
{
  return Se (h) * (Theta_sat - Theta_res) + Theta_res;
}

double 
HydraulicM_vG::K (const double h) const
{
  if (h < 0.0)
    {
      const double Se_h = Se (h);
      return K_sat * sqrt (Se_h)
	* pow (1.0 - pow (1.0 - pow (Se_h, 1.0/m), m), 2.0);
    }
  else
    return K_sat;
}

double 
HydraulicM_vG::Cw2 (const double h) const
{
  if (h < 0.0)
    return - (  (Theta_sat - Theta_res)
	      * (m * (  pow (1.0 / (1.0 + pow (a * h, n)), m - 1.0)
		      * (n * (pow (a * h, n - 1.0) * a))))
	      / pow (1.0 + pow(a * h, n), 2.0));
  else
    return 0.0;
}

double 
HydraulicM_vG::h (const double Theta) const
{
  assert (Theta_res <= Theta);
  if (Theta < Theta_sat)
    return pow(pow(Theta_res / (Theta_res - Theta_sat) 
		   + Theta / (Theta_sat - Theta_res), -1.0 / m)
	       - 1.0, 1.0 / n) / a;
  else
    return 0.0;
}

double 
HydraulicM_vG::M (double h) const
{
  // Use.
  static PLF plf;
  static bool initialized = false;
  if (!initialized)
    {
      K_to_M (plf, 500);
      initialized = true;
    }
  return plf (h);
}

double 
HydraulicM_vG::Se (double h) const
{
  if (h < 0.0)
    {
      const double Se_h = pow (1.0 / (1.0 + pow (a * h, n)), m);
      assert (Se_h >= 0.0);
      assert (Se_h <= 1.0);
      return Se_h;
    }
  else
    return 1.0;
}

HydraulicM_vG::HydraulicM_vG (const AttributeList& al)
  : Hydraulic (al),
    alpha (al.number ("alpha")),
    a (-alpha),
    n (al.number ("n")),
    m (1 - 1 / n),
    K_sat (al.number ("K_sat"))
{ }

HydraulicM_vG::~HydraulicM_vG ()
{ }

// Add the HydraulicM_vG syntax to the syntax table.

Hydraulic&
HydraulicM_vG::make (const AttributeList& al)
{
  return *new HydraulicM_vG (al);
}

static struct HydraulicM_vGSyntax
{
  HydraulicM_vGSyntax ();
} hydraulicM_vG_syntax;

HydraulicM_vGSyntax::HydraulicM_vGSyntax ()
{ 
  Syntax& syntax = *new Syntax ();
  AttributeList& alist = *new AttributeList ();
  alist.add ("description", 
	     "van Genuchten retention curve model with Mualem theory.");
  Hydraulic::load_syntax (syntax, alist);
  syntax.add ("alpha", "cm^-1", Syntax::Const,
	      "van Genuchten alpha.");
  syntax.add ("n", Syntax::None (), Syntax::Const,
	      "van Genuchten n.");
  syntax.add ("K_sat", "cm/h", Syntax::Const,
	      "Water conductivity of saturated soil.");

  Librarian<Hydraulic>::add_type ("M_vG", alist, syntax, &HydraulicM_vG::make);
}
