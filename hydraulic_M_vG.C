// hydraulic_M_vG.C
//
// van Gebuchten retention curve model with Mualem theory.

#include "hydraulic.h"
#include "syntax.h"
#include "alist.h"
#include "common.h"
#include "csmp.h"

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
  if (h < 0)
    {
      const double Se_h = Se (h);
      return K_sat * sqrt (Se_h) * pow (1 - pow (1 - pow (Se_h, 1/m), m), 2);
    }
  else
    return K_sat;
}

double 
HydraulicM_vG::Cw2 (const double h) const
{
  if (h < 0)
    return - (  (Theta_sat - Theta_res)
	      * (m * (  pow (1 / (1 + pow (a * h, n)), m - 1)
		      * (n * (pow (a * h, n - 1) * a))))
	      / pow (1 + pow(a * h, n), 2));
  else
    return 0.0;
}

double 
HydraulicM_vG::h (const double Theta) const
{
  if (Theta < Theta_sat)
    return pow(pow(Theta_res / (Theta_res - Theta_sat) 
		   + Theta / (Theta_sat - Theta_res), -1 / m) - 1, 1 / n) / a;
  else
    return 0.0;
}

double 
HydraulicM_vG::M (double h) const
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
HydraulicM_vG::Se (double h) const
{
  if (h < 0)
    return pow (1 / (1 + pow (a * h, n)), m);
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
  Hydraulic::load_syntax (syntax, alist);
  syntax.add ("alpha", Syntax::Number, Syntax::Const);
  syntax.add ("n", Syntax::Number, Syntax::Const);
  syntax.add ("K_sat", Syntax::Number, Syntax::Const);

  Hydraulic::add_type ("M_vG", alist, syntax, &HydraulicM_vG::make);
}
