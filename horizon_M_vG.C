// horizon_M_vG.C
//
// van Gebuchten retention curve model with Mualem theory.

#include "horizon.h"
#include "syntax.h"
#include "alist.h"
#include "common.h"
#include "csmp.h"

class HorizonM_vG : public Horizon
{
  // Content.
  const double Theta_sat;
  const double Theta_res_;
  const double alpha;
  const double a;		// - alpha
  const double n;
  const double m;		// 1 - 1/n
  const double K_sat;

  // Use.
public:
  double Theta (double h) const;
  double Theta_res () const;
  double K (double h) const;
  double Cw2 (double h) const;
  double h (double Theta) const;
  double M (double h) const;
private:
  double Se (double h) const;
  
  // Create and Destroy.
private:
  friend class HorizonM_vGSyntax;
  static Horizon& make (AttributeList& al);
  HorizonM_vG (const AttributeList&);
public:
  ~HorizonM_vG ();
};

double 
HorizonM_vG::Theta (const double h) const
{
  return Se (h) * (Theta_sat - Theta_res_) + Theta_res_;
}

double 
HorizonM_vG::Theta_res () const
{
  return Theta_res_;
}

double 
HorizonM_vG::K (const double h) const
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
HorizonM_vG::Cw2 (const double h) const
{
  if (h < 0)
    return - (  (Theta_sat - Theta_res_)
	      * (m * (  pow (1 / (1 + pow (a * h, n)), m - 1)
		      * (n * (pow (a * h, n - 1) * a))))
	      / pow (1 + pow(a * h, n), 2));
  else
    return 0.0;
}

double 
HorizonM_vG::h (const double Theta) const
{
  if (Theta < Theta_sat)
    return pow(pow(Theta_res_ / (Theta_res_ - Theta_sat) 
		   + Theta / (Theta_sat - Theta_res_), -1 / m) - 1, 1 / n) / a;
  else
    return 0.0;
}

double 
HorizonM_vG::M (double h) const
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
HorizonM_vG::Se (double h) const
{
  return pow (1 / (1 + pow (a * h, n)), m);
}

HorizonM_vG::HorizonM_vG (const AttributeList& al)
     : Theta_sat (al.number ("Theta_sat")),
       Theta_res_ (al.number ("Theta_res")),
       alpha (al.number ("alpha")),
       a (-alpha),
       n (al.number ("n")),
       m (1 - 1 / n),
       K_sat (al.number ("K_sat"))
{ }

HorizonM_vG::~HorizonM_vG ()
{ }

// Add the HorizonM_vG syntax to the syntax table.

Horizon&
HorizonM_vG::make (AttributeList& al)
{
  return *new HorizonM_vG (al);
}

static struct HorizonM_vGSyntax
{
  HorizonM_vGSyntax ();
} horizonM_vG_syntax;

HorizonM_vGSyntax::HorizonM_vGSyntax ()
{ 
  Syntax& syntax = *new Syntax ();
  AttributeList& alist = *new AttributeList ();
  syntax.add ("Theta_sat", Syntax::Number);
  syntax.add ("Theta_res", Syntax::Number);
  syntax.add ("alpha", Syntax::Number);
  syntax.add ("n", Syntax::Number);
  syntax.add ("K_sat", Syntax::Number);

  Horizon::add_type ("M_vG", alist, syntax, &HorizonM_vG::make);
}
