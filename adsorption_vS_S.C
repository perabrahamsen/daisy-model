// adsorption_vS_S.C

#include "adsorption.h"
#include "soil.h"
#include "mathlib.h"

class Adsorption_vS_S : public Adsorption
{
  // Simulation.
public:
  double C_to_A (const Soil& soil, int i, double C) const;
  double C_to_M (const Soil&, double Theta, int, double C) const;
  double M_to_C (const Soil&, double Theta, int, double M) const;

  // Chemical soil constants.
  double K_planar () const
    { return 6.3e-5; }		// [g/cm³]
  double K_edge () const
    { return 1.372e-5; }	// [g/cm³]
  double v_planar (const Soil& soil, int i) const
    { 
      // Maximum specific absorbtion [g / g clay]
      const double S_planar = 5.964e-3;
      const double porosity = soil.Theta (i, 0.0, 0.0);
      const double clay = soil.clay (i);
      return S_planar * clay * soil.dry_bulk_density (i) * (1.0 - porosity); 
    }
  double v_edge (const Soil& soil, int i) const
    {
      const double S_edge = 0.308e-3;	// Same for edges. [g / g clay]
      const double porosity = soil.Theta (i, 0.0, 0.0);
      const double clay = soil.clay (i);
      
      return S_edge * clay * soil.dry_bulk_density (i) * (1.0 - porosity); 
    }

  // Create.
public:
  Adsorption_vS_S (const AttributeList& al)
    : Adsorption (al.name ("type"))
    { }
};

double
Adsorption_vS_S::C_to_A (const Soil& soil, int i, double C) const
{
  return (v_planar (soil, i) * C) / (K_planar () + C)
    + (v_edge (soil, i) * C) / (K_edge () + C);
}

double 
Adsorption_vS_S::C_to_M (const Soil& soil, double Theta, int i, double C) const
{
  return C_to_A (soil, i, C) + Theta * C;
}

double 
Adsorption_vS_S::M_to_C (const Soil& soil, double Theta, int i, double M) const
{
  const double ve = v_edge (soil, i); 
  const double Ke = K_edge ();
  const double vp = v_planar (soil, i);
  const double Kp = K_planar ();

  double C;

  if (M < 1e-6 * min (Ke, Kp))
    // There are numerical problems in the general solution for small M. 
    C = M / (Theta + ve  / Ke + vp / Kp);
  else
    {
      assert (Theta > 0.0);
      const double a = Theta;
      const double b = Theta * (Kp + Ke) + vp + ve - M;
      const double c = vp * Ke + ve * Kp - M * (Kp + Ke) + Kp * Ke * Theta;
      const double d = - M * Kp * Ke;
    
      C = single_positive_root_of_cubic_equation (a, b, c, d);
      assert (approximate (M, C_to_M (soil, Theta, i, C)));
    }
  return C;
}

static struct Adsorption_vS_SSyntax
{
  static Adsorption& make (const AttributeList& al)
  {
    return *new Adsorption_vS_S (al);
  }

  Adsorption_vS_SSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "\
Model by van Schouwenberg and Schuffelen, 1963, with\n\
parameterization by Hansen et.al., 1990.");
    Librarian<Adsorption>::add_type ("vS_S", alist, syntax, &make);
  }
} Adsorption_vS_S_syntax;
