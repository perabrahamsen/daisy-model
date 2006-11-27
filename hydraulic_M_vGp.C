// hydraulic_M_vGp.C
// 
// Copyright 1996-2001, 2003 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001, 2003 KVL.
//
// This file is part of Daisy.
// 
// Daisy is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser Public License as published by
// the Free Software Foundation; either version 2.1 of the License, or
// (at your option) any later version.
// 
// Daisy is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser Public License for more details.
// 
// You should have received a copy of the GNU Lesser Public License
// along with Daisy; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

//
// van Genuchten retention curve model with Mualem theory and power function..

#include "hydraulic.h"
#include "block.h"
#include "alist.h"
#include "plf.h"
#include "mathlib.h"
#include "check.h"

class HydraulicM_vGp : public Hydraulic
{
  // Content.
  const double alpha;
  const double a;		// - alpha
  const double n;
  const double m;		// 1 - 1/n
  const double l;               // tortuosity parameter
  mutable PLF M_;
  // Power function.
  const double h_m;		// matrix/macro boundary.
  const double f;		// shape parameter.
  const double p_m_matrix;	// power function for matrix flow.

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
  friend class HydraulicM_vGpSyntax;
  static Hydraulic& make (Block& al);
  HydraulicM_vGp (Block&);
public:
  ~HydraulicM_vGp ();
};

double 
HydraulicM_vGp::Theta (const double h) const
{
  return Se (h) * (Theta_sat - Theta_res) + Theta_res;
}

double 
HydraulicM_vGp::K (const double h) const
{
  if (h < 0.0)
    {
      const double Se_h = Se (h);
      const double M_vG_K =  K_sat * pow (Se_h, l)
	* pow (1.0 - pow (1.0 - pow (Se_h, 1.0/m), m), 2.0);
      const double X = 1.0; 	// [cm^-1]
      const double p_m = (h > h_m)
	? pow (1.0 / (-h * X + 1.0), f)
	: p_m_matrix;
      return M_vG_K * p_m;
    }
  else
    return K_sat;
}

double 
HydraulicM_vGp::Cw2 (const double h) const
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
HydraulicM_vGp::h (const double Theta) const
{
  daisy_assert (Theta_res <= Theta);
  if (Theta < Theta_sat)
    return pow(pow(Theta_res / (Theta_res - Theta_sat) 
		   + Theta / (Theta_sat - Theta_res), -1.0 / m)
	       - 1.0, 1.0 / n) / a;
  else
    return 0.0;
}

double 
HydraulicM_vGp::M (double h) const
{
  if (M_.size () == 0)
    K_to_M (M_, 500);

  return M_ (h);
}

double 
HydraulicM_vGp::Se (double h) const
{
  if (h < 0.0)
    {
      const double Se_h = pow (1.0 / (1.0 + pow (a * h, n)), m);
      daisy_assert (Se_h >= 0.0);
      daisy_assert (Se_h <= 1.0);
      return Se_h;
    }
  else
    return 1.0;
}

HydraulicM_vGp::HydraulicM_vGp (Block& al)
  : Hydraulic (al),
    alpha (al.number ("alpha")),
    a (-alpha),
    n (al.number ("n")),
    m (1 - 1 / n),
    l (al.number ("l")),
    M_ (),
    h_m (al.number ("h_m")),
    f (al.number ("f")),
    p_m_matrix (pow (1.0 / (-h_m * 1.0 + 1.0), f))
{ }

HydraulicM_vGp::~HydraulicM_vGp ()
{ }

// Add the HydraulicM_vGp syntax to the syntax table.

Hydraulic&
HydraulicM_vGp::make (Block& al)
{
  return *new HydraulicM_vGp (al);
}

static struct HydraulicM_vGpSyntax
{
  HydraulicM_vGpSyntax ();
} hydraulicM_vGp_syntax;

HydraulicM_vGpSyntax::HydraulicM_vGpSyntax ()
{ 
  Syntax& syntax = *new Syntax ();
  AttributeList& alist = *new AttributeList ();
  alist.add ("description", 
	     "van Genuchten retention curve model with Mualem theory.\n\
A p_m (h) function is multiplied to the conductivity to simulate the\n\
change near maropores.\n\
\n\
p_m = (1/(-h X + 1))^f; h > h_m\n\
p_m = (1/(-h_m X + 1))^f; h <= h_m\n\
X = 1 cm^-1\n\
\n\
This function is described in ``Soil hydraulic properties near\n\
saturation, an improved model'' by Boergesen et. al, submitted to\n\
Water Resources Research 2003.");
  Hydraulic::load_Theta_res (syntax, alist);
  syntax.add ("K_sat", "cm/h", Check::non_negative (), Syntax::OptionalConst,
	      "Water conductivity of saturated soil.");
  syntax.add ("alpha", "cm^-1", Syntax::Const,
	      "van Genuchten alpha.");
  syntax.add ("n", Syntax::None (), Syntax::Const,
	      "van Genuchten n.");
  syntax.add ("l", Syntax::None (), Syntax::Const,
	      "tortuosity parameter.");
  alist.add ("l", 0.5);
  syntax.add ("h_m", "cm", Check::negative (), Syntax::Const,
	      "Pressure point of chance between matrix and macropores.");
  syntax.add ("f", Syntax::None (), Check::non_negative (), Syntax::Const,
	      "Macropores conductivity curve shape parameter.");
    
  Librarian<Hydraulic>::add_type ("M_vGp", alist, syntax,
				  &HydraulicM_vGp::make);
}
