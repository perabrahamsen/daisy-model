// hydraulic_M_BivG.C
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
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
// Bimodal van Genuchten retention curve model with Mualem theory.

#define BUILD_DLL

#include "hydraulic.h"
#include "block_model.h"
#include "plf.h"
#include "mathlib.h"
#include "librarian.h"
#include "frame.h"

using std::abs;

class HydraulicM_BivG : public Hydraulic
{
  // Content.
  const double alpha1;
  const double a1;		// - alpha
  const double n1;
  const double m1;		// 1 - 1/n
  const double alpha2;
  const double a2;		// - alpha
  const double n2;
  const double m2;		// 1 - 1/n
  const double w2;
  const double l;               // tortuosity parameter
  mutable PLF M_;
  PLF pF_Theta;
  // Use.
public:
  double Theta (double h) const;
  double K (double h) const;
  double Cw2 (double h) const;
  double h (double Theta) const;
  double M (double h) const;
private:
  double Se1 (double h) const;
  double Se2 (double h) const;
  
  // Create and Destroy.
public:
  HydraulicM_BivG (const BlockModel&);
  ~HydraulicM_BivG ();
};

double 
HydraulicM_BivG::Theta (const double h) const
{
  return ((1-w2)*Se1(h)+w2*Se2(h)) * (Theta_sat - Theta_res) + Theta_res;
}

double 
HydraulicM_BivG::K (const double h) const
{
  if (h < 0.0)
    {
      const double Se1_h = Se1 (h);
	  const double Se2_h = Se2(h);
      return K_sat * pow ((1-w2)*Se1_h+ w2*Se2_h, l)
	* pow ((1-w2)*a1*(1.0 - pow (1.0 - pow (Se1_h, 1.0/m1), m1))
		+w2*a2*(1.0 - pow(1.0 - pow(Se2_h, 1.0 / m2), m2))
		, 2.0)/pow((1-w2)*a1+w2*a2,2);
    }
  else
    return K_sat;
}

double 
HydraulicM_BivG::Cw2 (const double h) const
{
#if 0
  const double temp = -(Theta_sat - Theta_res)*((1 - w2)
		* (m1 * (pow(1.0 / (1.0 + pow(a1 * h, n1)), m1 - 1.0)
			* (n1 * (pow(a1 * h, n1 - 1.0) * a1))))
		/ pow(1.0 + pow(a1 * h, n1), 2.0)
		+ w2 * (m2 * (pow(1.0 / (1.0 + pow(a2 * h, n2)), m2 - 1.0)
			* (n2 * (pow(a2 * h, n2 - 1.0) * a2))))
		/ pow(1.0 + pow(a2 * h, n2), 2.0));
#endif
  if (h < 0.0)
	  return -(Theta_sat - Theta_res)*((1-w2)
	      * (m1 * (  pow (1.0 / (1.0 + pow (a1 * h, n1)), m1 - 1.0)
		      * (n1 * (pow (a1 * h, n1 - 1.0) * a1))))
	      / pow (1.0 + pow(a1 * h, n1), 2.0)
		+w2 * (m2 * (pow(1.0 / (1.0 + pow(a2 * h, n2)), m2 - 1.0)
			* (n2 * (pow(a2 * h, n2 - 1.0) * a2))))
		/ pow(1.0 + pow(a2 * h, n2), 2.0));
   
  else
    return 0.0;
}

double 
HydraulicM_BivG::h (const double Theta) const
{
  daisy_assert (Theta_res <= Theta);
  if (Theta < Theta_sat)
	  return pF2h(pF_Theta(Theta));
	  // pow(pow(Theta_res / (Theta_res - Theta_sat)
      //   + Theta / (Theta_sat - Theta_res), -1.0 / m1)
	  //     - 1.0, 1.0 / n1) / a1 
    else
    return 0.0;
}

double 
HydraulicM_BivG::M (double h) const
{
  if (M_.size () == 0)
    K_to_M (M_, 500);

  return M_ (h);
}

double 
HydraulicM_BivG::Se1 (double h) const
{
  if (h < 0.0)
    {
	  const double Se1_h = pow(1.0 / (1.0 + pow(a1 * h, n1)), m1);
		
      daisy_assert(Se1_h >= 0.0);
      daisy_assert(Se1_h <= 1.0);
      return Se1_h;
    }
  else
    return 1.0;
}

double
HydraulicM_BivG::Se2 (double h) const
{
	if (h < 0.0)
	{
		const double Se2_h = pow(1.0 / (1.0 + pow(a2 * h, n2)), m2);

		daisy_assert(Se2_h >= 0.0);
		daisy_assert(Se2_h <= 1.0);
		return Se2_h;
	}
	else
		return 1.0;
}




HydraulicM_BivG::HydraulicM_BivG (const BlockModel& al)
  : Hydraulic (al),
    alpha1 (al.number ("alpha1")),
    a1 (-alpha1),
    n1 (al.number ("n1")),
    m1 (1 - 1 / n1),
	alpha2(al.number("alpha2")),
	a2(-alpha2),
	n2(al.number("n2")),
	m2(1 - 1 / n2),
	w2(al.number("w2")),
	l (al.number ("l")),
    M_ ()
{
	const double pf_min = -6.0;
	const double pf_max = 7.0;
	const double dpf = 0.05;
	const int nsteps = (abs(pf_min) + pf_max) / dpf;
	for (int i = nsteps; i >= 0; --i)
	{
		const double x = pf_min + dpf * i;
		const double y = Theta(-pow(10.0, x));
		pF_Theta.add(y, x);	// code block to be executed
	}
}

HydraulicM_BivG::~HydraulicM_BivG ()
{ }

// Add the HydraulicM_vG syntax to the syntax table.

static struct HydraulicM_BivGSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new HydraulicM_BivG (al); }


  HydraulicM_BivGSyntax ()
    : DeclareModel (Hydraulic::component, "M_BivG", 
               "Bimodal van Genuchten retention curve model with Mualem theory (Durner, 1994).")
  { }
  void load_frame (Frame& frame) const
  { 
    Hydraulic::load_Theta_res (frame);
    Hydraulic::load_K_sat (frame);
    frame.declare ("alpha1", "cm^-1", Attribute::Const,
                "van Genuchten alpha. for mode 1");
    frame.declare ("n1", Attribute::None (), Attribute::Const,
                "van Genuchten n. for mode 1");
	frame.declare("alpha2", "cm^-1", Attribute::Const,
		"van Genuchten alpha. for mode 2");
	frame.declare("n2", Attribute::None(), Attribute::Const,
		"van Genuchten n. for mode 2");
	frame.declare("w2", Attribute::None(), Attribute::Const,
		"weight of mode 2");
    frame.declare ("l", Attribute::None (), Attribute::Const,
                "tortuosity parameter.");
    frame.set ("l", 0.5);

  }
} hydraulicM_BivG_syntax;

// hydraulic_M_BivG.C ends here.
