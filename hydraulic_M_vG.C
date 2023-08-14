// hydraulic_M_vG.C
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
// van Genuchten retention curve model with Mualem theory.

#define BUILD_DLL

#include "hydraulic.h"
#include "block_model.h"
#include "plf.h"
#include "mathlib.h"
#include "librarian.h"
#include "frame.h"

class HydraulicM_vG : public Hydraulic
{
  // Content.
  const double alpha;
  const double a;		// - alpha
  const double n;
  const double m;		// 1 - 1/n
  const double l;               // tortuosity parameter
  mutable PLF M_;

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
public:
  HydraulicM_vG (const BlockModel&);
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
      return K_sat * pow (Se_h, l)
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
  daisy_assert (Theta_res <= Theta);
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
  if (M_.size () == 0)
    K_to_M (M_, 500);

  return M_ (h);
}

double 
HydraulicM_vG::Se (double h) const
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

HydraulicM_vG::HydraulicM_vG (const BlockModel& al)
  : Hydraulic (al),
    alpha (al.number ("alpha")),
    a (-alpha),
    n (al.number ("n")),
    m (1 - 1.0 / n),
    l (al.number ("l")),
    M_ ()
{ }

HydraulicM_vG::~HydraulicM_vG ()
{ }

// Add the HydraulicM_vG syntax to the syntax table.

static struct HydraulicM_vGSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new HydraulicM_vG (al); }


  HydraulicM_vGSyntax ()
    : DeclareModel (Hydraulic::component, "M_vG", 
               "van Genuchten retention curve model with Mualem theory.")
  { }
  void load_frame (Frame& frame) const
  { 
    Hydraulic::load_Theta_res (frame);
    Hydraulic::load_K_sat (frame);
    frame.declare ("alpha", "cm^-1", Attribute::Const,
                "van Genuchten alpha.");
    frame.declare ("n", Attribute::None (), Attribute::Const,
                "van Genuchten n.");
    frame.declare ("l", Attribute::None (), Attribute::Const,
                "tortuosity parameter.");
    frame.set ("l", 0.5);

  }
} hydraulicM_vG_syntax;
