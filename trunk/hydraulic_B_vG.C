// hydraulic_B_vG.C
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
// van Genuchten retention curve model with Burdine theory.

#define BUILD_DLL

#include "hydraulic.h"
#include "block_model.h"
#include "plf.h"
#include "mathlib.h"
#include "librarian.h"
#include "frame.h"

class HydraulicB_vG : public Hydraulic
{
  // Content.
  const double alpha;
  const double a;		// - alpha
  const double n;
  const double m;		// 1 - 2/n
  const double l;               // tortuosity parameter
  mutable PLF M_;

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
public:
  HydraulicB_vG (const BlockModel&);
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
      return K_sat * pow (Se_h, l) * (1 - pow (1 - pow (Se_h, 1.0/m), m));
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
    return pow (pow (Theta_res / (Theta_res - Theta_sat) 
                     + Theta / (Theta_sat - Theta_res), -1.0 / m) 
                - 1.0, 1.0 / n) / a;
  else
    return 0.0;
}

double 
HydraulicB_vG::M (double h) const
{
  if (M_.size () == 0)
    K_to_M (M_, 500);

  return M_ (h);
}

double 
HydraulicB_vG::Se (double h) const
{
  return pow (1 / (1 + pow (a * h, n)), m);
}

HydraulicB_vG::HydraulicB_vG (const BlockModel& al)
  : Hydraulic (al),
    alpha (al.number ("alpha")),
    a (-alpha),
    n (al.number ("n")),
    m (1.0 - 2.0 / n),
    l (al.number ("l")),
    M_ ()
{ }

HydraulicB_vG::~HydraulicB_vG ()
{ }

// Add the HydraulicB_vG syntax to the syntax table.

static struct HydraulicB_vGSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const 
  { return new HydraulicB_vG (al); }

  HydraulicB_vGSyntax ()
    : DeclareModel (Hydraulic::component, "B_vG", 
               "van Genuchten retention curve model with Burdine theory.")
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
    frame.set ("l", 2.0);

  }
} hydraulicB_vG_syntax;
