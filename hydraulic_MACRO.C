// hydraulic_MACRO.C -- A bimodal variant of M_vG used in MACRO.
// 
// Copyright 1996-2001, 2003 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001, 2003, 2010 KVL.
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
// van Genuchten retention curve model with Mualem theory and power function.

#define BUILD_DLL

#include "hydraulic.h"
#include "block_model.h"
#include "plf.h"
#include "mathlib.h"
#include "check.h"
#include "librarian.h"
#include "frame.h"
#include <sstream>

class HydraulicMACRO : public Hydraulic
{
  // van Genuchten parameters.
  const double alpha;
  const double a;		// - alpha
  const double n;
  const double l;               // tortuosity parameter

  // van Genuchten helpers.
  const double m;		// 1 - 1/n
  mutable PLF M_;

  // Boundary point paramaters.
  const double h_b;
  const double Theta_b;
  const double K_b;
  // Boundary point helpers.
  const double Se_b;
  const double K_x_b;
  const double Theta_sat_fict; // Fictional Theta_sat for micropores.

  // Macropore conductivity.
  const double n_ma;

  // Use.
public:
  double Theta_micro (double h) const;
  double Theta (double h) const;
  double K (double h) const;
  double Cw2 (double h) const;
  double h (double Theta) const;
  double M (double h) const;
private:
  double S_ma (double h) const;
  double Se (double h) const;
  
  // Create and Destroy.
  void initialize (const Texture& texture, double rho_b, bool top_soil,
                   Treelog& msg)
  {
    Hydraulic::initialize (texture, rho_b, top_soil, msg);
    std::stringstream tmp;
    tmp << "Theta_sat_fict = " << Theta_sat_fict;
    msg.debug (tmp.str ());
  }
public:
  HydraulicMACRO (const BlockModel&);
  ~HydraulicMACRO ();
};

double 
HydraulicMACRO::Theta_micro (const double h) const
{
  return Se (h) * (Theta_sat_fict - Theta_res) + Theta_res;
}

double 
HydraulicMACRO::Theta (const double h) const
{
  // Micropores only.
  const double Theta_mi = Theta_micro (h);
  if (h < h_b)
    return Theta_mi;

  // Scale macropores.
  return Theta_b + (Theta_sat - Theta_b) * S_ma (h);
}

double 
HydraulicMACRO::K (const double h) const
{
  if (h >= 0.0)
    return K_sat;

  const double Se_h = Se (h);
  const double K_mi = K_b * pow (Se_h / Se_b, l)
    * pow ((1.0 - pow (1.0 - pow (Se_h, 1.0/m), m)) / K_x_b, 2.0);
  
  if (h < h_b)
    return K_mi;

  const double K_ma = (K_sat - K_b) * std::pow (S_ma (h), n_ma);
  
  return K_mi + K_ma;
}

double 
HydraulicMACRO::Cw2 (const double h) const
{
  if (h < 0.0)
    return - (  (Theta_sat_fict - Theta_res)
	      * (m * (  pow (1.0 / (1.0 + pow (a * h, n)), m - 1.0)
		      * (n * (pow (a * h, n - 1.0) * a))))
	      / pow (1.0 + pow(a * h, n), 2.0));
  else
    return 0.0;
}

double 
HydraulicMACRO::h (const double Theta) const
{
  daisy_assert (Theta_res <= Theta);
  if (Theta < Theta_sat_fict)
    return pow (pow (Theta_res / (Theta_res - Theta_sat_fict) 
                     + Theta / (Theta_sat_fict - Theta_res), -1.0 / m)
                - 1.0, 1.0 / n) / a;
  else
    return 0.0;
}

double 
HydraulicMACRO::M (double h) const
{
  if (M_.size () == 0)
    K_to_M (M_, 500);

  return M_ (h);
}

double 
HydraulicMACRO::S_ma (double h) const
{
  // How much of the macropores are filled?
  if (h <= h_b)
    // Empty at h_b
    return 0;
  if (h >= -1.0)
    // Filled at pF=0.0
    return 1.0;

  const double pF = h2pF (h);
  const double pF_b = h2pF (h_b);
  // Scale linearly between.
  const double S = 1.0 - pF / pF_b;
  daisy_assert (S >= 0.0);
  daisy_assert (S <= 1.0);
  return S;
}

double 
HydraulicMACRO::Se (double h) const
{
  return std::pow (1.0 / (1.0 + std::pow (a * h, n)), m);
}

HydraulicMACRO::HydraulicMACRO (const BlockModel& al)
  : Hydraulic (al),
    alpha (al.number ("alpha")),
    a (-alpha),
    n (al.number ("n")),
    l (al.number ("l")),
    m (1.0 - 1.0 / n),
    M_ (),
    h_b (al.number ("h_b")),
    Theta_b (al.number ("Theta_b")),
    K_b (al.number ("K_b")),
    Se_b (Se (h_b)),
    K_x_b (1.0 - std::pow (1.0 - std::pow (Se_b, 1.0/m), m)),
    Theta_sat_fict (Theta_res + (Theta_b - Theta_res) / Se_b),
    n_ma (al.number ("n_ma"))
{ 
  daisy_approximate (Theta_micro (h_b), Theta_b);
  daisy_approximate (Theta (h_b), Theta_b);
  daisy_approximate (h_b, h (Theta_b));
  daisy_approximate (K_sat, K (0.0));
  daisy_assert (Theta_sat_fict <= Theta_sat);
  daisy_assert (Theta_sat_fict >= Theta_b);
  daisy_approximate (Theta_sat_fict, Theta_micro (0.0));
  daisy_approximate (Theta_sat, Theta (0.0));
  daisy_approximate (1.0, S_ma (0.0));
  daisy_approximate (1.0, 1.0 + S_ma (h_b));
}

HydraulicMACRO::~HydraulicMACRO ()
{ }

// Add the HydraulicMACRO syntax to the syntax table.

static struct HydraulicMACROSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new HydraulicMACRO (al); }

  HydraulicMACROSyntax ()
    : DeclareModel (Hydraulic::component, "MACRO", 
                    "van Genuchten retention curve model with Mualem theory.\n\
The near saturated retention and hydraulic properties have been adjusted\n\
to take macropores into account.")
  { }
  void load_frame (Frame& frame) const
  { 
    frame.set_strings ("cite", "larsbo2003macro");
    Hydraulic::load_Theta_res (frame);
    frame.declare ("K_sat", "cm/h",
                   Check::non_negative (), Attribute::OptionalConst,
                   "Water conductivity of saturated soil.");
    frame.declare ("alpha", "cm^-1", Attribute::Const,
                   "van Genuchten alpha.");
    frame.declare ("n", Attribute::None (), Attribute::Const,
                   "van Genuchten n.");
    frame.declare ("l", Attribute::None (), Attribute::Const,
                   "tortuosity parameter.");
    frame.set ("l", 0.5);
    frame.declare ("h_b", "cm", Check::negative (), Attribute::Const, "\
Pressure at boundary point of change between matrix and macropores domains.");
    frame.declare_fraction ("Theta_b", Attribute::Const, "\
Water content at boundary point.");
    frame.declare ("K_b", "cm/h",
                   Check::non_negative (), Attribute::OptionalConst, "\
Water conductivity at boundary point.");
    frame.declare ("n_ma", Attribute::None (), Attribute::Const,
                   "Macropore size distribution factor.");
  }
} hydraulicMACRO_syntax;

// hydraulic_MACRO.C ends here.
