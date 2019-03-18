// hydraulic_B_C_inverse.C -- Campbell theory with inverse modelling.
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
// Copyright 2010 KU.
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

#define BUILD_DLL
#include "hydraulic.h"
#include "block_model.h"
#include "check.h"
#include "mathlib.h"
#include "librarian.h"
#include "frame.h"
#include "texture.h"
#include <sstream>

struct HydraulicB_C_inverse : public Hydraulic
{
  // Known points.
  /* const */ double Theta_wp;
  const double Theta_fc;

  // Cambell parameters.
  /* const */ double b;
  /* const */ double h_b;

  // Utilities.
  double Sr (const double h) const
  {
    if (h < h_b)
      return pow (h_b / h, 1.0 / b);
    else
      return 1;
  }
  double Theta (const double h) const
  { return Sr (h) * Theta_sat; }
  double K (const double h) const
  { return K_sat * pow (Sr (h), (2 + 3.0 / b) * b); }
  double Cw2 (const double h) const
  {
    if (h < h_b)
      return -(Theta_sat*(pow(h_b / h, 1 / b - 1)*h_b) / (pow(h, 2)*b));
    else
      return 0.0;
  }
  double h (const double Theta) const
  {
    if (Theta < Theta_sat)
      return h_b / pow(Theta / Theta_sat, b);
    else
      return h_b;
  }    
  double M (const double h) const
  {
    if (h <= h_b)
      return K_sat * (-h_b / (1.0 + 3.0 / b)) * pow (h_b / h, 1.0 + 3.0 / b);
    else
      return M (h_b) + K_sat * (h - h_b);
  }
  
  // Create and Destroy.
  static double find_b (const double Theta_wp, const double Theta_fc)
  {
    const double h_fc = -100.0;
    const double h_wp = -15000.0;
    daisy_assert (Theta_fc > Theta_wp);
    const double b = std::log (h_wp / h_fc) / std::log (Theta_fc / Theta_wp);
    return b;
  }
  static double find_h_b (const double Theta_wp, const double Theta_fc,
                          const double Theta_sat, const double b)
  {
    const double h_fc = -100.0;
    daisy_assert (Theta_fc < Theta_sat);
    const double h_b_fc = h_fc * std::pow (Theta_fc / Theta_sat, b);
    daisy_assert (Theta_wp < Theta_sat);
    const double h_wp = -15000.0;
    const double h_b_wp = h_wp * std::pow (Theta_wp / Theta_sat, b);
    daisy_approximate (h_b_fc, h_b_wp);
    return h_b_fc;
  }
  void initialize (const Texture& texture,
                   double rho_b, const bool top_soil, const double CEC,
                   const double center_z, Treelog& msg)
  {
    TREELOG_MODEL (msg);
    std::ostringstream tmp;

    // Find Theta_sat.
    if (Theta_sat < 0.0)
      {
        if (rho_b < 0.0)
          {
            msg.error ("You must specify either dry bulk density or porosity");
            rho_b = 1.5;
            tmp << "Forcing rho_b = "  << rho_b << " g/cm^3\n";
          }
        Theta_sat = 1.0 - rho_b / texture.rho_soil_particles ();
        tmp << "(Theta_sat " << Theta_sat << " [])\n";
        daisy_assert (Theta_sat < 1.0);
      }
    if (Theta_sat <= Theta_fc)
      {
        msg.error ("Field capacity must be below saturation point");
        Theta_sat = (1.0 + 4.0 * Theta_fc) / 5.0;
        tmp << "Forcing Theta_sat = " << Theta_sat << " []\n";
      }

    // Find Theta_wp.
    if (Theta_wp < 0.0)
      {
        const double clay_lim // USDA Clay
          = texture.fraction_of_minerals_smaller_than ( 2.0 /* [um] */);
        const double silt_lim // USDA Silt 
          = texture.fraction_of_minerals_smaller_than (50.0 /* [um] */);
        daisy_assert (clay_lim >= 0.0);
        daisy_assert (silt_lim >= clay_lim);
        daisy_assert (silt_lim <= 1.0);
        const double mineral = texture.mineral ();
        const double clay = mineral * clay_lim * 100 /* [%] */;
        const double silt = mineral * (silt_lim - clay_lim) * 100 /* [%] */;
        const double humus = texture.humus * 100 /* [%] */;
        // Madsen and Platou (1983).
        Theta_wp = 0.758 * humus + 0.520 * clay + 0.075 * silt + 0.42;
        Theta_wp /= 100.0;      // [%] -> []
      }

    b = find_b (Theta_wp, Theta_fc);
    h_b = find_h_b (Theta_wp, Theta_fc, Theta_sat, b);
    tmp << "(b " << b << " [])\n"
        << "(h_b " << h_b << " [cm])";
    msg.debug (tmp.str ());

    // Must be called last (K_init depends on the other parameters).
    Hydraulic::initialize (texture, rho_b, top_soil, CEC, center_z, msg);
  }    
                           
  HydraulicB_C_inverse (const BlockModel& al)
    : Hydraulic (al),
      Theta_wp (al.number ("Theta_wp", -42.42e42)),
      Theta_fc (al.number ("Theta_fc"))
  { }
  ~HydraulicB_C_inverse ()
  { }
};

static struct HydraulicB_C_inverseSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new HydraulicB_C_inverse (al); }

  HydraulicB_C_inverseSyntax ()
    : DeclareModel (Hydraulic::component, "B_C_inverse", 
                    "Campbell retention curve model with Burdine theory.\n\
\n\
This implementation is based on inverse modelling, you specify water\n\
at wilting point (pF 4.2) and field capacity (pF 2.0), from which the\n\
retention curve (the b and h_b parameters) is derived.  Based on this,\n\
the condutivity curve is fully specified by a single point.")
  { }
  static bool check_alist (const Metalib&, const Frame& al, Treelog& msg)
  { 
    const double Theta_sat = al.number ("Theta_sat");
    const double Theta_fc = al.number ("Theta_fc");
    const double Theta_wp = al.number ("Theta_wp");

    bool ok = true;

    if (Theta_wp >= 0.0 && Theta_wp >= Theta_fc)
      {
        msg.error ("Wilting point should be below field capacity");
        ok = false;
      }
    if (Theta_sat >= 0.0 && Theta_fc >= Theta_sat)
      {
        msg.error ("Field capacity should be below saturation point");
        ok = false;
      }
    return ok;
  }
  
  void load_frame (Frame& frame) const
  { 
    frame.set_strings ("cite", "campbell1974simple", "burdine1953");
    frame.add_check (check_alist);
    Hydraulic::load_Theta_sat (frame);
    Hydraulic::load_K_sat_optional (frame);
    frame.declare_fraction ("Theta_sat",  Attribute::OptionalState, "\
Saturation point.\n\
By default, this will be estimated from soil composition and\n\
dry bulk density.");
    frame.declare_fraction ("Theta_fc",  Attribute::Const, " \
Field capacity."); 
    frame.declare_number_cited ("Theta_wp",  Attribute::Fraction (), 
                                Check::fraction (), 
                                Attribute::OptionalConst,
                                Attribute::Singleton, "\
Wilting point.\n\
By default, this value will be estimated from texture.", 
                                "madsen1983land"); 
  }
} hydraulicB_C_inverse_syntax;

// hydraulic_B_C_inverse.C ends here.
