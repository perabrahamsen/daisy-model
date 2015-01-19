// hydraulic_wepp.C --- Hydraulic propertoes predicted by WEPP and HYPRES.
// 
// Copyright 2014 KU
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
// Parameters specified by the HYPRES and WEPP.

#define BUILD_DLL

#include "hydraulic.h"
#include "block_model.h"
#include "texture.h"
#include "plf.h"
#include "treelog.h"
#include <sstream>
#include "mathlib.h"
#include "librarian.h"
#include "frame.h"
#include "log.h"

// Bulk density at 330 cm, after consolidation. [g/cm^3]
static double 
consolidated_bulk_density (const Texture& texture, 
                           const double CEC /* [cmolc/kg] */)
{
  // Extract USDA3 texture.
  const double mineral = texture.mineral (); // [g mineral/g soil]
  const double clay = mineral                // [g clay/g soil]
    * texture.fraction_of_minerals_smaller_than (2.0 /* [um] */);
  daisy_assert (clay >= 0.0);
  const double silt = mineral                // [g clay/g soil]
    * texture.fraction_of_minerals_smaller_than (50.0 /* [um] */)
    - clay;
  daisy_assert (silt >= 0.0);
  const double sand = mineral                // [g clay/g soil]
    * texture.fraction_of_minerals_smaller_than (2000.0 /* [um] */)
    - clay - silt;
  daisy_assert (sand >= 0.0);
  const double om = texture.humus;           // [g om/g soil]
  daisy_assert (clay + silt + sand + om < 1.001);

  // From Danish soil pedotrasfer [krogh2000cation]
  const double CECr_sane = 50.0;     // [cmolc/kg clay] 
  // Apparently there is a mistake in WEPP: CECr is really mmol/g, 
  // not per mmol/100g. Source: Merete Styczen personal correspondance.
  const double CECr = CECr_sane * 0.01; // [mmolc/g clay]

  // CECc is the part of CEC originating from clay.
  const double CECc = CECr_sane * clay; // [cmolc/kg soil]
  
  // Consolidated bulk density.
  const double p_c = (1.514 + 0.25 * sand -  13.0 * sand * om -  6.0 * clay * om
                      - 0.48 * clay * CECr) * 1000.0; // [kg / m^3]
  return p_c;
}

// Bulk density after tillage. [kg/m^3]
static double
bulk_density_after_tillage (const double p_tm1, // [kg/m^3]
                            const double p_c,   // [kg/m^3]
                            const double T_ds,  // []
                            const double consolidate_factor // []
                            )
// p_tm1 is the bulk density before tillage
// p_c is consolidated bulk density.
// T_ds is fraction of surface being affected.
// consolidate_factor is the ratio between consolidated and loose bulk density.
// consolidate_factor should default to 0.667, as it is used in wepp.
{
  // p_tet is is bulk density after tillage.
  const double p_tet = p_tm1 - (p_tm1 - consolidate_factor * p_c) * T_ds;
  return p_tet;
}

class HydraulicWEPP : public Hydraulic
{
  // WEPP parametre.
  const double consolidate_factor; // [0-1];

  // Static soil properties, calculated on initialization.
  double clay;                  // [%]
  double silt;                  // [%]
  double humus;                 // [%]

  // Static wepp properties, calculated on initialization.
  double p_c;                   // consolidated_bulk_density [kg/m^3]

  // Danymic properties calculated by wepp.
  double rho_b;       // [g/cm^3]

  // Dynamic M_vG parameters calculated by hypres.
  double alpha;
  double a;		// - alpha
  double n;
  double m;		// 1 - 1/n
  double l;         // tortuosity parameter
  PLF M_;
  double K_fc;
  double K_wp;
  double Theta_fc;
  double Theta_wp;

  // Prevent changing Theta_sat.
public:
  void set_porosity (double)
  { throw ("Can't change porosity for WEPP hydraulic model"); }
  void tillage (const double T_ds);
  void tick_daily (const double rain);
  void output (Log& log) const
  {
    output_variable (rho_b, log);
    output_variable (alpha, log);
    output_variable (n, log);
    output_variable (l, log);
    output_variable (K_sat, log);
    output_variable (K_fc, log);
    output_variable (K_wp, log);
    output_variable (Theta_sat, log);
    output_variable (Theta_fc, log);
    output_variable (Theta_wp, log);
  };

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
  HydraulicWEPP (const BlockModel&);
  HydraulicWEPP (symbol name, double K_sat);
  void initialize (const Texture&, double rho_b, bool top_soil, double CEC,
		   Treelog& msg);
private:
  void hypres ();
public:
  ~HydraulicWEPP ();
};

void 
HydraulicWEPP::tillage (const double T_ds)
{
  daisy_assert (T_ds >= 0.0 && T_ds <= 1.0);
  const double p_tm1 = rho_b * (100.0 * 100.0 * 100.0) / 1000.0; // [kg/m^3]
  const double p_t = bulk_density_after_tillage (p_tm1, p_c, T_ds,
                                                 consolidate_factor);
  rho_b = p_t * 1000.0 / (100.0 * 100.0 * 100.0); // [g/cm^3]

  hypres ();
}

double 
HydraulicWEPP::Theta (const double h) const
{
  return Se (h) * (Theta_sat - Theta_res) + Theta_res;
}

double 
HydraulicWEPP::K (const double h) const
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
HydraulicWEPP::Cw2 (const double h) const
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
HydraulicWEPP::h (const double Theta) const
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
HydraulicWEPP::M (double h) const
{ return M_ (h); }

double 
HydraulicWEPP::Se (double h) const
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

void
HydraulicWEPP::initialize (const Texture& texture,
                           double, const bool, const double CEC, Treelog& msg)
{
  const double clay_lim 
    = texture.fraction_of_minerals_smaller_than ( 2.0 /* [um] USDA Clay */);
  const double silt_lim 
    = texture.fraction_of_minerals_smaller_than (50.0 /* [um] USDA Silt */);
  daisy_assert (clay_lim >= 0.0);
  daisy_assert (silt_lim >= clay_lim);
  daisy_assert (silt_lim <= 1.0);
  const double mineral = texture.mineral ();
  clay = mineral * clay_lim * 100 /* [%] */;
  silt = mineral * (silt_lim - clay_lim) * 100 /* [%] */;
  const double sand = mineral * (1.0 - silt_lim) * 100 /* [%] */;
  humus = texture.humus * 100 /* [%] */;

  // We should check for these earlier.
  if (!(clay > 0))
    {
      msg.error ("clay must be present when using hypres, assuming 1%");
      clay = 1.0;
    }
  if (!(silt > 0))
    {
      msg.error ("silt must be present when using hypres, assuming 1%");
      silt = 1.0;
    }
  if (!(humus > 0))
    {
      msg.error ("humus must be present when using hypres, assuming 1%");
      humus = 1.0;
    }

  if (!approximate (clay + silt + sand + humus, 100.0))
    {
      std::ostringstream tmp;
      tmp << "The sum of all fractions should be 100%, it is "
             << clay + silt + sand + humus;
      msg.error (tmp.str ());
    }

  p_c = consolidated_bulk_density (texture, CEC);
  {
    std::ostringstream tmp;
    tmp << "Consolidated bulk density " << p_c << " kg/m^3";
    msg.debug (tmp.str ());
  }
  rho_b = p_c * 1000.0 / (100.0 * 100.0 * 100.0); // [g/cm^3]
  hypres ();
  M_.clear ();
  K_to_M (M_, 500);
}

void
HydraulicWEPP::hypres ()
{
  daisy_assert (rho_b > 0.0);
  // WEPP is only for surface.
  const bool top_soil = true;

  Theta_sat = 0.7919 + 0.001691 * clay - 0.29619 * rho_b 
    - 0.000001491 * silt * silt
    + 0.0000821 * humus * humus + 0.02427 / clay + 0.01113 / silt
    + 0.01472 * log (silt) - 0.0000733 * humus * clay - 0.000619 * rho_b * clay
    - 0.001183 * rho_b * humus;
  if (top_soil)
    Theta_sat -= 0.0001664 * silt;
  daisy_assert (Theta_sat > 0.0);
  daisy_assert (Theta_sat < 1.0);

  double alpha_star 
    = -14.96 + 0.03135 * clay  + 0.0351 * silt + 0.646 * humus + 15.29 * rho_b
    - 4.671 * rho_b * rho_b - 0.000781 * clay * clay - 0.00687 * humus * humus
    + 0.0449 / humus + 0.0663 * log (silt) + 0.1482 * log (humus)
    - 0.04546 * rho_b * silt - 0.4852 * rho_b * humus;
    ;
  if (top_soil)
    {
      alpha_star -= 0.192;
      alpha_star += 0.00673 * clay;
    }
  alpha = exp (alpha_star);

  double n_star 
    = -25.23 - 0.02195 * clay + 0.0074 * silt - 0.1940 * humus + 45.5 * rho_b
    - 7.24 * rho_b * rho_b + 0.0003658 * clay * clay + 0.002885 * humus * humus
    - 12.81 / rho_b - 0.1524 / silt - 0.01958 / humus - 0.2876 * log (silt)
    - 0.0709 * log (humus) - 44.6 * log (rho_b) - 0.02264 * rho_b * clay
    + 0.0896 * rho_b * humus;
  if (top_soil)
    n_star += 0.00718 * clay;
  n = exp (n_star) + 1.0;

  const double l_star 
    = 0.0202 + 0.0006193 * clay * clay - 0.001136 * humus * humus
    - 0.2316 * log (humus) - 0.03544 * rho_b * clay + 0.00283 * rho_b * silt
    + 0.0488 * rho_b * humus;
  l = (10.0 * exp (l_star) - 10.0) / (1.0 + exp (l_star));
  
  if (K_sat < 0.0 && K_init == NULL)
    {
      double K_sat_star 
	= 7.755 + 0.0352 * silt - 0.967 * rho_b * rho_b 
	- 0.000484 * clay * clay 
	- 0.000322 * silt * silt + 0.001 / silt - 0.0748 / humus
	- 0.643 * log (silt) - 0.01398 * rho_b * clay - 0.1673 * rho_b * humus
	;
      if (top_soil)
	{
	  K_sat_star += 0.93;
	  K_sat_star += 0.02986 * clay;
	  K_sat_star -= 0.03305 * silt;
	}
      K_sat = exp (K_sat_star) / 24.0;
    }
  
  a = -alpha;
  m = 1.0 - 1.0 / n;

  const double h_fc = -100.0;
  const double h_wp = -15000.0;
  K_fc = K (h_fc);
  K_wp = K (h_wp);
  Theta_fc = Theta (h_fc);
  Theta_wp = Theta (h_wp);
}


HydraulicWEPP::HydraulicWEPP (const BlockModel& al)
  : Hydraulic (al),
    consolidate_factor (al.number ("consolidate_factor")),
    alpha (-42.42e42),
    a (-42.42e42),
    n (-42.42e42),
    m (-42.42e42),
    l (-42.42e42),
    M_ ()
{ }


HydraulicWEPP::~HydraulicWEPP ()
{ }

// Add the HydraulicWEPP syntax to the syntax table.

static struct HydraulicWEPPSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new HydraulicWEPP (al); }

  static bool check_alist (const Metalib&, const Frame&, Treelog&)
  {
    bool ok = true;
    return ok;
  }
  HydraulicWEPPSyntax ()
    : DeclareModel (Hydraulic::component, "wepp", 
                    "van Genuchten retention curve model with Mualem theory.\n\
Parameters specified by the HYPRES transfer function.\n\
Tillage and weather dynamics from WEPP.")
  { }
  void load_frame (Frame& frame) const
  { 
    frame.set_strings ("cite", "hypres", "wepp");
    frame.declare_fraction ("Theta_res",
                            Attribute::Const, "Soil residual water.");
    frame.set ("Theta_res", 0.01);
    frame.add_check (check_alist);
    frame.declare_fraction ("consolidate_factor", Attribute::Const, "\
The ratio between consolidated and loose bulk density.");
    frame.set ("consolidate_factor", 0.667);
    frame.declare ("rho_b", "g/cm^3", Attribute::LogOnly,
                   "Dry bulk density.");
    frame.declare ("alpha", "cm^-1", Attribute::LogOnly,
                   "van Genuchten alpha.");
    frame.declare ("n", Attribute::None (), Attribute::LogOnly,
                   "van Genuchten n.");
    frame.declare ("l", Attribute::None (), Attribute::LogOnly,
                   "tortuosity parameter.");
    frame.declare ("K_sat", "cm/h", Attribute::LogOnly,
                   "Water conductivity of saturated soil.");
    frame.declare ("K_fc", "cm/h", Attribute::LogOnly,
                   "Water conductivity at field capaicy (pF 2).");
    frame.declare ("K_wp", "cm/h", Attribute::LogOnly,
                   "Water conductivity at wilting point (pF 4.2).");
    frame.declare_fraction ("Theta_sat",  Attribute::LogOnly, 
                            "Saturated water content.");
    frame.declare_fraction ("Theta_fc",  Attribute::LogOnly, 
                            "Water content at field capacity (pF 2.0).");
    frame.declare_fraction ("Theta_wp",  Attribute::LogOnly, 
                            "Water content at wilting point (pF 4.2).");
  }
} hydraulicWEPP_syntax;

// hydraulic_wepp.C ends here.
