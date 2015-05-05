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
#include "check.h"

class HydraulicWEPP : public Hydraulic
{
  // WEPP parametre.
  const double consolidate_factor; // [0-1]
  const double consolidate_factor_water; // [0-1]
  const double delta_pmx_fixed;    // [kg/m^3] forced value of delta_pmx.
  const double time_consolidation; // [d^-1] how fast the soil consolidates
  const double average_depth;      // [cm]
  const double CECr_fixed;         // Fixed value for CECr.
  const bool allow_negative_delta_pc; // delta_pc can be < 0
  const double crust_thickness;       // [m]

  double p_c;                   // consolidated_bulk_density [kg/m^3]

  // Static soil properties, calculated on initialization.
  double clay;                  // [%]
  double silt;                  // [%]
  double humus;                 // [%]

  // Static wepp properties, calculated on initialization.
  double delta_pmx_clay;        // static rain bulk density change [kg/m^3]
  double K_b;                   // baseline effective conductivity [mm/h]
  double SC;                    // Subsoil crust correction fact. [?] wepp:7.9.5
  double Psi;                   // Cap. pot at crust/subc intf. [?] wepp:7.9.6
  double C;                     // Soil stability factor [m^2/J] wepp:7.9.8
  double b;                     // RR coeff. [mm] wepp:7.5.4
  double L_sand;                // Sand contribution to L. [m]
  double L_clay;                // Clay factor for L. [m^4/kg]

  // Properties calculated by wepp after tillage.
  double p_t;                   // dry bulk density after tillage. [kg/m^3]
  double delta_pmx;             // rain bulk density change [kg/m^3]
  double delta_pc;             // time bulk density change [kg/m^3]
  double RRi;                  // Random roughness after last tillage [m]
  double br;                   // Burried residue 0-15 cm. [kg DM/m^2]
  
  // Properties calculated by wepp daily.
  double day_count;             // Number of days since tillage [d]
  double R_c;                   // Accumulated rain since tillage [m]
  double rho_b;                 // Current dry bulk density [g/cm^3]
  double E_a;                   // Accumulated rainergy since tillage [J/m^2]
  double RRt;                   // Random roughness [m]
  double K_bare;                // Dynamic base soil conductivity.

  // Frost.
  const double freeze_effect;   // Max effect of frost [kg/m^3]
  const double freeze_on;       // Ice content for entering frozen state. []
  const double freeze_off;      // Ice content for leaving frozen state. []
  bool frozen;                  // Frozen state active.

  // Dynamic M_vG parameters calculated by hypres.
  double alpha;
  double a;		// - alpha
  double n;
  double m;		// 1 - 1/n
  double l;         // tortuosity parameter
  PLF M_;
  double K_15;
  double K_30;
  double K_60;
  double K_120;
  double K_fc;
  double K_wp;
  double Theta_fc;
  double Theta_wp;

  // Prevent changing Theta_sat.
public:
  void set_porosity (double)
  { throw ("Can't change porosity for WEPP hydraulic model"); }
  void tillage (const double T_ds, const double RR0, const double Theta,
                const double AOM15);
  void calculate_rho_b ();
  void tick (const double dt, const double rain, const double ice, Treelog& msg);
  void output (Log& log) const;

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
  void initialize_wepp (const Texture& texture, 
                        const double CEC /* [cmolc/kg] */,
                        const double center_z /* [cm] */,
                        Treelog& msg);
  void initialize (const Texture&, double rho_b, bool top_soil, double CEC,
                   double center_z,
		   Treelog& msg);
private:
  void hypres ();
public:
  ~HydraulicWEPP ();
};

void 
HydraulicWEPP::tillage (const double T_ds, const double RR0, const double Theta,
                        const double AOM15)
{
  daisy_assert (T_ds >= 0.0 && T_ds <= 1.0);
  const double p_tm1 = rho_b * (100.0 * 100.0 * 100.0) / 1000.0; // [kg/m^3]

  // Relative water content.
  const double Se = (Theta - Theta_res) / (Theta_sat - Theta_res);
  const double K1 = consolidate_factor + Se * consolidate_factor_water;

  // wepp:7.7.1
  p_t = p_tm1 - (p_tm1 - K1 * p_c) * T_ds;

  if (delta_pmx_fixed > 0)
    // Merete Styczen want is as a calibration parameter.
    delta_pmx = delta_pmx_fixed;
  else
    // wepp:7.7.10
    delta_pmx = std::max (0.0, delta_pmx_clay - 0.92 * p_t); // [kg/m^3]
  
  // Dry bulk density after 100 mm (0.1 m) of rain.
  const double p_t01m = p_t + delta_pmx * (0.1 / (0.01 + 0.1));
  std::ostringstream tmp;
  if (p_t01m > p_c)
    {
      tmp << "negative delta_pc ";
      if (allow_negative_delta_pc)
        {
          // delta_pc = p_t - p_c;
          delta_pc = p_c - p_t01m;
          tmp << "activated";
        }
      else
        delta_pc = 0.0;
      tmp << "\n";
    }
  else
    delta_pc = p_c - p_t01m;
  tmp << "AOM15 = " << AOM15 << "; delta_pc = " << delta_pc << "; p_c = " << p_c
      << "; p_t01m = " << p_t01m  << "; R_c = " << R_c << "; E_a = " << E_a
      << "; p_t = " << p_t 
      << "; delta_pmx = " << delta_pmx 
      << ";  delta_pmx_clay = " <<  delta_pmx_clay 
      << "; delta_pmx_fixed = " << delta_pmx_fixed
      << "; T_ds = " << T_ds << "; Se = " << Se << "; Theta = " << Theta 
      << "; K1 = " << K1;
  Assertion::message (tmp.str ());

  // Random roughness.
  RRi = RR0 * T_ds + RRt * (1.0 - T_ds); // [m]
  br = std::min (AOM15, 0.3); // [kg DM/m^2]

  day_count = 0.0;                                // [d]
  R_c = 0.0;                                      // [m]
  E_a = 0.0;
  hypres ();
}

void
HydraulicWEPP::calculate_rho_b ()
{
  // Fraction of time loosening effect until now. wepp:7.712
  const double F_dc = 1.0 - std::exp (- time_consolidation * day_count); // []
  // Loosening due to time. wepp:7.7.12
  const double delta_pwt = delta_pc * F_dc; 
  // Loosening due to rain. wepp:7.7.9
  const double delta_prf = delta_pmx * (R_c / (0.01 + R_c)); // [kg/m^3]
  // New dry bulk density. wepp:7.7.14
  const double p_new = p_t + delta_prf + delta_pwt; // [kg/m^3]
  // Convert to Daisy units.
  rho_b = p_new * 1000.0 / (100.0 * 100.0 * 100.0); // [g/cm^3]
}

void 
HydraulicWEPP::tick (const double dt /* [h] */, const double rain /* [mm/h] */,
                     const double ice /* [] */, Treelog& msg)
{
  day_count += dt / 24.0;       // [d]
  R_c += rain * dt * 0.001;          // [m]
  const double I = rain;   // [mm/h]
  const double E_i = (I > 0.0) 
    ? std::max ((11.9 + 8.73 * std::log10 (I)) * rain, 0.0)
    : 0.0; // [J/m^2]
  E_a += E_i;                   // [J/m^2]
  
                       
  if (ice > freeze_on && !frozen)
    {
      frozen = true;

      // Loosening due to rain. wepp:7.7.9
      const double delta_prf = delta_pmx * (R_c / (0.01 + R_c)); // [kg/m^3]
      if (delta_prf < freeze_effect)
        R_c = 0.0;
      else
        {
          R_c = (-0.01 * freeze_effect) / (-delta_pmx + freeze_effect);
          const double new_delta_prf = delta_pmx * (R_c / (0.01 + R_c));
          if (!approximate (delta_prf - freeze_effect, new_delta_prf))
            {
              std::ostringstream tmp;
              tmp << "ice = " << ice
                  << "; freeze_on = " << freeze_on
                  << "; frozen = " << frozen
                  << "; R_c = " << R_c
                  << "; new_delta_prf = " << new_delta_prf
                  << "; freeze_effect = " << freeze_effect
                  << "; delta_prf = " << delta_prf
                  << "; delta_pmx = " << delta_pmx
                  << "; rho_b = " << rho_b;
              msg.error (tmp.str ());
            }
        }

      msg.message ("Surface freezes");
    }
  else if (ice < freeze_off && frozen)
    {
      frozen = false;
      msg.message ("Surface thaws");
    }
  hypres ();

  const double Cbr = 1.0 - 0.5 * br; // [] wepp:7.5.3
  const double p_b = rho_b 
    * (100.0 * 100.0 * 100.0) / 1000.0; // wepp units: [kg/m^3]
  const double L = std::max (crust_thickness, L_sand - L_clay * p_b); // wepp:7.9.7 [m]
  const double CF = SC / (1.0 + Psi / (100.0 * L));    // [?] wepp:7.9.4

  RRt = RRi * std::exp (-Cbr * std::pow (R_c / b, 0.6));  // [m] wepp:7.5.2
  // wepp:7.9.3
  K_bare = K_b * (CF + (1.0 - CF) * std::exp (-C * E_a * (1.0 - RRt / 0.04)));
}

void 
HydraulicWEPP::output (Log& log) const
{
  output_variable (p_t, log);
  output_variable (delta_pmx, log);
  output_variable (delta_pc, log);
  output_variable (RRi, log);
  output_variable (br, log);
  output_variable (day_count, log);
  output_variable (R_c, log);
  output_variable (rho_b, log);
  output_variable (E_a, log);
  output_variable (K_bare, log);
  output_variable (RRt, log);
  output_variable (delta_pmx, log);
  output_variable (alpha, log);
  output_variable (n, log);
  output_variable (l, log);
  output_variable (K_sat, log);
  output_variable (K_15, log);
  output_variable (K_30, log);
  output_variable (K_60, log);
  output_variable (K_120, log);
  output_variable (K_fc, log);
  output_variable (K_wp, log);
  output_variable (Theta_sat, log);
  output_variable (Theta_fc, log);
  output_variable (Theta_wp, log);
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
HydraulicWEPP::initialize_wepp (const Texture& texture, 
                                const double CEC /* [cmolc/kg] */,
                                const double center_z /* [cm] */,
                                Treelog& msg)
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

  std::ostringstream tmp;
  if (p_c < 0.0)
    {
      double CECr_sane;             // [cmolc/kg soil]
      if (CECr_fixed > 0.0)
        // User specified value.
        CECr_sane = CECr_fixed;
      else
        {
          double Dg;            // [m]
          if (average_depth > 0.0)
            Dg = average_depth / 100.0;
          else if (center_z < 0.0)
            Dg = -center_z / 100.0;
          else
            {
              msg.error ("'average_depth' unspecified, assuming 10 cm");
              Dg = 0.1;
            }
          const double CECc                    // [cmolc/kg clay]
            = CEC - om * (142.0 + 170.0 * Dg); // wepp:7.7.3
          daisy_assert (clay > 0.0);
          CECr_sane = CECc / clay;
          tmp << "CECr = " << CECr_sane << " cmolc/kg clay\n";
        }

      // Apparently there is a mistake in WEPP: CECr is really mmol/g, 
      // not per mmol/100g. Source: Merete Styczen personal correspondance.
      const double CECr = CECr_sane * 0.01; // [mmolc/g clay]

  
      // Consolidated bulk density. wepp:7.7.2
      p_c = (1.514 + 0.25 * sand -  13.0 * sand * om -  6.0 * clay * om
             - 0.48 * clay * CECr) * 1000.0; // [kg / m^3]
      tmp << "Consolidated bulk density " << p_c << " kg/m^3\n";
    }
  // Fixed contribution to maximum soil bulk density change due to rain.
  // wepp:7.7.10 (first part)
  delta_pmx_clay = 1650.0 - 2900.0 * clay + 3000 * clay * clay; // [kg/m^3]

  // For calculating K_bare.
  SC = 0.736 + 0.19 * sand;                               // [?] wepp:7.9.5
  Psi = 45.19 - 46.68 * SC;                               // [?] wepp:7.9.6
  C = bound (0.0001, -0.0028 + 0.0113 * sand + 0.125 * clay / CEC,
             0.01);        // wepp:7.9.8
  b = 63.0 + 62.7 * std::log (50.0 * humus) + 1570.0 * clay
    + 2500 * clay * clay;       // [mm] wepp:7.5.4
  L_sand = 0.147 - 0.15 * sand * sand;
  L_clay = 0.0003 * clay;

  // Baseline hydraulic conductivity 
  if (CEC <= 1.0)
    {
      msg.debug ("CEC should be > 1 meq/100g");
      K_b = 10.0;
    }
  else if (clay < 0.4)
    K_b = -0.265 + 0.0086 * std::pow (100.0 * sand, 1.8) 
      + 11.46 * std::pow (CEC, -0.75);
  else
    K_b = 0.0066 * std::exp (2.44 / clay);
  tmp << "K_b = " << K_b << " mm/h";
  K_bare = K_b;
  msg.debug (tmp.str ());
}

void
HydraulicWEPP::initialize (const Texture& texture,
                           double, const bool, const double CEC, 
                           const double center_z, Treelog& msg)
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

  initialize_wepp (texture, CEC, center_z, msg);

  if (p_t < 0.0)
    p_t = p_c;
  hypres ();
  M_.clear ();
  K_to_M (M_, 500);
}

void
HydraulicWEPP::hypres ()
{
  calculate_rho_b ();
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
  
  a = -alpha;
  m = 1.0 - 1.0 / n;

  const double h_fc = -100.0;
  const double h_wp = -15000.0;
  K_15 = K (-1.5);
  K_30 = K (-3.0);
  K_60 = K (-6.0);
  K_120 = K (-12.0);
  K_fc = K (h_fc);
  K_wp = K (h_wp);
  Theta_fc = Theta (h_fc);
  Theta_wp = Theta (h_wp);
}


HydraulicWEPP::HydraulicWEPP (const BlockModel& al)
  : Hydraulic (al),
    consolidate_factor (al.number ("consolidate_factor")),
    consolidate_factor_water (al.number ("consolidate_factor_water")),
    delta_pmx_fixed (al.number ("delta_pmx_fixed", -42.42e42)),
    time_consolidation (al.number ("time_consolidation")),
    average_depth (al.number ("average_depth", -42.42e42)),
    CECr_fixed (al.number ("CECr", -42.42e42)),
    allow_negative_delta_pc (al.flag ("allow_negative_delta_pc")),
    crust_thickness (al.number ("crust_thickness")),
    p_c (al.number ("consolidated_bulk_density", -42.42e42)),
    clay (-42.42e42),
    silt (-42.42e42),
    humus (-42.42e42),
    delta_pmx_clay (-42.42e42),
    K_b (-42.42e42),
    SC (-42.42e42),
    Psi (-42.42e42),
    C (-42.42e42),
    b (-42.42e42),
    L_sand (-42.42e42),
    L_clay (-42.42e42),
    p_t (al.number ("p_t", -42.42e42)),
    delta_pmx (al.number ("delta_pmx")),
    delta_pc (al.number ("delta_pc")),
    RRi (al.number ("RRi")),
    br (al.number ("br")),
    day_count (al.number ("day_count")),
    R_c (al.number ("R_c")),
    rho_b (-42.42e42),
    E_a (al.number ("E_a")),
    RRt (al.number ("RRt")),
    K_bare (-42.42e42),
    freeze_effect (al.number ("freeze_effect")),
    freeze_on (al.number ("freeze_on")),
    freeze_off (al.number ("freeze_off")),
    frozen (al.flag ("frozen")),
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

  static bool check_alist (const Metalib&, const Frame& al, Treelog& msg)
  {
    bool ok = true;
    return ok;
  }
  HydraulicWEPPSyntax ()
    : DeclareModel (Hydraulic::component, "wepp", 
                    "van Genuchten retention curve model with Mualem theory.\n\
Parameters specified by the HYPRES transfer function.\n\
Tillage and weather dynamics from WEPP. CECr from Krogh et al.")
  { }
  void load_frame (Frame& frame) const
  { 
    frame.set_strings ("cite", "hypres", "wepp");
    frame.declare_fraction ("Theta_res",
                            Attribute::Const, "Soil residual water.");
    frame.set ("Theta_res", 0.01);
    frame.add_check (check_alist);
    frame.declare ("consolidate_factor", 
                   Attribute::None (), Attribute::Const, "\
The ratio between consolidated and loose bulk density.");
    frame.set ("consolidate_factor", 0.667);
    frame.declare ("consolidate_factor_water", 
                   Attribute::None (), Attribute::Const, "\
An additinal water dependent consolitdate factor.\n\
This is multiplied with the relative water content [0-1] at tillage,\n\
and added to `consolidate_factor'.");
    frame.set ("consolidate_factor_water", 0.0);
    frame.declare ("delta_pmx_fixed", "kg/m^3", Check::non_negative (), 
                   Attribute::OptionalConst, "\
Fixed value for 'delta_pmx'.\n\
By default 'delta_pmx' will be calculated from clay content and\n\
dry bulk density.");
    frame.declare ("time_consolidation", "d^-1", Attribute::Const, "\
Controls consolidation as a function of passing time.\n\
consolidation = max * (1 - exp (- time_consolidation * day_count))\n\
Larger values, faster consolidation.");
    frame.set ("time_consolidation", 0.005);
    frame.declare ("average_depth", "cm", Check::positive (), 
                   Attribute::OptionalConst, "\
Average depth of horizon (positive).\n\
Used for estimating 'CECr' iff unspecified.\n\
If unspecified, the value from 'Soil' is used.");
    frame.declare ("CECr", "cmolc/kg clay", Check::positive (), 
                   Attribute::OptionalConst, "\
Ratio of the cation exchange content of the clay (CECc) to the clay content.\n\
Used for calculating 'consolidated_bulk_density', iff unspecified.\n\
By default calculated from 'average_depth', CEC, and texture.\n\
\n\
If you have not specified 'CEC' in the horizon, setting 'CECr' to 50.0\n\
will make most sense due to the way CEC is estimated from texture.");
    frame.declare ("consolidated_bulk_density", "kg/m^3", 
                   Attribute::OptionalConst,
                   "Consolidated dry bulk density after rain and time.\n\
By default, this will be calculated from texture, CEC, and depth.");
    frame.declare ("p_t", "kg/m^3", Attribute::OptionalState,
                   "Dry bulk density after last tillage.\n\
By default, this will be consolidated dry bulk density per wepp.");
    frame.declare ("delta_pmx", "kg/m^3", Attribute::State,
                   "Potential dry bulk density change due to rain.");
    frame.set ("delta_pmx", 0.0);
    frame.declare ("delta_pc", "kg/m^3", Attribute::State,
                   "Potential dry bulk density change due to time.");
    frame.set ("delta_pc", 0.0);
    frame.declare ("RRi", "kg/m^3", Attribute::State,
                   "Random roughness after last tillage.");
    frame.set ("RRi", 0.0);
    frame.declare ("br", "kg DM/m^2", Attribute::State,
                   "Burried residue 0-15 cm at last tillage.");
    frame.set ("br", 0.3);
    frame.declare_boolean ("allow_negative_delta_pc", Attribute::Const, "\
Allow 'delta_pc' to be negative after tillage.");
    frame.set ("allow_negative_delta_pc", false);
    frame.declare ("crust_thickness", "m",
                   Check::positive (), Attribute::Const, 
                   "Thickness of soil layer that form crust after rain.");
    frame.set ("crust_thickness", 0.005);
    frame.declare ("day_count", "d", Check::non_negative (), Attribute::State, 
                   "Number of days since last tillage.");
    frame.set ("day_count", 0.0);
    frame.declare ("R_c", "m", Check::non_negative (), Attribute::State, 
                   "Rain since last tillage.");
    frame.set ("R_c", 0.0);
    frame.declare ("rho_b", "g/cm^3", Attribute::LogOnly,
                   "Current dry bulk density.");
    frame.declare ("E_a", "J/m^2", Check::non_negative (), Attribute::State, 
                   "Energy in rain since last tillage.");
    frame.set ("E_a", 0.0);
    frame.declare ("K_bare", "mm/h", Attribute::LogOnly, 
                   "Bare soil conductivity.");
    frame.declare ("RRt", "m", Check::positive (), Attribute::State, 
                   "Random roughness.");
    frame.set ("RRt", 0.013);
    frame.declare ("freeze_effect", "kg/m^3", Check::non_negative (),
                   Attribute::Const, "\
Maximum decrease of bulk density from frost.\n\
The freeze effect works by decreasing `R_c'. `R_c' will never be\n\
decreased below zero.");
    frame.set ("freeze_effect", 0.0);
    frame.declare_fraction ("freeze_on", Attribute::Const, "\
Volumetric ice content above this will activate the frozen state.\n\
The freeze effect will be activated if the soil is not already frozen.");
    frame.set ("freeze_on", 1.0);
    frame.declare_fraction ("freeze_off", Attribute::Const, "\
Volumetric ice content below this will deactivate frozen state.");
    frame.set ("freeze_off", 0.0);
    frame.declare_boolean ("frozen", Attribute::State, "\
True if the soil is considered frozen.\n\
The soil will be considered frozen after the volumetric ice content\n\
reaches above `freeze_on', and will stay frozen until the volumetric\n\
ice content fall below `freeze_off'. When the soil enter the frozen state\n\
the `freeze_effect' trigger.");
    frame.set ("frozen", false);
    frame.declare ("alpha", "cm^-1", Attribute::LogOnly,
                   "van Genuchten alpha.");
    frame.declare ("n", Attribute::None (), Attribute::LogOnly,
                   "van Genuchten n.");
    frame.declare ("l", Attribute::None (), Attribute::LogOnly,
                   "tortuosity parameter.");
    frame.declare ("K_sat", "cm/h", Attribute::LogOnly,
                   "Water conductivity of saturated soil.");
    frame.declare ("K_15", "cm/h", Attribute::LogOnly,
                   "Water conductivity for h = -1.5 cm.");
    frame.declare ("K_30", "cm/h", Attribute::LogOnly,
                   "Water conductivity for h = -3.0 cm.");
    frame.declare ("K_60", "cm/h", Attribute::LogOnly,
                   "Water conductivity for h = -6.0 cm.");
    frame.declare ("K_120", "cm/h", Attribute::LogOnly,
                   "Water conductivity for h = -12.0 cm.");
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

// The 'Styczen' parametrization.

static struct HydraulicStyczenSyntax : public DeclareParam
{ 
  HydraulicStyczenSyntax ()
    : DeclareParam (Hydraulic::component, "Styczen", "wepp", "\
Parameterization of for Danish soils.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.set_cited ("CECr", 50.0, "Average value for examined Danish soils.", 
                     "krogh2000cation");
    frame.set ("allow_negative_delta_pc", true);
    frame.set ("delta_pmx_fixed", 80.0);
    frame.set ("freeze_effect", 100.0);
    frame.set ("freeze_on", 0.19);
    frame.set ("freeze_off", 0.01);
  }
} HydraulicStyczen_syntax;

// hydraulic_wepp.C ends here.
