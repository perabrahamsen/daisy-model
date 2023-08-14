// hydraulic_hyprop.C -- HYPROP-FIT PDI functions.
// 
// Copyright 2023 KU
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
#include "librarian.h"
#include "frame.h"
#include "assertion.h"
#include "mathlib.h"
#include "check.h"
#include "vcheck.h"
#include "iterative.h"
#include <sstream>
#include <algorithm>

class HydraulicHyprop : public Hydraulic
{
  // Parameters.
  const double h0;		// Pressure at minimal water [cm]
  const double ha;		// Pressure at air entry [cm]
  const double x0;		// h2pF (h0) [pF]
  const double xa;		// h2pF (ha) [pF]

  // multi modal van Genuchten parameters.
  const size_t modes;		// Number of modes.
  const std::vector<double> w;	// [] Weight for each mode, sum = 1
  const std::vector<double> alpha; // [cm^1] vG alpha parameter
  const std::vector<double> n;	// [] vG n parameter
  const std::vector<double> m;	// [] vG m parameter
  const double Gamma0;		// [] Gamma (h0)
  const double b;		// [pF] Shape parameter.

  // Mualem conductivity parameters.
  const double omega;		// [] Weight of film conductivity.
  const symbol vapor_conductivity; // 'false', 'true' or 'only'.
  const double a;		// [] Slope of the log-log scale.
  const double tau;		// [] Capillary conductivity parameter.
  
  mutable PLF M_;

  // Adsorptive saturation function
  double S_ad (const double h) const
  {
    if (h >= 0.0)
      return 1.0;
    if (h <= h0)
      return 0.0;
    
    const double x = h2pF (h);

    const double result		// Eq 2.
      =  1.0
      + (1.0 / (xa - x0))
      * (x - xa + b * std::log (1.0 + std::exp ((xa - x) / b)));

#if 0
    std::ostringstream tmp;
    tmp << "result  = " << result
	<< "; ha = " << ha
	<< "; xa = " << xa
	<< "; x0 = " << x0
	<< "; b = " << b
	<< "; h = " << h
	<< "; x = " << x
	<< "; (xa - x) / b) = " << (xa - x) / b
	<< "; std::exp ((xa - x) / b) = " << std::exp ((xa - x) / b)
	<< "; std::log (1.0 + std::exp ((xa - x) / b)) = " << std::log (1.0 + std::exp ((xa - x) / b))
	<< "; (1.0 / (xa - x0)) = " << (1.0 / (xa - x0))
	<< "; (xa - x) / b = " << (xa - x) / b
    	<< "; std::exp ((xa - x) / b) = " << std::exp ((xa - x) / b);

    Assertion::message (tmp.str ());
#endif
    
    daisy_assert (result >= 0.0);
    daisy_assert (result <= 1.01);
    return result;
  }

  // van Genuchten retention.
  double Gamma (const int i, const double h) const
  {
    const double result		// Eq 6.
      = std::pow (1.0 / (1.0 + pow (-alpha[i] * h, n[i])), m[i]);
    daisy_assert (result >= 0.0);
    daisy_assert (result <= 1.0);
    return result;
  }
  
  double Gamma (const double h) const
  {
    // Eq 8.
    double sum = 0.0;
    for (int i = 0; i < modes; ++i)
      sum += w[i] * Gamma (i, h);
    daisy_assert (sum >= 0.0);
    daisy_assert (sum <= 1.0);
    return sum;
  }
  
  // Capillary saturation function.
  double S_cap (const double h) const
  {
    if (h >= 0.0)
      return 1.0;
    if (h <= h0)
      return 0.0;
    
    const double result 	// Eq 3.
      = (Gamma (h) - Gamma0) / (1.0 - Gamma0);

    daisy_assert (result >= -0.01);
    daisy_assert (result <= 1.0);
    return std::max (result, 0.0);
  }

  // Relative conductivity for capillary flow.
  double K_cap_rel (const double h) const
  {
    if (h >= 0.0)
      return 1.0;
    if (h <= h0)
      return 0.0;

    // Eq 25.
    double numerator = 0.0;
    double denominator = 0.0;
    for (int i = 0; i < modes; ++i)
      {
	numerator += w[i] * alpha[i]
	  * std::pow (1.0 - std::pow (Gamma (i, h), 1.0 / m[i]), m[i]);
	denominator += w[i] * alpha[i]
	  * std::pow (1.0 - std::pow (Gamma (i, h0), 1.0 / m[i]), m[i]);
      }
    daisy_assert (std::isnormal (denominator));
    const double result = std::pow (S_cap (h), tau)
      * sqr (1.0 - numerator / denominator);
    daisy_assert (result >= 0.0);
    daisy_assert (result <= 1.0);

    return result;
  }

  double K_film_rel (const double h) const
  {
    // Eq 28.
    const double S = S_ad (h);
    const double result = std::pow (h0/ha, a * (1.0 - S));
    daisy_assert (result >= 0.0);
    daisy_assert (result <= 1.01);
    return result;
  }

  double K_vap (const double h, const double T_dgC) const
  {
    if (h >= 0.0)
      return 0.0;

    // Kelvin.
    const double T_zero = 273.15; // [K]
    
    // Temperature [K]
    const double T = T_zero + T_dgC;

    // Volumetric air content []
    const double Theta_a = Theta_sat - Theta (h);

    if (Theta_a <= 0.0)
      return 0.0;
    
    // Universal gas constant [J/mol/K]
    const double R = 8.314;
    
    // Molecular weight of water [kg/mol]
    const double M = 0.018015;

    // Gravitational accelaration [m/s^2]
    const double g = 9.81;

    // Liquid density of water [kg/m^3]

    // Eq 35. The Kelvin equation. Relative humidity []
    // Errata: h in eq is pressure head [m], not suction [cm],
    // so they should multiply with -0.01  We use pressure head [cm],
    // so we multiply with 0.01
    const double H_r = std::exp (0.01 * h * M * g / (R * T));
    // [] = exp ([]) = exp ([m] [kg/mol] * [m/s^2] / ([J/mol/K] [K]))
    daisy_assert (H_r <= 1);

    // Eq 34. Saturated vapor density [kg/m^3]
    const double rho_sv
      = 1e-3 * std::exp (31.3716 - 6014.79 / T - 7.92495e-3 * T) / T;
    daisy_assert (rho_sv > 0);

    // Liquid water density [kg/m^3]
    const double rho_w = 1000.0;

    // Eq 33. Diffusivity of water vapor in air [m^2/s]
    const double D_a = 2.14e-5 * sqr (T / T_zero);

    // Eq 32. Tortuosity factor for gas transport. []
    // Millingstom and Quirk (1961).
    const double xi = std::pow (Theta_a, 7.0 / 3.0) / sqr (Theta_sat);
      
    // Eq 31. Vapor diffusivity [m^2/s]
    const double D = xi * Theta_a * D_a;
    
    // Eq 30.
    const double result = (rho_sv / rho_w) * D * (M * g) / (R * T) * H_r;
    // [m/s] = [m^2/s] * [kg/mol] * [m/s^2] / ([J/mol/K] K) 

    if (!(result >= 0.0))
      {
	std::ostringstream tmp;
	tmp << "h = " << h
	    << "\nT_dgC = " << T_dgC
	    << "\nT = " << T
	    << "\nTheta_a = " << Theta_a
	    << "\nH_r = " << H_r
	    << "\nrho_sv = " << rho_sv
	    << "\nD_a = " << D_a
	    << "\nxi = " << xi
	    << "\nD = " << D
	    << "\nresult = " << result;
	Assertion::warning (tmp.str ());
	return 0.0;
      }

    const double m_per_s_to_cm_h = 360000.0;
    return result * m_per_s_to_cm_h;
  }
    
  
  // Interface.
  double Theta (const double h) const // Eq 1.
  {
    if (h >= 0.0)
      return Theta_sat;

    const double result 	// Eq 1.
      = (Theta_sat - Theta_res) * S_cap (h) + Theta_res * S_ad (h);
    daisy_assert (result >= 0.0);
    daisy_assert (result <= 1.0);
    return result;
  }

  double KT (const double h, const double T) const
  {
    if (h >= 0.0)
      return K_sat;
    
    static const symbol only_s = "only";
    if (vapor_conductivity == only_s)
      return K_vap (h, T);
    
    static const symbol false_s = "false";
    if (vapor_conductivity == false_s)
      return K_sat * ((1 - omega) * K_cap_rel (h) + omega * K_film_rel (h));
    
    // Eq 29.
    return K_sat * ((1 - omega) * K_cap_rel (h) + omega * K_film_rel (h))
      + K_vap (h, T);
  }
  double Cw2 (const double h) const
  {
    if (h >= 0.0)
      return 0;
    
    const double x = h2pF (h);

    // Eq 42.
    double dGamma_dh = 0.0;
    for (int i = 0; i < modes; i++)
      {
	// Eq 39. 
	const double dGamma_i_dh
	  = alpha[i] * n[i] * m[i]
	  * std::pow (-alpha[i] * h, n[i] - 1.0)
	  * std::pow (1.0 + std::pow (-alpha[i] * h, n[i]),
		      -(m[i] + 1.0))
	  // Scaling.
	  / (1.0 - Gamma (i, h0));

	dGamma_dh += w[i] * dGamma_i_dh;
      }

    // Eq 38.
    const double dS_ad_dh
      = (1.0 / (h * M_LN10 * (xa - x0)))
      * (1.0 - (std::exp ((xa - x) / b)
		/ (1.0 + std::exp ((xa - x) / b))));

    // Eq 37.
    const double result
      = (Theta_sat - Theta_res) / (1.0 - Gamma0) * dGamma_dh
      + Theta_res * dS_ad_dh;
	       
    return result;
  }

  double h (const double Theta_in) const
  {
    if (Theta_in >= Theta_sat)
      return 0.0;
    
    // Bisection.

    const double h_fc = -100;	// [cm]
    const double Theta_fc = Theta (h_fc); // []
    const double pF_fc = h2pF (h_fc); // [pF]

    double h_out;
    if (Theta_in >= Theta_fc)
      {
	const auto f
	  = [&, this](const double h) { return Theta (h) - Theta_in; };
	h_out = bisection (h_fc, 0.0, f);
      }
    else
      {
	const auto f
	  = [&, this](const double pF) { return Theta (pF2h (pF)) - Theta_in; };
	h_out = pF2h (bisection (pF_fc, 20, f));
      }

    // Check result.
    const double Theta_out = Theta (h_out);
    if (!approximate (Theta_in, Theta_out))
      {
	std::ostringstream tmp;
	tmp << "h (" << Theta_in << ") = " << h_out
	    << " but Theta (" << h_out << ") = " << Theta_out;
	Assertion::warning (tmp.str ());
      }
    return h_out;
  }
  double M (double h) const;


  // Create and Destroy.
  void initialize (const Texture&, double rho_b, bool top_soil,
		   double CEC, double center_z, Treelog& msg)
  {
    TREELOG_MODEL (msg);

    std::ostringstream tmp;
    tmp << "Gamma0 = " << Gamma0 * 100.0 << " % rel, "
	<< Gamma0 * (Theta_sat - Theta_res) * 100.0 << " % abs"
	<< "\nha = " << ha << " cm";
    msg.debug (tmp.str ());
  }
public:
  HydraulicHyprop (const BlockModel&);
  virtual ~HydraulicHyprop ();
};

double 
HydraulicHyprop::M (double h) const
{
  if (M_.size () == 0)
    K_to_M (M_, 500);

  return M_ (h);
}

static double
find_ha (const BlockModel& al)
{
  if (al.check ("ha"))
    return al.number ("ha");
  const std::vector<double>& alpha = al.number_sequence ("alpha");
  daisy_assert (alpha.size () > 0);
  const auto i =  std::max_element (alpha.begin (), alpha.end());
  daisy_assert (i != alpha.end ());
  const double value = -*i;
  daisy_assert (std::isnormal (value));
  daisy_assert (value <= 0.0);
  const double result = 1.0 / value;
  daisy_assert (std::isnormal (result));
  return result;
}

static std::vector<double>
find_m (const BlockModel& al)
{
  if (al.check ("m"))
    return al.number_sequence ("m");
  const std::vector<double> n = al.number_sequence ("n");
  std::vector<double> m;
  for (double value: n)
    m.push_back (1.0 - 1.0 / value);
  return m;
}

static double
find_b (const BlockModel& al)
{
  const double Theta_res = al.number ("Theta_res");
  const double Theta_sat = al.number ("Theta_sat");
  const std::vector<double> alpha = al.number_sequence ("alpha");
  const std::vector<double> n = al.number_sequence ("n");
  
  const int i
    =  std::max_element (alpha.begin (), alpha.end()) - alpha.begin ();
  daisy_assert (i >= 0);
  daisy_assert (i < alpha.size ());
  const double b		// Eq
    = 0.1 + (0.2 / sqr (n[i]))
    * (1.0 - std::exp (-sqr (Theta_res / (Theta_sat - Theta_res))));
  return b;
}

HydraulicHyprop::HydraulicHyprop (const BlockModel& al)
  : Hydraulic (al),
    h0 (al.number ("h0")),
    ha (find_ha (al)),
    x0 (h2pF (h0)),
    xa (h2pF (ha)),
    modes (al.number_sequence ("w").size ()),
    w (al.number_sequence ("w")),
    alpha (al.number_sequence ("alpha")),
    n (al.number_sequence ("n")),
    m (find_m (al)),
    Gamma0 (Gamma (h0)),
    b (find_b (al)),
    omega (al.number ("omega")),
    vapor_conductivity (al.name ("vapor_conductivity")),
    a (al.number ("a")),
    tau (al.number ("tau"))
	   
{
  daisy_assert (modes > 0);
  daisy_assert (w.size () == modes);
  daisy_assert (alpha.size () == modes);
  daisy_assert (n.size () == modes);
  daisy_assert (m.size () == modes);
}
    
HydraulicHyprop::~HydraulicHyprop ()
{ }

static struct HydraulicHypropSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new HydraulicHyprop (al); }
  HydraulicHypropSyntax ()
    : DeclareModel (Hydraulic::component, "hyprop", "\
HYPROP-FIT parameters for bimodal van Genuchten retention curve.\n\
It includes adsorptive water retention and scaling (code 1211).")
  { }
  static bool check_alist (const Metalib&, const Frame& al, Treelog& msg)
  {
    bool ok = true;
    
    // Check number of modes is consistent.
    const int w = al.number_sequence ("w").size ();
    const int alpha = al.number_sequence ("alpha").size ();
    const int n = al.number_sequence ("n").size ();
    const int m = al.check ("m")
      ? al.number_sequence ("m").size ()
      : w;
    if (w != alpha || w != n || w != m)
      {
	msg.error ("'w', 'alpha', 'n', and 'm' moust all be the same size");
	ok = false;
      }

    return ok;
  }
  void load_frame (Frame& frame) const
  {
    frame.add_check (check_alist);
    frame.set_strings ("cite", "iden2014comment", "peters2013", "hypropfit");
    Hydraulic::load_Theta_res (frame);
    Hydraulic::load_K_sat (frame);
    frame.declare ("ha", "cm", Check::negative (), Attribute::OptionalConst, "\
Pressure at air entry for the adsorptive retention.\n\
By default, 1/max (alpha).");
    frame.declare ("h0", "cm", Check::negative (), Attribute::Const, "\
Pressure where the water content is zero.");
    const double x0 = 6.8;	// [pF]
    frame.set_cited ("h0", pF2h (x0), "\
The pressure at oven dryness for 105 dg C", "schneider2012prediction");
    frame.declare_fraction ("w", Attribute::Const, Attribute::Variable, "\
Weight of each mode. Sum must be 1.");
    frame.set_check ("w", VCheck::sum_equal_1 ());
    frame.declare ("alpha", "cm^-1", Check::positive (),
		   Attribute::Const, Attribute::Variable, "\
van Genuchten alpha parameters for each mode.");
    frame.set_check ("alpha", VCheck::min_size_1 ());
    frame.declare ("n", Attribute::None (),
		   Attribute::Const, Attribute::Variable, "\
van Genuchten n parameters for each mode.");
    frame.set_check ("n", VCheck::min_size_1 ());
    frame.declare ("m", Attribute::None (), Attribute::OptionalConst,
		   Attribute::Variable, "\
van Genuchten m parameters for each mode. By default (1 - (1 / n)).");
    frame.set_check ("m", VCheck::min_size_1 ());
    frame.declare_fraction ("omega", Attribute::Const, "\
Weight of film conductivity.");
    frame.declare_string ("vapor_conductivity", Attribute::Const, "\
Include vapor conductivity.\n\
Possible values, 'true, 'false', 'only'.\n\
'false': include capillary and film conductivity, but not vapor.\n\
'true': include capillary, film, and vapor conductivity.\n\
'only': include vapor conductivity but not capillary or film.");
    frame.set ("vapor_conductivity", "true");
    static VCheck::Enum vapor_check ("false", "true", "only");
    frame.set_check ("vapor_conductivity", vapor_check);
    frame.declare ("a", Attribute::None (), Attribute::Const, "\
Slope on the log-log scale.");
    frame.set_cited ("a", -1.5, "Fixed value.", "peters2013");
    frame.declare ("tau", Attribute::None (), Attribute::Const, "\
Capillary conductivity parameter.");
  }
} hydraulicHyprop_syntax;

// hydraulic_hyprop.C ends here.
