// hydraulic_hypweb.C
// 
// Copyright 1996-2002 Per Abrahamsen and Søren Hansen
// Copyright 2000-2002 KVL.
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
// 2step pedotransfer model for the Brunschwick-VGM model.
//           VGM Parameters specified by the HYPRES transfer function.
//           Brunschwick-VGM parameters accoring to Weber.et.al.2020 WRR"

#define BUILD_DLL
using namespace std;
#include "hydraulic.h"
#include "block_model.h"
#include "texture.h"
#include "plf.h"
#include "treelog.h"
#include <sstream>
#include "mathlib.h"
#include "librarian.h"
#include "frame.h"
#include <iostream>
#include <fstream>
#include <complex>
#include "hyp_2F1.h"

#ifndef M_LN10
const double M_LN10 = 2.302585092994046;
#endif

class HydraulicHypweb : public Hydraulic
{
  // Content.
  enum { top, bottom, unknown } soil_type;
  /* const */ double alpha;
  /* const */ double a;		// - alpha
  /* const */ double n;
  /* const */ double m;		// 1 - 1/n
  /* const */ double l;         // tortuosity parameter
  const double pf0;
  const double afilm;
  /* const */  double Theta_cap;
  const double Theta_nc;
  /* const */  double Ks_cap;
  const double Ks_nc;
  double sumk;
  double Gama_h_cap0 = pow(1.0 / (1.0 + pow(a * pow(10.0, pf0), n)), m);
  double Gama_h_nc0 = (1.0 / n / M_LN10)*(Beta_inc(1.0 + pow(alpha*pow(10.0, pf0), n), 1.0) - Beta_inc(1.0 + pow(alpha*pow(10.0, pf0), n), 1.0 / n) + n - 1.0 / n + (1.0 / n - 1.0)*sumk); 
  /* const   double Gama_h_cap0 = pow(1.0 / (1.0 + pow(a * pow(10.0, pf0), n)), m); */
  /* const   double Gama_h_nc0 = (1.0 / n / M_LN10)*(Beta_inc(1.0 + pow(alpha*pow(10.0, pf0), n), 1.0) - Beta_inc(1.0 + pow(alpha*pow(10.0, pf0), n), 1.0 / n) + n - 1.0 / n + (1.0 / n - 1.0)*sumk); */
  mutable PLF M_;
  PLF pF_Theta;

  // Prevent changing Theta_sat.
public:
  void set_porosity (double)
  { throw ("Can't change porosity for HYPRES hydraulic model"); }
  void output (Log&) const
  { };

  // Use.
public:
  double Theta (double h) const;
  double K (double h) const;
  double Cw2 (double h) const;
  double h (double Theta) const;
  double M (double h) const;
private:
  double Se (double h) const;
  double Se_h_cap(double h) const;
  double Se_h_nc(double h) const;
  double Beta_inc(const double x, const double barg1) const;
  // Create and Destroy.
public:
  HydraulicHypweb (const BlockModel&);

  HydraulicHypweb (symbol name, double Ks_cap);    //i have to ask Per here it was K_sat
  void initialize (const Texture&, double rho_b, bool top_soil, double CEC,
                   double center_z, Treelog& msg);
public:
  ~HydraulicHypweb ();
};


double
HydraulicHypweb::Beta_inc(const double x, const double barg1) const
{
	return pow(x, barg1) / barg1 * real(hyp_2F1(barg1, 1.0, barg1 + 1.0, x));
}

double 
HydraulicHypweb::Theta (const double h) const
{
	const double temp1 = Se_h_cap(h);
	const double 	temp2 = Se_h_nc(h);
	return Theta_cap * Se_h_cap(h) + Theta_nc * Se_h_nc(h);
}

double 
HydraulicHypweb::K (const double h) const
{
	if (h < 0.0)
	{
		const double Gama_h_cap = pow(1.0 / (1.0 + pow(alpha * pow(10.0, log10(abs(h))), n)), m);

		return Ks_cap * pow(Se_h_cap(h), l)*pow(1.0 - pow((1.0 - pow(Gama_h_cap, 1.0 / m)) / (1.0 - pow(Gama_h_cap0, 1.0 / m)), m), 2.0) +
			Ks_nc * pow(pow(10.0, pf0), -afilm * (1.0 - Se_h_nc(h)));



	}
	else
		return Ks_cap + Ks_nc;
}

double 
HydraulicHypweb::Cw2 (const double h) const
{
	const double Gama_h_cap = pow(1.0 / (1.0 + pow(alpha * pow(10.0, log10(abs(h))), n)), m);

	if (h < 0.0)
		return Theta_cap * (n - 1.0)*h*-pow(alpha, n)*pow(abs(h), n - 2.0)*pow(pow(alpha, n)*pow(abs(h), n) + 1.0, 1.0 / n - 2.0)
		+ Theta_nc * (-1.0 / Gama_h_nc0)*(Gama_h_cap - 1.0) / h / M_LN10;

	else
		return 0.0;
}

double 
HydraulicHypweb::h (const double Theta) const
{
 // daisy_assert (Theta_res <= Theta);
	if (Theta < Theta_cap + Theta_nc)

		return pF2h(pF_Theta(Theta));
	else
		return 0.0;
}

double 
HydraulicHypweb::M (double h) const
{
  if (M_.size () == 0)
    K_to_M (M_, 500);

  return M_ (h);
}

double
HydraulicHypweb::Se_h_cap(double h) const
{
	const double Gama_h_cap = pow(1.0 / (1.0 + pow(alpha * pow(10.0, log10(abs(h))), n)), m);
	daisy_assert(Gama_h_cap0<1.0);
	if (h < 0.0)
		return (Gama_h_cap - Gama_h_cap0) / (1.0 - Gama_h_cap0);

	else
		return 1.0;
}


double
HydraulicHypweb::Se_h_nc(double h) const
{
	const double Gama_h_nc = (1.0 / n / M_LN10)*(Beta_inc(1.0 + pow(alpha*pow(10.0, log10(abs(h))), n), 1.0) - Beta_inc(1.0 + pow(alpha*pow(10.0, log10(abs(h))), n), 1.0 / n) + n - 1.0 / n + (1.0 / n - 1.0)*sumk);
	if (h<0.0)
		return 1.0 - Gama_h_nc / Gama_h_nc0;
	else
		return 1.0;

}

double 
HydraulicHypweb::Se (double h) const
{
	const double Se_h = (Se_h_cap(h)*Theta_cap + Se_h_nc(h) * Theta_nc) / (Theta_cap + Theta_nc);

	daisy_assert(Se_h >= 0.0);
	daisy_assert(Se_h <= 1.0);
	return Se_h;
}

void
HydraulicHypweb::initialize (const Texture& texture,
                             double rho_b, bool top_soil, double CEC, 
                             double center_z, Treelog& msg)
{
  TREELOG_MODEL (msg);

  const double clay_lim 
    = texture.fraction_of_minerals_smaller_than ( 2.0 /* [um] USDA Clay */);
  const double silt_lim 
    = texture.fraction_of_minerals_smaller_than (50.0 /* [um] USDA Silt */);
  daisy_assert (clay_lim >= 0.0);
  daisy_assert (silt_lim >= clay_lim);
  daisy_assert (silt_lim <= 1.0);
  const double mineral = texture.mineral ();
  /* const */ double clay = mineral * clay_lim * 100 /* [%] */;
  /* const */ double silt = mineral * (silt_lim - clay_lim) * 100 /* [%] */;
  const double sand = mineral * (1.0 - silt_lim) * 100 /* [%] */;
  /* const */ double humus = texture.humus * 100 /* [%] */;

  if (soil_type == top)
    top_soil = true;
  else if (soil_type == bottom)
    top_soil = false;
  else
    daisy_assert (soil_type == unknown);

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

  if (rho_b <= 0.0)
    {
      msg.error ("\
You must specify dry_bulk_density in order to use the hypres \
pedotransfer function");
      rho_b = 1.5;
    }
  if (!approximate (clay + silt + sand + humus, 100.0))
    {
      std::ostringstream tmp;
      tmp << "The sum of all fractions should be 100%, it is "
             << clay + silt + sand + humus;
      msg.error (tmp.str ());
    }
  

  Theta_cap =0.7919 + 0.001691 * clay - 0.29619 * rho_b 
    - 0.000001491 * silt * silt
    + 0.0000821 * humus * humus + 0.02427 / clay + 0.01113 / silt
    + 0.01472 * log (silt) - 0.0000733 * humus * clay - 0.000619 * rho_b * clay
    - 0.001183 * rho_b * humus;
  if (top_soil)
    Theta_cap -= 0.0001664 * silt;
  daisy_assert (Theta_cap > 0.0);
  daisy_assert (Theta_cap < 1.0);
  Theta_cap = 0.993*Theta_cap + 1.89e-3 - Theta_nc;
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
  alpha = pow(10.0,0.986*log10(exp (alpha_star))-2.06e-2);

  double n_star 
    = -25.23 - 0.02195 * clay + 0.0074 * silt - 0.1940 * humus + 45.5 * rho_b
    - 7.24 * rho_b * rho_b + 0.0003658 * clay * clay + 0.002885 * humus * humus
    - 12.81 / rho_b - 0.1524 / silt - 0.01958 / humus - 0.2876 * log (silt)
    - 0.0709 * log (humus) - 44.6 * log (rho_b) - 0.02264 * rho_b * clay
    + 0.0896 * rho_b * humus;
  if (top_soil)
    n_star += 0.00718 * clay;
  n =1.0+pow(10.0,0.933*log10(exp (n_star))+6.42e-2);

  const double l_star 
    = 0.0202 + 0.0006193 * clay * clay - 0.001136 * humus * humus
    - 0.2316 * log (humus) - 0.03544 * rho_b * clay + 0.00283 * rho_b * silt
    + 0.0488 * rho_b * humus;
  l = (10.0 * exp (l_star) - 10.0) / (1.0 + exp (l_star));
  l = 1.833*l + 2.95e-2;
  
  if (Ks_cap+Ks_nc < 0.0 && K_init == NULL)
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
	  Ks_cap = log10(exp(K_sat_star) / 24.0);
	  Ks_cap = pow(10.0,1.060*Ks_cap + 1.16e-01)-Ks_nc;
  }
  
  a = -alpha;
  m = 1.0 - 1.0 / n;
  Theta_sat = Theta_cap + Theta_nc;

  Gama_h_cap0 = pow(1.0 / (1.0 + pow(alpha * pow(10.0, pf0), n)), m);
  sumk = 0.0;
  for (double k = 1.0; k < 20.5; k++)
  {
	  sumk = sumk + 1.0 / k / (k + 1.0) / (n*k + 1.0);
  }
  Gama_h_nc0 = (1.0 / n / M_LN10)*(Beta_inc(1.0 + pow(alpha*pow(10.0, pf0), n), 1.0) - Beta_inc(1.0 + pow(alpha*pow(10.0, pf0), n), 1.0 / n) + n - 1.0 / n + (1.0 / n - 1.0)*sumk);
 
  const double pf_min = -6.0;
  const double pf_max = pf0;
  const double dpf = 0.01;
  const int nsteps = (abs(pf_min) + pf_max) / dpf;
  for (int i = nsteps; i >= 0; --i)
  {
	  const double x = pf_min + dpf * i;
	  const double y = Theta(-pow(10.0, x));
	  pF_Theta.add(y, x);	// code block to be executed
  }

 
  Hydraulic::initialize (texture, rho_b, top_soil, CEC, center_z, msg);
  daisy_assert (Ks_cap+Ks_nc > 0.0);


  // Debug messages.
  std::ostringstream tmp;
  tmp << ";; clay = " << clay << ", silt = " << silt << ", sand = "
	 << sand << ", humus = " << humus << ", rho_b = " << rho_b 
	 << (top_soil ? " (topsoil) " : " (subsoil)") << "\n";
  tmp << "M_vGBS\n";
  tmp << "(Theta_cap " << Theta_cap << ")\n";
  tmp << "(Theta_nc " << Theta_nc << ")\n";
  tmp << "(alpha " << alpha << " [cm^-1])\n";
  tmp << "(n " << n << ")\n";
  tmp << ";; (m " << m << ")\n";
  tmp << "(pf0 " << pf0 << ")\n";
  tmp << "(Ks_cap " << Ks_cap << " [cm/h])\n";
  tmp << "(Ks_nc " << Ks_nc << " [cm/h])\n";
  tmp << "(afilm " << afilm << ")\n";
  tmp << "(l " << l << ")\n";
  msg.debug (tmp.str ());
}


HydraulicHypweb::HydraulicHypweb (const BlockModel& al)
  : Hydraulic (al),
    soil_type (al.check ("topsoil") 
	       ? (al.flag ("topsoil") ? top : bottom)
	       : unknown),
    alpha (-42.42e42),
    a (-42.42e42),
    n (-42.42e42),
    m (-42.42e42),
    l (-42.42e42),
	pf0(al.number("pf0")),
	afilm(al.number("afilm")),
	Theta_nc(al.number("Theta_nc")),
	Ks_cap(-42.42e42),
	Ks_nc(al.number("Ks_nc")),
    M_ ()
{
	
}

HydraulicHypweb::~HydraulicHypweb ()
{ }

// Add the HydraulicHypres syntax to the syntax table.

static struct HydraulicHypwebSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new HydraulicHypweb (al); }

  static bool check_alist (const Metalib&, const Frame&, Treelog&)
  {
    bool ok = true;
    return ok;
  }
  HydraulicHypwebSyntax ()
    : DeclareModel (Hydraulic::component, "hypweb", 
	       "2step pedotransfer model for the Brunschwick-VGM model.\n\
           VGM Parameters specified by the HYPRES transfer function.\n\
           Brunschwick-VGM parameters accoring to Weber.et.al.2020 WRR")
  { }
  void load_frame (Frame& frame) const
  { 
    frame.set_strings ("cite", "hypweb");
    frame.add_check (check_alist);
//    Hydraulic::load_K_sat_optional (frame);
    frame.declare_fraction ("Theta_nc", Attribute::Const, "saturated water content of the non-capillary part");
    frame.set_cited("Theta_nc",1.285*0.01-1.58e-3,"Table 3", "weber2019modular");
	frame.declare("pf0", Attribute::None(), Attribute::Const,
		"pf at oven dryness");
	frame.set("pf0", 6.8);
	frame.declare("Ks_nc", "cm/h", Attribute::Const,
		"saturated conductivity of the non-capillary part");
	frame.set_cited("Ks_nc", pow(10.0, -1.72) / 24.0, "Table 3", "weber2019modular");
	frame.declare("afilm", Attribute::None(), Attribute::Const,
		"slope of loglog film conductivity");
	frame.set_cited("afilm", 1.5, "", "weber2019modular");
    frame.declare_boolean ("topsoil", Attribute::OptionalConst, "\
If set true this horizon will be initialized as a topsoil (i.e. the\n\
plowing layer), if set false it will be initialized as a subsoil.\n\
By default, the horizon will be initialized as a topsoil if and only if\n\
it is the topmost horison in the soil profile.");
  }
} hydraulicHypweb_syntax;

// hydraulic_hypres.C ends here.
