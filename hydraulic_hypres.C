// hydraulic_hypres.C
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
// van Genuchten retention curve model with Mualem theory.
// Parameters specified by the HYPRES transfer function.

#include "hydraulic.h"
#include "block.h"
#include "alist.h"
#include "texture.h"
#include "plf.h"
#include "treelog.h"
#include <sstream>
#include "mathlib.h"
#include "librarian.h"

class HydraulicHypres : public Hydraulic
{
  // Content.
  enum { top, bottom, unknown } soil_type;
  /* const */ double alpha;
  /* const */ double a;		// - alpha
  /* const */ double n;
  /* const */ double m;		// 1 - 1/n
  /* const */ double l;         // tortuosity parameter
  mutable PLF M_;

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
  
  // Create and Destroy.
public:
  HydraulicHypres (Block&);
  void initialize (const Texture&, double rho_b, bool top_soil,
		   Treelog& msg);
public:
  ~HydraulicHypres ();
};

double 
HydraulicHypres::Theta (const double h) const
{
  return Se (h) * (Theta_sat - Theta_res) + Theta_res;
}

double 
HydraulicHypres::K (const double h) const
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
HydraulicHypres::Cw2 (const double h) const
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
HydraulicHypres::h (const double Theta) const
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
HydraulicHypres::M (double h) const
{
  if (M_.size () == 0)
    K_to_M (M_, 500);

  return M_ (h);
}

double 
HydraulicHypres::Se (double h) const
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
HydraulicHypres::initialize (const Texture& texture,
                             double rho_b, bool top_soil, Treelog& msg)
{
  Treelog::Open nest (msg, name);

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

  Hydraulic::initialize (texture, rho_b, top_soil, msg);
  daisy_assert (K_sat > 0.0);


  // Debug messages.
  std::ostringstream tmp;
  tmp << ";; clay = " << clay << ", silt = " << silt << ", sand = "
	 << sand << ", humus = " << humus << ", rho_b = " << rho_b 
	 << (top_soil ? " (topsoil) " : " (subsoil)") << "\n";
  tmp << "M_vG\n";
  tmp << "(l " << l << ")\n";
  tmp << ";; (m " << m << ")\n";
  tmp << "(n " << n << ")\n";
  tmp << "(alpha " << alpha << ")\n";
  tmp << "(K_sat " << K_sat << " [cm/h])\n";
  tmp << "(Theta_sat " << Theta_sat << ")";
  msg.debug (tmp.str ());
}


HydraulicHypres::HydraulicHypres (Block& al)
  : Hydraulic (al),
    soil_type (al.check ("topsoil") 
	       ? (al.flag ("topsoil") ? top : bottom)
	       : unknown),
    alpha (-42.42e42),
    a (-42.42e42),
    n (-42.42e42),
    m (-42.42e42),
    l (-42.42e42),
    M_ ()
{ }

HydraulicHypres::~HydraulicHypres ()
{ }

// Add the HydraulicHypres syntax to the syntax table.

static struct HydraulicHypresSyntax
{
  static Model& make (Block& al)
  { return *new HydraulicHypres (al); }

  static bool check_alist (const AttributeList&, Treelog&)
  {
    bool ok = true;
    return ok;
  }
  HydraulicHypresSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    syntax.add_check (check_alist);
    alist.add ("description", 
	       "van Genuchten retention curve model with Mualem theory.\n\
Parameters specified by the HYPRES transfer function.\n\
See <http://www.macaulay.ac.uk/hypres/>.");
    Hydraulic::load_K_sat_optional (syntax, alist);
    syntax.add ("topsoil", Syntax::Boolean, Syntax::OptionalConst, "\
If set true this horizon will be initialized as a topsoil (i.e. the\n\
plowing layer), if set false it will be initialized as a subsoil.\n\
By default, the horizon will be initialized as a topsoil if and only if\n\
it is the topmost horison in the soil profile.");
    Librarian::add_type (Hydraulic::component, "hypres", alist, syntax, make);
  }
} hydraulicHypres_syntax;

