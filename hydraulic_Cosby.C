// hydraulic_Cosby.C
// 
// Copyright 2002 Per Abrahamsen and KVL.
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
//
// Modified Campbell retention curve model with Burdine theory.
// Parameters specified by Cosby et al.

#include "hydraulic.h"
#include "texture.h"
#include "treelog.h"
#include "mathlib.h"
#include <sstream>

class Hydraulic_Cosby : public Hydraulic
{
  // Content.
  /* const */ double h_b;
  /* const */ double b;

  // Prevent changing Theta_sat.
public:
  void set_porosity (double)
  { throw ("Can't change porosity for Cosby_et_al hydraulic model"); }
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
  double Sr (double h) const;
  
  // Create and Destroy.
public:
  void initialize (const Texture&, double rho_b, bool top_soil, Treelog&);
  Hydraulic_Cosby (Block&);
  ~Hydraulic_Cosby ();
};

double 
Hydraulic_Cosby::Theta (const double h) const
{
  daisy_assert (Theta_sat >= 0.0);
  return Sr (h) * Theta_sat;
}

double 
Hydraulic_Cosby::K (const double h) const
{
  daisy_assert (K_sat > 0.0);
  return K_sat * pow (Sr (h), (2 + 3 / b) * b);
}

double 
Hydraulic_Cosby::Cw2 (const double h) const
{
  daisy_assert (Theta_sat >= 0.0);
  if (h < 0.0)
    {
      return -(Theta_sat 
	       * pow (h / h_b, 4.) 
	       * pow (1. / (pow (h / h_b, 5.) + 1.), 0.2 / b - 1) 
	       / (b * h_b * pow (pow (h / h_b, 5.) + 1., 2)));
    }
  else
    return 0.0;
}

double 
Hydraulic_Cosby::h (const double Theta) const
{
  daisy_assert (Theta_sat >= 0.0);
  if (Theta < Theta_sat)
    return h_b * pow (pow (Theta / Theta_sat, -5.0 * b) - 1.0, 1.0/5.0);
  else
    return 0.0;
}

double 
Hydraulic_Cosby::M (double h) const
{
  daisy_assert (K_sat > 0.0);
  if (h <= h_b)
    return K_sat * (-h_b / (1.0 + 3.0 / b)) * pow (h_b / h, 1.0 + 3.0 / b);
  else
    return M (h_b) + K_sat * (h - h_b);
}

double 
Hydraulic_Cosby::Sr (double h) const
{
  if (h < 0.0)
    return pow (1.0 / (1.0 + pow (h / h_b, 5.0)), 1.0 / (b * 5.0));
  else
    return 1;
}

void
Hydraulic_Cosby::initialize (const Texture& texture, double /* rho_b */,
			     bool /* top_soil */, Treelog& msg)
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
  const double clay = mineral * clay_lim * 100 /* [%] */;
  const double silt = mineral * (silt_lim - clay_lim) * 100 /* [%] */;
  const double sand = mineral * (1.0 - silt_lim) * 100 /* [%] */;
  const double cm_per_inch = 2.54;

  b = 3.10 + 0.157 * clay - 0.003 * sand; // []
  daisy_assert (b > 0.0);
  h_b   = -pow (10.0, 1.54 - 0.0095 * sand + 0.0063 * silt); // [cm]
  daisy_assert (h_b < 0.0);
  K_sat = pow (10.0, -0.60 + 0.0126 * sand - 0.0064 * clay)
    * cm_per_inch; // [cm/h]
  daisy_assert (K_sat > 0.0);
  Theta_sat = (50.50 - 0.1420 * sand - 0.0370 * clay) * 0.01; // [%]
  daisy_assert (Theta_sat > 0.0);
  daisy_assert (Theta_sat < 1.0);

  // Debug messages.
  std::ostringstream tmp;
  tmp << "mod_C\n";
  tmp << "(b " << b << " [])\n";
  tmp << "(h_b " << h_b << " [cm])\n";
  tmp << "(K_sat " << K_sat << " [cm/h])\n";
  tmp << "(Theta_sat " << Theta_sat << " [])";
  msg.debug (tmp.str ());
}

Hydraulic_Cosby::Hydraulic_Cosby (Block& al)
  : Hydraulic (al),
    h_b (-42.42e42),
    b (-42.42e42)
{ }

Hydraulic_Cosby::~Hydraulic_Cosby ()
{ }

// Add the Hydraulic_Cosby syntax to the syntax table.
static struct Hydraulic_CosbySyntax
{
  static Hydraulic& make (Block& al)
  { return *new Hydraulic_Cosby (al); }

  Hydraulic_CosbySyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "\
Modified Campbell retention curve model with Burdine theory.\n\
Parameters estimated from soil texture as specified by Cosby et at.");
    Librarian<Hydraulic>::add_type ("Cosby_et_al", alist, syntax, &make);
  }
} hydraulic_Cosby_syntax;
