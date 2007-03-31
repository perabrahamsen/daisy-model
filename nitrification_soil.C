// nitrification_soil.C
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


#include "nitrification.h"
#include "block.h"
#include "alist.h"
#include "mathlib.h"
#include "plf.h"
#include "check.h"
#include "librarian.h"

class NitrificationSoil : public Nitrification
{
  // Parameters.
private: 
  const double k;
  const double k_10;
  const PLF heat_factor;
  const PLF water_factor;

  // Simulation.
public:
  void tick (const double M, const double C, 
             const double M_left,
             const double h, const double T,
             double& NH4, double& N2O, double& NO3, double dt) const;

  // Create.
public:
  NitrificationSoil (Block&);
};

void 
NitrificationSoil::tick (const double M, const double /* C */, 
                         const double M_left,
                         const double h, const double T,
                         double& NH4, double& N2O, double& NO3,
                         const double dt) const
{
  const double T_factor = (heat_factor.size () < 1)
    ? f_T (T)
    : heat_factor (T);
  const double w_factor = (water_factor.size () < 1)
    ? f_h (h)
    : water_factor (h);

  const double rate = k_10 * w_factor * T_factor * M / (k + M);
  daisy_assert (rate >= 0.0);
  daisy_assert (M_left >= 0.0);
  const double M_new = std::min (rate, M_left / dt - 1e-8);
  if (M_new > 0.0)
    {
      NH4 = M_new;
      N2O = M_new * N2O_fraction;
      NO3 = NH4 - N2O;
    }
  else
    NH4 = N2O = NO3 = 0.0;
}

NitrificationSoil::NitrificationSoil (Block& al)
  : Nitrification (al),
    k (al.number ("k")),
    k_10 (al.number ("k_10")),
    heat_factor (al.plf ("heat_factor")),
    water_factor (al.plf ("water_factor"))
{ }

static struct NitrificationSoilSyntax
{
  static Model& make (Block& al)
  { return *new NitrificationSoil (al); }
  NitrificationSoilSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    Nitrification::load_syntax (syntax, alist);

    alist.add ("description", 
               "k_10 * M / (k + M).  Michaelis-Menten kinetics,\n\
with nitrification based on total ammonium content.");
    syntax.add ("k", "g N/cm^3", Check::positive (), Syntax::Const, 
                "Half saturation constant.");
    syntax.add ("k_10", "g N/cm^3/h", Check::non_negative (), Syntax::Const,
                "Max rate.");
    syntax.add ("heat_factor", "dg C", Syntax::None (), Syntax::Const,
                "Heat factor.");
    alist.add ("heat_factor", PLF::empty ());
    syntax.add ("water_factor", "cm", Syntax::None (), Syntax::Const,
                "Water potential factor.");
    alist.add ("water_factor", PLF::empty ());
    Librarian::add_type (Nitrification::component, "soil", alist, syntax, &make);
  }
} NitrificationSoil_syntax;
