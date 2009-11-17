// stomatacon_exp.C -- Stomata conductance calculated by Ball & Berry model
// 
// Copyright 2008 Birgitte Gjettermann and KU
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
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.See the
// GNU Lesser Public License for more details.
// 
// You should have received a copy of the GNU Lesser Public License
// along with Daisy; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

#define BUILD_DLL

#include "stomatacon.h"
#include "mathlib.h"
#include <sstream>
#include "check.h"
#include "block_model.h"
#include "librarian.h"
#include "frame.h"

struct StomataCon_BB : public StomataCon
{
  // Parameters.
  const double m;     // Stomatal slope factor.
  const double b;     // Stomatal intercept.
  
  // Simulation.
  double minimum () const
  { return b; }
  double stomata_con (const double wsf, const double hs, 
                      const double pz, const double Ptot, const double cs,
                      const double Gamma, 
                      const double,  Treelog&);

  void output (Log&) const
  { }

  // Create.
  StomataCon_BB (const BlockModel& al)
    : StomataCon (al),
      m (al.number("m")),
      b (al.number("b"))
  { }
};

double
StomataCon_BB::stomata_con (const double wsf /*[]*/, 
                            const double hs /*[]*/, 
                            const double pz /*[mol/m²leaf/s]*/,
                            const double Ptot /*[Pa]*/,
                            const double cs /*[Pa]*/,
                            const double, 
                            const double, Treelog&)
{
  if (pz <= 0.0)
    return b;
  const double gsw = wsf * (m * hs * pz * Ptot)/cs + b;

  daisy_assert (gsw >= 0.0);
  return gsw;
}

static struct StomataConBBSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new StomataCon_BB (al); }
  StomataConBBSyntax ()
    : DeclareModel (StomataCon::component, "BB", 
	       "Stomata conductance calculated by the Ball & Berry model.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare ("m", Attribute::None (), Check::positive (),
                   Attribute::Const, "\
Stomatal slope factor.\n\
Ball and Berry (1982): m = 9 for soyabean.\n\
Wang and Leuning(1998): m = 11 for wheat");
    frame.declare ("b", "mol/m^2/s", Check::positive (), Attribute::Const, "\
Stomatal intercept.\n\
Ball and Berry (1982) & Wang and Leuning(1998): (0.01 mol/m2/s)");
  }
} StomataConBBsyntax;

// stomatacon_BB.C ends here.
