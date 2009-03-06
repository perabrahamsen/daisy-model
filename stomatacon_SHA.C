// stomatacon_SHA.C -- Stomata conductance calculated by Seyed Hamid Ahmadi model
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
#include "block.h"
#include "librarian.h"
#include "frame.h"

struct StomataCon_SHA : public StomataCon
{
  // Parameters.
private:
  const double lambda, alpha;  //Coefficients []
  const double Amax; //Max photosynthesis [mol/m²leaf/s]
  const double M;    //Parameter [mol/m²leaf/s]

  
  // Simulation.
  double stomata_con (const double wsf, const double m, const double hs, 
                      const double pz, const double Ptot, const double cs,
                      const double Gamma, const double intercept,
                      const double CO2_atm, const double Ds, Treelog&);
  void output (Log&) const
  { }

  // Create.
  public:
  StomataCon_SHA (Block& al)
    : StomataCon (al),
      lambda (al.number ("lambda")),
      alpha (al.number ("alpha")),
      Amax (al.number ("Amax")),
      M (al.number ("M"))
  { }
};

double
StomataCon_SHA::stomata_con (const double wsf /*[]*/, 
                             const double,
                             const double hs /*[]*/, 
                             const double pz /*[mol/m²leaf/s]*/,
                             const double Ptot /*[Pa]*/, const double cs /*[Pa]*/,
                             const double, const double intercept /*[mol/m²leaf/s]*/,
                             const double CO2_atm, const double, Treelog&)
{
  const double gsw = wsf * (M * pow(hs, alpha) * pow(pz, lambda))/(cs) 
                     + intercept;
  
  daisy_assert (gsw >= 0.0);
  return gsw;
}

static struct StomataConSHASyntax : public DeclareModel
{
  Model* make (Block& al) const
  { return new StomataCon_SHA (al); }
  StomataConSHASyntax ()
    : DeclareModel (StomataCon::component, "SHA", 
	       "Stomata conductance calculated by the model given by Seyed Hamid Ahmadi.")
  { }
  void load_frame (Frame& frame) const
  {

    frame.add ("lambda", Value::None (), Check::non_negative (), Value::Const,
                "Coefficient");
    frame.add ("lambda", 1.0);
    frame.add ("alpha", Value::None (), Check::non_negative (), Value::Const,
                "Coefficient");
    frame.add ("alpha", 1.0);
    frame.add ("M", "[mol/m²leaf/s]", Check::non_negative (), Value::Const,
	       "Parameter ??");
    frame.add ("M", 1.0);

  }
} StomataConSHAsyntax;


