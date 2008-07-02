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
  const AttributeList& default_model ();

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
  const double gsw = wsf * (M * pow(hs, alpha) * pow(pz/Amax, lambda))/(cs/CO2_atm) 
                     + intercept;
  
  daisy_assert (gsw >= 0.0);
  return gsw;
}

static struct StomataConSHASyntax
{
  static Model& make (Block& al)
  { return *new StomataCon_SHA (al); }
  static void load_syntax (Syntax& syntax, AttributeList& alist)
  {
    syntax.add ("lambda", Syntax::None (), Check::non_negative (), Syntax::Const,
                "Coefficient");
    alist.add ("lambda", 1.0);

    syntax.add ("alpha", Syntax::None (), Check::non_negative (), Syntax::Const,
                "Coefficient");
    alist.add ("alpha", 1.0);

    syntax.add ("Amax", "[mol/m²leaf/s]", Check::non_negative (), Syntax::Const,
                "Max photosynthesis");
    alist.add ("Amax", 1.0);

    syntax.add ("M", "[mol/m²leaf/s]", Check::non_negative (), Syntax::Const,
                "Parameter ??");
    alist.add ("M", 1.0);
  }  
  StomataConSHASyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", 
	       "Stomata conductance calculated by the model given by Seyed Hamid Ahmadi.");

    load_syntax (syntax, alist);

    Librarian::add_type (StomataCon::component, "SHA", alist, syntax, &make);
  }
} StomataConSHAsyntax;


