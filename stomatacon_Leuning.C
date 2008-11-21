// stomatacon_Leuning.C -- Stomata conductance calculated by Ball & Berry model
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
#include "treelog.h"

struct StomataCon_Leuning : public StomataCon
{
  // Parameters.
  const double Do;
private:
  // Simulation.
  double stomata_con (const double wsf, const double m, const double hs, 
                      const double pz, const double Ptot, const double cs,
                      const double Gamma, const double intercept,
                      const double CO2_atm, const double Ds, Treelog&);

  void output (Log&) const
  { }

  // Create.
  public:
  StomataCon_Leuning (Block& al)
    : StomataCon (al),
      Do (al.number ("Do"))
  { }
};

double
StomataCon_Leuning::stomata_con (double wsf /*[]*/, const double m /*[]*/,
                            const double , const double pz /*[mol/m²leaf/s]*/,
                            const double Ptot /*[Pa]*/, const double cs /*[Pa]*/,
                            const double Gamma /*[Pa]*/, 
                            const double intercept /*[mol/m²leaf/s]*/, const double,
                            const double Ds /*[Pa]*/, Treelog& msg)
{
  daisy_assert (cs > Gamma);
  daisy_assert (cs > 0.0);
  daisy_assert (Gamma >= 0.0);
  daisy_assert (pz > 0.0);
  daisy_assert (Ds > 0.0);
  daisy_assert (Do > 0.0);
 
  if (!(wsf > 0.0))
    {
      std::ostringstream tmp;
      tmp << "wsf = " << wsf << ", should be positive";
      msg.warning (tmp.str ());
      wsf = 1.0;
    }
  daisy_assert (((cs - Gamma)*(1 + Ds/Do)) > 0.0);
  const double gsw = (wsf * m * pz * Ptot)/((cs - Gamma)*(1 + Ds/Do)) + intercept;
  daisy_assert (gsw >= 0.0);

  return gsw;
}

static struct StomataConLeuningSyntax
{
  static Model& make (Block& al)
  { return *new StomataCon_Leuning (al); }
  static void load_syntax (Syntax& syntax, AttributeList& alist)
  {
    syntax.add ("Do", "[Pa]", Check::non_negative (), Value::Const,
                "Coefficient, value after Leuning (1995)");
    alist.add ("Do", 1500.);

  }  
  StomataConLeuningSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", 
	       "Stomata conductance calculated by the Ball & Berry model.");

    load_syntax (syntax, alist);

    Librarian::add_type (StomataCon::component, "Leuning", alist, syntax, &make);
  }
} StomataConLeuningsyntax;


const AttributeList& 
StomataCon::default_model ()
{
  static AttributeList alist;
  
  if (!alist.check ("type"))
    {
      Syntax syntax;
      StomataConLeuningSyntax::load_syntax (syntax, alist);
      alist.add ("type", "Leuning");
    }
  return alist;
}
