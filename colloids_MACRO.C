// colloids_MACRO.C -- Colloids calculated by the MACRO model 
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

#include "colloids.h"
#include "mathlib.h"
#include <sstream>
#include "check.h"
#include "block.h"
#include "librarian.h"

struct Colloids_MACRO : public Colloids
{
  // Parameters.
  const double Mmax;
  const double kd;
  const double kr;
  const double alpha;
  double Ms;
  double As;

private:
  // Simulation.
  void initialize (const double Rain_intensity, Treelog&);
  double colloid_generation (const double dt, const double zi, 
                             const double bulk_density, 
                             const double Rain_intensity /*[mm/h]*/, Treelog& msg);
  double colloid_filtration (const double Rain_intensity, Treelog& msg);

  void output (Log&) const { }

  const AttributeList& default_model ();

  // Create.
  public:
  Colloids_MACRO (Block& al)
    : Colloids (al),
      Mmax (al.number ("Mmax")),
      kd (al.number ("kd")),
      kr (al.number ("kr")),
      alpha (al.number ("alpha")),
      Ms (al.number ("Ms")),
      As (al.number ("As"))
  {}
};
void
Colloids_MACRO::initialize (const double Rain_intensity/*[]*/, Treelog& msg)
{
  //Readily available dispersible particles
  //Initially 10% of Mmax is readily avilable
  Ms = Mmax / 100 * alpha; //[g colloids g^-1 soil]

  //Kinetic energy of rain
  const double E = 29.0 * (1 - 0.72 * exp(-0.05 * Rain_intensity));  //[J m^-2 mm^-1]

  //Detachment of colloids at the surface
  const double D = kd * E * Rain_intensity * Ms; //[g colloids m^-2 h^-1]
  
  //rates of water infiltrating in macropores and micropores
  const double i_ma = 1.0; // [m h^-1]
  const double i_mi = 1.0; // [m h^-1]

  //Initial boundary at the surface of concentration of particles in soil water 
  //(in both macropores and micropores)
  const double cs = D / (i_ma + i_mi); //[g colloids m^-3]


}

double
Colloids_MACRO::colloid_generation (const double dt, const double zi, 
                                    const double bulk_density, 
                                    const double Rain_intensity /*[mm/h]*/, 
                                    Treelog& msg)
{
  //Amount of available dispersible particles at the soil surface
  As = Ms * bulk_density /*[g soil m^-3]*/ * zi /*[m]*/; //[g colloids m^-2]

  //Kinetic energy of rain
  const double E                // [J m^-2 mm^-1]
    = 29.0 * (1 - 0.72 * exp(-0.05 * Rain_intensity)); 

  //Detachment of colloids at the surface
  const double D = kd * E * Rain_intensity * Ms; //[g m^-2 h^-1]

  //Replenishment of colloids in the surface layer
  const double P = kr * (1-Ms/Mmax); //[g m^-2 h^-1]

  //Mass balance
  const double dAs = (-D + P) * dt/*[h]*/;  //[g m^-2]
  As = As + dAs;

  return dAs;//[g m^-2]
}

double
Colloids_MACRO::colloid_filtration (const double /*[]*/, Treelog& msg)
{
  //


  return 1.0;
}

static struct ColloidsMACROSyntax
{
  static Model& make (Block& al)
  { return *new Colloids_MACRO (al); }
  static void load_syntax (Syntax& syntax, AttributeList& alist)
  {
    syntax.add ("Mmax", "g/g", Check::non_negative (), Syntax::Const,
                "Maximum amount of detachable particles");
    alist.add ("Mmax", 0.165);
    syntax.add ("kd", "g/J", Check::non_negative (), Syntax::Const,
                "Detachment rate coefficient");
    alist.add ("kd", 15.0);
    syntax.add ("kr", "g/m^2/h", Check::non_negative (), Syntax::Const,
                "Replenishment rate coefficient");
    alist.add ("kr", 0.1);
    syntax.add ("alpha", "%", Check::non_negative (), Syntax::Const,
                "Percent of Mmax initially readily available, Ms");
    alist.add ("alpha", 10.);

  }  
  ColloidsMACROSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", 
	       "Colloids in flowing soil water calculated by the MACRO model.");

    load_syntax (syntax, alist);

    Librarian::add_type (Colloids::component, "MACRO", alist, syntax, &make);
  }
} ColloidsMACROsyntax;


const AttributeList& 
Colloids::default_model ()
{
  static AttributeList alist;
  
  if (!alist.check ("type"))
    {
      Syntax syntax;
      ColloidsMACROSyntax::load_syntax (syntax, alist);
      alist.add ("type", "MACRO");
    }
  return alist;
}
