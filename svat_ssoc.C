// svat_ssoc.C -- SVAT model Sun Shade Open Canopy.
// 
// Copyright 2008 Per Abrahamsen, Birgitte Gjettermann and Søren Hansen
// Copyright 2008 KU.
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

#include "svat.h"
#include "syntax.h"
#include "alist.h"
#include "block.h"
#include "librarian.h"
#include "resistance.h"
#include "net_radiation.h"
#include "assertion.h"
#include "treelog.h"
#include "mathlib.h"
#include "librarian.h"

struct SVAT_SSOC : public SVAT
{
  // Parameters.
  static const double epsilon; 
  static const double sigma; // Stefan Boltzman constant [W m^-2 K^-4]
  const double c_p;   // specific heat of air [J kg^-1 K^-1]

  std::auto_ptr<NetRadiation> net_radiation;

  // Driving variables.
  // - Upper boundary
  // - Lower boundary 
  // - Canopy
  // Intermediate variables.

  // "Unknowns."
  // - Temperatures * 4
  // - Resistances * 4
  double G_R;
  double G_H_canopy;
  // - ?
  
  // Fluxes.
  // - Heat
  // - H2O
  // - CO2

  // Simulation.
  void tick (const Weather&, const Vegetation&,
	     const Surface&, const Soil&, const SoilHeat&,
	     const SoilWater&, const Pet&,
	     double /* canopy_ea */, double /* snow_ea */,
	     double /* pond_ea */, double /* soil_ea */,
             double /* crop_ea */, double /* crop_ep */); 

  void calculate_resistances(const double T_a); 

  // find T
  // løs - find r og T (rs_sun, rs_sha)
  // find flux


  double production_stress () const
  { return -1; }

  static void load_syntax (Syntax& syntax, AttributeList& alist);

  // Create.
public:
  SVAT_SSOC (Block& al)
    : SVAT (al), 
      c_p (al.number ("c_p")),
      net_radiation (Librarian::build_item<NetRadiation> (al, "net_radiation"))
  { }
 ~SVAT_SSOC ()
  { }

};

const double SVAT_SSOC::epsilon = 0.98;
const double SVAT_SSOC::sigma = 5.67e-8; //Stefan Boltzman constant [W/m^2/K^4]

// Simulation.
void
SVAT_SSOC::tick (const Weather&, const Vegetation&,
	     const Surface&, const Soil&, const SoilHeat&,
	     const SoilWater&, const Pet&,
	     double /* canopy_ea */, double /* snow_ea */,
	     double /* pond_ea */, double /* soil_ea */,
             double /* crop_ea */, double /* crop_ep */)
{
  // indlæse driving variabler
}

void 
SVAT_SSOC:: calculate_resistances(const double T_a)
{
  // air density 
  const double rho_a = Resistance:: rho_a(T_a); // [kg m^-3]
  
  // Radiation
  G_R = 4. * epsilon * sigma * pow(T_a, 3.);
  
  
} 

// find T
// løs - find r og T (rs_sun, rs_sha)
// find flux


void 
SVAT_SSOC::load_syntax (Syntax& syntax, AttributeList& alist)
{

  syntax.add ("c_p", "J/kg/K^1", Syntax::Const,
              "Specific heat of air.");
  alist.add ("c_p", 1010.);

  syntax.add_object ("net_radiation", NetRadiation::component,
                     "Net radiation.");
  alist.add ("net_radiation", NetRadiation::default_model ());
  
}

static struct SVAT_SSOCSyntax
{
  static Model& make (Block& al)
  { return *new SVAT_SSOC (al); }
  SVAT_SSOCSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    SVAT::load_syntax (syntax, alist);
    Librarian::add_type (SVAT::component, "SSOC", alist, syntax, &make);
  }
} SVAT_ssoc_syntax;
