// soil_chemical.C
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


#include "soil_chemical.h"
#include "chemical.h"
#include "soil.h"
#include "soil_heat.h"
#include "soil_water.h"
#include "organic_matter.h"
#include "log.h"
#include "submodel.h"

PLF* SoilChemical::no_lag = NULL;

void 
SoilChemical::uptake (const Soil& soil, 
		      const SoilWater& soil_water)
{
  assert (uptaken.size () == soil.size ());

  const double rate = 1.0 - chemical.crop_uptake_reflection_factor ();
  
  for (unsigned int i = 0; i < soil.size (); i++)
    uptaken[i] = C (i) * soil_water.S_root (i) * rate;
  
  add_to_sink (uptaken);
}

void 
SoilChemical::decompose (const Soil& soil, 
			 const SoilWater& soil_water,
			 const SoilHeat& soil_heat,
			 const OrganicMatter* organic_matter)
{
  assert (decomposed.size () == soil.size ());

  const double decompose_rate = chemical.decompose_rate ();

  unsigned int size = soil.size ();
  if (!chemical.active_groundwater ())
    size = soil_water.first_groundwater_node ();

  // Update lag time.
  bool found = false;
  for (unsigned int i = 0; i < size; i++)
    {
      lag[i] += lag_increment (C_[i]) * dt;
      
      if (lag[i] >= 1.0)
	{
	  lag[i] = 1.0;
	  found = true;
	}
      else if (lag[i] < 0.0)
	{
	  lag[i] = 0.0;
	}
    }

  // No decomposition.
  if (!found)
    size = 0;

  for (unsigned int i = 0; i < size; i++)
    {
      const double heat_factor 
	= chemical.decompose_heat_factor (soil_heat.T (i));
      const double water_factor 
	= chemical.decompose_water_factor (soil_water.h (i));
      const double CO2_factor 
	= organic_matter
	? chemical.decompose_CO2_factor (organic_matter->CO2 (i))
	: 1.0;
      const double conc_factor
	= chemical.decompose_conc_factor (C_[i]);
      const double depth_factor
	= chemical.decompose_depth_factor (soil.z (i));
      const double rate
	= decompose_rate * heat_factor * water_factor * CO2_factor
	* conc_factor * depth_factor;
      decomposed[i] = M_left (i) * rate;
    }
  for (unsigned int i = size; i < soil.size (); i++)
    decomposed[i] = 0.0;

  add_to_sink (decomposed);
}

void
SoilChemical::output (Log& log) const
{
  Solute::output (log);
  log.output ("uptaken", uptaken);
  log.output ("decomposed", decomposed);
}

double
SoilChemical::diffusion_coefficient () const
{ return chemical.diffusion_coefficient (); }

void
SoilChemical::load_syntax (Syntax& syntax, AttributeList& alist)
{
  Solute::load_syntax (syntax, alist);
  alist.add ("submodel", "SoilChemical");
  alist.add ("description", "Chemical solute in soil.");
  syntax.add ("uptaken", "g/cm^3/h", Syntax::LogOnly, Syntax::Sequence,
	      "Amount uptaken by crops in this time step.");
  syntax.add ("decomposed", "g/cm^3/h", Syntax::LogOnly, Syntax::Sequence,
	      "Amount decomposed in this time step.");
  // Use "none" adsorption by default.
  AttributeList none;
  none.add ("type", "none");

  syntax.add ("lag_increment", "g/cm^3", Syntax::Fraction (), Syntax::Const,
	      "Increment lag with the value of this PLF for the current\n\
concentration each timestep.  When lag in any node reaches 1.0,\n\
decomposition begins.  It can never be more than 1.0 or less than 0.0.");
  if (!no_lag)
    {
      no_lag = new PLF ();
      no_lag->add (0.0, 1.0);
      no_lag->add (1.0, 1.0);
    }
  alist.add ("lag_increment", *no_lag);
  syntax.add ("lag", Syntax::None (), Syntax::OptionalState,
	      "This state variable grows with lag_increment (C) each hour.\n\
When it reached 1.0, decomposition begins.");
  alist.add ("adsorption", none);
}

void
SoilChemical::initialize (const AttributeList& al,
		    const Soil& soil, const SoilWater& soil_water)
{
  Solute::initialize (al, soil, soil_water);
  uptaken.insert (uptaken.begin (), soil.size (), 0.0);
  decomposed.insert (decomposed.begin (), soil.size (), 0.0);
  lag.insert (lag.end (), soil.size () - lag.size (), 0.0);
}

SoilChemical::SoilChemical (const Chemical& chem, const AttributeList& al)
  : Solute (al),
    chemical (chem),
    lag_increment (al.plf ("lag_increment"))
{
  if (al.check ("lag"))
    lag = al.number_sequence ("lag");
}

SoilChemical::SoilChemical (const Chemical& chem)
  : Solute (chem.solute_alist ()),
    chemical (chem),
    lag_increment (*no_lag)
{ }

static Submodel::Register soil_chemical_submodel ("SoilChemical",
						  SoilChemical::load_syntax);
