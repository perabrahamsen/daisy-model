// soil_chemical.C

#include "soil_chemical.h"
#include "chemical.h"
#include "soil.h"
#include "soil_heat.h"
#include "soil_water.h"
#include "organic_matter.h"

void 
SoilChemical::decompose (const Soil& soil, 
			 const SoilWater& soil_water,
			 const SoilHeat& soil_heat,
			 const OrganicMatter& organic_matter)
{
  decomposed.erase (decomposed.begin (), decomposed.end ());

  const double decompose_rate = chemical.decompose_rate ();
  
  for (unsigned int i = 0; i < soil.size (); i++)
    {
      const double heat_factor 
	= chemical.decompose_heat_factor (soil_heat.T (i));
      const double water_factor 
	= chemical.decompose_water_factor (soil_water.h (i));
      const double CO2_factor 
	= chemical.decompose_CO2_factor (organic_matter.CO2 (i));
      const double rate
	= decompose_rate * heat_factor * water_factor * CO2_factor;
      const double M = M_left (i) * rate;
      decomposed.push_back (M);
    }
  add_to_sink (decomposed);
}

double 
SoilChemical::diffusion_coefficient () const
{ return chemical.diffusion_coefficient (); }

void
SoilChemical::load_syntax (Syntax& syntax, AttributeList& alist)
{
  Solute::load_syntax (syntax, alist); 
  // Use "none" adsorption by default.
  AttributeList& none = *new AttributeList ();
  none.add ("type", "none");
  alist.add ("adsorption", none);
}

SoilChemical::SoilChemical (const Chemical& chem, const AttributeList& al)
  : Solute (al),
    chemical (chem)
{ }

SoilChemical::SoilChemical (const Chemical& chem)
  : Solute (chem.solute_alist ()),
    chemical (chem)
{ }
