// pt_std.C -- Use remaining potential evapotranspiration for transpiration

#include "pt.h"
#include "pet.h"
#include "vegetation.h"
#include "surface.h"
#include "log.h"

struct PT_standard : public PT
{
  // State.
  double potential_transpiration_;

  // Log variable.
  double potential_crop_transpiration;
  double potential_soil_transpiration;

  // Simulation.
  void tick (const Weather&, const Vegetation& crops,
	     const Surface& surface, const Soil&, const SoilHeat&,
	     const SoilWater&, const Pet& pet, 
	     double canopy_ea, double snow_ea,
	     double pond_ea, double soil_ea)
    {
      const double divide_ep = pet.wet () - snow_ea;
      const double canopy_ep = divide_ep * crops.cover ();
      const double pond_ep = divide_ep - canopy_ep;
      
      potential_crop_transpiration = canopy_ep - canopy_ea;
      potential_soil_transpiration 
	= (pond_ep - pond_ea - soil_ea) * surface.EpInterchange ();
      potential_transpiration_ = min (max (0.0, 
					   potential_crop_transpiration
					   + potential_soil_transpiration),
				      pet.dry ());
    }

  double potential_transpiration () const
    { return potential_transpiration_; }

  void output (Log& log, Filter& filter) const
    {
      PT::output (log, filter);
      log.output ("potential_crop_transpiration", 
		  filter, potential_crop_transpiration, true);
      log.output ("potential_soil_transpiration", 
		  filter, potential_soil_transpiration, true);
    }

  // Create.
  PT_standard (const AttributeList& al)
    : PT (al),
      potential_transpiration_ (-42.42e42),
      potential_crop_transpiration (-42.42e42),
      potential_soil_transpiration (-42.42e42)
    { }
};

static struct PT_StandardSyntax
{
  static PT&
  make (const AttributeList& al)
    { return *new PT_standard (al); }
  PT_StandardSyntax ()
    {
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      PT::load_syntax (syntax, alist);
      syntax.add ("potential_crop_transpiration", "mm/h", Syntax::LogOnly,
		  "Potential canopy evapotranspiration not satified by \
intercepted water");
      syntax.add ("potential_soil_transpiration", "mm/h", Syntax::LogOnly,
		  "Unsatisfied potential soil evaporation transmogriffed \
into transpiration");
      Librarian<PT>::add_type ("default", alist, syntax, &make);
    }
} PT_standard_syntax;
