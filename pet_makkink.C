// pet_makkink.C -- Potential evopotranspiration using Makkink's Equation.

#include "pet.h"
#include "weather.h"
#include "log.h"

struct PetMakkink : public Pet
{
  // State.
  double reference_evapotranspiration;
  double potential_evapotranspiration;

  // Simulation.
  void tick (const Weather& weather, const Vegetation& crops,
	     const Surface& surface, const Soil&, const SoilHeat&,
	     const SoilWater&)
    {
      reference_evapotranspiration 
	= Weather::Makkink (weather.hourly_air_temperature (),
			    weather.hourly_global_radiation ());
      potential_evapotranspiration 
	= reference_to_potential (crops, surface, 
				  reference_evapotranspiration);
    }

  void output (Log& log) const
    {
      Pet::output (log);
      log.output ("reference_evapotranspiration", 
		  reference_evapotranspiration);
    }

  double wet () const
    { return potential_evapotranspiration; }

  // Create.
  PetMakkink (const AttributeList& al)
    : Pet (al)
    { }
};

static struct PetMakkinkSyntax
{
  static Pet&
  make (const AttributeList& al)
    { return *new PetMakkink (al); }
  PetMakkinkSyntax ()
    {
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      alist.add ("description", 
		 "Potential evopotranspiration using Makkink's Equation.");
      Pet::load_syntax (syntax, alist);
      Librarian<Pet>::add_type ("makkink", alist, syntax, &make);
    }
} PetMakkink_syntax;
