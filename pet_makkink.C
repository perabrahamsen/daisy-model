// pet_makkink.C -- Potential evopotranspiration using Makkink's Equation.

#include "pet.h"
#include "weather.h"

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
      // Use Makkink's equation for calculating reference_evapotranspiration.
      const double T = 273.16 + weather.daily_air_temperature ();
      const double Delta = 5362.7 / pow (T, 2.0) * exp (26.042 - 5362.7 / T);
      reference_evapotranspiration 
	= 1.05e-3 
	* Delta / (Delta + 66.7) * weather.hourly_global_radiation ();

      potential_evapotranspiration 
	= reference_to_potential (crops, surface, 
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
      Pet::load_syntax (syntax, alist);
      Librarian<Pet>::add_type ("makkink", alist, syntax, &make);
    }
} PetMakkink_syntax;
