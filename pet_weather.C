// pet_weather.C -- Potential evopotranspiration using weather data.

#include "pet.h"
#include "weather.h"

struct PetWeather : public Pet
{
  // State.
  double reference_evapotranspiration;
  double potential_evapotranspiration;

  // Simulation.
  void tick (const Weather& weather, const Vegetation& crops,
	     const Surface& surface, const Soil&, const SoilHeat&,
	     const SoilWater&)
    {
      reference_evapotranspiration = weather.reference_evapotranspiration ();
      potential_evapotranspiration 
	= reference_to_potential (crops, surface, 
				  reference_evapotranspiration);
    }

  double wet () const
    { return potential_evapotranspiration; }

  // Create.
  PetWeather (const AttributeList& al)
    : Pet (al)
    { }
};

static struct PetWeatherSyntax
{
  static Pet&
  make (const AttributeList& al)
    { return *new PetWeather (al); }
  PetWeatherSyntax ()
    {
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      alist.add ("description", 
		 "Potential evopotranspiration using weather data.");
      Pet::load_syntax (syntax, alist);
      Librarian<Pet>::add_type ("weather", alist, syntax, &make);
    }
} PetWeather_syntax;
