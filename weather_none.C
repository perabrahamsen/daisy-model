// weather_simple.C

#include "weather.h"
#include "syntax.h"
#include "alist.h"
#include "common.h"

class WeatherNone : public Weather
{
  double air_temperature;
  double global_radiation;
  double reference_evapotranspiration_;
  double rain_;
  double snow_;

  // Simulation.
public:
  void tick (const Time& t)
    { Weather::tick (t); }
  double hourly_air_temperature () const
    { return air_temperature; }
  double daily_air_temperature () const
    { return air_temperature; }
  double hourly_global_radiation () const
    { return global_radiation; }
  double daily_global_radiation () const
    { return global_radiation; }
  double reference_evapotranspiration () const
    { return reference_evapotranspiration_; }
  double rain () const
    { return rain_; }
  double snow () const
    { return snow_; }

  // Communication with external model.
  void put_precipitation (double prec)
    { 
      Weather::distribute (prec / 24.0); 
      rain_ = Weather::rain ();
      snow_ = Weather::snow ();
    }
  void put_air_temperature (double T)
    { air_temperature = T; }
  void put_reference_evapotranspiration (double ref)
    { reference_evapotranspiration_ = ref; }
  void put_global_radiation (double rad)
    { global_radiation = rad; }

  // Create and Destroy.
private:
  friend class WeatherNoneSyntax;
  static Weather& make (const AttributeList&);
  WeatherNone (const AttributeList&);
public:
  ~WeatherNone ();
};

WeatherNone::WeatherNone (const AttributeList& al)
  : Weather (al),
    air_temperature (al.number ("air_temperature")),
    global_radiation (al.number ("global_radiation")),
    reference_evapotranspiration_ (al.number ("reference_evapotranspiration")),
    rain_ (al.number ("rain")),
    snow_ (al.number ("snow"))
{ }

WeatherNone::~WeatherNone ()
{ }

// Add the WeatherNone syntax to the syntax table.
Weather&
WeatherNone::make (const AttributeList& al)
{
  return *new WeatherNone (al);
}

static struct WeatherNoneSyntax
{
  WeatherNoneSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", 
	       "Weather that does not change during the simulation.");
    Weather::load_syntax (syntax, alist);
    syntax.add ("air_temperature", "dg C", Syntax::Const,
		"Constant air temperature");
    alist.add ("air_temperature", 0.0);
    syntax.add ("global_radiation", "W/m^2", Syntax::Const,
		"Constant global radiation.");
    alist.add ("global_radiation", 0.0);
    syntax.add ("reference_evapotranspiration", "mm/h", Syntax::Const,
		"Constant reference evapotranspiration.");
    alist.add ("reference_evapotranspiration", 0.0);
    syntax.add ("rain", "mm/h", Syntax::Const, "Constant rain.");
    alist.add ("rain", 0.0);
    syntax.add ("snow", "mm/h", Syntax::Const, "Constant snow.");
    alist.add ("snow", 0.0);
    Librarian<Weather>::add_type ("none", alist, syntax, &WeatherNone::make);
  }
} WeatherNone_syntax;
