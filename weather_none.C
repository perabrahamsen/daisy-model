// weather_simple.C

#include "weather.h"
#include "syntax.h"
#include "alist.h"
#include "common.h"

class WeatherNone : public Weather
{
  double air_temperature;
  const double global_radiation;
  double reference_evapotranspiration;
  double rain;
  double snow;

  // Simulation.
public:
  void tick (const Time&)
    { }
  double AirTemperature () const
    { return air_temperature; }
  double GlobalRadiation () const
    { return global_radiation; }
  double DailyRadiation () const
    { return global_radiation * 24.0; }
  double ReferenceEvapotranspiration () const
    { return reference_evapotranspiration; }
  double Rain () const
    { return rain; }
  double Snow () const
    { return snow; }

  // Communication with external model.
  void put_precipitation (double prec)
    { 
      Weather::distribute (prec / 24.0); 
      rain = Weather::Rain ();
      snow = Weather::Snow ();
    }
  void put_air_temperature (double T)
    { air_temperature = T; }
  void put_reference_evapotranspiration (double ref)
    { reference_evapotranspiration = ref; }

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
    reference_evapotranspiration (al.number ("reference_evapotranspiration")),
    rain (al.number ("rain")),
    snow (al.number ("snow"))
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
    Weather::load_syntax (syntax, alist);
    syntax.add ("air_temperature", Syntax::Number, Syntax::Const);
    alist.add ("air_temperature", 0.0);
    syntax.add ("global_radiation", Syntax::Number, Syntax::Const);
    alist.add ("global_radiation", 0.0);
    syntax.add ("reference_evapotranspiration", Syntax::Number, Syntax::Const);
    alist.add ("reference_evapotranspiration", 0.0);
    syntax.add ("rain", Syntax::Number, Syntax::Const);
    alist.add ("rain", 0.0);
    syntax.add ("snow", Syntax::Number, Syntax::Const);
    alist.add ("snow", 0.0);
    Librarian<Weather>::add_type ("none", alist, syntax, &WeatherNone::make);
  }
} WeatherNone_syntax;
