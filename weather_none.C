// weather_simple.C

#include "weather.h"
#include "syntax.h"
#include "alist.h"
#include "common.h"

class WeatherNone : public Weather
{
  const double air_temperature;
  const double global_radiation;
  const double reference_evapotranspiration;
  const double rain;
  const double snow;

  // Simulation.
public:
  void tick ()
  { }
  void output (Log&, const Filter&) const
  { }
  double AirTemperature () const
  { return air_temperature; }
  double GlobalRadiation () const
  { return global_radiation; }
  double ReferenceEvapotranspiration () const
  { return reference_evapotranspiration; }
  double Rain () const
  { return rain; }
  double Snow () const
  { return snow; }

  // Create and Destroy.
private:
  friend class WeatherNoneSyntax;
  static Weather& make (const Time&, const AttributeList&);
  WeatherNone (const Time&, const AttributeList&);
public:
  ~WeatherNone ();
};

WeatherNone::WeatherNone (const Time& t, const AttributeList& al)
  : Weather (t, al.number ("Latitude"), al.name ("type")),
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
WeatherNone::make (const Time& t, const AttributeList& al)
{
  return *new WeatherNone (t, al);
}

static struct WeatherNoneSyntax
{
  WeatherNoneSyntax ();
} WeatherNone_syntax;

WeatherNoneSyntax::WeatherNoneSyntax ()
{ 
  Syntax& syntax = *new Syntax ();
  AttributeList& alist = *new AttributeList ();
  syntax.add ("Latitude", Syntax::Number, Syntax::Const);
  alist.add ("Latitude", 56.0);
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
  Weather::add_type ("none", alist, syntax, &WeatherNone::make);
}
