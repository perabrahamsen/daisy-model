// weather_simple.C

#include "weather.h"
#include "syntax.h"
#include "alist.h"
#include "common.h"
#include <algobase.h>

class WeatherSimple : public Weather
{
  const double T1;
  const double T2;
    // Simulation.
public:
  void tick ();
  double AirTemperature () const;
  double GlobalRadiation () const;
  double ReferenceEvapotranspiration () const;
  double Precipitation () const;
  double Rain () const;
  double Snow () const;

    // Create and Destroy.
private:
  friend class WeatherSimpleSyntax;
  static Weather& make (const Time&, const AttributeList&);
  WeatherSimple (const Time&, const AttributeList&);
public:
  ~WeatherSimple ();
};

void
WeatherSimple::tick ()
{ }

double
WeatherSimple::AirTemperature (void) const
{
  double t = 2 * M_PI / 365 * time.yday ();
  return (7.7 - 7.7 * cos (t) - 3.6 * sin (t));
}

double
WeatherSimple::GlobalRadiation () const
{
  /*a function of the weather*/
  static const double A0[] =
  { 17.0, 44.0, 94.0, 159.0, 214.0, 247.0, 214.0, 184.0, 115.0, 58.0, 25.0,
    13.0 };
  static const double A1[] = 
  { -31.0, -74.0, -148.0, -232.0, -291.0, -320.0, -279.0, -261.0, -177.0,
    -96.0, -45.0, -24.0 };
  static const double B1[] =
  { -7.0, -20.0, -34.0, -45.0, -53.0, -63.0, -67.0, -52.0, -30.0, -13.0, 
    -6.0, -4.0 };
  static const double A2[] = 
  { 21.0, 42.0, 68.0, 77.0, 67.0, 0.0, 0.0, 75.0, 73.0, 54.0, 32.0, 18.0 };
  static const double B2[] = 
  { 11.0, 25.0, 32.0, 29.0, 23.0, 0.0, 0.0, 29.0, 25.0, 15.0, 8.0, 7.0 };

  double t = 2 * M_PI / 24 * time.hour ();
  int m = time.month () - 1;
  double Si = (  A0[m] + A1[m] * cos (t) + B1[m] * sin (t)
		 + A2[m] * cos (2 * t) + B2[m] * sin (2 * t));
  return max (0.0, Si);
}

double
WeatherSimple::ReferenceEvapotranspiration () const
{
  const double T = 273.16 + AirTemperature ();
  const double Delta = 5362.7 / pow (T, 2) * exp (26.042 - 5362.7 / T);
  return 0.0245 * Delta / (Delta + 66.7) * GlobalRadiation ();
}

double
WeatherSimple::Precipitation () const
{
  return 0.0;
}

double
WeatherSimple::Rain () const
{
  return Precipitation () - Snow ();
}

double
WeatherSimple::Snow () const
{
  if (AirTemperature () < T1)
    return Precipitation ();
  else if (T2 < AirTemperature ())
    return 0.0;
  else
    return Precipitation () * (T2 - AirTemperature ()) / (T2 - T1);
}

WeatherSimple::WeatherSimple (const Time& t, const AttributeList& al)
  : Weather (t, al.number ("Latitude")),
    T1 (al.number ("T1")),
    T2 (al.number ("T2"))
{ }

WeatherSimple::~WeatherSimple ()
{ }

// Add the WeatherSimple syntax to the syntax table.
Weather&
WeatherSimple::make (const Time& t, const AttributeList& al)
{
  return *new WeatherSimple (t, al);
}

static struct WeatherSimpleSyntax
{
  WeatherSimpleSyntax ();
} WeatherSimple_syntax;

WeatherSimpleSyntax::WeatherSimpleSyntax ()
{ 
  Syntax& syntax = *new Syntax ();
  AttributeList& alist = *new AttributeList ();
  syntax.add ("Latitude", Syntax::Number);
  alist.add ("Latitude", 56.0);
  syntax.add ("T1", Syntax::Number);
  alist.add ("T1", -2.0);
  syntax.add ("T2", Syntax::Number);
  alist.add ("T2", 2.0);
  Weather::add_type ("simple", alist, syntax, &WeatherSimple::make);
}
