// weather.C

#include "weather.h"
#include "log.h"
#include "time.h"
#include "mathlib.h"
#include "net_radiation.h"

Librarian<Weather>::Content* Librarian<Weather>::content = NULL;

const char *const Weather::description = "\
Meteorological data, as well as the global positioning, are the\n\
responsibility of the `weather' component, typically be reading the\n\
data from a file.  The meteorological data are common to all columns.";

struct Weather::Implementation
{
  // Parameters.
  const double Latitude;
  const double Elevation;
  const IM DryDeposit;
  const IM WetDeposit;
  const double T1;
  const double T2;
  const double average;		// Average temperature at bottom [C]
  const double amplitude;	// Variation in bottom temperature [C]
  const double omega;		// Length of year [ rad / yday]
  const double max_Ta_yday;	// Warmest day in the year [yday]

  // State.
  double daily_extraterrastial_radiation; // [MJ/m2/d]
  double daily_global_radiation; // [W/m²]
  double Prain;
  double Psnow;
  double day_length;
  double day_cycle;
  Time time;

  // Astronomic utilities.
  static double SolarDeclination (const Time& time); // [rad]
  static double RelativeSunEarthDistance (const Time& time);
  static double SunsetHourAngle (double Dec, double Lat); // [rad]
  double ExtraterrestrialRadiation (const Time& time); // [MJ/m2/d]

  // Create and Destroy.
  Implementation (const AttributeList& al)
    : Latitude (al.number ("Latitude")),
      Elevation (al.number ("Elevation")),
      DryDeposit (al.alist ("DryDeposit")),
      WetDeposit (al.alist ("WetDeposit")),
      T1 (al.number ("T1")),
      T2 (al.number ("T2")),
      average (al.number ("average")),
      amplitude (al.number ("amplitude")),
      omega (al.number ("omega")),
      max_Ta_yday (al.number ("max_Ta_yday")),
      daily_global_radiation (-42.42e42),
      Prain (0.0),
      Psnow (0.0),
      day_length (-42.42e42),
      day_cycle (42.42e42),
      time (1, 1, 1, 1)
    { }
};

double 
Weather::Implementation::SolarDeclination (const Time& time) // [rad]
{
  return (0.409 * sin (2.0 * M_PI * time.yday () / 365.0 - 1.39));
}


double 
Weather::Implementation::RelativeSunEarthDistance (const Time& time)
{
  return (1.0 + 0.033 * cos (2.0 * M_PI * time.yday () / 365.0));
}


double 
Weather::Implementation::SunsetHourAngle (double Dec, double Lat) // [rad]
{
  return (acos (-tan (Dec) * tan (Lat)));
}


double 
Weather::Implementation::ExtraterrestrialRadiation (const Time& time)
  // [MJ/m2/d]
{
  const double Dec = SolarDeclination (time);
  const double Lat = M_PI / 180 * Latitude;
  const double x1 = SunsetHourAngle (Dec, Lat) * sin (Lat) * sin (Dec);
  const double x2 = cos (Lat) * cos (Dec) * sin (SunsetHourAngle (Dec, Lat));
  return (37.6 * RelativeSunEarthDistance (time) * (x1 + x2));
}

void 
Weather::tick (const Time& time)
{
  impl.day_length = day_length (time);
  assert (impl.day_length >= 0.0);
  assert (impl.day_length <= 24.0);

  impl.day_cycle = max (0.0, M_PI_2 / day_length ()
			* cos (M_PI * (time.hour () - 12) / day_length ()));
  assert (impl.day_cycle >= 0.0);
  assert (impl.day_cycle <= 1.0);

  impl.daily_extraterrastial_radiation 
    = impl.ExtraterrestrialRadiation (time);
  assert (impl.daily_extraterrastial_radiation >= 0.0);

  impl.time = time;
}

double
Weather::hourly_global_radiation () const
{ return (day_cycle () * 24.0) * daily_global_radiation (); }

double
Weather::daily_global_radiation () const
{ return impl.daily_global_radiation; }

double 
Weather::daily_extraterrastial_radiation () const
{ return impl.daily_extraterrastial_radiation; }

double
Weather::rain () const
{
  return impl.Prain;
}

double
Weather::snow () const
{
  return impl.Psnow;
}

void
Weather::output (Log& log) const
{ 
  log.output ("hourly_air_temperature", hourly_air_temperature ());
  log.output ("daily_air_temperature", daily_air_temperature ());
  log.output ("hourly_global_radiation", 
	      hourly_global_radiation ());
  log.output ("daily_global_radiation", 
	      daily_global_radiation ());
  log.output ("daily_extraterrastial_radiation", 
	      daily_extraterrastial_radiation ());
  log.output ("reference_evapotranspiration", 
	      reference_evapotranspiration ());
  log.output ("rain", rain ());
  log.output ("snow", snow ());
  log.output ("cloudiness", cloudiness ());
  log.output ("vapor_pressure", vapor_pressure ());
  log.output ("wind", wind ()); 
  log.output ("day_length", day_length ());
  log.output ("day_cycle", day_cycle ());
}

void 
Weather::distribute (double precipitation)
{
  const double T = hourly_air_temperature ();
  if (T < impl.T1)
    impl.Psnow = precipitation;
  else if (impl.T2 < T)
    impl.Psnow = 0.0;
  else
    impl.Psnow = precipitation * (impl.T2 - T) / (impl.T2 - impl.T1);

  impl.Prain = precipitation - snow ();
}

double
Weather::hourly_air_temperature () const
{
  // BUG: Should add some kind of day cycle.  
  return daily_air_temperature (); 
}

double
Weather::reference_evapotranspiration () const
{
  const double T = 273.16 + daily_air_temperature ();
  const double Delta = 5362.7 / pow (T, 2.0) * exp (26.042 - 5362.7 / T);
  return 1.05e-3 * Delta / (Delta + 66.7) * hourly_global_radiation ();
}

double
Weather::day_length () const
{
  return impl.day_length;
}

double
Weather::day_length (const Time& time) const
{
  double t = 2 * M_PI / 365 * time.yday ();
  
  double Dec = (0.3964 - 22.97 * cos (t) + 3.631 * sin (t)
		- 0.03885 * cos (2 * t) 
		+ 0.03838 * sin (2 * t) - 0.15870 * cos (3 * t) 
		+ 0.07659 * sin (3 * t) - 0.01021 * cos (4 * t));
  t = (24 / M_PI
       * acos (-tan (M_PI / 180 * Dec) * tan (M_PI / 180 * impl.Latitude)));
  return (t < 0) ? t + 24.0 : t;
}

double
Weather::day_cycle () const
{
  return impl.day_cycle;
}

IM
Weather::deposit () const
{
  const double Precipitation = rain () + snow (); // [mm]
  assert (Precipitation >= 0.0);
  
  // [kg N/ha/year -> [g/cm²/h]
  const double hours_to_years = 365.2425 * 24.0;
  const double kg_per_ha_to_g_cm2 
    = 1000.0 / ((100.0 * 100.0) * (100.0 * 100.0));
  const IM dry (impl.DryDeposit, kg_per_ha_to_g_cm2 / hours_to_years);
  const IM wet (impl.WetDeposit, 1.0e-7); // [ppm] -> [g/cm²/mm]

  const IM result = dry + wet * Precipitation;
  assert (result.NO3 >= 0.0);
  assert (result.NH4 >= 0.0);

  assert (approximate (result.NO3, 
		       impl.DryDeposit.NO3 * kg_per_ha_to_g_cm2/hours_to_years
		       + Precipitation * impl.WetDeposit.NO3 * 1e-7));
  assert (approximate (result.NH4, 
		       impl.DryDeposit.NH4 * kg_per_ha_to_g_cm2/hours_to_years
		       + Precipitation * impl.WetDeposit.NH4 * 1e-7));
  return result;
}

double 
Weather::cloudiness () const
{ 
  const double Si = daily_global_radiation () 
    * (60.0 * 60.0 * 24.0) / 1e6; // W/m^2 -> MJ/m^2/d
  const double reference_cloudiness 
    = CloudinessFactor_Humid (impl.time, Si);
  assert (reference_cloudiness >= 0.0);
  assert (reference_cloudiness <= 1.0);
  return reference_cloudiness;
}
 
double 
Weather::vapor_pressure () const
{ 
  const double T_min = daily_air_temperature () - 5.0;
  return SaturationVapourPressure (T_min);
}

double 
Weather::wind () const
{ return 0.0; }

void 
Weather::put_precipitation (double prec)
{ distribute (prec / 24.0); }

void 
Weather::put_air_temperature (double)
{ assert (false); }

void 
Weather::put_reference_evapotranspiration (double)
{ assert (false); }

void
Weather::put_global_radiation (double radiation) // [W/m²]
{ impl.daily_global_radiation = radiation; }

double
Weather::average () const
{ return impl.average; }

double
Weather::amplitude () const
{ return impl.amplitude; }

double
Weather::omega () const
{ return impl.omega; }

double
Weather::max_Ta_yday () const
{ return impl.max_Ta_yday; }

double
Weather::LatentHeatVaporization (double Temp) // [MJ/kg]
{ return (2.501 - 2.361e-3 * Temp); }

double 
Weather::PsychrometricConstant (double AtmPressure, double Temp) // [kPa/K]
{ return (0.00163 * AtmPressure / LatentHeatVaporization (Temp)); }

double 
Weather::AtmosphericPressure ()	const // [kPa]
{ return (101.3 * pow ((293 - 0.0065 * impl.Elevation) / 293, 5.26)); }

double 
Weather::AirDensity (double AtmPressure, double Temp) // [kg/m3]
{
  const double Tvirtuel = 1.01 * (Temp + 273);
  return (3.486 * AtmPressure / Tvirtuel);
}

double 
Weather::SaturationVapourPressure (double Temp) // [kPa]
{ return (0.611 * exp (17.27 * Temp / (Temp + 237.3))); }

double 
Weather::SlopeVapourPressureCurve (double Temp) // [kPa/K]
{ return (4098 * SaturationVapourPressure (Temp) / pow (Temp + 237.3, 2)); }

double 
Weather::CloudinessFactor_Arid (const Time& time, double Si) const
{
  const double a = 1.35;
  const double x = Si / 0.75 / impl.ExtraterrestrialRadiation (time);
  return (a * min (1.0, x) + 1 - a);
}

double 
Weather::CloudinessFactor_Humid (const Time& time, double Si) const
{
  const double a = 1.00;
  const double x = Si / 0.75 / impl.ExtraterrestrialRadiation (time);
  const double cfh = (a * min (1.0, x) + 1 - a);
  return cfh;
}

double 
Weather::RefNetRadiation (const Time& time, double Si,
			  double Temp, double ea) const
{
  static NetRadiation* net_radiation = NULL;
  if (net_radiation == NULL)
    {
      Syntax syntax;
      AttributeList alist;
      alist.add ("type", "brunt");
      net_radiation = &Librarian<NetRadiation>::create (alist);
    }

  const double albedo = 0.23;
  net_radiation->tick (CloudinessFactor_Arid (time, Si),
		       Temp, ea, Si, albedo);
  return net_radiation->net_radiation ();
}

#ifdef BORLAND_TEMPLATES
template class add_submodule<IM>;
#endif

void
Weather::load_syntax (Syntax& syntax, AttributeList& alist)
{
  // Where in the world are we?
  syntax.add ("Latitude", "dg", Syntax::Const,
	      "The position of the weather station on the globe.");
  alist.add ("Latitude", 56.0);

  syntax.add ("Elevation", "m", Syntax::Const,
	      "Heigh above sea level.");
  alist.add ("Elevation", 0.0);
  syntax.add ("UTM_x", Syntax::Unknown (), Syntax::OptionalConst,
	      "X position of weather station."); // Unused.
  syntax.add ("UTM_y", Syntax::Unknown (), Syntax::OptionalConst,
	      "Y position of weather station."); // Unused.

  add_submodule<IM> ("DryDeposit", syntax, alist, Syntax::Const, "\
Dry atmospheric deposition of nitrogen [kg N/year/ha].");
  add_submodule<IM> ("WetDeposit", syntax, alist, Syntax::Const, "\
Deposition of nitrogen solutes with precipitation [ppm].");

  // Division between Rain and Snow.
  syntax.add ("T1", "dg C", Syntax::Const,
	      "Below this air temperature all precipitation is snow.");
  alist.add ("T1", -2.0);
  syntax.add ("T2", "dg C", Syntax::Const, 
	      "Above this air temperature all precipitation is rain.");
  alist.add ("T2", 2.0);

  // Yearly average temperatures.
  syntax.add ("average", "dg C", Syntax::Const,
	      "Average temperature at this location.");
  alist.add ("average", 7.8);
  syntax.add ("amplitude", "dg C", Syntax::Const,
	      "How much the temperature change during the year.");
  alist.add ("amplitude", 8.5);
  syntax.add ("omega", "d^-1", Syntax::Const,
	      "Fraction of a full yearly cycle (2 pi) covered in a day.\n\
The default value corresponds to Earths orbit around the Sun.\n\
Martian take note: You must change this for your home planet.");
  alist.add ("omega", 2.0 * M_PI / 365.0);
  syntax.add ("max_Ta_yday", "d", Syntax::Const,
	      "Julian day where the highest temperature is expected.");
  alist.add ("max_Ta_yday", 209.0);

  // Logs.
  syntax.add ("hourly_air_temperature", "dg C", Syntax::LogOnly,
	      "Temperature this hour.");
  syntax.add ("daily_air_temperature", "dg C", Syntax::LogOnly,
	      "Average temperatures this day.");
  syntax.add ("hourly_global_radiation", "W/m^2", Syntax::LogOnly,
	      "Global radiation this hour.");
  syntax.add ("daily_global_radiation", "W/m^2", Syntax::LogOnly,
	      "Average radiation this day.");
  syntax.add ("reference_evapotranspiration", "mm/h", Syntax::LogOnly,
	      "Reference evapotranspiration this hour");
  syntax.add ("daily_extraterrastial_radiation", "MJ/m^2/d", Syntax::LogOnly,
	      "Extraterrestrial radiation this day.");
  syntax.add ("rain", "mm/h", Syntax::LogOnly, "Rain this hour.");
  syntax.add ("snow", "mm/h", Syntax::LogOnly, "Snow this hour.");
  syntax.add ("cloudiness", Syntax::None (), Syntax::LogOnly,
	      "Fraction of sky covered by clouds [0-1].");
  syntax.add ("vapor_pressure", "Pa", Syntax::LogOnly, "Humidity.");
  syntax.add ("wind", "m/s", Syntax::LogOnly, "Wind speed.");
  syntax.add ("day_length", "h", Syntax::LogOnly,
	      "Number of light hours this day.");
  syntax.add ("day_cycle", Syntax::None (), Syntax::LogOnly, 
	      "Fraction of daily radiation received this hour.");
}

Weather::Weather (const AttributeList& al)
  : impl (*new Implementation (al)),
    name (al.name ("type"))
{ }

Weather::~Weather ()
{ 
  delete &impl;
}
