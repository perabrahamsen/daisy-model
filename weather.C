// weather.C

#include "weather.h"
#include "time.h"
#include "net_radiation.h"
#include "log.h"

Librarian<Weather>::Content* Librarian<Weather>::content = NULL;

const char *const Weather::description = "\
Meteorological data, as well as the global positioning, are the\n\
responsibility of the `weather' component, typically be reading the\n\
data from a file.  The meteorological data are common to all columns.";

void 
Weather::tick (const Time& time)
{
  day_length_ = day_length (time);
  assert (day_length_ >= 0.0);
  assert (day_length_ <= 24.0);

  if (time.hour () > 12 + day_length () || time.hour () < 12 - day_length ())
    day_cycle_ = 0.0;
  else
    day_cycle_ = max (0.0, M_PI_2 / day_length ()
		     * cos (M_PI * (time.hour () - 12) / day_length ()));
  assert (day_cycle_ >= 0.0);
  assert (day_cycle_ <= 1.0);
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

double
Weather::day_length (const Time& time) const
{
  double t = 2 * M_PI / 365 * time.yday ();
  
  double Dec = (0.3964 - 22.97 * cos (t) + 3.631 * sin (t)
		- 0.03885 * cos (2 * t) 
		+ 0.03838 * sin (2 * t) - 0.15870 * cos (3 * t) 
		+ 0.07659 * sin (3 * t) - 0.01021 * cos (4 * t));
  t = (24 / M_PI
       * acos (-tan (M_PI / 180 * Dec) * tan (M_PI / 180 * latitude)));
  return (t < 0) ? t + 24.0 : t;
}

double
Weather::LatentHeatVaporization (double Temp) // [MJ/kg]
{ return (2.501 - 2.361e-3 * Temp); }

double 
Weather::PsychrometricConstant (double AtmPressure, double Temp) // [kPa/K]
{ return (0.00163 * AtmPressure / LatentHeatVaporization (Temp)); }

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
Weather::AtmosphericPressure ()	const // [kPa]
{ return (101.3 * pow ((293 - 0.0065 * elevation) / 293, 5.26)); }

double 
Weather::CloudinessFactor_Arid (const Time& time, double Si) const
{
  const double a = 1.35;
  const double x = Si / 0.75 / ExtraterrestrialRadiation (time);
  return (a * min (1.0, x) + 1 - a);
}

double 
Weather::CloudinessFactor_Humid (const Time& time, double Si) const
{
  const double a = 1.00;
  const double x = Si / 0.75 / ExtraterrestrialRadiation (time);
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

double 
Weather::SolarDeclination (const Time& time) // [rad]
{
  return (0.409 * sin (2.0 * M_PI * time.yday () / 365.0 - 1.39));
}

double 
Weather::RelativeSunEarthDistance (const Time& time)
{
  return (1.0 + 0.033 * cos (2.0 * M_PI * time.yday () / 365.0));
}

double 
Weather::SunsetHourAngle (double Dec, double Lat) // [rad]
{
  return (acos (-tan (Dec) * tan (Lat)));
}

double 
Weather::ExtraterrestrialRadiation (const Time& time) const // [MJ/m2/d]
{
  const double Dec = SolarDeclination (time);
  const double Lat = M_PI / 180 * latitude;
  const double x1 = SunsetHourAngle (Dec, Lat) * sin (Lat) * sin (Dec);
  const double x2 = cos (Lat) * cos (Dec) * sin (SunsetHourAngle (Dec, Lat));
  return (37.6 * RelativeSunEarthDistance (time) * (x1 + x2));
}

double
Weather::HourlyExtraterrestrialRadiation (const Time& time) const // [MJ/m2/h]
{
  static const double EQT0   = 0.002733;
  static const double EQT1[] = {-7.343,-9.470,-0.3289,-0.1955};
  static const double EQT2[] = {0.5519,-3.020,-0.07581,-0.1245};
  const double Dec = SolarDeclination (time);
  const double Lat = M_PI / 180 * latitude;
  const double timelag = (timezone - longitude) / 15.0;
  double EQT = EQT0;
  for (unsigned int i = 0; i < 3; i++)
    {
       const double P = 2.0 * M_PI / 365.0 * (i+1) * time.yday();
       EQT += EQT1[i] * sin(P) + EQT2[i] * cos(P);
    }
  EQT /= 60.0;
  const double SunHourAngle = M_PI / 12.0 * (time.hour() + 1 + EQT - timelag);
  const double SolarConstant = 37.6 / 24.0 * RelativeSunEarthDistance (time);
  return ( SolarConstant *
            (sin(Lat)*sin(Dec) + cos(Lat)*cos(Dec)*cos(SunHourAngle)));
}

Weather::Weather (const AttributeList& al)
  : name (al.name ("type")),
    latitude (-42.42e42),
    longitude (-42.42e42),
    elevation (-42.42e42),
    T_average_ (-42.42e42),
    T_amplitude_ (-42.42e42),
    rad_per_day_ (2.0 * M_PI / 365.0),
    max_Ta_yday_ (-42.42e42),
    day_length_ (-42.42e42),
    day_cycle_ (-42.42e42)
{ }

Weather::~Weather ()
{ }

void
Weather::load_syntax (Syntax& syntax, AttributeList&)
{
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

