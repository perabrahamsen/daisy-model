// weather.C

#include "weather.h"
#include "time.h"
#include "net_radiation.h"
#include "log.h"
#include "mathlib.h"

Librarian<Weather>::Content* Librarian<Weather>::content = NULL;

const char *const Weather::description = "\
Meteorological data, as well as the global positioning, are the\n\
responsibility of the `weather' component, typically be reading the\n\
data from a file.  The meteorological data are common to all columns.";

const double SolarConstant = 1366.7; // {W/m2]

double
Weather::screen_height () const	// [m];
{ return screen_height_; }

void
Weather::tick (const Time& time)
{
  // Day length.
  day_length_ = day_length (time);
  day_cycle_ = day_cycle (time);
}

void 
Weather::tick_after (const Time& time)
{
  // Hourly claudiness.
  const double Si = hourly_global_radiation (); 
  if (Si > 25.0)
    {
      hourly_cloudiness_ = CloudinessFactor_Humid (time, Si);
      assert (hourly_cloudiness_ >= 0.0);
      assert (hourly_cloudiness_ <= 1.0);
    }

  // Daily claudiness.
  if (time.hour () == 0.0)
    {
      const double Si = daily_global_radiation () ;
      if (Si > 25.0)
	{
	  daily_cloudiness_ = CloudinessFactor_Humid (time, Si);
	  assert (daily_cloudiness_ >= 0.0);
	  assert (daily_cloudiness_ <= 1.0);
	}
    }
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
  log.output ("reference_evapotranspiration",
	      reference_evapotranspiration ());
  log.output ("rain", rain ());
  log.output ("snow", snow ());
  log.output ("hourly_cloudiness", hourly_cloudiness ());
  log.output ("daily_cloudiness", daily_cloudiness ());
  log.output ("vapor_pressure", vapor_pressure ());
  log.output ("wind", wind ());
  log.output ("day_length", day_length ());
  log.output ("day_cycle", day_cycle ());
}

IM
Weather::deposit () const // [g [stuff] /cm²/h]
{
  const double Precipitation = rain () + snow (); // [mm]
  assert (Precipitation >= 0.0);
  
  // [kg N/ha/year -> [g/cm²/h]
  const double hours_to_years = 365.2425 * 24.0;
  const double kg_per_ha_to_g_cm2 
    = 1000.0 / ((100.0 * 100.0) * (100.0 * 100.0));
  const IM dry (DryDeposit, kg_per_ha_to_g_cm2 / hours_to_years);
  const IM wet (WetDeposit, 1.0e-7); // [ppm] -> [g/cm²/mm]

  const IM result = dry + wet * Precipitation;
  assert (result.NO3 >= 0.0);
  assert (result.NH4 >= 0.0);

  assert (approximate (result.NO3, 
		       DryDeposit.NO3 * kg_per_ha_to_g_cm2/hours_to_years
		       + Precipitation * WetDeposit.NO3 * 1e-7));
  assert (approximate (result.NH4, 
		       DryDeposit.NH4 * kg_per_ha_to_g_cm2/hours_to_years
		       + Precipitation * WetDeposit.NH4 * 1e-7));
  return result;
}

double 
Weather::day_cycle (const Time& time) const	// Sum over a day is 1.0.
{
  // Day length.
  const double dl = day_length (time);
  assert (dl >= 0.0);
  assert (dl <= 24.0);
  
  const int hour = time.hour ();

  // Day cycle.
  double dc;
  if (hour > 12 + dl || hour < 12 - dl)
    dc = 0.0;
  else
    dc = max (0.0, M_PI_2 / dl * cos (M_PI * (hour - 12) / dl));
  assert (dc >= 0.0);
  assert (dc <= 1.0);  

  return dc;
}

double
Weather::day_length (const Time& time) const
{
  double t = 2 * M_PI / 365 * time.yday ();

  const double Dec = (0.3964 - 22.97 * cos (t) + 3.631 * sin (t)
		      - 0.03885 * cos (2 * t)
		      + 0.03838 * sin (2 * t) - 0.15870 * cos (3 * t)
		      + 0.07659 * sin (3 * t) - 0.01021 * cos (4 * t));
  double my_tan 
    = -tan (M_PI / 180.0 * Dec) * tan (M_PI / 180.0 * latitude);
  if (my_tan <= -1.0)
    my_tan = -1.0;
  else if (my_tan >= 1.0)
    my_tan = 1.0;
  t = (24 / M_PI * acos (my_tan));
  const double dl = (t < 0) ? t + 24.0 : t;
  assert (dl >= 0.0);
  assert (dl <= 24.0);
  return dl;
}

double
Weather::LatentHeatVaporization (double Temp) // [J/kg]
{ return ((2.501 - 2.361e-3 * Temp) * 1.0e6); }

double
Weather::PsychrometricConstant (double AtmPressure, double Temp) // [Pa/K]
{ return (1.63 * AtmPressure / LatentHeatVaporization (Temp)); }

double
Weather::T_normal (const Time& time, double delay) const
{
  const double rad_per_day = 2.0 * M_PI / 365.0;

  return T_average
    + T_amplitude
    * exp (delay)
    * cos (rad_per_day * (time.yday () - max_Ta_yday) + delay);
}

double
Weather::AirDensity (double AtmPressure, double Temp) // [kg/m3]
{
  const double Tvirtuel = 1.01 * (Temp + 273);
  return (3.486 * AtmPressure / Tvirtuel);
}

double
Weather::SaturationVapourPressure (double Temp) // [Pa]
{ return (611.0 * exp (17.27 * Temp / (Temp + 237.3))); }

double
Weather::SlopeVapourPressureCurve (double Temp) // [Pa/K]
{ return (4.098E6 * SaturationVapourPressure (Temp) / pow (Temp + 237.3, 2)); }

double
Weather::AtmosphericPressure ()	const // [Pa]
{ return (101300. * pow ((293 - 0.0065 * elevation) / 293, 5.26)); }

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
Weather::ExtraterrestrialRadiation (const Time& time) const // [W/m2]
{
  const double Dec = SolarDeclination (time);
  const double Lat = M_PI / 180 * latitude;
  const double x1 = SunsetHourAngle (Dec, Lat) * sin (Lat) * sin (Dec);
  const double x2 = cos (Lat) * cos (Dec) * sin (SunsetHourAngle (Dec, Lat));
  return (SolarConstant * RelativeSunEarthDistance (time) * (x1 + x2) / M_PI);
}

double
Weather::Makkink (double air_temperature /* dg C */,
		  double global_radiation /* W/m^2 */) /* mm/h */
{
  // Use Makkink's equation for calculating reference_evapotranspiration.
  const double T = 273.16 + air_temperature; // dg C -> K
  const double Delta = 5362.7 / pow (T, 2.0) * exp (26.042 - 5362.7 / T);
  return 1.05e-3
    * Delta / (Delta + 66.7) * global_radiation;
}

double
Weather::HourlyExtraterrestrialRadiation (const Time& time) const // [W/m2]
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
  return ( SolarConstant * RelativeSunEarthDistance (time) *
            (sin(Lat)*sin(Dec) + cos(Lat)*cos(Dec)*cos(SunHourAngle)));
}

Weather::Weather (const AttributeList& al)
  : name (al.name ("type")),
    latitude (-42.42e42),
    longitude (-42.42e42),
    elevation (-42.42e42),
    timezone (-42.42e42),
    surface (Surface::reference),
    screen_height_ (2.0),
    T_average (-42.42e42),
    T_amplitude (-42.42e42),
    max_Ta_yday (-42.42e42),
    day_length_ (-42.42e42),
    day_cycle_ (-42.42e42),
    hourly_cloudiness_ (0.0),	// It may be dark at the start.
    daily_cloudiness_ (0.0)
{
  WetDeposit.NO3 = -42.42e42;
  WetDeposit.NH4 = -42.42e42;
  DryDeposit.NO3 = -42.42e42;
  DryDeposit.NH4 = -42.42e42;
}

Weather::~Weather ()
{ }

bool
Weather::check (const Time&, const Time&, ostream&) const
{ return true; }

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
  syntax.add ("daily_extraterrastial_radiation", "W/m^2", Syntax::LogOnly,
	      "Extraterrestrial radiation this day.");
  syntax.add ("rain", "mm/h", Syntax::LogOnly, "Rain this hour.");
  syntax.add ("snow", "mm/h", Syntax::LogOnly, "Snow this hour.");
  syntax.add ("hourly_cloudiness", Syntax::Fraction (), Syntax::LogOnly,
	      "Fraction of sky covered by clouds [0-1].");
  syntax.add ("daily_cloudiness", Syntax::Fraction(), Syntax::LogOnly,
	      "Fraction of sky covered by clouds [0-1].");
  syntax.add ("vapor_pressure", "Pa", Syntax::LogOnly, "Humidity.");
  syntax.add ("wind", "m/s", Syntax::LogOnly, "Wind speed.");
  syntax.add ("day_length", "h", Syntax::LogOnly,
	      "Number of light hours this day.");
  syntax.add ("day_cycle", Syntax::None (), Syntax::LogOnly,
	      "Fraction of daily radiation received this hour.");
}

