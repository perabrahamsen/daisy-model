// weather_old.C --- Common code for old weather models.

#include "weather_old.h"
#include "time.h"
#include "mathlib.h"

struct WeatherOld::Implementation
{
  // Parameters.
  const IM DryDeposit;
  const IM WetDeposit;

  // Snow Model.
  const double T_rain;
  const double T_snow;

  // State.
  double daily_extraterrastial_radiation; // [MJ/m2/d]
  double daily_global_radiation; // [W/m²]
  double Prain;
  double Psnow;
  Time time;

  // Create and Destroy.
  Implementation (const AttributeList& al)
    : DryDeposit (al.alist ("DryDeposit")),
      WetDeposit (al.alist ("WetDeposit")),
      T_rain (al.number ("T_rain")),
      T_snow (al.number ("T_snow")),
      daily_extraterrastial_radiation (-42.42e42),
      daily_global_radiation (-42.42e42),
      Prain (0.0),
      Psnow (0.0),
      time (1, 1, 1, 1)
    { }
};

void 
WeatherOld::tick (const Time& time)
{
  Weather::tick (time);

  impl.daily_extraterrastial_radiation 
    = ExtraterrestrialRadiation (time);
  assert (impl.daily_extraterrastial_radiation >= 0.0);

  impl.time = time;
}

void
WeatherOld::output (Log& log) const
{ Weather::output (log); }

double
WeatherOld::hourly_global_radiation () const
{ return (day_cycle () * 24.0) * daily_global_radiation (); }

double
WeatherOld::daily_global_radiation () const
{ return impl.daily_global_radiation; }

double 
WeatherOld::daily_extraterrastial_radiation () const
{ return impl.daily_extraterrastial_radiation; }

double
WeatherOld::rain () const
{
  return impl.Prain;
}

double
WeatherOld::snow () const
{
  return impl.Psnow;
}

void 
WeatherOld::distribute (double precipitation)
{
  const double T = hourly_air_temperature ();
  if (T < impl.T_snow)
    impl.Psnow = precipitation;
  else if (impl.T_rain < T)
    impl.Psnow = 0.0;
  else
    impl.Psnow
      = precipitation * (impl.T_rain - T) / (impl.T_rain - impl.T_snow);

  impl.Prain = precipitation - snow ();
}

double
WeatherOld::hourly_air_temperature () const
{
  // BUG: Should add some kind of day cycle.  
  return daily_air_temperature (); 
}

double
WeatherOld::reference_evapotranspiration () const
{
  const double T = 273.16 + daily_air_temperature ();
  const double Delta = 5362.7 / pow (T, 2.0) * exp (26.042 - 5362.7 / T);
  return 1.05e-3 * Delta / (Delta + 66.7) * hourly_global_radiation ();
}

IM
WeatherOld::deposit () const
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
WeatherOld::cloudiness () const
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
WeatherOld::vapor_pressure () const
{ 
  const double T_min = daily_air_temperature () - 5.0;
  return SaturationVapourPressure (T_min);
}

double 
WeatherOld::wind () const
{ return 0.0; }

void 
WeatherOld::put_precipitation (double prec)
{ distribute (prec / 24.0); }

void 
WeatherOld::put_air_temperature (double)
{ assert (false); }

void 
WeatherOld::put_reference_evapotranspiration (double)
{ assert (false); }

void
WeatherOld::put_global_radiation (double radiation) // [W/m²]
{ impl.daily_global_radiation = radiation; }

WeatherOld::WeatherOld (const AttributeList& al)
  : Weather (al),
    impl (*new Implementation (al))
{ 
  latitude = al.number ("Latitude");
  longitude = al.number ("Longitude");
  elevation = al.number ("Elevation");
  T_average_ = al.number ("average");
  T_amplitude_ = al.number ("amplitude");
  rad_per_day_ = al.number ("omega");
  max_Ta_yday_ = al.number ("max_Ta_yday");
}

WeatherOld::~WeatherOld ()
{ 
  delete &impl;
}

#ifdef BORLAND_TEMPLATES
template class add_submodule<IM>;
#endif

void
WeatherOld::load_syntax (Syntax& syntax, AttributeList& alist)
{
  Weather::load_syntax (syntax, alist);
  // Where in the world are we?
  syntax.add ("Latitude", "dg North", Syntax::Const,
	      "The position of the weather station on the globe.");
  alist.add ("Latitude", 56.0);
  syntax.add ("Longitude", "dg East", Syntax::Const,
	      "The position of the weather station on the globe.");
  alist.add ("Longitude", 10.0);
  syntax.add ("Elevation", "m", Syntax::Const,
	      "Height above sea level.");
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
  syntax.add ("T_rain", "dg C", Syntax::Const, 
	      "Above this air temperature all precipitation is rain.");
  alist.add ("T_rain", 2.0);
  syntax.add ("T_snow", "dg C", Syntax::Const,
	      "Below this air temperature all precipitation is snow.");
  alist.add ("T_snow", -2.0);

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
}

