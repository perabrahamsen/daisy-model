// weather.C

#include "weather.h"
#include "time.h"
#include "mathlib.h"

Librarian<Weather>::Content* Librarian<Weather>::content = NULL;

struct Weather::Implementation
{
  const double Latitude;
  const IM DryDeposit;
  const IM WetDeposit;
  const double T1;
  const double T2;
  double Prain;
  double Psnow;
  const double average;		// Average temperature at bottom [C]
  const double amplitude;	// Variation in bottom temperature [C]
  const double omega;		// Period length for above [ rad / day]
  const double omega_offset;	// Period start for above [rad]
  Implementation (const AttributeList& al)
    : Latitude (al.number ("Latitude")),
      DryDeposit (al.alist ("DryDeposit")),
      WetDeposit (al.alist ("WetDeposit")),
      T1 (al.number ("T1")),
      T2 (al.number ("T2")),
      Prain (0.0),
      Psnow (0.0),
      average (al.number ("average")),
      amplitude (al.number ("amplitude")),
      omega (al.number ("omega")),
      omega_offset (al.number ("omega_offset"))
    { }
};

double
Weather::Rain () const
{
  return impl.Prain;
}

double
Weather::Snow () const
{
  return impl.Psnow;
}

void
Weather::output (Log&, Filter&) const
{ }

void 
Weather::distribute (double precipitation)
{
  const double T = AirTemperature ();
  if (T < impl.T1)
    impl.Psnow = precipitation;
  else if (impl.T2 < T)
    impl.Psnow = 0.0;
  else
    impl.Psnow = precipitation * (impl.T2 - T) / (impl.T2 - impl.T1);

  impl.Prain = precipitation - Snow ();
}

double
Weather::DayLength (const Time& time) const
{
  return DayLength (impl.Latitude, time);
}

double
Weather::DayLength (double Latitude, const Time& time)
{
  double t = 2 * M_PI / 365 * time.yday ();
  
  double Dec = (0.3964 - 22.97 * cos (t) + 3.631 * sin (t)
		- 0.03885 * cos (2 * t) 
		+ 0.03838 * sin (2 * t) - 0.15870 * cos (3 * t) 
		+ 0.07659 * sin (3 * t) - 0.01021 * cos (4 * t));
  t = (24 / M_PI
       * acos (-tan (M_PI / 180 * Dec) * tan (M_PI / 180 * Latitude)));
  return (t < 0) ? t + 24.0 : t;
}

double
Weather::DayCycle (const Time& time) const
{
  return max (0.0, M_PI_2 / DayLength (time)
	      * cos (M_PI * (time.hour () - 12) / DayLength (time)));
}

IM
Weather::Deposit() const
{
  const double Precipitation = Rain () + Snow (); // [mm]
  const IM dry (impl.DryDeposit, 0.1/24.0); // [kg/m²/d] -> [g/cm²/h]
  const IM wet (impl.WetDeposit, 0.1); // [kg/m²/mm] -> [g/cm²/mm]

  const IM result = dry + wet * Precipitation;

  assert (approximate (result.NO3, 
		       impl.DryDeposit.NO3 / 10.0 / 24.0
		       + Precipitation * impl.WetDeposit.NO3 / 10.0));
  assert (approximate (result.NH4, 
		       impl.DryDeposit.NH4 / 10.0 / 24.0
		       + Precipitation * impl.WetDeposit.NH4 / 10.0));
  return result;
}

double 
Weather::cloudiness () const
{ return 0.0; }

double 
Weather::vaporpressure () const
{ return 0.0; }

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
Weather::omega_offset () const
{ return impl.omega_offset; }


void
Weather::load_syntax (Syntax& syntax, AttributeList& alist)
{
  // Where in the world are we?
  syntax.add ("Latitude", Syntax::Number, Syntax::Const);
  alist.add ("Latitude", 56.0);

  syntax.add ("Elevation", Syntax::Number, Syntax::Optional); // Unused.
  syntax.add ("UTM_x", Syntax::Number, Syntax::Optional); // Unused.
  syntax.add ("UTM_y", Syntax::Number, Syntax::Optional); // Unused.

  // DryDeposit
  {
    Syntax& s = *new Syntax ();
    AttributeList& a = *new AttributeList ();
    IM::load_syntax (s, a);
    // These values change often, and shouldn't have defaults.
    // a.add ("NH4", 0.6e-6);	// kg/m²/d
    // a.add ("NO3", 0.3e-6);	// kg/m²/d
    syntax.add ("DryDeposit", s, Syntax::Const, Syntax::Singleton);
    alist.add ("DryDeposit", a);
  }
  // WetDeposit
  {
    Syntax& s = *new Syntax ();
    AttributeList& a = *new AttributeList ();
    IM::load_syntax (s, a);
    // These values change often, and shouldn't have defaults.
    // a.add ("NO3", 0.6e-6); // kg/m²/mm
    // a.add ("NH4", 0.9e-6); // kg/m²/mm
    syntax.add ("WetDeposit", s, Syntax::Const, Syntax::Singleton);
    alist.add ("WetDeposit", a);
  }
  // Division between Rain and Snow.
  syntax.add ("T1", Syntax::Number, Syntax::Const);
  alist.add ("T1", -2.0);
  syntax.add ("T2", Syntax::Number, Syntax::Const);
  alist.add ("T2", 2.0);
  syntax.add ("Prain", Syntax::Number, Syntax::LogOnly);
  syntax.add ("Psnow", Syntax::Number, Syntax::LogOnly);

  // Yearly average temperatures.
  syntax.add ("average", Syntax::Number, Syntax::Const);
  alist.add ("average", 7.8);
  syntax.add ("amplitude", Syntax::Number, Syntax::Const);
  alist.add ("amplitude", 8.5);
  syntax.add ("omega", Syntax::Number, Syntax::Const);
  alist.add ("omega", 2.0 * M_PI / 365.0);
  syntax.add ("omega_offset", Syntax::Number, Syntax::Const);
  alist.add ("omega_offset", -209.0);
}

Weather::Weather (const AttributeList& al)
  : impl (*new Implementation (al)),
    name (al.name ("type"))
{ }

Weather::~Weather ()
{ 
  delete &impl;
}
