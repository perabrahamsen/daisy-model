// weather.C

#include "weather.h"
#include "time.h"
#include "library.h"
#include "alist.h"
#include "syntax.h"
#include "mathlib.h"
#include "common.h"
#include <map>
// Doesn't exist in Borland C++ 5.0.1
// #include <algobase.h>

Librarian<Weather>::Content* Librarian<Weather>::content = NULL;

struct Weather::Implementation
{
  const double Latitude;
  const IM DryDeposit;
  const IM SoluteDeposit;
  Implementation (const AttributeList& al)
    : Latitude (al.number ("Latitude")),
      DryDeposit (al.alist ("DryDeposit")),
      SoluteDeposit (al.alist ("SoluteDeposit"))
  { }
};

void
Weather::output (Log&, Filter&) const
{ }

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
  const IM solute (impl.SoluteDeposit, 0.1); // [kg/m²/mm] -> [g/cm²/mm]

  const IM result = dry + solute * Precipitation;

  assert (approximate (result.NO3, 
		       impl.DryDeposit.NO3 / 10.0 / 24.0
		       + Precipitation * impl.SoluteDeposit.NO3 / 10.0));
  assert (approximate (result.NH4, 
		       impl.DryDeposit.NH4 / 10.0 / 24.0
		       + Precipitation * impl.SoluteDeposit.NH4 / 10.0));
  return result;
}

void
Weather::load_syntax (Syntax& syntax, AttributeList& alist)
{
  syntax.add ("Latitude", Syntax::Number, Syntax::Const);
  alist.add ("Latitude", 56.0);
  // DryDeposit
  {
    Syntax& s = *new Syntax ();
    AttributeList& a = *new AttributeList ();
    IM::load_syntax (s, a);
    a.add ("NH4", 0.6e-6);	// kg/m²/d
    a.add ("NO3", 0.3e-6);	// kg/m²/d
    syntax.add ("DryDeposit", s, Syntax::Const, Syntax::Singleton);
    alist.add ("DryDeposit", a);
  }
  // SoluteDeposit
  {
    Syntax& s = *new Syntax ();
    AttributeList& a = *new AttributeList ();
    IM::load_syntax (s, a);
    a.add ("NO3", 0.6e-6); // kg/m²/mm
    a.add ("NH4", 0.9e-6); // kg/m²/mm
    syntax.add ("SoluteDeposit", s, Syntax::Const, Syntax::Singleton);
    alist.add ("SoluteDeposit", a);
  }
}

Weather::Weather (const AttributeList& al)
  : impl (*new Implementation (al)),
    name (al.name ("type"))
{ }

Weather::~Weather ()
{ 
  delete &impl;
}
