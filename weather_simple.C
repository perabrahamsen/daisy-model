// weather_simple.C
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
//
// This file is part of Daisy.
// 
// Daisy is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser Public License as published by
// the Free Software Foundation; either version 2.1 of the License, or
// (at your option) any later version.
// 
// Daisy is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser Public License for more details.
// 
// You should have received a copy of the GNU Lesser Public License
// along with Daisy; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA


#include "weather_old.h"
#include "time.h"
#include "log.h"
#include "mathlib.h"

class WeatherSimple : public WeatherOld
{
  const double precipitation;
  const int interval;
  double reference_evapotranspiration_;
  Time time;

  // Communication with external model.
  void put_reference_evapotranspiration (double ref)
    { reference_evapotranspiration_ = ref; }

  // Simulation.
public:
  void tick (const Time&, Treelog&);
  double daily_air_temperature () const;
  double daily_max_air_temperature () const
  { return daily_air_temperature (); }
  double daily_min_air_temperature () const
  { return daily_air_temperature (); }
  double hourly_global_radiation () const;
  double daily_global_radiation () const;
  double reference_evapotranspiration () const;
  double Precipitation () const;

  // Create and Destroy.
public:
  WeatherSimple (const AttributeList&);
  ~WeatherSimple ();
};

void
WeatherSimple::tick (const Time& t, Treelog& o)
{ 
  WeatherOld::tick (t, o);
  time = t;
  WeatherOld::distribute (Precipitation ());
  Weather::tick_after (time, o);
}

double
WeatherSimple::daily_air_temperature () const // [C]
{
  double t = 2 * M_PI / 365 * time.yday ();
  return (7.7 - 7.7 * cos (t) - 3.6 * sin (t));
}

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

double
WeatherSimple::hourly_global_radiation () const	// [W/m²]
{
  double t = 2 * M_PI / 24 * time.hour ();
  int m = time.month () - 1;
  double Si = (  A0[m] + A1[m] * cos (t) + B1[m] * sin (t)
		 + A2[m] * cos (2 * t) + B2[m] * sin (2 * t));
  return max (0.0, Si);
}

double
WeatherSimple::daily_global_radiation () const	// [W/m²]
{
  int m = time.month () - 1;
  double Si = A0[m];
  return max (0.0, Si);
}

double
WeatherSimple::reference_evapotranspiration () const // [mm/h]
{
  if (reference_evapotranspiration_ < 0)
    return WeatherOld::reference_evapotranspiration ();
  else
    return reference_evapotranspiration_;
}

double
WeatherSimple::Precipitation () const
{
  if (((time.yday () * 24 + time.hour ()) % interval) == 0)
    return precipitation;
  else 
    return 0.0;
}

WeatherSimple::WeatherSimple (const AttributeList& al)
  : WeatherOld (al),
    precipitation (al.number ("precipitation")),
    interval (al.integer ("interval")),
    reference_evapotranspiration_ (al.number ("reference_evapotranspiration")),
    time (1, 1, 1, 1)
{ }

WeatherSimple::~WeatherSimple ()
{ }

// Add the WeatherSimple syntax to the syntax table.
static struct WeatherSimpleSyntax
{
  static Weather& make (const AttributeList& al)
    {
      return *new WeatherSimple (al);
    }

  WeatherSimpleSyntax ()
    { 
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      alist.add ("description", "\
A weather model with precipitation at regular intervals.");
      WeatherOld::load_syntax (syntax, alist);
      syntax.add ("precipitation", "mm/h", Syntax::Const,
		  "Amount of precipitation.");
      alist.add ("precipitation", 0.0);
      syntax.add ("interval", Syntax::Integer, Syntax::Const,
		  "Number of hours between each precipitation event.");
      alist.add ("interval", 1);
      syntax.add ("reference_evapotranspiration", "mm/h", Syntax::Const,
		  "Constant reference evapotranspiration.");
      alist.add ("reference_evapotranspiration", -1.0);
      Librarian<Weather>::add_type ("simple", alist, syntax, &make);
    }
} WeatherSimple_syntax;
