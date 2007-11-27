// timestep.C -- Time difference.
// 
// Copyright 2006 Per Abrahamsen and KVL.
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

#define BUILD_DLL

#include "timestep.h"
#include "syntax.h"
#include "alist.h"
#include "block.h"
#include "assertion.h"
#include "mathlib.h"
#include <memory>

struct Timestep::Implementation
{ 
  int years;
  int days;
  int hours;
  int minutes;
  int seconds;
  Implementation (int y, int d, int h, int m, int s)
    : years (y),
      days (d),
      hours (h),
      minutes (m),
      seconds (s)
  { }
};

int
Timestep::years () const
{ return impl->years; }

int
Timestep::days () const
{ return impl->days; }

int
Timestep::hours () const
{ return impl->hours; }

int
Timestep::minutes () const
{ return impl->minutes; }

int
Timestep::seconds () const
{ return impl->seconds; }

double 
Timestep::total_hours () const
{
  return (365.2425 * years () + days ()) * 24.0 + hours ()
    + (minutes () + seconds () / 60.0) / 60.0;
}

void
Timestep::GenCheck::check (const Metalib&,
			   const Syntax& syntax, 
                           const AttributeList& alist, 
                           const std::string& key) const throw (std::string)
{ 
  daisy_assert (alist.check (key));
  daisy_assert (syntax.lookup (key) == Syntax::AList);
  daisy_assert (!syntax.is_log (key));
  daisy_assert (syntax.size (key) == Syntax::Singleton);

  Timestep timestep (alist.alist (key));
  check_dt (timestep.total_hours ());
}

const VCheck& 
Timestep::positive ()
{
  static struct Postive : public GenCheck
  {
    void check_dt (const double dt) const throw (std::string)
    {
      if (dt <= 0.0)
        throw std::string ("Timestep must be positive");
    }
  } positive;
  return positive;
}

const VCheck& 
Timestep::non_zero ()
{
  static struct NonZero : public GenCheck
  {
    void check_dt (const double dt) const throw (std::string)
    {
      if (iszero (dt))
        throw std::string ("Timestep must be non-zero");
    }
  } non_zero;
  return non_zero;
}

void 
Timestep::load_syntax (Syntax& syntax, AttributeList& alist)
{
  syntax.add ("years", Syntax::Integer, Syntax::State, 
              "Number of years.");
  alist.add ("years", 0);
  syntax.add ("days", Syntax::Integer, Syntax::State, 
              "Number of days.");
  alist.add ("days", 0);
  syntax.add ("hours", Syntax::Integer, Syntax::State, 
              "Number of hours.");
  alist.add ("hours", 0);
  syntax.add ("minutes", Syntax::Integer, Syntax::State, 
              "Number of minutes.");
  alist.add ("minutes", 0);
  syntax.add ("seconds", Syntax::Integer, Syntax::State, 
              "Number of seconds.");
  alist.add ("seconds", 0);
}

Timestep::Timestep (Block& al)
  : impl (new Implementation (al.integer ("years"), 
                              al.integer ("days"),
                              al.integer ("hours"),
                              al.integer ("minutes"),
                              al.integer ("seconds")))
{ }

Timestep::Timestep (const AttributeList& al)
  : impl (new Implementation (al.integer ("years"), 
                              al.integer ("days"),
                              al.integer ("hours"),
                              al.integer ("minutes"),
                              al.integer ("seconds")))
{ }

Timestep::Timestep (int y, int d, int h, int m, int s)
  : impl (new Implementation (y, d, h, m, s))
{ }

Timestep::Timestep (const Timestep& other)
  : impl (new Implementation (other.years (), other.days (), other.hours (),
                              other.minutes (), other.seconds ()))
{ }

Timestep::~Timestep ()
{ }

void operator+= (Time& time, const Timestep& step)
{
  time.tick_year (step.years ());
  time.tick_day (step.days ());
  time.tick_hour (step.hours ());
  time.tick_minute (step.minutes ());
  time.tick_second (step.seconds ());
}

Time operator+ (const Time& old, const Timestep& step)
{
  Time time = old;
  time += step;
  return time;
}

Time operator- (const Time& old, const Timestep& step)
{
  Time time = old;
  time += -step;
  return time;
}

Timestep operator+ (const Timestep& a, const Timestep& b)
{ return Timestep (a.years () + b.years (),
                   a.days () + b.days (),
                   a.hours () + b.hours (),
                   a.minutes () + b.minutes (),
                   a.seconds () + b.seconds ()); }

Timestep operator- (const Timestep& step)
{ return Timestep (-step.years (), -step.days (), -step.hours (), 
                   -step.minutes (), -step.seconds ()); }

Timestep operator- (const Time& a, const Time& b)
{
  if (a < b)
    return -(b - a);

  int seconds = a.second () - b.second ();
  int minutes = a.minute () - b.minute ();
  int hours = a.hour () - b.hour ();
  int days = a.yday () - b.yday ();
  int years = a.year () - b.year ();
  if (seconds < 0)
    {
      seconds += 60;
      minutes -= 1;
    }
  if (minutes < 0)
    {
      minutes += 60;
      hours -= 1;
    }
  if (hours < 0)
    {
      hours += 24;
      days -= 1;
    }
  if (days < 0)
    { 
      days += 365;
      if (Time::leap (a.year ()))
        days += 1;
      years -= 1;
    }
  daisy_assert (years >= 0);
  return Timestep (years, days, hours, minutes, seconds);
}

// timestep.C ends here
