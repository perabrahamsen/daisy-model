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
#include "frame_submodel.h"
#include "block.h"
#include "assertion.h"
#include "mathlib.h"
#include "treelog.h"
#include "librarian.h"
#include <sstream>
#include <iomanip>

struct Timestep::Implementation
{ 
  int days;
  int hours;
  int minutes;
  int seconds;
  int microseconds;
  Implementation (int d, int h, int m, int s, int us)
    : days (d),
      hours (h),
      minutes (m),
      seconds (s),
      microseconds (us)
  { }
};

const Timestep& 
Timestep::day ()
{
  static const Timestep value (1, 0, 0, 0);
  return value;
}

const Timestep& 
Timestep::hour ()
{
  static const Timestep value (0, 1, 0, 0);
  return value;
}

const Timestep& 
Timestep::minute ()
{
  static const Timestep value (0, 0, 1, 0);
  return value;
}

const Timestep& 
Timestep::second ()
{
  static const Timestep value (0, 0, 0, 1);
  return value;
}

const Timestep& 
Timestep::microsecond ()
{
  static const Timestep value (0, 0, 0, 0, 1);
  return value;
}

const Timestep& 
Timestep::zero ()
{
  static Timestep step (0, 0, 0, 0);
  return step;
}

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

int
Timestep::microseconds () const
{ return impl->microseconds; }

double 
Timestep::total_hours () const
{
  double result =  0;           // [us]
  result += microseconds ();    
  result /= 1000000.0;          // [s]
  result += seconds ();           
  result /= 60.0;               // [m]
  result += minutes ();
  result /= 60.0;               // [h]
  result += hours () + days () * 24.0;
  return result;
}

std::string
Timestep::print () const
{
  std::ostringstream tmp;
  if (days () > 0)
    tmp << days () << "d";
  if (hours () > 0)
    tmp << hours () << "h";
  if (minutes () > 0)
    tmp << minutes () << "m";
  if (seconds () > 0 || microseconds () > 0)
    {
      tmp << seconds ();
      if (microseconds () > 0)
        tmp << "." << std::setfill ('0') << std::setw (6) << microseconds ();
      tmp << "s";
    }
  std::string result = tmp.str ();
  if (result.size () == 0)
    result = "0s";
  return result;
}

bool
Timestep::GenCheck::verify (const Metalib&, const Frame& frame, const symbol key, 
                            Treelog& msg) const
{ 
  daisy_assert (frame.check (key));
  daisy_assert (frame.lookup (key) == Attribute::Submodel);
  daisy_assert (!frame.is_log (key));
  daisy_assert (frame.type_size (key) == Attribute::Singleton);

  Timestep timestep (frame.submodel (key));
  return check_dt (timestep.total_hours (), msg);
}

const VCheck& 
Timestep::positive ()
{
  static struct Postive : public GenCheck
  {
    bool check_dt (const double dt, Treelog& msg) const
    {
      if (dt > 0.0)
        return true;

      msg.error ("Timestep must be positive");
      return false;
    }
  } positive;
  return positive;
}

const VCheck& 
Timestep::non_zero ()
{
  static struct NonZero : public GenCheck
  {
    bool check_dt (const double dt, Treelog& msg) const
    {
      if (std::isnormal (dt))
        return true;

      msg.error ("Timestep must be non-zero");
      return false;
    }
  } non_zero;
  return non_zero;
}

void 
Timestep::load_syntax (Frame& frame)
{ load_frame (frame); }

void 
Timestep::load_frame (Frame& frame)
{
  frame.declare_integer ("days", Attribute::State, 
              "Number of days.");
  frame.set ("days", 0);
  frame.declare_integer ("hours", Attribute::State, 
              "Number of hours.");
  frame.set ("hours", 0);
  frame.declare_integer ("minutes", Attribute::State, 
              "Number of minutes.");
  frame.set ("minutes", 0);
  frame.declare_integer ("seconds", Attribute::State, 
              "Number of seconds.");
  frame.set ("seconds", 0);
  frame.declare_integer ("microseconds", Attribute::State, 
              "Number of microseconds.");
  frame.set ("microseconds", 0);
}

const Timestep& 
Timestep::null ()
{ return zero (); }

Timestep
Timestep::build_hours (const double dt /* h */)
{
  daisy_assert (dt > 0);
  static const double us = 1.0 / (60.0 * 60.0 * 1000000.0);
  double time = dt + 0.1 * us;       // We add 0.1 [us] for rounding.
  daisy_assert (time >= 0.0);
  time /= 24.0;                 // [d]
  const int days = double2int (time);
  time -= days;
  daisy_assert (time >= 0.0);
  daisy_assert (time < 1.0);
  time *= 24.0;                 // [h]
  const int hours = double2int (time);
  daisy_assert (hours < 24);
  time -= hours;
  daisy_assert (time >= 0.0);
  daisy_assert (time < 1.0);
  time *= 60.0;                 // [m]
  const int minutes = double2int (time);
  daisy_assert (minutes < 60);
  time -= minutes;
  daisy_assert (time >= 0.0);
  daisy_assert (time < 1.0);
  time *= 60.0;                 // [s]
  const int seconds = double2int (time);
  daisy_assert (seconds < 60);
  time -= seconds;
  daisy_assert (time >= 0.0);
  daisy_assert (time < 1.0);
  time *= 1000000.0;            // [us]
  int microseconds = double2int (time);
  daisy_assert (microseconds < 1000000);
  time -= microseconds;
  daisy_assert (time >= 0.0);
  daisy_assert (time < 1.0);

  Timestep result (days, hours, minutes, seconds, microseconds);
  const double new_dt = result.total_hours ();
  daisy_assert (new_dt <= dt + 0.2 * us);
  daisy_assert (new_dt > 0);
  daisy_assert ((dt - new_dt) * 1000000.0 < 1.0);
  return result;
}

Timestep::Timestep (const Block& al)
  : impl (new Implementation (al.integer ("days"),
                              al.integer ("hours"),
                              al.integer ("minutes"),
                              al.integer ("seconds"),
                              al.integer ("microseconds")))
{ }

Timestep::Timestep (const FrameSubmodel& al)
  : impl (new Implementation (al.integer ("days"),
                              al.integer ("hours"),
                              al.integer ("minutes"),
                              al.integer ("seconds"),
                              al.integer ("microseconds")))
{ }

Timestep::Timestep (int d, int h, int m, int s, int us)
  : impl (new Implementation (d, h, m, s, us))
{ }

Timestep::~Timestep ()
{ }

Timestep::Timestep (const Timestep& other)
  : impl (new Implementation (other.days (), other.hours (),
                              other.minutes (), other.seconds (),
                              other.microseconds ()))
{ }

const Timestep& 
Timestep::operator= (const Timestep& t)
{
  *(this->impl) = *(t.impl);
  return *this;
}

void operator+= (Time& time, const Timestep& step)
{
  time.tick_day (step.days ());
  time.tick_hour (step.hours ());
  time.tick_minute (step.minutes ());
  time.tick_second (step.seconds ());
  time.tick_microsecond (step.microseconds ());
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
{ return Timestep (a.days () + b.days (),
                   a.hours () + b.hours (),
                   a.minutes () + b.minutes (),
                   a.seconds () + b.seconds (),
                   a.microseconds () + b.microseconds ()); }

Timestep operator- (const Timestep& step)
{ return Timestep (-step.days (), -step.hours (), 
                   -step.minutes (), -step.seconds (),
                   -step.microseconds ()); }

Timestep operator- (const Time& a, const Time& b)
{
  if (a < b)
    return -(b - a);

  int microseconds = a.microsecond () - b.microsecond ();
  int seconds = a.second () - b.second ();
  int minutes = a.minute () - b.minute ();
  int hours = a.hour () - b.hour ();
  const Time a_day (a.year (), a.month (), a.mday (), 0, 0, 0);
  const Time b_day (b.year (), b.month (), b.mday (), 0, 0, 0);
  int days = Time::whole_days_between (b_day, a_day);
  if (microseconds < 0)
    {
      microseconds += 1000000;
      seconds -= 1;
    }
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
  return Timestep (days, hours, minutes, seconds, microseconds);
}

Timestep operator/ (const Timestep& step, int divisor)
{
  int days = step.days ();
  int hours = (days % divisor) * 24 + step.hours ();
  days /= divisor;
  int minutes = (hours % divisor) * 60 + step.minutes ();
  hours /= divisor;
  int seconds = (minutes % divisor) * 60 + step.seconds ();
  minutes /= divisor;
  int microseconds = (seconds % divisor) * 1000000 + step.microseconds ();
  seconds /= divisor;
  microseconds /= divisor;

  return Timestep (days, hours, minutes, seconds, microseconds);
}

bool 
operator== (const Timestep& a, const Timestep& b)
{
  static const Time center (5000, 1, 1, 0);
  return center + a == center + b;
}

static DeclareSubmodel 
timestep_submodel (Timestep::load_syntax, "Timestep", "\
Relative time.");

// timestep.C ends here
