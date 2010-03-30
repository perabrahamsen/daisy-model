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
#include <memory>

struct Timestep::Implementation
{ 
  int days;
  int hours;
  int minutes;
  int seconds;
  Implementation (int d, int h, int m, int s)
    : days (d),
      hours (h),
      minutes (m),
      seconds (s)
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
  return (days ()) * 24.0 + hours () + (minutes () + seconds () / 60.0) / 60.0;
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
}

const Timestep& 
Timestep::null ()
{
  static Timestep none (0, 0, 0, 0);
  return none;
}

Timestep::Timestep (const Block& al)
  : impl (new Implementation (al.integer ("days"),
                              al.integer ("hours"),
                              al.integer ("minutes"),
                              al.integer ("seconds")))
{ }

Timestep::Timestep (const FrameSubmodel& al)
  : impl (new Implementation (al.integer ("days"),
                              al.integer ("hours"),
                              al.integer ("minutes"),
                              al.integer ("seconds")))
{ }

Timestep::Timestep (int d, int h, int m, int s)
  : impl (new Implementation (d, h, m, s))
{ }

Timestep::~Timestep ()
{ }

Timestep::Timestep (const Timestep& other)
  : impl (new Implementation (other.days (), other.hours (),
                              other.minutes (), other.seconds ()))
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
                   a.seconds () + b.seconds ()); }

Timestep operator- (const Timestep& step)
{ return Timestep (-step.days (), -step.hours (), 
                   -step.minutes (), -step.seconds ()); }

Timestep operator- (const Time& a, const Time& b)
{
  if (a < b)
    return -(b - a);

  int seconds = a.second () - b.second ();
  int minutes = a.minute () - b.minute ();
  int hours = a.hour () - b.hour ();
  const Time a_day (a.year (), a.month (), a.mday (), 0, 0, 0);
  const Time b_day (b.year (), b.month (), b.mday (), 0, 0, 0);
  int days = Time::days_between (b_day, a_day);
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
  return Timestep (days, hours, minutes, seconds);
}

bool 
operator== (const Timestep& a, const Timestep& b)
{
  static const Time center (5000, 1, 1, 0);
  return center + a == center + b;
}

// timestep.C ends here
