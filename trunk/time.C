// time.C
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen.
// Copyright 2000-2001 KVL.
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

#include "time.h"
#include "assertion.h"
#include "log.h"
#include "frame_submodel.h"
#include "vcheck.h"
#include "librarian.h"
#include "block.h"
#include "treelog.h"
#include <sstream>
#include <iomanip>
#include <ctime>

// Content.

struct Time::Implementation
{
  static const int mlen[];
  static const symbol mname[];
  static const symbol wname[];
  short year;
  short yday;
  char hour;  
  char minute;
  char second;
  int microsecond;              // Assume 21 bit int minimum.
  Implementation (int, int, int, int, int, int);
  Implementation (const Implementation&);
};

const int Time::Implementation::mlen[] =
{ -999, 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365 };

const symbol Time::Implementation::mname[] =
{ "Error", "January", "February", "March", "April", "May", "June",
  "July", "August", "September", "October", "November" , "December" };

const symbol Time::Implementation::wname[] =
{ "Error", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", 
  "Saturday", "Sunday" };

Time::Implementation::Implementation (int y, int d, int h, int m, int s, int us)
  : year (static_cast<short> (y)),
    yday (static_cast<short> (d)),
    hour (static_cast<char> (h)),
    minute (static_cast<char> (m)),
    second (static_cast<char> (s)),
    microsecond (us)
{ }

Time::Implementation::Implementation (const Implementation& i)
  : year (i.year),
    yday (i.yday),
    hour (i.hour), 
    minute (i.minute), 
    second (i.second),
    microsecond (i.microsecond)
{ }

// @ Extract.

int
Time::year () const
{ return impl->year; }

int
Time::month () const
{ return yday2month (impl->year, impl->yday); }

int
Time::week () const
{ return yday2week (impl->year, impl->yday); }

int
Time::yday () const
{ return impl->yday; }

int
Time::mday () const
{ return yday2mday (impl->year, impl->yday); }

int
Time::wday () const
{ return yday2wday (impl->year, impl->yday); }

int
Time::hour () const
{ return impl->hour; }

int
Time::minute () const
{ return impl->minute; }

int
Time::second () const
{ return impl->second; }

int
Time::microsecond () const
{ return impl->microsecond; }

std::string
Time::print () const
{
  std::ostringstream tmp;
  tmp << std::setfill ('0') << std:: setw (4) << year () << "-"
      << std::setw (2) << month () << "-" 
      << std::setw (2) << mday () << "T"
      << std::setw (2) << hour () << ":"
      << std::setw (2) << minute () << ":"
      << std::setw (2) << second ();
  const int us = microsecond ();
  if (us != 0)
    tmp << "." << std::setw (6) << microsecond ();
  
  return tmp.str ();
}

void 
Time::set_time (Frame& parent, const symbol key) const
{
  boost::shared_ptr<FrameSubmodel> child (&parent.submodel (key).clone ());
  child->set ("year", year ());
  child->set ("month", month ());
  child->set ("mday", mday ());
  child->set ("hour", hour ());
  child->set ("minute", minute ());
  child->set ("second", second ());
  child->set ("microsecond", microsecond ());
  parent.set (key, child);
}

double 
Time::year_fraction () const
{
  // Fraction of year since new year.
  const double total_days = leap (year ()) ? 366.0 : 365.0;
  const double days = yday () + day_fraction ();
  return days / total_days;
}

double 
Time::day_fraction () const
{
  // Fraction of day since midnight.
  const double microseconds = microsecond ();
  const double seconds = second () + microseconds / 1000000.0;
  const double minutes = minute () + seconds / 60.0;
  const double hours = hour () + minutes / 60.0;
  return hours / 24.0;
}

int 
Time::component_value (component_t c) const
{
  switch (c)
    {
    case Year:
      return year ();
    case Month:
      return month ();
    case Week:
      return week ();
    case Yday:
      return yday ();
    case Mday:
      return mday ();
    case Wday:
      return wday ();
    case Hour:
      return hour ();
    case Minute:
      return minute ();
    case Second:
      return second ();
    case Microsecond:
      return microsecond ();
    }
  daisy_notreached ();
}

symbol
Time::component_name (component_t c)
{
  switch (c)
    {
    case Year:
      return "year";
    case Month:
      return "month";
    case Week:
      return "week";
    case Yday:
      return "yday";
    case Mday:
      return "mday";
    case Wday:
      return "wday";
    case Hour:
      return "hour";
    case Minute:
      return "minute";
    case Second:
      return "second";
    case Microsecond:
      return "microsecond";
    }
  daisy_notreached ();
}

symbol
Time::component_documentation (component_t c)
{
  switch (c)
    {
    case Year:
      return "Year";
    case Month:
      return "Month";
    case Week:
      return "Week number (first Thursday is in week 1)";
    case Yday:
      return "Julian day";
    case Mday:
      return "Day in month";
    case Wday:
      return "Weekday (Sunday = 7)";
    case Hour:
      return "Hour";
    case Minute:
      return "Minute";
    case Second:
      return "Second";
    case Microsecond:
      return "Microsecond";
    }
  daisy_notreached ();
}

// Simulate. 

void 
Time::output (Log& log) const
{ 
  output_value (year (), "year", log);
  output_value (month (), "month", log);
  output_value (week (), "week", log);
  output_value (yday (), "yday", log);
  output_value (mday (), "mday", log);
  output_value (wday (), "wday", log);
  output_value (hour (), "hour", log);
  output_value (minute (), "minute", log);
  output_value (second (), "second", log);
  output_value (microsecond (), "microsecond", log);
}

int
Time::tick_generic (const int amount, const int limit, 
                    void (Time::*next) (int), const int old)
{
  const long int sum = old + amount;
  if (sum >= limit)
    {
      int div = sum / limit;
      int mod = sum % limit;
      daisy_assert (sum == div * limit + mod);
      (this->*next) (div);
      return mod;
    }
  if (sum < 0)
    {
      // My hopeless way to implement protable div/mod for negative integers.
      int div = -((-sum) / limit + 1);
      int mod = limit - (-sum) % limit;
      if (mod == limit)
        {
          div += 1;
          mod = 0;
        }
      daisy_assert (sum == div * limit + mod);
      (this->*next) (div);
      return mod;
      
    }
  else
    return sum;
}

void
Time::tick_microsecond (int microseconds)
{ 
  impl->microsecond = tick_generic (microseconds, 1000000,
                                    &Time::tick_second, impl->microsecond); 
}

void
Time::tick_second (int seconds)
{ impl->second 
    = tick_generic (seconds, 60, &Time::tick_minute, impl->second); }

void
Time::tick_minute (int minutes)
{ impl->minute
    = tick_generic (minutes, 60, &Time::tick_hour, impl->minute); }

void
Time::tick_hour (int hours)
{ impl->hour
    = tick_generic (hours, 24, &Time::tick_day, impl->hour); }

void 
Time::tick_day (int days)
{
  for (; days > 0; --days)
    switch (impl->yday)
      {
      case 365:
	if (leap (impl->year))
	  {
	    impl->yday++;
	    return;
	  }
	/* fallthrough */
      case 366:
	impl->yday = 1;
	impl->year++;
	break;
      default:
	impl->yday++;
      }
  for (; days < 0; ++days)
    if (impl->yday > 1)
      impl->yday--;
    else
      {
	impl->year--;
	if (leap (impl->year))
	  impl->yday = 366;
	else
	  impl->yday = 365;
      }
}

void
Time::tick_year (int years)
{ impl->year += static_cast<short> (years); }

// @ Convert.

symbol
Time::month_name (int month)
{
  daisy_assert (month >= 1 && month <= 12);
  return Implementation::mname[month];
}

symbol
Time::wday_name (int wday)
{
  daisy_assert (wday >= 1 && wday <= 7);
  return Implementation::wname[wday];
}

int
Time::month_number (symbol name)
{
  for (int month = 1; month <= 12; month++)
    if (Implementation::mname[month] == name)
      return month;
  daisy_notreached ();
}

int
Time::wday_number (symbol name)
{
  for (int wday = 1; wday <= 7; wday++)
    if (Implementation::wname[wday] == name)
      return wday;
  daisy_notreached ();
}

int
Time::mday2yday (int year, int month, int mday)
{
  bool ly = leap (year) && (month > 2);
  return Implementation::mlen[month] + mday + ly;
}

int
Time::yday2mday (int year, int yday)
{
  int month = yday2month (year, yday);
  return yday - Implementation::mlen[month] - (leap (year) && month > 2);
}

int 
Time::yday2wday (int year, int yday) // 1 = monday, 7 = sunday.
{
  const int first_thursday 
    = 7 - (1 + (year-1600) + (year-1597) / 4 
	   - (year-1501) / 100 + (year-1201) % 400) % 7;
  return (7 + yday - first_thursday + 3) % 7 + 1;
  
}

int
Time::yday2month (int year, int yday)
{
  int month;
  bool ly = leap (year);
  for (month = 1;
       Implementation::mlen[month + 1] + (ly && (month >= 2)) < yday;
       month++)
    /* do nothing */;
  return month;
}

int
Time::yday2week (int year, int yday)
{
  static const int thursday = 4;
  const int first_day = yday2wday (year, 1);
  const int number_of_weeks = ((yday - 1) + (first_day - 1)) / 7;
  if (first_day <= thursday)
    return number_of_weeks + 1;
  else
    return number_of_weeks;
}

// Test.

bool
Time::leap (int year)
{
  return (year % 4 == 0) && ((year % 100 != 0) || (year % 400 == 0));
}

int
Time::month_length (int year, int month)
{
  return Implementation::mlen[month + 1] - Implementation::mlen[month] 
    + (month == 2 && leap (year));
}

bool 
Time::valid (int year, int month, int mday, int hour, int minute, int second,
             int microsecond)
{
  if (1 > year || year > 9999)
    return false;
  if (1 > month || month > 12)
    return false;
  if (1 > mday || mday > Time::month_length (year, month))
    return false;
  if (0 > hour || hour > 23)
    return false;
  if (0 > minute || minute > 59)
    return false;
  if (0 > second || second > 59)
    return false;
  if (0 > microsecond || microsecond > 999999)
    return false;

  return true;
}

int
Time::days_between (const Time& first, const Time& last)
{
  // Same years, just use the difference between the year numbers.
  if (first.year () == last.year ())
    return last.yday () - first.yday ();
  
  // Otherwise, we have the number of days covered in the last year ...
  int days = last.yday ();

  // ... plus the number of days remaining in the current year ...
  if (leap (first.year ()))
    days += (366 - first.yday ());
  else
    days += (365 - first.yday ());

  // ... plus the days of the intervening years ...
  for (int year = first.year () + 1; year < last.year (); year++)
    if (leap (year))
      days += 366;
    else
      days += 365;

  // ... and thats it!
  return days;
}

int
Time::hours_between (const Time& first, const Time& last)
{
  return last.hour () - first.hour () + 24 * days_between (first, last);
}

// @ Create.

static bool check_alist (const Metalib&, const Frame& al, Treelog& msg)
{ 
  bool ok = true;

  const int year = al.integer ("year");
  const int month = al.integer ("month");
  const int mday = al.integer ("mday");
  const int hour = al.integer ("hour");
  const int minute = al.integer ("minute");
  const int second = al.integer ("second");
  const int microsecond = al.integer ("microsecond");

  if (!Time::valid (year, month, mday, hour, minute, second, microsecond))
    {
      msg.error ("Invalid date");
      ok = false;
    }
  return ok;
}
void
Time::load_syntax (Frame& frame)
{
  frame.add_check (check_alist);
  frame.declare_integer ("year", Attribute::State, "Current year.");
  frame.set_check ("year", VCheck::valid_year ());
  frame.declare_integer ("month", Attribute::State, "Current month.");
  static VCheck::IRange mm (1, 12);
  frame.set_check ("month", mm);
  frame.declare_integer ("mday", Attribute::State, 
	      "Current day in the month.");
  static VCheck::IRange dd (1, 31);
  frame.set_check ("mday", dd);
  frame.declare_integer ("hour", Attribute::State, "Current hour.");
  static VCheck::IRange hh (0, 23);
  frame.set_check ("hour", hh);
  frame.set ("hour", 0);
  frame.order ("year", "month", "mday", "hour");
  frame.declare_integer ("minute", Attribute::State, "Current minute.");
  static VCheck::IRange ss (0, 59);
  frame.set_check ("minute", ss);
  frame.set ("minute", 0);
  frame.declare_integer ("second", Attribute::State, "Current second.");
  frame.set_check ("second", ss);
  frame.set ("second", 0);
  static VCheck::IRange us (0, 999999);
  frame.declare_integer ("microsecond", Attribute::State, "\
Current microsecond.");
  frame.set_check ("microsecond", us);
  frame.set ("microsecond", 0);
  frame.declare_integer ("week", Attribute::LogOnly, "Current week.");
  frame.declare_integer ("yday", Attribute::LogOnly, "Current Julian day.");
  frame.declare_string ("wday", Attribute::LogOnly, "Current weekday.\n\
Monday is 1, Sunday is 7.");
}

Time::Time (const Block& al)
  : impl (new Implementation (al.integer ("year"), 
                              mday2yday (al.integer ("year"),
                                         al.integer ("month"),
                                         al.integer ("mday")),
                              al.integer ("hour"),
                              al.integer ("minute"),
                              al.integer ("second"),
                              al.integer ("microsecond")))
{ }

Time::Time (const FrameSubmodel& al)
  : impl (new Implementation (al.integer ("year"), 
                              mday2yday (al.integer ("year"),
                                         al.integer ("month"),
                                         al.integer ("mday")),
                              al.integer ("hour"),
                              al.integer ("minute"),
                              al.integer ("second"),
                              al.integer ("microsecond")))
{ }

static DeclareSubmodel 
time_submodel (Time::load_syntax, "Time", "\
Year, month, day and hour, minute, second and microsecond.");

// @ Construct.

const Time& 
Time::null ()
{
  static Time no_time;
  return no_time;
}

Time 
Time::now ()
{
  const std::time_t now_time = std::time (NULL);
  const std::tm now_tm = *std::localtime (&now_time);
  Time time (now_tm.tm_year, now_tm.tm_mon + 1, now_tm.tm_mday, 
             now_tm.tm_hour, now_tm.tm_min, now_tm.tm_sec);
  return time;
}

const Time& 
Time::operator= (const Time& t)
{
  *(this->impl) = *(t.impl);
  return *this;
}

Time::Time (int y, int mo, int md, int h, int mi, int s, int us)
  : impl (new Implementation (y, mday2yday (y, mo, md), h, mi, s, us))
{ 
  daisy_assert (mo > 0 && mo < 13);
  daisy_assert (md > 0 && mo == month ());
  daisy_assert (h >= 0 && h < 24);
  daisy_assert (mi >= 0 && mi < 60);
  daisy_assert (s >= 0 && s < 60);
  daisy_assert (us >= 0 && us < 1000000);
}
    
Time::Time (const Time& t)
  : impl (new Implementation (*t.impl))
{ }

Time::~Time ()
{ }

Time::Time ()
  : impl (new Implementation (99999, 99999, 99999, 99999, 99999, 99999))
{ }

// Operators.

bool 
Time::operator== (const Time& other) const
{ return (impl->year == other.impl->year)
    && (impl->yday == other.impl->yday)
    && (impl->hour == other.impl->hour)
    && (impl->minute == other.impl->minute)
    && (impl->second == other.impl->second)
    && (impl->microsecond == other.impl->microsecond); }

bool
Time::operator!= (const Time& other) const
{ return !(*this == other); }
    
bool
Time::operator<  (const Time& other) const
{ 
  if (impl->year < other.impl->year)
    return true;
  if (impl->year > other.impl->year)
    return false;
  if (impl->yday < other.impl->yday)
    return true;
  if (impl->yday > other.impl->yday)
    return false;
  if (impl->hour < other.impl->hour)
    return true;
  if (impl->hour > other.impl->hour)
    return false;
  if (impl->minute < other.impl->minute)
    return true;
  if (impl->minute > other.impl->minute)
    return false;
  if (impl->second < other.impl->second)
    return true;
  if (impl->second > other.impl->second)
    return false;
  if (impl->microsecond < other.impl->microsecond)
    return true;
#if 0
  if (impl->microsecond > other.impl->microsecond)
    return false;
#endif
  return false;
}

bool
Time::operator<= (const Time& other) const
{ return *this == other || *this < other; }

bool
Time::operator>= (const Time& other) const
{ return !(*this < other); }

bool
Time::operator>  (const Time& other) const
{ return !(*this <= other); }

bool 
Time::between (const Time& from, const Time& to) const
{ 
  daisy_assert (from <= to);
  if (*this  < from)
    return false;
  if (to < *this)
    return false;
  return true;
}

// time.C ends here.
