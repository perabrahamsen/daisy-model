// @ time.C
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


#include "time.h"
#include "assertion.h"
#include "log.h"
#include "syntax.h"
#include "alist.h"
#include "vcheck.h"
#include "submodel.h"
using namespace std;

// @ Content.

struct Time::Implementation
{
  static const int mlen[];
  static const string mname[];
  static const string wname[];
  short year;
  short yday;
  char hour;  
  Implementation (int, int, int);
  Implementation (const Implementation&);
};

const int Time::Implementation::mlen[] =
{ -999, 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365 };

const string Time::Implementation::mname[] =
{ "Error", "January", "February", "March", "April", "May", "June",
  "July", "August", "September", "October", "November" , "December" };

const string Time::Implementation::wname[] =
{ "Error", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", 
  "Saturday", "Sunday" };

Time::Implementation::Implementation (int y, int d, int h)
  : year (short (y)), yday (short (d)), hour (char (h))
{ }

Time::Implementation::Implementation (const Implementation& i)
  : year (i.year), yday (i.yday), hour (i.hour)
{ }

// @ Extract.

int
Time::year () const
{
  return impl.year;
}

int
Time::month () const
{
  return yday2month (impl.year, impl.yday);
}

int
Time::week () const
{
  return yday2week (impl.year, impl.yday);
}

int
Time::yday () const
{
  return impl.yday;
}

int
Time::mday () const
{
  return yday2mday (impl.year, impl.yday);
}

int
Time::wday () const
{
  return yday2wday (impl.year, impl.yday);
}

int
Time::hour () const
{
  return impl.hour;
}

// @ Simulate. 

void 
Time::output (Log& log) const
{ 
  log.output ("year", year ());
  log.output ("month", month ());
  log.output ("week", week ());
  log.output ("yday", yday ());
  log.output ("mday", mday ());
  log.output ("wday", wday ());
  log.output ("hour", hour ());
}

void 
Time::tick_hour (int hours)
{
  for (; hours > 0; --hours)
    if (impl.hour < 23)
      impl.hour++;
    else
      {
	impl.hour = 0;
	tick_day (1);
      }
  for (; hours < 0; ++hours)
    if (impl.hour > 0)
      impl.hour--;
    else
      {
	impl.hour = 23;
	tick_day (-1);
      }
}

void 
Time::tick_day (int days)
{
  for (; days > 0; --days)
    switch (impl.yday)
      {
      case 365:
	if (leap (impl.year))
	  {
	    impl.yday++;
	    return;
	  }
	/* fallthrough */
      case 366:
	impl.yday = 1;
	impl.year++;
	break;
      default:
	impl.yday++;
      }
  for (; days < 0; ++days)
    if (impl.yday > 1)
      impl.yday--;
    else
      {
	impl.year--;
	if (leap (impl.year))
	  impl.yday = 366;
	else
	  impl.yday = 365;
      }
}

void
Time::tick_year (int years)
{ impl.year += static_cast<short> (years); }

// @ Convert.

string
Time::month_name (int month)
{
  daisy_assert (month >= 1 && month <= 12);
  return Implementation::mname[month];
}

string
Time::wday_name (int wday)
{
  daisy_assert (wday >= 1 && wday <= 7);
  return Implementation::wname[wday];
}

int
Time::month_number (string name)
{
  for (int month = 1; month <= 12; month++)
    if (Implementation::mname[month] == name)
      return month;
  throw invalid_argument ("Time::month_number");
}

int
Time::wday_number (string name)
{
  for (int wday = 1; wday <= 7; wday++)
    if (Implementation::wname[wday] == name)
      return wday;
  throw invalid_argument ("Time::wday_number");
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
Time::valid (int year, int month, int mday, int hour)
{
  if (1 > year || year > 9999)
    return false;
  if (1 > month || month > 12)
    return false;
  if (1 > mday || mday > Time::month_length (year, month))
    return false;
  if (0 > hour || hour > 23)
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

static bool check_alist (const AttributeList& al, Treelog& msg)
{ 
  bool ok = true;

  const int year = al.integer ("year");
  const int month = al.integer ("month");
  const int mday = al.integer ("mday");
  const int hour = al.integer ("hour");

  if (!Time::valid (year, month, mday, hour))
    {
      msg.error ("Invalid date");
      ok = false;
    }
  return ok;
}
void
Time::load_syntax (Syntax& syntax, AttributeList& alist)
{
  alist.add ("submodel", "Time");
  alist.add ("description", "Year, month, day and hour.");
  syntax.add_check (check_alist);
  syntax.add ("year", Syntax::Integer, Syntax::State, "Current year.");
  syntax.add_check ("year", VCheck::valid_year ());
  syntax.add ("month", Syntax::Integer, Syntax::State, "Current month.");
  static VCheck::IRange mm (1, 12);
  syntax.add_check ("month", mm);
  syntax.add ("mday", Syntax::Integer, Syntax::State, 
	      "Current day in the month.");
  static VCheck::IRange dd (1, 31);
  syntax.add_check ("mday", dd);
  syntax.add ("hour", Syntax::Integer, Syntax::State, "Current hour.");
  static VCheck::IRange hh (0, 23);
  syntax.add_check ("hour", hh);
  syntax.order ("year", "month", "mday", "hour");
  syntax.add ("week", Syntax::Integer, Syntax::LogOnly, "Current week.");
  syntax.add ("yday", Syntax::Integer, Syntax::LogOnly, "Current Julian day.");
  syntax.add ("wday", Syntax::String, Syntax::LogOnly, "Current weekday.\n\
Monday is 1, Sunday is 7.");
}

Time::Time (const AttributeList& al)
  : impl (*new Implementation (al.integer ("year"), 
			       mday2yday (al.integer ("year"),
					  al.integer ("month"),
					  al.integer ("mday")),
			       al.integer ("hour")))
{ }

static Submodel::Register 
time_submodel ("Time", Time::load_syntax);


// @ Construct.

const Time& 
Time::operator= (const Time& t)
{
  this->impl = t.impl;
  return *this;
}

Time::Time (int y, int m, int md, int h)
  : impl (*new Implementation (y, mday2yday (y, m, md), h))
{ 
  daisy_assert (m > 0 && m < 13);
  daisy_assert (md > 0 && m == month ());
  daisy_assert (h >= 0 && h < 24);
}
    
Time::Time (const Time&t)
  : impl (*new Implementation (t.impl))
{
}

Time::~Time ()
{
  delete &impl;
}

// @ Operators.

bool 
Time::operator== (const Time& other) const
{
  return (impl.year == other.impl.year)
    && (impl.yday == other.impl.yday)
    && (impl.hour == other.impl.hour);
}

bool
Time::operator!= (const Time& other) const
{
  return !(*this == other);
}
    
bool
Time::operator<  (const Time& other) const
{
  if (impl.year < other.impl.year)
    return true;
  if (impl.year > other.impl.year)
    return false;
  if (impl.yday < other.impl.yday)
    return true;
  if (impl.yday > other.impl.yday)
    return false;
  if (impl.hour < other.impl.hour)
    return true;
  return false;

}

bool
Time::operator<= (const Time& other) const
{
  return *this == other || *this < other;
}

bool
Time::operator>= (const Time& other) const
{
  return !(*this < other);
}

bool
Time::operator>  (const Time& other) const
{
  return !(*this <= other);
}

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
