// @ time.C

#include "time.h"

// @ Content.

struct Time::Implementation
{
  static int mlen[];
  static string mname[];
  short year;
  short yday;
  char hour;  
  Implementation (short, short, char);
  Implementation (const Implementation&);
};

int Time::Implementation::mlen[] =
{ -999, 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365 };

string Time::Implementation::mname[] =
{ "January", "February", "March", "April", "May", "June",
  "July", "August", "September", "October", "November" , "December" };

Time::Implementation::Implementation (short y, short d, char h)
  : year (y), yday (d), hour (h)
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
  // BUG: Weekdays are unimplemented.
  assert (false);
  return -1; // SHUT UP.
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
  // BUG: Weekdays are unimplemented.
  assert (false);
  return -1; // SHUT UP.
}

int
Time::hour () const
{
  return impl.hour;
}

// @ Simulate. 

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
}

// @ Convert.

string
Time::month_name (int month)
{
  return Implementation::mname[month];
}

string
Time::wday_name (int /* yday */)
{
  // BUG: Weekdays are unimplemented.
  assert (false);
  return "yesterday"; // SHUT UP.
}

int
Time::month_number (string name)
{
  for (int month = 1; month < 13; month++)
    if (Implementation::mname[month] == name)
      return month;
  // Bug: Should probably throw an exception here.
  return -1;
}

int
Time::wday_number (string /* name */)
{
  // BUG: Weekdays are unimplemented.
  assert (false);
  return -42;
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
Time::yday2month (int year, int yday)
{
  int month;
  bool ly = leap (year);
  for (month = 1;
       Implementation::mlen[month + 1] + (ly && (month >= 2)) < yday;
       month++)
    ;
  return month;
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
  return Implementation::mlen[month + 1] + Implementation::mlen[month] 
    + (month == 2 && leap (year));
}

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
  assert (m > 0 && m < 13);
  assert (md > 0 && m == month ());
  assert (h >= 0 && h < 24);
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
operator== (const Time& a, const Time& b)
{
  return (a.impl.year == b.impl.year)
    && (a.impl.yday == b.impl.yday)
    && (a.impl.hour == b.impl.hour);
}

bool
operator!= (const Time& a, const Time& b)
{
  return !(a == b);
}
    
bool
operator<  (const Time& a, const Time& b)
{
  if (a.impl.year < b.impl.year)
    return true;
  if (a.impl.year > b.impl.year)
    return false;
  if (a.impl.yday < b.impl.yday)
    return true;
  if (a.impl.yday > b.impl.yday)
    return false;
  if (a.impl.hour < b.impl.hour)
    return true;
  return false;

}

bool
operator<= (const Time& a, const Time& b)
{
  return a == b || a < b;
}

bool
operator>= (const Time& a, const Time& b)
{
  return !(a < b);
}

bool
operator>  (const Time& a, const Time& b)
{
  return !(a <= b);
}
