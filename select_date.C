// select_date.C --- Select a state variable.
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


#include "select.h"
#include "time.h"

struct SelectDate : public Select
{
  // Content.
  Time value;			// Date.

  // Output routines.
  void output_time (const string& name, const Time& time)
    { 
      if (!is_active ())
	return;

      if (!valid (name))
	return;

      value = time;
      count++;
    }

  // Print result at end of time step.
  void done (Destination& dest)
    {
      if (count == 0)
	dest.missing (tag);
      else 
	dest.add (tag, extract_date_component ());

      if (!accumulate)
	count = 0;
    }
  virtual int extract_date_component () const = 0;

  // Create and Destroy.
  SelectDate (const AttributeList& al)
    : Select (al),
      value (1, 1, 1, 1)
    { }
};

struct SelectYear : public SelectDate
{
  int extract_date_component () const
    { return value.year (); }

  SelectYear (const AttributeList& al)
    : SelectDate (al)
    { }
};

struct SelectMonth : public SelectDate
{
  int extract_date_component () const
    { return value.month (); }

  SelectMonth (const AttributeList& al)
    : SelectDate (al)
    { }
};

struct SelectMDay : public SelectDate
{
  int extract_date_component () const
    { return value.mday (); }

  SelectMDay (const AttributeList& al)
    : SelectDate (al)
    { }
};

struct SelectHour : public SelectDate
{
  int extract_date_component () const
    { return value.hour (); }

  SelectHour (const AttributeList& al)
    : SelectDate (al)
    { }
};

struct SelectWeek : public SelectDate
{
  int extract_date_component () const
    { return value.week (); }

  SelectWeek (const AttributeList& al)
    : SelectDate (al)
    { }
};

struct SelectYDay : public SelectDate
{
  int extract_date_component () const
    { return value.yday (); }

  SelectYDay (const AttributeList& al)
    : SelectDate (al)
    { }
};

struct SelectWDay : public SelectDate
{
  int extract_date_component () const
    { return value.wday (); }

  SelectWDay (const AttributeList& al)
    : SelectDate (al)
    { }
};

static struct SelectDateSyntax
{
  static Select& make_year (const AttributeList& al)
    { return *new SelectYear (al); }
  static Select& make_month (const AttributeList& al)
    { return *new SelectMonth (al); }
  static Select& make_mday (const AttributeList& al)
    { return *new SelectMDay (al); }
  static Select& make_hour (const AttributeList& al)
    { return *new SelectHour (al); }
  static Select& make_week (const AttributeList& al)
    { return *new SelectWeek (al); }
  static Select& make_yday (const AttributeList& al)
    { return *new SelectYDay (al); }
  static Select& make_wday (const AttributeList& al)
    { return *new SelectWDay (al); }

  SelectDateSyntax ()
    { 
      Syntax& syntax = *new Syntax ();
      AttributeList alist;
      Select::load_syntax (syntax, alist);

      AttributeList& alist_year = *new AttributeList (alist);
      alist_year.add ("description", "Extract specified year.");
      alist_year.add ("tag", "year");

      AttributeList& alist_month = *new AttributeList (alist);
      alist_month.add ("description", "Extract specified month.");
      alist_month.add ("tag", "month");

      AttributeList& alist_mday = *new AttributeList (alist);
      alist_mday.add ("description", "Extract specified day in the month.");
      alist_mday.add ("tag", "mday");

      AttributeList& alist_hour = *new AttributeList (alist);
      alist_hour.add ("description", "Extract specified hour (0-23).");
      alist_hour.add ("tag", "hour");

      AttributeList& alist_yday = *new AttributeList (alist);
      alist_yday.add ("description", "Extract specified day in the year.");
      alist_yday.add ("tag", "yday");

#if 0
      AttributeList& alist_week = *new AttributeList (alist);
      alist_week.add ("description", "Extract specified week number.");
      alist_week.add ("tag", "week");

      AttributeList& alist_wday = *new AttributeList (alist);
      alist_wday.add ("description", "Extract specified day in the week.");
      alist_wday.add ("tag", "wday");
#endif

      Librarian<Select>::add_type ("year", alist_year, syntax, &make_year);
      Librarian<Select>::add_type ("month", alist_month, syntax, &make_month);
      Librarian<Select>::add_type ("mday", alist_mday, syntax, &make_mday);
      Librarian<Select>::add_type ("hour", alist_hour, syntax, &make_hour);
      Librarian<Select>::add_type ("yday", alist_yday, syntax, &make_yday);
#if 0 
      Librarian<Select>::add_type ("week", alist_week, syntax, &make_week);
      Librarian<Select>::add_type ("wday", alist_wday, syntax, &make_wday);
#endif
    }
} Select_syntax;
