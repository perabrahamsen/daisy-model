// time.h -- tick tick
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


#ifndef TIME_H
#define TIME_H

#include <string>

class Time
{
  // Content.
private:
  struct Implementation;
  Implementation& impl;
    
  // Extract.
public:
  int year () const;
  int month () const;
  int week () const;
  int yday () const;
  int mday () const;
  int wday () const;		// 0=monday, 6=sunday.
  int hour () const;

  // Simulate. 
public:
  void tick_hour (int hours = 1);
  void tick_day (int days = 1);
  void tick_year (int years = 1);

  // Convert.
public:
  static std::string month_name (int month);
  static std::string wday_name (int wday);
  static int month_number (std::string name);
  static int wday_number (std::string day);
  static int mday2yday (int year, int month, int mday);
  static int yday2mday (int year, int yday);
  static int yday2wday (int year, int yday); // 0=monday, 6=sunday.
  static int yday2month (int year, int yday);
  static int yday2week (int year, int yday);

  // Test.
  static bool leap (int year);
  static int month_length (int year, int month);
  static bool valid (int year, int month, int mday, int hour = 0);
  static int days_between (const Time& first, const Time& last);
  static int hours_between (const Time& first, const Time& last);

  bool operator== (const Time&) const;
  bool operator!= (const Time&) const;
  bool operator<  (const Time&) const;
  bool operator<= (const Time&) const;
  bool operator>= (const Time&) const;
  bool operator>  (const Time&) const;
  bool between (const Time&, const Time&) const;

  // Construct.
public:
  const Time& operator= (const Time&);
  Time (int year, int month, int mday, int hour);
  Time (const Time&);
  ~Time ();
};

#endif // TIME_H
