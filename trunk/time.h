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
#include <vector>
#include <memory>

#ifdef __unix
#define EXPORT /* Nothing */
#elif defined (BUILD_DLL)
/* DLL export */
#define EXPORT __declspec(dllexport)
#else
/* EXE import */
#define EXPORT __declspec(dllimport)
#endif

class AttributeList;
class Syntax;
class Log;
class Block;

class EXPORT Time
{
  // Content.
private:
  struct Implementation;
  const std::auto_ptr<Implementation> impl;
    
  // Extract.
public:
  int year () const;
  int month () const;
  int week () const;
  int yday () const;
  int mday () const;
  int wday () const;		// 1=monday, 7=sunday.
  int hour () const;
  int minute () const;
  int second () const;
  std::string print () const;
  void set_alist (AttributeList& alist) const;

  enum component_t {
    Year, Month, Week, Yday, Mday, Wday, Hour, Minute, Second, 
    First = Year, Last = Second
  };
  int component_value (component_t) const;
  static std::string component_name (component_t);
  static std::string component_documentation (component_t);

  // Simulate. 
  int tick_generic (const int amount, const int limit, 
                    void (Time::*next) (int), const int old);
public:
  void tick_second (int seconds = 1);
  void tick_minute (int minutes = 1);
  void tick_hour (int hours = 1);
  void tick_day (int days = 1);
  void tick_year (int years = 1);
  void output (Log&) const;
  
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
  static bool valid (int year, int month, int mday, 
                     int hour = 0, int minute = 0, int second = 0);
  static int days_between (const Time& first, const Time& last);
  static int hours_between (const Time& first, const Time& last);

  bool operator== (const Time&) const;
  bool operator!= (const Time&) const;
  bool operator<  (const Time&) const;
  bool operator<= (const Time&) const;
  bool operator>= (const Time&) const;
  bool operator>  (const Time&) const;
  bool between (const Time&, const Time&) const;

  // Create.
public:
  static void load_syntax (Syntax&, AttributeList&);
  explicit Time (const AttributeList&);
  explicit Time (Block&);

  // Construct.
public:
  static const Time& null ();
  const Time& operator= (const Time&);
  Time (int year, int month, int mday, int hour,
        int minute = 0, int second = 0);
  Time (const Time&);
  ~Time ();
private:                    
  explicit Time ();
};

#endif // TIME_H
