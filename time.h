// time.h -- tick tick

#ifndef TIME_H
#define TIME_H

#include <string>
using namespace std;

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

  // Convert.
public:
  static string month_name (int month);
  static string wday_name (int wday);
  static int month_number (string name);
  static int wday_number (string day);
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

  // Construct.
public:
  const Time& operator= (const Time&);
  Time (int year, int month, int mday, int hour);
  Time (const Time&);
  ~Time ();
};

#endif // TIME_H
