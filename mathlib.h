// mathlib.h
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


#ifndef MATHLIB_H
#define MATHLIB_H
#include "assertion.h"
#include <vector>
#define _GLIBCPP_USE_C99 1
#define __USE_ISOC99 1
#include <math.h>

#ifdef _MSC_VER
template <class T>
T min (T a, T b)
{ return (a < b) ? a : b; }

template <class T>
T max (T a, T b)
{ return (a > b) ? a : b; }
#endif // Visual C++

#if !defined (isfinite)
inline bool isfinite (double x)
{ return x <= 0.0 || x >= 0.0; }
#endif

#if !defined (isnormal)
inline bool isnormal (double x)
{ return isfinite (x) && x != 0.0; }
#endif

#ifndef pow
#define pow(x, y) safe_pow (x, y, __FILE__, __LINE__)
#endif //!pow
#ifndef sqrt
#define sqrt(x) safe_sqrt (x, __FILE__, __LINE__)
#endif //!sqrt
#ifndef log
#define log(x) safe_log (x, __FILE__, __LINE__)
#endif //!log
#ifndef acos
#define acos(x) safe_acos (x, __FILE__, __LINE__)
#endif //!acos
#ifndef asin
#define asin(x) safe_asin (x, __FILE__, __LINE__)
#endif //!asin

inline double safe_pow (double x, double y, const char* file, int line)
{
  if (x >= 0)
    return (pow)(x, y);
  Assertion::failure (file, line, "pow", "x >= 0");
}

inline double safe_pow (double x, int y, const char*, int)
{ return (pow)(x, y); }

inline double safe_sqrt (double x, const char* file, int line)
{
  if (x >= 0)
    return (sqrt)(x);
  Assertion::failure (file, line, "sqrt", "x >= 0");
}

inline double safe_log (double x, const char* file, int line)
{
  if (x > 0)
    return (log)(x);
  Assertion::failure (file, line, "log", "x > 0");
}

inline double safe_acos (double x, const char* file, int line)
{
  if (x >= -1 && x <= 1)
    return (acos)(x);
  Assertion::failure (file, line, "acos", "x >= -1 && x <= 1");
}

inline double safe_asin (double x, const char* file, int line)
{
  if (x >= -1 && x <= 1)
    return (asin)(x);
  Assertion::failure (file, line, "asin", "x >= -1 && x <= 1");
}

#ifndef M_LN2
#define	M_LN2		0.69314718055994530942
#endif

#ifndef M_PI
#define	M_PI		3.14159265358979323846
#endif

#ifndef M_PI_2
#define	M_PI_2		1.57079632679489661923
#endif

void
tridia (int from,
	unsigned int size,
	const std::vector<double>& a,
	const std::vector<double>& b, 
	const std::vector<double>& c,
	const std::vector<double>& d,
	std::vector<double>::iterator x);

double
single_positive_root_of_cubic_equation 
(double a, double b, double c, double d);

double
single_positive_root_of_square_equation 
(double a, double b, double c);

extern bool approximate (double a, double b, double noise = 0.0001);

inline double bound (double a, double x, double b)
{
  daisy_assert (a <= b);
  if (x < a)
    return a;
  if (x > b)
    return b;
  return x;
}

inline void set_bound (double a, double& x, double b)
{
  daisy_assert (a <= b);
  if (x < a)
    x = a;
  else if (x > b)
    x = b;
}

inline int double2int (double x)
{ return static_cast<int> (x); }

inline double int2double (int x)
{ return x; }

inline char int2char (int x)
{ return static_cast<char> (x); }

inline double pF2h (double pF)
{ 
  return -pow (10, pF);
}

inline double h2pF (double h)
{
  return log10 (-h);
}

extern double halftime_to_rate (double halftime);
extern double rate_to_halftime (double rate);

#endif // MATHLIB_H
