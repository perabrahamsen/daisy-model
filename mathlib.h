// mathlib.h

#ifndef MATHLIB_H
#define MATHLIB_H

#include "common.h"
#include <vector>
#include <math.h>

#ifdef VISUALCPP
template <class T>
T min (T a, T b)
{ return (a < b) ? a : b; }
#endif // VISUALCPP

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
	const vector<double>& a,
	const vector<double>& b, 
	const vector<double>& c,
	const vector<double>& d,
	vector<double>::iterator x);

double
single_positive_root_of_cubic_equation 
(double a, double b, double c, double d);

double
single_positive_root_of_square_equation 
(double a, double b, double c);

extern bool approximate (double a, double b, double noise = 0.0001);

inline double bound (double a, double x, double b)
{
  assert (a <= b);
  if (x < a)
    return a;
  if (x > b)
    return b;
  return x;
}

inline void set_bound (double a, double& x, double b)
{
  assert (a <= b);
  if (x < a)
    x = a;
  else if (x > b)
    x = b;
}

inline double pF2h (double pF)
{ 
  return -pow (10, pF);
}

inline double h2pF (double h)
{
  return log10 (-h);
}
#endif // MATHLIB_H
