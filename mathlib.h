// mathlib.h

#ifndef MATHLIB_H
#define MATHLIB_H

#include "common.h"
#include <vector>
#include <math.h>

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

extern bool approximate (double a, double b, double noise = 0.0001);

inline double pF2h (double pF)
{ 
  return -pow (10, pF);
}

inline double h2pF (double h)
{
  return log10 (-h);
}
#endif MATHLIB_H
