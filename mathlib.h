// mathlib.h

#ifndef MATHLIB_H
#define MATHLIB_H

#include "common.h"
#include <vector>
#include <math.h>

extern double abs (double);

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

#endif MATHLIB_H
