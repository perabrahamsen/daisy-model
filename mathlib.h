// mathlib.h

#ifndef MATHLIB_H
#define MATHLIB_H

#include <vector.h>

void
tridia (const unsigned int N,
	const vector<double>& a,
	const vector<double>& b, 
	const vector<double>& c,
	const vector<double>& d,
	vector<double>::iterator x);

int abs (int);
double abs (double);

#endif MATHLIB_H
