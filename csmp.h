// csmp.h
// 
// A CSMP is a function defined by a number of points.  
//
// The points must be added by increasing x value, and the function is
// defined by drawing a straight line from each added point to the next.

#ifndef CSMP_H
#define CSMP_H

#include "common.h"
#include <vector>

struct Log;
struct Filter;

class CSMP
{
  // Content.
  struct Implementation;
  Implementation& impl;
public:
  // Use.
  double operator ()(double x) const;
  CSMP inverse () const;
  CSMP integrate_stupidly () const;

  static double find (const vector<double>& x, const vector<double>& y,
		      double value);

  void output (Log&) const;

  // Create and Destroy.
  void add (double, double);
  void operator += (const CSMP&);
  void operator = (const CSMP&);
  CSMP (const CSMP&);
  CSMP ();
  ~CSMP ();
};

#endif CSMP_H
