// csmp.h
// 
// A CSMP is a function defined by a number of points.  
//
// The points must be added by increasing x value, and the function is
// defined by drawing a straight line from each added point to the next.

#ifndef CSMP_H
#define CSMP_H

#include <vector>
using namespace std;

struct Log;

class CSMP
{
  // Content.
  struct Implementation;
  Implementation& impl;
public:
  // Use.
  double operator ()(double x) const;
  CSMP inverse () const;
  double integrate (double from, double to) const;
  CSMP integrate_stupidly () const;
  void offset (double offset);	// Add `offset' to all y values.

  // Utilities.
  static double find (const vector<double>& x, const vector<double>& y,
		      double value);

  // Simulation.
  void output (Log&) const;

  // Print
  unsigned int size () const;
  double x (unsigned int i) const;
  double y (unsigned int i) const;

  // Compare.
  bool operator == (const CSMP&) const;

  // Create and Destroy.
  void clear ();
  void add (double, double);
  void operator += (const CSMP&);
  void operator = (const CSMP&);
  CSMP (const CSMP&);
  CSMP ();
  ~CSMP ();
};

#endif CSMP_H
