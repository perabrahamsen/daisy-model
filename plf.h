// plf.h
// 
// A PLF is a piecewise liniear function defined by a number of points.  
//
// The points must be added by increasing x value, and the function is
// defined by drawing a straight line from each added point to the next.

#ifndef PLF_H
#define PLF_H

#include <vector>
using namespace std;

class Log;

class PLF
{
  // Content.
  struct Implementation;
  Implementation& impl;
public:
  // Use.
  double operator ()(double x) const;
  PLF inverse () const;
  double first_interesting () const;
  double last_interesting () const;
  double max_at () const;
  double integrate (double from, double to) const;
  PLF integrate_stupidly () const;
  void offset (double offset);	// Add 'offset' to all y values.

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
  bool operator == (const PLF&) const;

  // Create and Destroy.
  void clear ();
  void add (double, double);
  void operator += (const PLF&);
  void operator = (const PLF&);
  PLF (const PLF&);
  PLF ();
  ~PLF ();
};

#endif // PLF_H
