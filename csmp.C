// csmp.C

#include "csmp.h"
#include "log.h"
#include <vector>
#include <algo.h>
#include <assert.h>

struct CSMP::Implementation
{
  struct pair
  {
    double x;
    double y;
#if 0
    bool operator < (const pair&) const;
    { return (x < pair.x /* || (x == pair.x && y < pair.y) */); }
#endif
    pair (double a, double b) : x (a), y (b) { };
    pair () : x (0.0), y (0.0) { };	// Needed by vector<>
  };
  typedef vector<pair> PairVector;
  PairVector points;
  Implementation (const PairVector& pts) : points (pts) { };
  Implementation () { };
};

double
CSMP::operator() (const double x) const
{
#if 1
  int min = 0;
  int max = impl.points.size () - 1;

  if (x <= impl.points[min].x)
    return impl.points[min].y;
  else if (x >= impl.points[max].x)
    return impl.points[max].y;

  while (true)
    {
      if (max - min == 1)
	return impl.points[min].y 
	  +   (impl.points[max].y - impl.points[min].y)
	/ (impl.points[max].x - impl.points[min].x)
	* (x - impl.points[min].x);
      
      int guess = (max + min) / 2;
      if (impl.points[guess].x < x)
	min = guess;
      else if (impl.points[guess].x > x)
	max = guess;
      else 
	return impl.points[guess].y;
      assert (max > min);
    }
#elif 1
  for (unsigned int i = 0; i < impl.points.size(); i++)
    {
      if (x <= impl.points[i].x)
	{
	  if (i == 0)
	    return impl.points[i].y;
		    
	  return impl.points[i-1].y 
	    +   (impl.points[i].y - impl.points[i-1].y)
	    / (impl.points[i].x - impl.points[i-1].x)
	    * (x - impl.points[i-1].x);
	}
    }
  return impl.points[impl.points.size () - 1].y;
#else
  assert (impl.points.size () > 0);
  if (x <= impl.points[0].x)
    return impl.points[0].y;
  else if (x >= impl.points[impl.points.size () - 1].x)
    return impl.points[impl.points.size () - 1].y;
  else
    {
      const Implementation::pair p(x, 0.0);
      Implementation::PairVector::const_iterator lower 
	= lower_bound (impl.points.begin (), impl.points.end (), p);
      Implementation::PairVector::const_iterator upper
	= upper_bound (impl.points.begin (), impl.points.end (), p);
      assert (upper != impl.points.end ());
      assert (lower != impl.points.end ());
      if (upper == lower)
	return upper->y;
      else
	{
	  assert (upper - lower == 1);
	  assert (upper->x > lower->x);
	  assert (upper->x >= x);
	  assert (x >= lower->x);
	  assert (upper->y >= lower->y);
	  const double r = (lower->y
			    + (upper->y - lower->y) / (upper->x - lower->x)
			    * (x - lower->x));
	  assert (r >= lower->y && r <= upper->y);
	  return r;
	}
    }
#endif
}

// Calculate the inverse function of a CSMP.  
// We ssume that the original CSMP is monotonously increasing.

CSMP
CSMP::inverse () const
{ 
  CSMP csmp;

  double last = -1.0;
  for (Implementation::PairVector::iterator i = impl.points.begin ();
       i != impl.points.end ();
       i++)
    if (last != i->y)
      {
	assert (last <= i->y);
	csmp.add (i->y, i->x);
	last = i->y;
      }
  return csmp;
}

// Integrate a CSMP by pretending that line piece is really a constant
// with the mean value of the line piece.  That way, the result can be
// described as a CSMP itself.  
// We assume that the first point of the CSMP is (0.0, 0.0).

CSMP 
CSMP::integrate_stupidly () const
{
  CSMP csmp;
  
  double sum = 0.0;
  Implementation::pair last (0.0, 0.0);
  
  csmp.add (0.0, 0.0);
  for (Implementation::PairVector::iterator i = impl.points.begin ();
       i != impl.points.end ();
       i++)
    {
      if (i->x > last.x)
	{
	  sum += (last.y + i->y) * 0.5 * (i->x - last.x);
	  csmp.add (i->x, sum);
	}
      else
	{
	  // The CSMP is discontinues at this point.
	  assert (i->x == last.x);
	}
      last = *i;
    }
  return csmp;
}

double 
CSMP::find (const vector<double>& x, const vector<double>& y, double value)
{
  assert (x.size () == y.size ());
    
  for (unsigned int i = 0; i < x.size(); i++)
    {
      if (value < x[i])
	{
	  if (i == 0)
	    return y[i];
		    
	  return y[i-1] 
	    +   (y[i] - y[i-1])
	    / (x[i] - x[i-1])
	    * (value - x[i-1]);
	}
    }
  return y[y.size () - 1];
}

void 
CSMP::output (Log& log) const
{
  for (Implementation::PairVector::iterator i = impl.points.begin ();
       i != impl.points.end ();
       i++)
    {
      log.output_point (i->x, i->y);
    }
}

void 
CSMP::add (double x, double y)
{
  assert (impl.points.size () == 0 || x >= impl.points[impl.points.size () - 1].x);
  impl.points.push_back (Implementation::pair(x, y));
}

// Add two CSMPs a and b giving c so that c (x) == a (x) + b (x).

void
CSMP::operator += (const CSMP& csmp)
{
  if (impl.points.size () == 0)
    {
      impl.points = csmp.impl.points;
      return;
    }
  if (csmp.impl.points.size () == 0)
    return;

  // Loop variables.
  Implementation::PairVector::iterator i = impl.points.begin ();
  Implementation::PairVector::iterator i_end = impl.points.end ();
  Implementation::PairVector::iterator j = csmp.impl.points.begin ();
  Implementation::PairVector::iterator j_end = csmp.impl.points.end ();

  // This is out first point on the new CSMP.
  Implementation::pair last (min (i->x, j->x), i->y + j->y);

  // We start with a 
  double di = 0.0;		// Slope between present and next i.
  double dj = 0.0;		// Slope between present and next j.

  CSMP out;

  // We now add points by looping over both CSMPs simultaneously.
  while (i != i_end || j != j_end)
    {
      // Add points from i until we pass the next point from j.
      while (i != i_end && i->x <= j->x)
	{
	  // Calculate new point by using the slope from both CSMPs.
	  last.y = last.y + (i->x - last.x) * (di + dj);
	  last.x = i->x;
	  out.add (last.x, last.y);

	  // Calculate new i slope.
	  if (i + 1 == i_end)
	    // Zero slope after last point.
	    di = 0.0;
	  else if (i[0].x < i[1].x)
	    di = (i[1].y - i[0].y) / (i[1].x - i[0].x);
	  else
	    // It doesn't matter what the slope is when the distanse is zero.
	    assert (i[0].x == i[1].x);
	  
	  i++;
	}
      // Add points from j until we pass the next point from i.
      while (j != j_end && j->x <= i->x)
	{
	  // Calculate new point by using the slope from both CSMPs.
	  last.y = last.y + (j->x - last.x) * (dj + di);
	  last.x = j->x;
	  out.add (last.x, last.y);

	  // Calculate new j slope.
	  if (j + 1 == j_end)
	    // Zero slope after last point.
	    dj = 0.0;
	  else if (j[0].x < j[1].x)
	    dj = (j[1].y - j[0].y) / (j[1].x - j[0].x);
	  else
	    // It doesn't matter what the slope is when the distanse is zero.
	    assert (j[0].x == j[1].x);
	  
	  j++;
	}
    }
  impl.points = out.impl.points; 
}

void 
CSMP::operator = (const CSMP& csmp)
{
  impl.points = csmp.impl.points;
}


CSMP::CSMP (const CSMP& csmp)
  : impl (*new Implementation (csmp.impl.points))
{}

CSMP::CSMP ()
  : impl (*new Implementation)
{}

CSMP::~CSMP ()
{
  delete &impl;
}

