// csmp.C

#include "csmp.h"
#include "log.h"
#include <vector>
#include <algo.h>
#include <assert.h>

struct CSMP::Implementation
{
  vector<double> x;
  vector<double> y;

  double operator () (const double pos) const;
  CSMP inverse () const;
  CSMP integrate_stupidly () const;
  void operator = (CSMP::Implementation& impl)
  { 
    x = impl.x;
    y = impl.y;
  }
  Implementation () { };
  Implementation (CSMP::Implementation& impl) : x (impl.x), y (impl.y) { };
};

double 
CSMP::Implementation::operator () (const double pos) const
{
  int min = 0;
  int max = x.size () - 1;

  if (pos <= x[min])
    return y[min];
  else if (pos >= x[max])
    return y[max];

  while (true)
    {
      if (max - min == 1)
	return y[min] 
	  + (y[max] - y[min]) / (x[max] - x[min]) * (pos - x[min]);

      int guess = (max + min) / 2;

      if (x[guess] < pos)
	min = guess;
      else if (x[guess] > pos)
	max = guess;
      else 
	return y[guess];
      assert (max > min);
    }
}

// Calculate the inverse function of a CSMP.  
// We assume that the original CSMP is monotonously increasing.
CSMP 
CSMP::Implementation::inverse () const
{
  const int size = x.size ();
  CSMP csmp;

  double last = -1.0;
  for (int i = 0; i < size; i++)
    if (last != y[i])
      {
	assert (last <= y[i]);
	csmp.add (y[i], x[i]);
	last = y[i];
      }
  return csmp;
}

// Integrate a CSMP by pretending that line piece is really a constant
// with the mean value of the line piece.  That way, the result can be
// described as a CSMP itself.  
// We assume that the first point of the CSMP is (0.0, 0.0).
CSMP 
CSMP::Implementation::integrate_stupidly () const
{
  CSMP csmp;
  
  const int size = x.size ();
  double sum = 0.0;
  double last_x = 0.0;
  double last_y = 0.0;
  
  csmp.add (0.0, 0.0);
  for (int i = 0; i < size; i++)
    {
      if (x[i] > last_x)
	{
	  sum += (last_y + y[i]) * 0.5 * (x[i] - last_x);
	  csmp.add (x[i], sum);
	}
      else
	{
	  // The CSMP is discontinues at this point.
	  assert (x[i] == last_x);
	}
      last_x = x[i];
      last_y = y[i];
    }
  return csmp;
}

double
CSMP::operator () (const double x) const
{
  return impl (x);
}

CSMP
CSMP::inverse () const
{ 
  return impl.inverse ();
}

CSMP 
CSMP::integrate_stupidly () const
{
  return impl.integrate_stupidly ();
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
  const int size = impl.x.size ();

  for (int i = 0; i < size; i++)
    {
      log.output_point (impl.x[i], impl.y[i]);
    }
}

void 
CSMP::add (double x, double y)
{
  const int size = impl.x.size ();
  assert (size == 0 || x >= impl.x[size - 1]);
  impl.x.push_back (x);
  impl.y.push_back (y);
}

// Add two CSMPs a and b giving c so that c (x) == a (x) + b (x).

void
CSMP::operator += (const CSMP& csmp)
{
  if (impl.x.size () == 0)
    {
      impl.x = csmp.impl.x;
      impl.y = csmp.impl.y;
      return;
    }
  if (csmp.impl.x.size () == 0)
    return;
  
  // This does not work yet.
  assert (false);

#if 0
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
#endif
}

void 
CSMP::operator = (const CSMP& csmp)
{
  impl = csmp.impl;
}


CSMP::CSMP (const CSMP& csmp)
  : impl (*new Implementation (csmp.impl))
{ }

CSMP::CSMP ()
  : impl (*new Implementation ())
{ }

CSMP::~CSMP ()
{
  delete &impl;
}

