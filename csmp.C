// csmp.C

#include "csmp.h"
#include <vector>
#include <list>
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
  const unsigned int intervals = 10;
  const unsigned int size = x.size ();
  double sum = 0.0;
  double last_x = 0.0;
  double last_y = 0.0;
  
  csmp.add (0.0, 0.0);
  for (unsigned int i = 0; i < size; i++)
    {
      if (x[i] > last_x)
	{
	  // Add intervals-1 intermediate points.
	  const double dx = (x[i] - last_x) / intervals;
	  for (unsigned int j = 1; j < intervals; j++)
	    {
	      const double x = last_x + j * dx;
	      const double y = this->operator ()(x);
	      csmp.add (x, sum + (last_y + y) * 0.5 * (x - last_x));
	    }
	  // Add final point.
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

unsigned int 
CSMP::size () const
{ return impl.x.size (); }

double 
CSMP::x (unsigned int i) const
{ return impl.x[i]; }

double 
CSMP::y (unsigned int i) const
{ return impl.y[i]; }

bool 
CSMP::operator == (const CSMP& other) const
{ return impl.x == other.impl.x && impl.y == other.impl.y; }

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
  // We can't calculate with empty CSMPs.  They don't have a defined
  // value in any points.  Check for those first.
  if (impl.x.size () == 0)
    {
      // If this is empty, use the other.
      impl = csmp.impl;
      return;
    }
  if (csmp.impl.x.size () == 0)
    {
      // If theother is empty, use this one.
      return;
    }

  // I want a vector with all the x points.
  // First I create two lists containing the x points from each csmp.
  list<double> combined (impl.x.begin (), impl.x.end ());
  list<double> other (csmp.impl.x.begin (), csmp.impl.x.end ());
  // Then I merge them and remove duplicates.
  combined.merge (other);
  combined.unique ();
  // Finally, I convert it to a vector.
#ifdef HAS_TEMPLATE_MEMBERS
  vector<double> points (combined.begin (), combined.end ());
#else
  vector<double> points;
  for (list<double>::iterator i = combined.begin ();
       i != combined.end ();
       i++)
    points.push_back (*i);
#endif

  // I then add the points to a temporary CSMP.
  CSMP result;

  for (unsigned int i = 0; i < points.size (); i++)
    {
      //  The y value of the combined csmp is at all points the
      // combined y value of the individual csmps.  And the function
      // is piecewise linear between the x points.
      const double x = points[i];
      const double y = impl (x) + csmp (x);
      result.add (x, y);
    }

  // We now store the result in this csmp.
  impl = result.impl;
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

