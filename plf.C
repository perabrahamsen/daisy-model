// plf.C

#include "plf.h"
#include <vector>
#include <list>
#include <assert.h>

struct PLF::Implementation
{
  vector<double> x;
  vector<double> y;

  double operator () (const double pos) const;
  PLF inverse () const;
  double integrate (const double from, const double to) const;
  PLF integrate_stupidly () const;
  void clear () 
    { 
      x.erase (x.begin (), x.end ());
      y.erase (y.begin (), y.end ());
    }
  void operator = (PLF::Implementation& impl)
  { 
    x = impl.x;
    y = impl.y;
  }
  Implementation () { };
  Implementation (PLF::Implementation& impl) : x (impl.x), y (impl.y) { };
};

double 
PLF::Implementation::operator () (const double pos) const
{
  assert (x.size () > 0);
  assert (x.size () == y.size ());

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

// Calculate the inverse function of a PLF.  
// We assume that the original PLF is monotonously increasing.
PLF 
PLF::Implementation::inverse () const
{
  const int size = x.size ();
  PLF plf;

  double last = y[0] - 1.0;
  for (int i = 0; i < size; i++)
    {
      assert (last <= y[i]);
      plf.add (y[i], x[i]);
      last = y[i];
    }
  return plf;
}

double
PLF::Implementation::integrate (const double from, const double to) const
{
  assert (from <= to);
  const int size = x.size ();

  // First point.
  double last_y = operator ()(from);
  double last_x = from;

  // Intermediate points.
  double total = 0.0;
  for (unsigned int i = 0; i < size; i++)
    {
      if (x[i] < last_x)
	continue;
      if (x[i] >= to)
	break;
      total += (x[i] - last_x) * (y[i] + last_y) / 2.0;
      last_x = x[i];
      last_y = y[i];
    }
  assert (last_x <= to);
  
  // Last point.
  total += (to - last_x) * (operator ()(to) + last_y) / 2.0;

  return total;
}

// Integrate a PLF by pretending that line piece is really a constant
// with the mean value of the line piece.  That way, the result can be
// described as a PLF itself.  
// We assume that the first point of the PLF is (0.0, 0.0).
PLF 
PLF::Implementation::integrate_stupidly () const
{
  PLF plf;
  const unsigned int intervals = 10;
  const unsigned int size = x.size ();
  double sum = 0.0;
  double last_x = 0.0;
  double last_y = 0.0;
  
  plf.add (0.0, 0.0);
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
	      plf.add (x, sum + (last_y + y) * 0.5 * (x - last_x));
	    }
	  // Add final point.
	  sum += (last_y + y[i]) * 0.5 * (x[i] - last_x);
	  plf.add (x[i], sum);
	}
      else
	{
	  // The PLF is discontinues at this point.
	  assert (x[i] == last_x);
	}
      last_x = x[i];
      last_y = y[i];
    }
  return plf;
}

double
PLF::operator () (const double x) const
{
  return impl (x);
}

PLF
PLF::inverse () const
{ 
  return impl.inverse ();
}

double
PLF::integrate (const double from, const double to) const
{
  return impl.integrate (from, to);
}

PLF 
PLF::integrate_stupidly () const
{
  return impl.integrate_stupidly ();
}

void
PLF::offset (double offset)	// Add `offset' to all y values.
{
  for (unsigned int i = 0; i < impl.y.size(); i++)
    impl.y[i] += offset;
}

double 
PLF::find (const vector<double>& x, const vector<double>& y, double value)
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
PLF::size () const
{ return impl.x.size (); }

double 
PLF::x (unsigned int i) const
{ return impl.x[i]; }

double 
PLF::y (unsigned int i) const
{ return impl.y[i]; }

bool 
PLF::operator == (const PLF& other) const
{ return impl.x == other.impl.x && impl.y == other.impl.y; }

void 
PLF::add (double x, double y)
{
  const int size = impl.x.size ();
  assert (size == 0 || x >= impl.x[size - 1]);
  impl.x.push_back (x);
  impl.y.push_back (y);
}

// Add two PLFs a and b giving c so that c (x) == a (x) + b (x).

void
PLF::operator += (const PLF& plf)
{
  // We can't calculate with empty PLFs.  They don't have a defined
  // value in any points.  Check for those first.
  if (impl.x.size () == 0)
    {
      // If this is empty, use the other.
      impl = plf.impl;
      return;
    }
  if (plf.impl.x.size () == 0)
    {
      // If theother is empty, use this one.
      return;
    }

  // I want a vector with all the x points.
  // First I create two lists containing the x points from each plf.
  list<double> combined (impl.x.begin (), impl.x.end ());
  list<double> other (plf.impl.x.begin (), plf.impl.x.end ());
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

  // I then add the points to a temporary PLF.
  PLF result;

  for (unsigned int i = 0; i < points.size (); i++)
    {
      //  The y value of the combined plf is at all points the
      // combined y value of the individual plfs.  And the function
      // is piecewise linear between the x points.
      const double x = points[i];
      const double y = impl (x) + plf (x);
      result.add (x, y);
    }

  // We now store the result in this plf.
  impl = result.impl;
}

void
PLF::clear ()
{ impl.clear (); }

void 
PLF::operator = (const PLF& plf)
{ impl = plf.impl; }


PLF::PLF (const PLF& plf)
  : impl (*new Implementation (plf.impl))
{ }

PLF::PLF ()
  : impl (*new Implementation ())
{ }

PLF::~PLF ()
{
  delete &impl;
}

