// plf.C
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
//
// This file is part of Daisy.
// 
// Daisy is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser Public License as published by
// the Free Software Foundation; either version 2.1 of the License, or
// (at your option) any later version.
// 
// Daisy is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser Public License for more details.
// 
// You should have received a copy of the GNU Lesser Public License
// along with Daisy; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

#define BUILD_DLL

#include "plf.h"
#include "assertion.h"
#include "mathlib.h"
#include <vector>
#include <list>
#include <stdexcept>

struct PLF::Implementation
{
  std::vector<double> x;
  std::vector<double> y;
  std::vector<double> slope;

  double operator () (const double pos) const;
  PLF inverse () const;
  PLF inverse_safe () const;
  double first_interesting () const;
  double last_interesting () const;
  double min () const;
  double max () const;
  double max_at () const;
  double integrate (const double from, const double to) const;
  PLF integrate_stupidly () const;
  void clear () 
    { 
      x.erase (x.begin (), x.end ());
      y.erase (y.begin (), y.end ());
      slope.erase (slope.begin (), slope.end ());
      
    }
  void add (double x, double y);
  void operator = (PLF::Implementation& impl)
  { 
    x = impl.x;
    y = impl.y;
    slope = impl.slope;
  }
  Implementation () { };
  Implementation (PLF::Implementation& impl)
    : x (impl.x),
      y (impl.y), 
      slope (impl.slope)
  { };
};

double 
PLF::Implementation::operator () (const double pos) const
{
  daisy_assert (x.size () > 0);
  daisy_assert (x.size () == y.size ());
  daisy_assert (x.size () == slope.size () + 1);

  unsigned int min = 0;
  unsigned int max = x.size () - 1;

  if (pos <= x[min])
    return y[min];
  else if (pos >= x[max])
    return y[max];

  while (true)
    {
      if (max - min == 1)
	return y[min] + slope[min] * (pos - x[min]);

      const unsigned int guess = (max + min) / 2;

      if (x[guess] < pos)
	min = guess;
      else if (iszero (pos - x[guess]))
	// We need this case to avoid numeric clutter.
	return y[guess];
      else			// x[guess] > pos
	max = guess;
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
      daisy_assert (last <= y[i]);
      plf.add (y[i], x[i]);
      last = y[i];
    }
  return plf;
}

// Calculate the inverse function of a PLF.  
// We only use the first monotonously increasing part, if any.

PLF 
PLF::Implementation::inverse_safe () const
{
  const int size = x.size ();
  PLF plf;

  double last = y[0] - 1.0;
  for (int i = 0; i < size; i++)
    {
      if (last > y[i])
	break;
      plf.add (y[i], x[i]);
      last = y[i];
    }
  return plf;
}

// Find the x value where the function stop being constant.
double
PLF::Implementation::first_interesting () const
{
  const int size = x.size ();
  for (unsigned int i = 1U; i < size; i++)
    if (std::isnormal (y[i] - y[i-1]))
      return x[i-1];
  throw std::invalid_argument ("PLF::first_interesting: constant function");
}

// Find the x value where the function start being constant.
double
PLF::Implementation::last_interesting () const
{
  const int size = x.size ();
  for (int i = size-2; i >= 0; i--)
    if (std::isnormal (y[i] - y[i+1]))
      return x[i+1];
  throw std::invalid_argument ("PLF::last_interesting: constant function");
}

// Find the functions minimum value.
double
PLF::Implementation::min () const
{
  const int size = x.size ();
  if (size < 1)
    throw std::invalid_argument ("PLF::min: empty function");
  double min_y = y[0];
  
  for (unsigned int i = 1; i < size; i++)
    if (y[i] < min_y)
      min_y = y[i];

  return min_y;
}

// Find the functions maximum value.
double
PLF::Implementation::max () const
{
  const int size = x.size ();
  if (size < 1)
    throw std::invalid_argument ("PLF::max: empty function");
  double max_y = y[0];
  
  for (unsigned int i = 1; i < size; i++)
    if (y[i] > max_y)
      max_y = y[i];

  return max_y;
}

// Find an x value where the function reach its maximum value.
double
PLF::Implementation::max_at () const
{
  const int size = x.size ();
  if (size < 1)
    throw std::invalid_argument ("PLF::max_at: empty function");
  double max_x = x[0];
  double max_y = y[0];
  
  for (unsigned int i = 1; i < size; i++)
    if (y[i] > max_y)
      {
	max_x = x[i];
	max_y = y[i];
      }
  return max_x;
}

double
PLF::Implementation::integrate (const double from, const double to) const
{
  daisy_assert (from <= to);
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
  daisy_assert (last_x <= to);
  
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
	  daisy_assert (iszero (x[i] - last_x));
	}
      last_x = x[i];
      last_y = y[i];
    }
  return plf;
}

void 
PLF::Implementation::add (double x_, double y_)
{
  const int size = x.size ();
  daisy_assert (size == 0 || x_ >= x[size - 1]);
  x.push_back (x_);
  y.push_back (y_);

  if (size > 0)
    slope.push_back ((y[size] - y[size-1]) / (x[size] - x[size-1]));

  daisy_assert (slope.size () == size);
}

double
PLF::operator () (const double x) const
{ return impl (x); }

PLF
PLF::inverse () const
{ return impl.inverse (); }

PLF
PLF::inverse_safe () const
{ return impl.inverse_safe (); }

double
PLF::first_interesting () const
{ return impl.first_interesting (); }

double
PLF::last_interesting () const
{ return impl.last_interesting (); }

double
PLF::min () const
{ return impl.min (); }

double
PLF::max () const
{ return impl.max (); }

double
PLF::max_at () const
{ return impl.max_at (); }

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
PLF::offset (double offset)	// Add 'offset' to all y values.
{
  for (unsigned int i = 0; i < impl.y.size(); i++)
    impl.y[i] += offset;
}

double 
PLF::find (const std::vector<double>& x, const std::vector<double>& y, double value)
{
  daisy_assert (x.size () == y.size ());

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
{ impl.add (x, y); }

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
      // If the other is empty, use this one.
      return;
    }

  // I want a vector with all the x points.
  // First I create two lists containing the x points from each plf.
  std::list<double> combined (impl.x.begin (), impl.x.end ());
  std::list<double> other (plf.impl.x.begin (), plf.impl.x.end ());
  // Then I merge them and remove duplicates.
  combined.merge (other);
  combined.unique ();
  // Finally, I convert it to a vector.
  std::vector<double> points (combined.begin (), combined.end ());

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
const PLF& 
PLF::empty ()			// An empty PLF.
{ 
  static const PLF none;
  return none;
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

