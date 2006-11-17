// geometry.C
// 
// Copyright 2006 Per Abrahamsen and KVL.
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


#include "geometry.h"
#include "volume.h"
#include "alist.h"
#include "check.h"
#include "vcheck.h"
#include <sstream>

const int Geometry::cell_above;
const int Geometry::cell_below;
const int Geometry::cell_left;
const int Geometry::cell_right;
const int Geometry::cell_front;
const int Geometry::cell_back;

std::string
Geometry::cell_name (int n) const
{ 
  switch (n)
    {
    case cell_above:
      return "top";
    case cell_below:
      return "bottom";
    case cell_left:
      return "left";
    case cell_right:
      return "right";
    case cell_front:
      return "front";
    case cell_back:
      return "back";
    default:
      std::ostringstream tmp;
      switch (dimensions ())
        {
        case 1:
          tmp << z (n);
          break;
        case 2:
          tmp << "(" << z (n) << " " << x (n) << ")";
          break;
        case 3:
          tmp << "(" << z (n) << " " << x (n) << " " << y (n) << ")";
          break;
        default:
          daisy_panic ("Only 1, 2 and 3 dimensional geometries supported");
        }
      return tmp.str ();
    }
}

std::string 
Geometry::edge_name (size_t e) const
{
  std::ostringstream tmp;
  tmp << "(" << cell_name(edge_from (e)) << " " 
      << cell_name (edge_to (e)) << ")";
  return tmp.str ();
}

int
Geometry::edge_index (const int from, const int to)
{
  for (size_t e = 0; e < edge_size (); e++)
    if (edge_from (e) == from && edge_to (e) == to)
      return e;
  return -1;
}

double
Geometry::z_safe (int n) const
{
  if (n >= 0)
    return z (n);
  switch (n)
    {
    case cell_above:
      return top () + 1.0 /* [cm] */;
    case cell_below:
      return bottom () - 1.0 /* [cm] */;
    default:
      return (bottom () - top ()) / 2.0;
    }
}
  
double
Geometry::x_safe (int n) const
{
  if (n >= 0)
    return x (n);
  switch (n)
    {
    case cell_right:
      return right () + 1.0 /* [cm] */;
    case cell_left:
      return left () - 1.0 /* [cm] */;
    default:
      return (right () - left ()) / 2.0;
    }
}

double 
Geometry::right () const
{ return 1.0 /* [cm] */; }  

double
Geometry::y_safe (int n) const
{
  if (n >= 0)
    return y (n);
  switch (n)
    {
    case cell_back:
      return back () + 1.0 /* [cm] */;
    case cell_front:
      return front () - 1.0 /* [cm] */;
    default:
      return (back () - front ()) / 2.0;
    }
}

double
Geometry::volume_in_z_interval (const double from, const double to, 
                                std::vector<double>& frac) const
{
  const size_t cell_size = this->cell_size ();
  daisy_assert (frac.size () == cell_size);
  double volume = 0.0;
  for (size_t i = 0; i < cell_size; i++)
    {
      const double f = fraction_in_z_interval (i, from, to);
      if (f > 0.0)
        {
          const double rel_vol = f * this->volume (i);
          frac[i] = f;
          volume += rel_vol;
        }
    }
  return volume;
}

bool 
Geometry::edge_cross_z (const size_t e, const double zd) const
{ 
  const double z_from = z_safe (edge_from (e));
  const double z_to = z_safe (edge_to (e));
  const double z_above = std::max (z_from, z_to);
  const double z_below = std::min (z_from, z_to);
  return z_above >= zd && zd > z_below;
}

bool 
Geometry::node_center_in_volume (int c, const Volume& volume) const
{
  if (!is_regular_cell (c))
    return false;
  return volume.contain_point (z (c), x (c), y (c));
}

void
Geometry::mix (std::vector<double>& v, double from, double to) const
{
  const double old_total = total (v);
  add (v, from, to, extract (v, from, to));
  daisy_assert (approximate (old_total, total (v)));
}

void
Geometry::mix (std::vector<double>& v, const double from, const double to, 
               std::vector<double>& change) const
{
  const size_t cell_size = this->cell_size ();
  daisy_assert (v.size () == cell_size);
  daisy_assert (change.size () == cell_size);

  const std::vector<double> old = v;
  mix (v, from, to);
  for (size_t i = 0; i < cell_size; i++)
    change[i] += v[i] - old[i];
}

void
Geometry::add (std::vector<double>& v, const double from, const double to, 
               const double amount) const
{
  // Pre-conditions.
  daisy_assert (to < from);
  const size_t cell_size = this->cell_size ();
  daisy_assert (v.size () == cell_size);

  // Remember old value for post-condition.
  const double old_total = total (v);

  // Find total volume and cell volumes inside interval.
  std::vector<double> frac (cell_size, 0.0);
  const double total_volume = volume_in_z_interval (from, to, frac);
  daisy_assert (total_volume > 0.0);

  // Divide amount relative to volume.
  const double density = amount / total_volume;
  for (size_t i = 0; i < cell_size; i++)
    v[i] += density * frac[i];

  // Post-condition.
  const double new_total = total (v);
  if (!approximate (old_total + amount, new_total))
    {
      std::ostringstream tmp;
      tmp << "Old total (" << old_total << ") + amount (" << amount
             << ") != new total (" << total (v) 
          << "); [" << from << ":" << to << "], total_volume =" 
          << total_volume << ", density = " << density;
      daisy_warning (tmp.str ());
    }
}

void
Geometry::add (std::vector<double>& v, const std::vector<double>& density,
               const double amount) const
{
  const size_t cell_size = this->cell_size ();
  daisy_assert (v.size () == cell_size);
  const double old_total = total (v);

  const double total_density = total (density);
  daisy_assert (total_density > 0.0);
  for (size_t i = 0; i < cell_size; i++)
    if (density.size () > i)
      v[i] += amount * density[i] / total_density;

  daisy_assert (approximate (old_total + amount, total (v)));
}

double
Geometry::extract (std::vector<double>& v, 
                   const double from, const double to) const
{
  const size_t cell_size = this->cell_size ();
  daisy_assert (v.size () == cell_size);

  const double old_total = total (v);

  double amount = 0.0;
  for (size_t i = 0; i < cell_size; i++)
    {
      const double f = fraction_in_z_interval (i, from, to);
      if (f > 0.0)
        {
          amount += f * volume (i) * v[i];

	  if (f < 1.0)
	    v[i] *= (1.0 - f);
	  else
	    v[i] = 0.0;

	}
    }

  daisy_assert (approximate (old_total, total (v) + amount));
  return amount;
}

void
Geometry::set (std::vector<double>& v, 
               const double from, const double to, double amount) const
{
  const size_t cell_size = this->cell_size ();
  daisy_assert (v.size () == cell_size);

  const double old_total = total (v);
  const double old_amount = total (v, from, to);

  std::vector<double> frac (cell_size, 0.0);
  const double density = amount / volume_in_z_interval (from, to, frac);

  for (size_t i = 0; i < cell_size; i++)
    {
      const double f = fraction_in_z_interval (i, from, to);
      if (f > 0.0)
        {
	  if (f < 1.0)
            v[i] = v[i] + (density - v[i]) * f;
          else
            v[i] = density;
	}
    }
  
  daisy_assert (approximate (old_total - old_amount + amount, total (v)));
}

void
Geometry::swap (std::vector<double>& v,
                const double from, const double middle, const double to) const
{
  const size_t cell_size = this->cell_size ();
  daisy_assert (v.size () == cell_size);

  const double old_total = total (v);

  const double top = total (v, from, middle);
  const double bottom = total (v, middle, to);
  set (v, from, to, 0.0);
  add (v, from, middle, bottom);
  add (v, middle, to, top);

  daisy_assert (approximate (old_total, total (v)));
}

void
Geometry::swap (std::vector<double>& v, 
                const double from,  const double middle, const double to, 
                std::vector<double>& change) const
{
  const size_t cell_size = this->cell_size ();
  daisy_assert (v.size () == cell_size);
  daisy_assert (change.size () == cell_size);

  const std::vector<double> old = v;
  swap (v, from, middle, to);
  for (size_t i = 0; i < v.size (); i++)
    change[i] += v[i] - old[i];
}

double
Geometry::total (const std::vector<double>& v) const
{
  const size_t cell_size = this->cell_size ();
  daisy_assert (v.size () == cell_size);
  double sum = 0.0;
  for (size_t i = 0; i < cell_size; i++)
    sum += v[i] * volume (i);
  return sum;
}

double
Geometry::total (const std::vector<double>& v, 
                 const double from, const double to) const
{
  const size_t cell_size = this->cell_size ();
  daisy_assert (v.size () == cell_size);
  double sum = 0.0;

  for (size_t i = 0; i < cell_size; i++)
    {
      const double f = fraction_in_z_interval (i, from, to);
      if (f > 0.0)
        sum += v[i] * volume (i) * f;
    }

  return sum;
}

static struct CheckLayers : public VCheck
{
  void check (const Syntax& syntax, const AttributeList& alist, 
	      const std::string& key) const throw (std::string)
  {
    daisy_assert (alist.check (key));
    daisy_assert (syntax.lookup (key) == Syntax::AList);
    daisy_assert (!syntax.is_log (key));
    daisy_assert (syntax.size (key) == Syntax::Sequence);

    const std::vector<AttributeList*>& layers = alist.alist_sequence (key);

    double last = 0.0;
    for (unsigned int i = 0; i < layers.size (); i++)
      {
	if (!layers[i]->check ("end"))
	  continue;

	const double next = layers[i]->number ("end");
	if (next < last)
	  last = next;
	else
	  {
	    std::ostringstream tmp;
	    tmp << "Layer ending at " << next 
		   << " should be below " << last;
	    throw std::string (tmp.str ());
	  }
      }
  }
} check_layers;

void 
Geometry::add_layer (Syntax& syntax, Syntax::category cat, 
                     const std::string& name,
		     const std::string& dimension,
                     const std::string& description)
{
  Syntax& layer = *new Syntax ();
  layer.add ("end", "cm", Check::negative (), Syntax::Const, 
	     "End point of this layer (a negative number).");
  if (dimension == Syntax::Fraction ())
    layer.add_fraction ("value", Syntax::Const, description);
  else
    layer.add ("value", dimension, Syntax::Const, description);
  layer.order ("end", "value");

  const std::string iname = "initial_" + name;
  syntax.add (iname, layer,
	      Syntax::OptionalConst, Syntax::Sequence, 
	      "Initial value of the '" + name + "' parameter.\n\
The initial value is given as a sequence of (END VALUE) pairs, starting\n\
from the top and going down.  The parameter will be initialized to\n\
VALUE from the END of the previous layer, to the END of the current layer.");
  syntax.add_check (iname, check_layers);
  if (dimension == Syntax::Fraction ())
    syntax.add_fraction (name, cat, Syntax::Sequence, 
                         description);
  else
    syntax.add (name, dimension, cat, Syntax::Sequence, 
                description);
}

void 
Geometry::initialize_layer (std::vector<double>& array, 
                            const AttributeList& al, 
                            const std::string& name, Treelog& out) const
{
  const std::string initial = std::string ("initial_") + name;
  daisy_assert (array.size () == 0);
  if (al.check (name))
    // Specified by user.
    array = al.number_sequence (name);
  else if (al.check (initial))
    {
      // Initialize by layers.
      const std::vector<AttributeList*>& layers = al.alist_sequence (initial);
      const double soil_end = bottom ();
      double last = 0.0;
      for (size_t i = 0; i < layers.size (); i++)
	{
	  double next = layers[i]->number ("end");
	  daisy_assert (next < last);
	  const double value = layers[i]->number ("value");
	  if (next < soil_end)
	    {
	      out.warning (std::string ("WARNING: initial_") + name 
			   + " layer ends below the last cell");
	      next = soil_end;
	      i = layers.size ();
	    }
          for (size_t cell = 0; cell < cell_size (); cell++)
            {
              const double f = fraction_in_z_interval (cell, last, next);
              if (f > 0.001)
                {
                  // We only grow array as needed, which helps the 1D case.
                  while (array.size () <= cell)
                    array.push_back (0.0);
                  array[cell] += f * value;
                }
            }
	  last = next;
	}
    }
  // We must leave any remaining values unspecified, the
  // initialization of Theta and h in SoilWater depends on that.
}

void
Geometry::initialize_intervals (const std::vector<double>& end, 
                                std::vector<double>& center,
                                std::vector<double>& distance)
{
  double last = 0.0;
  for (size_t i = 0; i < end.size (); i++)
    {
      const double next = end[i];
      const double diff = next - last;
      distance.push_back (std::fabs (diff));
      center.push_back (last + diff / 2.0);
      last = next;
    }
}
Geometry::Geometry (Block&)
  : size_ (0)
{ }

Geometry::~Geometry ()
{ }
