// geometry.C
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


#include "geometry.h"
#include "syntax.h"
#include "alist.h"
#include "tmpstream.h"
#include "mathlib.h"
#include "check.h"
#include "groundwater.h"
#include "assertion.h"

unsigned int 
Geometry::interval_plus (double z) const
{
  unsigned int i;
  for (i = 0; i < size_; i++)
    {
      if (zplus_[i] <= z)
	return i;
    }
  daisy_assert (false);
}

unsigned int
Geometry::interval_border (double z) const
{
  double best = fabs (z - zplus_[0]);
  
  for (unsigned int i = 1; i < size_; i++)
    {
      double dist = fabs (z - zplus_[i]);
      if (dist > best)
	return i - 1;
      best = dist;
    }
  return size_;
}

bool 
Geometry::check (Treelog&) const
{
  bool ok = true;
  return ok;
}

static bool 
check_alist (const AttributeList& al, Treelog& err)
{
  bool ok = true;
  if (al.check ("zplus"))
    {
      const vector<double> zplus = al.number_sequence ("zplus");
  
      if (zplus.size () < 1)
	{
	  err.entry ("You need at least one interval");
	  ok = false;
	}
      double last = 0.0;
      for (unsigned int i = 0; i < zplus.size (); i++)
	{
	  if (zplus[i] >= last)
	    {
	      TmpStream tmp;
	      tmp () << "Intervals should be monotonically decreasing, but "
		     << zplus[i] << " > " << last;
	      err.entry (tmp.str ());
	      ok = false;
	      break;
	    }
	  else 
	    last = zplus[i];
	}
    }
  return ok;
}

double
Geometry::total (const vector<double>& v) const
{
  const unsigned int to = min (v.size (), size ());
  double sum = 0.0;
  for (unsigned int i = 0; i < to; i++)
    sum += v[i] * dz (i);
  return sum;
}

double
Geometry::total (const vector<double>& v, double from, double to) const
{
  double amount = 0.0;
  double old = 0.0;

  for (unsigned i = 0; i < v.size () && old > to ; i++)
    {
      if (zplus_[i] < from)
	{
	  const double height = (min (old, from) - max (zplus_[i], to));
	  amount += v[i] * height;
	}
      old = zplus_[i];
    }
  return amount;
}

void
Geometry::add (vector<double>& v, double from, double to, double amount) const
{
  daisy_assert (to < from);
  const double old_total = total (v);

  const unsigned int last = interval_plus (to);
  while (v.size () <= last)
    v.push_back (0.0);
  const double density = amount / (from - to);
  double old = 0.0;

  for (unsigned int i = 0; i <= last; i++)
    {
      if (zplus_[i] < from)
	v[i] += density * (min (old, from) - max (zplus_[i], to)) / dz_[i];
      old = zplus_[i];
    }

  daisy_assert (approximate (old_total + amount, total (v)));
}

void
Geometry::add (vector<double>& v, const vector<double>& density,
	       double amount) const
{
  const double old_total = total (v);
  const double total_density = total (density);
  daisy_assert (total_density > 0.0);
  for (unsigned int i = 0; i <= size (); i++)
    if (density.size () > i)
      v[i] += amount * density[i] / total_density;

  daisy_assert (approximate (old_total + amount, total (v)));
}

void
Geometry::mix (vector<double>& v, double from, double to) const
{
  const double old_total = total (v);
  add (v, from, to, extract (v, from, to));
  daisy_assert (approximate (old_total, total (v)));
}

double
Geometry::extract (vector<double>& v, double from, double to) const
{
  const double old_total = total (v);
  const unsigned int last = interval_plus (to);
  while (v.size () <= last)
    v.push_back (0.0);
  double amount = 0.0;
  double old = 0.0;

  for (unsigned i = 0; i <= last; i++)
    {
      if (zplus_[i] < from)
	{
	  const double height = (min (old, from) - max (zplus_[i], to));
	  amount += v[i] * height;
	  v[i] -= v[i] * height / (old - zplus_[i]);
	}
      old = zplus_[i];
    }
  daisy_assert (approximate (old_total, total (v) + amount));
  return amount;
}

void
Geometry::set (vector<double>& v, double from, double to, double amount) const
{
  const unsigned int last = interval_plus (to);
  while (v.size () <= last)
    v.push_back (0.0);
  const double density = amount / (from - to);
  double old = 0.0;

  for (unsigned i = 0; i <= last; i++)
    {
      if (zplus_[i] < from)
	{
	  const double height = (min (old, from) - max (zplus_[i], to));
	  v[i] -= v[i] * height / (old - zplus_[i]); // Remove old.
	  v[i] += density * height; // Add new.
	}
      old = zplus_[i];
    }
}

void
Geometry::swap (vector<double>& v, double from, double middle, double to) const
{
  
  const double old_total = total (v);
  const double top_content = extract (v, from, middle);
  double bottom_content = extract (v, middle, to);
  // We want to extract 100% of the interval containing middle, since
  // we already extracted the rest into top_content.
  const int middle_interval = interval_plus (middle);
  bottom_content += v[middle_interval] * dz (middle_interval);
  v[middle_interval] = 0.0;
  const double new_middle = from + to - middle;

  add (v, from, new_middle, bottom_content);
  add (v, new_middle, to, top_content);
  daisy_assert (approximate (old_total, total (v)));
}

static bool 
check_layers (const vector<AttributeList*>& layers, Treelog& err)
{
  double last = 0.0;
  for (unsigned int i = 0; i < layers.size (); i++)
    {
      const double next = layers[i]->number ("end");
      if (next < last)
	last = next;
      else
	{
	  TmpStream tmp;
	  tmp () << "Layer ending at " << next 
		 << " should be below " << last;
	  err.entry (tmp.str ());
	  return false;
	}
    }
  return true;
}

void 
Geometry::add_layer (Syntax& syntax, const string& name,
		     const string& dimension, const string& description)
{
  Syntax& layer = *new Syntax ();
  layer.add_check (check_layers);
  if (!layer.ordered ())
    {
      // Initialize as first call.
      layer.add ("end", "cm", Check::negative (), Syntax::Const, 
		 "End point of this layer (a negative number).");
      layer.add ("value", dimension, Syntax::Const, description);
      layer.order ("end", "value");
    }
  syntax.add (string ("initial_") + name, layer,
	      Syntax::OptionalConst, Syntax::Sequence, 
	      string ("Initial value of the '") + name + "' parameter.\n\
The initial value is given as a sequence of (END VALUE) pairs, starting\n\
from the top and going down.  The parameter will be initialized to\n\
VALUE from the END of the previous layer, to the END of the current layer.");
  syntax.add (name, dimension, Syntax::OptionalState, Syntax::Sequence, 
	      description);
}

void 
Geometry::initialize_layer (vector<double>& array, 
			    const AttributeList& al, 
			    const string& name, Treelog& out) const
{
  const string initial = string ("initial_") + name;
  daisy_assert (array.size () == 0);
  if (al.check (name))
    // Specified by user.
    array = al.number_sequence (name);
  else if (al.check (initial))
    {
      // Initialize by layers.
      const vector<AttributeList*>& layers = al.alist_sequence (initial);
      array.insert (array.begin (), size (), 0.0);
      const double soil_end = zplus (size () - 1);
      double last = 0.0;
      for (unsigned int i = 0; i < layers.size (); i++)
	{
	  double next = layers[i]->number ("end");
	  daisy_assert (next < last);
	  const double value = layers[i]->number ("value");
	  if (next < soil_end)
	    {
	      out.warning (string ("WARNING: initial_") + name 
			   + " layer ends below the last node");
	      next = soil_end;
	      i = layers.size ();
	    }
	  add (array, last, next, value * (last - next));
	  last = next;
	}
    }
}

void
Geometry::load_syntax (Syntax& syntax, AttributeList&)
{ 
  syntax.add_check (check_alist);
  syntax.add ("zplus", "cm", Check::negative (), 
	      Syntax::OptionalConst, Syntax::Sequence,
	      "Depth of each numeric layer (a negative number).\n\
The end points are listed descending from the surface to the bottom.");
}
  
Geometry::Geometry (const AttributeList& al)
{ 
  if (al.check ("zplus"))
    zplus_ = al.number_sequence ("zplus");
}

void
Geometry::initialize_zplus (const Groundwater& groundwater,
			    const vector<double>& fixed,
			    const double max_rooting_depth,
			    Treelog& msg)
{
  if (zplus_.empty ())
    {
      Treelog::Open nest (msg, "Geometry");
      const Library& library = Librarian<Groundwater>::library ();
      const string name = groundwater.name;
      const bool volatile_bottom = 
	library.is_derived_from (name, "lysimeter")
	|| library.is_derived_from (name, "pipe");

      double last = 0.0;
      for (unsigned int i = 0; i < fixed.size ();)
	{
	  const double current = fixed[i];

	  // We divide the soil into zones with desired interval sizes.
	  double zone_end;
	  double zone_size;
	  bool do_round = true;
	  
	  if (last > -5.0)
	    {
	      zone_end = -5.0;
	      zone_size = 2.5;
	      do_round = true;
	    }
	  else if (volatile_bottom)
	    {
	      zone_end = current;
	      zone_size = 5.0;
	    }
	  else if (last > -10.0)
	    {
	      zone_end = -10.0;
	      zone_size = 5.0;
	    }
	  else if (last > max_rooting_depth - 50.0)
	    {
	      zone_end = max_rooting_depth - 50.0;
	      zone_size = 10.0;
	    }
	  else
	    {
	      zone_end = current;
	      zone_size = 20.0;
	    }

	  if (current < zone_end - zone_size)
	    // The zone ends before the next fixed interval limit.
	    while (last > zone_end)
	      // Just add intervals to the end of the zone.
	      {
		last -= zone_size;
		zplus_.push_back (last);
	      }
	  else
	    // The next fixed interval limit is before the end of the zone.
	    {
	      // Find approximate number of intervals until fixed limit.
	      const int intervals = double2int ((last - current) / zone_size);
	      if (intervals > 1)
		{
		  // Add interior intervals.
		  const double step 
		    = (last - current) / int2double (intervals);
		  const double first = last;
		  for (int j = 1; j < intervals; j++)
		    {
		      const double next = first - step * j;
		      if (do_round)
			{
			  if (!approximate (double2int (next), last))
			    {
			      last = double2int (next);
			      zplus_.push_back (last);
			    }
			}
		      else
			{
			  last = next;
			  zplus_.push_back (last);
			}
		    }
		}
	      // Add fixed limit.
	      if (!approximate (last, current))
		{
		  last = current;
		  zplus_.push_back (current);
		}
	      // Next fixed limit.
	      i++;
	    }
	}

      // Debug messages.
      TmpStream tmp;
      tmp () << "(zplus ";
      for (unsigned int i = 0; i < zplus_.size (); i++)
	tmp () << " " << zplus_[i];
      tmp () << ") ; Intervals: " << zplus_.size ();
      msg.debug (tmp.str ());
      // Check that zplus is strictly decreasing.
      last = 0.0;
      for (unsigned int i = 0; i < zplus_.size (); i++)
	{
	  daisy_assert (zplus_[i] < last);
	  last = zplus_[i];
	}
    }

  // Update z and dz from zplus.
  size_ = zplus_.size ();
  double last = 0.0;
  for (unsigned int i = 0; i < size_; i++)
    {
      double zplus = zplus_[i];
      double dz = last - zplus;
      dz_.push_back (dz);
      double z = last - dz / 2;
      z_.push_back (z);
      last = zplus;
    }
};

Geometry::~Geometry ()
{ }
