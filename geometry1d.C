// geometry1d.C  -- One dimensional discretization.
// 
// Copyright 1996-2001, 2006 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001, 2006 KVL.
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


#include "geometry1d.h"
#include "syntax.h"
#include "alist.h"
#include "mathlib.h"
#include "check.h"
#include "vcheck.h"
#include "groundwater.h"
#include "assertion.h"
#include <sstream>

double 
Geometry1D::fraction_in_z_interval (const size_t i, 
                                    const double from, const double to) const
{ 
  daisy_assert (from > to);

  const double zp = zplus (i);
  const double zm = (i == 0) ? 0.0 : zplus (i-1);
  daisy_assert (zm > zp);

  if (zp >= from)
    // Node is fully above interval.
    return 0.0;
  if (zm <= to)
    // Node is fully below interval.
    return 0.0;


  if (zm <= from)
    {
      if (zp >= to)
        // Node is fully within interval.
        return 1.0;
      
      // Node overlap bottom of interval.
      const double dz1 = zm - zp;
      daisy_assert (approximate (dz1, dz (i)));
      const double overlap = zm - to;
      daisy_assert (overlap > 0.0);
      daisy_assert (dz1 > overlap);
      return overlap / dz1;
    }
  if (zp >= to)
    {
      // Node overlap top of interval.
      const double dz1 = zm - zp;
      daisy_assert (approximate (dz1, dz (i)));
      const double overlap = from - zp;
      daisy_assert (overlap > 0.0);
      daisy_assert (dz1 > overlap);
      return overlap / dz1;
    }

  // Interval is fully within node.
  const double dz1 = zm - zp;
  daisy_assert (approximate (dz1, dz (i)));
  const double overlap = from - to;
  daisy_assert (overlap > 0.0);
  daisy_assert (dz1 > overlap);
  return overlap / dz1;
}

bool 
Geometry1D::edge_cross_z (const size_t e, const double zd) const
{ 
  // Positive number means below bottom.
  if (zd > 0.0)
    return e == edge_size () - 1;

  const double z_above = (e == 0) ? z (0) + dz (0) : z (e - 1);
  const double z_below = (e == size ()) ? z (e - 1) - dz (e - 1) : z (e);
  return zd < z_above && zd > z_below;
}

bool
Geometry1D::contain_z (const size_t i, const double z) const
// True iff node i// includes depth z.
{ return i == interval_plus (z); }

size_t 
Geometry1D::interval_plus (double z) const
{
  size_t i;
  for (i = 0; i < size_; i++)
    {
      if (zplus_[i] <= z)
	return i;
    }
  daisy_assert (false);
}

size_t
Geometry1D::interval_border (double z) const
{
  double best = fabs (z - 0.0);
  
  for (size_t i = 1; i <= size_; i++)
    {
      double dist = fabs (z - zplus_[i-1]);
      if (dist > best)
	return i - 1;
      best = dist;
    }
  return size_;
}

bool 
Geometry1D::check (Treelog&) const
{
  bool ok = true;
  return ok;
}

bool 
Geometry1D::check_border (const double border, Treelog& err) const
{
  bool ok = false;

  for (size_t i = 0; i < size (); i++)
    if (approximate (border, zplus (i)))
      ok = true;

  if (!ok)
    {
      std::ostringstream tmp;
      tmp << "No geometric border near " << border 
             << " [cm], log results may be inexact";
      err.warning (tmp.str ());
    }

  return ok;
}

static bool 
check_alist (const AttributeList&, Treelog&)
{
  bool ok = true;
  return ok;
}

double
Geometry1D::total (const std::vector<double>& v) const
{
  const size_t to = std::min (v.size (), size ());
  double sum = 0.0;
  for (size_t i = 0; i < to; i++)
    sum += v[i] * dz (i);
  return sum;
}

double
Geometry1D::total (const std::vector<double>& v, 
                 const double from, const double to) const
{
  double amount = 0.0;
  double old = 0.0;

  for (unsigned i = 0; i < v.size () && old > to ; i++)
    {
      if (zplus_[i] < from)
	{
	  const double height = (std::min (old, from) - std::max (zplus_[i], to));
	  amount += v[i] * height;
	}
      old = zplus_[i];
    }
  return amount;
}

void
Geometry1D::add (std::vector<double>& v, const double from, const double to, 
               const double amount) const
{
  daisy_assert (to < from);
  const double old_total = total (v);

  const size_t last = interval_plus (to);
  while (v.size () <= last)
    v.push_back (0.0);
  const double density = amount / (from - to);
  double old = 0.0;

  for (size_t i = 0; i <= last; i++)
    {
      if (zplus_[i] < from)
	{
	  const double top = std::min (old, from);
	  const double bottom = std::max (zplus_[i], to);
	  daisy_assert (top > bottom);
	  v[i] += density * (top - bottom) / dz_[i];
	}
      old = zplus_[i];
    }

  const double new_total = total (v);
  if (!approximate (old_total + amount, new_total))
    {
      std::ostringstream tmp;
      tmp << "Olt total (" << old_total << ") + amount (" << amount
             << ") != new total (" << total (v) 
             << "); [" << from << ":" << to << "]";
      daisy_warning (tmp.str ());
    }
}

void
Geometry1D::add (std::vector<double>& v, const std::vector<double>& density,
                 double amount) const
{
  const double old_total = total (v);
  const double total_density = total (density);
  daisy_assert (total_density > 0.0);
  for (size_t i = 0; i <= size (); i++)
    if (density.size () > i)
      v[i] += amount * density[i] / total_density;

  daisy_assert (approximate (old_total + amount, total (v)));
}

void
Geometry1D::mix (std::vector<double>& v, double from, double to) const
{
  const double old_total = total (v);
  add (v, from, to, extract (v, from, to));
  daisy_assert (approximate (old_total, total (v)));
}

void
Geometry1D::mix (std::vector<double>& v, const double from, const double to, 
               std::vector<double>& change) const
{
  const std::vector<double> old = v;
  mix (v, from, to);
  daisy_assert (v.size () <= change.size ());
  for (size_t i = 0; i < v.size (); i++)
    change[i] += v[i] - (i < old.size () ? old[i] : 0.0);
}

double
Geometry1D::extract (std::vector<double>& v, const double from, const double to) const
{
  const double old_total = total (v);
  const size_t last = interval_plus (to);
  while (v.size () <= last)
    v.push_back (0.0);
  double amount = 0.0;
  double old = 0.0;

  for (unsigned i = 0; i <= last; i++)
    {
      daisy_assert (approximate (old - zplus_[i], dz_[i]));
      if (zplus_[i] < from)
	{
	  const double top = std::min (old, from);
	  const double bottom = std::max (zplus_[i], to);
	  daisy_assert (top > bottom);
	  const double height = top - bottom;
	  amount += v[i] * height;
	  if (approximate (height, old - zplus_[i]))
	    v[i] = 0;
	  else
	    v[i] -= v[i] * height / dz_[i];
	}
      old = zplus_[i];
    }
  daisy_assert (approximate (old_total, total (v) + amount));
  return amount;
}

void
Geometry1D::set (std::vector<double>& v, double from, double to, double amount) const
{
  const size_t last = interval_plus (to);
  while (v.size () <= last)
    v.push_back (0.0);
  const double density = amount / (from - to);
  double old = 0.0;

  for (unsigned i = 0; i <= last; i++)
    {
      if (zplus_[i] < from)
	{
	  const double height = (std::min (old, from) - std::max (zplus_[i], to));
	  v[i] -= v[i] * height / (old - zplus_[i]); // Remove old.
	  v[i] += density * height; // Add new.
	}
      old = zplus_[i];
    }
}

void
Geometry1D::swap (std::vector<double>& v, double from, double middle, double to) const
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

void
Geometry1D::swap (std::vector<double>& v, double from, double middle, double to, 
                std::vector<double>& change) const
{
  const std::vector<double> old = v;
  swap (v, from, middle, to);
  daisy_assert (v.size () <= change.size ());
  for (size_t i = 0; i < v.size (); i++)
    change[i] += v[i] - (i < old.size () ? old[i] : 0.0);
}

void 
Geometry1D::initialize_layer (std::vector<double>& array, 
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
      const double soil_end = zplus (size () - 1);
      double last = 0.0;
      for (size_t i = 0; i < layers.size (); i++)
	{
	  double next = layers[i]->number ("end");
	  daisy_assert (next < last);
	  const double value = layers[i]->number ("value");
	  if (next < soil_end)
	    {
	      out.warning (std::string ("WARNING: initial_") + name 
			   + " layer ends below the last node");
	      next = soil_end;
	      i = layers.size ();
	    }
	  add (array, last, next, value * (last - next));
	  last = next;
	}
    }
  // We must leave any remaining values unspecified, the
  // initialization of Theta and h in SoilWater depends on that.
}

void
Geometry1D::load_syntax (Syntax& syntax, AttributeList&)
{ 
  syntax.add_check (check_alist);
  syntax.add ("zplus", "cm", Check::negative (), 
	      Syntax::OptionalConst, Syntax::Sequence,
	      "Depth of each numeric layer (a negative number).\n\
The end points are listed descending from the surface to the bottom.");
  static VCheck::All zplus_check (VCheck::decreasing (), 
				  VCheck::min_size_1 ());
  syntax.add_check ("zplus", zplus_check);
}
  
Geometry1D::Geometry1D (Block& al)
  : Geometry (al)
{ 
  if (al.check ("zplus"))
    zplus_ = al.number_sequence ("zplus");
}

void
Geometry1D::initialize_zplus (const Groundwater& groundwater,
			    const std::vector<double>& fixed,
			    const double max_rooting_depth,
			    const double max_interval,
			    Treelog& msg)
{
  if (zplus_.empty ())
    {
      Treelog::Open nest (msg, "Geometry");
      const bool volatile_bottom = 
	groundwater.bottom_type () == Groundwater::lysimeter 
	|| groundwater.is_pipe ();
      
      bool warn_about_small_intervals = true;
      double last = 0.0;
      double last_fixed = 0.0;
      for (size_t i = 0; i < fixed.size ();)
	{
	  const double current = fixed[i];

	  // We divide the soil into zones with desired interval sizes.
	  double zone_end;
	  double zone_size;
	  
	  if (last > -5.0)
	    {
	      zone_end = -5.0;
	      zone_size = 2.5;
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

          zone_size = std::min (zone_size, (last_fixed - current) / 3.0);

          if (warn_about_small_intervals && zone_size < 0.99)
            {
              warn_about_small_intervals = false;
              std::ostringstream tmp;
              tmp << "\
Can't automatically make discretizations less than 1 [cm], needed at " 
                     << last << " [cm].\nPlease set zplus manually.";
              msg.warning (tmp.str ());
            }

	  // Dispersivity limit.
	  if (zone_size > max_interval)
	    zone_size = max_interval;

	  if (current < zone_end - zone_size + 1e-8)
	    // The zone ends before the next fixed interval limit.
	    while (last > zone_end + 1e-8)
	      // Just add intervals to the end of the zone.
	      {
		last -= zone_size;
		zplus_.push_back (last);
	      }
	  else
	    // The next fixed interval limit is before the end of the zone.
	    {
	      // Find approximate number of intervals until fixed limit.
	      const int intervals 
                = double2int ((last - current) / zone_size + 0.499);
	      if (intervals > 1)
		{
		  // Add interior intervals.
		  const double step 
		    = (last - current) / int2double (intervals);
		  const double first = last;
		  for (int j = 1; j < intervals; j++)
		    {
		      const double next = first - step * j;
                      if (!approximate (double2int (next), last))
                        {
                          last = double2int (next);
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
              last_fixed = current;
	      i++;
	    }
	}

      // Debug messages.
      std::ostringstream tmp;
      tmp << "(zplus";
      for (size_t i = 0; i < zplus_.size (); i++)
	tmp << " " << zplus_[i];
      tmp << "); " << zplus_.size () << " nodes.";
      msg.debug (tmp.str ());
      // Check that zplus is strictly decreasing.
      last = 0.0;
      for (size_t i = 0; i < zplus_.size (); i++)
	{
	  daisy_assert (zplus_[i] < last);
	  last = zplus_[i];
	}
    }

  // Update z and dz from zplus.
  size_ = zplus_.size ();
  double last = 0.0;
  for (size_t i = 0; i < size_; i++)
    {
      double zplus = zplus_[i];
      double dz = last - zplus;
      dz_.push_back (dz);
      double z = last - dz / 2;
      z_.push_back (z);
      last = zplus;
    }
}

Geometry1D::~Geometry1D ()
{ }
