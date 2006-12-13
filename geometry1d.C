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
#include "volume.h"
#include "block.h"
#include "syntax.h"
#include "alist.h"
#include "mathlib.h"
#include "check.h"
#include "vcheck.h"
#include "submodel.h"
#include "assertion.h"
#include <sstream>

std::string
Geometry1D::edge_name (const size_t e) const
{
  if (e == 0)
    return "0";

  std::ostringstream tmp;
  tmp << zplus (e-1U);
  return tmp.str ();
}

size_t 
Geometry1D::cell_at (const double z, double, double) const
{
  for (size_t cell = 1; cell < cell_size (); cell++)
    if (zplus (cell-1U) >= z)
      return cell;
  return cell_size () - 1;
}

double 
Geometry1D::fraction_in_z_interval (const size_t i, 
                                    const double from, const double to) const
{ return fraction_within (zplus (i), zminus (i), to, from); }

double 
Geometry1D::fraction_in_volume (size_t n, const Volume& volume) const
{ return volume.box_fraction (zplus (n), zminus (n)); }

bool 
Geometry1D::contain_z (size_t i, double z) const
{ return  z <= zminus (i) && z >= zplus(i); }

size_t 
Geometry1D::interval_plus (double z) const
{
  size_t i;
  for (i = 0; i < size_; i++)
    {
      if (zplus (i) <= z)
	return i;
    }
  daisy_notreached ();
}

size_t
Geometry1D::interval_border (double z) const
{
  double best = fabs (z - 0.0);
  
  for (size_t i = 1; i <= size_; i++)
    {
      const double dist = fabs (z - zplus (i-1U));
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
Geometry1D::check_x_border (const double, Treelog& err) const
{
  err.warning ("Logging on x-axis on a 1D geometry is meaningless");
  return false;
}

bool 
Geometry1D::check_y_border (const double, Treelog& err) const
{
  err.warning ("Logging on y-axis on a 1D geometry is meaningless");
  return false;
}

static bool 
check_alist (const AttributeList&, Treelog&)
{
  bool ok = true;
  return ok;
}

void
Geometry1D::swap (std::vector<double>& v, double from, double middle, double to) const
{
  
  const double old_total = total_soil (v);
  const double top_content = extract_soil (v, from, middle);
  double bottom_content = extract_soil (v, middle, to);
  // We want to extract 100% of the interval containing middle, since
  // we already extracted the rest into top_content.
  const int middle_interval = interval_plus (middle);
  bottom_content += v[middle_interval] * dz (middle_interval);
  v[middle_interval] = 0.0;
  const double new_middle = from + to - middle;

  add_soil (v, from, new_middle, bottom_content);
  add_soil (v, new_middle, to, top_content);
  daisy_assert (approximate (old_total, total_soil (v)));
}

void
Geometry1D::load_syntax (Syntax& syntax, AttributeList& alist)
{ 
  alist.add ("submodel", "Geometry1D");
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
  : GeometryVert (al)
{ 
  if (al.check ("zplus"))
    zplus_ = al.number_sequence ("zplus");
}

void
Geometry1D::initialize_zplus (const bool volatile_bottom,
                              const std::vector<double>& fixed,
                              const double max_rooting_depth,
                              const double max_interval,
                              Treelog& msg)
{
  if (zplus_.empty ())
    {
      Treelog::Open nest (msg, "Geometry");
      
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
      tmp << "); " << zplus_.size () << " cells.";
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
  initialize_intervals (zplus_, z_, dz_);

  // Initialize base!
  size_ = zplus_.size ();
}

Geometry1D::~Geometry1D ()
{ }

static Submodel::Register 
geometry1d_submodel ("Geometry1D", Geometry1D::load_syntax);
