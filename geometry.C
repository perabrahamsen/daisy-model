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

#define BUILD_DLL

#include "geometry.h"
#include "volume.h"
#include "check.h"
#include "vcheck.h"
#include "treelog.h"
#include "frame_submodel.h"
#include "assertion.h"
#include "mathlib.h"
#include "librarian.h"
#include "plf.h"
#include "check.h"
#include <sstream>

const int Geometry::cell_above;
const int Geometry::cell_below;
const int Geometry::cell_left;
const int Geometry::cell_right;
const int Geometry::cell_front;
const int Geometry::cell_back;
const int Geometry::cell_error;

bool 
Geometry::cell_is_external (int cell) const
{ return cell == cell_above
    || cell == cell_below
    || cell == cell_left
    || cell == cell_right
    || cell == cell_front
    || cell == cell_back; }

bool
Geometry::cell_is_valid (int cell) const
{ return cell_is_internal (cell) || cell_is_external (cell); }

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
      daisy_assert (cell_is_internal (n));
      std::ostringstream tmp;
      switch (dimensions ())
        {
        case 1:
          tmp << cell_z (n);
          break;
        case 2:
          tmp << "(" << cell_z (n) << " " << cell_x (n) << ")";
          break;
        case 3:
          tmp << "(" << cell_z (n) << " " << cell_x (n) << " " << cell_y (n) << ")";
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
Geometry::edge_index (const int from, const int to) const
{
  for (size_t e = 0; e < edge_size (); e++)
    if (edge_from (e) == from && edge_to (e) == to)
      return e;
  return -1;
}

double 
Geometry::edge_cos_angle (size_t e) const // Rel. hor. plane [-1:1]
{ return std::cos (std::asin (edge_sin_angle (e))); }

double
Geometry::z_safe (int n) const
{
  if (n >= 0)
    return cell_z (n);
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
    return cell_x (n);
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
    return cell_y (n);
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

Geometry::Access::~Access ()
{ }
  
double 
Geometry::access_content_height (const Access& access, const double z) const
{
  double total_volume = 0.0;
  double total_content = 0.0;

  for (size_t i = 0; i < this->cell_size (); i++)
    if (this->contain_z (i, z))
      {
        const double volume = cell_volume (i);
        total_volume += volume;
        total_content += volume * access (i);
      }
  if (iszero (total_volume))
    return 0.0;

  return total_content / total_volume;
}

double 
Geometry::access_content_hood (const Access& access, const int center) const
{
  double total_area = 0.0;
  double total_content = 0.0;

  const std::vector<size_t>& hood = this->cell_edges (center);
  const size_t hood_size = hood.size ();
  
  for (size_t i = 0; i < hood_size; i++)
    {
      const int edge = hood[i];
      const int neighbor = this->edge_other (edge, center);
      if (this->cell_is_internal (neighbor))
        {
          const double area = this->edge_area (edge);
          total_area += area;
          total_content += access (neighbor) * area;
        }
    }
  if (iszero (total_area))
    return 0.0;

  return total_content / total_area;
}

double 
Geometry::content_hood (const std::vector<double>& content, 
                        const int center) const
{
  double total_area = 0.0;
  double total_content = 0.0;

  const std::vector<size_t>& hood = this->cell_edges (center);
  const size_t hood_size = hood.size ();
  
  for (size_t i = 0; i < hood_size; i++)
    {
      const int edge = hood[i];
      const int neighbor = this->edge_other (edge, center);
      if (this->cell_is_internal (neighbor))
        {
          const double area = this->edge_area (edge);
          total_area += area;
          total_content += content[neighbor] * area;
        }
    }
  if (iszero (total_area))
    return 0.0;

  return total_content / total_area;
}

double 
Geometry::access_content_cell_or_hood (const Access& access,
                                       const int cell) const
{
  if (cell_is_internal (cell))
    return access (cell);
  
  return access_content_hood (access, cell);
}

double 
Geometry::access_content_interval (const Access& access, 
                                   const double from, const double to) const
{
  double total_volume = 0.0;
  double total_content = 0.0;

  for (size_t i = 0; i < this->cell_size (); i++)
    {
      const double volume = cell_volume (i)
        * this->fraction_in_z_interval (i, from, to);
      if (volume > 0.0)
        {
          total_volume += volume;
          total_content += volume * access (i);
        }
    }
  if (iszero (total_volume))
    return 0.0;

  return total_content / total_volume;
}

double 
Geometry::access_content_volume (const Access& access, 
                                 const Volume& vol) const
{
  double total_volume = 0.0;
  double total_content = 0.0;

  for (size_t i = 0; i < this->cell_size (); i++)
    {
      const double volume = cell_volume (i)
        * this->fraction_in_volume (i, vol);
      if (volume > 0.0)
        {
          total_volume += volume;
          total_content += volume * access (i);
        }
    }
  if (iszero (total_volume))
    return 0.0;

  return total_content / total_volume;
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
          const double rel_vol = f * cell_volume (i);
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
Geometry::cell_center_in_volume (int c, const Volume& volume) const
{
  if (!cell_is_internal (c))
    return false;
  return volume.contain_point (cell_z (c), cell_x (c), cell_y (c));
}

size_t 
Geometry::cell_pseudo_number (const int n) const
{
  switch (n)
    {
    case cell_above:
      return cell_size () + 0;
    case cell_below:
      return cell_size () + 1;
    case cell_left:
      return cell_size () + 2;
    case cell_right:
      return cell_size () + 3;
    case cell_front:
      return cell_size () + 4;
    case cell_back:
      return cell_size () + 5;
    default:
      daisy_assert (n >= 0);
      daisy_assert (n < cell_size ());
      return n;
    }
}

void
Geometry::mix (std::vector<double>& v, double from, double to) const
{
  const double old_total = total_soil (v);
  add_soil (v, from, to, extract_soil (v, from, to));
  daisy_approximate (old_total, total_soil (v));
}

void
Geometry::mix (std::vector<double>& v, const Volume& volume) const
{
  const double old_total = total_soil (v);
  add_soil (v, volume, extract_soil (v, volume));
  daisy_approximate (old_total, total_soil (v));
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
    change[i] += (v[i] - old[i]);
}

void
Geometry::mix (std::vector<double>& v, const Volume& volume, 
               std::vector<double>& change) const
{
  const size_t cell_size = this->cell_size ();
  daisy_assert (v.size () == cell_size);
  daisy_assert (change.size () == cell_size);

  const std::vector<double> old = v;
  mix (v, volume);
  for (size_t i = 0; i < cell_size; i++)
    change[i] += (v[i] - old[i]);
}

void
Geometry::add_soil (std::vector<double>& v, 
                    const double from, const double to, 
                    const double amount) const
{
  // Pre-conditions.
  daisy_assert (to < from);
  const size_t cell_size = this->cell_size ();
  if (v.size () != cell_size)
    {
      std::ostringstream tmp;
      tmp << "v has " << v.size () << " elements, but cell size is " 
          << cell_size;
      daisy_panic (tmp.str ());
    }

  // Remember old value for post-condition.
  const double old_total = total_soil (v);

  // Find total volume and cell volumes inside interval.
  std::vector<double> frac (cell_size, 0.0);
  const double total_volume = volume_in_z_interval (from, to, frac);
  daisy_assert (total_volume > 0.0);

  // Divide amount relative to volume.
  const double density = amount / total_volume;
  for (size_t i = 0; i < cell_size; i++)
    v[i] += density * frac[i];

  // Post-condition.
  const double new_total = total_soil (v);
  if (!approximate (old_total + amount, new_total))
    {
      std::ostringstream tmp;
      tmp << "Old total (" << old_total << ") + amount (" << amount
             << ") != new total (" << total_soil (v) 
          << "); [" << from << ":" << to << "], total_volume =" 
          << total_volume << ", density = " << density;
      daisy_warning (tmp.str ());
    }
}

void
Geometry::add_soil (std::vector<double>& v, const std::vector<double>& density,
                    const double amount) const
{
  const size_t cell_size = this->cell_size ();
  daisy_assert (v.size () == cell_size);
  const double old_total = total_soil (v);

  const double total_density = total_soil (density);
  daisy_assert (total_density > 0.0);
  for (size_t i = 0; i < cell_size; i++)
    if (density.size () > i)
      v[i] += amount * density[i] / total_density;

  daisy_assert (approximate (old_total + amount, total_soil (v)));
}

void
Geometry::add_soil (std::vector<double>& v, const Volume& volume,
                    const double amount) const
{ add_soil (v, volume.density (*this), amount); }

void 
Geometry::add_surface (std::vector<double>& v,
                       const double from, const double to, 
                       const double amount) const
{ add_soil (v, from, to, amount * surface_area ()); }

void 
Geometry::add_surface (std::vector<double>& v,
                       const std::vector<double>& density,
                       const double amount) const
{ add_soil (v, density, amount * surface_area ()); }

void 
Geometry::add_surface (std::vector<double>& v,
                       const Volume& volume,
                       const double amount) const
{ add_soil (v, volume, amount * surface_area ()); }

double
Geometry::extract_soil (std::vector<double>& v, 
                        const double from, const double to) const
{
  const size_t cell_size = this->cell_size ();
  daisy_assert (v.size () == cell_size);

  const double old_total = total_soil (v);

  double amount = 0.0;
  for (size_t i = 0; i < cell_size; i++)
    {
      const double f = fraction_in_z_interval (i, from, to);
      if (f > 0.0)
        {
          amount += f * cell_volume (i) * v[i];

	  if (f < 1.0)
	    v[i] *= (1.0 - f);
	  else
	    v[i] = 0.0;

	}
    }
  daisy_assert (approximate (old_total, total_soil (v) + amount));
  return amount;
}

double
Geometry::extract_soil (std::vector<double>& v, const Volume& volume) const
{
  const size_t cell_size = this->cell_size ();
  daisy_assert (v.size () == cell_size);

  const double old_total = total_soil (v);

  double amount = 0.0;
  for (size_t i = 0; i < cell_size; i++)
    {
      const double f = fraction_in_volume (i, volume);
      if (f > 0.0)
        {
          amount += f * cell_volume (i) * v[i];

	  if (f < 1.0)
	    v[i] *= (1.0 - f);
	  else
	    v[i] = 0.0;

	}
    }
  daisy_assert (approximate (old_total, total_soil (v) + amount));
  return amount;
}

double
Geometry::extract_surface (std::vector<double>& v, 
                           const double from, const double to) const
{ return extract_soil (v, from, to) / surface_area (); }

void
Geometry::set_soil (std::vector<double>& v, 
                    const double from, const double to, 
                    const double amount) const
{
  const size_t cell_size = this->cell_size ();
  daisy_assert (v.size () == cell_size);

  const double old_total = total_soil (v);
  const double old_amount = total_soil (v, from, to);

  std::vector<double> frac (cell_size, 0.0);
  const double density = amount / volume_in_z_interval (from, to, frac);

  for (size_t i = 0; i < cell_size; i++)
    {
      const double f = frac[i];
      if (f > 0.0)
        {
	  if (f < 1.0)
            v[i] = v[i] + (density - v[i]) * f;
          else
            v[i] = density;
	}
    }
  const double new_total = total_soil (v);
  const double sum = old_total - old_amount + amount;
  if (!approximate (sum, new_total)
      && ! approximate (new_total + old_amount, old_amount + amount))
    {
      const double rel = std::fabs (sum) / std::max (std::fabs (old_total),
                                                     std::fabs (sum));
      std::ostringstream tmp;
      tmp << "old_total (" << old_total << ") - old_amount (" << old_amount 
          << ") + amount (" << amount << ") = " << sum << " != new_total (" << new_total 
          << "); diff = " << sum - new_total << "; rel = " << rel; 
      daisy_warning (tmp.str ());
    }
}

void 
Geometry::set_surface (std::vector<double>& v,
                       const double from, const double to, 
                       const double amount) const
{ set_soil (v, from, to, amount * surface_area ()); }

void
Geometry::swap (std::vector<double>& v,
                const double from, const double middle, const double to) const
{
  const size_t cell_size = this->cell_size ();
  daisy_assert (v.size () == cell_size);

  const double old_total = total_soil (v);

  const double top = total_soil (v, from, middle);
  const double bottom = total_soil (v, middle, to);
  set_soil (v, from, to, 0.0);
  const double new_middle = to + (from - middle);
  daisy_assert (new_middle < from);
  daisy_assert (new_middle > to);
  add_soil (v, from, new_middle, bottom);
  add_soil (v, new_middle, to, top);

  daisy_assert (approximate (old_total, total_soil (v)));
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
    change[i] += (v[i] - old[i]);
}

double
Geometry::total_soil (const std::vector<double>& v) const
{
  const size_t cell_size = this->cell_size ();
  daisy_assert (v.size () == cell_size);
  double sum = 0.0;
  for (size_t i = 0; i < cell_size; i++)
    sum += v[i] * cell_volume (i);
  return sum;
}

double
Geometry::total_soil (const std::vector<double>& v, 
                      const double from, const double to) const
{
  const size_t cell_size = this->cell_size ();
  daisy_assert (v.size () == cell_size);
  double sum = 0.0;

  for (size_t i = 0; i < cell_size; i++)
    {
      const double f = fraction_in_z_interval (i, from, to);
      if (f > 0.0)
        sum += v[i] * cell_volume (i) * f;
    }

  return sum;
}

double
Geometry::total_soil (const std::vector<double>& v, const Volume& volume) const
{
  const size_t cell_size = this->cell_size ();
  daisy_assert (v.size () == cell_size);

  double amount = 0.0;
  for (size_t i = 0; i < cell_size; i++)
    {
      const double f = fraction_in_volume (i, volume);
      if (f > 0.0)
        amount += f * cell_volume (i) * v[i];
    }
  return amount;
}

double 
Geometry::total_surface (const std::vector<double>& v) const
{ return total_soil (v) / surface_area (); }

double
Geometry::total_surface (const std::vector<double>& v, 
                         const double from, const double to) const
{ return total_soil (v, from, to) / surface_area (); }

static struct CheckLayers : public VCheck
{
  bool verify (const Metalib&, const Frame& frame, const symbol key, 
               Treelog& msg) const
  {
    daisy_assert (frame.check (key));
    daisy_assert (frame.lookup (key) == Attribute::Submodel);
    daisy_assert (!frame.is_log (key));
    daisy_assert (frame.type_size (key) == Attribute::Variable);

    const std::vector<boost::shared_ptr<const FrameSubmodel>/**/>& layers 
      = frame.submodel_sequence (key);

    bool ok = true;
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
	    msg.error (tmp.str ());
            ok = false;
	  }
      }
    return ok;
  }
} check_layers;

void 
Geometry::add_layer (Frame& frame, const symbol dimension, const Check& check,
                     const Attribute::category cat, 
                     const symbol description)
{
  frame.declare ("end", "cm", Check::negative (), Attribute::Const, 
	     "End point of this layer (a negative number).");
  if (dimension == Attribute::Fraction ())
    frame.declare_fraction ("value", Attribute::Const, description);
  else
    frame.declare ("value", dimension, check, Attribute::Const, description);
  frame.order ("end", "value");
}

void 
Geometry::add_layer (Frame& frame, const Attribute::category cat, 
                     const symbol name,
                     load_syntax_t load_syntax)
{
  const FrameSubmodel& child = *Librarian::submodel_frame (load_syntax).get ();
  const symbol description = child.description ("value");
  const symbol dimension = child.dimension ("value");

  // initial_X
  const std::string iname = "initial_" + name;
  frame.declare_submodule_sequence (iname, Attribute::OptionalConst, "\
Initial value of the '" + name + "' parameter.\n\
The initial value is given as a sequence of (END VALUE) pairs, starting\n\
from the top and going down.  The parameter will be initialized to\n\
VALUE from the END of the previous layer, to the END of the current layer.\n\
Only used if '" + name + "' is unspecified.",
				    load_syntax);
  frame.set_check (iname, check_layers);

  // initial_X_plf
  const std::string pname = "initial_" + name + "_plf";
  frame.declare (pname, "cm", dimension,  Attribute::OptionalConst, "\
Initial value of the '" + name + "' parameter.\n\
The initial value is given as a sequence of (DEPTH VALUE) pairs, starting\n\
from the bottom and going up.  The parameter will be initialized to the\n\
the value of the PLF at the center of the cell.\n\
Only used if '" + name + "' and '" + iname + "' are unspecified.");

  // X
  if (dimension == Attribute::Fraction ())
    frame.declare_fraction (name, cat, Attribute::SoilCells, description);
  else
    frame.declare (name, dimension, cat, Attribute::SoilCells, description);
}

void 
Geometry::initialize_layer (std::vector<double>& array, 
                            const Frame& al, symbol name, Treelog& out) const
{
  const std::string initial = std::string ("initial_") + name;
  const std::string initial_plf = std::string ("initial_") + name + "_plf";
  daisy_assert (array.size () == 0);
  array.reserve (cell_size ());
  if (al.check (name))
    // Specified by user.
    array = al.number_sequence (name);
  else if (al.check (initial))
    {
      // Initialize by layers.
      const std::vector<boost::shared_ptr<const FrameSubmodel>/**/>& layers
	= al.submodel_sequence (initial);
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
  else if (al.check (initial_plf))
    {
      // Initialize by layers.
      const PLF plf = al.plf (initial_plf);
      for (size_t cell = 0; cell < cell_size (); cell++)
	array.push_back (plf (cell_z (cell)));
      daisy_assert (array.size () == cell_size ());
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

void
Geometry::build_common ()
{
  // Cell edges.
  cell_edges_.insert (cell_edges_.end (), cell_pseudo_size (),
                      std::vector<size_t> ());
  for (size_t e = 0; e < edge_size (); e++)
    { 
      cell_edges_[cell_pseudo_number (edge_from (e))].push_back (e);
      cell_edges_[cell_pseudo_number (edge_to (e))].push_back (e);
    }

  // Area per length.
  daisy_assert (edge_area_.size () == edge_size ());
  daisy_assert (edge_length_.size () == edge_size ());

  for (size_t e = 0; e < edge_size (); e++)
    { 
      const double length = edge_length (e);
      daisy_assert (length > 0.0);
      edge_area_per_length_.push_back (edge_area (e) / length);
    }
}

Geometry::Geometry (const Block&)
  : size_ (0)
{ }

Geometry::~Geometry ()
{ }

// Utilities.

static void
biopore_pass_below5 (const Geometry& geo,
                    const std::vector<double>& from_matrix,
                    const size_t edge_above,
                    const int cell_above,
                    std::vector<double>& flux)
{
  const double flux_above = flux[edge_above];
  const int cell = geo.edge_other (edge_above, cell_above);
  if (!geo.cell_is_internal (cell))
    return;
  const double volume = geo.cell_volume (cell);
  const double area_above = geo.edge_area (edge_above);
  const double volume_below = flux_above * area_above 
    - from_matrix[cell] * volume;
  const std::vector<size_t>& cell_edges = geo.cell_edges (cell);
  const size_t cell_edges_size = cell_edges.size ();
  size_t lowest_edge = edge_above;
  double lowest_z = geo.cell_z (cell);
  for (size_t i = 0; i < cell_edges_size; i++)
    {
      const size_t edge_below = cell_edges[i];
      const int cell_below = geo.edge_other (edge_below, cell);
      if (cell_below == Geometry::cell_below)
        {
          lowest_edge = edge_below;
          break;
        }
      if (!geo.cell_is_internal (cell_below))
        continue;
      const double z = geo.cell_z (cell_below);
      if (z < lowest_z)
        {
          lowest_z = z;
          lowest_edge = edge_below;
        }
    }
  daisy_assert (iszero (flux[lowest_edge]));
  const double area_below = geo.edge_area (lowest_edge);
  const double flux_below = volume_below / area_below;
  flux[lowest_edge] = flux_below;
  biopore_pass_below5 (geo, from_matrix, lowest_edge, cell, flux);
}

void
Geometry::biopore_pass_below (const std::vector<double>& from_matrix,
			      std::vector<double>& flux) const
{
  // Update flux
  const std::vector<size_t>& edge_above = cell_edges (Geometry::cell_above);
  const size_t edge_above_size = edge_above.size ();
  for (size_t i = 0; i < edge_above_size; i++)
    {
      const size_t edge = edge_above[i];
      biopore_pass_below5 (*this, from_matrix, edge, 
			   Geometry::cell_above, flux);
    }
}

static void 
biopore_pass_pipe_below (const Geometry& geo,
                         const double pipe_position,
                         const std::vector<double>& from_matrix,
                         const size_t edge_above,
                         const int cell_above,
                         std::vector<double>& flux,
                         std::vector<double>& S_from_drain)
{
  const int cell = geo.edge_other (edge_above, cell_above);
  if (!geo.cell_is_internal (cell))
    return;
  const double flux_above = flux[edge_above];
  const double S = from_matrix[cell];
  const double volume = geo.cell_volume (cell);
  const double area_above = geo.edge_area (edge_above);
  if (geo.cell_bottom (cell) <= pipe_position)
    // drain cell.
    {
      daisy_assert (iszero (S_from_drain[cell]));
      S_from_drain[cell] -= flux_above * area_above / volume - S;
      return;
    }
  // Not a drain cell.
  const double volume_below = flux_above * area_above - S * volume;
  const std::vector<size_t>& cell_edges = geo.cell_edges (cell);
  const size_t cell_edges_size = cell_edges.size ();
  size_t lowest_edge = edge_above;
  double lowest_z = geo.cell_z (cell);
  for (size_t i = 0; i < cell_edges_size; i++)
    {
      const size_t edge_below = cell_edges[i];
      const int cell_below = geo.edge_other (edge_below, cell);
      if (cell_below == Geometry::cell_below)
        {
          lowest_edge = edge_below;
          break;
        }
      if (!geo.cell_is_internal (cell_below))
        continue;
      const double z = geo.cell_z (cell_below);
      if (z < lowest_z)
        {
          lowest_z = z;
          lowest_edge = edge_below;
        }
    }
  daisy_assert (iszero (flux[lowest_edge]));
  const double area_below = geo.edge_area (lowest_edge);
  const double flux_below = volume_below / area_below;
  flux[lowest_edge] = flux_below;
  biopore_pass_pipe_below (geo, pipe_position, from_matrix, lowest_edge, cell,
                           flux, S_from_drain);
}

static void 
biopore_pass_pipe_above (const Geometry& geo,
                         const double pipe_position,
                         const std::vector<double>& from_matrix,
                         const size_t edge_below,
                         const int cell_below,
                         std::vector<double>& flux,
                         std::vector<double>& S_from_drain)
{
  const int cell = geo.edge_other (edge_below, cell_below);
  if (!geo.cell_is_internal (cell))
    return;
  const double flux_below = flux[edge_below];
  const double S = from_matrix[cell];
  const double volume = geo.cell_volume (cell);
  const double area_below = geo.edge_area (edge_below);
  if (geo.cell_top (cell) > pipe_position)
    // drain cell.
    {
      // S already counted in pass_below.
      S_from_drain[cell] -= flux_below * area_below / volume /* - S */;
      return;
    }
  // Not a drain cell.
  const double volume_above = flux_below * area_below - S * volume;
  const std::vector<size_t>& cell_edges = geo.cell_edges (cell);
  const size_t cell_edges_size = cell_edges.size ();
  size_t highest_edge = edge_below;
  double highest_z = geo.cell_z (cell);
  for (size_t i = 0; i < cell_edges_size; i++)
    {
      const size_t edge_above = cell_edges[i];
      const int cell_above = geo.edge_other (edge_above, cell);
      if (cell_above == Geometry::cell_above)
        {
          highest_edge = edge_above;
          break;
        }
      if (!geo.cell_is_internal (cell_above))
        continue;
      const double z = geo.cell_z (cell_above);
      if (z > highest_z)
        {
          highest_z = z;
          highest_edge = edge_above;
        }
    }
  daisy_assert (iszero (flux[highest_edge]));
  const double area_above = geo.edge_area (highest_edge);
  const double flux_above = volume_above / area_above;
  flux[highest_edge] = flux_above;
  biopore_pass_pipe_above (geo, pipe_position, from_matrix, highest_edge, cell,
                           flux, S_from_drain);
}

void
Geometry::biopore_pass_pipes (const double pipe_position,
			      const std::vector<double>& from_matrix,
			      std::vector<double>& flux,
			      std::vector<double>& S_from_drain) const
{
  // Update flux starting from the top.
  const std::vector<size_t>& edge_above 
    = cell_edges (Geometry::cell_above);
  const size_t edge_above_size = edge_above.size ();
  for (size_t i = 0; i < edge_above_size; i++)
    {
      const size_t edge = edge_above[i];
      biopore_pass_pipe_below (*this, pipe_position, from_matrix,
                               edge, Geometry::cell_above,
                               flux, S_from_drain);
    }
  // Update flux starting from the bottom.
  const std::vector<size_t>& edge_below 
    = cell_edges (Geometry::cell_below);
  const size_t edge_below_size = edge_below.size ();
  for (size_t i = 0; i < edge_below_size; i++)
    {
      const size_t edge = edge_below[i];
      // May not be zero if drain connected biopores with drain below bottom
      // daisy_assert (iszero (flux[edge]));
      biopore_pass_pipe_above (*this, pipe_position, from_matrix,
                               edge, Geometry::cell_below, 
                               flux, S_from_drain);
    }
}

// geometry.C ends here.
