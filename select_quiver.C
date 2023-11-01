// select_quiver.C --- Log a SoilEdges array as a vector quiver.
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
// Copyright 2006, 2008 Per Abrahamsen and KVL.
// Copyright 2010, 2021 KU
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

#include "select.h"
#include "soil.h"
#include "bdconv.h"
#include "block_model.h"
#include "mathlib.h"
#include "librarian.h"
#include "frame.h"
#include "column.h"
#include "geometry.h"
#include <sstream>

struct SelectQuiver : public Select
{
  int type_size () const
  { return original_size (); }

  // Content.
  bool found;                      // Value found since last print.
  std::vector<double> small_value; // Value in small timestep.
  double dt;			   // Time passed since last print [h]
  std::vector<double> value;	   // Total array.
  std::vector<double> result;      // For logging (needs to be persistent!).
  
  const Column* last_column;
  
  // Output routines.
  void set_column (const Column& column, Treelog&)
  {
    if (last_column != &column)
      {
	last_column = &column;
	result = std::vector<double> (geometry ()->cell_size () * 2, 0.0);
      }
    
  }

  void output_array (const std::vector<double>& array)
  { 
    if (array.size () > small_value.size ())
      small_value.insert (small_value.end (), 
                          array.size () - small_value.size (),
                          0.0);

    switch (multi)
      {
      case Multi::min:
        if (first_result)
          for (size_t i = 0; i < array.size (); i++)
            small_value[i] = array[i];
        else
          for (size_t i = 0; i < array.size (); i++)
            small_value[i] = std::min (small_value[i], array[i]);
        break;
      case Multi::max:
        if (first_result)
          for (size_t i = 0; i < array.size (); i++)
            small_value[i] = array[i];
        else
          for (size_t i = 0; i < array.size (); i++)
            small_value[i] = std::max (small_value[i], array[i]);
        break;
      case Multi::sum:
        for (size_t i = 0; i < array.size (); i++)
          small_value[i] += array[i] * relative_weight;
        break;
      }
    
    first_result = false;
  }

  void done_small (const double ddt)
  {
    dt += ddt;
    if (first_result)
      return;

    if (small_value.size () > value.size ())
      value.insert (value.end (), 
		    small_value.size () - value.size (),
		    0.0);

    switch (handle)
      {
      case Handle::average:
      case Handle::sum:
        if (ddt > 0.0)
          {
            for (size_t i = 0; i < small_value.size (); i++)
              value[i] += small_value[i] * ddt;
            first_small = false;
          }
        break;
      case Handle::content_sum:
        for (size_t i = 0; i < small_value.size (); i++)
          value[i] += small_value[i];
        first_small = false;
        break;
      case Handle::min:
        if (first_small)
          value = small_value;
        else
          {
            for (size_t i = 0; i < small_value.size (); i++)
              value[i] = std::min (value[i], small_value[i]);
            first_small = false;
          }
        break;
      case Handle::max:
        if (first_small)
          value = small_value;
        else
          {
            for (size_t i = 0; i < small_value.size (); i++)
              value[i] = std::max (value[i], small_value[i]);
            first_small = false;
          }
        break;
      case Handle::current:
        value = small_value;
        first_small = false;
        break;
      }
    
    std::fill (small_value.begin (), small_value.end (), 0.0);
    first_result = true;
  }

  void done_initial ()
  {
    if (first_result)
      return;

    if (small_value.size () > value.size ())
      value.insert (value.end (), 
		    small_value.size () - value.size (),
		    0.0);

    switch (handle)
      {
      case Handle::average:
      case Handle::sum:
        break;
      case Handle::content_sum:
        for (size_t i = 0; i < small_value.size (); i++)
          value[i] += small_value[i];
        first_small = false;
        break;
      case Handle::min:
        if (first_small)
          value = small_value;
        else
          {
            for (size_t i = 0; i < small_value.size (); i++)
              value[i] = std::min (value[i], small_value[i]);
            first_small = false;
          }
        break;
      case Handle::max:
        if (first_small)
          value = small_value;
        else
          {
            for (size_t i = 0; i < small_value.size (); i++)
              value[i] = std::max (value[i], small_value[i]);
            first_small = false;
          }
        break;
      case Handle::current:
        value = small_value;
        first_small = false;
        break;
      }
    
    std::fill (small_value.begin (), small_value.end (), 0.0);
    first_result = true;
  }

  void print_missing ()
  {
    const Geometry *const geo = geometry ();
    if (geo)
      {
	const size_t size = 2 * geo->cell_size ();
	for (size_t i = 0; i < size; i++)
	  dest.missing ();
      }
    else
      dest.add ("no value, no geometry");
  }

  // Print result at end of time step.
  void done_print ()
  {
    // Missing value.
    if (first_small)
      {
        print_missing ();
        return;
      }

    const Geometry *const geo = geometry ();
    if (!geo)
      {
	dest.add ("no value, no geometry");
	return;
      }
    const size_t cell_size = geo->cell_size ();
    const size_t edge_size = geo->edge_size ();
    daisy_assert (value.size () == edge_size);

    // Make sure result has the right size.
    if (result.size () != 2 * cell_size)
      result = std::vector<double> (2 * cell_size, 0.0);


    std::vector<double> handled = value;
    if (handle == Handle::average)
      {
	if (dt > 0.0)
	  for (size_t e = 0; e < handled.size (); e++)
	    handled[e] /= dt;
	else
	  {
	    print_missing ();
	    return;
	  }
      }
    
    std::vector<double> weight_z (cell_size, 0.0);
    std::vector<double> weight_x (cell_size, 0.0);

    for (size_t e = 0; e < value.size (); e++)
      {
	const double val = convert (handled[e]);
	const double dz = geo->edge_sin_angle (e);
	const double dx = geo->edge_cos_angle (e);

	auto add_cell = [&](int c)
	  {
	    if (geo->cell_is_internal (c))
	      {
		weight_z[c] += dz;
		weight_x[c] += dx;
		result[c] += val * dz;
		result[c + cell_size] += val * dx;
	      }
	  };
	add_cell (geo->edge_from (e));
	add_cell (geo->edge_to (e));
      }

    for (size_t c = 0; c < cell_size; c++)
      {
	const double dz = weight_z[c];
	const double dx = weight_x[c];
	daisy_assert (dz > 0.0);
	daisy_assert (dz > 0.0);
	result[c] /= dz;
	result[c + cell_size] /= dx;
      }
      
    dest.add (result);

    // Clear for next.
    if (!accumulate)
      {
        dt = 0.0;
        std::fill (value.begin (), value.end (), 0.0);
        first_small = true;
      }
  }

  symbol raw_tag (size_t i) const
  {
    std::ostringstream tmp;
    tmp << tag () << "[" << i << "]";
    return tmp.str ();
  }

  const Geometry* geometry () const
  { 
    if (last_column)
      return &last_column->get_geometry ();
    return NULL;
  }

  int size () const
  { return result.size (); }

  // Create and Destroy.
  SelectQuiver (const BlockModel& al)
    : Select (al),
      found (false),
      dt (0.0),
      last_column (NULL)
  { }
};

static struct SelectQuiverSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new SelectQuiver (al); }
  SelectQuiverSyntax ()
    : DeclareModel (Select::component, "quiver", "\
Convert edge flow to node based flow vectors.")
  { }
  void load_frame (Frame& frame) const
  { }
} SelectQuiver_syntax;

// select_quiver.C ends here.
