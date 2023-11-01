// select_array.C --- Log a state array.
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
// Copyright 2006, 2008 Per Abrahamsen and KVL.
// Copyright 2010 KU
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

struct SelectArray : public Select
{
  int type_size () const
  { return original_size (); }

  // Content.
  bool found;                      // Value found since last print.
  std::vector<double> small_value; // Value in small timestep.
  double dt;                    // Time passed since last print [h]
  std::vector<double> value;		// Total array.
  std::vector<double> result;        // For logging (needs to be persistent!).
  std::vector<double> bulk;     // Soil dry bulk density.
  
  const Column* last_column;

  // Bulk density convertions.
  std::unique_ptr<BD_convert> bd_convert;
  const Convert* special_convert (const Units& units, 
                                  const symbol has, const symbol want)
  {
    daisy_assert (!bd_convert.get ());
    static const symbol bulk_density ("g/cm^3");
    const symbol bulk_dim = default_dimension (bulk_density);
    if (units.can_convert (has, bulk_dim)
        && units.can_convert (Units::dry_soil_fraction (), want))
      bd_convert.reset (new BD_convert (units, has, want, bulk_dim));
    return bd_convert.get ();
  }

  // Output routines.
  void set_column (const Column& column, Treelog&)
  { 
    if (&column != last_column)
      {
        last_column = &column;

        const Soil& soil = column.get_soil ();

        // Make room.
        if (soil.size () > bulk.size ())
          bulk.insert (bulk.end (), soil.size () - bulk.size (), 0.0);
        
        for (size_t c = 0; c < soil.size (); c++)
          bulk[c] = soil.dry_bulk_density (c);
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
    int size = 1;
    switch (type_size ())
      {
      case Attribute::SoilCells:
        if (geo)
          size = geo->cell_size ();
        break;
      case Attribute::SoilEdges:
        if (geo)
          size = geo->edge_size ();
      }
    daisy_assert (size >= 0);
    for (size_t i = 0; i < size; i++)
      dest.missing ();
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

    // Make sure result has the right size.
    if (result.size () != value.size ())
      result = value;

    if (bd_convert.get ())  // Bulk density convertion.
      {
        if (bulk.size () < value.size ())
          throw "\
The 'array' select model only handle bulk density for soil sized variables";

        switch (handle)
          {
          case Handle::average:
            if (dt > 0.0)
              {
                for (size_t i = 0; i < value.size (); i++)
                  {
                    bd_convert->set_bulk (bulk[i]);
                    result[i] = convert (value[i] / dt);
                  }
                break;
              }
            print_missing ();
            return;
          default:
            for (size_t i = 0; i < value.size (); i++)
              {
                bd_convert->set_bulk (bulk[i]);
                result[i] = convert (value[i]);
              }
          }
      }
    else                    // No bd_convert.
      switch (handle)
        {
        case Handle::average:
          if (dt > 0.0)
            {
              for (size_t i = 0; i < value.size (); i++)
                result[i] = convert (value[i] / dt);
              break;
            }
          print_missing ();
          return;
        default:
          for (size_t i = 0; i < value.size (); i++)
            result[i] = convert (value[i]);
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

  symbol cell_tag (size_t i) const
  {
    const Geometry* geo = geometry ();
    static const symbol empty_symbol ("");
    std::ostringstream tmp;
    if (tag () != empty_symbol)
      tmp << tag () << " @ ";
    tmp << geo->cell_name (i);
    return tmp.str ();
  }

  symbol edge_tag (size_t i) const
  {
    const Geometry* geo = geometry ();
    static const symbol empty_symbol ("");
    std::ostringstream tmp;
    if (tag () != empty_symbol)
      tmp << tag () << " @ ";
    tmp << geo->edge_name (i);
    return tmp.str ();
  }

  const Geometry* geometry () const
  { 
    if (last_column)
      return &last_column->get_geometry ();
    return NULL;
  }

  int size () const
  { return value.size (); }

  // Create and Destroy.
  SelectArray (const BlockModel& al)
    : Select (al),
      found (false),
      dt (0.0),
      last_column (NULL)
      // , bd_convert (NULL)
  { }
};

static struct SelectArraySyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new SelectArray (al); }
  SelectArraySyntax ()
    : DeclareModel (Select::component, "array", "Log all members of an array.")
  { }
  void load_frame (Frame& frame) const
  { }
} SelectArray_syntax;

// select_array.C ends here.
