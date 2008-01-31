// select_array.C --- Log a state array.
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
// Copyright 2006, 2008 Per Abrahamsen and KVL.
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
#include "block.h"
#include "alist.h"
#include "mathlib.h"
#include "librarian.h"

struct SelectArray : public Select
{
  type_t type () const
  { return NumberSequence; }

  // Content.
  std::vector<double> value;		// Total array.
  std::vector<double> result;        // For logging (needs to be persistent!).
  std::vector<double> bulk;     // Soil dry bulk density.
  
  const Geometry* last_geo;     // For printing dimensions;
  const Soil* last_soil;        // For dry bulk density;

  // Bulk density convertions.
  std::auto_ptr<BD_convert> bd_convert;
  const Units::Convert* special_convert (const symbol has, const symbol want)
  {
    daisy_assert (!bd_convert.get ());
    static const symbol bulk_density ("g/cm^3");
    const symbol bulk_dim = default_dimension (bulk_density);
    if (Units::can_convert (has, bulk_dim)
        && Units::can_convert (Syntax::fraction (), want))
      bd_convert.reset (new BD_convert (has, want, bulk_dim));
    return bd_convert.get ();
  }

  // Output routines.
  void output_array (const std::vector<double>& array, 
		     const Geometry* geo, const Soil* soil, const Vegetation*, 
		     Treelog&)
  { 
    if (geo != last_geo)
      last_geo = geo;

    if (soil != last_soil)
      {
        last_soil = soil;       // Remember it.

        if (bd_convert.get ())
          if (!soil)
            throw "Soil required for this use of 'array' select model";
          else
            {
              // Make room.
              if (soil->size () > bulk.size ())
                bulk.insert (bulk.end (), soil->size () - bulk.size (), 0.0);

              for (size_t c = 0; c < soil->size (); c++)
                bulk[c] = soil->dry_bulk_density (c);
            }
      }

    if (array.size () > value.size ())
      value.insert (value.end (), 
		    array.size () - value.size (),
		    0.0);
    if (count == 0)
      {
        if (handle == Handle::geometric)
          for (unsigned int i = 0; i < array.size (); i++)
            value[i] = log (array[i]);
        else
          for (unsigned int i = 0; i < array.size (); i++)
            value[i] = array[i];
      }
    else switch (handle)
      {
      case Handle::min:
        for (unsigned int i = 0; i < array.size (); i++)
          value[i] = std::min (value[i], array[i]);
        break;
      case Handle::max:
        for (unsigned int i = 0; i < array.size (); i++)
          value[i] = std::max (value[i], array[i]);
        break;
      case Handle::current:    
        // We may have count > 0 && Handle::current when selecting
        // multiple items with "*", e.g. multiple SOM pools.  
        // In that case, we use the sum.
      case Handle::average:
      case Handle::sum:
        for (unsigned int i = 0; i < array.size (); i++)
          value[i] += array[i];
        break;
      case Handle::geometric:
        for (unsigned int i = 0; i < array.size (); i++)
          value[i] += log (array[i]);
        break;
      }
    count++;
  }

  // Print result at end of time step.
  void done (const double dt)
  {
    if (count == 0)
      dest.missing ();
    else 
      {
        // Make sure result has the right size.
        if (result.size () != value.size ())
          result = value;

        if (bd_convert.get ())  // Bulk density convertion.
          {
            if (bulk.size () < value.size ())
              throw "The 'array' select model only handle bulk density for soil sized variables";

            switch (handle)
              {
              case Handle::average:
                for (size_t i = 0; i < value.size (); i++)
                  {
                    bd_convert->set_bulk (bulk[i]);
                    result[i] = convert (value[i] / count);
                  }
                dest.add (result);
                break;
              case Handle::geometric:
                for (size_t i = 0; i < value.size (); i++)
                  {
                    bd_convert->set_bulk (bulk[i]);
                    result[i] = convert (exp (value[i] / count));
                  }
                dest.add (result);
                break;
              case Handle::sum:
                for (size_t i = 0; i < value.size (); i++)
                  {
                    bd_convert->set_bulk (bulk[i]);
                    result[i] = convert (value[i] * dt);
                  }
                dest.add (result);
                break;
              default:
                for (size_t i = 0; i < value.size (); i++)
                  {
                    bd_convert->set_bulk (bulk[i]);
                    result[i] = convert (value[i]);
                  }
                dest.add (result);
              }
          }
        else                    // No bd_convert.
          switch (handle)
            {
            case Handle::average:
              for (size_t i = 0; i < value.size (); i++)
                result[i] = convert (value[i] / count);
              dest.add (result);
              break;
            case Handle::geometric:
              for (size_t i = 0; i < value.size (); i++)
                result[i] = convert (exp (value[i] / count));
              dest.add (result);
              break;
            case Handle::sum:
              for (size_t i = 0; i < value.size (); i++)
                result[i] = convert (value[i] * dt);
              dest.add (result);
              break;
            default:
              for (size_t i = 0; i < value.size (); i++)
                result[i] = convert (value[i]);
              dest.add (result);
            }
      }
    if (!accumulate)
      count = 0;
  }

  bool prevent_printing ()
  { return count == 0; }

  const Geometry* geometry () const
  { return last_geo; }

  int size () const
  { return value.size (); }

  // Create and Destroy.
  SelectArray (Block& al)
    : Select (al),
      value (al.number_sequence ("value")),
      last_geo (NULL),
      last_soil (NULL),
      bd_convert (NULL)
  { }
};

static struct SelectArraySyntax
{
  static Model& make (Block& al)
  { return *new SelectArray (al); }

  SelectArraySyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    Select::load_syntax (syntax, alist);

    syntax.add ("value", Syntax::Unknown (), Syntax::State, Syntax::Sequence,
		"The current accumulated value.");
    std::vector<double> empty;
    alist.add ("value", empty);

    Librarian::add_type (Select::component, "array", alist, syntax, &make);
  }
} Select_syntax;
