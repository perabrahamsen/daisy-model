// select_content.C --- Select a state variable.
// 
// Copyright 1996-2002 Per Abrahamsen and Søren Hansen
// Copyright 2000-2002 KVL.
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


#include "select_value.h"
#include "geometry.h"
#include "soil.h"
#include "check.h"
#include "mathlib.h"

struct SelectContent : public SelectValue
{
  // Content.
  const double height;
  const Geometry* old_geo;
  const Soil* old_soil;
  std::vector<size_t> cell;     // Cells at height.
  std::vector<double> weight;   // Relative volume for cell.

  // Output routines.
  void output_array (const std::vector<double>& array, 
		     const Geometry* geo, const Soil* soil, Treelog&)
  { 
    if (soil != old_soil)
        old_soil = soil;

    if (geo != old_geo)
      {
        old_geo = geo;

        cell.erase (cell.begin (), cell.end ());
        double total_volume = 0.0;
        const size_t cell_size = geo->cell_size ();
        for (size_t i = 0; i < cell_size; i++)
          if (geo->contain_z (i, height))
            {
              const double volume = geo->volume (i);
              total_volume += volume;
              weight.push_back (volume);
              cell.push_back (i);
            }
        daisy_assert (total_volume > 0.0);
        for (size_t i = 0; i < cell.size (); i++)
          weight[i] /= total_volume;
      }
    
    double result = 0.0;
    for (size_t i = 0; i < cell.size (); i++)
      result += array[cell[i]] * weight[i];
    add_result (result); 
  }

  // Create and Destroy.
  SelectContent (Block& al)
    : SelectValue (al),
      height (al.number ("height")),
      old_geo (NULL),
      old_soil (NULL)
    { }
};

static struct SelectContentSyntax
{
  static Select& make (Block& al)
    { return *new SelectContent (al); }

  SelectContentSyntax ()
    { 
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      SelectValue::load_syntax (syntax, alist);

      alist.add ("description", "Extract content at specified height.");
      syntax.add ("height", "cm", Check::non_positive (), Syntax::Const,
		  "Specify height (negative) to measure content.\n\
The value willbe a weighted average of all cells containing height.");

      Librarian<Select>::add_type ("content", alist, syntax, &make);
    }
} Select_syntax;
