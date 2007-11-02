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

#define BUILD_DLL

#include "select_value.h"
#include "block.h"
#include "alist.h"
#include "geometry.h"
#include "soil.h"
#include "check.h"
#include "mathlib.h"
#include "librarian.h"

struct SelectContent : public SelectValue
{
  // Content.
  const bool has_z;
  const double z;
  const bool has_x;
  const double x;
  const bool has_y;
  const double y;
  const Geometry* old_geo;
  const Soil* old_soil;
  std::vector<size_t> cell;     // Cells at height.
  std::vector<double> weight;   // Relative volume for cell.

  // Output routines.
  void output_array (const std::vector<double>& array, 
		     const Geometry* geo, const Soil* soil, const Vegetation*, 
		     Treelog&)
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
	  {
	    bool include_cell = true;
	    if (has_z && !geo->contain_z (i, z))
	      include_cell = false;
	    else if (has_x && !geo->contain_x (i, x))
	      include_cell = false;
	    else if (has_y && !geo->contain_y (i, y))
	      include_cell = false;
	    
	    if (include_cell)
	      {
		const double volume = geo->cell_volume (i);
		total_volume += volume;
		weight.push_back (volume);
		cell.push_back (i);
	      }
	  }
        daisy_assert (total_volume > 0.0 || cell.size () == 0);
        for (size_t i = 0; i < cell.size (); i++)
          weight[i] /= total_volume;
      }
    const size_t cell_size = cell.size ();
    if (cell_size < 1)
      // No matching cells => missing value.
      return;

    double result = 0.0;
    for (size_t i = 0; i < cell_size; i++)
      result += array[cell[i]] * weight[i];
    add_result (result); 
  }

  // Create and Destroy.
  SelectContent (Block& al)
    : SelectValue (al),
      has_z (al.check ("z") || al.check ("height")),
      z (al.number ("z", al.number ("height", -42.42e42))),
      has_x (al.check ("x")),
      x (al.number ("x", -42.42e42)),
      has_y (al.check ("y")),
      y (al.number ("y", -42.42e42)),
      old_geo (NULL),
      old_soil (NULL)
  { }
};

#include <sstream>

static struct SelectContentSyntax
{
  static Model& make (Block& al)
  { return *new SelectContent (al); }

  static bool check_alist (const AttributeList& al, Treelog& msg)
  {
    if (al.check ("z") && al.check ("height"))
      msg.warning ("Paramater 'z' overwrites 'height'");

    return true;
  }
  SelectContentSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    SelectValue::load_syntax (syntax, alist);
    syntax.add_check (check_alist);

    alist.add ("description", "Extract content at specified location.\n\
The \"location\" may be a line, plane or volume if one or more dimension\n\
parameters are left out.  In that case, the weighted average is used.");
    syntax.add ("height", "cm", Check::non_positive (), Syntax::OptionalConst,
		"OBSOLETE: Use 'z' instead.");
    syntax.add ("z", "cm", Syntax::OptionalConst,
		"Specify height (negative below surface) to measure content.\n\
The value will be a weighted average of all cells containing height.\n\
By default, cell in all heights will be included.");
    syntax.add ("x", "cm", Syntax::OptionalConst,
		"Specify width (distance from left side) to measure content.\n\
The value will be a weighted average of all cells containing width.\n\
By default, cell in all widths will be included.");
    syntax.add ("y", "cm", Syntax::OptionalConst,
		"Specify length (distance from front) to measure content.\n\
The value will be a weighted average of all cells containing length.\n\
By default, cell in all lengths will be included.");

    Librarian::add_type (Select::component, "content", alist, syntax, &make);
  }
} Select_syntax;
