// select_flux_top.C --- Select a state variable.
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

struct SelectFluxTop : public SelectValue
{
  // Content.
  double height;

  // Output routines.
  void output_array (symbol name, const vector<double>& array, 
		     const Geometry* geometry)
    { 
      if (valid (name))
	{
	  int index = geometry->interval_border (height);

	  if (count == 0)	 
	    value = array[index];	
	  else
	    value += array[index];	
	  count++;
	}
    }

  // Create and Destroy.
  void initialize (const string_map& conv, 
		   double default_from, double default_to,
		   const string& timestep)
    {
      Select::initialize (conv, default_from, default_to, timestep);

      // Overwrite default height.
      if (default_from < 0.0)
	height = default_from;
    }
  SelectFluxTop (const AttributeList& al)
    : SelectValue (al),
      height (0.0)
    { }
};

static struct SelectFluxTopSyntax
{
  static Select& make (const AttributeList& al)
    { return *new SelectFluxTop (al); }

  SelectFluxTopSyntax ()
    { 
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      SelectValue::load_syntax (syntax, alist);

      alist.add ("description", "Extract flux at top of specified interval.\n\
By default, log the first member of the sequence.");

      Librarian<Select>::add_type ("flux_top", alist, syntax, &make);
    }
} Select_syntax;
