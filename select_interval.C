// select_interval.C --- Select a state variable.
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


#include "select.h"
#include "geometry.h"
#include "units.h"

struct SelectInterval : public Select
{
  // Content.
  const bool density;
  double from;
  double to;
  double value;	

  // Output routines.

  void output_array (symbol name, const vector<double>& array, 
		     const Geometry* geometry)
    { 
      if (valid (name))
	{
	  if (to > 0.0)
	    to = geometry->zplus (geometry->size () - 1);

	  if (count == 0)	 
	    value = geometry->total (array, from, to);	
	  else
	    value += geometry->total (array, from, to);
	  count++;
	}
    }

  // Print result at end of time step.
  void done (Destination& dest)
    {
      if (count == 0)
	dest.missing (tag ());
      else if (density)
	dest.add (tag (), convert (value / (from - to)));
      else
	dest.add (tag (), convert (value));

      if (!accumulate)
	count = 0;
    }

  // Create and Destroy.
  const string default_dimension (const string& spec_dim) const
  { 
    if (density)
      return spec_dim;
    
    return Units::multiply (spec_dim, "cm");
  }

  void initialize (const string_map& conv, 
		   double default_from, double default_to, 
		   const string& timestep)
    {
      Select::initialize (conv, default_from, default_to, timestep);

      // Set default range.
      if (default_from <= 0.0 && from > 0.0)
	from = default_from;
      if (default_to <= 0.0 && to > 0.0)
	to = default_to;

      if (from > 0.0)
	from = 0.0;
    }
  SelectInterval (const AttributeList& al)
    : Select (al),
      density (al.flag ("density")),
      from (al.check ("from") ? al.number ("from") : 1.0),
      to (al.check ("to") ? al.number ("to") : 1.0),
      value (al.number ("value"))
    { }
};

static struct SelectIntervalSyntax
{
  static Select& make (const AttributeList& al)
    { return *new SelectInterval (al); }

  SelectIntervalSyntax ()
    { 
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      Select::load_syntax (syntax, alist);
      alist.add ("description", "Summarize specified interval.");

      syntax.add ("density", Syntax::Boolean, Syntax::Const, 
		  "If true, divide value with interval height.");
      alist.add ("density", false);
      syntax.add ("from", "cm", Syntax::OptionalConst,
		  "Specify height (negative) to measure from.\n\
By default, measure from the top.");
      syntax.add ("to", "cm", Syntax::OptionalConst,
		  "Specify height (negative) to measure interval.\n\
By default, measure to the bottom.");
      syntax.add ("value", Syntax::Unknown (), Syntax::State,
		  "The current accumulated value.");
      alist.add ("value", 0.0);

      Librarian<Select>::add_type ("interval", alist, syntax, &make);
    }
} Select_syntax;
