// select_array.C --- Select a state variable.
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

struct SelectArray : public Select
{
  // Content.
  vector<double> value;		// Total array.
  const Geometry* last_geometry; // For printing dimensions;

  // Output routines.
  void output_array (const string& name, const vector<double>& array, 
		     const Geometry* geometry)
  { 
    if (geometry)
      last_geometry = geometry;

    if (!is_active ())
      return;

    if (valid (name))
      {
	if (array.size () > value.size ())
	  value.insert (value.end (), 
			array.size () - value.size (),
			0.0);
	if (count == 0)
	  for (unsigned int i = 0; i < array.size (); i++)
	    value[i] = convert (array[i]);
	else
	  for (unsigned int i = 0; i < array.size (); i++)
	    value[i] += convert (array[i]);
	count++;
      }
  }

  // Print result at end of time step.
  void done (Destination& dest)
  {
    if (count == 0)
      dest.missing (tag ());
    else 
      dest.add (tag (), value);

    if (!accumulate)
      count = 0;
  }

  bool prevent_printing ()
  { return count == 0; }

  const Geometry* geometry () const
  { return last_geometry; }

  int size () const
  { return value.size (); }

  // Create and Destroy.
  SelectArray (const AttributeList& al)
    : Select (al),
      value (al.number_sequence ("value")),
      last_geometry (NULL)
  { }
};

static struct SelectArraySyntax
{
  static Select& make (const AttributeList& al)
  { return *new SelectArray (al); }

  SelectArraySyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    Select::load_syntax (syntax, alist);

    syntax.add ("value", Syntax::Unknown (), Syntax::State, Syntax::Sequence,
		"The current accumulated value.");
    vector<double> empty;
    alist.add ("value", empty);

    Librarian<Select>::add_type ("array", alist, syntax, &make);
  }
} Select_syntax;
