// select_average.C --- Select a state variable.
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

struct SelectAverage : public Select
{
  // Content.
  double value;	

  // Output routines.
  void output_number (const string& name, double number)
    { 
      if (!is_active ())
	return;

      if (!valid (name))
	return;

      if (count == 0)
	value = number;
      else
	value += number;
      count++;
    }

  // Print result at end of time step.
  void done (Destination& dest)
    {
      if (count == 0)
	dest.missing (tag);
      else 
	dest.add (tag, (value/(double) count) * factor + offset);

      if (!accumulate)
	count = 0;
    }
  // Create and Destroy.
  SelectAverage (const AttributeList& al)
    : Select (al),
      value (al.number ("value"))
    { }
};

static struct SelectAverageSyntax
{
  static Select& make (const AttributeList& al)
    { return *new SelectAverage (al); }

  SelectAverageSyntax ()
    { 
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      Select::load_syntax (syntax, alist);
      alist.add ("description", "Extract average value.");

      syntax.add ("value", Syntax::Unknown (), Syntax::State,
		  "The current accumulated value.");
      alist.add ("value", 0.0);

      Librarian<Select>::add_type ("average", alist, syntax, &make);
    }
} Select_syntax;
