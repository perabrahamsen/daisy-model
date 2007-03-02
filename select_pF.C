// select_pF.C --- Select a state variable.
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
#include "block.h"
#include "alist.h"
#include "mathlib.h"
#include "check.h"
#include "vcheck.h"

using namespace std;

struct SelectPF : public Select
{
  type_t type () const
  { return NumberSequence; }

  // Content.
  const double max_h;
  vector<double> value;		// Total array.
  const Geometry* last_geo; // For printing dimensions;

  double cm2pF (double cm)
  { return h2pF (min (cm, max_h)); }
    
  // Output routines.
  void output_array (const vector<double>& array, 
		     const Geometry* geo, const Soil*, Treelog&)
  { 
    if (geo)
      last_geo = geo;

    if (array.size () > value.size ())
      value.insert (value.end (), 
		    array.size () - value.size (),
		    0.0);
    if (count == 0)
      for (unsigned int i = 0; i < array.size (); i++)
	value[i] = cm2pF (array[i]);
    else
      for (unsigned int i = 0; i < array.size (); i++)
	value[i] += cm2pF (array[i]);
    count++;
  }

  // Print result at end of time step.
  void done (const double)
  {
    if (count == 0)
      dest.missing ();
    else 
      dest.add (value);

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
  SelectPF (Block& al)
    : Select (al),
      max_h (al.number ("max_h")),
      value (al.number_sequence ("value")),
      last_geo (NULL)
  { }
};

static struct SelectPFSyntax
{
  static Select& make (Block& al)
  { return *new SelectPF (al); }

  static bool check_alist (const AttributeList& al, Treelog& err)
  {
    bool ok = true;

    if (al.name ("dimension") != "pF")
      {
        err.error ("This select type can only show numbers as pF");
        ok = false;
      }
    return ok;
  }

  SelectPFSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    Select::load_syntax (syntax, alist);
    static VCheck::Enum current_only ("current");
    syntax.add_check ("handle", current_only);
    syntax.add_check (check_alist);
    alist.add ("description", "Extract pF for all array points.\n\
The original dimension is assumed to be in cm, no matter what is specified.");

    syntax.add ("max_h", "cm", Check::negative (), Syntax::Const, 
                "Maximum water pressure in log.\n\
Pressure above this value will be represented as this value.");
    alist.add ("max_h", -0.1);
    syntax.add ("value", Syntax::Unknown (), Syntax::State, Syntax::Sequence,
		"The current accumulated value.");
    vector<double> empty;
    alist.add ("value", empty);
    alist.add ("dimension", "pF");

    Librarian<Select>::add_type ("pF", alist, syntax, &make);
  }
} Select_syntax;
