// select_index.C --- Select a state variable.
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

struct SelectIndex : public SelectValue
{
  // Index.
  const int index;

  // Output routines.
  void output_array (const string& name, const vector<double>& array, 
		     const Geometry*)
  { output (name, array); }

  void output (const string& name, const vector<double>& array)
  { 
    if (!is_active ())
      return;

    if (valid (name))
      {
	if (count == 0)	 
	  value = array[index];	
	else
	  value += array[index];	
	count++;
      }
  }

  // Create and Destroy.
  SelectIndex (const AttributeList& al)
    : SelectValue (al),
      index (al.integer ("index"))
  { }
};

static struct SelectIndexSyntax
{
  static Select& make (const AttributeList& al)
  { return *new SelectIndex (al); }

  SelectIndexSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    SelectValue::load_syntax (syntax, alist);

    alist.add ("description", "Extract content at specified array index.");
    syntax.add ("index", Syntax::Integer, Syntax::Const,
		"Specify array index to select.");

    Librarian<Select>::add_type ("index", alist, syntax, &make);
  }
} Select_syntax;
