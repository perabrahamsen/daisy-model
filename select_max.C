// select_max.C --- Select a state variable.
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

struct SelectMax : public SelectValue
{
  // Output routines.
  void output_number (symbol name, double number)
    { 
      if (!valid (name))
	return;

      if (count == 0)
	value = number;
      else if (number > value)
	value = number;
      count++;
    }

  // Create and Destroy.
  SelectMax (const AttributeList& al)
    : SelectValue (al)
    { }
};

static struct SelectMaxSyntax
{
  static Select& make (const AttributeList& al)
    { return *new SelectMax (al); }

  SelectMaxSyntax ()
    { 
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      SelectValue::load_syntax (syntax, alist);
      alist.add ("description", "Extract maximum value.");

      Librarian<Select>::add_type ("max", alist, syntax, &make);
    }
} Select_syntax;
