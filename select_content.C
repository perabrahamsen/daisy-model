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
#include "soil.h"
#include "check.h"

struct SelectContent : public SelectValue
{
  // Content.
  const double height;

  // Output routines.
  void output_array (const std::vector<double>& array, 
		     const Soil* soil, Treelog&)
  { add_result (array[soil->interval_plus (height)]); }

  // Create and Destroy.
  SelectContent (const Block& bl)
    : SelectValue (bl),
      height (bl.alist ().number ("height"))
    { }
};

static struct SelectContentSyntax
{
  static Select& make (const Block& bl)
    { return *new SelectContent (bl); }

  SelectContentSyntax ()
    { 
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      SelectValue::load_syntax (syntax, alist);

      alist.add ("description", "Extract content at specified height.");
      syntax.add ("height", "cm", Check::non_positive (), Syntax::Const,
		  "Specify height (negative) to measure content.");

      Librarian<Select>::add_type ("content", alist, syntax, &make);
    }
} Select_syntax;
