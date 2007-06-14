// select_number.C --- Select a state variable.
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
#include "syntax.h"
#include "alist.h"
#include "librarian.h"

struct SelectNumber : public SelectValue
{
  // Output routines.
  void output_number (double number)
  { add_result (number); }
  void output_integer (int integer)
  { output_number (integer); }

  // Create and Destroy.
  SelectNumber (Block& al)
    : SelectValue (al)
  { }
};

static struct SelectNumberSyntax
{
  static Model& make (Block& al)
  { return *new SelectNumber (al); }

  SelectNumberSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    SelectValue::load_syntax (syntax, alist);
    alist.add ("description", "Extract specified number.");

    Librarian::add_type (Select::component, "number", alist, syntax, &make);
  }
} Select_syntax;
