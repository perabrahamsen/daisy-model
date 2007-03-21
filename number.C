// number.C --- Numbers in Daisy.
// 
// Copyright 2004 Per Abrahamsen and KVL.
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


#include "number.h"
#include "block.h"

template<>
BuildBase* Librarian<Number>::content = NULL;

const char *const Number::description = "\
Generic representation of numbers.";

const char *const Number::component = "number";

symbol
Number::title () const
{ return name; }

bool 
Number::known (symbol dim)
{ return dim != Syntax::unknown (); }

Number::Number (Block& al)
  : name (al.identifier ("type"))
{ }

Number::~Number ()
{ }
