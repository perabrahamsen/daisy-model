// pedo.C --- Pedotransfer functions based on soil attributes.
// 
// Copyright 2002 Per Abrahamsen and KVL.
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


#include "pedo.h"

EMPTY_TEMPLATE
Librarian<Pedotransfer>::Content* Librarian<Pedotransfer>::content = NULL;

const char *const Pedotransfer::description = "\
Pedotransfer functions based on soil attributes.";

bool
Pedotransfer::check (const Soil&, Treelog&) const
{ return true; }

void
Pedotransfer::initialize (const Soil&)
{ }

void
Pedotransfer::load_syntax (Syntax& syntax, AttributeList&)
{
  syntax.add ("description", Syntax::String, Syntax::OptionalConst,
	      "Description of this parameterization."); 
}

Pedotransfer::Pedotransfer (const AttributeList& al)
  : name (al.identifier ("type"))
{ }

Pedotransfer::~Pedotransfer ()
{ }
