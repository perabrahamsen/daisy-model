// dom.C --- A single dissolved organic matter pool.
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


#include "dom.h"
#include "submodel.h"
#include "alist.h"

void 
DOM::load_syntax (Syntax& syntax, AttributeList& alist)
{
  OM::load_syntax (syntax, alist); 
  alist.add ("submodel", "DOM");
  alist.add ("description", "\
A single Dissolved Organic Matter pool.");
}

DOM::DOM (const AttributeList& al)
  : OM (al)
{ }

static Submodel::Register dom_submodel ("DOM", DOM::load_syntax);
