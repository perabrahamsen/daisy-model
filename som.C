// som.C --- A single soil organic matter pool.
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


#include "som.h"
#include "submodel.h"
#include "alist.h"

void 
SOM::load_syntax (Syntax& syntax, AttributeList& alist)
{
  OM::load_syntax (syntax, alist); 
  alist.add ("submodel", "SOM");
  alist.add ("description", "\
A single Soil Organic Matter pool.");
}

SOM::SOM (const AttributeList& al)
  : OM (al)
{ }

static Submodel::Register som_submodel ("SOM", SOM::load_syntax);
