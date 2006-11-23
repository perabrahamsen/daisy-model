// organic_matter.C -- Soil organic matter.
// 
// Copyright 2006 Per Abrahamsen and KVL.
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

#include "organic_matter.h"
#include "block.h"

template<>
Librarian<OrganicMatter>::Content* Librarian<OrganicMatter>::content = NULL;

const char *const OrganicMatter::description = "\
Turnover of organic matter in the soil.";

OrganicMatter::OrganicMatter (Block& al)
  : name (al.identifier ("type"))
{ }

OrganicMatter::~OrganicMatter ()
{ }
