// scope_id.C -- A name -> value map.
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


#include "scope_id.h"
#include "assertion.h"

void 
ScopeID::tick (const Scope&, Treelog&)
{ }

const std::vector<symbol>& 
ScopeID::all_numbers () const
{ return all_numbers_; }

bool 
ScopeID::has_number (symbol name) const
{ return name == tag; }

double 
ScopeID::number (symbol) const
{ return value; }

symbol 
ScopeID::dimension (symbol) const
{ return dim;}

ScopeID::ScopeID (const symbol name, const symbol d)
  : tag (name),
    value (-42.42e42),
    dim (d)
{ 
  if (all_numbers_.size () < 1)
    all_numbers_.push_back (tag);
}

ScopeID::~ScopeID ()
{ daisy_assert (all_numbers_.size () == 1); }

// scope_id.C ends here
