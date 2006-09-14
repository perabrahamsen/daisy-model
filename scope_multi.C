// scope_multi.C --- A scope combining multiple scopes.
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

#include "scope_multi.h"
#include "assertion.h"

bool 
ScopeMulti::has_number (const symbol tag) const
{
  for (size_t i = 0; i < scopes.size (); i++)
    if (scopes[i]->has_number (tag))
      return true;
  
  return false;
}

double
ScopeMulti::number (const symbol tag) const
{
  for (size_t i = 0; i < scopes.size (); i++)
    if (scopes[i]->has_number (tag))
      return scopes[i]->number (tag);
  
  daisy_panic ("'" + tag + "' not found in any scope");
}    

symbol 
ScopeMulti::dimension (const symbol tag) const
{
  for (size_t i = 0; i < scopes.size (); i++)
    if (scopes[i]->has_number (tag))
      return scopes[i]->dimension (tag);
  
  daisy_panic ("'" + tag + "' not found in any scope");
}

ScopeMulti::ScopeMulti (const Scope& first, const Scope& second)
{ 
  scopes.push_back (&first);
  scopes.push_back (&second);
}

ScopeMulti::~ScopeMulti ()
{ }

// scope_multi.C ends here.
