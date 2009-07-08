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

#define BUILD_DLL

#include "scope_multi.h"
#include "assertion.h"
#include "librarian.h"

void 
ScopeMulti::entries (std::set<symbol>& all) const
{
  for (size_t i = 0; i < scopes.size (); i++)
    scopes[i]->entries (all);
}

Attribute::type 
ScopeMulti::lookup (const symbol name) const
{
  for (size_t i = 0; i < scopes.size (); i++)
    {
      Attribute::type type = scopes[i]->lookup (name);
      if (type != Attribute::Error)
        return type;
    }
  return Attribute::Error;
}

bool 
ScopeMulti::check (const symbol name) const
{
  for (size_t i = 0; i < scopes.size (); i++)
    if (scopes[i]->check (name))
      return true;

  return false;
}

double
ScopeMulti::number (const symbol tag) const
{
  for (size_t i = 0; i < scopes.size (); i++)
    {
      Attribute::type type = scopes[i]->lookup (tag);
      switch (type)
        {
        case Attribute::Error:
          // Not here, try next scope.
          continue;
        case Attribute::Number:
          // Yeah!
          daisy_assert (scopes[i]->check (tag));
          return scopes[i]->number (tag);
        default:
          // Wrong type.
          daisy_panic ("'" + tag + "' should be a " 
                       + Attribute::type_name (Attribute::Number) + ", is a "
                       + Attribute::type_name (type));
        }
    }
  daisy_panic ("'" + tag + "' not found in any scope");
}    

symbol 
ScopeMulti::dimension (const symbol tag) const
{
  for (size_t i = 0; i < scopes.size (); i++)
    if (scopes[i]->lookup (tag) == Attribute::Number)
      return scopes[i]->dimension (tag);
  
  daisy_panic ("'" + tag + "' not found in any scope");
}

symbol
ScopeMulti::description (symbol tag) const
{
  for (size_t i = 0; i < scopes.size (); i++)
    if (scopes[i]->lookup (tag) != Attribute::Error)
      return scopes[i]->description (tag);
  
  daisy_panic ("'" + tag + "' not found in any scope");
}

std::vector<const Scope*>
ScopeMulti::vectorize (const Scope* first, const Scope* second)
{
  std::vector<const Scope*> result;
  result.push_back (first);
  result.push_back (second);
  return result;
}

ScopeMulti::ScopeMulti (const Scope& first, const Scope& second)
  : scopes (vectorize (&first, &second))
{ }

ScopeMulti::~ScopeMulti ()
{ }

// scope_multi.C ends here.
