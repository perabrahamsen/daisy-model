// scope.C -- A name -> value map.
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

#define BUILD_DLL

#include "scope.h"
#include "assertion.h"

// The 'Scope' Interface.

symbol 
Scope::title () const
{ return Attribute::Unknown (); }

bool
Scope::can_extract_as (const symbol tag, Attribute::type type) const
{ return lookup (tag) == type; }

int 
Scope::type_size (const symbol tag) const
{ return Attribute::Singleton; }

int 
Scope::value_size (const symbol tag) const
{ return Attribute::Singleton; }

symbol
Scope::name (symbol) const
{ daisy_notreached (); }

int
Scope::integer (symbol) const
{ daisy_notreached (); }

Scope&
Scope::null ()
{ 
  static struct ScopeNull : public Scope
  {
    // Use.
    void entries (std::set<symbol>&) const
    { }
    Attribute::type lookup (symbol) const
    { return Attribute::Error; }
    bool check (symbol) const
    { return false; }
    double number (symbol) const
    { daisy_notreached (); }
    symbol dimension (symbol) const
    { daisy_notreached (); }
    symbol description (symbol) const
    { daisy_notreached (); }

    // Create and Destroy.
    ScopeNull ()
    { }
    ~ScopeNull ()
    { }
  } nullscope;

  return nullscope; 
}

Scope::Scope ()
{ }

Scope::~Scope ()
{ }

// scope.C ends here
