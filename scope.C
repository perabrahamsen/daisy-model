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
#include "block.h"
#include "assertion.h"
#include "librarian.h"

const char *const Scope::component = "scope";

symbol 
Scope::title () const
{ return title_; }

bool 
Scope::is_number (symbol tag) const
{
  if (has_number (tag))
    return true;

  const std::vector<symbol>& all = all_numbers ();

  for (size_t i = 0; i < all.size (); i++)
    if (all[i] == tag)
      return true;

  return false;
}

bool
Scope::has_identifier (symbol) const
{ return false; }

symbol
Scope::identifier (symbol) const
{ daisy_notreached (); }

bool
Scope::has_integer (symbol) const
{ return false; }

int
Scope::integer (symbol) const
{ daisy_notreached (); }

struct ScopeNull : public Scope
{
  // Use.
  const std::vector<symbol>& all_numbers () const
  {
    static std::vector<symbol> all_numbers_;
    return all_numbers_; 
  }
  bool has_number (symbol) const
  { return false; }
  double number (symbol) const
  { daisy_notreached (); }
  symbol dimension (symbol) const
  { daisy_notreached (); }
  symbol get_description (symbol) const
  { daisy_notreached (); }

  // Create and Destroy.
  ScopeNull ()
    : Scope ("null")
  { }
  ~ScopeNull ()
  { }
};

Scope&
Scope::null ()
{ 
  static ScopeNull nullscope;
  return nullscope; 
}

Scope::Scope (symbol t)
  : title_ (t)
{ }

Scope::Scope (const char *const t)
  : title_ (t)
{ }

Scope::Scope (Block& al)
  : title_ (al.identifier ("where", al.identifier ("type")))
{ }

Scope::~Scope ()
{ }

WScope::WScope (const symbol t)
  : Scope (t)
{ }

WScope::WScope (const char *const t)
  : Scope (t)
{ }

WScope::WScope (Block& al)
  : Scope (al)
{ }

WScope::~WScope ()
{ }

static Librarian Scope_init (Scope::component, "\
A scope maps identifiers to values.");

// scope.C ends here
