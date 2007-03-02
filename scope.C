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


#include "scope.h"
#include "assertion.h"

struct ScopeNull : public Scope
{
  // Use.
  void tick (const Scope&, Treelog&)
  { }
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

  // Create and Destroy.
  ScopeNull ()
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

Scope::Scope ()
{ }

Scope::~Scope ()
{ }

// scope.C ends here
