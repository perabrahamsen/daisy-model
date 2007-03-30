// intrinsics.C -- The build in models of Daisy.
// 
// Copyright 2007 Per Abrahamsen and KVL.
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


#include "intrinsics.h"
#include "assertion.h"
#include "library.h"

Intrinsics::Intrinsics ()
  : count (1)
{ }

Intrinsics::~Intrinsics ()
{ daisy_assert (count == 0); }

void 
Intrinsics::add (const char *const component, 
                 const char *const description)
{
  const std::map<symbol, Library*>::const_iterator i
    = all.find (symbol (component));
  
  if (i != all.end ())
    return;

  all[symbol (component)] = new Library (component, description);
}

Library&
Intrinsics::library (const char *const component) const
{
  const std::map<symbol, Library*>::const_iterator i
    = all.find (symbol (component));
  if (i == all.end ())
    daisy_panic ("Component '" + std::string (component) + "' not found");
  
  return *(*i).second;
}

// intrinsics.C ends here
