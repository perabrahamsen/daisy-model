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
#include "memutils.h"

std::map<symbol, Library*> 
Intrinsics::clone () const
{ 
  closed = true;

  std::map<symbol, Library*> result;
  
  for (std::map<symbol, Library*>::const_iterator i = all.begin ();
       i != all.end ();
       i++)
    result[(*i).first] = (*i).second->clone ();

  return result;
}

Library&
Intrinsics::add (const char *const component)
{
  daisy_assert (!closed);
  const std::map<symbol, Library*>::const_iterator i
    = all.find (symbol (component));
  
  if (i != all.end ())
    return *(*i).second;

  Library *const lib = new Library (component);
  all[symbol (component)] = lib;
  return *lib;
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

Intrinsics::Intrinsics ()
  : count (1),
    closed (false)
{ }

Intrinsics::~Intrinsics ()
{ 
  map_delete (all.begin (), all.end ());
  daisy_assert (count == 0); 
}


// intrinsics.C ends here
