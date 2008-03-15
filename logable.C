// logable.C -- Base class for all logable model in Daisy.
// 
// Copyright 2008 Per Abrahamsen and KVL.
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

#include "logable.h"
#include "log.h"

// The 'ModelNamed' Class.

ModelNamed::ModelNamed (const symbol n)
  : name (n)
{ }

// The 'ModelAListed' Class.

void 
ModelAListed::output_as_object (const symbol key, Log& log) const
{
  const char *const component = library_id ().name ().c_str ();
  if (log.check_derived (key, name, component))
    {
      Log::Object object (log, key, name, alist, component);
      output (log);
    }
}

void 
ModelAListed::output_as_entry (Log& log) const
{
  const char *const component = library_id ().name ().c_str ();
 
  if (log.check_entry (name, component))
    {
      Log::Entry entry (log, name, alist, component);
      output (log);
    }
}

ModelAListed::ModelAListed (const AttributeList& al)
  : ModelNamed (al.identifier ("type")),
    alist (al)
{ }

ModelAListed::ModelAListed (const symbol n)
  : ModelNamed (n)
{ }

// logable.C ends here.
