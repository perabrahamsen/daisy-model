// model_derived.C -- Base class for all derived model in Daisy.
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

#define BUILD_DLL

#include "model_derived.h"
#include "log.h"

// The 'ModelDerived' Class.

void 
ModelDerived::output_as_derived (const symbol key, Log& log) const
{
  const symbol component = library_id ();
  if (log.check_derived (key, objid, component))
    {
      Log::Derived derived (log, key, objid, component);
      output (log);
    }
}

ModelDerived::ModelDerived (const symbol n)
  : ModelLogable (n)
{ }

// model_derived.C ends here.
