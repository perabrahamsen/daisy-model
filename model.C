// model.C -- Base class for all model in Daisy.
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

#include "model.h"
#include "log.h"
#include "syntax.h"

// Base class 'Model'

void 
Model::load_model (Syntax& syntax, AttributeList& alist)
{ 
  syntax.add ("description", Syntax::String, Syntax::OptionalConst, "\
Description of this model or parameterization.\n\
The value will appear in the reference manual, and may also appear in some \
GUI front ends.");
  syntax.add ("cite", Syntax::String, Syntax::Const, Syntax::Sequence, "\
BibTeX keys that would be relevant for this model or paramterization.");
  alist.add ("cite", std::vector<symbol> ());
}

Model::Model ()
{ }

Model::~Model ()
{ }

// The 'ModelLogable' Class.

void 
ModelLogable::output_as_derived (const symbol key, Log& log) const
{
  const char *const component = library_id ().name ().c_str ();
  if (log.check_derived (key, name, component))
    {
      Log::Derived derived (log, key, name, component);
      output (log);
    }
}

ModelLogable::ModelLogable (const symbol n)
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
  : ModelLogable (al.identifier ("type")),
    alist (al)
{ }

ModelAListed::ModelAListed (const symbol n)
  : ModelLogable (n)
{ }

// model.C ends here.
