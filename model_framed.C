// model_framed.C -- Base class for all framed model in Daisy.
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

#include "model_framed.h"
#include "log.h"
#include "frame_model.h"
#include "assertion.h"
#include "block_model.h"

const FrameModel& 
ModelFramed::frame () const
{
  daisy_assert (my_frame.get ());
  return *my_frame;
}

void 
ModelFramed::output_as_object (const symbol key, Log& log) const
{
  const symbol component = library_id ();
  if (log.check_derived (key, objid, component))
    {
      Log::Model object (log, key, objid, frame (), component);
      output (log);
    }
}

void 
ModelFramed::output_as_entry (Log& log) const
{
  const symbol component = library_id ();
 
  if (log.check_entry (objid, component))
    {
      Log::Entry entry (log, objid, frame (), component);
      output (log);
    }
}

ModelFramed::ModelFramed (const BlockModel& al)
  : ModelLogable (al.type_name ()),
    // Block is sometimes fed a temporary frame, thus the need for clone.
    my_frame (&(al.frame ().clone ()))
{ daisy_assert (my_frame.get ());  }

ModelFramed::ModelFramed (const symbol n)
  : ModelLogable (n)
    // , my_frame (NULL)
{ }

ModelFramed::~ModelFramed ()
{ }

// model_framed.C ends here.
