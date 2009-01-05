// frame_model.C -- Model and parameterizations.
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

#include "frame_model.h"
#include "block.h"
#include "assertion.h"

const FrameModel* 
FrameModel::parent () const
{ return parent_; }

Model&
FrameModel::construct (Block& context, const symbol key) const
{ 
  if (builder)
    {
      Block block (context, syntax (), alist (), key);
      return builder (block);
    }
  daisy_assert (parent ());
  return parent ()->construct (context, key);
}

FrameModel::FrameModel ()
  : Frame (),
    parent_ (NULL),
    builder (NULL)
{ }

const FrameModel& 
FrameModel::root ()
{
  static const FrameModel frame;
  return frame;
}

FrameModel::FrameModel (const FrameModel& parent, parent_copy_t)
  : Frame (parent),
    parent_ (&parent),
    builder (parent.builder)
{ }

FrameModel::FrameModel (const FrameModel& parent, parent_link_t)
  : Frame (),
    parent_ (&parent),
    builder (parent.builder)
{ }

FrameModel::~FrameModel ()
{ }

// frame_model.C ends here.

