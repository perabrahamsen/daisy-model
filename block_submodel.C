// submodeler.C  --- Utilities for handling submodels.
// 
// Copyright 2009 Per Abrahamsen and KVL.
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

#include "block_submodel.h"
#include "submodeler.h"
#include "frame_submodel.h"
#include "assertion.h"

const FrameSubmodel& 
BlockSubmodel::frame () const
{ return frame_; }

BlockSubmodel::BlockSubmodel (const Block& parent, const symbol key)
  : BlockNested (parent, key),
    frame_ (parent.submodel (key))
{ }

BlockSubmodel::BlockSubmodel (const Block& parent, const symbol key, 
                              const size_t index)
  : BlockNested (parent, sequence_id (key, index)),
    frame_ (*parent.submodel_sequence (key)[index])
{ 
  daisy_assert (parent.submodel_sequence (key).size () > index);
  daisy_assert (parent.submodel_sequence (key)[index]);
}

BlockSubmodel::~BlockSubmodel ()
{ }

// submodeler.C ends here.
