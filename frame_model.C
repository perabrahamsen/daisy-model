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
#include "treelog.h"
#include "librarian.h"

const FrameModel* 
FrameModel::parent () const
{ return parent_; }

bool 
FrameModel::buildable () const
{
  // Check builder.
  for (const FrameModel* current = this; current; current = current->parent ())
    if (current->builder || current->declaration)
      return true;

  return false;
}

Model*
FrameModel::construct (Block& context, const symbol key, 
                       const FrameModel& frame) const
{ 
  if (builder || declaration)
    {
      Block block (context, frame, key);
      
      if (!frame.check (context))
        return NULL;

      try
        { return builder ? &builder (block) : declaration->make (block); }
      catch (const std::string& err)
        { block.error ("Build failed: " + err); }
      catch (const char *const err)
        { block.error ("Build failure: " + std::string (err)); }
      return NULL;
    }
  if (!parent ())
    {
      context.error ("Cannot build base model '" + key + "'");
      return NULL;
    }
  return parent ()->construct (context, key, frame);
}

Model*
FrameModel::construct (Block& context, const symbol key) const
{ return construct (context, key, *this); }

FrameModel::FrameModel ()
  : Frame (),
    parent_ (NULL),
    builder (NULL),
    declaration (NULL)
{ }

const FrameModel& 
FrameModel::root ()
{
  static const FrameModel frame;
  return frame;
}

FrameModel::FrameModel (const FrameModel& parent, parent_link_t)
  : Frame (),
    parent_ (&parent),
    builder (parent.builder),
    declaration (NULL)
{ }

FrameModel::FrameModel (const FrameModel& parent, parent_copy_t)
  // For cloning a library.
  : Frame (parent),
    parent_ (&parent),
    builder (parent.builder),
    declaration (NULL)
{ }

FrameModel::FrameModel (const Syntax& s, const AttributeList& a)
  // Base class.
  : Frame (s, a),
    parent_ (NULL),
    builder (NULL),
    declaration (NULL)
{ }

FrameModel::FrameModel (const Syntax& s, const AttributeList& a,
                        const builder_t b)
  // Base class.
  : Frame (s, a),
    parent_ (NULL),
    builder (b),
    declaration (NULL)
{ }

FrameModel::FrameModel (const FrameModel& p, const AttributeList& a)
  // build_alist
  : Frame (p.syntax (), a),
    parent_ (&p),
    builder (NULL),
    declaration (NULL)
{ }

FrameModel::FrameModel (const FrameModel& p, 
                        const Syntax& s, const AttributeList& a)
  // add_derived
  : Frame (s, a),
    parent_ (&p),
    builder (NULL),
    declaration (NULL)
{ }

FrameModel::FrameModel (const Declare& declare)
  // Declared.
  : Frame (),
    parent_ (declare.parent_model ()),
    builder (NULL),
    declaration (dynamic_cast<const DeclareModel*> (&declare))
{ declare.load (*this); }

FrameModel::~FrameModel ()
{ }

// frame_model.C ends here.

