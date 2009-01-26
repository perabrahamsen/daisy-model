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
  if (!parent ())
    return false;
  
  return parent ()->buildable ();
}

Model*
FrameModel::construct (Block& context, const symbol key, 
                       const FrameModel& frame) const
{ 
  if (!parent ())
    {
      daisy_panic ("Cannot build base model '" + key + "'");
      return NULL;
    }
  return parent ()->construct (context, key, frame);
}

Model*
FrameModel::construct (Block& context, const symbol key) const
{ return construct (context, key, *this); }

FrameModel::FrameModel ()
  : Frame (),
    parent_ (NULL)
{ }

const FrameModel& 
FrameModel::root ()
{
  static const struct FrameRoot : public FrameModel
  {
    FrameRoot ()
      : FrameModel ()
    { 
#ifdef SHARED_PARAMETERS
      this->add ("description", Value::String, Value::OptionalConst, "\
Description of this model or parameterization.\n\
The value will appear in the reference manual, and may also appear in some \
GUI front ends.");
      this->add ("cite", Value::String, Value::Const, Value::Sequence, "\
BibTeX keys that would be relevant for this model or paramterization.");
      this->add ("cite", std::vector<symbol> ());
#endif 
    }
    FrameRoot& clone () const
    { daisy_notreached (); }
  } frame;
  return frame;
}

FrameModel::FrameModel (const FrameModel& parent, parent_link_t)
  : Frame (),
    parent_ (&parent)
{ }

FrameModel::FrameModel (const FrameModel& parent, parent_copy_t)
  // For cloning a library.
  : Frame (parent),
    // We use parent builder.
    parent_ (&parent)
{ }

FrameModel::FrameModel (const FrameModel& parent, parent_clone_t)
  // For cloning a model
  : Frame (parent),
    parent_ (parent.parent ())
{ }

FrameModel::FrameModel (const FrameModel& p, const AttributeList& a)
  // build_alist
  : Frame (p.syntax (), a),
    parent_ (&p)
{ }

FrameModel::FrameModel (const FrameModel& p, 
                        const Syntax& s, const AttributeList& a)
  // add_derived
  : Frame (s, a),
    parent_ (&p)
{ }

FrameModel&
FrameModel::clone () const
{ return *new FrameModel (*this, parent_clone); }

FrameModel::~FrameModel ()
{ }

// frame_model.C ends here.

