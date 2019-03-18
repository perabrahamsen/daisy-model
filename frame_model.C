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
#include "assertion.h"
#include "treelog.h"
#include "librarian.h"
#include "metalib.h"
#include "library.h"

const FrameModel* 
FrameModel::parent () const
{ return parent_; }

void 
FrameModel::replace_parent (const Frame* new_parent) const
{ 
  if (new_parent)
    {
      parent_ = dynamic_cast<const FrameModel*> (new_parent); 
      daisy_assert (parent_);
    }
  else
    parent_ = NULL;
}

bool 
FrameModel::subset (const Metalib& metalib, const FrameModel& other) const
{
  // Can only compare objects from the same library.
  const symbol component = this->component ();
  if (component != other.component ())
    {
      daisy_warning ("'" + component + "' != '" + other.component ()
                     + "' in '" + type_name () + "'");
      return false;
    }
  if (!metalib.exist (component))
    {
      daisy_warning ("'" + component + "' no such library in '"
                     + type_name () + "'");
      return false;
    }
  const Library& library = metalib.library (component);
  const symbol my_name = this->type_name ();
  const symbol his_name = other.type_name ();
  if (!library.is_derived_from (my_name, his_name))
    // Subsets must be derived from supersets.
    return false;

  return subset_elements (metalib, other);
}

symbol
FrameModel::component () const
{ 
  if (!parent ())
    {
      daisy_warning ("No parent '" + type_name () + "'");
      return Attribute::Unknown ();
    }
  return parent ()->component ();
}

bool 
FrameModel::buildable () const
{
  if (!parent ())
    return false;
  
  return parent ()->buildable ();
}

Model*
FrameModel::construct (const Block& context, const symbol key, 
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
FrameModel::construct (const Block& context, const symbol key) const
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
    { }
    FrameRoot& clone () const
    { daisy_notreached (); }
  } frame;
  return frame;
}

FrameModel::FrameModel (const FrameModel& parent, parent_link_t)
  : Frame (),
    parent_ (&parent)
{
  daisy_assert (this->parent ());
  this->parent ()->register_child (this); 
}

FrameModel::FrameModel (const FrameModel& parent, parent_clone_t)
  // For cloning a model
  : Frame (parent),
    parent_ (parent.parent ())
{ 
  if (this->parent ())
    this->parent ()->register_child (this); 
}

FrameModel&
FrameModel::clone () const
{ return *new FrameModel (*this, parent_clone); }

FrameModel::~FrameModel ()
{
  if (parent ())
    parent ()->unregister_child (this);
}

// frame_model.C ends here.

