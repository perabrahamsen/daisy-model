// action_repeat.C
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
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

#include "action.h"
#include "daisy.h"
#include "block_model.h"
#include "log.h"
#include "librarian.h"
#include "frame_model.h"
#include "assertion.h"
#include <memory>

struct ActionRepeat : public Action
{
  const Metalib& metalib;
  FrameModel modified_frame;
  const std::unique_ptr<FrameModel> repeat;
  std::unique_ptr<Action> action;

  const FrameModel& frame () const
  { return modified_frame; }

  void tick (const Daisy& daisy, const Scope& scope, Treelog& msg)
  { 
    if (action.get ())
      action->tick (daisy, scope, msg); 
  }

  void doIt (Daisy& daisy, const Scope& scope, Treelog& msg)
  { 
    if (action.get () && action->done (daisy, scope, msg))
	action.reset (NULL);
    if (!action.get ())
      {
	action.reset(Librarian::build_frame<Action> (metalib, 
                                                     msg, *repeat, "repeat"));
	action->initialize (daisy, scope, msg);
	if (!action->check (daisy, scope, msg))
          action.reset (NULL);
	else
	  action->tick (daisy, scope, msg);
      }
    if (action.get ())         // Build free may fail.
      action->doIt (daisy, scope, msg);
  }

  bool done (const Daisy&, const Scope&, Treelog&) const
  { return false; }

  void output (Log& log) const
  { 
    if (action.get ())
      output_object (action, "do", log);
  }

  void initialize (const Daisy& daisy, const Scope& scope, Treelog& msg)
  { 
    if (action.get ())
      action->initialize (daisy, scope, msg); 
  }

  bool check (const Daisy& daisy, const Scope& scope, Treelog& err) const
  { 
    if (action.get ())
      return action->check (daisy, scope, err);
    else
      return true;
  }

  ActionRepeat (const BlockModel& al)
    : Action (al),
      metalib (al.metalib ()),
      modified_frame (Action::frame (), FrameModel::parent_link),
      repeat (&al.model ("repeat").clone ()),
      action (al.check ("do") 
              ? Librarian::build_item<Action> (al, "do")
              : Librarian::build_item<Action> (al, "repeat"))
  { 
    if (!modified_frame.check ("do"))
      modified_frame.set ("do", *repeat);
  }
  ~ActionRepeat ()
  { }
};

static struct ActionRepeatSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ActionRepeat (al); }

  ActionRepeatSyntax ()
    : DeclareModel (Action::component, "repeat", "\
Perform all of the specified action.  When done, repeat the action.\n\
The action may take several timesteps.")
  { }
  void load_frame (Frame& frame) const
  {
      frame.declare_object ("repeat", Action::component,
                         Attribute::Const, Attribute::Singleton,
                         "Action to perform repeatedly.");
      frame.declare_object ("do", Action::component, 
                         Attribute::OptionalState, Attribute::Singleton,
                         "Action currently being performed.");
      frame.order ("repeat");
    }
} ActionRepeat_syntax;

// action_repeat.C ends here.
