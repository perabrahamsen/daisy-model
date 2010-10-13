// action_activity.C
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
#include "frame.h"
#include "log.h"
#include "memutils.h"
#include "librarian.h"
#include "metalib.h"
#include "library.h"
#include "treelog.h"
#include "block_model.h"

struct ActionActivity : public Action
{
  auto_vector<Action*> actions;

  void tick (const Daisy& daisy, const Scope& scope, Treelog& out)
  { 
    for (std::vector<Action*>::const_iterator i = actions.begin ();
	 i != actions.end ();
	 i++)
      (*i)->tick (daisy, scope, out);
  }

  void doIt (Daisy& daisy, const Scope& scope, Treelog& out)
  { 
    while (true)
      {
        if (actions.size () == 0U)
          return;

        Action* action = actions.front ();
        action->doIt (daisy, scope, out);

        if (!action->done (daisy, scope, out))
          // Not finished yet?  Save for next timestep.
          return;

        delete action;
        actions.erase (actions.begin ());
      }
  }

  bool done (const Daisy&, const Scope&, Treelog&) const
  { return (actions.size () == 0U); }

  void output (Log& log) const
  { 
    output_list (actions, "do", log, Action::component);
  }

  void initialize (const Daisy& daisy, const Scope& scope, Treelog& out)
  { 
    for (std::vector<Action*>::const_iterator i = actions.begin ();
	 i != actions.end ();
	 i++)
      (*i)->initialize (daisy, scope, out);
  }

  bool check (const Daisy& daisy, const Scope& scope, Treelog& err) const
  { 
    bool ok = true;
    for (std::vector<Action*>::const_iterator i = actions.begin ();
	 i != actions.end ();
	 i++)
      {
	if (!(*i)->check (daisy, scope, err))
	  ok = false;
      }
    return ok;
  }

  ActionActivity (const BlockModel& al)
    : Action (al),
      actions (Librarian::build_vector<Action> (al, "do"))
  { }

  ~ActionActivity ()
  { }
};

static struct ActionSequenceSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ActionActivity (al); }

  static bool check_alist (const Metalib& metalib, const Frame& al, Treelog& msg)
  {
    bool ok = true;

    const symbol description = al.description ();
    const Library& library = metalib.library (Action::component);
    if (library.check (description))
      msg.warning ("'" + description + "' is taken as a description of this sequence, but is also a valid action.  Maybe you meant to write '(" + description + ") instead");

    return ok;
  }

  ActionSequenceSyntax ()
    : DeclareModel (Action::component, "sequence", "\
Perform all the specified actions in the sequence listed.  Each\n\
action is performed until done.  At most one action can be performed\n\
at each time step.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.add_check (check_alist);
    frame.declare_object ("do", Action::component, 
                       Attribute::State, Attribute::Variable,
                       "Sequence of actions to perform.");
    frame.set_empty ("do");
  }
} ActionSequence_syntax;

static struct ActionActivitySyntax : public DeclareParam
{
  ActionActivitySyntax ()
    : DeclareParam (Action::component, "activity", "sequence", "\
Perform all the specified actions in the sequence listed.  Each\n\
action is performed until done.  At most one action can be performed\n\
at each time step.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.order ("do");
  }
} ActionActivity_syntax;

// action_activity.C ends here
