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


#include "action.h"
#include "log.h"

struct ActionActivity : public Action
{
  vector<Action*> actions;

  void tick (const Daisy& daisy, Treelog& out)
  { 
    for (vector<Action*>::const_iterator i = actions.begin ();
	 i != actions.end ();
	 i++)
      (*i)->tick (daisy, out);
  }

  void doIt (Daisy& daisy, Treelog& out)
  { 
    if (actions.size () == 0U)
      return;

    Action* action = actions.front ();
    action->doIt (daisy, out);

    if (action->done (daisy))
      {
	delete action;
	actions.erase (actions.begin ());
      }
  }

  bool done (const Daisy&) const
  { return (actions.size () == 0U); }

  void output (Log& log) const
  { 
    output_list (actions, "actions", log, 
		 Librarian<Action>::library ());
  }

  bool check (const Daisy& daisy, Treelog& err) const
  { 
    bool ok = true;
    for (vector<Action*>::const_iterator i = actions.begin ();
	 i != actions.end ();
	 i++)
      {
	if (!(*i)->check (daisy, err))
	  ok = false;
      }
    return ok;
  }

  ActionActivity (const AttributeList& al)
    : Action (al),
      actions (map_create<Action> (al.alist_sequence ("actions")))
  { }

  ~ActionActivity ()
  { sequence_delete (actions.begin (), actions.end ()); }
};

static struct ActionActivitySyntax
{
  static Action& make (const AttributeList& al)
  { return *new ActionActivity (al); }

  ActionActivitySyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "\
Perform all the specified actions in the sequence listed.  Each\n\
action is performed until done.  At most one action can be performed\n\
at each time step.");
    syntax.add ("actions", Librarian<Action>::library (), Syntax::Sequence,
		"Sequence of actions to perform.");
    alist.add ("actions", vector<AttributeList*> ());
    syntax.order ("actions");
    Librarian<Action>::add_type ("activity", alist, syntax, &make);
  }
} ActionActivity_syntax;
