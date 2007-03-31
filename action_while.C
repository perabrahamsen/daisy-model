// action_while.C
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
#include "syntax.h"
#include "log.h"
#include "assertion.h"
#include "memutils.h"

using namespace std;

struct ActionWhile : public Action
{
  const vector<Action*> actions;

  void tick (const Daisy& daisy, Treelog& out)
  { 
    for (unsigned int i = 0; i < actions.size (); i++)
      actions[i]->tick (daisy, out);
  }

  void doIt (Daisy& daisy, Treelog& out)
  { 
    for (unsigned int i = 0; i < actions.size (); i++)
      actions[i]->doIt (daisy, out);
  }

  bool done (const Daisy& daisy, Treelog& msg) const
  {
    daisy_assert (actions.size () != 0U);
    return (actions[0]->done (daisy, msg)); 
  }

  void output (Log& log) const
  { output_list (actions, "actions", log, Action::component); }

  bool check (const Daisy& daisy, Treelog& err) const
  { 
    Treelog::Open nest (err, "while");
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

  ActionWhile (Block& al)
    : Action (al),
      actions (BuildBase::build_vector<Action> (al, "actions"))
  { }

  ~ActionWhile ()
  { 
    sequence_delete (actions.begin (), actions.end ());
  }
};

static struct ActionWhileSyntax
{
  static Model& make (Block& al)
  { return *new ActionWhile (al); }

  static bool check_alist (const AttributeList& al, Treelog& err)
  {
    bool ok = true;

    if (al.size ("actions") < 1)
      {
	err.entry ("You must specify at least one action");
	ok = false;
      }
    return ok;
  }

  ActionWhileSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    syntax.add_check (check_alist);
    alist.add ("description", "\
Perform all the specified actions in the sequence listed, but in the\n\
same timestep.  The 'while' action is done when the first action in the\n\
list is done.");
    syntax.add_object ("actions", Action::component, 
                       Syntax::State, Syntax::Sequence,
                       "List of actions to perform.");
    syntax.order ("actions");
    BuildBase::add_type (Action::component, "while", alist, syntax, &make);
  }
} ActionWhile_syntax;
