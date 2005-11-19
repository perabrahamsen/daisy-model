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


#include "action.h"
#include "log.h"

struct ActionRepeat : public Action
{
  const AttributeList repeat;
  Action* action;

  void tick (const Daisy& daisy, Treelog& out)
  { action->tick (daisy, out); }
    
  void doIt (Daisy& daisy, Treelog& out)
  { 
    if (action && action->done (daisy))
      {
	delete action;
	action = NULL;
      }
    if (action == NULL)
      action = Librarian<Action>::build_free (out, repeat, "repeat");
    if (action != NULL)         // Build free may fail.
      action->doIt (daisy, out);
  }

  bool done (const Daisy&) const
  { 
    return false;
  }

  void output (Log& log) const
  { 
    output_derived (action, "do", log);
  }

  bool check (const Daisy& daisy, Treelog& err) const
  { 
    if (action)
      return action->check (daisy, err);
    else
      return true;
  }

  static AttributeList add_do (const AttributeList& al)
  {
    AttributeList alist (al);
    if (!alist.check ("do"))
      alist.add ("do", alist.alist ("repeat"));
    return alist;
  }

  ActionRepeat (Block& al)
    : Action (al, add_do (al.alist ())),
      repeat (al.alist ("repeat")),
      action (Librarian<Action>::build_alist (al, (al.check ("do") 
                                                   ? al.alist ("do")
                                                   : repeat), "do"))
  { }

  ~ActionRepeat ()
  { 
    if (action)
      delete action;
  }
};

static struct ActionRepeatSyntax
{
  static Action& make (Block& al)
  { return *new ActionRepeat (al); }

  ActionRepeatSyntax ()
    {
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      alist.add ("description", "\
Perform all of the specified action.  When done, repeat the action.\n\
The action may take several timesteps.");
      syntax.add ("repeat", Librarian<Action>::library (),
		  Syntax::Const, Syntax::Singleton,
		  "Action to perform repeatedly.");
      syntax.add ("do", Librarian<Action>::library (), 
		  Syntax::OptionalState, Syntax::Singleton,
		  "Action currently being performed.");
      syntax.order ("repeat");
      Librarian<Action>::add_type ("repeat", alist, syntax, &make);
    }
} ActionRepeat_syntax;
