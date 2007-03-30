// action_with.C --- restrict actions to a specific columns
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
#include "block.h"
#include "daisy.h"
#include "syntax.h"
#include "alist.h"
#include "field.h"
#include "log.h"
#include "memutils.h"

using namespace std;

struct ActionWithColumn : public Action
{
  const symbol column;
  vector<Action*> actions;

public:
  void tick (const Daisy& daisy, Treelog& out)
  { 
    Field::Restrict restriction (*daisy.field, column);
    Treelog::Open nest (out, column);
    for (vector<Action*>::iterator i = actions.begin ();
	 i != actions.end ();
	 i++)
      {
	(*i)->tick (daisy, out);
      }
  }

  void doIt (Daisy& daisy, Treelog& out)
  { 
    Field::Restrict restriction (*daisy.field, column);
    Treelog::Open nest (out, column);
    for (vector<Action*>::iterator i = actions.begin ();
	 i != actions.end ();
	 i++)
      {
	(*i)->doIt (daisy, out);
      }
  }

  void output (Log& log) const
  { output_list (actions, "actions", log, Action::component); }

  bool check (const Daisy& daisy, Treelog& err) const
  { 
    Treelog::Open nest (err, string ("with") + column);
    bool ok = true;
    for (vector<Action*>::const_iterator i = actions.begin ();
	 i != actions.end ();
	 i++)
      {
	if (!(*i)->check (daisy, err))
	  ok = false;
      }
    if (!daisy.field->find (column))
      {
	err.entry (string ("No column '") + column + "'");
	ok = false;
      }
    return ok;
  }

  ActionWithColumn (Block& al)
    : Action (al),
      column (al.identifier ("column")),
      actions (Librarian<Action>::build_vector (al, "actions"))
  { }
public:
  ~ActionWithColumn ()
  { sequence_delete (actions.begin (), actions.end ()); }
};

static struct ActionWithColumnSyntax
{
  static Model& make (Block& al)
  { return *new ActionWithColumn (al); }

  ActionWithColumnSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "Perform actions on a specific column.");
    syntax.add ("column", Syntax::String, Syntax::Const, 
		"Name of column to perform actions on.");
    syntax.add_object ("actions", Action::component, 
                       Syntax::State, Syntax::Sequence,
                       "Actions to perform on the specified column.");
    syntax.order ("column", "actions");
    BuildBase::add_type (Action::component, "with-column", alist, syntax, &make);
  }
} ActionWithColumn_syntax;
