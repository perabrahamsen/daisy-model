// action_lisp.C
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
#include "daisy.h"
#include "log.h"
#include "memutils.h"
#include "submodeler.h"

// We need to initialize the Condition library.
#include "condition.h"

using namespace std;

struct ActionNil : public Action
{
  void doIt (Daisy&, Treelog&)
  { }

  ActionNil (Block& al)
    : Action (al)
  { }
};

struct ActionT : public Action
{
  void doIt (Daisy&, Treelog&)
  { }

  bool done (const Daisy&, Treelog&) const
  { return false; }

  ActionT (Block& al)
    : Action (al)
  { }
};

struct ActionProgn : public Action
{
  vector<Action*> actions;

  void tick (const Daisy& daisy, Treelog& out)
  { 
    for (vector<Action*>::iterator i = actions.begin ();
	 i != actions.end ();
	 i++)
      {
	(*i)->tick (daisy, out);
      }
  }

  void doIt (Daisy& daisy, Treelog& out)
  { 
    for (vector<Action*>::iterator i = actions.begin ();
	 i != actions.end ();
	 i++)
      {
	(*i)->doIt (daisy, out);
      }
  }

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

  ActionProgn (Block& al)
    : Action (al),
      actions (Librarian<Action>::build_vector (al, "actions"))
  { }

  ~ActionProgn ()
  { sequence_delete (actions.begin (), actions.end ()); }
};


struct ActionCond : public Action
{
  struct clause
  {
    std::auto_ptr<Condition> condition;
    vector<Action*> actions;
    void output (Log& log) const
    { 
      output_derived (condition, "condition", log);
      output_list (actions, "actions", log, Librarian<Action>::library ());
    }
    static void load_syntax (Syntax& syntax, AttributeList&)
    {
      syntax.add_object ("condition", Condition::component, 
                  "Condition for performing the actions.");
      syntax.add_object ("actions", Action::component,
                         Syntax::State, Syntax::Sequence, 
                         "Actions to perform when condition is meet.");
      syntax.order ("condition", "actions");
    }
    clause (Block& al) 
      : condition (Librarian<Condition>::build_item (al, "condition")),
        actions (Librarian<Action>::build_vector (al, "actions"))
    { }
    ~clause ()
    { sequence_delete (actions.begin (), actions.end ()); }
  };
  vector<clause*> clauses;

  void tick (const Daisy& daisy, Treelog& out)
  { 
    for (vector<clause*>::iterator i = clauses.begin (); 
	 i != clauses.end ();
	 i++)
      {
	(*i)->condition->tick (daisy, out);
	vector<Action*>& actions = (*i)->actions;
	for (unsigned int j = 0; j < actions.size (); j++)
	  actions[j]->tick (daisy, out);
      }
  }

  void doIt (Daisy& daisy, Treelog& out)
  { 
    for (vector<clause*>::iterator i = clauses.begin (); 
	 i != clauses.end ();
	 i++)
      {
	if ((*i)->condition->match (daisy, out))
	  {
	    vector<Action*>& actions = (*i)->actions;
	    for (unsigned int j = 0; j < actions.size (); j++)
	      actions[j]->doIt (daisy, out);
	    break;
	  }
      }
  }

  void output (Log& log) const
  { 
    static const symbol clauses_symbol ("clauses");
    if (log.check_interior (clauses_symbol))
      {
	Log::Open open (log, clauses_symbol);
	for (vector<clause*>::const_iterator item = clauses.begin ();
	     item != clauses.end ();
	     item++)
	  {
	    Log::Unnamed unnamed (log);
	    (*item)->output (log);
	  }
      }
  }

  bool check (const Daisy& daisy, Treelog& err) const
  { 
    bool ok = true;
    for (vector<clause*>::const_iterator i = clauses.begin (); 
	 i != clauses.end ();
	 i++)
      {
	const vector<Action*>& actions = (*i)->actions;
	for (unsigned int j = 0; j < actions.size (); j++)
	  if (!actions[j]->check (daisy, err))
	    ok = false;
      }
    return ok;
  }

  ActionCond (Block& al)
    : Action (al),
      clauses (map_submodel<clause> (al, "clauses"))
  { }

  ~ActionCond ()
  { sequence_delete (clauses.begin (), clauses.end ()); }
};

struct ActionIf : public Action
{
  auto_ptr<Condition> if_c;
  auto_ptr<Action> then_a;
  auto_ptr<Action> else_a;

  void tick (const Daisy& daisy, Treelog& out)
  { 
    if_c->tick (daisy, out);
    then_a->tick (daisy, out);
    else_a->tick (daisy, out);
  }

  void doIt (Daisy& daisy, Treelog& out)
  { 
    if (if_c->match (daisy, out))
      then_a->doIt (daisy, out);
    else
      else_a->doIt (daisy, out);
  }

  void output (Log& log) const
  { 
    output_derived (if_c, "if", log);
    output_derived (then_a, "then", log);
    output_derived (else_a, "else", log);
  }

  bool check (const Daisy& daisy, Treelog& err) const
  { 
    bool ok = true; 
    if (!then_a->check (daisy, err))
      ok = false;
    if (!else_a->check (daisy, err))
      ok = false;
    return ok;
  }

  ActionIf (Block& al)
    : Action (al),
      if_c (Librarian<Condition>::build_item (al, "if")),
      then_a (Librarian<Action>::build_item (al, "then")),
      else_a (Librarian<Action>::build_item (al, "else"))
  { }

  ~ActionIf ()
  { }
};

static struct ActionLispSyntax
{
  static Model& make_nil (Block& al)
  { return *new ActionNil (al); }
  static Model& make_t (Block& al)
  { return *new ActionT (al); }
  static Model& make_progn (Block& al)
  { return *new ActionProgn (al); }
  static Model& make_cond (Block& al)
  { return *new ActionCond (al); }
  static Model& make_if (Block& al)
  { return *new ActionIf (al); }
  ActionLispSyntax ();
} ActionLisp_syntax;

ActionLispSyntax::ActionLispSyntax ()
{ 
  // "nil"
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "This action does nothing, always done.");
    Librarian<Action>::add_type ("nil", alist, syntax, &make_nil);
  }
  // "t"
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "This action does nothing, never done.");
    Librarian<Action>::add_type ("t", alist, syntax, &make_nil);
  }
  // "progn"
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "\
Perform all the specified actions in the sequence listed.\n\
All the actions will be performed in the same time step.");
    syntax.add_object ("actions", Action::component, 
                       Syntax::State, Syntax::Sequence,
                       "List of actions to perform.");
    syntax.order ("actions");
    Librarian<Action>::add_type ("progn", alist, syntax, &make_progn);
  }
  // "cond"
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "\
Perform the actions associated with the first true condition in the list.");
    syntax.add_submodule_sequence ("clauses", Syntax::State, "\
Each clause consist of a condition and a sequence of actions.\n\
The first clause whose condition is true, will have its actions activated.",
                                   ActionCond::clause::load_syntax);
    syntax.order ("clauses");
    Librarian<Action>::add_type ("cond", alist, syntax, &make_cond);
  }
  // "if"
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "\
If the condition is true, perform the first action,\n\
otherwise perform the second action.");
    syntax.add_object ("if", Condition::component, 
                       "Condition determining which action to perform.");
    syntax.add_object ("then", Action::component, 
                       "Action to perform if the condition is true.");
    syntax.add_object ("else", Action::component, 
                       "Action to perform if the condition is false.");
    syntax.order ("if", "then", "else");
    AttributeList nilAlist;
    nilAlist.add ("type", "nil");
    alist.add ("else", nilAlist);
    Librarian<Action>::add_type ("if", alist, syntax, &make_if);
  }
}
