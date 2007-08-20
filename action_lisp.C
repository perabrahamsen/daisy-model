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

#define BUILD_DLL

#include "action.h"
#include "daisy.h"
#include "log.h"
#include "memutils.h"
#include "submodeler.h"
#include "librarian.h"

// We need to initialize the Condition library.
#include "condition.h"

struct ActionNil : public Action
{
  void doIt (Daisy&, const Scope&, Treelog&)
  { }

  void tick (const Daisy&, const Scope&, Treelog&)
  { }
  void initialize (const Daisy&, const Scope&, Treelog&)
  { }
  bool check (const Daisy&, const Scope&, Treelog& err) const
  { return true; }

  ActionNil (Block& al)
    : Action (al)
  { }
};

struct ActionT : public Action
{
  void doIt (Daisy&, const Scope&, Treelog&)
  { }

  bool done (const Daisy&, const Scope&, Treelog&) const
  { return false; }

  void tick (const Daisy&, const Scope&, Treelog&)
  { }
  void initialize (const Daisy&, const Scope&, Treelog&)
  { }
  bool check (const Daisy&, const Scope&, Treelog& err) const
  { return true; }

  ActionT (Block& al)
    : Action (al)
  { }
};

struct ActionProgn : public Action
{
  std::vector<Action*> actions;

  void tick (const Daisy& daisy, const Scope& scope, Treelog& out)
  { 
    for (std::vector<Action*>::iterator i = actions.begin ();
	 i != actions.end ();
	 i++)
      {
	(*i)->tick (daisy, scope, out);
      }
  }

  void doIt (Daisy& daisy, const Scope& scope, Treelog& out)
  { 
    for (std::vector<Action*>::iterator i = actions.begin ();
	 i != actions.end ();
	 i++)
      {
	(*i)->doIt (daisy, scope, out);
      }
  }

  bool done (const Daisy& daisy, const Scope& scope, Treelog& out) const
  {
    bool all_done = true;
    Treelog::Open nest (out, name);
    for (std::vector<Action*>::const_iterator i = actions.begin ();
	 i != actions.end ();
	 i++)
      {
	if (!(*i)->done (daisy, scope, out))
	  all_done = false;
      }
    return all_done;
  }

  void output (Log& log) const
  { output_list (actions, "actions", log, Action::component); }

  void initialize (const Daisy& daisy, const Scope& scope, Treelog& out)
  { 
    for (std::vector<Action*>::iterator i = actions.begin ();
	 i != actions.end ();
	 i++)
      {
	(*i)->initialize (daisy, scope, out);
      }
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

  ActionProgn (Block& al)
    : Action (al),
      actions (Librarian::build_vector<Action> (al, "actions"))
  { }

  ~ActionProgn ()
  { sequence_delete (actions.begin (), actions.end ()); }
};


struct ActionCond : public Action
{
  struct clause
  {
    std::auto_ptr<Condition> condition;
    std::vector<Action*> actions;
    void output (Log& log) const
    { 
      output_derived (condition, "condition", log);
      output_list (actions, "actions", log, Action::component);
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
      : condition (Librarian::build_item<Condition> (al, "condition")),
        actions (Librarian::build_vector<Action> (al, "actions"))
    { }
    ~clause ()
    { sequence_delete (actions.begin (), actions.end ()); }
  };
  std::vector<clause*> clauses;

  void tick (const Daisy& daisy, const Scope& scope, Treelog& out)
  { 
    for (std::vector<clause*>::iterator i = clauses.begin (); 
	 i != clauses.end ();
	 i++)
      {
	(*i)->condition->tick (daisy, scope, out);
	std::vector<Action*>& actions = (*i)->actions;
	for (unsigned int j = 0; j < actions.size (); j++)
	  actions[j]->tick (daisy, scope, out);
      }
  }

  void doIt (Daisy& daisy, const Scope& scope, Treelog& out)
  { 
    for (std::vector<clause*>::iterator i = clauses.begin (); 
	 i != clauses.end ();
	 i++)
      {
	if ((*i)->condition->match (daisy, scope, out))
	  {
	    std::vector<Action*>& actions = (*i)->actions;
	    for (unsigned int j = 0; j < actions.size (); j++)
	      actions[j]->doIt (daisy, scope, out);
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
	for (std::vector<clause*>::const_iterator item = clauses.begin ();
	     item != clauses.end ();
	     item++)
	  {
	    Log::Unnamed unnamed (log);
	    (*item)->output (log);
	  }
      }
  }

  void initialize (const Daisy& daisy, const Scope& scope, Treelog& out)
  { 
    for (std::vector<clause*>::iterator i = clauses.begin (); 
	 i != clauses.end ();
	 i++)
      {
	(*i)->condition->initialize (daisy, scope, out);
	std::vector<Action*>& actions = (*i)->actions;
	for (unsigned int j = 0; j < actions.size (); j++)
	  actions[j]->initialize (daisy, scope, out);
      }
  }

  bool check (const Daisy& daisy, const Scope& scope, Treelog& err) const
  { 
    bool ok = true;
    for (std::vector<clause*>::const_iterator i = clauses.begin (); 
	 i != clauses.end ();
	 i++)
      {
	const vector<Action*>& actions = (*i)->actions;
	for (unsigned int j = 0; j < actions.size (); j++)
	  if (!actions[j]->check (daisy, scope, err))
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

  void tick (const Daisy& daisy, const Scope& scope, Treelog& out)
  { 
    if_c->tick (daisy, scope, out);
    then_a->tick (daisy, scope, out);
    else_a->tick (daisy, scope, out);
  }

  void doIt (Daisy& daisy, const Scope& scope, Treelog& out)
  { 
    if (if_c->match (daisy, scope, out))
      then_a->doIt (daisy, scope, out);
    else
      else_a->doIt (daisy, scope, out);
  }

  void output (Log& log) const
  { 
    output_derived (if_c, "if", log);
    output_derived (then_a, "then", log);
    output_derived (else_a, "else", log);
  }

  void initialize (const Daisy& daisy, const Scope& scope, Treelog& out)
  { 
    if_c->initialize (daisy, scope, out);
    then_a->initialize (daisy, scope, out);
    else_a->initialize (daisy, scope, out);
  }

  bool check (const Daisy& daisy, const Scope& scope, Treelog& err) const
  { 
    bool ok = true; 
    if (!if_c->check (daisy, scope, err))
      ok = false;
    if (!then_a->check (daisy, scope, err))
      ok = false;
    if (!else_a->check (daisy, scope, err))
      ok = false;
    return ok;
  }

  ActionIf (Block& al)
    : Action (al),
      if_c (Librarian::build_item<Condition> (al, "if")),
      then_a (Librarian::build_item<Action> (al, "then")),
      else_a (Librarian::build_item<Action> (al, "else"))
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
    Librarian::add_type (Action::component, "nil", alist, syntax, &make_nil);
  }
  // "t"
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "This action does nothing, never done.");
    Librarian::add_type (Action::component, "t", alist, syntax, &make_nil);
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
    Librarian::add_type (Action::component, "progn", alist, syntax, &make_progn);
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
    Librarian::add_type (Action::component, "cond", alist, syntax, &make_cond);
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
    Librarian::add_type (Action::component, "if", alist, syntax, &make_if);
  }
}
