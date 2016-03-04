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
#include "treelog.h"
#include "frame.h"
#include "block_model.h"

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

  ActionNil (const BlockModel& al)
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

  ActionT (const BlockModel& al)
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

  bool done (const Daisy& daisy, const Scope& scope, Treelog& msg) const
  {
    bool all_done = true;
    TREELOG_MODEL (msg);
    for (std::vector<Action*>::const_iterator i = actions.begin ();
	 i != actions.end ();
	 i++)
      {
	if (!(*i)->done (daisy, scope, msg))
	  all_done = false;
      }
    return all_done;
  }

  void output (Log& log) const
  { output_list (actions, "actions", log, Action::component); }

  void initialize (const Daisy& daisy, const Scope& scope, Treelog& msg)
  { 
    for (std::vector<Action*>::iterator i = actions.begin ();
	 i != actions.end ();
	 i++)
      {
	(*i)->initialize (daisy, scope, msg);
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

  ActionProgn (const BlockModel& al)
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
    std::unique_ptr<Condition> condition;
    std::vector<Action*> actions;
    void output (Log& log) const
    { 
      output_object (condition, "condition", log);
      output_list (actions, "actions", log, Action::component);
    }
    static void load_syntax (Frame& frame)
    {
      frame.declare_object ("condition", Condition::component, 
                  "Condition for performing the actions.");
      frame.declare_object ("actions", Action::component,
                         Attribute::State, Attribute::Variable, 
                         "Actions to perform when condition is meet.");
      frame.order ("condition", "actions");
    }
    clause (const Block& al) 
      : condition (Librarian::build_item<Condition> (al, "condition")),
        actions (Librarian::build_vector<Action> (al, "actions"))
    { }
    ~clause ()
    { sequence_delete (actions.begin (), actions.end ()); }
  };
  std::vector<clause*> clauses;

  void tick (const Daisy& daisy, const Scope& scope, Treelog& msg)
  { 
    for (std::vector<clause*>::iterator i = clauses.begin (); 
	 i != clauses.end ();
	 i++)
      {
	(*i)->condition->tick (daisy, scope, msg);
	std::vector<Action*>& actions = (*i)->actions;
	for (unsigned int j = 0; j < actions.size (); j++)
	  actions[j]->tick (daisy, scope, msg);
      }
  }

  void doIt (Daisy& daisy, const Scope& scope, Treelog& msg)
  { 
    for (std::vector<clause*>::iterator i = clauses.begin (); 
	 i != clauses.end ();
	 i++)
      {
	if ((*i)->condition->match (daisy, scope, msg))
	  {
	    std::vector<Action*>& actions = (*i)->actions;
	    for (unsigned int j = 0; j < actions.size (); j++)
	      actions[j]->doIt (daisy, scope, msg);
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

  void initialize (const Daisy& daisy, const Scope& scope, Treelog& msg)
  { 
    for (std::vector<clause*>::iterator i = clauses.begin (); 
	 i != clauses.end ();
	 i++)
      {
	(*i)->condition->initialize (daisy, scope, msg);
	std::vector<Action*>& actions = (*i)->actions;
	for (unsigned int j = 0; j < actions.size (); j++)
	  actions[j]->initialize (daisy, scope, msg);
      }
  }

  bool check (const Daisy& daisy, const Scope& scope, Treelog& err) const
  { 
    bool ok = true;
    for (std::vector<clause*>::const_iterator i = clauses.begin (); 
	 i != clauses.end ();
	 i++)
      {
	const std::vector<Action*>& actions = (*i)->actions;
	for (unsigned int j = 0; j < actions.size (); j++)
	  if (!actions[j]->check (daisy, scope, err))
	    ok = false;
      }
    return ok;
  }

  ActionCond (const BlockModel& al)
    : Action (al),
      clauses (map_submodel<clause> (al, "clauses"))
  { }

  ~ActionCond ()
  { sequence_delete (clauses.begin (), clauses.end ()); }
};

struct ActionIf : public Action
{
  std::unique_ptr<Condition> if_c;
  std::unique_ptr<Action> then_a;
  std::unique_ptr<Action> else_a;

  void tick (const Daisy& daisy, const Scope& scope, Treelog& msg)
  { 
    if_c->tick (daisy, scope, msg);
    then_a->tick (daisy, scope, msg);
    else_a->tick (daisy, scope, msg);
  }

  void doIt (Daisy& daisy, const Scope& scope, Treelog& msg)
  { 
    if (if_c->match (daisy, scope, msg))
      then_a->doIt (daisy, scope, msg);
    else
      else_a->doIt (daisy, scope, msg);
  }

  void output (Log& log) const
  { 
    output_object (if_c, "if", log);
    output_object (then_a, "then", log);
    output_object (else_a, "else", log);
  }

  void initialize (const Daisy& daisy, const Scope& scope, Treelog& msg)
  { 
    if_c->initialize (daisy, scope, msg);
    then_a->initialize (daisy, scope, msg);
    else_a->initialize (daisy, scope, msg);
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

  ActionIf (const BlockModel& al)
    : Action (al),
      if_c (Librarian::build_item<Condition> (al, "if")),
      then_a (Librarian::build_item<Action> (al, "then")),
      else_a (Librarian::build_item<Action> (al, "else"))
  { }

  ~ActionIf ()
  { }
};

static struct ActionNilSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ActionNil (al); }
  ActionNilSyntax ()
    : DeclareModel (Action::component, "nil", "\
This action does nothing, always done.")
  { }
  void load_frame (Frame&) const
  { }
} ActionNil_syntax;

static struct ActionTSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ActionT (al); }
  ActionTSyntax ()
    : DeclareModel (Action::component, "t", "\
This action does nothing, never done.")
  { }
  void load_frame (Frame&) const
  { }
} ActionT_syntax;

static struct ActionPrognSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ActionProgn (al); }
  ActionPrognSyntax ()
    : DeclareModel (Action::component, "progn", "\
Perform all the specified actions in the sequence listed.\n\
All the actions will be performed in the same time step.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare_object ("actions", Action::component, 
                          Attribute::State, Attribute::Variable,
                          "List of actions to perform.");
    frame.order ("actions");
  }
} ActionProgn_syntax;

static struct ActionCondSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ActionCond (al); }
  ActionCondSyntax ()
    : DeclareModel (Action::component, "cond", "\
Perform the actions associated with the first true condition in the list.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare_submodule_sequence ("clauses", Attribute::State, "\
Each clause consist of a condition and a sequence of actions.\n\
The first clause whose condition is true, will have its actions activated.",
                                   ActionCond::clause::load_syntax);
    frame.order ("clauses");
  }
} ActionCond_syntax;

static struct ActionIfSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ActionIf (al); }
  ActionIfSyntax ()
    : DeclareModel (Action::component, "if", "\
If the condition is true, perform the first action,\n\
otherwise perform the second action.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare_object ("if", Condition::component, 
                       "Condition determining which action to perform.");
    frame.declare_object ("then", Action::component, 
                       "Action to perform if the condition is true.");
    frame.declare_object ("else", Action::component, 
                       "Action to perform if the condition is false.");
    frame.order ("if", "then", "else");
    frame.set ("else", "nil");
  }
} ActionIf_syntax;

// action_lisp.C ends here.
