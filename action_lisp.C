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
// We need to initialize the Condition library.
#include "condition.h"

struct ActionNil : public Action
{
  void doIt (Daisy&)
  { }

  ActionNil (const AttributeList& al)
    : Action (al)
  { }
};

struct ActionT : public Action
{
  void doIt (Daisy&)
  { }

  bool done (const Daisy&) const
  { return false; }

  ActionT (const AttributeList& al)
    : Action (al)
  { }
};

struct ActionProgn : public Action
{
  vector<Action*>& actions;

  void tick (const Daisy& daisy)
  { 
    for (vector<Action*>::iterator i = actions.begin ();
	 i != actions.end ();
	 i++)
      {
	(*i)->tick (daisy);
      }
  }

  void doIt (Daisy& daisy)
  { 
    for (vector<Action*>::iterator i = actions.begin ();
	 i != actions.end ();
	 i++)
      {
	(*i)->doIt (daisy);
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

  ActionProgn (const AttributeList& al)
    : Action (al),
      actions (map_create<Action> (al.alist_sequence ("actions")))
  { }

  ~ActionProgn ()
  { 
    sequence_delete (actions.begin (), actions.end ());
    delete &actions;
  }
};

struct clause
{
  Condition* condition;
  vector<Action*> actions;
  void output (Log& log) const
  { 
    output_derived (*condition, "condition", log);
    output_list (actions, "actions", log, Librarian<Action>::library ());
  }
  clause (Condition* c, vector<Action*>& a) 
    : condition (c),
      actions (a)
  { }
  clause ()
    : condition (NULL)
  { }
};

#ifdef BORLAND_TEMPLATES
template class vector<clause>;
#endif

vector<clause>& make_clauses (const vector<AttributeList*>& s)
{
  vector<clause>& c = *new vector<clause>;
  
  for (vector<AttributeList*>::const_iterator i = s.begin ();
       i != s.end ();
       i++)
    {
      Condition& condition 
	= Librarian<Condition>::create ((*i)->alist ("condition"));
      vector<Action*> actions 
	= map_create<Action> ((*i)->alist_sequence ("actions"));
      c.push_back (clause (&condition, actions));
    }
  return c;
}

struct ActionCond : public Action
{
  vector<clause>& clauses;

  void tick (const Daisy& daisy)
  { 
    for (vector<clause>::iterator i = clauses.begin (); 
	 i != clauses.end ();
	 i++)
      {
	(*i).condition->tick (daisy);
	vector<Action*>& actions = (*i).actions;
	for (unsigned int j = 0; j < actions.size (); j++)
	  actions[j]->tick (daisy);
      }
  }

  void doIt (Daisy& daisy)
  { 
    for (vector<clause>::iterator i = clauses.begin (); 
	 i != clauses.end ();
	 i++)
      {
	if ((*i).condition->match (daisy))
	  {
	    vector<Action*>& actions = (*i).actions;
	    for (unsigned int j = 0; j < actions.size (); j++)
	      actions[j]->doIt (daisy);
	    break;
	  }
      }
  }

  void output (Log& log) const
  { 
    if (log.check ("clauses"))
      {
	log.open ("clauses");
	for (vector<clause>::const_iterator item = clauses.begin ();
	     item != clauses.end ();
	     item++)
	  {
	    Log::Unnamed unnamed (log);
	    (*item).output (log);
	  }
	log.close ();
      }
  }

  bool check (const Daisy& daisy, Treelog& err) const
  { 
    bool ok = true;
    for (vector<clause>::const_iterator i = clauses.begin (); 
	 i != clauses.end ();
	 i++)
      {
	const vector<Action*>& actions = (*i).actions;
	for (unsigned int j = 0; j < actions.size (); j++)
	  if (!actions[j]->check (daisy, err))
	    ok = false;
      }
    return ok;
  }

  ActionCond (const AttributeList& al)
    : Action (al),
      clauses (make_clauses (al.alist_sequence ("clauses")))
  { }

  ~ActionCond ()
  { 
    for (vector<clause>::const_iterator i = clauses.begin (); 
	 i != clauses.end ();
	 i++)
      {
#ifdef CONST_DELETE
	delete (*i).condition;
#endif
      }
    delete &clauses;
  }
};

struct ActionIf : public Action
{
  Condition& if_c;
  Action& then_a;
  Action& else_a;

  void tick (const Daisy& daisy)
  { 
    if_c.tick (daisy);
    then_a.tick (daisy);
    else_a.tick (daisy);
  }

  void doIt (Daisy& daisy)
  { 
    if (if_c.match (daisy))
      then_a.doIt (daisy);
    else
      else_a.doIt (daisy);
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
    if (!then_a.check (daisy, err))
      ok = false;
    if (!else_a.check (daisy, err))
      ok = false;
    return ok;
  }

  ActionIf (const AttributeList& al)
    : Action (al),
      if_c (Librarian<Condition>::create (al.alist ("if"))),
      then_a (Librarian<Action>::create (al.alist ("then"))),
      else_a (Librarian<Action>::create (al.alist ("else")))
  { }

  ~ActionIf ()
  { 
#ifdef CONST_DELETE
    delete &if_c;
#endif
    delete &then_a;
    delete &else_a;
  }
};

static struct ActionLispSyntax
{
  static Action& make_nil (const AttributeList& al)
  { return *new ActionNil (al); }
  static Action& make_t (const AttributeList& al)
  { return *new ActionT (al); }
  static Action& make_progn (const AttributeList& al)
  { return *new ActionProgn (al); }
  static Action& make_cond (const AttributeList& al)
  { return *new ActionCond (al); }
  static Action& make_if (const AttributeList& al)
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
    syntax.add ("actions", Librarian<Action>::library (), Syntax::Sequence,
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
    Syntax& clauseSyntax = *new Syntax ();
    clauseSyntax.add ("condition",
		      Librarian<Condition>::library (), 
		      "Condition for performing the actions.");
    clauseSyntax.add ("actions", 
		      Librarian<Action>::library (), Syntax::Sequence, 
		      "Actions to perform when condition is meet.");
    clauseSyntax.order ("condition", "actions");
    syntax.add ("clauses", clauseSyntax, Syntax::Sequence,
		"\
Each clause consist of a condition and a sequence of actions.\n\
The first clause whose condition is true, will have its actions activated.");
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
    syntax.add ("if", Librarian<Condition>::library (), 
		"Condition determining which action to perform.");
    syntax.add ("then", Librarian<Action>::library (), 
		"Action to perform if the condition is true.");
    syntax.add ("else", Librarian<Action>::library (), 
		"Action to perform if the condition is false.");
    syntax.order ("if", "then", "else");
    AttributeList& nilAlist = *new AttributeList ();
    nilAlist.add ("type", "nil");
    alist.add ("else", nilAlist);
    Librarian<Action>::add_type ("if", alist, syntax, &make_if);
  }
}
