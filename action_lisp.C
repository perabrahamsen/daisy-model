// action_lisp.C

#include "action.h"
#include "daisy.h"
#include "column.h"
#include "condition.h"		// We need to initialize the Condition library.
#include "syntax.h"
#include "alist.h"
#include "common.h"
#include <iostream.h>

class ActionNil : public Action
{
public:
  void doIt (Daisy&)
  { }

  // Create and Destroy.
private:
  friend class ActionLispSyntax;
  static Action& make (const AttributeList& al, const Action *const p)
  { return *new ActionNil (al, p); }
  ActionNil (const AttributeList&, const Action *const p)
    : Action (p)
  { }
public:
  ~ActionNil ()
  { }
};

class ActionProgn : public Action
{
  vector<Action*>& actions;

public:
  void doIt (Daisy& daisy)
  { 
    for (vector<Action*>::iterator i = actions.begin ();
	 i != actions.end ();
	 i++)
      {
	(*i)->doIt (daisy);
      }
  }

  // Create and Destroy.
public:
  bool check (Daisy& daisy) const
  { 
    bool ok = true;
    for (vector<const Action*>::const_iterator i = actions.begin ();
	 i != actions.end ();
	 i++)
      {
	if (!(*i)->check (daisy))
	  ok = false;
      }
    return ok;
  }
private:
  friend class ActionLispSyntax;
  static Action& make (const AttributeList& al, const Action *const p)
  { return *new ActionProgn (al, p); }
  ActionProgn (const AttributeList& al, const Action *const p)
    : Action (p),
      actions (map_create1<Action, const Action *const>
	       (al.list_sequence ("actions"), this))
  { }
public:
  ~ActionProgn ()
  { 
    sequence_delete (actions.begin (), actions.end ());
    delete &actions;
  }
};

class ActionCond : public Action
{
  typedef vector<pair<const Condition*, Action*>/**/> clause_t;
  clause_t& make_clauses (const vector<const AttributeList*>& s, 
			  const Action *const p)
  {
    clause_t& c = *new clause_t;

    for (vector<const AttributeList*>::const_iterator i = s.begin ();
	 i != s.end ();
	 i++)
      {
	c.push_back (pair<const Condition*, Action*>
		     (&Condition::create ((*i)->list ("condition")),
		      &Action::create ((*i)->list ("action"), p)));
      }
    return c;
  }
    
  clause_t& clauses;

public:
  void doIt (Daisy& daisy)
  { 
    for (clause_t::iterator i = clauses.begin (); 
	 i != clauses.end ();
	 i++)
      {
	if ((*i).first->match (daisy))
	  {
	    (*i).second->doIt (daisy);
	    break;
	  }
      }
  }
  
  // Create and Destroy.
public:
  bool check (Daisy& daisy) const
  { 
    bool ok = true;
    for (clause_t::const_iterator i = clauses.begin (); 
	 i != clauses.end ();
	 i++)
      {
	if (!(*i).second->check (daisy))
	  ok = false;
      }
    return ok;
  }
private:
  friend class ActionLispSyntax;
  static Action& make (const AttributeList& al, const Action *const p)
  { return *new ActionCond (al, p); }
  ActionCond (const AttributeList& al, const Action *const p)
    : Action (p),
      clauses (make_clauses (al.list_sequence ("clauses"), this))
  { }
public:
  ~ActionCond ()
  { 
    for (clause_t::const_iterator i = clauses.begin (); 
	 i != clauses.end ();
	 i++)
      {
	delete (*i).first;
	delete (*i).second;
      }
    delete &clauses;
  }
};

class ActionIf : public Action
{
  const Condition& if_c;
  Action& then_a;
  Action& else_a;

public:
  void doIt (Daisy& daisy)
  { 
    if (if_c.match (daisy))
      then_a.doIt (daisy);
    else
      else_a.doIt (daisy);
  }

  // Create and Destroy.
public:
  bool check (Daisy& daisy) const
  { 
    bool ok = true; 
    if (!then_a.check (daisy))
      ok = false;
    if (!else_a.check (daisy))
      ok = false;
    return ok;
  }
private:
  friend class ActionLispSyntax;
  static Action& make (const AttributeList& al, const Action *const p)
  { return *new ActionIf (al, p); }
  ActionIf (const AttributeList& al, const Action *const p)
    : Action (p),
      if_c (Condition::create (al.list ("if"))),
      then_a (Action::create (al.list ("then"), this)),
      else_a (Action::create (al.list ("else"), this))
  { }
public:
  ~ActionIf ()
  { 
    delete &if_c;
    delete &then_a;
    delete &else_a;
  }
};

static struct ActionLispSyntax
{
  ActionLispSyntax ();
} ActionLisp_syntax;

ActionLispSyntax::ActionLispSyntax ()
{ 
  // "nil"
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    Action::add_type ("nil", alist, syntax, &ActionNil::make);
  }
  // "progn"
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    syntax.add ("actions", Action::library (), Syntax::Const,
		Syntax::Sequence);
    syntax.order ("actions");
    Action::add_type ("progn", alist, syntax, &ActionProgn::make);
  }
  // "cond"
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    Syntax& clauseSyntax = *new Syntax ();
    clauseSyntax.add ("condition", Condition::library (), Syntax::Const);
    clauseSyntax.add ("action", Action::library (), Syntax::Const);
    clauseSyntax.order ("condition", "action");
    syntax.add ("clauses", clauseSyntax, Syntax::Const, Syntax::Sequence);
    syntax.order ("clauses");
    Action::add_type ("cond", alist, syntax, &ActionCond::make);
  }
  // "if"
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    syntax.add ("if", Condition::library (), Syntax::Const);
    syntax.add ("then", Action::library (), Syntax::Const);
    syntax.add ("else", Action::library (), Syntax::Const);
    syntax.order ("if", "then", "else");
    AttributeList& nilAlist = *new AttributeList ();
    nilAlist.add ("type", "nil");
    alist.add ("else", nilAlist);
    Action::add_type ("if", alist, syntax, &ActionIf::make);
  }
}
