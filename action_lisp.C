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
  void doIt (Daisy&) const
  { }

  // Create and Destroy.
private:
  friend class ActionLispSyntax;
  static Action& make (const AttributeList& al)
  { return *new ActionNil (al); }
  ActionNil (const AttributeList&)
  { }
public:
  ~ActionNil ()
  { }
};

class ActionProgn : public Action
{
  const vector<const Action*>& actions;

public:
  void doIt (Daisy& daisy) const
  { 
    for (vector<const Action*>::const_iterator i = actions.begin ();
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
  static Action& make (const AttributeList& al)
  { return *new ActionProgn (al); }
  ActionProgn (const AttributeList& al)
    : actions (map_create<const Action> (al.list_sequence ("actions")))
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
  typedef vector<pair<const Condition*, const Action*>/**/> clause_t;
  const clause_t& make_clauses (const vector<const AttributeList*>& s)
  {
    clause_t& c = *new clause_t;

    for (vector<const AttributeList*>::const_iterator i = s.begin ();
	 i != s.end ();
	 i++)
      {
	c.push_back (pair<const Condition*, const Action*>
		     (&Condition::create ((*i)->list ("condition")),
		      &Action::create ((*i)->list ("action"))));
      }
    return c;
  }
    
  const clause_t& clauses;

public:
  void doIt (Daisy& daisy) const
  { 
    for (clause_t::const_iterator i = clauses.begin (); 
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
  static Action& make (const AttributeList& al)
  { return *new ActionCond (al); }
  ActionCond (const AttributeList& al)
    : clauses (make_clauses (al.list_sequence ("clauses")))
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
  const Action& then_a;
  const Action& else_a;

public:
  void doIt (Daisy& daisy) const
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
  static Action& make (const AttributeList& al)
  { return *new ActionIf (al); }
  ActionIf (const AttributeList& al)
    : if_c (Condition::create (al.list ("if"))),
      then_a (Action::create (al.list ("then"))),
      else_a (Action::create (al.list ("else")))
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
