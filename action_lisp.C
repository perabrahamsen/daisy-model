// action_lisp.C

#include "action.h"
#include "daisy.h"
#include "column.h"
// We need to initialize the Condition library.
#include "condition.h"

struct ActionNil : public Action
{
  void doIt (const Frame&, Daisy&)
    { }

  ActionNil (const AttributeList& al)
    : Action (al.name ("type"))
    { }
};

struct ActionProgn : public Action
{
  vector<Action*>& actions;

  void doIt (const Frame& frame, Daisy& daisy)
    { 
      for (vector<Action*>::iterator i = actions.begin ();
	   i != actions.end ();
	   i++)
	{
	  (*i)->doIt (frame, daisy);
	}
    }

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

  ActionProgn (const AttributeList& al)
    : Action (al.name ("type")),
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
  const Condition* condition;
  Action* action;
  clause (const Condition *const c, Action *const a) 
    : condition (c),
      action (a)
    { }
  clause ()
    : condition (NULL),
      action (NULL)
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
      const AttributeList& condition = (*i)->alist ("condition");
      const AttributeList& action = (*i)->alist ("action");
      c.push_back (clause (&Librarian<Condition>::create (condition),
			   &Librarian<Action>::create (action)));
    }
  return c;
}

struct ActionCond : public Action
{
  vector<clause>& clauses;

  void doIt (const Frame& frame, Daisy& daisy)
    { 
      for (vector<clause>::iterator i = clauses.begin (); 
	   i != clauses.end ();
	   i++)
	{
	  if ((*i).condition->match (frame, daisy))
	    {
	      (*i).action->doIt (frame, daisy);
	      break;
	    }
	}
    }
  
  bool check (Daisy& daisy) const
    { 
      bool ok = true;
      for (vector<clause>::const_iterator i = clauses.begin (); 
	   i != clauses.end ();
	   i++)
	{
	  if (!(*i).action->check (daisy))
	    ok = false;
	}
      return ok;
    }

  ActionCond (const AttributeList& al)
    : Action (al.name ("type")),
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
	  delete (*i).action;
#endif
	}
      delete &clauses;
    }
};

struct ActionIf : public Action
{
  const Condition& if_c;
  Action& then_a;
  Action& else_a;

  void doIt (const Frame& frame, Daisy& daisy)
    { 
      if (if_c.match (frame, daisy))
	then_a.doIt (frame, daisy);
      else
	else_a.doIt (frame, daisy);
    }

  bool check (Daisy& daisy) const
    { 
      bool ok = true; 
      if (!then_a.check (daisy))
	ok = false;
      if (!else_a.check (daisy))
	ok = false;
      return ok;
    }

  ActionIf (const AttributeList& al)
    : Action (al.name ("type")),
      if_c (Librarian<Condition>::create (al.alist ("if"))),
      then_a (Librarian<Action>::create (al.alist ("then"))),
      else_a (Librarian<Action>::create (al.alist ("else")))
    { }

  ~ActionIf ()
    { 
#ifdef CONST_DELETE
      delete &if_c;
      delete &then_a;
      delete &else_a;
#endif
    }
};

static struct ActionLispSyntax
{
  static Action& make_nil (const AttributeList& al)
    { return *new ActionNil (al); }
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
    Librarian<Action>::add_type ("nil", alist, syntax, &make_nil);
  }
  // "progn"
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    syntax.add ("actions", Librarian<Action>::library (), Syntax::Const,
		Syntax::Sequence);
    syntax.order ("actions");
    Librarian<Action>::add_type ("progn", alist, syntax, &make_progn);
  }
  // "cond"
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    Syntax& clauseSyntax = *new Syntax ();
    clauseSyntax.add ("condition",
		      Librarian<Condition>::library (), Syntax::Const);
    clauseSyntax.add ("action", Librarian<Action>::library (), Syntax::Const);
    clauseSyntax.order ("condition", "action");
    syntax.add ("clauses", clauseSyntax, Syntax::Const, Syntax::Sequence);
    syntax.order ("clauses");
    Librarian<Action>::add_type ("cond", alist, syntax, &make_cond);
  }
  // "if"
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    syntax.add ("if", Librarian<Condition>::library (), Syntax::Const);
    syntax.add ("then", Librarian<Action>::library (), Syntax::Const);
    syntax.add ("else", Librarian<Action>::library (), Syntax::Const);
    syntax.order ("if", "then", "else");
    AttributeList& nilAlist = *new AttributeList ();
    nilAlist.add ("type", "nil");
    alist.add ("else", nilAlist);
    Librarian<Action>::add_type ("if", alist, syntax, &make_if);
  }
}
