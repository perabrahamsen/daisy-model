// condition_logic.C
//
// Logical operators.

#include "condition.h"

struct ConditionOr : public Condition
{
  const vector<const Condition*>& conditions;

  bool match (const Frame& frame, const Daisy& daisy) const
    {
      for (vector<const Condition*>::const_iterator i = conditions.begin ();
	   i != conditions.end ();
	   i++)
	{
	  if ((*i)->match (frame, daisy))
	    return true;
	}
      return false;
    }

  ConditionOr (const AttributeList& al)
    : conditions (map_create_const<Condition> (al.alist_sequence ("operands")))
    { }

  ~ConditionOr ()
    {
#ifdef CONST_DELETE
      sequence_delete (conditions.begin (), conditions.end ());
#endif
      delete &conditions;
    }
};

struct ConditionAnd : public Condition
{
  const vector<const Condition*>& conditions;

  bool match (const Frame& frame, const Daisy& daisy) const
    {
      for (vector<const Condition*>::const_iterator i = conditions.begin ();
	   i != conditions.end ();
	   i++)
	{
	  if (!(*i)->match (frame, daisy))
	    return false;
	}
      return true;
    }

  ConditionAnd (const AttributeList& al)
    : conditions (map_create_const<Condition> (al.alist_sequence ("operands")))
    { }

  ~ConditionAnd ()
    {
#ifdef CONST_DELETE
      sequence_delete (conditions.begin (), conditions.end ());
#endif
      delete &conditions;
    }
};

struct ConditionNot : public Condition
{
  const Condition& condition;

  bool match (const Frame& frame, const Daisy& daisy) const
    { return !condition.match (frame, daisy); }

  ConditionNot (const AttributeList& al)
    : condition (Librarian<Condition>::create (al.alist ("operand")))
    { }

  ~ConditionNot ()
    {
#ifdef CONST_DELETE
      delete &condition; 
#endif
    }
};

struct ConditionIf : public Condition
{
  const Condition& if_c;
  const Condition& then_c;
  const Condition& else_c;

  bool match (const Frame& frame, const Daisy& daisy) const
    { 
      if (if_c.match (frame, daisy))
	return then_c.match (frame, daisy);
      else
	return else_c.match (frame, daisy); 
    }

  ConditionIf (const AttributeList& al)
    : if_c (Librarian<Condition>::create (al.alist ("if"))),
      then_c (Librarian<Condition>::create (al.alist ("then"))),
      else_c (Librarian<Condition>::create (al.alist ("else")))
    { }

  ~ConditionIf ()
    {
#ifdef CONST_DELETE
      delete &if_c;
      delete &then_c;
      delete &else_c;
#endif
    }
};

static struct ConditionLogicSyntax
{
  static Condition& make_or (const AttributeList& al)
    { return *new ConditionOr (al); }
  static Condition& make_and (const AttributeList& al)
    { return *new ConditionAnd (al); }
  static Condition& make_not (const AttributeList& al)
    { return *new ConditionNot (al); }
  static Condition& make_if (const AttributeList& al)
    { return *new ConditionIf (al); }
  ConditionLogicSyntax ();
} ConditionLogic_syntax;

ConditionLogicSyntax::ConditionLogicSyntax ()
{
  // "or", "and".
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    syntax.add ("operands", Librarian<Condition>::library (), 
		Syntax::Const, Syntax::Sequence);
    syntax.order ("operands");
    Librarian<Condition>::add_type ("or", alist, syntax, &make_or);
    Librarian<Condition>::add_type ("and", alist, syntax, &make_and);
  }
  // "not".
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    syntax.add ("operand", Librarian<Condition>::library (), Syntax::Const);
    syntax.order ("operand");
    Librarian<Condition>::add_type ("not", alist, syntax, &make_not);
  }
  // "if".
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    syntax.add ("if", Librarian<Condition>::library (), Syntax::Const);
    syntax.add ("then", Librarian<Condition>::library (), Syntax::Const);
    syntax.add ("else", Librarian<Condition>::library (), Syntax::Const);
    syntax.order ("if", "then", "else");
    Librarian<Condition>::add_type ("if", alist, syntax, &make_if);
  }
}
