// condition_logic.C
//
// Logical operators.

#include "condition.h"

struct ConditionOr : public Condition
{
  const vector<const Condition*>& conditions;

  bool match (const Daisy& daisy) const
    {
      for (vector<const Condition*>::const_iterator i = conditions.begin ();
	   i != conditions.end ();
	   i++)
	{
	  if ((*i)->match (daisy))
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

  bool match (const Daisy& daisy) const
    {
      for (vector<const Condition*>::const_iterator i = conditions.begin ();
	   i != conditions.end ();
	   i++)
	{
	  if (!(*i)->match (daisy))
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

  bool match (const Daisy& daisy) const
    { return !condition.match (daisy); }

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

  bool match (const Daisy& daisy) const
    { 
      if (if_c.match (daisy))
	return then_c.match (daisy);
      else
	return else_c.match (daisy); 
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
    AttributeList& alist_or = *new AttributeList ();
    alist_or.add ("description", "\
True iff any of the listed conditions are true.\n\
The conditions are tested in the sequence listed, until a true is found,\n\
or the end of the list is reached.");
    AttributeList& alist_and = *new AttributeList ();
    alist_or.add ("description", "\
True iff all the listed conditions are true.\n\
The conditions are tested in the sequence listed, until a false is found,\n\
or the end of the list is reached.");
    syntax.add ("operands", Librarian<Condition>::library (), 
		Syntax::Sequence, "Conditions to test.");
    syntax.order ("operands");
    Librarian<Condition>::add_type ("or", alist_or, syntax, &make_or);
    Librarian<Condition>::add_type ("and", alist_and, syntax, &make_and);
  }
  // "not".
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "True iff the operand is not true.");
    syntax.add ("operand", Librarian<Condition>::library (), 
		"Condition to test.");
    syntax.order ("operand");
    Librarian<Condition>::add_type ("not", alist, syntax, &make_not);
  }
  // "if".
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "\
If the first condition is true, return the value of the second condition,\n\
else return the value of the third condition.");
    syntax.add ("if", Librarian<Condition>::library (), 
		"Condition to test for.");
    syntax.add ("then", Librarian<Condition>::library (), 
		"Condition to use of the `if' test was true.");
    syntax.add ("else", Librarian<Condition>::library (), 
		"Condition to use if the `if' test was false.");
    syntax.order ("if", "then", "else");
    Librarian<Condition>::add_type ("if", alist, syntax, &make_if);
  }
}
