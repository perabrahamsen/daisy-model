// condition_logic.C
//
// Logical operators.

#include "condition.h"
#include "syntax.h"
#include "alist.h"
#include "common.h"
#include <vector.h>

class ConditionOr : public Condition
{
  const vector<const Condition*>& conditions;
public:
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
    : conditions (map_create<const Condition> (al.list_sequence ("operands")))
  { }
  ~ConditionOr ()
  {
    sequence_delete (conditions.begin (), conditions.end ());
    delete &conditions;
  }
  static Condition& make (const AttributeList& al)
  { return *new ConditionOr (al); }
};

class ConditionAnd : public Condition
{
  const vector<const Condition*>& conditions;
public:
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
  ConditionAnd (const AttributeList& al)
    : conditions (map_create<const Condition> (al.list_sequence ("operands")))
  { }
  ~ConditionAnd ()
  {
    sequence_delete (conditions.begin (), conditions.end ());
    delete &conditions;
  }
  static Condition& make (const AttributeList& al)
  { return *new ConditionAnd (al); }
};

class ConditionNot : public Condition
{
  const Condition& condition;
public:
  bool match (const Daisy& daisy) const
  { return !condition.match (daisy); }
  ConditionNot (const AttributeList& al)
    : condition (Condition::create (al.list ("operand")))
  { }
  ~ConditionNot ()
  { delete &condition; }
  static Condition& make (const AttributeList& al)
  { return *new ConditionNot (al); }
};

class ConditionIf : public Condition
{
  const Condition& if_c;
  const Condition& then_c;
  const Condition& else_c;
public:
  bool match (const Daisy& daisy) const
  { return if_c.match (daisy) ? then_c.match (daisy) : else_c.match (daisy); }
  ConditionIf (const AttributeList& al)
    : if_c (Condition::create (al.list ("if"))),
      then_c (Condition::create (al.list ("then"))),
      else_c (Condition::create (al.list ("else")))
  { }
  ~ConditionIf ()
  {
    delete &if_c;
    delete &then_c;
    delete &else_c;
  }
  static Condition& make (const AttributeList& al)
  { return *new ConditionIf (al); }
};

static struct ConditionLogicSyntax
{
  ConditionLogicSyntax ();
} ConditionLogic_syntax;

ConditionLogicSyntax::ConditionLogicSyntax ()
{
  // "or", "and".
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    syntax.add ("operands", Condition::library (), 
		Syntax::Const, Syntax::Sequence);
    syntax.order ("operands");
    Condition::add_type ("or", alist, syntax, &ConditionOr::make);
    Condition::add_type ("and", alist, syntax, &ConditionAnd::make);
  }
  // "not".
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    syntax.add ("operand", Condition::library (), Syntax::Const);
    syntax.order ("operand");
    Condition::add_type ("not", alist, syntax, &ConditionNot::make);
  }
  // "if".
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    syntax.add ("if", Condition::library (), Syntax::Const);
    syntax.add ("then", Condition::library (), Syntax::Const);
    syntax.add ("else", Condition::library (), Syntax::Const);
    syntax.order ("if");
    Condition::add_type ("if", alist, syntax, &ConditionIf::make);
  }
}
