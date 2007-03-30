// condition_logic.C
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

//
// Logical operators.

#include "condition.h"
#include "syntax.h"
#include "alist.h"
#include "memutils.h"
#include <memory>

using namespace std;

struct ConditionFalse : public Condition
{
  bool match (const Daisy&, Treelog&) const
  { return false; }
  void output (Log&) const
  { }

  ConditionFalse (Block& al)
    : Condition (al)
  { }

  ~ConditionFalse ()
  { }
};

struct ConditionTrue : public Condition
{
  bool match (const Daisy&, Treelog&) const
  { return true; }
  void output (Log&) const
  { }

  ConditionTrue (Block& al)
    : Condition (al)
  { }

  ConditionTrue (const char* const id)
    : Condition (id)
  { }

  ~ConditionTrue ()
  { }
};

std::auto_ptr<Condition> 
Condition::create_true ()
{ return std::auto_ptr<Condition> (new ConditionTrue (__FUNCTION__)); }

struct ConditionOr : public Condition
{
  const vector<Condition*> conditions;

  void tick (const Daisy& daisy, Treelog& out)
  {
    for (vector<Condition*>::const_iterator i = conditions.begin ();
	 i != conditions.end ();
	 i++)
      {
	(*i)->tick (daisy, out);
      }
  }
  bool match (const Daisy& daisy, Treelog& msg) const
  {
    for (vector<Condition*>::const_iterator i = conditions.begin ();
	 i != conditions.end ();
	 i++)
      {
	if ((*i)->match (daisy, msg))
	  return true;
      }
    return false;
  }
  void output (Log&) const
  { }

  ConditionOr (Block& al)
    : Condition (al),
      conditions (Librarian<Condition>::build_vector (al, "operands"))
  { }

  ~ConditionOr ()
  { sequence_delete (conditions.begin (), conditions.end ()); }
};

struct ConditionAnd : public Condition
{
  const vector<Condition*> conditions;

  void tick (const Daisy& daisy, Treelog& out)
  {
    for (vector<Condition*>::const_iterator i = conditions.begin ();
	 i != conditions.end ();
	 i++)
      {
	(*i)->tick (daisy, out);
      }
  }
  bool match (const Daisy& daisy, Treelog& msg) const
  {
    for (vector<Condition*>::const_iterator i = conditions.begin ();
	 i != conditions.end ();
	 i++)
      {
	if (!(*i)->match (daisy, msg))
	  return false;
      }
    return true;
  }
  void output (Log&) const
  { }

  ConditionAnd (Block& al)
    : Condition (al),
      conditions (Librarian<Condition>::build_vector (al, "operands"))
  { }

  ~ConditionAnd ()
  { sequence_delete (conditions.begin (), conditions.end ()); }
};

struct ConditionNot : public Condition
{
  auto_ptr<Condition> condition;

  bool match (const Daisy& daisy, Treelog& msg) const
  { return !condition->match (daisy, msg); }

  void tick (const Daisy& daisy, Treelog& out)
  { condition->tick (daisy, out); }

  void output (Log&) const
  { }

  ConditionNot (Block& al)
    : Condition (al),
      condition (Librarian<Condition>::build_item (al, "operand"))
  { }

  ~ConditionNot ()
  { }
};

struct ConditionIf : public Condition
{
  auto_ptr<Condition> if_c;
  auto_ptr<Condition> then_c;
  auto_ptr<Condition> else_c;

  void tick (const Daisy& daisy, Treelog& out)
  { 
    if_c->tick (daisy, out);
    then_c->tick (daisy, out);
    else_c->tick (daisy, out);
  }
  bool match (const Daisy& daisy, Treelog& msg) const
  { 
    if (if_c->match (daisy, msg))
      return then_c->match (daisy, msg);
    else
      return else_c->match (daisy, msg); 
  }
  void output (Log&) const
  { }

  ConditionIf (Block& al)
    : Condition (al),
      if_c (Librarian<Condition>::build_item (al, "if")),
      then_c (Librarian<Condition>::build_item (al, "then")),
      else_c (Librarian<Condition>::build_item (al, "else"))
  { }

  ~ConditionIf ()
  { }
};

static struct ConditionLogicSyntax
{
  static Model& make_false (Block& al)
  { return *new ConditionFalse (al); }
  static Model& make_true (Block& al)
  { return *new ConditionTrue (al); }
  static Model& make_or (Block& al)
  { return *new ConditionOr (al); }
  static Model& make_and (Block& al)
  { return *new ConditionAnd (al); }
  static Model& make_not (Block& al)
  { return *new ConditionNot (al); }
  static Model& make_if (Block& al)
  { return *new ConditionIf (al); }
  ConditionLogicSyntax ();
} ConditionLogic_syntax;

ConditionLogicSyntax::ConditionLogicSyntax ()
{
  // "false", "true".
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist_false = *new AttributeList ();
    alist_false.add ("description", "Always false.");
    AttributeList& alist_true = *new AttributeList ();
    alist_true.add ("description", "Always true.");
    BuildBase::add_type (Condition::component, "false", alist_false, syntax, &make_false);
    BuildBase::add_type (Condition::component, "true", alist_true, syntax, &make_true);
  }

  // "or", "and".
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist_or = *new AttributeList ();
    alist_or.add ("description", "\
True iff any of the listed conditions are true.\n\
The conditions are tested in the sequence listed, until a true is found,\n\
or the end of the list is reached.");
    AttributeList& alist_and = *new AttributeList ();
    alist_and.add ("description", "\
True iff all the listed conditions are true.\n\
The conditions are tested in the sequence listed, until a false is found,\n\
or the end of the list is reached.");
    syntax.add_object ("operands", Condition::component, 
                       Syntax::State, Syntax::Sequence, "Conditions to test.");
    syntax.order ("operands");
    BuildBase::add_type (Condition::component, "or", alist_or, syntax, &make_or);
    BuildBase::add_type (Condition::component, "and", alist_and, syntax, &make_and);
  }
  // "not".
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "True iff the operand is not true.");
    syntax.add_object ("operand", Condition::component, 
                       "Condition to test.");
    syntax.order ("operand");
    BuildBase::add_type (Condition::component, "not", alist, syntax, &make_not);
  }
  // "if".
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "\
If the first condition is true, return the value of the second condition,\n\
else return the value of the third condition.");
    syntax.add_object ("if", Condition::component, 
                       "Condition to test for.");
    syntax.add_object ("then", Condition::component, 
                       "Condition to use of the 'if' test was true.");
    syntax.add_object ("else", Condition::component, 
                       "Condition to use if the 'if' test was false.");
    syntax.order ("if", "then", "else");
    BuildBase::add_type (Condition::component, "if", alist, syntax, &make_if);
  }
}
