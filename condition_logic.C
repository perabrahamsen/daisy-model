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

#define BUILD_DLL

#include "condition.h"
#include "log.h"
#include "syntax.h"
#include "alist.h"
#include "memutils.h"
#include "librarian.h"
#include <memory>

struct ConditionFalse : public Condition
{
  bool match (const Daisy&, const Scope&, Treelog&) const
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
  bool match (const Daisy&, const Scope&, Treelog&) const
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

struct ConditionOperands : public Condition
{
  const auto_vector<Condition*> conditions;

  void tick (const Daisy& daisy, const Scope& scope, Treelog& out)
  {
    for (std::vector<Condition*>::const_iterator i = conditions.begin ();
	 i != conditions.end ();
	 i++)
      {
	(*i)->tick (daisy, scope, out);
      }
  }
  void output (Log& log) const
  { 
    output_vector (conditions, "operands", log);
  }

  void initialize (const Daisy& daisy, const Scope& scope, Treelog& msg)
  { 
    for (std::vector<Condition*>::const_iterator i = conditions.begin ();
	 i != conditions.end ();
	 i++)
      (*i)->initialize (daisy, scope, msg);
  }

  bool check (const Daisy& daisy, const Scope& scope, Treelog& msg) const
  {
    bool ok = true;
    for (std::vector<Condition*>::const_iterator i = conditions.begin ();
	 i != conditions.end ();
	 i++)
      {
	if (!(*i)->check (daisy, scope, msg))
	  ok = false;
      }
    return ok;
  }


  ConditionOperands (Block& al)
    : Condition (al),
      conditions (Librarian::build_vector<Condition> (al, "operands"))
  { }

  ~ConditionOperands ()
  { }
};

struct ConditionOr : public ConditionOperands
{
  bool match (const Daisy& daisy, const Scope& scope, Treelog& msg) const
  {
    for (std::vector<Condition*>::const_iterator i = conditions.begin ();
	 i != conditions.end ();
	 i++)
      {
	if ((*i)->match (daisy, scope, msg))
	  return true;
      }
    return false;
  }


  ConditionOr (Block& al)
    : ConditionOperands (al)
  { }

  ~ConditionOr ()
  { }
};

struct ConditionAnd : public ConditionOperands
{
  bool match (const Daisy& daisy, const Scope& scope, Treelog& msg) const
  {
    for (std::vector<Condition*>::const_iterator i = conditions.begin ();
	 i != conditions.end ();
	 i++)
      {
	if (!(*i)->match (daisy, scope, msg))
	  return false;
      }
    return true;
  }

  ConditionAnd (Block& al)
    : ConditionOperands (al)
  { }

};

struct ConditionNot : public Condition
{
  std::auto_ptr<Condition> condition;

  bool match (const Daisy& daisy, const Scope& scope, Treelog& msg) const
  { return !condition->match (daisy, scope, msg); }

  void tick (const Daisy& daisy, const Scope& scope, Treelog& out)
  { condition->tick (daisy, scope, out); }

  void output (Log&) const
  { }

  ConditionNot (Block& al)
    : Condition (al),
      condition (Librarian::build_item<Condition> (al, "operand"))
  { }

  ~ConditionNot ()
  { }
};

struct ConditionIf : public Condition
{
  std::auto_ptr<Condition> if_c;
  std::auto_ptr<Condition> then_c;
  std::auto_ptr<Condition> else_c;

  void tick (const Daisy& daisy, const Scope& scope, Treelog& out)
  { 
    if_c->tick (daisy, scope, out);
    then_c->tick (daisy, scope, out);
    else_c->tick (daisy, scope, out);
  }
  bool match (const Daisy& daisy, const Scope& scope, Treelog& msg) const
  { 
    if (if_c->match (daisy, scope, msg))
      return then_c->match (daisy, scope, msg);
    else
      return else_c->match (daisy, scope, msg); 
  }
  void output (Log&) const
  { }

  ConditionIf (Block& al)
    : Condition (al),
      if_c (Librarian::build_item<Condition> (al, "if")),
      then_c (Librarian::build_item<Condition> (al, "then")),
      else_c (Librarian::build_item<Condition> (al, "else"))
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
    Librarian::add_type (Condition::component, "false", alist_false, syntax, &make_false);
    Librarian::add_type (Condition::component, "true", alist_true, syntax, &make_true);
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
    Librarian::add_type (Condition::component, "or", alist_or, syntax, &make_or);
    Librarian::add_type (Condition::component, "and", alist_and, syntax, &make_and);
  }
  // "not".
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "True iff the operand is not true.");
    syntax.add_object ("operand", Condition::component, 
                       "Condition to test.");
    syntax.order ("operand");
    Librarian::add_type (Condition::component, "not", alist, syntax, &make_not);
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
    Librarian::add_type (Condition::component, "if", alist, syntax, &make_if);
  }
}
