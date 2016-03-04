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
#include "frame.h"
#include "memutils.h"
#include "librarian.h"
#include "block_model.h"
#include <memory>

struct ConditionFalse : public Condition
{
  bool match (const Daisy&, const Scope&, Treelog&) const
  { return false; }
  void output (Log&) const
  { }

  void tick (const Daisy&, const Scope&, Treelog&)
  { }

  void initialize (const Daisy&, const Scope&, Treelog&)
  { }

  bool check (const Daisy&, const Scope&, Treelog&) const
  { return true; }

  ConditionFalse (const BlockModel& al)
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

  void tick (const Daisy&, const Scope&, Treelog&)
  { }

  void initialize (const Daisy&, const Scope&, Treelog&)
  { }

  bool check (const Daisy&, const Scope&, Treelog&) const
  { return true; }

  ConditionTrue (const BlockModel& al)
    : Condition (al)
  { }

  ConditionTrue (const char* const id)
    : Condition (id)
  { }

  ~ConditionTrue ()
  { }
};

std::unique_ptr<Condition> 
Condition::create_true ()
{ return std::unique_ptr<Condition> (new ConditionTrue (__FUNCTION__)); }

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
    output_list (conditions, "operands", log, Condition::component);
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


  ConditionOperands (const BlockModel& al)
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


  ConditionOr (const BlockModel& al)
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

  ConditionAnd (const BlockModel& al)
    : ConditionOperands (al)
  { }

};

struct ConditionNot : public Condition
{
  std::unique_ptr<Condition> condition;

  bool match (const Daisy& daisy, const Scope& scope, Treelog& msg) const
  { return !condition->match (daisy, scope, msg); }

  void tick (const Daisy& daisy, const Scope& scope, Treelog& out)
  { condition->tick (daisy, scope, out); }

  void output (Log&) const
  { }

  void initialize (const Daisy& daisy, const Scope& scope, Treelog& msg)
  { condition->initialize (daisy, scope, msg); }

  bool check (const Daisy& daisy, const Scope& scope, Treelog& msg) const
  { return condition->check (daisy, scope, msg); }

  ConditionNot (const BlockModel& al)
    : Condition (al),
      condition (Librarian::build_item<Condition> (al, "operand"))
  { }

  ~ConditionNot ()
  { }
};

struct ConditionIf : public Condition
{
  std::unique_ptr<Condition> if_c;
  std::unique_ptr<Condition> then_c;
  std::unique_ptr<Condition> else_c;

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

  void initialize (const Daisy& daisy, const Scope& scope, Treelog& msg)
  {
    if_c->initialize (daisy, scope, msg);
    then_c->initialize (daisy, scope, msg);
    else_c->initialize (daisy, scope, msg);
  }

  bool check (const Daisy& daisy, const Scope& scope, Treelog& msg) const
  { 
    bool ok = true;

    if (!if_c->check (daisy, scope, msg))
      ok = false;
    if (!then_c->check (daisy, scope, msg))
      ok = false;
    if (!else_c->check (daisy, scope, msg))
      ok = false;

    return ok;
  }

  ConditionIf (const BlockModel& al)
    : Condition (al),
      if_c (Librarian::build_item<Condition> (al, "if")),
      then_c (Librarian::build_item<Condition> (al, "then")),
      else_c (Librarian::build_item<Condition> (al, "else"))
  { }

  ~ConditionIf ()
  { }
};

static struct ConditionFalseSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ConditionFalse (al); }
  ConditionFalseSyntax ()
    : DeclareModel (Condition::component, "false", "Always false.")
  { }
  void load_frame (Frame&) const
  { }
} ConditionFalse_syntax;

static struct ConditionTrueSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ConditionTrue (al); }
  ConditionTrueSyntax ()
    : DeclareModel (Condition::component, "true", "Always true.")
  { }
  void load_frame (Frame&) const
  { }
} ConditionTrue_syntax;

static struct ConditionOrSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ConditionOr (al); }
  ConditionOrSyntax ()
    : DeclareModel (Condition::component, "or", "\
True iff any of the listed conditions are true.\n\
The conditions are tested in the sequence listed, until a true is found,\n\
or the end of the list is reached.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare_object ("operands", Condition::component, 
                       Attribute::State, Attribute::Variable, "Conditions to test.");
    frame.order ("operands");
  }
} ConditionOr_syntax;

static struct ConditionAndSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ConditionAnd (al); }
  ConditionAndSyntax ()
    : DeclareModel (Condition::component, "and", "\
True iff all the listed conditions are true.\n\
The conditions are tested in the sequence listed, until a false is found,\n\
or the end of the list is reached.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare_object ("operands", Condition::component, 
                       Attribute::State, Attribute::Variable, "Conditions to test.");
    frame.order ("operands");
  }
} ConditionAnd_syntax;

static struct ConditionNotSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ConditionNot (al); }
  ConditionNotSyntax ()
    : DeclareModel (Condition::component, "not", "\
True iff the operand is not true.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare_object ("operand", Condition::component, 
                       "Condition to test.");
    frame.order ("operand");
  }
} ConditionNot_syntax;

static struct ConditionIfSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ConditionIf (al); }
  ConditionIfSyntax ()
    : DeclareModel (Condition::component, "if", "\
If the first condition is true, return the value of the second condition,\n\
else return the value of the third condition.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare_object ("if", Condition::component, 
                       "Condition to test for.");
    frame.declare_object ("then", Condition::component, 
                       "Condition to use of the 'if' test was true.");
    frame.declare_object ("else", Condition::component, 
                       "Condition to use if the 'if' test was false.");
    frame.order ("if", "then", "else");
  }
} ConditionIf_syntax;

// condition_logic.C ends here.
