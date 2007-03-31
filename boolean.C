// boolean.C --- Booleans in Daisy.
// 
// Copyright 2005 Per Abrahamsen and KVL.
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


#include "boolean.h"
#include "block.h"
#include "syntax.h"
#include "alist.h"
#include "assertion.h"
#include "memutils.h"
#include "librarian.h"
#include <sstream>
#include <vector>

const char *const Boolean::component = "boolean";

const std::string& 
Boolean::title () const
{ return name.name (); }

Boolean::Boolean (Block& al)
  : name (al.identifier ("type"))
{ }

Boolean::~Boolean ()
{ }

struct BooleanTrue : public Boolean
{
  // Simulation.
  void tick (const Scope&, Treelog&)
  { }
  bool missing (const Scope&) const
  { return false; }
  bool value (const Scope&) const
  { return true; }

  // Create.
  bool initialize (Treelog&)
  { return true; }
  bool check (const Scope&, Treelog&) const
  { return true; }
  BooleanTrue (Block& al)
    : Boolean (al)
  { }
};

static struct BooleanTrueSyntax
{
  static Model& make (Block& al)
  { return *new BooleanTrue (al); }
  BooleanTrueSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", 
	       "Always true.");
    Librarian::add_type (Boolean::component, "true", alist, syntax, &make);
  }
} BooleanTrue_syntax;


struct BooleanFalse : public Boolean
{
  // Simulation.
  void tick (const Scope&, Treelog&)
  { }
  bool missing (const Scope&) const
  { return false; }
  bool value (const Scope&) const
  { return false; }

  // Create.
  bool initialize (Treelog&)
  { return true; }
  bool check (const Scope&, Treelog&) const
  { return true; }
  BooleanFalse (Block& al)
    : Boolean (al)
  { }
};

static struct BooleanFalseSyntax
{
  static Model& make (Block& al)
  { return *new BooleanFalse (al); }
  BooleanFalseSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", 
	       "Always false.");
    Librarian::add_type (Boolean::component, "false", alist, syntax, &make);
  }
} BooleanFalse_syntax;

struct BooleanOperands : public Boolean
{
  const std::vector<Boolean*> operand;

  // Simulation.
  void tick (const Scope& scope, Treelog& msg)
  { 
    for (size_t i = 0; i < operand.size (); i++)
      operand[i]->tick (scope, msg);
  }
  bool missing (const Scope& scope) const
  { 
    for (size_t i = 0; i < operand.size (); i++)
      if (operand[i]->missing (scope))
        return true;
    
    return false;
  }

  // Create.
  bool initialize (Treelog& msg)
  { 
    bool ok = true;

    for (size_t i = 0; i < operand.size (); i++)
      if (!operand[i]->initialize (msg))
        {
          std::ostringstream tmp;
          tmp << name << "[" << i << "]";
          Treelog::Open nest (msg, tmp.str ());
          ok = false;
        }
    return ok;
  }
  bool check (const Scope& scope, Treelog& msg) const
  { 
    Treelog::Open nest (msg, name);
    bool ok = true;

    for (size_t i = 0; i < operand.size (); i++)
      if (!operand[i]->check (scope, msg))
        ok = false;

    return ok;
  }
  static void load_syntax (Syntax& syntax, AttributeList&)
  {
    syntax.add_object ("operands", Boolean::component, 
                       Syntax::Const, Syntax::Sequence, "\
List of operands to compare.");
    syntax.order ("operands");
  }
  BooleanOperands (Block& al)
    : Boolean (al),
      operand (Librarian::build_vector<Boolean> (al, "operands"))
  { }
  ~BooleanOperands ()
  { sequence_delete (operand.begin (), operand.end ()); }
};

struct BooleanAnd : public BooleanOperands 
{
  bool value (const Scope& scope) const
  { 
    for (size_t i = 0; i < operand.size (); i++)
      if (!operand[i]->value (scope))
        return false;
    return true;
  }
  BooleanAnd (Block& al)
    : BooleanOperands (al)
  { }
};

static struct BooleanAndSyntax
{
  static Model& make (Block& al)
  { return *new BooleanAnd (al); }
  BooleanAndSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    BooleanOperands::load_syntax (syntax, alist);
    alist.add ("description", 
	       "True if and only if all operands are true.");
    Librarian::add_type (Boolean::component, "and", alist, syntax, &make);
  }
} BooleanAnd_syntax;


struct BooleanOr : public BooleanOperands 
{
  bool value (const Scope& scope) const
  { 
    for (size_t i = 0; i < operand.size (); i++)
      if (operand[i]->value (scope))
        return true;
    return false;
  }
  BooleanOr (Block& al)
    : BooleanOperands (al)
  { }
};

static struct BooleanOrSyntax
{
  static Model& make (Block& al)
  { return *new BooleanOr (al); }
  BooleanOrSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    BooleanOperands::load_syntax (syntax, alist);
    alist.add ("description", 
	       "True if and only if any operand is true.");
    Librarian::add_type (Boolean::component, "or", alist, syntax, &make);
  }
} BooleanOr_syntax;

struct BooleanXOr : public BooleanOperands 
{
  bool value (const Scope& scope) const
  { 
    daisy_assert (operand.size () == 2);
    return operand[0]->value (scope) != operand[1]->value (scope);
  }
  BooleanXOr (Block& al)
    : BooleanOperands (al)
  { }
};

static struct BooleanXOrSyntax
{
  static Model& make (Block& al)
  { return *new BooleanXOr (al); }
  BooleanXOrSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    syntax.add_object ("operands", Boolean::component, 
                       Syntax::Const, 2, "\
The two operands to compare.");
    syntax.order ("operands");
    alist.add ("description", 
	       "True if and only if one operand is true, and one false.");
    Librarian::add_type (Boolean::component, "xor", alist, syntax, &make);
  }
} BooleanXOr_syntax;

struct BooleanNot : public BooleanOperands 
{
  bool value (const Scope& scope) const
  { 
    daisy_assert (operand.size () == 1);
    return !operand[0]->value (scope);
  }
  BooleanNot (Block& al)
    : BooleanOperands (al)
  { }
};

static struct BooleanNotSyntax
{
  static Model& make (Block& al)
  { return *new BooleanNot (al); }
  BooleanNotSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    syntax.add_object ("operands", Boolean::component, 
                       Syntax::Const, 1, "\
The operand to check.");
    syntax.order ("operands");
    alist.add ("description", 
	       "True if and only if the operand is not true.");
    Librarian::add_type (Boolean::component, "not", alist, syntax, &make);
  }
} BooleanNot_syntax;

static Librarian Boolean_init (Boolean::component, "\
Generic representation of booleans.");
