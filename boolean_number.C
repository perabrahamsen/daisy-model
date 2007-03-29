// boolean_number.C --- Boolean operations on numbers.
// 
// Copyright 2006 Per Abrahamsen and KVL.
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
#include "syntax.h"
#include "alist.h"
#include "number.h"
#include "memutils.h"
#include <sstream>
#include <vector>

struct BooleanNumbers : public Boolean
{
  const std::vector<Number*> operand;

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

    symbol dim = Syntax::unknown ();
    for (size_t i = 0; i < operand.size (); i++)
      if (!operand[i]->check (scope, msg))
        ok = false;
      else 
        {
          static const symbol blank ("");
          symbol new_dim = operand[i]->dimension (scope);
          if (new_dim == Syntax::none ()
              || new_dim == Syntax::fraction ())
            new_dim = blank;
          if (new_dim != dim)
            if (dim == Syntax::unknown ())
              dim = new_dim;
            else if (new_dim != Syntax::unknown ())
              {
                msg.error ("I don't know how to compare [" + dim + "] with ["
                           + new_dim + "]");
                dim = new_dim;
                ok = false;
              }
        }
    return ok;
  }
  static void load_syntax (Syntax& syntax, AttributeList&)
  {
    syntax.add_object ("operands", Number::component, 
                       Syntax::Const, Syntax::Sequence, "\
List of operands to compare.");
    syntax.order ("operands");
  }
  BooleanNumbers (Block& al)
    : Boolean (al),
      operand (Librarian<Number>::build_vector (al, "operands"))
  { }
  ~BooleanNumbers ()
  { sequence_delete (operand.begin (), operand.end ()); }
};

struct BooleanNumGT : public BooleanNumbers
{
  bool value (const Scope& scope) const
  { 
    const size_t size = operand.size ();
    if (size < 1)
      return true;
    double prev = operand[0]->value (scope);
    for (size_t i = 1; i < size; i++)
      {
        const double next = operand[i]->value (scope);
        if (!(prev > next))
          return false;
        prev = next;
      }
    return true;
  }
  BooleanNumGT (Block& al)
    : BooleanNumbers (al)
  { }
};

static struct BooleanNumGTSyntax
{
  static Model& make (Block& al)
  { return *new BooleanNumGT (al); }
  BooleanNumGTSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    BooleanNumbers::load_syntax (syntax, alist);
    alist.add ("description", 
	       "True iff each operand is larger than the next.");
    Librarian<Boolean>::add_type (">", alist, syntax, &make);
  }
} BooleanNumGT_syntax;

struct BooleanNumGTE : public BooleanNumbers
{
  bool value (const Scope& scope) const
  { 
    const size_t size = operand.size ();
    if (size < 1)
      return true;
    double prev = operand[0]->value (scope);
    for (size_t i = 1; i < size; i++)
      {
        const double next = operand[i]->value (scope);
        if (!(prev >= next))
          return false;
        prev = next;
      }
    return true;
  }
  BooleanNumGTE (Block& al)
    : BooleanNumbers (al)
  { }
};

static struct BooleanNumGTESyntax
{
  static Model& make (Block& al)
  { return *new BooleanNumGTE (al); }
  BooleanNumGTESyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    BooleanNumbers::load_syntax (syntax, alist);
    alist.add ("description", "\
True iff each operand is at least as large as the next.");
    Librarian<Boolean>::add_type (">=", alist, syntax, &make);
  }
} BooleanNumGTE_syntax;

struct BooleanNumLT : public BooleanNumbers
{
  bool value (const Scope& scope) const
  { 
    const size_t size = operand.size ();
    if (size < 1)
      return true;
    double prev = operand[0]->value (scope);
    for (size_t i = 1; i < size; i++)
      {
        const double next = operand[i]->value (scope);
        if (!(prev < next))
          return false;
        prev = next;
      }
    return true;
  }
  BooleanNumLT (Block& al)
    : BooleanNumbers (al)
  { }
};

static struct BooleanNumLTSyntax
{
  static Model& make (Block& al)
  { return *new BooleanNumLT (al); }
  BooleanNumLTSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    BooleanNumbers::load_syntax (syntax, alist);
    alist.add ("description", 
	       "True iff each operand is smaller than the next.");
    Librarian<Boolean>::add_type ("<", alist, syntax, &make);
  }
} BooleanNumLT_syntax;

struct BooleanNumLTE : public BooleanNumbers
{
  bool value (const Scope& scope) const
  { 
    const size_t size = operand.size ();
    if (size < 1)
      return true;
    double prev = operand[0]->value (scope);
    for (size_t i = 1; i < size; i++)
      {
        const double next = operand[i]->value (scope);
        if (!(prev <= next))
          return false;
        prev = next;
      }
    return true;
  }
  BooleanNumLTE (Block& al)
    : BooleanNumbers (al)
  { }
};

static struct BooleanNumLTESyntax
{
  static Model& make (Block& al)
  { return *new BooleanNumLTE (al); }
  BooleanNumLTESyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    BooleanNumbers::load_syntax (syntax, alist);
    alist.add ("description", "\
True iff each operand is smaller than or equal to the next.");
    Librarian<Boolean>::add_type ("<=", alist, syntax, &make);
  }
} BooleanNumLTE_syntax;

// boolean_number.C ends here.
