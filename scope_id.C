// scope_id.C -- A name -> value map.
// 
// Copyright 2004 Per Abrahamsen and KVL.
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

#define BUILD_DLL

#include "scope_id.h"
#include "assertion.h"
#include "mathlib.h"
#if 0
#include "block.h"
#include "alist.h"
#include "librarian.h"
#endif

void 
ScopeID::entries (std::vector<symbol>& all) const
{ all.push_back (tag); }

Value::type 
ScopeID::lookup (const symbol name) const
{ return (name == tag) ? Value::Number : Value::Error; }

bool 
ScopeID::check (const symbol name) const
{ return name == tag && std::isfinite (value); }

double 
ScopeID::number (symbol) const
{ return value; }

symbol 
ScopeID::dimension (symbol) const
{ return dim;}

symbol
ScopeID::description (symbol tag) const
{ return symbol ("Use '" + tag.name () + "' as a free variable"); }

  // WScope interface.
void
ScopeID::add (symbol, double val)
{ value = val; }

void
ScopeID::set_dimension (symbol, symbol d)
{ dim = d; }

ScopeID::ScopeID (const symbol name, const symbol d)
  : tag (name),
    value (NOT_A_NUMBER),
    dim (d)
{ }

#if 0
ScopeID::ScopeID (Block& al)
  : WScope (al),
    tag (al.name ("name")), 
    value (al.number ("value")),
    dim (al.name ("value"))
{ }
#endif

ScopeID::~ScopeID ()
{ }

#if 0
static struct ScopeIDSyntax
{
  static Model& make (Block& al)
  { return *new ScopeID (al); }

  ScopeIDSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", 
               "A scope containing just a single number.");
    syntax.add ("name", Value::String, Value::Const, 
                "Identifier name.");
    syntax.add ("value", Value::User (), Value::Const, 
                "Initial value and dimension.");
    Librarian::add_type (Scope::component, "id", alist, syntax, &make);
  }
} ScopeID_syntax;
#endif

// scope_id.C ends here
