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


#include "scope_id.h"
#include "block.h"
#include "alist.h"
#include "assertion.h"

const std::vector<symbol>& 
ScopeID::all_numbers () const
{ return all_numbers_; }

bool 
ScopeID::has_number (symbol name) const
{ return name == tag; }

double 
ScopeID::number (symbol) const
{ return value; }

symbol 
ScopeID::dimension (symbol) const
{ return dim;}

symbol
ScopeID::get_description (symbol tag) const
{ return symbol ("Use '" + tag.name () + "' as a free variable"); }

  // WScope interface.
void
ScopeID::set_number (symbol, double val)
{ value = val; }

void
ScopeID::set_dimension (symbol, symbol d)
{ dim = d; }

ScopeID::ScopeID (const symbol name, const symbol d)
  : tag (name),
    value (-42.42e42),
    dim (d)
{ all_numbers_.push_back (tag); }

ScopeID::ScopeID (Block& al)
  : tag (al.identifier ("name")), 
    value (al.number ("value")),
    dim (al.identifier ("value"))
{ all_numbers_.push_back (tag); }

ScopeID::~ScopeID ()
{ daisy_assert (all_numbers_.size () == 1); }

static struct ScopeIDSyntax
{
  static Scope&
  make (Block& al)
  { return *new ScopeID (al); }

  ScopeIDSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", 
               "A scope containing just a single number.");
    syntax.add ("name", Syntax::String, Syntax::Const, 
                "Identifier name.");
    syntax.add ("value", Syntax::User (), Syntax::Const, 
                "Initial value and dimension.");
    Librarian<Scope>::add_type ("id", alist, syntax, &make);
  }
} ScopeID_syntax;

// scope_id.C ends here
