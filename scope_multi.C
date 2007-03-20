// scope_multi.C --- A scope combining multiple scopes.
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

#include "scope_multi.h"
#include "syntax.h"
#include "alist.h"
#include "assertion.h"

const std::vector<symbol>& 
ScopeMulti::all_numbers () const
{ return all_numbers_; }

bool 
ScopeMulti::has_number (const symbol tag) const
{
  for (size_t i = 0; i < scopes.size (); i++)
    if (scopes[i]->has_number (tag))
      return true;
  
  return false;
}

double
ScopeMulti::number (const symbol tag) const
{
  for (size_t i = 0; i < scopes.size (); i++)
    if (scopes[i]->has_number (tag))
      return scopes[i]->number (tag);
  
  daisy_panic ("'" + tag + "' not found in any scope");
}    

symbol 
ScopeMulti::dimension (const symbol tag) const
{
  for (size_t i = 0; i < scopes.size (); i++)
    if (scopes[i]->has_number (tag))
      return scopes[i]->dimension (tag);
  
  daisy_panic ("'" + tag + "' not found in any scope");
}

symbol
ScopeMulti::get_description (symbol tag) const
{
  for (size_t i = 0; i < scopes.size (); i++)
    if (scopes[i]->has_number (tag))
      return scopes[i]->get_description (tag);
  
  daisy_panic ("'" + tag + "' not found in any scope");
}

std::vector<symbol>
ScopeMulti::find_numbers (const std::vector<const Scope*>& scopes)
{
  std::vector<symbol> result;
  for (size_t i = 0; i < scopes.size (); i++)
    {
      const std::vector<symbol>& child = scopes[i]->all_numbers ();
      result.insert (result.end (), child.begin (), child.end ());
    }
  return result;
}

std::vector<const Scope*>
ScopeMulti::vectorize (const Scope* first, const Scope* second)
{
  std::vector<const Scope*> result;
  result.push_back (first);
  result.push_back (second);
  return result;
}

ScopeMulti::ScopeMulti (const Scope& first, const Scope& second)
  : Scope ("multi"),
    scopes (vectorize (&first, &second)),
    all_numbers_ (find_numbers (scopes))
{ }

ScopeMulti::ScopeMulti (Block& al)
  : Scope (al),
    scopes (Librarian<Scope>::build_vector_const (al, "scope")),
    all_numbers_ (find_numbers (scopes))
{ }

ScopeMulti::~ScopeMulti ()
{ }

static struct ScopeMultiSyntax
{
  static Model& make (Block& al)
  { return *new ScopeMulti (al); }

  ScopeMultiSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", 
               "A scope combining other scopes.");
    syntax.add ("scope", Librarian<Scope>::library (),
                Syntax::Const, Syntax::Sequence, 
                "List of scopes to combine, first one takes precedence.");
    Librarian<Scope>::add_type ("multi", alist, syntax, &make);
  }
} ScopeMulti_syntax;

// scope_multi.C ends here.
