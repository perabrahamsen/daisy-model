// scope_table.C --- A scope based on a table.
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

#include "scope_table.h"
#include "lexer_table.h"
#include "assertion.h"

void 
ScopeTable::tick (const Scope&, Treelog&)
{ }

bool 
ScopeTable::has_number (const symbol tag) const
{
  const int tag_c = lex.find_tag (tag);
  
  if (tag_c < 0)
    return false;
  if (values.size () == 0)
    // Kludge: Uninitialized, simply checking if tags are there...
    return true;

  return !lex.is_missing (values[tag_c]);
}

double
ScopeTable::number (const symbol tag) const
{
  const int tag_c = lex.find_tag (tag);
  daisy_assert (tag_c >= 0);
  daisy_assert (values.size () > tag_c);
  return lex.convert_to_double (values[tag_c]);
}    

symbol 
ScopeTable::dimension (const symbol tag) const
{
  const int tag_c = lex.find_tag (tag);
  daisy_assert (tag_c >= 0);
  return symbol (lex.dimension (tag_c));
}

void 
ScopeTable::set (const std::vector<std::string>& entries)
{ values = entries; }

ScopeTable::ScopeTable (const LexerTable& l)
  : lex (l),
    all_numbers_ (l.get_tag_names ())
{ }

ScopeTable::~ScopeTable ()
{ }
