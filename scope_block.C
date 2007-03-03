// scope_block.C -- Treat a block as a scope.
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

#include "scope_block.h"
#include "number.h"
#include "stringer.h"
#include "alist.h"

void 
ScopeBlock::tick (const Scope&, Treelog&)
{ }

const std::vector<symbol>& 
ScopeBlock::all_numbers () const
{ return Scope::null ().all_numbers (); }

bool 
ScopeBlock::has_number (const symbol tag_symbol) const
{
  const std::string& tag = tag_symbol.name ();

  Syntax::type type = block.lookup (tag);
  if (type == Syntax::Error)
    return false;

  const Syntax& syntax = block.find_syntax (tag);
  if (syntax.size (tag) != Syntax::Singleton)
    return false;

  const AttributeList& alist = block.find_alist (tag);
  if (!alist.check (tag))
    return false;

  //Handle primitive numbers.
  if (type == Syntax::Number)
    return true;
  
  // Handle number objects.
  if (type != Syntax::Object)
    return false;
  const Library& library = syntax.library (tag);
  if (&library != &Librarian<Number>::library ())
    return false;
  if (!syntax.check (alist, block.msg ()))
    return false;
  std::auto_ptr<Number> number (Librarian<Number>::build_alist
                                (block, alist.alist (tag), tag));
  if (!number.get ())
    return false;
  if (!number->initialize (block.msg ()))
    return false;
  if (number->missing (*this))
    return false;

  return true;
}

double 
ScopeBlock::number (const symbol tag_symbol) const
{ 
  const std::string& tag = tag_symbol.name ();

  Syntax::type type = block.lookup (tag);
  daisy_assert (type != Syntax::Error);
  const Syntax& syntax = block.find_syntax (tag);

  daisy_assert (syntax.size (tag) == Syntax::Singleton);
  const AttributeList& alist = block.find_alist (tag);
  daisy_assert (alist.check (tag));

  //Handle primitive numbers.
  if (type == Syntax::Number)
    return alist.number (tag);

  // Handle number objects.
  daisy_assert (type == Syntax::Object);
  daisy_assert (&syntax.library (tag) == &Librarian<Number>::library ());
  daisy_assert (syntax.check (alist, block.msg ()));
  std::auto_ptr<Number> number (Librarian<Number>::build_alist 
                                (block, alist.alist (tag), tag));
  daisy_assert (number.get ());
  daisy_assert (number->initialize (block.msg ()));
  daisy_assert (number->check (*this, block.msg ()));
  number->tick (*this, block.msg ());
  daisy_assert (!number->missing (*this));
  return number->value (*this);
}

symbol
ScopeBlock::dimension (symbol tag_symbol) const
{ 
  const std::string& tag = tag_symbol.name ();

  Syntax::type type = block.lookup (tag);
  if (type == Syntax::Error)
    return Syntax::unknown ();
  const Syntax& syntax = block.find_syntax (tag);
  if (syntax.size (tag) != Syntax::Singleton)
    return Syntax::unknown ();
  const AttributeList& alist = block.find_alist (tag);

  //Handle primitive numbers.
  if (type == Syntax::Number)
    {
      const std::string& dim = syntax.dimension (tag); 
      if (dim != Syntax::User ())
        return symbol (dim);
      if (!alist.check (tag))
        return Syntax::unknown ();
      return alist.identifier (tag);
    }

  // Handle number objects.
  if (type != Syntax::Object)
    return Syntax::unknown ();
  if (&syntax.library (tag) != &Librarian<Number>::library ())
    return Syntax::unknown ();
  if (!syntax.check (alist, block.msg ()))
    return Syntax::unknown ();
    
  std::auto_ptr<Number> number (Librarian<Number>::build_alist
                                (block, alist.alist (tag), tag));
  if (!number.get ())
    return Syntax::unknown ();
  if (!number->initialize (block.msg ()))
    return Syntax::unknown ();
  
  return number->dimension (*this);
}

bool 
ScopeBlock::has_identifier (const symbol tag_symbol) const
{
  const std::string& tag = tag_symbol.name ();

  Syntax::type type = block.lookup (tag);
  if (type == Syntax::Error)
    return false;

  const Syntax& syntax = block.find_syntax (tag);
  if (syntax.size (tag) != Syntax::Singleton)
    return false;

  const AttributeList& alist = block.find_alist (tag);
  if (!alist.check (tag))
    return false;

  //Handle primitive names.
  if (type == Syntax::String)
    return true;
  
  // Handle stringer objects.
  if (type != Syntax::Object)
    return false;
  const Library& library = syntax.library (tag);
  if (&library != &Librarian<Stringer>::library ())
    return false;
  if (!syntax.check (alist, block.msg ()))
    return false;
  std::auto_ptr<Stringer> stringer (Librarian<Stringer>::build_alist
                                    (block, alist.alist (tag), tag));
  if (!stringer.get ())
    return false;
  if (!stringer->initialize (block.msg ()))
    return false;
  if (stringer->missing (*this))
    return false;

  return true;
}

symbol
ScopeBlock::identifier (const symbol tag_symbol) const
{ 
  const std::string& tag = tag_symbol.name ();

  Syntax::type type = block.lookup (tag);
  daisy_assert (type != Syntax::Error);
  const Syntax& syntax = block.find_syntax (tag);

  daisy_assert (syntax.size (tag) == Syntax::Singleton);
  const AttributeList& alist = block.find_alist (tag);
  daisy_assert (alist.check (tag));

  //Handle primitive names.
  if (type == Syntax::String)
    return alist.identifier (tag);

  // Handle number objects.
  daisy_assert (type == Syntax::Object);
  daisy_assert (&syntax.library (tag) == &Librarian<Stringer>::library ());
  daisy_assert (syntax.check (alist, block.msg ()));
  std::auto_ptr<Stringer> stringer (Librarian<Stringer>::build_alist 
                                (block, alist.alist (tag), tag));
  daisy_assert (stringer.get ());
  daisy_assert (stringer->initialize (block.msg ()));
  daisy_assert (stringer->check (*this, block.msg ()));
  stringer->tick (*this, block.msg ());
  daisy_assert (!stringer->missing (*this));
  return symbol (stringer->value (*this));
}

symbol 
ScopeBlock::get_description (symbol) const
{ return symbol ("Descriptions not implemented yet"); }

// scope_block.C ends here
