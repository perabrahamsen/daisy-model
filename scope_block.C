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

#define BUILD_DLL

#include "scope_block.h"
#include "library.h"
#include "number.h"
#include "stringer.h"
#include "alist.h"
#include "assertion.h"
#include "librarian.h"

void 
ScopeBlock::entries (std::vector<symbol>& all) const
{ block.entries (all); }

Value::type 
ScopeBlock::lookup (const symbol tag) const
{ return block.lookup (tag); }

symbol
ScopeBlock::dimension (symbol tag) const
{ 
  Value::type type = block.lookup (tag);
  if (type == Value::Error)
    return Value::Unknown ();
  const Syntax& syntax = block.find_syntax (tag);
  if (syntax.size (tag) != Value::Singleton)
    return Value::Unknown ();
  const AttributeList& alist = block.find_alist (tag);

  //Handle primitive numbers.
  if (type == Value::Number)
    {
      const symbol dim = syntax.dimension (tag); 
      if (dim != Value::User ())
        return symbol (dim);
      if (!alist.check (tag))
        return Value::Unknown ();
      return alist.name (tag);
    }

  // Handle number objects.
  if (type != Value::Object)
    return Value::Unknown ();
  if (syntax.library (block.metalib (), tag).name () 
      != symbol (Number::component))
    return Value::Unknown ();
  if (!syntax.check (block.metalib (), alist, block.msg ()))
    return Value::Unknown ();
    
  std::auto_ptr<Number> number (Librarian::build_alist<Number>
                                (block, alist.alist (tag), tag));
  if (!number.get ())
    return Value::Unknown ();
  if (!number->initialize (block.units (), *this, block.msg ()))
    return Value::Unknown ();
  
  return number->dimension (*this);
}

symbol 
ScopeBlock::description (symbol tag) const
{ 
  static const symbol no_symbol ("No such symbol");
  
  Value::type type = block.lookup (tag);
  if (type == Value::Error)
    return no_symbol;

  const Syntax& syntax = block.find_syntax (tag);
  return symbol (syntax.description (tag));
}

int
ScopeBlock::type_size (const symbol tag) const
{ return block.type_size (tag); }

bool 
ScopeBlock::check (const symbol tag) const
{ return block.check (tag); }

int 
ScopeBlock::value_size (const symbol tag) const
{ return block.value_size (tag); }

bool 
ScopeBlock::has_number (const symbol tag) const
{
  Value::type type = block.lookup (tag);
  if (type == Value::Error)
    return false;

  const Syntax& syntax = block.find_syntax (tag);
  if (syntax.size (tag) != Value::Singleton)
    return false;

  const AttributeList& alist = block.find_alist (tag);
  if (!alist.check (tag))
    return false;

  //Handle primitive numbers.
  if (type == Value::Number)
    return true;
  
  // Handle number objects.
  if (type != Value::Object)
    return false;
  const Library& library = syntax.library (block.metalib (), tag);
  if (library.name () != symbol (Number::component))
    return false;
  if (!syntax.check (block.metalib (), alist, block.msg ()))
    return false;
  std::auto_ptr<Number> number (Librarian::build_alist<Number>
                                (block, alist.alist (tag), tag));
  if (!number.get ())
    return false;
  if (!number->initialize (block.units (), *this, block.msg ()))
    return false;
  if (number->missing (*this))
    return false;

  return true;
}

double 
ScopeBlock::number (const symbol tag) const
{ 
  Value::type type = block.lookup (tag);
  daisy_assert (type != Value::Error);
  const Syntax& syntax = block.find_syntax (tag);

  daisy_assert (syntax.size (tag) == Value::Singleton);
  const AttributeList& alist = block.find_alist (tag);
  daisy_assert (alist.check (tag));

  //Handle primitive numbers.
  if (type == Value::Number)
    return alist.number (tag);

  // Handle number objects.
  daisy_assert (type == Value::Object);
  daisy_assert (syntax.library (block.metalib (), tag).name ()
                == symbol (Number::component));
  daisy_assert (syntax.check (block.metalib (), alist, block.msg ()));
  std::auto_ptr<Number> number (Librarian::build_alist<Number> 
                                (block, alist.alist (tag), tag));
  daisy_assert (number.get ());
  daisy_assert (number->initialize (block.units (), *this, block.msg ()));
  daisy_assert (number->check (block.units (), *this, block.msg ()));
  number->tick (block.units (), *this, block.msg ());
  daisy_assert (!number->missing (*this));
  return number->value (*this);
}

bool 
ScopeBlock::has_name (const symbol tag) const
{
  Value::type type = block.lookup (tag);
  if (type == Value::Error)
    return false;

  const Syntax& syntax = block.find_syntax (tag);
  if (syntax.size (tag) != Value::Singleton)
    return false;

  const AttributeList& alist = block.find_alist (tag);
  if (!alist.check (tag))
    return false;

  //Handle primitive names.
  if (type == Value::String)
    return true;
  
  // Handle stringer objects.
  if (type != Value::Object)
    return false;
  const Library& library = syntax.library (block.metalib (), tag);
  if (library.name () != symbol (Stringer::component))
    return false;
  if (!syntax.check (block.metalib (), alist, block.msg ()))
    return false;
  std::auto_ptr<Stringer> stringer (Librarian::build_alist<Stringer>
                                    (block, alist.alist (tag), tag));
  if (!stringer.get ())
    return false;
  if (!stringer->initialize (block.units (), *this, block.msg ()))
    return false;
  if (stringer->missing (*this))
    return false;

  return true;
}

symbol
ScopeBlock::name (const symbol tag) const
{ 
  Value::type type = block.lookup (tag);
  daisy_assert (type != Value::Error);
  const Syntax& syntax = block.find_syntax (tag);

  daisy_assert (syntax.size (tag) == Value::Singleton);
  const AttributeList& alist = block.find_alist (tag);
  daisy_assert (alist.check (tag));

  //Handle primitive names.
  if (type == Value::String)
    return alist.name (tag);

  // Handle number objects.
  daisy_assert (type == Value::Object);
  daisy_assert (syntax.library (block.metalib (), tag).name ()
                == symbol (Stringer::component));
  daisy_assert (syntax.check (block.metalib (), alist, block.msg ()));
  std::auto_ptr<Stringer> stringer (Librarian::build_alist<Stringer> 
                                (block, alist.alist (tag), tag));
  daisy_assert (stringer.get ());
  daisy_assert (stringer->initialize (block.units (), *this, block.msg ()));
  daisy_assert (stringer->check (block.units (), *this, block.msg ()));
  stringer->tick (block.units (), *this, block.msg ());
  daisy_assert (!stringer->missing (*this));
  return symbol (stringer->value (*this));
}

ScopeBlock::ScopeBlock (Block& b)
  : block (b)
{ }

ScopeBlock::~ScopeBlock ()
{ }

// scope_block.C ends here
