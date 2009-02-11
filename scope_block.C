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
#include "frame.h"
#include "assertion.h"
#include "librarian.h"

void 
ScopeBlock::entries (std::set<symbol>& all) const
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
  const Frame& frame = block.find_frame (tag);
  if (frame.type_size (tag) != Value::Singleton)
    return Value::Unknown ();

  //Handle primitive numbers.
  if (type == Value::Number)
    {
      const symbol dim = frame.dimension (tag); 
      if (dim != Value::User ())
        return symbol (dim);
      if (!frame.check (tag))
        return Value::Unknown ();
      return frame.name (tag);
    }

  // Handle number objects.
  if (type != Value::Object)
    return Value::Unknown ();
  if (frame.component (tag) != Number::component)
    return Value::Unknown ();
  if (!frame.check (block))
    return Value::Unknown ();
    
  std::auto_ptr<Number> number (Librarian::build_frame<Number>
                                (block, frame.model (tag), tag));
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

  const Frame& frame = block.find_frame (tag);
  return symbol (frame.description (tag));
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

  const Frame& frame = block.find_frame (tag);
  if (frame.type_size (tag) != Value::Singleton)
    return false;
  if (!frame.check (tag))
    return false;

  //Handle primitive numbers.
  if (type == Value::Number)
    return true;
  
  // Handle number objects.
  if (type != Value::Object)
    return false;
  if (frame.component (tag) != Number::component)
    return false;
  if (!frame.check (block))
    return false;
  std::auto_ptr<Number> number (Librarian::build_frame<Number>
                                (block, frame.model (tag), tag));
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
  const Frame& frame = block.find_frame (tag);
  daisy_assert (frame.type_size (tag) == Value::Singleton);
  daisy_assert (frame.check (tag));

  //Handle primitive numbers.
  if (type == Value::Number)
    return frame.number (tag);

  // Handle number objects.
  daisy_assert (type == Value::Object);
  daisy_assert (frame.component (tag) == Number::component);
  daisy_assert (frame.check (block));
  std::auto_ptr<Number> number (Librarian::build_frame<Number> 
                                (block, frame.model (tag), tag));
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

  const Frame& frame = block.find_frame (tag);
  if (frame.type_size (tag) != Value::Singleton)
    return false;
  if (!frame.check (tag))
    return false;

  //Handle primitive names.
  if (type == Value::String)
    return true;
  
  // Handle stringer objects.
  if (type != Value::Object)
    return false;
  if (frame.component (tag) != Stringer::component)
    return false;
  if (!frame.check (block))
    return false;
  std::auto_ptr<Stringer> stringer (Librarian::build_frame<Stringer>
                                    (block, frame.model (tag), tag));
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
  const Frame& frame = block.find_frame (tag);
  daisy_assert (frame.type_size (tag) == Value::Singleton);
  daisy_assert (frame.check (tag));

  //Handle primitive names.
  if (type == Value::String)
    return frame.name (tag);

  // Handle number objects.
  daisy_assert (type == Value::Object);
  daisy_assert (frame.component (tag) == Stringer::component);
  daisy_assert (frame.check (block));
  std::auto_ptr<Stringer> stringer (Librarian::build_frame<Stringer> 
                                (block, frame.model (tag), tag));
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
