// type.C -- Type system for values.
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
// Copyright 2008 Per Abrahamsen and KVL.
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

#include "type.h"
#include "assertion.h"

Value::category
Type::category () const 
{ return category_; }

bool 
Type::is_const () const
{ return category () == Value::Const || category () == Value::OptionalConst; }

bool 
Type::is_optional () const
{ return category () == Value::OptionalState 
    || category () == Value::OptionalConst; }

bool 
Type::is_log () const
{ return category () == Value::LogOnly; }

bool 
Type::is_state () const
{ return category () == Value::State || category () == Value::OptionalState; }

int 
Type::size () const 
{ return size_; }

symbol 
Type::description () const
{ return description_ ; }
  
symbol 
Type::dimension () const
{ daisy_notreached (); }

Frame::load_syntax_t  
Type::load_syntax () const
{ daisy_notreached (); }

symbol 
Type::domain () const
{ daisy_notreached (); }

symbol 
Type::range () const
{ daisy_notreached (); }

symbol 
Type::component () const
{ daisy_notreached (); }

Type::Type (const Value::category c, const int s, const symbol d)
  : category_ (c),
    size_ (s),
    description_ (d)
{ }

Type::~Type ()
{ }

Value::type 
TypeNumber::type () const
{ return Value::Number; }

symbol 
TypeNumber::dimension () const
{ return dimension_; }

TypeNumber::TypeNumber (const Value::category c, const int s, const symbol dim, 
                        const Check& chk,
                        const symbol desc)
  : Type (c, s, desc),
    dimension_ (dim),
    check_ (chk)
{ }

Value::type 
TypeAList::type () const
{ return Value::AList; }

Frame::load_syntax_t  
TypeAList::load_syntax () const
{ return load_syntax_; }

TypeAList::TypeAList (const Value::category c, const int s,
                      const Frame::load_syntax_t l, const symbol desc)
  : Type (c, s, desc),
    load_syntax_ (l)
{ }

Value::type 
TypePLF::type () const
{ return Value::PLF; }

symbol 
TypePLF::dimension () const
{ return domain (); }

symbol 
TypePLF::domain () const
{ return domain_; }

symbol 
TypePLF::range () const
{ return range_; }

TypePLF::TypePLF (const Value::category c, const int s, const symbol dom,
                  const symbol r, const Check& chk, const symbol desc)
  : Type (c, s, desc),
    domain_ (dom),
    range_ (r),
    check_ (chk)
{ }

Value::type 
TypeBoolean::type () const
{ return Value::Boolean; }

TypeBoolean::TypeBoolean (const Value::category c, const int s,
                          const symbol desc)
  : Type (c, s, desc)
{ }

Value::type 
TypeString::type () const
{ return Value::String; }

TypeString::TypeString (const Value::category c, const int s, const symbol desc)
  : Type (c, s, desc)
{ }

Value::type 
TypeInteger::type () const
{ return Value::Integer; }

TypeInteger::TypeInteger (const Value::category c, const int s,
                          const symbol desc)
  : Type (c, s, desc)
{ }

Value::type 
TypeObject::type () const
{ return Value::Object; }

symbol 
TypeObject::component () const
{ return component_; }

TypeObject::TypeObject (const Value::category c, const int s, const symbol comp,
                        const symbol desc)
  : Type (c, s, desc),
    component_ (c)
{ }

// type.C ends here.
