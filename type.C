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
#include "check.h"
#include "function.h"

Attribute::category
Type::category () const 
{ return category_; }

bool 
Type::is_const () const
{ return category () == Attribute::Const || category () == Attribute::OptionalConst; }

bool 
Type::is_optional () const
{ return category () == Attribute::OptionalState 
    || category () == Attribute::OptionalConst; }

bool 
Type::is_mandatory () const
{ return category () == Attribute::State || category () == Attribute::Const; }

bool 
Type::is_log () const
{ return category () == Attribute::LogOnly; }

bool 
Type::is_state () const
{ return category () == Attribute::State || category () == Attribute::OptionalState; }

int 
Type::size () const 
{ return size_; }

symbol 
Type::description () const
{ return description_ ; }
  
const std::vector<symbol>& 
Type::cite () const
{ 
  static const std::vector<symbol> empty;
  return empty;
}

bool
Type::is_text () const
{ return false; }

symbol 
Type::dimension () const
{ daisy_notreached (); }

bool
Type::verify (const double value, Treelog& msg) const
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

Type::Type (const Attribute::category c, const int s, const symbol d)
  : category_ (c),
    size_ (s),
    description_ (d)
{ }

Type::~Type ()
{ }

Attribute::type 
TypeNumber::type () const
{ return Attribute::Number; }

symbol 
TypeNumber::dimension () const
{ return dimension_; }

bool
TypeNumber::verify (const double value, Treelog& msg) const
{ return check.verify (value, msg); }

TypeNumber::TypeNumber (const Attribute::category c, const int s,
                        const symbol dim, const Check& chk,
                        const symbol desc)
  : Type (c, s, desc),
    dimension_ (dim),
    check (chk)
{ }

const std::vector<symbol>& 
TypeNumberCite::cite () const
{ return citations; }

TypeNumberCite::TypeNumberCite (const Attribute::category c, const int s,
                                const symbol dim, const Check& chk,
                                const symbol desc,
                                const std::vector<symbol>& cites)
  : TypeNumber (c, s, dim, chk, desc),
    citations (cites)
{ }

Attribute::type 
TypeSubmodel::type () const
{ return Attribute::Submodel; }

Frame::load_syntax_t  
TypeSubmodel::load_syntax () const
{ return load_syntax_; }

TypeSubmodel::TypeSubmodel (const Attribute::category c, const int s,
                      const Frame::load_syntax_t l, const symbol desc)
  : Type (c, s, desc),
    load_syntax_ (l)
{ }

Attribute::type 
TypePLF::type () const
{ return Attribute::PLF; }

symbol 
TypePLF::dimension () const
{ return domain (); }

symbol 
TypePLF::domain () const
{ return domain_; }

symbol 
TypePLF::range () const
{ return range_; }

bool
TypePLF::verify (const double value, Treelog& msg) const
{ return check.verify (value, msg); }

TypePLF::TypePLF (const Attribute::category c, const int s, const symbol dom,
                  const symbol r, const Check& chk, const symbol desc)
  : Type (c, s, desc),
    domain_ (dom),
    range_ (r),
    check (chk)
{ }

Attribute::type 
TypeBoolean::type () const
{ return Attribute::Boolean; }

TypeBoolean::TypeBoolean (const Attribute::category c, const int s,
                          const symbol desc)
  : Type (c, s, desc)
{ }

Attribute::type 
TypeString::type () const
{ return Attribute::String; }

TypeString::TypeString (const Attribute::category c, 
                        const int s, const symbol desc)
  : Type (c, s, desc)
{ }

TypeText::TypeText (const Attribute::category c, const int s, const symbol desc)
  : TypeString (c, s, desc)
{ }

bool
TypeText::is_text () const
{ return true; }


Attribute::type 
TypeInteger::type () const
{ return Attribute::Integer; }

TypeInteger::TypeInteger (const Attribute::category c, const int s,
                          const symbol desc)
  : Type (c, s, desc)
{ }

Attribute::type 
TypeModel::type () const
{ return Attribute::Model; }

symbol 
TypeModel::component () const
{ return component_; }

TypeModel::TypeModel (const Attribute::category c, const int s, const symbol comp,
                        const symbol desc)
  : Type (c, s, desc),
    component_ (comp)
{ }

Attribute::type 
TypeFunction::type () const
{ return Attribute::Function; }

symbol 
TypeFunction::component () const
{
  static const symbol name = Function::component; 
  return name;
}

symbol 
TypeFunction::dimension () const
{ return domain (); }

symbol 
TypeFunction::domain () const
{ return domain_; }

symbol 
TypeFunction::range () const
{ return range_; }

TypeFunction::TypeFunction (const symbol d, const symbol r,
			    const symbol desc)
  : Type (Attribute::Const, Attribute::Singleton, desc),
    domain_ (d),
    range_ (r)
{ }

// type.C ends here.
