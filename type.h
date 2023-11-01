// type.h -- Type system for values.
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


#ifndef TYPE_H
#define TYPE_H

#include "attribute.h"
#include "symbol.h"
#include "frame.h"
#include <boost/noncopyable.hpp>
#include <vector>

class Check;

class Type : boost::noncopyable
{
  // Content.
private:
  const Attribute::category category_;
  const int size_;
  const symbol description_;
public:
  virtual Attribute::type type () const = 0;
  Attribute::category category () const;
  bool is_const () const;
  bool is_optional () const;
  bool is_mandatory () const;
  bool is_log () const;
  bool is_state () const;
  int size () const;
  symbol description () const;
  virtual const std::vector<symbol>& cite () const;
  virtual bool is_text () const;
  virtual symbol dimension () const;
  virtual bool verify (double value, Treelog&) const;
  virtual Frame::load_syntax_t  load_syntax () const;
  virtual symbol domain () const;
  virtual symbol range () const;
  virtual symbol component () const;

  // Create and Destroy.
protected:
  Type (const Attribute::category c, const int s, const symbol d);
public:
  virtual ~Type ();
};

class TypeNumber : public Type
{
  const symbol dimension_;
  const Check& check;
  Attribute::type type () const;
  symbol dimension () const;
  bool verify (double value, Treelog&) const;
public:
  TypeNumber (const Attribute::category c, const int s, const symbol dim, 
              const Check&, const symbol desc);
};

class TypeNumberCite : public TypeNumber
{
  const std::vector<symbol> citations;
  const std::vector<symbol>& cite () const;
public:
  TypeNumberCite (const Attribute::category c, const int s, const symbol dim, 
                  const Check&, const symbol desc, 
                  const std::vector<symbol>& cites);
};

class TypeSubmodel : public Type
{
  const Frame::load_syntax_t load_syntax_;
  Attribute::type type () const;
  Frame::load_syntax_t  load_syntax () const;
public:
  TypeSubmodel (const Attribute::category c, const int s, const Frame::load_syntax_t l,
             const symbol desc);
};

class TypePLF : public Type
{
  const symbol domain_;
  const symbol range_;
  const Check& check;
  Attribute::type type () const;
  symbol domain () const;
  symbol range () const;
  symbol dimension () const;
  bool verify (double value, Treelog&) const;
public:
  TypePLF (const Attribute::category c, const int s, const symbol dom,
           const symbol r, const Check&, const symbol desc);
};

class TypeBoolean : public Type
{
  Attribute::type type () const;
public:
  TypeBoolean (const Attribute::category c, const int s, const symbol desc);
};

class TypeString : public Type
{
  Attribute::type type () const;
public:
  TypeString (const Attribute::category c, const int s, const symbol desc);
};

class TypeText : public TypeString
{
  bool is_text () const;
public:
  TypeText (const Attribute::category c, const int s, const symbol desc);
};

class TypeInteger : public Type
{
  Attribute::type type () const;
public:
  TypeInteger (const Attribute::category c, const int s, const symbol desc);
};

class TypeModel : public Type
{
  const symbol component_;
  Attribute::type type () const;
  symbol component () const;
public:
  TypeModel (const Attribute::category c, const int s, const symbol comp,
              const symbol desc);
};

class TypeFunction : public Type
{
  const symbol domain_;
  const symbol range_;
  Attribute::type type () const;
  symbol component () const;
  symbol domain () const;
  symbol range () const;
  symbol dimension () const;
public:
  TypeFunction (const symbol dom, const symbol r, const symbol desc);
};

#endif // TYPE_H
