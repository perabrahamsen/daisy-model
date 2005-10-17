// block.C -- Support for block scoped variables.
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

#include "block.h"
#include "alist.h"
#include "assertion.h"
#include <sstream>

struct Block::Implementation
{
  const Block *const parent;
  const Syntax& syntax;
  const AttributeList& alist;

  Implementation (const Block *const p, 
		  const Syntax& s, const AttributeList& a)
    : parent (p),
      syntax (s),
      alist (a)
  { }
};

const Syntax& 
Block::syntax () const
{ return impl->syntax; }

const AttributeList& 
Block::alist () const
{ return impl->alist; }

Syntax::type 
Block::lookup (const std::string& key) const
{
  Syntax::type type = impl->syntax.lookup (key);
  if (type == Syntax::Error && impl->parent)
    return impl->parent->lookup (key);
  return type;
}

const Syntax& 
Block::syntax (const std::string& key) const
{
  Syntax::type type = impl->syntax.lookup (key);
  if (type != Syntax::Error)
    return impl->syntax;
  daisy_assert (impl->parent != NULL);
  return impl->parent->syntax (key);
}

const AttributeList& 
Block::alist (const std::string& key) const
{
  Syntax::type type = impl->syntax.lookup (key);
  if (type != Syntax::Error)
    return impl->alist;
  daisy_assert (impl->parent != NULL);
  return impl->parent->alist (key);
}

const std::string 
Block::expand (const std::string& name) const
{
  daisy_assert (alist ().check (name));
  daisy_assert (syntax ().lookup (name) == Syntax::String);
  daisy_assert (syntax ().size (name) == Syntax::Singleton);
  
  const std::string& value = alist ().name (name);
  std::ostringstream result;
  enum mode_t { normal, escaped, keyed } mode = normal;
  std::string key;
  for (size_t i = 0; i < value.size (); i++)
    {
      char c = value[i];
      switch (mode)
	{
	case normal:
	  if (c == '\\')
	    mode = escaped;
	  else
	    result << c;
	  break;
	case escaped:
	  if (c == '{')
	    mode = keyed;
	  else
	    {
	      result << '\\' << c;
	      mode = normal;
	    }
	  break;
	case keyed:
	  if (c == '}')
	    {
	      const Syntax::type type = lookup (key);
	      if (type == Syntax::Error)
		throw "Unknown expansion: '" + key + "'";
	      const Syntax& syntax = this->syntax (key);
	      if (syntax.size (key) != Syntax::Singleton)
		throw "'" + key 
		  + "' is a sequence, can only expand singletons";
	      const AttributeList& alist = this->alist (key);
	      if (!alist.check (key))
		throw "'" + key + "' has no value";
	      switch (type)
		{
		case Syntax::String:
		  result << alist.name (key); 
		  break;
		case Syntax::Integer:
		  result << alist.integer (key); 
		  break;
		case Syntax::Number:
		  result << alist.number (key); 
		  break;
		default:
		  throw "'" + key + "' unhandled type";
		}
	      mode = normal;
	    }
	  else
	    key += c;
	}
    }
  if (mode != normal)
    throw "Unterminated expansion '" + key + "'";
  return result.str ();
}

Block::Block (const Block& block,
	      const Syntax& syntax, const AttributeList& alist)
  : impl (new Implementation (&block, syntax, alist))
{ }

Block::Block (const Syntax& syntax, const AttributeList& alist)
  : impl (new Implementation (NULL, syntax, alist))
{ }

Block::~Block ()
{ }
