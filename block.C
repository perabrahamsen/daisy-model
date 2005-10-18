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

  // Use.
  Syntax::type lookup (const std::string&) const;
  const Syntax& syntax (const std::string& key) const;
  const AttributeList& alist (const std::string& key) const;
  const std::string expand_string (const std::string& value) const;

  Implementation (const Block *const p, 
		  const Syntax& s, const AttributeList& a)
    : parent (p),
      syntax (s),
      alist (a)
  { }
};

Syntax::type 
Block::Implementation::lookup (const std::string& key) const
{
  Syntax::type type = syntax.lookup (key);
  if (type == Syntax::Error && parent)
    return parent->lookup (key);
  return type;
}

const Syntax& 
Block::Implementation::syntax (const std::string& key) const
{
  Syntax::type type = syntax.lookup (key);
  if (type != Syntax::Error)
    return syntax;
  daisy_assert (parent != NULL);
  return parent->syntax (key);
}

const AttributeList& 
Block::Implementation::alist (const std::string& key) const
{
  Syntax::type type = syntax.lookup (key);
  if (type != Syntax::Error)
    return alist;
  daisy_assert (parent != NULL);
  return parent->alist (key);
}

const std::string 
Block::Implementation::expand_string (const std::string& value) const
{
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

const AttributeList&
Block::alist () const
{ return impl->alist; }

const Syntax&
Block::syntax () const
{ return impl->syntax; }

bool 
Block::check (const std::string& key) const
{ return impl->alist.check (key); }

double 
Block::number (const std::string& key) const
{ return impl->alist.number (key); }

double 
Block::number (const std::string& key, double default_value) const
{ return impl->alist.number (key, default_value); }

const std::string
Block::name (const std::string& key) const
{ return impl->expand_string (impl->alist.name (key)); }

const std::string 
Block::name (const std::string& key, const std::string& default_value) const
{
  if (impl->alist ().check (key))
    return name (key);

  return default_value;
}

symbol 
Block::identifier (const std::string& key) const
{ return symbol (name (key)); }

bool 
Block::flag (const std::string& key) const
{ return impl->alist.flag (key); }

bool 
Block::flag (const std::string& key, bool default_value) const
{ return impl->alist.flag (key, default_value); }

const PLF& 
Block::plf (const std::string& key) const
{ return impl->alist.plf (key); }

AttributeList& 
Block::alist (const std::string& key) const
{ return impl->alist.alist (key); }

int 
Block::integer (const std::string& key) const
{ return impl->alist.integer (key); }

int 
Block::integer (const std::string& key, int default_value) const
{ return impl->alist.integer (key, default_value); }

const std::vector<double>& 
Block::number_sequence (const std::string& key) const
{ return impl->alist.number_sequence (key); }

const std::vector<symbol>
Block::identifier_sequence (const std::string& key) const
{
  const std::vector<std::string>& value = impl->alist ().name_sequence (key);
  std::vector<symbol> result;
  for (size_t i = 0; i < value.size (); i++)
    result.push_back (symbol (impl->expand_string (value[i])));
  return result;
}
  
const std::vector<std::string>
Block::name_sequence (const std::string& key) const
{
  const std::vector<std::string>& value = impl->alist ().name_sequence (key);
  std::vector<std::string> result;
  for (size_t i = 0; i < value.size (); i++)
    result.push_back (impl->expand_string (value[i]));
  return result;
}

const std::vector<bool>& 
Block::flag_sequence (const std::string& key) const
{ return impl->alist.flag_sequence (key); }

const std::vector<int>& 
Block::integer_sequence (const std::string& key) const
{ return impl->alist.integer_sequence (key); }

const std::vector<const PLF*>& 
Block::plf_sequence (const std::string& key) const
{ return impl->alist.plf_sequence (key); }

const std::vector<AttributeList*>& 
Block::alist_sequence (const std::string& key) const
{ return impl->alist.alist_sequence (key); }

Block::Block (const Block& block,
	      const Syntax& syntax, const AttributeList& alist)
  : impl (new Implementation (&block, syntax, alist))
{ }

Block::Block (const Syntax& syntax, const AttributeList& alist)
  : impl (new Implementation (NULL, syntax, alist))
{ }

Block::~Block ()
{ }
