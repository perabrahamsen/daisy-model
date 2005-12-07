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
#include "librarian.h"
#include "stringer.h"
#include "number.h"
#include "assertion.h"
#include "scope.h"
#include <sstream>

class ScopeBlock : public Scope
{
  // Content.
private:
  Block& block;
  
  // Interface.
public:
  bool has_number (const std::string& tag) const;
  double number (const std::string& tag) const;
  const std::string& dimension (const std::string& tag) const;

  // Create and Destroy.
private:
  ScopeBlock (const ScopeBlock&);
  ScopeBlock ();
public:
  explicit ScopeBlock (Block& b)
    : block (b)
  { }
  ~ScopeBlock ()
  { }
};

bool 
ScopeBlock::has_number (const std::string& tag) const
{
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
ScopeBlock::number (const std::string& tag) const
{ 
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
  daisy_assert (!number->missing (*this));
  return number->value (*this);
}

const std::string&
ScopeBlock::dimension (const std::string& tag) const
{ 
  Syntax::type type = block.lookup (tag);
  if (type == Syntax::Error)
    return Syntax::Unknown ();
  const Syntax& syntax = block.find_syntax (tag);
  if (syntax.size (tag) != Syntax::Singleton)
    return Syntax::Unknown ();
  const AttributeList& alist = block.find_alist (tag);

  //Handle primitive numbers.
  if (type == Syntax::Number)
    {
      const std::string& dim = syntax.dimension (tag); 
      if (dim != Syntax::User ())
        return dim;
      if (!alist.check (tag))
        return Syntax::Unknown ();
      return alist.name (tag);
    }

  // Handle number objects.
  if (type != Syntax::Object)
    return Syntax::Unknown ();
  if (&syntax.library (tag) != &Librarian<Number>::library ())
    return Syntax::Unknown ();
  if (!syntax.check (alist, block.msg ()))
    return Syntax::Unknown ();
    
  std::auto_ptr<Number> number (Librarian<Number>::build_alist
                                (block, alist.alist (tag), tag));
  if (!number.get ())
    return Syntax::Unknown ();
  if (!number->initialize (block.msg ()))
    return Syntax::Unknown ();
  
  return number->dimension (*this);
}

struct Block::Implementation
{
  Block *const parent;
  const Syntax& syntax;
  const AttributeList& alist;
  Treelog& msg;
  Treelog::Open msg_nest;
  bool is_ok;

  // Use.
  Syntax::type lookup (const std::string&) const;
  const Syntax& find_syntax (const std::string& key) const;
  const AttributeList& find_alist (const std::string& key) const;
  const std::string expand_string (Block&, const std::string&) const;

  Implementation (Block *const p, Treelog& m,
		  const Syntax& s, const AttributeList& a,
		  const std::string& scope_id)
    : parent (p),
      syntax (s),
      alist (a),
      msg (m),
      msg_nest (msg, scope_id),
      is_ok (true)
  { }
};

Syntax::type 
Block::Implementation::lookup (const std::string& key) const
{
  Syntax::type type = syntax.lookup (key);
  if (type == Syntax::Error && parent)
    return parent->impl->lookup (key);
  return type;
}

const Syntax& 
Block::Implementation::find_syntax (const std::string& key) const
{
  Syntax::type type = syntax.lookup (key);
  if (type != Syntax::Error)
    return syntax;
  daisy_assert (parent != NULL);
  return parent->impl->find_syntax (key);
}

const AttributeList& 
Block::Implementation::find_alist (const std::string& key) const
{
  Syntax::type type = syntax.lookup (key);
  if (type != Syntax::Error)
    return alist;
  daisy_assert (parent != NULL);
  return parent->impl->find_alist (key);
}

const std::string 
Block::Implementation::expand_string (Block& block,
				      const std::string& value) const
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
	  if (c == '$')
	    mode = escaped;
	  else
	    result << c;
	  break;
	case escaped:
	  if (c == '{')
	    mode = keyed;
	  else if (c == '$')
	    {
	      result << '$';
	      mode = normal;
	    }
	  else
	    {
#if 0
	      // BUG: We still have too many $col and $crop around to warn.
	      msg.warning (std::string ("Unknown $ escape '") 
			   + c + "', ignored");
#endif
	      result << '$' << c;
	      mode = normal;
	    }
	  break;
	case keyed:
	  if (c == '}')
            {
              try 
                {
                  const Syntax::type type = lookup (key);
                  if (type == Syntax::Error)
                    throw "Unknown expansion: '" + key + "'";
                  const Syntax& syntax = find_syntax (key);
                  if (syntax.size (key) != Syntax::Singleton)
                    throw "'" + key 
                      + "' is a sequence, can only expand singletons";
                  const AttributeList& alist = find_alist (key);
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
                    case Syntax::Object:
                      {
                        const AttributeList& obj = alist.alist (key);
                        const std::string type = obj.name ("type");
                        const Library& library = syntax.library (key);
                        const ScopeBlock scope (block);
                        if (&library == &Librarian<Stringer>::library ())
                          {
                            const std::auto_ptr<Stringer> stringer 
                              (Librarian<Stringer>::build_alist (block,
                                                                  obj, key));
                            if (!block.ok () 
                                || !stringer->initialize (msg)
                                || !stringer->check (scope, msg)
                                || stringer->missing (scope))
                              throw "Bad string: '" + type + "'";
                            result << stringer->value (scope);
                          }
                        else if (&library == &Librarian<Number>::library ())
                          {
                            const std::auto_ptr<Number> number 
                              (Librarian<Number>::build_alist (block, obj, key));
                            if (!block.ok () 
                                || !number->initialize (msg)
                                || !number->check (scope, msg)
                                || number->missing (scope))
                              throw "Bad number: '"+ type + "'";
                            result << number->value (scope);
                            const std::string dim = number->dimension (scope);
                            if (dim == Syntax::Fraction () 
                                || dim == Syntax::None ())
                              result << " []";
                            else if (dim != Syntax::Unknown ())
                              result << " [" << dim << "]";
                          }
                        else
                          throw "Unhandled object type '" + library.name ().name ()
                            + "'";
                        break;
                      }
                    default:
                      throw "'" + key + "' unhandled type";
                    }
                }
              catch (const std::string& error)
                {
                  result << "${" << key << "}";
                  msg.warning (error); 
                }
              mode = normal;
              key = "";
            }
	  else
	    key += c;
	}
    }
  if (mode != normal)
    throw "Unterminated expansion '" + key + "'";
  return result.str ();
}

Syntax::type 
Block::lookup (const std::string& key) const
{ return impl->lookup (key); }

const Syntax& 
Block::find_syntax (const std::string& key) const
{ return impl->find_syntax (key); }

const AttributeList& 
Block::find_alist (const std::string& key) const
{ return impl->find_alist (key); }

const AttributeList&
Block::alist () const
{ return impl->alist; }

const Syntax&
Block::syntax () const
{ return impl->syntax; }

Treelog&
Block::msg () const
{ return impl->msg; }

void
Block::error (const std::string& msg)
{ 
  set_error (); 
  impl->msg.error (msg); 
}

bool
Block::ok () const
{ return impl->is_ok; }

void
Block::set_error ()
{ 
  impl->is_ok = false; 
  if (impl->parent) 
    impl->parent->set_error (); 
}

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
Block::name (const std::string& key)
{ return impl->expand_string (*this, impl->alist.name (key)); }

const std::string 
Block::name (const std::string& key, const std::string& default_value)
{
  if (impl->alist.check (key))
    return name (key);

  return default_value;
}

symbol 
Block::identifier (const std::string& key)
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
Block::identifier_sequence (const std::string& key)
{
  const std::vector<std::string>& value = impl->alist.name_sequence (key);
  std::vector<symbol> result;
  for (size_t i = 0; i < value.size (); i++)
    result.push_back (symbol (impl->expand_string (*this, value[i])));
  return result;
}
  
std::vector<std::string>
Block::name_sequence (const std::string& key)
{
  const std::vector<std::string>& value = impl->alist.name_sequence (key);
  std::vector<std::string> result;
  for (size_t i = 0; i < value.size (); i++)
    result.push_back (impl->expand_string (*this, value[i]));
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

Block::Block (const Syntax& syntax, const AttributeList& alist, Treelog& msg, 
	      const std::string& scope_id)
  : impl (new Implementation (NULL, msg, syntax, alist, scope_id))
{ }

Block::Block (Block& block,
	      const Syntax& syntax, const AttributeList& alist, 
	      const std::string& scope_id)
  : impl (new Implementation (&block, block.msg (), syntax, alist,
			      scope_id))
{ }

Block::Block (Block& block, const std::string& key)
  : impl (new Implementation (&block, block.msg (), 
                              block.syntax ().syntax (key), 
                              block.alist ().alist (key),
			      key))
{ }

Block::Block (Block& block,
	      const Syntax& syntax, const AttributeList& alist, 
	      const std::string& scope_id, size_t index)
  : impl (new Implementation (&block, block.msg (),
			      syntax, alist, 
			      sequence_id (scope_id, index)))
{ }

Block::~Block ()
{ }
