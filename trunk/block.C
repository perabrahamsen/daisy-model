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

#define BUILD_DLL

#include "block.h"
#include "metalib.h"
#include "library.h"
#include "librarian.h"
#include "stringer.h"
#include "number.h"
#include "assertion.h"
#include "scope_block.h"
#include "librarian.h"
#include "treelog.h"
#include "frame_model.h"
#include <sstream>

struct Block::Implementation
{
  Metalib& metalib;
  Block *const context;
  const Frame& frame;
  mutable Treelog& msg;
  Treelog::Open msg_nest;
  bool is_ok;

  // Use.
  Value::type lookup (symbol) const;
  const Frame& find_frame (const symbol key) const;
  bool check (const symbol key) const;
  symbol expand_string (Block&, symbol);
  symbol expand_reference (const symbol key);
  void error (const std::string& msg);
  void set_error ();

  Implementation (Metalib& lib, Block *const p, Treelog& m,
                  const Frame& f,
		  const symbol scope_id)
    : metalib (lib),
      context (p),
      frame (f),
      msg (m),
      msg_nest (msg, scope_id),
      is_ok (true)
  { }
};

Value::type 
Block::Implementation::lookup (const symbol key) const
{
  Value::type type = frame.lookup (key);
  if (type == Value::Error && context)
    return context->impl->lookup (key);
  return type;
}

const Frame& 
Block::Implementation::find_frame (const symbol key) const
{
  if (frame.check (key) || frame.lookup (key) != Value::Error)
    return frame;
  if (context == NULL)
    daisy_panic ("Couldn't find '" + key + "' in this block or its context");
  return context->find_frame (key);
}

bool
Block::Implementation::check (const symbol key) const
{
  Value::type type = frame.lookup (key);
  if (type != Value::Error)
    return frame.check (key);
  if (context == NULL)
    return false;
  return context->impl->check (key);
}

symbol
Block::Implementation::expand_string (Block& block,
				      const symbol value_s)
{
  const std::string value = value_s.name ();
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
	      // BUG: We still have too many $col and $crop around to throw.
	      msg.warning (std::string ("Unknown $ escape '") 
			   + c + "', ignored");
	      result << '$' << c;
	      mode = normal;
	    }
	  break;
	case keyed:
	  if (c == '}')
            {
              try 
                {
                  const Value::type type = lookup (key);
                  if (type == Value::Error)
                    throw "Unknown expansion: '" + key + "'";
                  const Frame& frame = find_frame (key);
                  if (frame.type_size (key) != Value::Singleton)
                    throw "'" + key 
                      + "' is a sequence, can only expand singletons";
                  if (!frame.check (key))
                    throw "'" + key + "' has no value";
                  switch (type)
                    {
                    case Value::String:
                      result << frame.name (key); 
                      break;
                    case Value::Integer:
                      result << frame.integer (key); 
                      break;
                    case Value::Number:
                      result << frame.number (key); 
                      break;
                    case Value::Object:
                      {
                        Treelog::Open nest (msg, "${" + key + "}");
                        const FrameModel& obj = frame.model (key);
                        const symbol type = obj.type_name ();
                        const Library& library = frame.library (metalib, key);
                        const ScopeBlock scope (block);
                        if (library.name () == symbol (Stringer::component))
                          {
                            const std::auto_ptr<Stringer> stringer 
                              (Librarian::build_frame<Stringer> (block,
                                                                 obj, key));
                            if (!block.ok () 
                                || !stringer->initialize (block.units (),
                                                          scope, msg)
                                || !stringer->check (block.units (), scope, msg)
                                || stringer->missing (scope))
                              throw "Bad string: '" + type + "'";
                            result << stringer->value (scope);
                          }
                        else if (library.name () == symbol (Number::component))
                          {
                            const std::auto_ptr<Number> number 
                              (Librarian::build_frame<Number> (block, 
                                                               obj, key));
                            if (!block.ok () 
                                || !number->initialize (block.units (), scope, 
                                                        msg)
                                || !number->check (block.units (), scope, msg))
                              throw "Bad number: '"+ type + "'";
                            number->tick (block.units (), scope, msg);
                            if (number->missing (scope))
                              throw "Bad number: '"+ type + "'";
                            result << number->value (scope);
                            const symbol dim = number->dimension (scope);
                            if (dim == Value::Fraction ()
                                || dim == Value::None ())
                              result << " []";
                            else if (dim != Value::Unknown ())
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

symbol
Block::Implementation::expand_reference (const symbol key)
{
  if (!frame.is_reference (key))
    return key;
  
  const symbol var = frame.get_reference (key);
  if (var == key)
    {
      error ("Value of '" + key + "' refers to itself");
      throw "Reference loop";
    }
  if (lookup (var) == frame.lookup (key)
      && (find_frame (var).type_size (var) == frame.type_size (key)
          || (frame.type_size (key) == Value::Sequence
              && find_frame (var).type_size (var) != Value::Singleton)))
    return var;

  std::ostringstream tmp;
  tmp << "Value of '" << key << "' is $" << var
      << ", which is a " << Value::type_name (lookup (var));
  switch (find_frame (var).type_size (var))
    {
    case Value::Singleton:
      break;
    case Value::Sequence:
      tmp << " sequence";
      break;
    default:
      tmp << "[" << find_frame (var).type_size (var) << "]";
    }
  tmp << ", should be a " << Value::type_name (frame.lookup (key));
  switch (frame.type_size (key))
    {
    case Value::Singleton:
      break;
    case Value::Sequence:
      tmp << " sequence";
      break;
    default:
      tmp << "[" << frame.type_size (key) << "]";
    }
  error (tmp.str ());
  throw ("Bad reference");
}

void
Block::Implementation::error (const std::string& str)
{ 
  set_error (); 
  msg.error (str); 
}

void
Block::Implementation::set_error ()
{ 
  is_ok = false; 
  if (context) 
    context->set_error (); 
}

Metalib& 
Block::metalib ()
{ return impl->metalib; }

const Units& 
Block::units ()
{ return impl->metalib.units (); }

Path& 
Block::path ()
{ return impl->metalib.path (); }

const Frame&
Block::frame () const
{ return impl->frame; }

Treelog&
Block::msg () const
{ return impl->msg; }

symbol
Block::type_name () const
{ return impl->frame.type_name (); }

void
Block::error (const std::string& msg)
{ impl->error (msg); }

bool
Block::ok () const
{ return impl->is_ok; }

void
Block::set_error ()
{ impl->set_error (); }

const Frame& 
Block::find_frame (const symbol key) const
{ return impl->find_frame (key); }

Value::type 
Block::lookup (const symbol key) const
{ return impl->lookup (key); }

void
Block::entries (std::set<symbol>& all) const
{
  // Own entries.
  impl->frame.entries (all);
  // Context entries.
  if (impl->context)
    impl->context->entries (all);
}

int 
Block::type_size (const symbol tag) const
{ return find_frame (tag).type_size (tag); }


symbol 
Block::dimension (const symbol tag) const
{ return find_frame (tag).dimension (tag); }

symbol
Block::description (const symbol tag) const
{ return find_frame (tag).description (tag); }

bool 
Block::check (const symbol key) const
{ return impl->check (key); }

int 
Block::value_size (const symbol tag) const
{ return find_frame (tag).value_size (tag); }

double 
Block::number (const symbol key) const
{ 
  const Frame& frame = find_frame (key);
  if (frame.is_reference (key))
    return this->number (impl->expand_reference (key));

  return frame.number (key); 
}

double 
Block::number (const symbol key, double default_value) const
{ return check (key) ?  number (key) : default_value; }

symbol
Block::name (const symbol key)
{ 
  const Frame& frame = find_frame (key);
  if (frame.is_reference (key))
    return this->name (impl->expand_reference (key));

  return impl->expand_string (*this, frame.name (key)); 
}

symbol
Block::name (const symbol key, const symbol default_value)
{ return check (key) ? name (key) : default_value; }

bool 
Block::flag (const symbol key) const
{ 
  const Frame& frame = find_frame (key);
  if (frame.is_reference (key))
    return this->flag (impl->expand_reference (key));

  return frame.flag (key); 
}

bool 
Block::flag (const symbol key, bool default_value) const
{ return check (key) ? flag (key) : default_value; }

const PLF& 
Block::plf (const symbol key) const
{ 
  const Frame& frame = find_frame (key);
  if (frame.is_reference (key))
    return this->plf (impl->expand_reference (key));

  return frame.plf (key); 
}

const Frame& 
Block::frame (const symbol key) const
{ 
  const Frame& frame = find_frame (key);
  if (frame.is_reference (key))
    return this->frame (impl->expand_reference (key));

  return frame.frame (key); 
}

const FrameModel& 
Block::model (const symbol key) const
{ 
  const Frame& frame = find_frame (key);
  if (frame.is_reference (key))
    return this->model (impl->expand_reference (key));

  return frame.model (key); 
}

int 
Block::integer (const symbol key) const
{ 
  const Frame& frame = find_frame (key);
  if (frame.is_reference (key))
    return this->integer (impl->expand_reference (key));

  return frame.integer (key); 
}

int 
Block::integer (const symbol key, int default_value) const
{ return check (key) ? integer (key) : default_value; }

const std::vector<double>& 
Block::number_sequence (const symbol key) const
{ 
  const Frame& frame = find_frame (key);
  if (frame.is_reference (key))
    return this->number_sequence (impl->expand_reference (key));

  return frame.number_sequence (key); 
}

const std::vector<symbol>
Block::name_sequence (const symbol key)
{ 
  if (!impl->frame.is_reference (key))
    {
      const std::vector<symbol>& value 
        = impl->frame.name_sequence (impl->expand_reference (key));
      std::vector<symbol> result;
      for (size_t i = 0; i < value.size (); i++)
        result.push_back (impl->expand_string (*this, value[i]));
      return result;
    }
  const symbol var = impl->expand_reference (key);
  const std::vector<symbol>& value
    = impl->find_frame (var).name_sequence (var); 
  std::vector<symbol> result;
  for (size_t i = 0; i < value.size (); i++)
    result.push_back (impl->expand_string (*this, value[i]));
  return result;
}

const std::vector<bool>& 
Block::flag_sequence (const symbol key) const
{ 
  const Frame& frame = find_frame (key);
  if (frame.is_reference (key))
    return this->flag_sequence (impl->expand_reference (key));

  return frame.flag_sequence (key); 
}

const std::vector<int>& 
Block::integer_sequence (const symbol key) const
{ 
  const Frame& frame = find_frame (key);
  if (frame.is_reference (key))
    return this->integer_sequence (impl->expand_reference (key));

  return frame.integer_sequence (key); 
}

const std::vector<const Frame*>& 
Block::frame_sequence (const symbol key) const
{ 
  const Frame& frame = find_frame (key);
  if (frame.is_reference (key))
    return this->frame_sequence (impl->expand_reference (key));

  return frame.frame_sequence (key); 
}

const std::vector<const PLF*>& 
Block::plf_sequence (const symbol key) const
{ 
  const Frame& frame = find_frame (key);
  if (frame.is_reference (key))
    return this->plf_sequence (impl->expand_reference (key));

  return frame.plf_sequence (key); 
}

symbol
Block::sequence_id (const symbol key, size_t index)
{
  std::ostringstream tmp;
  tmp << key << "[" << index << "]";
  return tmp.str ();
}

Block::Block (Metalib& metalib, Treelog& msg, 
              const Frame& frame,
 	      const symbol scope_id)
  : impl (new Implementation (metalib, NULL, msg, 
                              frame, scope_id))
{ }

Block::Block (Block& block, const Frame& frame, symbol scope_tag)
  : impl (new Implementation (block.metalib (), &block, block.msg (),
                              frame, scope_tag))
{ }

Block::~Block ()
{ }

// block.C ends here.
