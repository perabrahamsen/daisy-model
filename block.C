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

#include "block_model.h"
#include "metalib.h"
#include "library.h"
#include "librarian.h"
#include "stringer.h"
#include "number.h"
#include "boolean.h"
#include "assertion.h"
#include "librarian.h"
#include "treelog.h"
#include "frame_model.h"
#include <sstream>

class PushStack
{
  std::set<symbol>& my_stack;
  const symbol my_key;
public:
  PushStack (std::set<symbol>& s, const symbol key)
    : my_stack (s),
      my_key (key)
  { my_stack.insert (my_key); }
  ~PushStack ()
  { my_stack.erase (my_key); }
};
  


symbol
Block::expand_string (const symbol value_s) const
{
  std::set<symbol> outer;
  return expand_string (value_s, outer);
}

symbol
Block::expand_string (const symbol value_s, std::set<symbol>& outer) const
{
  if (outer.find (value_s) != outer.end ())
    {
      std::ostringstream tmp;
      tmp << "Recursive expansion:";
      for (std::set<symbol>::const_iterator i = outer.begin ();
           i != outer.end ();
           i++)
        tmp << " '" << *i << "'";
      Assertion::error (tmp.str ());
      return tmp.str ();
    }

  PushStack nest (outer, value_s);
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
	      msg ().warning (std::string ("Unknown $ escape '") 
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
                  const Attribute::type type = lookup (key);
                  if (type == Attribute::Error)
                    throw "Unknown expansion: '" + key + "'";
                  const Frame& frame = find_frame (key);
                  if (frame.type_size (key) != Attribute::Singleton)
                    throw "'" + key 
                      + "' is a sequence, can only expand singletons";
                  if (!frame.check (key))
                    throw "'" + key + "' has no value";
                  switch (type)
                    {
                    case Attribute::String:
                      result << expand_string (frame.name (key), outer); 
                      break;
                    case Attribute::Integer:
                      result << frame.integer (key); 
                      break;
                    case Attribute::Number:
                      result << frame.number (key); 
                      break;
                    case Attribute::Model:
                      {
                        Treelog::Open nest (msg (), "${" + key + "}");
                        const FrameModel& obj = frame.model (key);
                        const symbol type = obj.type_name ();
                        const symbol component = frame.component (key);
                        if (component == Stringer::component)
                          {
                            const std::unique_ptr<Stringer> stringer 
                              (Librarian::build_frame<Stringer> (*this,
                                                                 obj, key));
                            if (!ok () 
                                || !stringer->initialize (units (),
                                                          *this, msg ())
                                || !stringer->check (units (), *this,
                                                     msg ())
                                || stringer->missing (*this))
                              throw "Bad string: '" + type + "'";
                            result << stringer->value (*this);
                          }
                        else if (component == Number::component)
                          {
                            const std::unique_ptr<Number> number 
                              (Librarian::build_frame<Number> (*this, 
                                                               obj, key));
                            if (!ok () 
                                || !number->initialize (units (), *this, 
                                                        msg ())
                                || !number->check (units (), *this, msg ()))
                              throw "Bad number: '"+ type + "'";
                            number->tick (units (), *this, msg ());
                            if (number->missing (*this))
                              throw "Bad number: '"+ type + "'";
                            result << number->value (*this);
                            const symbol dim = number->dimension (*this);
                            if (dim == Attribute::Fraction ()
                                || dim == Attribute::None ())
                              result << " []";
                            else if (dim != Attribute::Unknown ())
                              result << " [" << dim << "]";
                          }
                        else
                          result << type;
                        
                        break;
                      }
                    default:
                      throw "'" + key + "' unhandled type";
                    }
                }
              catch (const std::string& error)
                {
                  result << "${" << key << "}";
                  msg ().warning (error); 
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
Block::expand_reference (const symbol key) const
{
  const Frame& key_frame = find_frame (key);
  daisy_assert (key_frame.is_reference (key));

  const symbol var = key_frame.get_reference (key);
  if (var == key)
    {
      error ("Value of '" + key + "' refers to itself");
      throw "Reference loop";
    }
  const Attribute::type var_type = lookup (var);
  const Attribute::type key_type = key_frame.lookup (key);
  if (var_type != key_type)
    {
      std::ostringstream tmp;
      tmp << "Value of '" << key << "' is $" << var
          << ", which is a " << Attribute::type_name (var_type)
          << ", should be " << Attribute::type_name (key_type);
      error (tmp.str ());
      throw "Reference type mismatch";
    }
  const int var_size = find_frame (var).type_size (var);
  const int key_size = key_frame.type_size (key);
  if (var_size != key_size
      && (key_size != Attribute::Variable
          || var_size == Attribute::Singleton))
      // Matching dimension.
    {
      std::ostringstream tmp;
      tmp << "Value of '" << key << "' is $" << var
          << ", a " << Attribute::size_name (var_size)
          << ", should be " << Attribute::size_name (key_size);
      error (tmp.str ());
      throw "Reference size mismatch";
    }

  if (var_type == Attribute::Number)
    {
      daisy_assert (key_type == Attribute::Number);
      
      const symbol var_dim = find_frame (var).dimension (var);
      const symbol key_dim = key_frame.dimension (key);

      if (var_dim != key_dim)
        {
          std::ostringstream tmp;
          tmp << "Value of '" << key << "' is $" << var
              << ", which is a " << var_dim << ", should be " << key_dim;
          error (tmp.str ());
          throw "Reference unit mismatch";
        }
    }
  return var;
}

const Units& 
Block::units ()const
{ return metalib ().units (); }

Path& 
Block::path () const
{ return metalib ().path (); }

symbol
Block::type_name () const
{ return frame ().type_name (); }

void
Block::error (const std::string& value) const
{ 
  set_error (); 
  msg ().error (value); 
}

bool
Block::ok () const
{ return is_ok; }

void
Block::set_error () const
{ is_ok = false; }

void
Block::entries (std::set<symbol>& all) const
{ frame ().entries (all); }

Attribute::type 
Block::lookup (const symbol key) const
{ return frame ().lookup (key); }

bool 
Block::can_extract_as (const symbol key, Attribute::type other_type) const
{
  const Attribute::type my_type = lookup (key);
  if (my_type == other_type)
    return true;

  // BUG: Sometimes you can extract a name from a number (scalars?)
  if (my_type != Attribute::Model)
    return false;

  if (type_size (key) != Attribute::Singleton)
    return false;

  if (!check (key))
    return false;

  const Frame& frame = find_frame (key);
  switch (other_type)
    {
    case Attribute::Number:
      {
        if (frame.component (key) != Number::component)
          return false;
        if (!frame.check (*this))
          return false;
        
        std::unique_ptr<Number> number (Librarian::build_frame<Number>
                                      (*this, frame.model (key), key));
        if (!number.get ())
          return false;
        if (!number->initialize (units (), *this, msg ()))
          return false;
        if (number->missing (*this))
          return false;

        return true;
      }
    case Attribute::String:
      {
        if (frame.component (key) != Stringer::component)
          return false;
        if (!frame.check (*this))
          return false;
        
        std::unique_ptr<Stringer> stringer (Librarian::build_frame<Stringer>
                                      (*this, frame.model (key), key));
        if (!stringer.get ())
          return false;
        if (!stringer->initialize (units (), *this, msg ()))
          return false;
        if (stringer->missing (*this))
          return false;

        return true;
      }
    case Attribute::Boolean:
      {
        if (frame.component (key) != Boolean::component)
          return false;
        if (!frame.check (*this))
          return false;
        
        std::unique_ptr<Boolean> boolean (Librarian::build_frame<Boolean>
                                      (*this, frame.model (key), key));
        if (!boolean.get ())
          return false;
        if (!boolean->initialize (units (), *this, msg ()))
          return false;
        if (boolean->missing (*this))
          return false;

        return true;
      }
    default:
      return false;
    }
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
{
  const Frame& frame = find_frame (key);

  if (frame.lookup (key) == Attribute::Error)
    // Why are we asking?  No such attribute is defined.
    return false;

  if (!frame.is_reference (key))
    // Just a normal attribute.
    return frame.check (key);

  // A reference.
  const symbol var = frame.get_reference (key);
  if (key == var)
    {
      error ("Self reference: '" + key + "'");
      return false;
    }
  
  // Try expanded version.
  return this->check (var);
}

int 
Block::value_size (const symbol tag) const
{ return find_frame (tag).value_size (tag); }

double 
Block::number (const symbol key) const
{ 
  const Frame& frame = find_frame (key);
  if (frame.is_reference (key))
    return number (expand_reference (key));

  Attribute::type type = lookup (key);
  if (type != Attribute::Model)
    return frame.number (key);

  // Handle number objects.
  daisy_assert (type == Attribute::Model);
  daisy_assert (frame.component (key) == Number::component);
  daisy_assert (frame.check (*this));
  std::unique_ptr<Number> number (Librarian::build_frame<Number> 
                                (*this, frame.model (key), key));
  daisy_assert (number.get ());
  daisy_assert (number->initialize (units (), *this, msg ()));
  daisy_assert (number->check (units (), *this, msg ()));
  number->tick (units (), *this, msg ());
  daisy_assert (!number->missing (*this));
  return number->value (*this);
}

double 
Block::number (const symbol key, double default_value) const
{ 
  const Frame& frame = find_frame (key);
  if (frame.is_reference (key))
    return number (expand_reference (key), default_value);

  return frame.number (key, default_value); 
}

symbol
Block::name (const symbol key) const
{ 
  const Frame& frame = find_frame (key);
  if (frame.is_reference (key))
    return name (expand_reference (key));

  //Handle primitive names.
  Attribute::type type = lookup (key);
  if (type != Attribute::Model)
    return expand_string (frame.name (key)); 

  // Handle stringer objects.
  daisy_assert (type == Attribute::Model);
  daisy_assert (frame.component (key) == Stringer::component);
  daisy_assert (frame.check (*this));
  std::unique_ptr<Stringer> stringer (Librarian::build_frame<Stringer> 
                                (*this, frame.model (key), key));
  daisy_assert (stringer.get ());
  daisy_assert (stringer->initialize (units (), *this, msg ()));
  daisy_assert (stringer->check (units (), *this, msg ()));
  stringer->tick (units (), *this, msg ());
  daisy_assert (!stringer->missing (*this));
  return symbol (stringer->value (*this));
}

symbol
Block::name (const symbol key, const symbol default_value) const
{ 
  const Frame& frame = find_frame (key);
  if (frame.is_reference (key))
    return name (expand_reference (key), default_value);

  return expand_string (frame.name (key, default_value)); 
}

bool 
Block::flag (const symbol key) const
{ 
  const Frame& frame = find_frame (key);
  if (frame.is_reference (key))
    return flag (expand_reference (key));

  //Handle primitive flags.
  Attribute::type type = lookup (key);
  if (type != Attribute::Model)
    return frame.flag (key); 

  // Handle boolean objects.
  daisy_assert (type == Attribute::Model);
  daisy_assert (frame.component (key) == Boolean::component);
  daisy_assert (frame.check (*this));
  std::unique_ptr<Boolean> boolean (Librarian::build_frame<Boolean> 
                                (*this, frame.model (key), key));
  daisy_assert (boolean.get ());
  daisy_assert (boolean->initialize (units (), *this, msg ()));
  daisy_assert (boolean->check (units (), *this, msg ()));
  boolean->tick (units (), *this, msg ());
  daisy_assert (!boolean->missing (*this));
  return boolean->value (*this);
}

bool 
Block::flag (const symbol key, bool default_value) const
{ 
  const Frame& frame = find_frame (key);
  if (frame.is_reference (key))
    return flag (expand_reference (key), default_value);

  return frame.flag (key, default_value); 
}

const PLF& 
Block::plf (const symbol key) const
{ return *plf_ptr (key); }

boost::shared_ptr<const PLF>
Block::plf_ptr (const symbol key) const
{ 
  const Frame& frame = find_frame (key);
  if (frame.is_reference (key))
    return plf_ptr (expand_reference (key));

  return frame.plf_ptr (key); 
}

const FrameModel& 
Block::model (const symbol key) const
{ return *model_ptr (key); }

boost::shared_ptr<const FrameModel>
Block::model_ptr (const symbol key) const
{ 
  const Frame& frame = find_frame (key);
  if (frame.is_reference (key))
    return model_ptr (expand_reference (key));

  return frame.model_ptr (key); 
}

const FrameSubmodel& 
Block::submodel (const symbol key) const
{ return *submodel_ptr (key); }

boost::shared_ptr<const FrameSubmodel>
Block::submodel_ptr (const symbol key) const
{ 
  const Frame& frame = find_frame (key);
  if (frame.is_reference (key))
    return submodel_ptr (expand_reference (key));

  return frame.submodel_ptr (key); 
}

int 
Block::integer (const symbol key) const
{ 
  const Frame& frame = find_frame (key);
  if (frame.is_reference (key))
    return integer (expand_reference (key));

  return frame.integer (key); 
}

int 
Block::integer (const symbol key, int default_value) const
{ 
  const Frame& frame = find_frame (key);
  if (frame.is_reference (key))
    return integer (expand_reference (key), default_value);

  return frame.integer (key, default_value); 
}


const std::vector<double>& 
Block::number_sequence (const symbol key) const
{ 
  const Frame& frame = find_frame (key);
  if (frame.is_reference (key))
    return number_sequence (expand_reference (key));

  return frame.number_sequence (key); 
}

const std::vector<symbol>
Block::name_sequence (const symbol key) const
{ 
  const Frame& frame = find_frame (key);
  if (frame.is_reference (key))
    return name_sequence (expand_reference (key));

  const std::vector<symbol>& value = frame.name_sequence (key);
  std::vector<symbol> result;
  for (size_t i = 0; i < value.size (); i++)
    result.push_back (expand_string (value[i]));
  return result;
}

const std::vector<bool>& 
Block::flag_sequence (const symbol key) const
{ 
  const Frame& frame = find_frame (key);
  if (frame.is_reference (key))
    return flag_sequence (expand_reference (key));

  return frame.flag_sequence (key); 
}

const std::vector<int>& 
Block::integer_sequence (const symbol key) const
{ 
  const Frame& frame = find_frame (key);
  if (frame.is_reference (key))
    return integer_sequence (expand_reference (key));

  return frame.integer_sequence (key); 
}

const std::vector<boost::shared_ptr<const FrameModel>/**/>& 
Block::model_sequence (const symbol key) const
{ 
  const Frame& frame = find_frame (key);
  if (frame.is_reference (key))
    return model_sequence (expand_reference (key));

  return frame.model_sequence (key); 
}

const std::vector<boost::shared_ptr<const FrameSubmodel>/**/>& 
Block::submodel_sequence (const symbol key) const
{ 
  const Frame& frame = find_frame (key);
  if (frame.is_reference (key))
    return submodel_sequence (expand_reference (key));

  return frame.submodel_sequence (key); 
}

const std::vector<boost::shared_ptr<const PLF>/**/>& 
Block::plf_sequence (const symbol key) const
{ 
  const Frame& frame = find_frame (key);
  if (frame.is_reference (key))
    return plf_sequence (expand_reference (key));

  return frame.plf_sequence (key); 
}

symbol
Block::sequence_id (const symbol key, size_t index)
{
  std::ostringstream tmp;
  tmp << key << "[" << index << "]";
  return tmp.str ();
}

Block::Block ()
  : is_ok (true)
{ }

Block::~Block ()
{ }

// block.C ends here.
