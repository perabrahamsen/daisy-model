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
#include "assertion.h"
#include "scope_block.h"
#include "librarian.h"
#include "treelog.h"
#include "frame_model.h"
#include <sstream>

symbol
Block::expand_string (const symbol value_s) const
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
                      result << frame.name (key); 
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
                        const ScopeBlock scope (*this);
                        if (component == Stringer::component)
                          {
                            const std::auto_ptr<Stringer> stringer 
                              (Librarian::build_frame<Stringer> (*this,
                                                                 obj, key));
                            if (!ok () 
                                || !stringer->initialize (units (),
                                                          scope, msg ())
                                || !stringer->check (units (), scope,
                                                     msg ())
                                || stringer->missing (scope))
                              throw "Bad string: '" + type + "'";
                            result << stringer->value (scope);
                          }
                        else if (component == Number::component)
                          {
                            const std::auto_ptr<Number> number 
                              (Librarian::build_frame<Number> (*this, 
                                                               obj, key));
                            if (!ok () 
                                || !number->initialize (units (), scope, 
                                                        msg ())
                                || !number->check (units (), scope, msg ()))
                              throw "Bad number: '"+ type + "'";
                            number->tick (units (), scope, msg ());
                            if (number->missing (scope))
                              throw "Bad number: '"+ type + "'";
                            result << number->value (scope);
                            const symbol dim = number->dimension (scope);
                            if (dim == Attribute::Fraction ()
                                || dim == Attribute::None ())
                              result << " []";
                            else if (dim != Attribute::Unknown ())
                              result << " [" << dim << "]";
                          }
                        else
                          throw "Unhandled object type '"
                            + component.name () + "'";
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
  if (!frame ().is_reference (key))
    return key;
  
  const symbol var = frame ().get_reference (key);
  if (var == key)
    {
      error ("Value of '" + key + "' refers to itself");
      throw "Reference loop";
    }
  if (lookup (var) == frame ().lookup (key)
      && (find_frame (var).type_size (var) == frame ().type_size (key)
          || (frame ().type_size (key) == Attribute::Variable
              && (find_frame (var).type_size (var) 
                  != Attribute::Singleton))))
    return var;

  std::ostringstream tmp;
  tmp << "Value of '" << key << "' is $" << var
      << ", which is a " << Attribute::type_name (lookup (var));
  switch (find_frame (var).type_size (var))
    {
    case Attribute::Singleton:
      break;
    case Attribute::CanopyCells:
      tmp << " canopy intervals";
      break;
    case Attribute::CanopyEdges:
      tmp << " canopy boundaries";
      break;
    case Attribute::SoilCells:
      tmp << " soil cells";
      break;
    case Attribute::SoilEdges:
      tmp << " soil edges";
      break;
    case Attribute::Variable:
      tmp << " sequence";
      break;
    default:
      tmp << "[" << find_frame (var).type_size (var) << "]";
    }
  tmp << ", should be a " << Attribute::type_name (frame ().lookup (key));
  switch (frame ().type_size (key))
    {
    case Attribute::Singleton:
      break;
    case Attribute::CanopyCells:
      tmp << " canopy intervals";
      break;
    case Attribute::CanopyEdges:
      tmp << " canopy boundaries";
      break;
    case Attribute::SoilCells:
      tmp << " soil cells";
      break;
    case Attribute::SoilEdges:
      tmp << " soil edges";
      break;
    case Attribute::Variable:
      tmp << " sequence";
      break;
    default:
      tmp << "[" << frame ().type_size (key) << "]";
    }
  error (tmp.str ());
  throw ("Bad reference");
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

Attribute::type 
Block::lookup (const symbol key) const
{ return frame ().lookup (key); }

void
Block::entries (std::set<symbol>& all) const
{ frame ().entries (all); }

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
  Attribute::type type = frame ().lookup (key);
  if (type != Attribute::Error)
    return frame ().check (key);

  return false;
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

  return frame.number (key); 
}

double 
Block::number (const symbol key, double default_value) const
{ return check (key) ?  number (key) : default_value; }

symbol
Block::name (const symbol key) const
{ 
  const Frame& frame = find_frame (key);
  if (frame.is_reference (key))
    return name (expand_reference (key));

  return expand_string (frame.name (key)); 
}

symbol
Block::name (const symbol key, const symbol default_value) const
{ return check (key) ? name (key) : default_value; }

bool 
Block::flag (const symbol key) const
{ 
  const Frame& frame = find_frame (key);
  if (frame.is_reference (key))
    return flag (expand_reference (key));

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
    return plf (expand_reference (key));

  return frame.plf (key); 
}

const FrameModel& 
Block::model (const symbol key) const
{ 
  const Frame& frame = find_frame (key);
  if (frame.is_reference (key))
    return model (expand_reference (key));

  return frame.model (key); 
}

const FrameSubmodel& 
Block::submodel (const symbol key) const
{ 
  const Frame& frame = find_frame (key);
  if (frame.is_reference (key))
    return submodel (expand_reference (key));

  return frame.submodel (key); 
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
{ return check (key) ? integer (key) : default_value; }

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
  if (!frame ().is_reference (key))
    {
      const std::vector<symbol>& value 
        = frame ().name_sequence (expand_reference (key));
      std::vector<symbol> result;
      for (size_t i = 0; i < value.size (); i++)
        result.push_back (expand_string (value[i]));
      return result;
    }
  const symbol var = expand_reference (key);
  const std::vector<symbol>& value
    = find_frame (var).name_sequence (var); 
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
