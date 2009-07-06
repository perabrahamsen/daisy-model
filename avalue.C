// avalue.C -- Attribute values.
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
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

#include "avalue.h"
#include "mathlib.h"
#include "plf.h"
#include "frame_model.h"
#include "frame_submodel.h"
#include "assertion.h"
#include "memutils.h"
#include <sstream>

AValue::Scalar::Scalar (double d, symbol s)
  : number (d),
    name (s)
{ }

bool 
AValue::Scalar::operator== (const Scalar& s) const
{ return iszero (number - s.number) && name == s.name; }

AValue::AValue (const symbol v, int)
  : name (new symbol (v)),
    type (Value::Reference),	// A reference.
    is_sequence (false),
    ref_count (new int (1))
{ }

AValue::AValue (double v)
  : number (v),
    type (Value::Number),
    is_sequence (false),
    ref_count (new int (1))
{ }

AValue::AValue (double v, const symbol s)
  : scalar (new Scalar (v, symbol (s))),
    type (Value::Scalar),	// Number with user specified dimension.
    is_sequence (false),
    ref_count (new int (1))
{ }

AValue::AValue (const symbol v)
  : name (new symbol (v)),
    type (Value::String),
    is_sequence (false),
    ref_count (new int (1))
{ }

AValue::AValue (bool v)
  : flag (v),
    type (Value::Boolean),
    is_sequence (false),
    ref_count (new int (1))
{ }

AValue::AValue (const PLF& v)
  : plf (new PLF (v)),
    type (Value::PLF),
    is_sequence (false),
    ref_count (new int (1))
{ }

AValue::AValue (const FrameModel& f)
  : model (&f.clone ()),
    type (Value::Object),
    is_sequence (false),
    ref_count (new int (1))
{ }

AValue::AValue (const FrameSubmodel& f)
  : submodel (&f.clone ()),
    type (Value::AList),
    is_sequence (false),
    ref_count (new int (1))
{ }

AValue::AValue (int v)
  : integer (v),
    type (Value::Integer),
    is_sequence (false),
    ref_count (new int (1))
{ }

AValue::AValue (const std::vector<double>& v)
  : number_sequence (new std::vector<double> (v)),
    type (Value::Number),
    is_sequence (true),
    ref_count (new int (1))
{ }

AValue::AValue (const std::vector<symbol>& v)
  : name_sequence (new std::vector<symbol> (v)),
    type (Value::String),
    is_sequence (true),
    ref_count (new int (1))
{ }

std::vector<symbol>* 
AValue::new_symbol_vector (const std::vector<std::string>& org)
{
  std::vector<symbol>* copy = new std::vector<symbol> ();
  for (unsigned int i = 0; i < org.size (); i++)
    copy->push_back (symbol (org[i]));
  return copy;
}

AValue::AValue (const std::vector<std::string>& v)
  : name_sequence (new_symbol_vector (v)),
    type (Value::String),
    is_sequence (true),
    ref_count (new int (1))
{ }

AValue::AValue (const std::vector<bool>& v)
  : flag_sequence (new std::vector<bool> (v)),
    type (Value::Boolean),
    is_sequence (true),
    ref_count (new int (1))
{ }

AValue::AValue (const std::vector<int>& v)
  : integer_sequence (new std::vector<int> (v)),
    type (Value::Integer),
    is_sequence (true),
    ref_count (new int (1))
{ }

AValue::AValue (const std::vector<boost::shared_ptr<const PLF>/**/>& v)
  : plf_sequence (new const std::vector<boost::shared_ptr<const PLF>/**/> (v)),
    type (Value::PLF),
    is_sequence (true),
    ref_count (new int (1))
{ }

AValue::AValue (const std::vector<const FrameModel*>& v)
  : type (Value::Object),
    is_sequence (true),
    ref_count (new int (1))
{ 
  model_sequence = new std::vector<const FrameModel*> ();
  for (unsigned int i = 0; i < v.size (); i++)
    model_sequence->push_back (&v[i]->clone ());
}

AValue::AValue (const std::vector<boost::shared_ptr<const FrameSubmodel>/**/>& v)
  : submodel_sequence (new std::vector<boost::shared_ptr<const FrameSubmodel>/**/> (v)),
    type (Value::AList),
    is_sequence (true),
    ref_count (new int (1))
{ }

AValue::AValue ()
  : number (-42.42e42),
    type (Value::Error),
    is_sequence (false),
    ref_count (new int (1))
{ }

AValue::AValue (const AValue& v)
  : number (v.number),
    type (v.type),
    is_sequence (v.is_sequence),
    ref_count (v.ref_count)
{ (*ref_count)++; }

AValue::~AValue ()
{ cleanup (); }

bool
AValue::subset (const Metalib& metalib, const AValue& v) const
{
  daisy_assert (type == v.type);
  daisy_assert (is_sequence == v.is_sequence);

  if (!is_sequence)
    switch (type)
      {
      case Value::Number:
	return iszero (number - v.number);
      case Value::Boolean:
	return flag == v.flag;
      case Value::Integer:
	return integer == v.integer;
      case Value::Object:
	{
	  const FrameModel& value = *model;
	  const FrameModel& other = *v.model;
          return value.subset (metalib, other);
	}
      case Value::AList:
	{
	  const FrameSubmodel& value = *submodel;
	  const FrameSubmodel& other = *v.submodel;
          return value.subset (metalib, other);
	}
      case Value::PLF:
	return *plf == *v.plf;
      case Value::Reference:
      case Value::String:
	return *name == *v.name;
      case Value::Scalar:
        return *scalar == *v.scalar;
      case Value::Error:
      default:
	daisy_notreached ();
      }
  else
    switch (type)
      {
      case Value::Number:
        {
          // We get warnings with -Wfloat-equal if we just do a 
          //   return *number_sequence == *v.number_sequence
          // So here is the workaround...
          const size_t size = number_sequence->size ();
          if (size != v.number_sequence->size ())
            return false;
          for (size_t i = 0; i < size; i++)
            if (!iszero ((*number_sequence)[i] - (*v.number_sequence)[i]))
              return false;
          return true;
        }
      case Value::Boolean:
	return *flag_sequence == *v.flag_sequence;
      case Value::Integer:
	return *integer_sequence == *v.integer_sequence;
      case Value::Object:
	{
	  const std::vector<const FrameModel*>& value = *model_sequence;
	  const std::vector<const FrameModel*>& other = *v.model_sequence;

	  const unsigned int size = value.size ();
	  if (other.size () != size)
	    return false;

          for (unsigned int i = 0; i < size; i++)
            if (!value[i]->subset (metalib, *other[i]))
              return false;

	  return true;
	}
      case Value::AList:
	{
	  const std::vector<boost::shared_ptr<const FrameSubmodel>/**/>& value = *submodel_sequence;
	  const std::vector<boost::shared_ptr<const FrameSubmodel>/**/>& other = *v.submodel_sequence;

	  const unsigned int size = value.size ();
	  if (other.size () != size)
	    return false;

          for (unsigned int i = 0; i < size; i++)
            if (!value[i]->subset (metalib, *other[i]))
              return false;

	  return true;
	}
      case Value::PLF:
	return *plf_sequence == *v.plf_sequence;
      case Value::String:
	return *name_sequence == *v.name_sequence;
      case Value::Reference:
	return *name == *v.name;
      case Value::Scalar:
      case Value::Error:
      default:
	daisy_panic ("Not reached");
      }
  // Not reached.
}

void 
AValue::expect (const symbol key, Value::type expected) const
{
  if (type != expected)
    {
      std::ostringstream tmp;
      tmp << "AValue of parameter '" << key << "' is a '" 
          << Value::type_name (type) << "', expected '"
          << Value::type_name (expected) << "'";
      daisy_panic (tmp.str ());
    }
}

void
AValue::singleton (const symbol key) const
{
  if (!is_sequence)
    return;
  std::ostringstream tmp;
  tmp << "AValue of parameter '" << key 
      << "' is a sequence, expected a singleton";
  daisy_panic (tmp.str ());
}

void
AValue::sequence (const symbol key) const
{
  if (is_sequence)
    return;
  std::ostringstream tmp;
  tmp << "AValue of parameter '" << key 
      << "' is a singleton, expected a sequence";
  daisy_panic (tmp.str ());
}

void 
AValue::cleanup ()
{
  switch (*ref_count)
    {
    case 1:
      delete ref_count;
      if (!is_sequence)
	switch (type)
	  {
	  case Value::Number:
	  case Value::Boolean:
	  case Value::Integer:
	    // Primitives, do nothing.
	    break;
	  case Value::Object:
            delete model;
	    break;
	  case Value::AList:
            delete submodel;
	    break;
	  case Value::PLF:
	    delete plf;
	    break;
	  case Value::Scalar:
            delete scalar;
            break;
	  case Value::Reference:
	  case Value::String:
	    delete name;
	    break;
	  case Value::Error:
	    // Empty (dummy) value.
	    break;
	  default:
	    daisy_notreached ();
	  }
      else
	switch (type)
	  {
	  case Value::Number:
	    delete number_sequence;
	    break;
	  case Value::Boolean:
	    delete number_sequence;
	    break;
	  case Value::Integer:
	    delete number_sequence;
	    break;
	  case Value::Object:
            sequence_delete (model_sequence->begin (), model_sequence->end ());
            delete model_sequence;
	    break;
	  case Value::AList:
            // sequence_delete (submodel_sequence->begin (), submodel_sequence->end ());
            delete submodel_sequence;
	    break;
	  case Value::PLF:
	    // sequence_delete (plf_sequence->begin (), plf_sequence->end ());
	    delete plf_sequence;
	    break;
	  case Value::String:
	    delete name_sequence;
	    break;
	  case Value::Reference:
	    delete name;
	    break;
	  case Value::Scalar:
	  case Value::Error:
	  default:
	    daisy_notreached ();
	  }
      break;
    default:
      daisy_assert (*ref_count > 1);
      (*ref_count)--;
    }
}

AValue& 
AValue::operator= (const AValue& v)
{
  // Check that we aren't overwriting ourself.
  if (&v == this)
    return *this;

  // Take care of orphans.
  switch (type)
    {
    case Value::Object:
      if (is_sequence)
        {
          for (size_t i = 0; i < model_sequence->size (); i++)
            {
              const FrameModel& entry = *(*model_sequence)[i];
              entry.reparent_children (entry.parent ());
            }
        }
      else
        {
          daisy_assert (model != v.model);
          model->reparent_children (v.model);
        }
      break;
    case Value::AList:
      if (is_sequence)
        {
          for (size_t i = 0; i < submodel_sequence->size (); i++)
            {
              const FrameSubmodel& entry = *(*submodel_sequence)[i];
              entry.reparent_children (entry.parent ());
            }
        }
      else
        {
          daisy_assert (submodel != v.submodel);
          model->reparent_children (v.submodel);
        }
      break;
    default:
      /* Do nothing */;
    }

  // Delete old value, if necessary.
  cleanup ();

  // Copy the data.
  if (type == Value::Error
      || type == Value::Reference 
      || v.type == Value::Reference)
    {
      type = v.type;
      is_sequence = v.is_sequence;
    }
  else 
    {
      daisy_assert (type == v.type);
      daisy_assert (is_sequence == v.is_sequence);
    }
  ref_count = v.ref_count;
  (*ref_count)++;

  if (!is_sequence)
    switch (type)
      {
      case Value::Number:
	number = v.number;
        break;
      case Value::Boolean:
	flag = v.flag;
        break;
      case Value::Integer:
	integer = v.integer;
        break;
      case Value::Object:
        model = v.model;
        break;
      case Value::AList:
        submodel= v.submodel;
        break;
      case Value::PLF:
	plf = v.plf;
        break;
      case Value::Scalar:
        scalar = v.scalar;
        break;
      case Value::String:
      case Value::Reference:
	name = v.name;
        break;
      case Value::Error:
      default:
	daisy_notreached ();
      }
  else
    switch (type)
      {
      case Value::Number:
	number_sequence = v.number_sequence;
        break;
      case Value::Boolean:
	flag_sequence = v.flag_sequence;
        break;
      case Value::Integer:
	integer_sequence = v.integer_sequence;
        break;
      case Value::Object:
        model_sequence = v.model_sequence;
        break;
      case Value::AList:
        submodel_sequence = v.submodel_sequence;
        break;
      case Value::PLF:
	plf_sequence = v.plf_sequence;
        break;
      case Value::String:
	name_sequence = v.name_sequence;
        break;
      case Value::Reference:
	name = v.name;
        break;
      case Value::Scalar:
      case Value::Error:
      default:
	daisy_notreached ();
      }

  // Return this for further mutilation.
  return *this;
}

// avalue.C ends here.
