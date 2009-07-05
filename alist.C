// alist.C
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

#include "alist.h"
#include "avalue.h"
#include "plf.h"
#include "library.h"
#include "time.h"
#include "mathlib.h"
#include "memutils.h"
#include "assertion.h"
#include "frame_model.h"
#include "frame_submodel.h"
#include <map>
#include <sstream>

// Specific attribute values.

struct AttributeList::Implementation
{
  typedef std::map <symbol, AValue> value_map;
  value_map values;
  bool check (const symbol key) const;
  const AValue& lookup (const symbol key) const;
  void set (const symbol key, const AValue& value);
  void remove (const symbol key);
  void clear ();
};    

bool
AttributeList::Implementation::check (const symbol key) const
{ 
  return values.find (key) != values.end ();
}

const AValue& 
AttributeList::Implementation::lookup (const symbol key) const
{ 
  value_map::const_iterator i = values.find (key);
  
  if (i == values.end ())
    daisy_panic ("AList: Missing key '" + key + "'");

  return (*i).second;
}

void
AttributeList::Implementation::set (const symbol key, const AValue& value)
{
  values[key] = value;
}

void
AttributeList::Implementation::remove (const symbol key)
{
  value_map::iterator i = values.find (key);
  if (i != values.end ())
    values.erase (i);
}

void
AttributeList::Implementation::clear ()
{
  values.erase (values.begin (), values.end ());
}

bool
AttributeList::check (const symbol key) const
{
  return impl.check (key) && impl.lookup (key).type != Value::Reference; 
}

bool 
AttributeList::subset (const Metalib& metalib, 
                       const AttributeList& other, 
		       const symbol key) const
{
  // Both have key, check value.
  return impl.values[key].subset (metalib, other.impl.values[key]);
}

int
AttributeList::size (const symbol key)	const
{
  const AValue& value = impl.lookup (key);

  if (!value.is_sequence)
    return (value.type == Value::Reference) ? -1 : Value::Singleton;
  switch (value.type)
    {
    case Value::Number:
      return value.number_sequence->size ();
    case Value::Object:
      return value.model_sequence->size ();
    case Value::AList:
      return value.submodel_sequence->size ();
    case Value::PLF:
      return value.plf_sequence->size ();
    case Value::Boolean:
      return value.flag_sequence->size ();
    case Value::String:
      return value.name_sequence->size ();
    case Value::Integer:
      return value.integer_sequence->size ();
    case Value::Reference:
      return -1;
    case Value::Scalar:
    case Value::Error:
    default:
      daisy_notreached ();
    }
  // Not reached.
}

  // Variables.
void 
AttributeList::set_reference (const symbol key, const symbol v)
{ impl.set (key, AValue (v, -1)); }

bool
AttributeList::is_reference (const symbol key) const
{ return impl.check (key) && impl.lookup (key).type == Value::Reference; }
  
symbol 
AttributeList::get_reference (const symbol key) const
{
  daisy_assert (is_reference (key));
  const AValue& value = impl.lookup (key);
  value.expect (key, Value::Reference);
  return *value.name;
}

double 
AttributeList::number (const symbol key) const
{
  const AValue& value = impl.lookup (key);
  value.singleton (key);
  if (value.type == Value::Scalar)
    return value.scalar->number;
  value.expect (key, Value::Number);
  return value.number;
}

double 
AttributeList::number (const symbol key, const double default_value) const
{
  if (!check (key))
    return default_value;

  return number (key);
}

symbol
AttributeList::name (const symbol key) const
{
  const AValue& value = impl.lookup (key);
  value.singleton (key);
  if (value.type == Value::Scalar)
    return value.scalar->name;
  value.expect (key, Value::String);
  return *value.name;
}

symbol
AttributeList::name (const symbol key, const symbol default_value) const
{
  if (!check (key))
    return default_value;
  return name (key); 
}

bool 
AttributeList::flag (const symbol key) const
{
  const AValue& value = impl.lookup (key);
  value.expect (key, Value::Boolean);
  value.singleton (key);
  return value.flag;
}

bool
AttributeList::flag (const symbol key, const bool default_value) const
{
  if (!check (key))
    return default_value;
  return flag (key); 
}

int
AttributeList::integer (const symbol key) const
{
  const AValue& value = impl.lookup (key);
  value.expect (key, Value::Integer);
  value.singleton (key);
  return value.integer;
}

int
AttributeList::integer (const symbol key, const int default_value) const
{
  if (!check (key))
    return default_value;
  return integer (key); 
}

const PLF& 
AttributeList::plf (const symbol key) const
{
  const AValue& value = impl.lookup (key);
  value.expect (key, Value::PLF);
  value.singleton (key);
  return *value.plf;
}

const FrameModel& 
AttributeList::model (const symbol key) const
{
  const AValue& value = impl.lookup (key);
  value.expect (key, Value::Object);
  value.singleton (key);
  return *value.model;
}

const FrameSubmodel& 
AttributeList::submodel (const symbol key) const
{
  const AValue& value = impl.lookup (key);
  value.expect (key, Value::AList);
  value.singleton (key);
  return *value.submodel;
}

const std::vector<double>& 
AttributeList::number_sequence (const symbol key) const
{
  const AValue& value = impl.lookup (key);
  value.expect (key, Value::Number);
  value.sequence (key);
  return *value.number_sequence;
}

const std::vector<symbol>&
AttributeList::name_sequence (const symbol key) const
{
  const AValue& value = impl.lookup (key);
  value.expect (key, Value::String);
  value.sequence (key);
  return *value.name_sequence;
}

const std::vector<bool>& 
AttributeList::flag_sequence (const symbol key) const
{
  const AValue& value = impl.lookup (key);
  value.expect (key, Value::Boolean);
  value.sequence (key);
  return *value.flag_sequence;
}

const std::vector<int>& 
AttributeList::integer_sequence (const symbol key) const
{
  const AValue& value = impl.lookup (key);
  value.expect (key, Value::Integer);
  value.sequence (key);
  return *value.integer_sequence;
}

const std::vector<const PLF*>& 
AttributeList::plf_sequence (const symbol key) const
{
  const AValue& value = impl.lookup (key);
  value.expect (key, Value::PLF);
  value.sequence (key);
  return *value.plf_sequence;
}

const std::vector<const FrameModel*>& 
AttributeList::model_sequence (const symbol key) const
{
  const AValue& value = impl.lookup (key);
  value.expect (key, Value::Object);
  value.sequence (key);
  return *value.model_sequence;
}

const std::vector<const FrameSubmodel*>& 
AttributeList::submodel_sequence (const symbol key) const
{
  const AValue& value = impl.lookup (key);
  value.expect (key, Value::AList);
  value.sequence (key);
  return *value.submodel_sequence;
}

void 
AttributeList::set (const symbol key, double v)
{ impl.set (key, AValue (v)); }

void
AttributeList::set (const symbol key, double v, const symbol d)
{ impl.set (key, AValue (v, d)); }

void 
AttributeList::set (const symbol key, const symbol v)
{ impl.set (key, AValue (v)); }

void 
AttributeList::set (const symbol key, bool v)
{ impl.set (key, AValue (v)); }

void 
AttributeList::set (const symbol key, int v)
{ impl.set (key, AValue (v)); }

void 
AttributeList::set (const symbol key, const FrameModel& v)
{ impl.set (key, AValue (v)); }

void 
AttributeList::set (const symbol key, const FrameSubmodel& v)
{ impl.set (key, AValue (v)); }

void 
AttributeList::set (const symbol key, const PLF& v)
{ impl.set (key, AValue (v)); }

void 
AttributeList::set (const symbol key, const std::vector<double>& v)
{ impl.set (key, AValue (v)); }

void 
AttributeList::set (const symbol key, const std::vector<symbol>& v)
{ impl.set (key, AValue (v)); }

void 
AttributeList::set_strings (const symbol key)
{
  std::vector<symbol> all;
  set (key, all);
}

void 
AttributeList::set_strings (const symbol key, const symbol a)
{
  std::vector<symbol> all;
  all.push_back (symbol (a));
  set (key, all);
}

void 
AttributeList::set_strings (const symbol key,
                            const symbol a, const symbol b)
{
  std::vector<symbol> all;
  all.push_back (symbol (a));
  all.push_back (symbol (b));
  set (key, all);
}

void 
AttributeList::set_strings (const symbol key,
                            const symbol a, const symbol b,
                            const symbol c)
{
  std::vector<symbol> all;
  all.push_back (symbol (a));
  all.push_back (symbol (b));
  all.push_back (symbol (c));
  set (key, all);
}

void 
AttributeList::set (const symbol key, const std::vector<bool>& v)
{ impl.set (key, AValue (v)); }

void 
AttributeList::set (const symbol key, const std::vector<int>& v)
{ impl.set (key, AValue (v)); }

void 
AttributeList::set (const symbol key, 
		    const std::vector<const FrameModel*>& v)
{ impl.set (key, AValue (v)); }

void
AttributeList::set (const symbol key, 
		    const std::vector<const FrameSubmodel*>& v)
{ impl.set (key, AValue (v)); }

void 
AttributeList::set (const symbol key, const std::vector<const PLF*>& v)
{ impl.set (key, AValue (v)); }

void 
AttributeList::remove (const symbol key)
{ impl.remove (key); }

void
AttributeList::operator += (const AttributeList& al)
{
  for (Implementation::value_map::const_iterator i = al.impl.values.begin ();
       i != al.impl.values.end ();
       i++)
    impl.set ((*i).first, (*i).second);
}

void
AttributeList::operator = (const AttributeList& al)
{
  daisy_assert (&al != this);
  impl.values = al.impl.values;
}

void 
AttributeList::clear ()
{ impl.clear (); }

AttributeList::AttributeList ()
  : impl (*new Implementation ())
{ }

AttributeList::AttributeList (const AttributeList& old)
  : impl (*new Implementation ())
{ 
  daisy_assert (this != &old);
  impl.values = old.impl.values; 
}

AttributeList::~AttributeList ()
{ delete &impl; }

// alist.C ends here.
