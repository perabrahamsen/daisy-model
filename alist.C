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

#include "plf.h"
#include "library.h"
#include "alist.h"
#include "time.h"
#include "mathlib.h"
#include "memutils.h"
#include "assertion.h"
#include "frame.h"
#include <map>
#include <sstream>

// @ Value
//
// Common abstraction of an attribute value.

struct AValue
{
  struct Scalar
  {
    double number;
    symbol name;
    Scalar (double d, symbol s)
      : number (d),
        name (s)
    { }
    bool operator== (const Scalar& s)
    { return iszero (number - s.number) && name == s.name; }
  };
  union
  {
    double number;
    Scalar* scalar;
    symbol* name;
    bool flag;
    PLF* plf;
    Frame* frame;
    int integer;
    std::vector<double>* number_sequence;
    std::vector<symbol>* name_sequence;
    std::vector<bool>* flag_sequence;
    std::vector<int>* integer_sequence;
    std::vector<const PLF*>* plf_sequence;
    std::vector<const Frame*>* frame_sequence;
  };
  Value::type type;
  bool is_sequence;
  int* ref_count;

  bool subset (Metalib&, const AValue& other) const;

  void expect (const symbol key, Value::type expected) const;
  void singleton (const symbol key) const;
  void sequence (const symbol key) const;

  // Variable
  AValue (const symbol v, int)
    : name (new symbol (v)),
      type (Value::Object),	// A reference.
      is_sequence (false),
      ref_count (new int (1))
    { }
  AValue (double v)
    : number (v),
      type (Value::Number),
      is_sequence (false),
      ref_count (new int (1))
    { }
  AValue (double v, const symbol s)
    : scalar (new Scalar (v, symbol (s))),
      type (Value::Library),	// Number with user specified dimension.
      is_sequence (false),
      ref_count (new int (1))
  { }
  AValue (const symbol v)
    : name (new symbol (v)),
      type (Value::String),
      is_sequence (false),
      ref_count (new int (1))
  { }
  AValue (bool v)
    : flag (v),
      type (Value::Boolean),
      is_sequence (false),
      ref_count (new int (1))
    { }
  AValue (const PLF& v)
    : plf (new PLF (v)),
      type (Value::PLF),
      is_sequence (false),
      ref_count (new int (1))
    { }
  AValue (const Frame& f)
    : frame (&f.clone ()),
      type (Value::AList),
      is_sequence (false),
      ref_count (new int (1))
  { }
  AValue (int v)
    : integer (v),
      type (Value::Integer),
      is_sequence (false),
      ref_count (new int (1))
    { }
  AValue (const std::vector<double>& v)
    : number_sequence (new std::vector<double> (v)),
      type (Value::Number),
      is_sequence (true),
      ref_count (new int (1))
    { }
  AValue (const std::vector<symbol>& v)
    : name_sequence (new std::vector<symbol> (v)),
      type (Value::String),
      is_sequence (true),
      ref_count (new int (1))
    { }
  static std::vector<symbol>* 
  /**/ new_symbol_vector (const std::vector<std::string>& org)
  {
    std::vector<symbol>* copy = new std::vector<symbol> ();
    for (unsigned int i = 0; i < org.size (); i++)
      copy->push_back (symbol (org[i]));
    return copy;
  }
  AValue (const std::vector<std::string>& v)
    : name_sequence (new_symbol_vector (v)),
      type (Value::String),
      is_sequence (true),
      ref_count (new int (1))
    { }
  AValue (const std::vector<bool>& v)
    : flag_sequence (new std::vector<bool> (v)),
      type (Value::Boolean),
      is_sequence (true),
      ref_count (new int (1))
    { }
  AValue (const std::vector<int>& v)
    : integer_sequence (new std::vector<int> (v)),
      type (Value::Integer),
      is_sequence (true),
      ref_count (new int (1))
    { }
  static std::vector<const PLF*>* copy_plfs (const std::vector<const PLF*>& org)
  {
    std::vector<const PLF*>* copy = new std::vector<const PLF*> ();
    for (unsigned int i = 0; i < org.size (); i++)
      copy->push_back (new PLF (*org[i]));
    return copy;
  }
  AValue (const std::vector<const PLF*>& v)
    : plf_sequence (copy_plfs (v)),
      type (Value::PLF),
      is_sequence (true),
      ref_count (new int (1))
    { }
  AValue (const std::vector<const Frame*>& v)
    : type (Value::AList),
      is_sequence (true),
      ref_count (new int (1))
  { 
    frame_sequence = new std::vector<const Frame*> ();
    for (unsigned int i = 0; i < v.size (); i++)
      frame_sequence->push_back (&v[i]->clone ());
  }
  AValue ()
    : number (-42.42e42),
      type (Value::Error),
      is_sequence (false),
      ref_count (new int (1))
    { }
  AValue (const AValue& v)
    : number (v.number),
      type (v.type),
      is_sequence (v.is_sequence),
      ref_count (v.ref_count)
    { (*ref_count)++; }
  AValue& operator = (const AValue& v);
  ~AValue ()
    { cleanup (); }
  void cleanup ();
};

bool
AValue::subset (Metalib& metalib, const AValue& v) const
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
      case Value::AList:
	{
	  const Frame& value = *frame;
	  const Frame& other = *v.frame;
          return value.subset (metalib, other);
	}
      case Value::PLF:
	return *plf == *v.plf;
      case Value::Object:
      case Value::String:
	return *name == *v.name;
      case Value::Library:
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
      case Value::AList:
	{
	  const std::vector<const Frame*>& value = *frame_sequence;
	  const std::vector<const Frame*>& other = *v.frame_sequence;

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
      case Value::Object:
	return *name == *v.name;
      case Value::Library:
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
          << Value::type_name (type) << ", expected '"
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
	  case Value::AList:
            delete frame;
	    break;
	  case Value::PLF:
	    delete plf;
	    break;
	  case Value::Library:
            delete scalar;
            break;
	  case Value::Object:
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
	  case Value::AList:
            sequence_delete (frame_sequence->begin (), frame_sequence->end ());
            delete frame_sequence;
	    break;
	  case Value::PLF:
	    sequence_delete (plf_sequence->begin (), plf_sequence->end ());
	    delete plf_sequence;
	    break;
	  case Value::String:
	    delete name_sequence;
	    break;
	  case Value::Object:
	    delete name;
	    break;
	  case Value::Library:
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

  // Delete old value, if necessary.
  cleanup ();

  // Copy the data.
  type = v.type;
  is_sequence = v.is_sequence;
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
      case Value::AList:
        frame = v.frame;
        break;
      case Value::PLF:
	plf = v.plf;
        break;
      case Value::Library:
        scalar = v.scalar;
        break;
      case Value::String:
      case Value::Object:
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
      case Value::AList:
        frame_sequence = v.frame_sequence;
        break;
      case Value::PLF:
	plf_sequence = v.plf_sequence;
        break;
      case Value::String:
	name_sequence = v.name_sequence;
        break;
      case Value::Object:
	name = v.name;
        break;
      case Value::Library:
      case Value::Error:
      default:
	daisy_notreached ();
      }

  // Return this for further mutilation.
  return *this;
}

// Specific attribute values.

// @ AttributeList

typedef std::map <symbol, AValue> value_map;

struct AttributeList::Implementation
{
  value_map values;
  bool check (const symbol key) const;
  const AValue& lookup (const symbol key) const;
  void add (const symbol key, const AValue& value);
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
AttributeList::Implementation::add (const symbol key, const AValue& value)
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
  return impl.check (key) && impl.lookup (key).type != Value::Object; 
}

bool
AttributeList::subset (Metalib& metalib, 
                       const AttributeList& other, const Frame& syntax) const
{ 
  // Find syntax entries.
  std::vector<symbol> entries;
  syntax.entries (entries);
  const unsigned int size = entries.size ();

  // Loop over them.
  for (unsigned int i = 0; i < size; i++)
    {
      const symbol key = entries[i];
      if (!subset (metalib, other, key))
	return false;
    }
  return true;
}

bool 
AttributeList::subset (Metalib& metalib, 
                       const AttributeList& other, 
		       const symbol key) const
{
  if (!check (key))
    // Non-existing key is always a subset.
    return true;

  if (!other.check (key))
    // Non-existing key is never a superset.
    return false;

  // Both have key, check value.
  return impl.values[key].subset (metalib, other.impl.values[key]);
}

int
AttributeList::size (const symbol key)	const
{
  const AValue& value = impl.lookup (key);

  if (!value.is_sequence)
    return (value.type == Value::Object) ? -1 : Value::Singleton;
  switch (value.type)
    {
    case Value::Number:
      return value.number_sequence->size ();
    case Value::AList:
      return value.frame_sequence->size ();
    case Value::PLF:
      return value.plf_sequence->size ();
    case Value::Boolean:
      return value.flag_sequence->size ();
    case Value::String:
      return value.name_sequence->size ();
    case Value::Integer:
      return value.integer_sequence->size ();
    case Value::Object:
      return -1;
    case Value::Library:
    case Value::Error:
    default:
      daisy_notreached ();
    }
  // Not reached.
}

  // Variables.
void 
AttributeList::add_reference (const symbol key, const symbol v)
{ impl.add (key, AValue (v, -1)); }

bool
AttributeList::is_reference (const symbol key) const
{ return impl.check (key) && impl.lookup (key).type == Value::Object; }
  
symbol 
AttributeList::get_reference (const symbol key) const
{
  daisy_assert (is_reference (key));
  const AValue& value = impl.lookup (key);
  value.expect (key, Value::Object);
  return *value.name;
}

double 
AttributeList::number (const symbol key) const
{
  const AValue& value = impl.lookup (key);
  value.singleton (key);
  if (value.type == Value::Library)
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
  if (value.type == Value::Library)
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

AttributeList& 
AttributeList::alist (const symbol key) const
{
  const AValue& value = impl.lookup (key);
  value.expect (key, Value::AList);
  value.singleton (key);
  return value.frame->alist ();
}

Frame& 
AttributeList::frame (const symbol key) const
{
  const AValue& value = impl.lookup (key);
  value.expect (key, Value::AList);
  value.singleton (key);
  return *value.frame;
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

const std::vector<const Frame*>& 
AttributeList::frame_sequence (const symbol key) const
{
  const AValue& value = impl.lookup (key);
  value.expect (key, Value::AList);
  value.sequence (key);
  return *value.frame_sequence;
}

void 
AttributeList::add (const symbol key, double v)
{ impl.add (key, AValue (v)); }

void
AttributeList::add (const symbol key, double v, const symbol d)
{ impl.add (key, AValue (v, d)); }

void 
AttributeList::add (const symbol key, const symbol v)
{ impl.add (key, AValue (v)); }

void 
AttributeList::add (const symbol key, bool v)
{ impl.add (key, AValue (v)); }

void 
AttributeList::add (const symbol key, int v)
{ impl.add (key, AValue (v)); }

void 
AttributeList::add (const symbol key, const Frame& v)
{ impl.add (key, AValue (v)); }

void 
AttributeList::add (const symbol key, const PLF& v)
{ impl.add (key, AValue (v)); }

void 
AttributeList::add (const symbol key, const std::vector<double>& v)
{ impl.add (key, AValue (v)); }

void 
AttributeList::add (const symbol key, const std::vector<symbol>& v)
{ impl.add (key, AValue (v)); }

void 
AttributeList::add_strings (const symbol key)
{
  std::vector<symbol> all;
  add (key, all);
}

void 
AttributeList::add_strings (const symbol key, const symbol a)
{
  std::vector<symbol> all;
  all.push_back (symbol (a));
  add (key, all);
}

void 
AttributeList::add_strings (const symbol key,
                            const symbol a, const symbol b)
{
  std::vector<symbol> all;
  all.push_back (symbol (a));
  all.push_back (symbol (b));
  add (key, all);
}

void 
AttributeList::add_strings (const symbol key,
                            const symbol a, const symbol b,
                            const symbol c)
{
  std::vector<symbol> all;
  all.push_back (symbol (a));
  all.push_back (symbol (b));
  all.push_back (symbol (c));
  add (key, all);
}

void 
AttributeList::add (const symbol key, const std::vector<bool>& v)
{ impl.add (key, AValue (v)); }

void 
AttributeList::add (const symbol key, const std::vector<int>& v)
{ impl.add (key, AValue (v)); }

void 
AttributeList::add (const symbol key, 
		    const std::vector<const Frame*>& v)
{ impl.add (key, AValue (v)); }

void 
AttributeList::add (const symbol key, const std::vector<const PLF*>& v)
{ impl.add (key, AValue (v)); }

void 
AttributeList::remove (const symbol key)
{ impl.remove (key); }

void
AttributeList::operator += (const AttributeList& al)
{
  for (value_map::const_iterator i = al.impl.values.begin ();
       i != al.impl.values.end ();
       i++)
    impl.add ((*i).first, (*i).second);
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
