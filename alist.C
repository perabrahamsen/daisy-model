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
#include "syntax.h"
#include "time.h"
#include "mathlib.h"
#include "memutils.h"
#include "assertion.h"
#include <map>
#include <sstream>

// @ Value
//
// Common abstraction of an attribute value.

struct Value
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
    AttributeList* alist;
    int integer;
    std::vector<double>* number_sequence;
    std::vector<symbol>* name_sequence;
    std::vector<bool>* flag_sequence;
    std::vector<int>* integer_sequence;
    std::vector<const PLF*>* plf_sequence;
    std::vector<AttributeList*>* alist_sequence;
  };
  Syntax::type type;
  bool is_sequence;
  int* ref_count;

  bool subset (const Metalib&, 
               const Value& other, const Syntax&, const std::string& key) const;

  void expect (const std::string& key, Syntax::type expected) const;
  void singleton (const std::string& key) const;
  void sequence (const std::string& key) const;

  // Variable
  Value (const std::string& v, int)
    : name (new symbol (v)),
      type (Syntax::Object),	// A reference.
      is_sequence (false),
      ref_count (new int (1))
    { }
  Value (double v)
    : number (v),
      type (Syntax::Number),
      is_sequence (false),
      ref_count (new int (1))
    { }
  Value (double v, const std::string& s)
    : scalar (new Scalar (v, symbol (s))),
      type (Syntax::Library),	// Number with user specified dimension.
      is_sequence (false),
      ref_count (new int (1))
  { }
  Value (const symbol v)
    : name (new symbol (v)),
      type (Syntax::String),
      is_sequence (false),
      ref_count (new int (1))
  { }
  Value (const std::string& v)
    : name (new symbol (v)),
      type (Syntax::String),
      is_sequence (false),
      ref_count (new int (1))
    { }
  Value (const char *const v)
    : name (new symbol (v)),
      type (Syntax::String),
      is_sequence (false),
      ref_count (new int (1))
    { }
  Value (bool v)
    : flag (v),
      type (Syntax::Boolean),
      is_sequence (false),
      ref_count (new int (1))
    { }
  Value (const PLF& v)
    : plf (new PLF (v)),
      type (Syntax::PLF),
      is_sequence (false),
      ref_count (new int (1))
    { }
  Value (const AttributeList& v)
    : alist (new AttributeList (v)),
      type (Syntax::AList),
      is_sequence (false),
      ref_count (new int (1))
    { }
  Value (int v)
    : integer (v),
      type (Syntax::Integer),
      is_sequence (false),
      ref_count (new int (1))
    { }
  Value (const std::vector<double>& v)
    : number_sequence (new std::vector<double> (v)),
      type (Syntax::Number),
      is_sequence (true),
      ref_count (new int (1))
    { }
  Value (const std::vector<symbol>& v)
    : name_sequence (new std::vector<symbol> (v)),
      type (Syntax::String),
      is_sequence (true),
      ref_count (new int (1))
    { }
  static std::vector<symbol>* new_symbol_vector (const std::vector<std::string>& org)
  {
    std::vector<symbol>* copy = new std::vector<symbol> ();
    for (unsigned int i = 0; i < org.size (); i++)
      copy->push_back (symbol (org[i]));
    return copy;
  }
  Value (const std::vector<std::string>& v)
    : name_sequence (new_symbol_vector (v)),
      type (Syntax::String),
      is_sequence (true),
      ref_count (new int (1))
    { }
  Value (const std::vector<bool>& v)
    : flag_sequence (new std::vector<bool> (v)),
      type (Syntax::Boolean),
      is_sequence (true),
      ref_count (new int (1))
    { }
  Value (const std::vector<int>& v)
    : integer_sequence (new std::vector<int> (v)),
      type (Syntax::Integer),
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
  Value (const std::vector<const PLF*>& v)
    : plf_sequence (copy_plfs (v)),
      type (Syntax::PLF),
      is_sequence (true),
      ref_count (new int (1))
    { }
  std::vector<AttributeList*>* copy_alists (const std::vector<AttributeList*>& org)
  {
    std::vector<AttributeList*>* copy = new std::vector<AttributeList*> ();
    for (unsigned int i = 0; i < org.size (); i++)
      copy->push_back (new AttributeList (*org[i]));
    return copy;
  }
  Value (const std::vector<AttributeList*>& v)
    : alist_sequence (copy_alists (v)),
      type (Syntax::AList),
      is_sequence (true),
      ref_count (new int (1))
    { }
  Value ()
    : number (-42.42e42),
      type (Syntax::Error),
      is_sequence (false),
      ref_count (new int (1))
    { }
  Value (const Value& v)
    : number (v.number),
      type (v.type),
      is_sequence (v.is_sequence),
      ref_count (v.ref_count)
    { (*ref_count)++; }
  Value& operator = (const Value& v);
  ~Value ()
    { cleanup (); }
  void cleanup ();
};

bool
Value::subset (const Metalib& metalib, const Value& v, const Syntax& syntax, 
	       const std::string& key) const
{
  daisy_assert (type == v.type);
  daisy_assert (is_sequence == v.is_sequence);

  if (!is_sequence)
    switch (type)
      {
      case Syntax::Number:
	return iszero (number - v.number);
      case Syntax::Boolean:
	return flag == v.flag;
      case Syntax::Integer:
	return integer == v.integer;
      case Syntax::AList:
	{
	  const AttributeList& value = *alist;
	  const AttributeList& other = *v.alist;
	  const Syntax::type type = syntax.lookup (key);
	  if (type == Syntax::AList)
	    return value.subset (metalib, other, syntax.syntax (key));
	  daisy_assert (type == Syntax::Object);
	  const Library& library = syntax.library (metalib, key);

	  daisy_assert (value.check ("type"));
	  if (!other.check ("type"))
	    return false;
	  const symbol element = value.identifier ("type");
	  if (element != other.identifier ("type"))
	    return false;
	  if (!library.check (element))
	    return false;
	  return value.subset (metalib, other, library.syntax (element));
	}
      case Syntax::PLF:
	return *plf == *v.plf;
      case Syntax::Object:
      case Syntax::String:
	return *name == *v.name;
      case Syntax::Library:
        return *scalar == *v.scalar;
      case Syntax::Error:
      default:
	daisy_notreached ();
      }
  else
    switch (type)
      {
      case Syntax::Number:
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
      case Syntax::Boolean:
	return *flag_sequence == *v.flag_sequence;
      case Syntax::Integer:
	return *integer_sequence == *v.integer_sequence;
      case Syntax::AList:
	{
	  const std::vector<AttributeList*>& value = *alist_sequence;
	  const std::vector<AttributeList*>& other = *v.alist_sequence;

	  const unsigned int size = value.size ();
	  if (other.size () != size)
	    return false;
	  const Syntax::type type = syntax.lookup (key);
	  if (type == Syntax::AList)
	    {
	      const Syntax& nested = syntax.syntax (key);
	      for (unsigned int i = 0; i < size; i++)
		if (!value[i]->subset (metalib, *other[i], nested))
		  return false;
	      return true;
		
	    }
	  daisy_assert (type == Syntax::Object);
	  const Library& library = syntax.library (metalib, key);
	  for (unsigned int i = 0; i < size; i++)
	    {
	      daisy_assert (value[i]->check ("type"));
	      if (!other[i]->check ("type"))
		return false;
	      const symbol element = value[i]->identifier ("type");
	      if (element != other[i]->identifier ("type"))
		return false;
	      if (!library.check (element))
		return false;
	      if (!value[i]->subset (metalib, 
                                     *other[i], library.syntax (element)))
		return false;
	    }
	  return true;
	}
      case Syntax::PLF:
	return *plf_sequence == *v.plf_sequence;
      case Syntax::String:
	return *name_sequence == *v.name_sequence;
      case Syntax::Object:
	return *name == *v.name;
      case Syntax::Library:
      case Syntax::Error:
      default:
	daisy_panic ("Not reached");
      }
  // Not reached.
}

void 
Value::expect (const std::string& key, Syntax::type expected) const
{
  if (type != expected)
    {
      std::ostringstream tmp;
      tmp << "Value of parameter '" << key << "' is a '" 
          << Syntax::type_name (type) << ", expected '"
          << Syntax::type_name (expected) << "'";
      daisy_panic (tmp.str ());
    }
}

void
Value::singleton (const std::string& key) const
{
  if (!is_sequence)
    return;
  std::ostringstream tmp;
  tmp << "Value of parameter '" << key 
      << "' is a sequence, expected a singleton";
  daisy_panic (tmp.str ());
}

void
Value::sequence (const std::string& key) const
{
  if (is_sequence)
    return;
  std::ostringstream tmp;
  tmp << "Value of parameter '" << key 
      << "' is a singleton, expected a sequence";
  daisy_panic (tmp.str ());
}

void 
Value::cleanup ()
{
  switch (*ref_count)
    {
    case 1:
      delete ref_count;
      if (!is_sequence)
	switch (type)
	  {
	  case Syntax::Number:
	  case Syntax::Boolean:
	  case Syntax::Integer:
	    // Primitives, do nothing.
	    break;
	  case Syntax::AList:
	    delete alist;
	    break;
	  case Syntax::PLF:
	    delete plf;
	    break;
	  case Syntax::Library:
            delete scalar;
            break;
	  case Syntax::Object:
	  case Syntax::String:
	    delete name;
	    break;
	  case Syntax::Error:
	    // Empty (dummy) value.
	    break;
	  default:
	    daisy_notreached ();
	  }
      else
	switch (type)
	  {
	  case Syntax::Number:
	    delete number_sequence;
	    break;
	  case Syntax::Boolean:
	    delete number_sequence;
	    break;
	  case Syntax::Integer:
	    delete number_sequence;
	    break;
	  case Syntax::AList:
	    sequence_delete (alist_sequence->begin (), alist_sequence->end ());
	    delete alist_sequence;
	    break;
	  case Syntax::PLF:
	    sequence_delete (plf_sequence->begin (), plf_sequence->end ());
	    delete plf_sequence;
	    break;
	  case Syntax::String:
	    delete name_sequence;
	    break;
	  case Syntax::Object:
	    delete name;
	    break;
	  case Syntax::Library:
	  case Syntax::Error:
	  default:
	    daisy_notreached ();
	  }
      break;
    default:
      daisy_assert (*ref_count > 1);
      (*ref_count)--;
    }
}

Value& 
Value::operator= (const Value& v)
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
      case Syntax::Number:
	number = v.number;
        break;
      case Syntax::Boolean:
	flag = v.flag;
        break;
      case Syntax::Integer:
	integer = v.integer;
        break;
      case Syntax::AList:
        alist = v.alist;
        break;
      case Syntax::PLF:
	plf = v.plf;
        break;
      case Syntax::Library:
        scalar = v.scalar;
        break;
      case Syntax::String:
      case Syntax::Object:
	name = v.name;
        break;
      case Syntax::Error:
      default:
	daisy_notreached ();
      }
  else
    switch (type)
      {
      case Syntax::Number:
	number_sequence = v.number_sequence;
        break;
      case Syntax::Boolean:
	flag_sequence = v.flag_sequence;
        break;
      case Syntax::Integer:
	integer_sequence = v.integer_sequence;
        break;
      case Syntax::AList:
        alist_sequence = v.alist_sequence;
        break;
      case Syntax::PLF:
	plf_sequence = v.plf_sequence;
        break;
      case Syntax::String:
	name_sequence = v.name_sequence;
        break;
      case Syntax::Object:
	name = v.name;
        break;
      case Syntax::Library:
      case Syntax::Error:
      default:
	daisy_notreached ();
      }

  // Return this for further mutilation.
  return *this;
}

// Specific attribute values.

// @ AttributeList

typedef std::map <std::string, Value> value_map;

struct AttributeList::Implementation
{
  value_map values;
  bool check (const std::string& key) const;
  const Value& lookup (const std::string& key) const;
  void add (const std::string& key, const Value& value);
  void remove (const std::string& key);
  void clear ();
};    

bool
AttributeList::Implementation::check (const std::string& key) const
{ 
  return values.find (key) != values.end ();
}

const Value& 
AttributeList::Implementation::lookup (const std::string& key) const
{ 
  value_map::const_iterator i = values.find (key);
  
  if (i == values.end ())
    daisy_panic (std::string ("AList: Missing key '") + key + "'");

  return (*i).second;
}

void
AttributeList::Implementation::add (const std::string& key, const Value& value)
{
  values[key] = value;
}

void
AttributeList::Implementation::remove (const std::string& key)
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
AttributeList::check (const std::string& key) const
{ 
  return impl.check (key) && impl.lookup (key).type != Syntax::Object; 
}

bool
AttributeList::subset (const Metalib& metalib, 
                       const AttributeList& other, const Syntax& syntax) const
{ 
  // Find syntax entries.
  std::vector<std::string> entries;
  syntax.entries (entries);
  const unsigned int size = entries.size ();

  // Loop over them.
  for (unsigned int i = 0; i < size; i++)
    {
      const std::string& key = entries[i];
      if (!subset (metalib, other, syntax, key))
	return false;
    }
  return true;
}

bool 
AttributeList::subset (const Metalib& metalib, 
                       const AttributeList& other, const Syntax& syntax,
		       const std::string& key) const
{
  if (check (key))
    {
      if (other.check (key))
	{
	  if (!impl.values[key].subset (metalib, 
                                        other.impl.values[key], syntax, key))
	    return false;
	}
      else
	return false;
    }
  return true;
}

int
AttributeList::size (const std::string& key)	const
{
  const Value& value = impl.lookup (key);

  if (!value.is_sequence)
    return (value.type == Syntax::Object) ? -1 : Syntax::Singleton;
  switch (value.type)
    {
    case Syntax::Number:
      return value.number_sequence->size ();
    case Syntax::AList:
      return value.alist_sequence->size ();
    case Syntax::PLF:
      return value.plf_sequence->size ();
    case Syntax::Boolean:
      return value.flag_sequence->size ();
    case Syntax::String:
      return value.name_sequence->size ();
    case Syntax::Integer:
      return value.integer_sequence->size ();
    case Syntax::Object:
      return -1;
    case Syntax::Library:
    case Syntax::Error:
    default:
      daisy_notreached ();
    }
  // Not reached.
}

  // Variables.
void 
AttributeList::add_reference (const std::string& key, const std::string& v)
{ impl.add (key, Value (v, -1)); }

bool
AttributeList::is_reference (const std::string& key) const
{ return impl.check (key) && impl.lookup (key).type == Syntax::Object; }
  
const std::string& 
AttributeList::get_reference (const std::string& key) const
{
  daisy_assert (is_reference (key));
  const Value& value = impl.lookup (key);
  value.expect (key, Syntax::Object);
  return (*value.name).name (); 
}

double 
AttributeList::number (const std::string& key) const
{
  const Value& value = impl.lookup (key);
  value.singleton (key);
  if (value.type == Syntax::Library)
    return value.scalar->number;
  value.expect (key, Syntax::Number);
  return value.number;
}

double 
AttributeList::number (const std::string& key, const double default_value) const
{
  if (!check (key))
    return default_value;

  return number (key);
}

const std::string& 
AttributeList::name (const std::string& key) const
{ return identifier (key).name (); }

const std::string& 
AttributeList::name (const std::string& key, const std::string& default_value) const
{
  if (!check (key))
    return default_value;
  return identifier (key).name (); 
}

symbol
AttributeList::identifier (const std::string& key) const
{
  const Value& value = impl.lookup (key);
  value.singleton (key);
  if (value.type == Syntax::Library)
    return value.scalar->name;
  value.expect (key, Syntax::String);
  return *value.name;
}

bool 
AttributeList::flag (const std::string& key) const
{
  const Value& value = impl.lookup (key);
  value.expect (key, Syntax::Boolean);
  value.singleton (key);
  return value.flag;
}

bool
AttributeList::flag (const std::string& key, const bool default_value) const
{
  if (!check (key))
    return default_value;
  return flag (key); 
}

int
AttributeList::integer (const std::string& key) const
{
  const Value& value = impl.lookup (key);
  value.expect (key, Syntax::Integer);
  value.singleton (key);
  return value.integer;
}

int
AttributeList::integer (const std::string& key, const int default_value) const
{
  if (!check (key))
    return default_value;
  return integer (key); 
}

const PLF& 
AttributeList::plf (const std::string& key) const
{
  const Value& value = impl.lookup (key);
  value.expect (key, Syntax::PLF);
  value.singleton (key);
  return *value.plf;
}

AttributeList& 
AttributeList::alist (const std::string& key) const
{
  const Value& value = impl.lookup (key);
  value.expect (key, Syntax::AList);
  value.singleton (key);
  return *value.alist;
}

const std::vector<double>& 
AttributeList::number_sequence (const std::string& key) const
{
  const Value& value = impl.lookup (key);
  value.expect (key, Syntax::Number);
  value.sequence (key);
  return *value.number_sequence;
}

const std::vector<symbol>&
AttributeList::identifier_sequence (const std::string& key) const
{
  const Value& value = impl.lookup (key);
  value.expect (key, Syntax::String);
  value.sequence (key);
  return *value.name_sequence;
}

std::vector<std::string>
AttributeList::name_sequence (const std::string& key) const
{
  const std::vector<symbol>& v = identifier_sequence (key);
  std::vector<std::string> result;
  for (size_t i = 0; i < v.size (); i++)
    result.push_back (v[i].name ());
  return result;
}

const std::vector<bool>& 
AttributeList::flag_sequence (const std::string& key) const
{
  const Value& value = impl.lookup (key);
  value.expect (key, Syntax::Boolean);
  value.sequence (key);
  return *value.flag_sequence;
}

const std::vector<int>& 
AttributeList::integer_sequence (const std::string& key) const
{
  const Value& value = impl.lookup (key);
  value.expect (key, Syntax::Integer);
  value.sequence (key);
  return *value.integer_sequence;
}

const std::vector<const PLF*>& 
AttributeList::plf_sequence (const std::string& key) const
{
  const Value& value = impl.lookup (key);
  value.expect (key, Syntax::PLF);
  value.sequence (key);
  return *value.plf_sequence;
}

const std::vector<AttributeList*>& 
AttributeList::alist_sequence (const std::string& key) const
{
  const Value& value = impl.lookup (key);
  value.expect (key, Syntax::AList);
  value.sequence (key);
  return *value.alist_sequence;
}

void 
AttributeList::add (const std::string& key, double v)
{ impl.add (key, Value (v)); }

void
AttributeList::add (const std::string& key, double v, const std::string& d)
{ impl.add (key, Value (v, d)); }

void 
AttributeList::add (const std::string& key, const char *const v)
{ impl.add (key, Value (v)); }

void 
AttributeList::add (const std::string& key, const std::string& v)
{ impl.add (key, Value (v)); }

void 
AttributeList::add (const std::string& key, const symbol sym)
{ add (key, sym.name ()); }

void 
AttributeList::add (const std::string& key, bool v)
{ impl.add (key, Value (v)); }

void 
AttributeList::add (const std::string& key, int v)
{ impl.add (key, Value (v)); }

void 
AttributeList::add (const std::string& key, const AttributeList& v)
{ impl.add (key, Value (v)); }

void 
AttributeList::add (const std::string& key, const PLF& v)
{ impl.add (key, Value (v)); }

void 
AttributeList::add (const std::string& key, const std::vector<double>& v)
{ impl.add (key, Value (v)); }

void 
AttributeList::add (const std::string& key, const std::vector<symbol>& v)
{ impl.add (key, Value (v)); }

void 
AttributeList::add (const std::string& key, const std::vector<bool>& v)
{ impl.add (key, Value (v)); }

void 
AttributeList::add (const std::string& key, const std::vector<int>& v)
{ impl.add (key, Value (v)); }

void 
AttributeList::add (const std::string& key, const std::vector<AttributeList*>& v)
{ impl.add (key, Value (v)); }

void 
AttributeList::add (const std::string& key, const std::vector<const PLF*>& v)
{ impl.add (key, Value (v)); }

void 
AttributeList::remove (const std::string& key)
{ impl.remove (key); }

bool
AttributeList::revert (const Metalib& metalib,
                       const std::string& key, 
		       const AttributeList& default_alist, 
		       const Syntax& syntax)
{
  if (subset (metalib, default_alist, syntax, key))
    return false;

  if (!default_alist.check (key))
    {
      daisy_assert (check (key));
      remove (key);
      return true;
    }

  if (syntax.size (key) == Syntax::Singleton)
    switch (syntax.lookup (key))
      {
      case Syntax::Number:
        if (syntax.dimension (key) == Syntax::User ())
          add (key, default_alist.number (key), default_alist.name (key));
        else 
          add (key, default_alist.number (key));
        break;
      case Syntax::Boolean:
	add (key, default_alist.flag (key));
        break;
      case Syntax::Integer:
	add (key, default_alist.integer (key));
        break;
      case Syntax::AList:
      case Syntax::Object:
        add (key, default_alist.alist (key));
        break;
      case Syntax::PLF:
	add (key, default_alist.plf (key));
        break;
      case Syntax::String:
	add (key, default_alist.name (key));
        break;
      case Syntax::Library:
      case Syntax::Error:
      default:
	daisy_notreached ();
      }
  else
    switch (syntax.lookup (key))
      {
      case Syntax::Number:
	add (key, default_alist.number_sequence (key));
        break;
      case Syntax::Boolean:
	add (key, default_alist.flag_sequence (key));
        break;
      case Syntax::Integer:
	add (key, default_alist.integer_sequence (key));
        break;
      case Syntax::AList:
      case Syntax::Object:
        add (key, default_alist.alist_sequence (key));
        break;
      case Syntax::PLF:
	add (key, default_alist.plf_sequence (key));
        break;
      case Syntax::String:
	add (key, default_alist.identifier_sequence (key));
        break;
      case Syntax::Library:
      case Syntax::Error:
      default:
	daisy_notreached ();
      }
  return true;
}

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
