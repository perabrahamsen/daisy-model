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


#include "plf.h"
#include "library.h"
#include "alist.h"
#include "syntax.h"
#include "time.h"
#include "common.h"
#include "assertion.h"
#include <map>

// @ Value
//
// Common abstraction of an attribute value.



struct Value
{
  union
  {
    double number;
    symbol* name;
    bool flag;
    PLF* plf;
    AttributeList* alist;
    int integer;
    vector<double>* number_sequence;
    vector<symbol>* name_sequence;
    vector<bool>* flag_sequence;
    vector<int>* integer_sequence;
    vector<const PLF*>* plf_sequence;
    vector<AttributeList*>* alist_sequence;
  };
  Syntax::type type;
  bool is_sequence;
  int* ref_count;

  bool subset (const Value& other, const Syntax&, const string& key) const;

  Value (double v)
    : number (v),
      type (Syntax::Number),
      is_sequence (false),
      ref_count (new int (1))
    { }
  Value (const symbol v)
    : name (new symbol (v)),
      type (Syntax::String),
      is_sequence (false),
      ref_count (new int (1))
  { }
  Value (const string& v)
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
  Value (const vector<double>& v)
    : number_sequence (new vector<double> (v)),
      type (Syntax::Number),
      is_sequence (true),
      ref_count (new int (1))
    { }
  Value (const vector<symbol>& v)
    : name_sequence (new vector<symbol> (v)),
      type (Syntax::String),
      is_sequence (true),
      ref_count (new int (1))
    { }
  static vector<symbol>* new_symbol_vector (const vector<string>& org)
  {
    vector<symbol>* copy = new vector<symbol> ();
    for (unsigned int i = 0; i < org.size (); i++)
      copy->push_back (symbol (org[i]));
    return copy;
  }
  Value (const vector<string>& v)
    : name_sequence (new_symbol_vector (v)),
      type (Syntax::String),
      is_sequence (true),
      ref_count (new int (1))
    { }
  Value (const vector<bool>& v)
    : flag_sequence (new vector<bool> (v)),
      type (Syntax::Boolean),
      is_sequence (true),
      ref_count (new int (1))
    { }
  Value (const vector<int>& v)
    : integer_sequence (new vector<int> (v)),
      type (Syntax::Integer),
      is_sequence (true),
      ref_count (new int (1))
    { }
  static vector<const PLF*>* copy_plfs (const vector<const PLF*>& org)
  {
    vector<const PLF*>* copy = new vector<const PLF*> ();
    for (unsigned int i = 0; i < org.size (); i++)
      copy->push_back (new PLF (*org[i]));
    return copy;
  }
  Value (const vector<const PLF*>& v)
    : plf_sequence (copy_plfs (v)),
      type (Syntax::PLF),
      is_sequence (true),
      ref_count (new int (1))
    { }
  vector<AttributeList*>* copy_alists (const vector<AttributeList*>& org)
  {
    vector<AttributeList*>* copy = new vector<AttributeList*> ();
    for (unsigned int i = 0; i < org.size (); i++)
      copy->push_back (new AttributeList (*org[i]));
    return copy;
  }
  Value (const vector<AttributeList*>& v)
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
Value::subset (const Value& v, const Syntax& syntax, 
	       const string& key) const
{
  daisy_assert (type == v.type);
  daisy_assert (is_sequence == v.is_sequence);

  if (!is_sequence)
    switch (type)
      {
      case Syntax::Number:
	return number == v.number;
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
	    return value.subset (other, syntax.syntax (key));
	  daisy_assert (type == Syntax::Object);
	  const Library& library = syntax.library (key);

	  daisy_assert (value.check ("type"));
	  if (!other.check ("type"))
	    return false;
	  const symbol element = value.identifier ("type");
	  if (element != other.identifier ("type"))
	    return false;
	  if (!library.check (element))
	    return false;
	  return value.subset (other, library.syntax (element));
	}
      case Syntax::PLF:
	return *plf == *v.plf;
      case Syntax::String:
	return *name == *v.name;
      case Syntax::Object:
      case Syntax::Library:
      case Syntax::Error:
      default:
	daisy_assert (false);
      }
  else
    switch (type)
      {
      case Syntax::Number:
	return *number_sequence == *v.number_sequence;
      case Syntax::Boolean:
	return *flag_sequence == *v.flag_sequence;
      case Syntax::Integer:
	return *integer_sequence == *v.integer_sequence;
      case Syntax::AList:
	{
	  const vector<AttributeList*>& value = *alist_sequence;
	  const vector<AttributeList*>& other = *v.alist_sequence;

	  const unsigned int size = value.size ();
	  if (other.size () != size)
	    return false;
	  const Syntax::type type = syntax.lookup (key);
	  if (type == Syntax::AList)
	    {
	      const Syntax& nested = syntax.syntax (key);
	      for (unsigned int i = 0; i < size; i++)
		if (!value[i]->subset (*other[i], nested))
		  return false;
	      return true;
		
	    }
	  daisy_assert (type == Syntax::Object);
	  const Library& library = syntax.library (key);
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
	      if (!value[i]->subset (*other[i], library.syntax (element)))
		return false;
	    }
	  return true;
	}
      case Syntax::PLF:
	return *plf_sequence == *v.plf_sequence;
      case Syntax::String:
	return *name_sequence == *v.name_sequence;
      case Syntax::Object:
      case Syntax::Library:
      case Syntax::Error:
      default:
	daisy_assert (false);
      }
  // Not reached.
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
	  case Syntax::String:
	    delete name;
	    break;
	  case Syntax::Error:
	    // Empty (dummy) value.
	    break;
	  case Syntax::Object:
	  case Syntax::Library:
	  default:
	    daisy_assert (false);
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
	  case Syntax::Library:
	  case Syntax::Error:
	  default:
	    daisy_assert (false);
	  }
      break;
    default:
      daisy_assert (*ref_count > 1);
      (*ref_count)--;
    }
}

Value& 
Value::operator = (const Value& v)
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
      case Syntax::String:
	name = v.name;
        break;
      case Syntax::Object:
      case Syntax::Library:
      case Syntax::Error:
      default:
	daisy_assert (false);
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
      case Syntax::Library:
      case Syntax::Error:
      default:
	daisy_assert (false);
      }

  // Return this for further mutilation.
  return *this;
}

// Specific attribute values.

// @ AttributeList

typedef map <string, Value, less<string> > value_map;

struct AttributeList::Implementation
{
  value_map values;
  bool check (const string& key) const;
  const Value& lookup (const string& key) const;
  void add (const string& key, const Value& value);
  void remove (const string& key);
  void clear ();
};    

bool
AttributeList::Implementation::check (const string& key) const
{ 
  return values.find (key) != values.end ();
}

const Value& 
AttributeList::Implementation::lookup (const string& key) const
{ 
  value_map::const_iterator i = values.find (key);
  
  if (i == values.end ())
    throw (string ("AList: Missing key '") + key + "'");

  return (*i).second;
}

void
AttributeList::Implementation::add (const string& key, const Value& value)
{
  values[key] = value;
}

void
AttributeList::Implementation::remove (const string& key)
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
AttributeList::check (const string& key) const
{ 
  return impl.check (key);
}

bool
AttributeList::check (const char *const key) const
{ 
  return impl.check (key);
}

bool
AttributeList::subset (const AttributeList& other, const Syntax& syntax) const
{ 
  // Find syntax entries.
  vector<string> entries;
  syntax.entries (entries);
  const unsigned int size = entries.size ();

  // Loop over them.
  for (unsigned int i = 0; i < size; i++)
    {
      const string& key = entries[i];
      if (!subset (other, syntax, key))
	return false;
    }
  return true;
}

bool 
AttributeList::subset (const AttributeList& other, const Syntax& syntax,
		       const string& key) const
{
  if (check (key))
    {
      if (other.check (key))
	{
	  if (!impl.values[key].subset (other.impl.values[key], syntax, key))
	    return false;
	}
      else
	return false;
    }
  return true;
}

int
AttributeList::size (const string& key)	const
{
  const Value& value = impl.lookup (key);

  if (!value.is_sequence)
    return Syntax::Singleton;
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
      return value.alist_sequence->size ();
    case Syntax::Library:
    case Syntax::Error:
    default:
      daisy_assert (false);
    }
  // Not reached.
}

double 
AttributeList::number (const string& key) const
{
  const Value& value = impl.lookup (key);
  daisy_assert (value.type == Syntax::Number);
  daisy_assert (!value.is_sequence);
  return value.number;
}

double 
AttributeList::number (const char *const key) const
{
  const Value& value = impl.lookup (key);
  daisy_assert (value.type == Syntax::Number);
  daisy_assert (!value.is_sequence);
  return value.number;
}

double 
AttributeList::number (const string& key, const double default_value) const
{
  if (!check (key))
    return default_value;

  const Value& value = impl.lookup (key);
  daisy_assert (value.type == Syntax::Number);
  daisy_assert (!value.is_sequence);
  return value.number;
}

double 
AttributeList::number (const char *const key, const double default_value) const
{
  if (!check (key))
    return default_value;

  const Value& value = impl.lookup (key);
  daisy_assert (value.type == Syntax::Number);
  daisy_assert (!value.is_sequence);
  return value.number;
}

const string& 
AttributeList::name (const string& key) const
{ return identifier (key).name (); }

const string& 
AttributeList::name (const char *const key) const
{ return identifier (key).name (); }

symbol
AttributeList::identifier (const string& key) const
{
  const Value& value = impl.lookup (key);
  daisy_assert (value.type == Syntax::String);
  daisy_assert (!value.is_sequence);
  return *value.name;
}

symbol
AttributeList::identifier (const char *const key) const
{
  const Value& value = impl.lookup (key);
  daisy_assert (value.type == Syntax::String);
  daisy_assert (!value.is_sequence);
  return *value.name;
}

bool 
AttributeList::flag (const string& key) const
{
  const Value& value = impl.lookup (key);
  daisy_assert (value.type == Syntax::Boolean);
  daisy_assert (!value.is_sequence);
  return value.flag;
}

bool 
AttributeList::flag (const char *const key) const
{
  const Value& value = impl.lookup (key);
  daisy_assert (value.type == Syntax::Boolean);
  daisy_assert (!value.is_sequence);
  return value.flag;
}

int
AttributeList::integer (const string& key) const
{
  const Value& value = impl.lookup (key);
  daisy_assert (value.type == Syntax::Integer);
  daisy_assert (!value.is_sequence);
  return value.integer;
}

int
AttributeList::integer (const char *const key) const
{
  const Value& value = impl.lookup (key);
  daisy_assert (value.type == Syntax::Integer);
  daisy_assert (!value.is_sequence);
  return value.integer;
}

const PLF& 
AttributeList::plf (const string& key) const
{
  const Value& value = impl.lookup (key);
  daisy_assert (value.type == Syntax::PLF);
  daisy_assert (!value.is_sequence);
  return *value.plf;
}

const PLF& 
AttributeList::plf (const char *const key) const
{
  const Value& value = impl.lookup (key);
  daisy_assert (value.type == Syntax::PLF);
  daisy_assert (!value.is_sequence);
  return *value.plf;
}

AttributeList& 
AttributeList::alist (const string& key) const
{
  const Value& value = impl.lookup (key);
  daisy_assert (value.type == Syntax::AList);
  daisy_assert (!value.is_sequence);
  return *value.alist;
}

AttributeList& 
AttributeList::alist (const char *const key) const
{
  const Value& value = impl.lookup (key);
  daisy_assert (value.type == Syntax::AList);
  daisy_assert (!value.is_sequence);
  return *value.alist;
}

const vector<double>& 
AttributeList::number_sequence (const string& key) const
{
  const Value& value = impl.lookup (key);
  daisy_assert (value.type == Syntax::Number);
  daisy_assert (value.is_sequence);
  return *value.number_sequence;
}

const vector<double>& 
AttributeList::number_sequence (const char *const key) const
{
  const Value& value = impl.lookup (key);
  daisy_assert (value.type == Syntax::Number);
  daisy_assert (value.is_sequence);
  return *value.number_sequence;
}

const vector<symbol>&
AttributeList::identifier_sequence (const string& key) const
{
  const Value& value = impl.lookup (key);
  daisy_assert (value.type == Syntax::String);
  daisy_assert (value.is_sequence);
  return *value.name_sequence;
}

const vector<symbol>&
AttributeList::identifier_sequence (const char *const key) const
{
  const Value& value = impl.lookup (key);
  daisy_assert (value.type == Syntax::String);
  daisy_assert (value.is_sequence);
  return *value.name_sequence;
}

const vector<bool>& 
AttributeList::flag_sequence (const string& key) const
{
  const Value& value = impl.lookup (key);
  daisy_assert (value.type == Syntax::Boolean);
  daisy_assert (value.is_sequence);
  return *value.flag_sequence;
}

const vector<bool>& 
AttributeList::flag_sequence (const char *const key) const
{
  const Value& value = impl.lookup (key);
  daisy_assert (value.type == Syntax::Boolean);
  daisy_assert (value.is_sequence);
  return *value.flag_sequence;
}

const vector<int>& 
AttributeList::integer_sequence (const string& key) const
{
  const Value& value = impl.lookup (key);
  daisy_assert (value.type == Syntax::Integer);
  daisy_assert (value.is_sequence);
  return *value.integer_sequence;
}

const vector<int>& 
AttributeList::integer_sequence (const char *const key) const
{
  const Value& value = impl.lookup (key);
  daisy_assert (value.type == Syntax::Integer);
  daisy_assert (value.is_sequence);
  return *value.integer_sequence;
}

const vector<const PLF*>& 
AttributeList::plf_sequence (const string& key) const
{
  const Value& value = impl.lookup (key);
  daisy_assert (value.type == Syntax::PLF);
  daisy_assert (value.is_sequence);
  return *value.plf_sequence;
}

const vector<const PLF*>& 
AttributeList::plf_sequence (const char *const key) const
{
  const Value& value = impl.lookup (key);
  daisy_assert (value.type == Syntax::PLF);
  daisy_assert (value.is_sequence);
  return *value.plf_sequence;
}

const vector<AttributeList*>& 
AttributeList::alist_sequence (const string& key) const
{
  const Value& value = impl.lookup (key);
  daisy_assert (value.type == Syntax::AList);
  daisy_assert (value.is_sequence);
  return *value.alist_sequence;
}

const vector<AttributeList*>& 
AttributeList::alist_sequence (const char *const key) const
{
  const Value& value = impl.lookup (key);
  daisy_assert (value.type == Syntax::AList);
  daisy_assert (value.is_sequence);
  return *value.alist_sequence;
}

void 
AttributeList::add (const string& key, double v)
{ impl.add (key, Value (v)); }

void 
AttributeList::add (const string& key, const char *const v)
{ impl.add (key, Value (v)); }

void 
AttributeList::add (const string& key, const string& v)
{ impl.add (key, Value (v)); }

void 
AttributeList::add (const string& key, const symbol sym)
{ add (key, sym.name ()); }

void 
AttributeList::add (const string& key, bool v)
{ impl.add (key, Value (v)); }

void 
AttributeList::add (const string& key, int v)
{ impl.add (key, Value (v)); }

void 
AttributeList::add (const string& key, const AttributeList& v)
{ impl.add (key, Value (v)); }

void 
AttributeList::add (const string& key, const PLF& v)
{ impl.add (key, Value (v)); }

void 
AttributeList::add (const string& key, const vector<double>& v)
{ impl.add (key, Value (v)); }

void 
AttributeList::add (const string& key, const vector<symbol>& v)
{ impl.add (key, Value (v)); }

void 
AttributeList::add (const string& key, const vector<bool>& v)
{ impl.add (key, Value (v)); }

void 
AttributeList::add (const string& key, const vector<int>& v)
{ impl.add (key, Value (v)); }

void 
AttributeList::add (const string& key, const vector<AttributeList*>& v)
{ impl.add (key, Value (v)); }

void 
AttributeList::add (const string& key, const vector<const PLF*>& v)
{ impl.add (key, Value (v)); }

void 
AttributeList::remove (const string& key)
{ impl.remove (key); }

bool
AttributeList::revert (const string& key, 
		       const AttributeList& default_alist, 
		       const Syntax& syntax)
{
  if (subset (default_alist, syntax, key))
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
	daisy_assert (false);
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
	daisy_assert (false);
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
