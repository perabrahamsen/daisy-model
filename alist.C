// alist.C

#include "csmp.h"
#include "library.h"
#include "alist.h"
#include "syntax.h"
#include "time.h"
#include "common.h"
#include <assert.h>
#include <map>

// @ Value
//
// Common abstraction of an attribute value.

struct Value
{
  union
  {
    double number;
    string* name;
    bool flag;
    CSMP* csmp;
    AttributeList* alist;
    int integer;
    Time* time;
    vector<double>* number_sequence;
    vector<string>* name_sequence;
    vector<bool>* flag_sequence;
    vector<int>* integer_sequence;
    vector<const Time*>* time_sequence;
    vector<const CSMP*>* csmp_sequence;
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
  Value (const string& v)
    : name (new string (v)),
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
  Value (const CSMP& v)
    : csmp (new CSMP (v)),
      type (Syntax::CSMP),
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
  Value (const Time& v)
    : time (new Time (v)),
      type (Syntax::Date),
      is_sequence (false),
      ref_count (new int (1))
    { }
  Value (const vector<double>& v)
    : number_sequence (new vector<double> (v)),
      type (Syntax::Number),
      is_sequence (true),
      ref_count (new int (1))
    { }
  Value (const vector<string>& v)
    : name_sequence (new vector<string> (v)),
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
  Value (const vector<const Time*>& v)
    : time_sequence (new vector<const Time*> (v)),
      type (Syntax::Date),
      is_sequence (true),
      ref_count (new int (1))
    { }
  Value (const vector<const CSMP*>& v)
    : csmp_sequence (new vector<const CSMP*> (v)),
      type (Syntax::CSMP),
      is_sequence (true),
      ref_count (new int (1))
    { }
  Value (const vector<AttributeList*>& v)
    : alist_sequence (new vector<AttributeList*> (v)),
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
  assert (type == v.type);
  assert (is_sequence == v.is_sequence);

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
	  assert (type == Syntax::Object);
	  const Library& library = syntax.library (key);

	  assert (value.check ("type"));
	  if (!other.check ("type"))
	    return false;
	  const string element = value.name ("type");
	  if (element != other.name ("type"))
	    return false;
	  if (!library.check (element))
	    return false;
	  return value.subset (other, library.syntax (element));
	}
      case Syntax::CSMP:
	return *csmp == *v.csmp;
      case Syntax::String:
	return *name == *v.name;
      case Syntax::Date:
	return *time == *v.time;
      case Syntax::Object:
      case Syntax::Library:
      case Syntax::Error:
      default:
	assert (false);
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
	  assert (type == Syntax::Object);
	  const Library& library = syntax.library (key);
	  for (unsigned int i = 0; i < size; i++)
	    {
	      assert (value[i]->check ("type"));
	      if (!other[i]->check ("type"))
		return false;
	      const string element = value[i]->name ("type");
	      if (element != other[i]->name ("type"))
		return false;
	      if (!library.check (element))
		return false;
	      if (!value[i]->subset (*other[i], library.syntax (element)))
		return false;
	    }
	  return true;
	}
      case Syntax::CSMP:
	return *csmp_sequence == *v.csmp_sequence;
      case Syntax::String:
	return *name_sequence == *v.name_sequence;
      case Syntax::Date:
	return *time_sequence == *v.time_sequence;
      case Syntax::Object:
      case Syntax::Library:
      case Syntax::Error:
      default:
	assert (false);
      }
  // Not reached.
  return false;
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
	  case Syntax::CSMP:
	    delete csmp;
	    break;
	  case Syntax::String:
	    delete name;
	    break;
	  case Syntax::Date:
	    delete time;
	    break;
	  case Syntax::Error:
	    // Empty (dummy) value.
	    break;
	  case Syntax::Object:
	  case Syntax::Library:
	  default:
	    assert (false);
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
	    // BUG: Should delete elements also.
	    delete alist_sequence;
	    break;
	  case Syntax::CSMP:
	    delete csmp_sequence;
	    break;
	  case Syntax::String:
	    delete name_sequence;
	    break;
	  case Syntax::Date:
	    delete time_sequence;
	    break;
	  case Syntax::Object:
	  case Syntax::Library:
	  case Syntax::Error:
	  default:
	    assert (false);
	  }
      break;
    default:
      assert (*ref_count > 1);
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
      case Syntax::CSMP:
	csmp = v.csmp;
        break;
      case Syntax::String:
	name = v.name;
        break;
      case Syntax::Date:
	time = v.time;
        break;
      case Syntax::Object:
      case Syntax::Library:
      case Syntax::Error:
      default:
	assert (false);
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
      case Syntax::CSMP:
	csmp_sequence = v.csmp_sequence;
        break;
      case Syntax::String:
	name_sequence = v.name_sequence;
        break;
      case Syntax::Date:
	time_sequence = v.time_sequence;
        break;
      case Syntax::Object:
      case Syntax::Library:
      case Syntax::Error:
      default:
	assert (false);
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
  
  assert (i != values.end ());
  return (*i).second;
}

void
AttributeList::Implementation::add (const string& key, const Value& value)
{
  values[key] = value;
}

bool
AttributeList::check (const string& key) const
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
    case Syntax::CSMP:
      return value.csmp_sequence->size ();
    case Syntax::Boolean:
      return value.flag_sequence->size ();
    case Syntax::String:
      return value.name_sequence->size ();
    case Syntax::Date:
      return value.time_sequence->size ();
    case Syntax::Integer:
      return value.integer_sequence->size ();
    case Syntax::Object:
      return value.alist_sequence->size ();
    case Syntax::Library:
    case Syntax::Error:
    default:
      assert (false);
    }
  // Not reached.
  return Syntax::Unspecified;
}

double 
AttributeList::number (string key) const
{
  const Value& value = impl.lookup (key);
  assert (value.type == Syntax::Number);
  assert (!value.is_sequence);
  return value.number;
}

const string& 
AttributeList::name (string key) const
{
  const Value& value = impl.lookup (key);
  assert (value.type == Syntax::String);
  assert (!value.is_sequence);
  return *value.name;
}

bool 
AttributeList::flag (string key) const
{
  const Value& value = impl.lookup (key);
  assert (value.type == Syntax::Boolean);
  assert (!value.is_sequence);
  return value.flag;
}

int
AttributeList::integer (string key) const
{
  const Value& value = impl.lookup (key);
  assert (value.type == Syntax::Integer);
  assert (!value.is_sequence);
  return value.integer;
}

const Time&
AttributeList::time (string key) const
{
  const Value& value = impl.lookup (key);
  assert (value.type == Syntax::Date);
  assert (!value.is_sequence);
  return *value.time;
}

const CSMP& 
AttributeList::csmp (string key) const
{
  const Value& value = impl.lookup (key);
  assert (value.type == Syntax::CSMP);
  assert (!value.is_sequence);
  return *value.csmp;
}

AttributeList& 
AttributeList::alist (string key) const
{
  const Value& value = impl.lookup (key);
  assert (value.type == Syntax::AList);
  assert (!value.is_sequence);
  return *value.alist;
}

const vector<double>& 
AttributeList::number_sequence (string key) const
{
  const Value& value = impl.lookup (key);
  assert (value.type == Syntax::Number);
  assert (value.is_sequence);
  return *value.number_sequence;
}

const vector<string>& 
AttributeList::name_sequence (string key) const
{
  const Value& value = impl.lookup (key);
  assert (value.type == Syntax::String);
  assert (value.is_sequence);
  return *value.name_sequence;
}

const vector<bool>& 
AttributeList::flag_sequence (string key) const
{
  const Value& value = impl.lookup (key);
  assert (value.type == Syntax::Boolean);
  assert (value.is_sequence);
  return *value.flag_sequence;
}

const vector<int>& 
AttributeList::integer_sequence (string key) const
{
  const Value& value = impl.lookup (key);
  assert (value.type == Syntax::Integer);
  assert (value.is_sequence);
  return *value.integer_sequence;
}

const vector<const Time*>& 
AttributeList::time_sequence (string key) const
{
  const Value& value = impl.lookup (key);
  assert (value.type == Syntax::Date);
  assert (value.is_sequence);
  return *value.time_sequence;
}

const vector<const CSMP*>& 
AttributeList::csmp_sequence (string key) const
{
  const Value& value = impl.lookup (key);
  assert (value.type == Syntax::CSMP);
  assert (value.is_sequence);
  return *value.csmp_sequence;
}

const vector<AttributeList*>& 
AttributeList::alist_sequence (string key) const
{
  const Value& value = impl.lookup (key);
  assert (value.type == Syntax::AList);
  assert (value.is_sequence);
  return *value.alist_sequence;
}

void 
AttributeList::add (const string& key, double v)
{ impl.add (key, Value (v)); }

void 
AttributeList::add (const string& key, const char* v)
{ add (key, string (v)); }

void 
AttributeList::add (const string& key, const string& v)
{ impl.add (key, Value (v)); }

void 
AttributeList::add (const string& key, bool v)
{ impl.add (key, Value (v)); }

void 
AttributeList::add (const string& key, int v)
{ impl.add (key, Value (v)); }

void 
AttributeList::add (const string& key, const Time& v)
{ impl.add (key, Value (v)); }

void 
AttributeList::add (const string& key, AttributeList& v)
{ impl.add (key, Value (v)); }

void 
AttributeList::add (const string& key, const CSMP& v)
{ impl.add (key, Value (v)); }

void 
AttributeList::add (const string& key, const vector<double>& v)
{ impl.add (key, Value (v)); }

void 
AttributeList::add (const string& key, const vector<string>& v)
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
AttributeList::add (const string& key, const vector<const CSMP*>& v)
{ impl.add (key, Value (v)); }

void 
AttributeList::add (const string& key, const vector<const Time*>& v)
{ impl.add (key, Value (v)); }

void
AttributeList::operator += (const AttributeList& al)
{
  for (value_map::const_iterator i = al.impl.values.begin ();
       i != al.impl.values.end ();
       i++)
    impl.add ((*i).first, (*i).second);
}

AttributeList::AttributeList ()
  : impl (*new Implementation ())
{ }

AttributeList::AttributeList (const AttributeList& old)
  : impl (*new Implementation ())
{ impl.values = old.impl.values; }

AttributeList::~AttributeList ()
{ delete &impl; }

const AttributeList AttributeList::empty;
