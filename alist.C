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

class Value
{
public:
  // BAD KLUDGE.
  virtual bool is_alist_sequence () const
    { return false; }

  // Retrieve data (Singletons).
  virtual operator double () const
       throw1 (AttributeList::Invalid)
    { THROW (AttributeList::Invalid ()); }
  virtual operator const string& () const
       throw1 (AttributeList::Invalid)
    { THROW (AttributeList::Invalid ()); }
  virtual operator bool () const
       throw1 (AttributeList::Invalid)
    { THROW (AttributeList::Invalid ()); }
  virtual operator int () const
       throw1 (AttributeList::Invalid)
    { THROW (AttributeList::Invalid ()); }
  virtual operator const CSMP& () const
       throw1 (AttributeList::Invalid)
    { THROW (AttributeList::Invalid ()); }
  virtual operator AttributeList& () const
       throw1 (AttributeList::Invalid)
    { THROW (AttributeList::Invalid ()); }
  virtual operator const Time& () const throw1 (AttributeList::Invalid)
    { THROW (AttributeList::Invalid ()); }

  // Retrieve data (Sequences).
  virtual operator const vector<double>& () const
       throw1 (AttributeList::Invalid)
    { THROW (AttributeList::Invalid ()); }
  virtual operator const vector<string>& () const
       throw1 (AttributeList::Invalid)
    { THROW (AttributeList::Invalid ()); }
  virtual operator const vector<bool>& () const
       throw1 (AttributeList::Invalid)
    { THROW (AttributeList::Invalid ()); }
  virtual operator const vector<int>& () const
       throw1 (AttributeList::Invalid)
    { THROW (AttributeList::Invalid ()); }
  virtual operator const vector<const CSMP*>& () const
       throw1 (AttributeList::Invalid)
    { THROW (AttributeList::Invalid ()); }
  virtual operator const vector<AttributeList*>& () const
       throw1 (AttributeList::Invalid)
    { THROW (AttributeList::Invalid ()); }
  virtual operator const vector<const Time*>& () const
       throw1 (AttributeList::Invalid)
    { THROW (AttributeList::Invalid ()); }

  // Compare data.
  virtual bool subset (const Value&, const Syntax&, const string&) const = 0;

protected:
  Value ()
    { }
public:
  virtual ~Value ()
    { }
};

// Specific attribute values.

// We store most values as references.
template <class T>
class dValue : public Value
{
  const T& value;
public:
  operator const T& () const throw1 (AttributeList::Invalid)
  { return value; }
  bool subset (const Value& v, const Syntax&, const string&) const
    { return value == T (v); }
  dValue (const T& v)
    : value (v)
  { };
};

// Primitive and simple types are stored directly, though.
class dValue<double> : public Value
{
  double value;
public:
  operator double () const throw1 (AttributeList::Invalid)
  { return value; }
  bool subset (const Value& v, const Syntax&, const string&) const
    { return value == double (v); }
  dValue (double v)
    : value (v)
  { };
};

class dValue<int> : public Value
{
  int value;
public:
  operator int () const
    throw1 (AttributeList::Invalid)
  { return value; }
  bool subset (const Value& v, const Syntax&, const string&) const
    { return value == int (v); }
  dValue (int v)
    : value (v)
  { };
};

class dValue<bool> : public Value
{
  bool value;
public:
  operator bool () const throw1 (AttributeList::Invalid)
  { return value; }
  bool subset (const Value& v, const Syntax&, const string&) const
    { return value == bool (v); }
  dValue (bool v)
    : value (v)
  { };
};

class dValue<string> : public Value
{
  string value;
public:
  operator const string& () const throw1 (AttributeList::Invalid)
  { return value; }
  bool subset (const Value& v, const Syntax&, const string&) const
    { return value == string (v); }
  dValue (const string& v)
    : value (v)
  { };
};

class dValue<Time> : public Value
{
  const Time& value;
public:
  operator const Time& () const throw1 (AttributeList::Invalid)
  { return value; }
  bool subset (const Value& v, const Syntax&, const string&) const
    { return v == value; }
  dValue (const Time& v)
    : value (*new Time (v))
  { };
};

class dValue<AttributeList> : public Value
{
  AttributeList& value;
public:
  operator AttributeList& () const throw1 (AttributeList::Invalid)
  { return value; }
  bool subset (const Value& v, const Syntax& syntax, const string& key) const
    {
      const AttributeList& other = v;
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
  dValue (AttributeList& v)
    : value (v)
  { };
};

// Sequences of Attribute Lists and numbers can be dumped.
class dValue<vector<double>/**/> : public Value
{
  const vector<double> value;
public:
  operator const vector<double>& () const throw1 (AttributeList::Invalid)
  { return value; }
  bool subset (const Value& v, const Syntax&, 
	      const string&) const
    { return value == vector<double> (v); }
  dValue (const vector<double>& v)
    : value (v)
  { };
};

class dValue<vector<AttributeList*>/**/> : public Value
{
  const vector<AttributeList*> value;
public:
  operator const vector<AttributeList*>& () const
    throw1 (AttributeList::Invalid)
  { return value; }
  // BAD KLUDGE.
  bool is_alist_sequence () const
    { return true; }
  bool subset (const Value& v , const Syntax& syntax, const string& key) const
    {
      // Design Bug: `add_submodule' adds an alist instead of an
      // alist_sequence. 
      if (!v.is_alist_sequence ())
	return false;

      const vector<AttributeList*>& other = v;


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
  dValue (const vector<AttributeList*>& v)
    : value (v)
  { };
};

// @ AttributeList

typedef map <string, Value*, less<string> > value_map;

struct AttributeList::Implementation
{
  value_map values;
  bool check (const string& key) const throw0 ();
  Value* lookup (const string& key) const throw1 (Uninitialized);
  void add (const string& key, Value* value);
};    

bool
AttributeList::Implementation::check (const string& key) const throw0 ()
{ 
  return values.find (key) != values.end ();
}

Value* 
AttributeList::Implementation::lookup (const string& key) const throw1 (AttributeList::Uninitialized)
{ 
  value_map::const_iterator i = values.find (key);
  
  if (i != values.end ())
    return (*i).second;
  else
    THROW (Uninitialized ());
}

void
AttributeList::Implementation::add (const string& key, Value* value)
{
  value_map::iterator i = values.find (key);

  if (i == values.end ())
    values[key] = value;
  else
    {
      // BUG: Memory leak: We can't delete the old value, as it might
      // be shared with another alist.  Use reference counting to solve.
#if 0
      delete (*i).second;
#endif
      (*i).second = value;
    }
}

bool
AttributeList::check (const string& key) const throw0 ()
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
      // Design Bug: `add_submodule' adds an alist instead of an
      // alist_sequence. 
      if (syntax.lookup (key) == Syntax::AList
	  && syntax.size (key) != Syntax::Singleton
	  && !impl.values[key]->is_alist_sequence ())
	// This has not been overwritten from the default value.
	return true;

      if (other.check (key))
	{
	  if (!impl.values[key]->subset (*other.impl.values[key],
					 syntax, key))
	    return false;
	}
      else
	return false;
    }
  return true;
}

int
AttributeList::size (const string& key)	const // BUG: UNIMPLEMENTED.
{
  if (impl.values[key]->is_alist_sequence ())
    return Syntax::Sequence;
  
  return Syntax::Singleton;
}

double 
AttributeList::number (string key) const throw2 (AttributeList::Invalid, AttributeList::Uninitialized)
{
  return *impl.lookup (key);
}

const string& 
AttributeList::name (string key) const throw2 (AttributeList::Invalid, AttributeList::Uninitialized)
{
  return *impl.lookup (key);
}

bool 
AttributeList::flag (string key) const throw2 (AttributeList::Invalid, AttributeList::Uninitialized)
{
  return *impl.lookup (key);
}

int
AttributeList::integer (string key) const throw2 (AttributeList::Invalid, AttributeList::Uninitialized)
{
  return *impl.lookup (key);
}

const Time&
AttributeList::time (string key) const throw2 (AttributeList::Invalid, AttributeList::Uninitialized)
{
  return *impl.lookup (key);
}

const CSMP& 
AttributeList::csmp (string key) const throw2 (AttributeList::Invalid, AttributeList::Uninitialized)
{
  return *impl.lookup (key);
}

AttributeList& 
AttributeList::alist (string key) const throw2 (AttributeList::Invalid, AttributeList::Uninitialized)
{
  return *impl.lookup (key);
}

const vector<double>& 
AttributeList::number_sequence (string key) const
     throw2 (AttributeList::Invalid, AttributeList::Uninitialized)
{
  return *impl.lookup (key);
}

const vector<string>& 
AttributeList::name_sequence (string key) const
     throw2 (AttributeList::Invalid, AttributeList::Uninitialized)
{
  return *impl.lookup (key);
}

const vector<bool>& 
AttributeList::flag_sequence (string key) const
     throw2 (AttributeList::Invalid, AttributeList::Uninitialized)
{
  return *impl.lookup (key);
}

const vector<int>& 
AttributeList::integer_sequence (string key) const
     throw2 (AttributeList::Invalid, AttributeList::Uninitialized)
{
  return *impl.lookup (key);
}

const vector<const Time*>& 
AttributeList::time_sequence (string key) const
     throw2 (AttributeList::Invalid, AttributeList::Uninitialized)
{
  return *impl.lookup (key);
}

const vector<const CSMP*>& 
AttributeList::csmp_sequence (string key) const
     throw2 (AttributeList::Invalid, AttributeList::Uninitialized)
{
  return *impl.lookup (key);
}

const vector<AttributeList*>& 
AttributeList::alist_sequence (string key) const
     throw2 (AttributeList::Invalid, AttributeList::Uninitialized)
{
  return *impl.lookup (key);
}

void 
AttributeList::add (const string& key, double v)
{
  impl.add (key, new dValue<double> (v));
}

void 
AttributeList::add (const string& key, const char* v)
{
  impl.add (key, new dValue<string> (v));
}

void 
AttributeList::add (const string& key, const string& v)
{
  impl.add (key, new dValue<string> (v));
}

void 
AttributeList::add (const string& key, bool v)
{
  impl.add (key, new dValue<bool> (v));
}

void 
AttributeList::add (const string& key, int v)
{
  impl.add (key, new dValue<int> (v));
}

void 
AttributeList::add (const string& key, const Time& v)
{
  impl.add (key, new dValue<Time> (v));
}

void 
AttributeList::add (const string& key, AttributeList& v)
{
  impl.add (key, new dValue<AttributeList> (v));
}

void 
AttributeList::add (const string& key, const CSMP& v)
{
  impl.add (key, new dValue<CSMP> (v));
}

void 
AttributeList::add (const string& key, const vector<double>& v)
{
  impl.add (key, new dValue<vector<double>/**/> (v));
}

void 
AttributeList::add (const string& key, const vector<string>& v)
{
  impl.add (key, new dValue<vector<string>/**/> (v));
}

void 
AttributeList::add (const string& key, const vector<bool>& v)
{
  impl.add (key, new dValue<vector<bool>/**/> (v));
}

void 
AttributeList::add (const string& key, const vector<int>& v)
{
  impl.add (key, new dValue<vector<int>/**/> (v));
}

void 
AttributeList::add (const string& key, const vector<AttributeList*>& v)
{
  impl.add (key, new dValue<vector<AttributeList*>/**/> (v));
}

void 
AttributeList::add (const string& key, const vector<const CSMP*>& v)
{
  impl.add (key, new dValue<vector<const CSMP*>/**/> (v));
}

void 
AttributeList::add (const string& key, const vector<const Time*>& v)
{
  impl.add (key, new dValue<vector<const Time*>/**/> (v));
}

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
{ 
  impl.values = old.impl.values;
}

AttributeList::~AttributeList ()
{
  // BUG: Memory leak:  We don't delete the values since they can be
  // shared as above.  Solve by using reference counting.
  delete &impl; 
}

const AttributeList AttributeList::empty;
