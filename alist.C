// alist.C

#include "alist.h"
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
  // Retrieve data (Singletons).
  virtual operator double () const
       throw1 (AttributeList::Invalid);
  virtual operator string () const
       throw1 (AttributeList::Invalid);
  virtual operator bool () const
       throw1 (AttributeList::Invalid);
  virtual operator int () const
       throw1 (AttributeList::Invalid);
  virtual operator const CSMP& () const
       throw1 (AttributeList::Invalid);
  virtual operator const AttributeList& () const
       throw1 (AttributeList::Invalid);
  virtual operator const Time& () const throw1 (AttributeList::Invalid);

  // Retrieve data (Sequences).
  virtual operator const vector<double>& () const
       throw1 (AttributeList::Invalid);
  virtual operator const vector<string>& () const
       throw1 (AttributeList::Invalid);
  virtual operator const vector<bool>& () const
       throw1 (AttributeList::Invalid);
  virtual operator const vector<int>& () const
       throw1 (AttributeList::Invalid);
  virtual operator const vector<const CSMP*>& () const
       throw1 (AttributeList::Invalid);
  virtual operator const vector<const AttributeList*>& () const
       throw1 (AttributeList::Invalid);
  virtual operator const vector<const Time*>& () const
       throw1 (AttributeList::Invalid);
protected:
  Value ();
public:
  virtual ~Value ();
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
  dValue (bool v)
    : value (v)
  { };
};

class dValue<string> : public Value
{
  string value;
public:
  operator string () const throw1 (AttributeList::Invalid)
  { return value; }
  dValue (string v)
    : value (v)
  { };
};

class dValue<Time> : public Value
{
  const Time& value;
public:
  operator const Time& () const throw1 (AttributeList::Invalid)
  { return value; }
  dValue (const Time& v)
    : value (*new Time (v))
  { };
};

Value::operator double () const
     throw1 (AttributeList::Invalid)
{ 
  THROW (AttributeList::Invalid ());
}

Value::operator string () const
     throw1 (AttributeList::Invalid)
{ 
  THROW (AttributeList::Invalid ());
}

Value::operator bool () const
     throw1 (AttributeList::Invalid)
{ 
  THROW (AttributeList::Invalid ());
}

Value::operator int () const
     throw1 (AttributeList::Invalid)
{ 
  THROW (AttributeList::Invalid ());
}

Value::operator const Time& () const
     throw1 (AttributeList::Invalid)
{ 
  THROW (AttributeList::Invalid ());
}

Value::operator const CSMP& () const
     throw1 (AttributeList::Invalid)
{ 
  THROW (AttributeList::Invalid ());
}

Value::operator const AttributeList& () const
     throw1 (AttributeList::Invalid)
{ 
  THROW (AttributeList::Invalid ());
}

Value::operator const vector<double>& () const
     throw1 (AttributeList::Invalid)
{ 
  THROW (AttributeList::Invalid ());
}

Value::operator const vector<string>& () const
     throw1 (AttributeList::Invalid)
{ 
  THROW (AttributeList::Invalid ());
}

Value::operator const vector<bool>& () const
     throw1 (AttributeList::Invalid)
{ 
  THROW (AttributeList::Invalid ());
}

Value::operator const vector<int>& () const
     throw1 (AttributeList::Invalid)
{ 
  THROW (AttributeList::Invalid ());
}

Value::operator const vector<const CSMP*>& () const
     throw1 (AttributeList::Invalid)
{ 
  THROW (AttributeList::Invalid ());
}

Value::operator const vector<const AttributeList*>& () const
     throw1 (AttributeList::Invalid)
{ 
  THROW (AttributeList::Invalid ());
}

Value::operator const vector<const Time*>& () const
     throw1 (AttributeList::Invalid)
{ 
  THROW (AttributeList::Invalid ());
}

Value::Value ()
{ }

Value::~Value ()
{ }

// @ AttributeList

typedef map <string, Value*, less<string> > value_map;

struct AttributeList::Implementation
{
  value_map values;
  const Value* lookup (string key) const throw1 (Uninitialized);
  void add (string key, Value* value);
};    

const Value* 
AttributeList::Implementation::lookup (string key) const throw1 (AttributeList::Uninitialized)
{ 
  value_map::const_iterator i = values.find (key);
  
  if (i != values.end ())
    return (*i).second;
  else
    THROW (Uninitialized ());
}

void
AttributeList::Implementation::add (string key, Value* value)
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
AttributeList::check (string key) const throw0 ()
{ 
  return impl.values.find (key) != impl.values.end ();
}

double 
AttributeList::number (string key) const throw2 (AttributeList::Invalid, AttributeList::Uninitialized)
{
  return *impl.lookup (key);
}

string 
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

const AttributeList& 
AttributeList::list (string key) const throw2 (AttributeList::Invalid, AttributeList::Uninitialized)
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

const vector<const AttributeList*>& 
AttributeList::list_sequence (string key) const
     throw2 (AttributeList::Invalid, AttributeList::Uninitialized)
{
  return *impl.lookup (key);
}

void 
AttributeList::add (string key, double v)
{
  impl.add (key, new dValue<double> (v));
}

void 
AttributeList::add (string key, string v)
{
  impl.add (key, new dValue<string> (v));
}

void 
AttributeList::add (string key, bool v)
{
  impl.add (key, new dValue<bool> (v));
}

void 
AttributeList::add (string key, int v)
{
  impl.add (key, new dValue<int> (v));
}

void 
AttributeList::add (string key, const Time& v)
{
  impl.add (key, new dValue<Time> (v));
}

void 
AttributeList::add (string key, const AttributeList& v)
{
  impl.add (key, new dValue<AttributeList> (v));
}

void 
AttributeList::add (string key, const CSMP& v)
{
  impl.add (key, new dValue<CSMP> (v));
}

void 
AttributeList::add (string key, const vector<double>& v)
{
  impl.add (key, new dValue<vector<double>/**/> (v));
}

void 
AttributeList::add (string key, const vector<string>& v)
{
  impl.add (key, new dValue<vector<string>/**/> (v));
}

void 
AttributeList::add (string key, const vector<bool>& v)
{
  impl.add (key, new dValue<vector<bool>/**/> (v));
}

void 
AttributeList::add (string key, const vector<int>& v)
{
  impl.add (key, new dValue<vector<int>/**/> (v));
}

void 
AttributeList::add (string key, const vector<const AttributeList*>& v)
{
  impl.add (key, new dValue<vector<const AttributeList*>/**/> (v));
}

void 
AttributeList::add (string key, const vector<const CSMP*>& v)
{
  impl.add (key, new dValue<vector<const CSMP*>/**/> (v));
}

void 
AttributeList::add (string key, const vector<const Time*>& v)
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
