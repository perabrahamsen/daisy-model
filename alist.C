// alist.C

#include "alist.h"
#include "action.h"
#include "condition.h"
#include <assert.h>
#include <map.h>

// @ Value
//
// Common abstraction of an attribute value.

class Value
{
public:
  virtual bool merge (const Value*);
  virtual double number () const throw (AttributeList::Invalid);
  virtual string name () const throw (AttributeList::Invalid);
  virtual bool flag () const throw (AttributeList::Invalid);
  virtual int integer () const throw (AttributeList::Invalid);
  virtual const vector<double>& array () const throw (AttributeList::Invalid);
  virtual const Rules& rules () const throw (AttributeList::Invalid);
  virtual const CSMP& csmp () const throw (AttributeList::Invalid);
  virtual const AttributeList& list () const throw (AttributeList::Invalid);
  virtual const Time& time () const throw (AttributeList::Invalid);
  virtual const Sequence& sequence () const throw (AttributeList::Invalid);
  virtual const Layers& layers () const throw (AttributeList::Invalid);
protected:
  Value ();
public:
  virtual ~Value ();
};

// Specific attribute values.

class ValueNumber : public Value
{
  const double value;
public:
  double number () const;
  ValueNumber (double);
};

class ValueString : public Value
{
  string value;
public:
  string name () const;
  ValueString (string);
};

class ValueBool : public Value
{
  bool value;
public:
  bool flag () const;
  ValueBool (bool);
};

class ValueInteger : public Value
{
  int value;
public:
  int integer () const;
  ValueInteger (int);
};

class ValueTime : public Value
{
  Time value;
public:
  const Time& time () const;
  ValueTime (const Time&);
};

class ValueSequence : public Value
{
  Sequence value;
public:
  const Sequence& sequence () const;
  ValueSequence (const Sequence&);
};

class ValueLayers : public Value
{
  Layers value;
public:
  const Layers& layers () const;
  ValueLayers (const Layers&);
};

class ValueArray : public Value
{
  vector<double> value;
public:
  const vector<double>& array () const;
  ValueArray (const vector<double>&);
};

class ValueList : public Value
{
  AttributeList& value;
public:
  bool merge (const Value*);
  const AttributeList& list () const;
  ValueList (AttributeList&);
};

class ValueRules : public Value
{
  const Rules& value;
public:
  const Rules& rules () const;
  ValueRules (const Rules&);
};

class ValueCSMP : public Value
{
  const CSMP& value;
public:
  const CSMP& csmp () const;
  ValueCSMP (const CSMP&);
};

bool 
Value::merge (const Value*)
{
  return false;
}

double
Value::number () const throw (AttributeList::Invalid)
{ 
  THROW (AttributeList::Invalid ());
  return -1.0; // SHUT UP.
}

string
Value::name () const throw (AttributeList::Invalid)
{ 
  THROW (AttributeList::Invalid ());
  return "BUG"; // SHUT UP.
}

bool
Value::flag () const throw (AttributeList::Invalid)
{ 
  THROW (AttributeList::Invalid ());
  return false; // SHUT UP.
}

int
Value::integer () const throw (AttributeList::Invalid)
{ 
  THROW (AttributeList::Invalid ());
  return false; // SHUT UP.
}

const Time&
Value::time () const throw (AttributeList::Invalid)
{ 
  THROW (AttributeList::Invalid ());
  return *((Time*) 0); // SHUT UP.
}

const Sequence&
Value::sequence () const throw (AttributeList::Invalid)
{ 
  THROW (AttributeList::Invalid ());
  return *((Sequence*) 0); // SHUT UP.
}

const Layers&
Value::layers () const throw (AttributeList::Invalid)
{ 
  THROW (AttributeList::Invalid ());
  return *((Layers*) 0); // SHUT UP.
}

const
vector<double>& Value::array () const throw (AttributeList::Invalid)
{ 
  THROW (AttributeList::Invalid ());
  return *((vector<double>*) 0);
}

const
Rules& Value::rules () const throw (AttributeList::Invalid)
{ 
  THROW (AttributeList::Invalid ());
  return *((Rules*) 0); // SHUT UP.
}

const
CSMP& Value::csmp () const throw (AttributeList::Invalid)
{ 
  THROW (AttributeList::Invalid ());
  return *((CSMP*) 0); // SHUT UP.
}

const
AttributeList& Value::list () const throw (AttributeList::Invalid)
{ 
  THROW (AttributeList::Invalid ());
  return *((AttributeList*) 0); // SHUT UP.
}

Value::Value ()
{ }

Value::~Value ()
{ }

double 
ValueNumber::number () const
{
  return value;
}

ValueNumber::ValueNumber (double n) : value (n)
{ }

string
ValueString::name () const
{
  return value;
}

ValueString::ValueString (string s) : value (s)
{ }

bool
ValueBool::flag () const
{
  return value;
}

ValueBool::ValueBool (bool b) : value (b)
{ }

int
ValueInteger::integer () const
{
  return value;
}

ValueInteger::ValueInteger (int i) : value (i)
{ }

const Time&
ValueTime::time () const
{
  return value;
}

ValueTime::ValueTime (const Time& t) : value (t)
{ }

const Sequence&
ValueSequence::sequence () const
{
  return value;
}

ValueSequence::ValueSequence (const Sequence& t) : value (t)
{ }

const Layers&
ValueLayers::layers () const
{
  return value;
}

ValueLayers::ValueLayers (const Layers& t) : value (t)
{ }

const vector<double>&
ValueArray::array () const
{
  return value;
}

ValueArray::ValueArray (const vector<double>& v) : value (v)
{ }

const Rules& 
ValueRules::rules () const
{
  return value;
}

ValueRules::ValueRules (const Rules& v) : value (v)
{ }

const CSMP& 
ValueCSMP::csmp () const
{
  return value;
}

ValueCSMP::ValueCSMP (const CSMP& v) : value (v)
{ }

bool
ValueList::merge (const Value* other)
{
  value += other->list ();
  return true;
}

const AttributeList& 
ValueList::list () const
{
  return value;
}

ValueList::ValueList (AttributeList& v) : value (v)
{ }

// @ AttributeList

typedef map <string, Value*, less<string> > value_map;

struct AttributeList::Implementation
{
  value_map values;
  const Value* lookup (string key) const throw (Uninitialized);
  void add (string key, Value* value);
};    

const Value* 
AttributeList::Implementation::lookup (string key) const throw (Uninitialized)
{ 
  value_map::const_iterator i = values.find (key);
  
  if (i != values.end ())
    return (*i).second;
  else
    THROW (UninitializedValue ());
}

void
AttributeList::Implementation::add (string key, Value* value)
{
  value_map::iterator i = values.find (key);

  if (i == values.end ())
    values[key] = value;
  else if ((*i).second->merge (value))
    delete value;
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

#ifdef HANDLE_EXCEPTIONS
const char*
AttributeList::Invalid::what () const
{ 
  return "Invalid value";
}

const char*
AttributeList::Uninitialized::what () const
{ 
  return "Uninitialized value";
}

#endif

bool
AttributeList::check (string key) const throw0 ()
{ 
  return impl.values.find (key) != impl.values.end ();
}

double 
AttributeList::number (string key) const throw2 (Invalid, Uninitialized)
{
  return impl.lookup (key)->number ();
}

string 
AttributeList::name (string key) const throw2 (Invalid, Uninitialized)
{
  return impl.lookup (key)->name ();
}

bool 
AttributeList::flag (string key) const throw2 (Invalid, Uninitialized)
{
  return impl.lookup (key)->flag ();
}

int
AttributeList::integer (string key) const throw2 (Invalid, Uninitialized)
{
  return impl.lookup (key)->integer ();
}

const Time&
AttributeList::time (string key) const throw2 (Invalid, Uninitialized)
{
  return impl.lookup (key)->time ();
}

const Sequence&
AttributeList::sequence (string key) const throw2 (Invalid, Uninitialized)
{
  return impl.lookup (key)->sequence ();
}

const Layers&
AttributeList::layers (string key) const throw2 (Invalid, Uninitialized)
{
  return impl.lookup (key)->layers ();
}

const vector<double>& 
AttributeList::array (string key) const throw2 (Invalid, Uninitialized)
{
  return impl.lookup (key)->array ();
}

const Rules& 
AttributeList::rules (string key) const throw2 (Invalid, Uninitialized)
{
  return impl.lookup (key)->rules ();
}

const CSMP& 
AttributeList::csmp (string key) const throw2 (Invalid, Uninitialized)
{
  return impl.lookup (key)->csmp ();
}

const AttributeList& 
AttributeList::list (string key) const throw2 (Invalid, Uninitialized)
{
  return impl.lookup (key)->list ();
}

void 
AttributeList::add (string key, double v)
{
  impl.add (key, new ValueNumber (v));
}

void 
AttributeList::add (string key, string v)
{
  impl.add (key, new ValueString (v));
}

void 
AttributeList::add (string key, bool v)
{
  impl.add (key, new ValueBool (v));
}

void 
AttributeList::add (string key, int v)
{
  impl.add (key, new ValueInteger (v));
}

void 
AttributeList::add (string key, const Time& v)
{
  impl.add (key, new ValueTime (v));
}

void 
AttributeList::add (string key, const Sequence& v)
{
  impl.add (key, new ValueSequence (v));
}

void 
AttributeList::add (string key, const Layers& v)
{
  impl.add (key, new ValueLayers (v));
}

void 
AttributeList::add (string key, AttributeList& v)
{
  impl.add (key, new ValueList (v));
}

void 
AttributeList::add (string key, const Rules* v)
{
  impl.add (key, new ValueRules (*v));
}

void 
AttributeList::add (string key, const CSMP* v)
{
  impl.add (key, new ValueCSMP (*v));
}

void 
AttributeList::add (string key, const vector<double>& v)
{
  impl.add (key, new ValueArray (v));
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
