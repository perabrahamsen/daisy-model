// alist.C

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
  // Utilities.
  virtual void dump (const Syntax&, const string& key, int indent) = 0;

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
  void dump (const Syntax& syntax, const string& key, int indent)
    { cout << "<" << Syntax::type_name (syntax.lookup (key)) << ">"; }
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
  void dump (const Syntax& syntax, const string& key, int indent)
    { cout << value; }
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
  void dump (const Syntax& syntax, const string& key, int indent)
    { cout << value; }
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
  void dump (const Syntax& syntax, const string& key, int indent)
    { cout << (value ? "true" : "false"); }
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
  void dump (const Syntax& syntax, const string& key, int indent)
    { cout << value; }
  operator const string& () const throw1 (AttributeList::Invalid)
  { return value; }
  dValue (const string& v)
    : value (v)
  { };
};

class dValue<Time> : public Value
{
  const Time& value;
public:
  void dump (const Syntax& syntax, const string& key, int indent)
    { cout << "<" << Syntax::type_name (syntax.lookup (key)) << ">"; }
  operator const Time& () const throw1 (AttributeList::Invalid)
  { return value; }
  dValue (const Time& v)
    : value (*new Time (v))
  { };
};

class dValue<AttributeList> : public Value
{
  AttributeList& value;
public:
  void dump (const Syntax& syntax, const string& key, int indent)
    { 
      const Syntax::type type = syntax.lookup (key);
      if (type == Syntax::AList)
	value.dump (syntax.syntax (key), indent); 
      else 
	cout << "<obj " << Syntax::type_name (type) << ">";
    }
  operator AttributeList& () const throw1 (AttributeList::Invalid)
  { return value; }
  dValue (AttributeList& v)
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
  void dump (const Syntax& syntax, int indent) const;
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
AttributeList::Implementation::dump (const Syntax& syntax, 
				     int indent) const
{
  vector<string> entries;
  syntax.entries (entries);
  const vector<string>& order = syntax.order ();

  bool first = true;
  for (unsigned int i = 0; i < order.size (); i++)
    {
      first = false;
      const string& key = order[i];

      if (check (key))
	lookup (key)->dump (syntax, key, indent);
      else
	cout << " <missing " << key << ">";
    }
  
  for (value_map::const_iterator i = values.begin (); i != values.end (); i++)
    {
      const string key = (*i).first;

      if (syntax.order (key) < 0)
	{
	  if (first)
	    first = false;
	  else
	    cout << "\n" << string (indent, ' ');

	  cout << "(" << key << " ";
	  (*i).second->dump (syntax, key, indent + key.length () + 2);
	  cout << ")";
	}
    }
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

void
AttributeList::dump (const Syntax& syntax, int indent) const
{
  impl.dump (syntax, indent);
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
