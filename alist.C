// alist.C

#include "alist.h"
#include "action.h"
#include "condition.h"
#include <assert.h>
#include <vector.h>

// @ Value
//
// Common abstraction of an attribute value.

class Value
{
public:
    virtual double number () const throw (AttributeList::Invalid);
    virtual string name () const throw (AttributeList::Invalid);
    virtual bool flag () const throw (AttributeList::Invalid);
    virtual int integer () const throw (AttributeList::Invalid);
    virtual const vector<double>& array ()
        const throw (AttributeList::Invalid);
    virtual const Rules& rules () const throw (AttributeList::Invalid);
    virtual const CSMP& csmp () const throw (AttributeList::Invalid);
    virtual const AttributeList& list () const throw (AttributeList::Invalid);
    virtual CropList& crops () const throw (AttributeList::Invalid);
    virtual ColumnList& columns () const throw (AttributeList::Invalid);
    virtual const Time& time () const throw (AttributeList::Invalid);
protected:
    Value ();
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

class ValueArray : public Value
{
    vector<double> value;
public:
    const vector<double>& array () const;
    ValueArray (const vector<double>&);
};

class ValueList : public Value
{
    const AttributeList& value;
public:
    const AttributeList& list () const;
    ValueList (const AttributeList&);
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

class ValueColumns : public Value
{
    ColumnList& value;
public:
    ColumnList& columns () const;
    ValueColumns (ColumnList&);
};

class ValueCrops : public Value
{
    CropList& value;
public:
    CropList& crops () const;
    ValueCrops (CropList&);
};

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


ColumnList& Value::columns () const throw (AttributeList::Invalid)
{ 
    THROW (AttributeList::Invalid ());
    return *((ColumnList*) 0); // SHUT UP.
}

CropList& Value::crops () const throw (AttributeList::Invalid)
{ 
    THROW (AttributeList::Invalid ());
    return *((CropList*) 0); // SHUT UP.
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

ColumnList& 
ValueColumns::columns () const
{
    return value;
}

ValueColumns::ValueColumns (ColumnList& v) : value (v)
{ }

CropList& 
ValueCrops::crops () const
{
    return value;
}

ValueCrops::ValueCrops (CropList& v) : value (v)
{ }

const AttributeList& 
ValueList::list () const
{
    return value;
}

ValueList::ValueList (const AttributeList& v) : value (v)
{ }

// @ AttributeList

struct AttributeList::Implementation
{
    // This should be replaced with a STL map.
    // THIS SHOULD BE REPLACED WITH A STL MAP.
    // I'm not this stupid.  Honestly.
    const int UGLY_MAX_SIZE = 1024;
    string UGLY_key[UGLY_MAX_SIZE];
    const Value* UGLY_value[UGLY_MAX_SIZE];
    int size;
    const Value* lookup (string key) const throw (Uninitialized);
    void add (string key, const Value* value);
    Implementation ();
};    

AttributeList::Implementation::Implementation () : size (0)
{ }

const Value* 
AttributeList::Implementation::lookup (string key) const throw (Uninitialized)
{ 
    for (int i = 0; i < size; i++)
	if (UGLY_key[i] == key)
	    return UGLY_value[i];
    THROW (UninitializedValue ());
}

void
AttributeList::Implementation::add (string key, const Value* value)
{
    assert (size < UGLY_MAX_SIZE);
    UGLY_key[size] = key;
    UGLY_value[size] = value;
    size++;
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
    for (int i = 0; i < impl.size; i++)
	if (impl.UGLY_key[i] == key)
	    return true;
    return false;
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

ColumnList& 
AttributeList::columns (string key) const throw2 (Invalid, Uninitialized)
{
    return impl.lookup (key)->columns ();
}

CropList& 
AttributeList::crops (string key) const throw2 (Invalid, Uninitialized)
{
    return impl.lookup (key)->crops ();
}

const AttributeList& 
AttributeList::list (string key) const throw2 (Invalid, Uninitialized)
{
    return impl.lookup (key)->list ();
}

void 
AttributeList::add (string key , double v)
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
AttributeList::add (string key, const AttributeList* v)
{
    impl.add (key, new ValueList (*v));
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
AttributeList::add (string key, ColumnList* v)
{
    impl.add (key, new ValueColumns (*v));
}

void 
AttributeList::add (string key, CropList* v)
{
    impl.add (key, new ValueCrops (*v));
}

void 
AttributeList::add (string key, const vector<double>& v)
{
    impl.add (key, new ValueArray (v));
}

AttributeList::AttributeList ()
    : impl (*new Implementation ())
{ }

AttributeList::AttributeList (const AttributeList& old)
    : impl (*new Implementation ())
{ 
    impl.size = old.impl.size;
    for (int i = 0; i < impl.size; i++)
	{
	    impl.UGLY_key[i] = old.impl.UGLY_key[i];
	    impl.UGLY_value[i] = old.impl.UGLY_value[i];
	}
}

AttributeList::~AttributeList ()
{
    delete &impl; 
}

const AttributeList AttributeList::empty;
