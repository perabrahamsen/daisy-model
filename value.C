// value.C

#include "value.h"
#include "action.h"
#include "condition.h"
#include <assert.h>

// @ Value
//
// Common abstraction of an attribute value.

class Value
{
public:
    virtual double number () const throw (AttributeList::Invalid);
    virtual string name () const throw (AttributeList::Invalid);
    virtual bool flag () const throw (AttributeList::Invalid);
    virtual const vector<double>& array ()
        const throw (AttributeList::Invalid);
    virtual const Rules& rules () const throw (AttributeList::Invalid);
    virtual const CSMP& csmp () const throw (AttributeList::Invalid);
    virtual const AttributeList& list () const throw (AttributeList::Invalid);
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

double
Value::number () const throw (AttributeList::Invalid)
{ 
    THROW (AttributeList::Invalid ());
}

string
Value::name () const throw (AttributeList::Invalid)
{ 
    THROW (AttributeList::Invalid ());
}

bool
Value::flag () const throw (AttributeList::Invalid)
{ 
    THROW (AttributeList::Invalid ());
}

const
vector<double>& Value::array () const throw (AttributeList::Invalid)
{ 
    THROW (AttributeList::Invalid ());
}

const
Rules& Value::rules () const throw (AttributeList::Invalid)
{ 
    THROW (AttributeList::Invalid ());
}

const
CSMP& Value::csmp () const throw (AttributeList::Invalid)
{ 
    THROW (AttributeList::Invalid ());
}

const
AttributeList& Value::list () const throw (AttributeList::Invalid)
{ 
    THROW (AttributeList::Invalid ());
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

const AttributeList& 
ValueList::list () const
{
    return value;
}

ValueList::ValueList (const AttributeList& v) : value (v)
{ }

// @ Rules

struct Rules::Implementation
{
    struct Rule
    {
	Condition const* condition;
	Action const* action;
	Rule (Condition const*, Action const*);
    };
    typedef list <Rule*> RuleList;
    RuleList rules;
    RuleList super;    
};

Rules::Implementation::Rule::Rule (Condition const* c, Action const* a) 
    : condition (c), 
      action (a)
{ }

void 
Rules::add (Condition* c, Action* a)
{
    impl.rules.push_front (new Implementation::Rule (c, a));
}

const Action*
Rules::match (ColumnList& cl, const Wheather& w, int day, int hour) const
{
    for (Implementation::RuleList::iterator i = impl.rules.begin ();
	 i != impl.rules.end ();
	 i++)
	{
	    if ((*i)->condition->match (cl, w, day, hour))
		return (*i)->action;
	}
    for (Implementation::RuleList::iterator i = impl.super.begin ();
	 i != impl.super.end ();
	 i++)
	{
	    if ((*i)->condition->match (cl, w, day, hour))
		return (*i)->action;
	}
    return &Action::null;
}

Rules::Rules (const Rules *const old = NULL)
    : impl (*new Implementation)
{ 
    if (old)
	{
	    for (Implementation::RuleList::iterator i 
		     = old->impl.rules.begin();
		 i != old->impl.rules.end ();
		 i++)
		{
		    impl.super.push_back (*i);
		}
	    for (Implementation::RuleList::iterator i
		     = old->impl.super.begin();
		 i != old->impl.super.end ();
		 i++)
		{
		    impl.super.push_back (*i);
		}
	}
}

Rules::~Rules ()
{ 
    delete &impl;
    // I should delete all members of `impl.rules' here, but they
    // might appear in some other Rules `impl.super'.  Obviously
    // the solution is to use `list <RefCount <Rules> >' instead of
    // `list <Rules*>'.  The same is true for the other places I use
    // STL containers.  I'll experiement with this latter.  BTW: It
    // isn't a real memory leak, as Rules are created during
    // initialization, and deleted only when the simulation is over,
    // after which the program ends. However, it might be useful one
    // day to run multiple simulations in the same program execution.
}

// @ CSMP

struct CSMP::Implementation
{
    struct pair
    {
	double x;
	double y;
	pair (double a, double b);
	pair ();		// Needed by vector<>
    };
    typedef vector<pair> PairVector;
    PairVector points;
};

CSMP::Implementation::pair::pair (double a, double b)
    : x(a), y(b)
{ }

CSMP::Implementation::pair::pair ()
    : x(0.0), y(0.0)
{ }

void 
CSMP::add (double x , double y)
{
    impl.points.push_back (Implementation::pair(x, y));
}

double
CSMP::operator() (double x) const
{
    for (unsigned int i = 0; i < impl.points.size(); i++)
	{
	    if (x > impl.points[i].x)
		{
		    if (i == 0)
			return impl.points[i].y;
		    
		    return impl.points[i-1].y 
			+   (impl.points[i].y - impl.points[i-1].y)
			  / (impl.points[i].x - impl.points[i-1].x)
			  * (x - impl.points[i-1].x);
		}
	}
    return impl.points[impl.points.size () - 1].y;
}

CSMP::CSMP ()
    : impl (*new Implementation)
{}
CSMP::~CSMP ()
{
    delete &impl;
}

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



