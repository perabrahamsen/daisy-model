// value.C

#include "value.h"
#include "action.h"
#include "condition.h"
#include <assert.h>

template class vector<double>;

const char*
InvalidValue::what () const
{ 
    return "Invalid value";
}

const char*
UninitializedValue::what () const
{ 
    return "Uninitialized value";
}

double
Value::number () const throw (InvalidValue)
{ 
    THROW (InvalidValue ());
}

string
Value::name () const throw (InvalidValue)
{ 
    THROW (InvalidValue ());
}

bool
Value::flag () const throw (InvalidValue)
{ 
    THROW (InvalidValue ());
}

#ifdef USE_VIRTUAL_VALUE
Value* 
Value::lookup (string) const throw (UninitializedValue, InvalidValue) 
{ 
    THROW (InvalidValue ());
}

Value* 
Value::check (string) const throw (InvalidValue)
{ 
    THROW (InvalidValue ());
}

const Action* 
Value::match (ColumnList&, const Wheather&,
	      int /* day */, int /* hour */) const throw (InvalidValue)
{
    THROW (InvalidValue ());
}    

double
Value::y (double /* x */) const throw (InvalidValue)
{ 
    THROW (InvalidValue ());
}

double
Value::operator[] (int) const throw (InvalidValue)
{ 
    THROW (InvalidValue ());
}

#endif

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

struct ValueList::Implementation
{
    // This should be replaced with a STL map.
    // THIS SHOULD BE REPLACED WITH A STL MAP.
    // I'm not this stupid.  Honestly.
    const int UGLY_MAX_SIZE = 1024;
    string UGLY_key[UGLY_MAX_SIZE];
    Value* UGLY_value[UGLY_MAX_SIZE];
    int size;
    Implementation ();
};    

ValueList::Implementation::Implementation () : size (0)
{ }

Value* 
ValueList::lookup (string key) const throw (UninitializedValue)
{ 
    Value* value = check (key);
    if (value)
	return value;
    THROW (UninitializedValue ());
}

Value* 
ValueList::check (string key) const throw0 ()
{ 
    for (int i = 0; i < impl.size; i++)
	if (impl.UGLY_key[i] == key)
	    return impl.UGLY_value[i];
    return NULL;
}

void
ValueList::add (string key, Value* value)
{
    assert (impl.size < impl.UGLY_MAX_SIZE);
    impl.UGLY_key[impl.size] = key;
    impl.UGLY_value[impl.size] = value;
    impl.size++;
}

ValueList::ValueList () : impl (*new Implementation ())
{ }

ValueList::ValueList (const ValueList& old) : impl (*new Implementation ())
{ 
    impl.size = old.impl.size;
    for (int i = 0; i < impl.size; i++)
	{
	    impl.UGLY_key[i] = old.impl.UGLY_key[i];
	    impl.UGLY_value[i] = old.impl.UGLY_value[i];
	}
}

ValueList::~ValueList ()
{
    delete &impl; 
}

const ValueList ValueList::empty;

struct ValueRules::Implementation
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

ValueRules::Implementation::Rule::Rule (Condition const* c, Action const* a) 
    : condition (c), 
      action (a)
{ }

void 
ValueRules::add (Condition* c, Action* a)
{
    impl.rules.push_front (new Implementation::Rule (c, a));
}

const Action*
ValueRules::match (ColumnList& cl, const Wheather& w, int day, int hour) const
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

ValueRules::ValueRules (const ValueRules *const old = NULL)
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

ValueRules::~ValueRules ()
{ 
    delete &impl;
    // I should delete all members of `impl.rules' here, but they
    // might appear in some other ValueRules `impl.super'.  Obviously
    // the solution is to use `list <RefCount <Rules> >' instead of
    // `list <Rules*>'.  The same is true for the other places I use
    // STL containers.  I'll experiement with this latter.  BTW: It
    // isn't a real memory leak, as ValueRules are created during
    // initialization, and deleted only when the simulation is over,
    // after which the program ends. However, it might be useful one
    // day to run multiple simulations in the same program execution.
}

struct ValueCSMP::Implementation
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

ValueCSMP::Implementation::pair::pair (double a, double b)
    : x(a), y(b)
{ }

ValueCSMP::Implementation::pair::pair ()
    : x(0.0), y(0.0)
{ }

void 
ValueCSMP::add (double x , double y)
{
    impl.points.push_back (Implementation::pair(x, y));
}

double
ValueCSMP::y (double x) const
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

ValueCSMP::ValueCSMP ()
    : impl (*new Implementation)
{}
ValueCSMP::~ValueCSMP ()
{
    delete &impl;
}

void
ValueArray::add (double d)
{ 
    impl.push_back (d);
}

double
ValueArray::operator[] (int index) const
{
    return impl[index];
}

ValueArray::ValueArray ()
{ }

ValueArray::~ValueArray ()
{ }

string
ValueString::name () const
{
    return impl;
}

ValueString::ValueString (string s) : impl (s)
{ }

ValueString:: ~ValueString ()
{ }

bool
ValueBool::flag () const
{
    return impl;
}

ValueBool::ValueBool (bool b) : impl (b)
{ }

ValueBool:: ~ValueBool ()
{ }

