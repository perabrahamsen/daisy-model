// rules.C

#include "rules.h"
#include "daisy.h"
#include "action.h"

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
Rules::add (const Condition* c, const Action* a)
{
    impl.rules.push_front (new Implementation::Rule (c, a));
}

const Action*
Rules::match (const Daisy& daisy) const
{
    for (Implementation::RuleList::iterator i = impl.rules.begin ();
	 i != impl.rules.end ();
	 i++)
	{
	    if (daisy.match ((*i)->condition))
		return (*i)->action;
	}
    for (Implementation::RuleList::iterator i = impl.super.begin ();
	 i != impl.super.end ();
	 i++)
	{
	    if (daisy.match ((*i)->condition))
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

