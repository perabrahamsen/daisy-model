// rules.C

#include "rules.h"
#include "daisy.h"
#include "action.h"
#include "condition.h"
#include "syntax.h"
#include "alist.h"
#include <list.h>

static const Syntax* Rules_syntax = NULL;

struct Rules::Implementation
{
  struct Rule
  {
    Condition const* condition;
    Action const* action;
    Rule (Condition const* c, Action const* a)
      : condition (c), 
	action (a)
    { }
    ~Rule ()
    {
      delete condition;
      delete action;
    }
  };
  typedef vector <Rule*> RuleList;
  RuleList rules;
  Implementation (const vector<const AttributeList*>& rl);
  ~Implementation ();
};

Rules::Implementation::Implementation (const vector<const AttributeList*>& rl)
  : rules ()
{
  for (vector<const AttributeList*>::const_iterator i = rl.begin ();
       i != rl.end ();
       i++)
    {
      const AttributeList& al = **i;
      rules.push_back (new Rule (&Condition::create (al.list ("condition")),
				 &Action::create (al.list ("action"))));
    }
}

Rules::Implementation::~Implementation ()
{
  for (RuleList::const_iterator i = rules.begin (); i != rules.end (); i++)
    delete *i;
}

const Syntax&
Rules::syntax ()
{
  assert (Rules_syntax);
  return *Rules_syntax;
}

void 
Rules::add (const Condition* c, const Action* a)
{
  impl.rules.push_back (new Implementation::Rule (c, a));
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
  return &Action::null;
}

Rules::Rules (const vector<const AttributeList*>& rl)
  : impl (*new Implementation (rl))
{ }

Rules::~Rules ()
{ 
  delete &impl;
}

int Rules_init::count;

Rules_init::Rules_init ()
{ 
  if (count++ == 0)
    {
      Syntax& rule = *new Syntax ();
      rule.add ("condition", Condition::library (), Syntax::Const);
      rule.add ("action", Action::library (), Syntax::Const);
      rule.order ("condition", "action");
      Rules_syntax = &rule;
    }
  assert (count > 0);
}

Rules_init::~Rules_init ()
{ 
  if (--count == 0)
    {
      delete Rules_syntax;
      Rules_syntax = NULL;
    }
  assert (count >= 0);
}
