// manager_rule.C

#include "manager.h"
#include "syntax.h"
#include "alist.h"
#include "action.h"
#include "condition.h"
#include "daisy.h"

class Rules
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
public:
  // Use.
  const Action* match (const Daisy&) const;
  // Create and Destroy.
  Rules (const vector <const AttributeList*>& rl);
  ~Rules ();
};

const Action*
Rules::match (const Daisy& daisy) const
{
  for (RuleList::const_iterator i = rules.begin ();
       i != rules.end ();
       i++)
    {
      if (daisy.match ((*i)->condition))
	return (*i)->action;
    }
  return &Action::null;
}

Rules::Rules (const vector<const AttributeList*>& rl)
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

Rules::~Rules ()
{
  for (RuleList::const_iterator i = rules.begin (); i != rules.end (); i++)
    delete *i;
}

class ManagerRule : public Manager
{
  // Content.
private:
  const Rules rules;

    // Simulation.
public:
  const Action* action (const Daisy&);

    // Create and Destroy.
private:
  friend class ManagerRuleSyntax;
  static Manager* make (const AttributeList&);
  ManagerRule (const AttributeList&);
public:
  ~ManagerRule ();
};

const Action*
ManagerRule::action (const Daisy& daisy)
{
  return rules.match (daisy);
}

ManagerRule::ManagerRule (const AttributeList& vl)
  : rules (vl.list_sequence ("rules"))
{ }

ManagerRule::~ManagerRule () 
{ }

// Add the ManagerRule syntax to the syntax table.
Manager* 
ManagerRule::make (const AttributeList& al)
{
  return new ManagerRule (al);
}

static struct ManagerRuleSyntax
{
  ManagerRuleSyntax ();
} ManagerRule_syntax;

ManagerRuleSyntax::ManagerRuleSyntax ()
{
  Syntax& rule = *new Syntax ();
  rule.add ("condition", Condition::library (), Syntax::Const);
  rule.add ("action", Action::library (), Syntax::Const);
  rule.order ("condition", "action");
  Syntax& syntax = *new Syntax ();
  AttributeList& alist = *new AttributeList ();
  syntax.add ("rules", rule, Syntax::Const, Syntax::Sequence);
  syntax.order ("rules");
  // syntax.order ("rules");
  Manager::add_type ("rule", alist, syntax, &ManagerRule::make);
}
