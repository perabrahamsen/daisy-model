// manager_rule.C

#include "manager.h"
#include "syntax.h"
#include "rules.h"
#include "alist.h"

class ManagerRule : public Manager
{
  // Content.
private:
  const Rules& rules;

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
  : rules (vl.rules ("rules"))
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
  Syntax& syntax = *new Syntax ();
  AttributeList& alist = *new AttributeList ();
  syntax.add ("rules", Syntax::Rules, Syntax::Const);
  Manager::add_type ("rule", alist, syntax, &ManagerRule::make);
}
