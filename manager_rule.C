// manager_rule.C

#include "manager_rule.h"
#include "syntax.h"
#include "rules.h"
#include "alist.h"

struct ManagerRule::Implementation
{ 
  const Rules& rules;
  Implementation (const AttributeList& vl);
};

ManagerRule::Implementation::Implementation (const AttributeList& vl)
  : rules (vl.rules ("rules"))
{ }

const Action*
ManagerRule::action (const Daisy& daisy)
{
  return impl.rules.match (daisy);
}

ManagerRule::ManagerRule (const AttributeList& vl)
  : impl (*new Implementation (vl))
{ }

ManagerRule::~ManagerRule () 
{
  delete &impl;
}

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
  Syntax* syntax = new Syntax ();
  AttributeList* alist = new AttributeList ();
  syntax->add ("rules", Syntax::Rules);
  Manager::add_type ("rule", *alist, *syntax, &ManagerRule::make);
}
