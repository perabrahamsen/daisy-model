// manager_rule.h

#ifndef MANAGER_RULE_H
#define MANAGER_RULE_H

#include "manager.h"

class ManagerRule : public Manager
{
  // Content.
private:
  struct Implementation;
  Implementation& impl;

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

#endif MANAGER_RULE_H
