// action_stop.C

#include "action.h"
#include "syntax.h"
#include "alist.h"

class ActionStop : public Action
{
public:
    void doIt (ColumnList&, const Weather&, Log&) const;
    bool stop () const;

  // Create and Destroy.
private:
  friend class ActionStopSyntax;
  static Action& make (const AttributeList&);
  ActionStop (const AttributeList&);
public:
  ~ActionStop ();
};

void 
ActionStop::doIt (ColumnList&, const Weather&, Log&) const
{
  assert (false);
}

bool
ActionStop::stop () const
{ 
  return true;
}

ActionStop::ActionStop (const AttributeList&)
{ }

ActionStop::~ActionStop ()
{ }

// Add the ActionStop syntax to the syntax table.
Action&
ActionStop::make (const AttributeList& al)
{
  return *new ActionStop (al);
}

static struct ActionStopSyntax
{
  ActionStopSyntax ();
} ActionStop_syntax;

ActionStopSyntax::ActionStopSyntax ()
{ 
  Syntax& syntax = *new Syntax ();
  AttributeList& alist = *new AttributeList ();
  Action::add_type ("stop", alist, syntax, &ActionStop::make);
}
