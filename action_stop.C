// action_stop.C

#include "action.h"
#include "syntax.h"
#include "alist.h"
#include "daisy.h"

class ActionStop : public Action
{
public:
  void doIt (Daisy& daisy)
  {
    daisy.running = false;
  }
  // Create and Destroy.
private:
  friend class ActionStopSyntax;
  static Action& make (const AttributeList& al, const Action *const p)
  {
    return *new ActionStop (al, p);
  }
  ActionStop (const AttributeList&, const Action *const p)
    : Action (p)
  { }
public:
  ~ActionStop ()
  { }
};

// Add the ActionStop syntax to the syntax table.
static struct ActionStopSyntax
{
  ActionStopSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    Action::add_type ("stop", alist, syntax, &ActionStop::make);
  }
} ActionStop_syntax;
