// action_stop.C

#include "action.h"
#include "daisy.h"

struct ActionStop : public Action
{
  void doIt (Daisy& daisy)
    { daisy.running = false; }

  ActionStop (const AttributeList& al)
    : Action (al.name ("type"))
    { }
};

static struct ActionStopSyntax
{
  static Action& make (const AttributeList& al)
  {
    return *new ActionStop (al);
  }
  ActionStopSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    Librarian<Action>::add_type ("stop", alist, syntax, &make);
  }
} ActionStop_syntax;
