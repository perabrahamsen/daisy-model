// action_sow.C

#include "action.h"
#include "column.h"
#include "crop.h"
#include "syntax.h"
#include "alist.h"
#include "common.h"
#include <iostream.h>

class ActionSow : public Action
{
  const AttributeList& crop;

public:
  void doIt (ColumnList&, const Weather&, Log&) const;

  // Create and Destroy.
private:
  friend class ActionSowSyntax;
  static Action& make (const AttributeList&);
  ActionSow (const AttributeList&);
public:
  ~ActionSow ();
};

void 
ActionSow::doIt (ColumnList& cl, const Weather&, Log& log) const
{
  cout << " [Sowing " << crop.name ("type") << "]";

  for (ColumnList::iterator i = cl.begin (); i != cl.end (); i++)
    {
      (*i)->sow (crop, log);
    }
}

ActionSow::ActionSow (const AttributeList& al)
  : crop (al)
{ }

ActionSow::~ActionSow ()
{ }

// Add the ActionSow syntax to the syntax table.
Action&
ActionSow::make (const AttributeList& al)
{
  return *new ActionSow (al);
}

static struct ActionSowSyntax
{
  ActionSowSyntax ();
} ActionSow_syntax;

ActionSowSyntax::ActionSowSyntax ()
{ 
  Syntax& syntax = *new Syntax ();
  AttributeList& alist = *new AttributeList ();
  syntax.add ("crop", Crop::library (), Syntax::Const);
  syntax.order ("crop");
  Action::add_type ("sow", alist, syntax, &ActionSow::make);
}
