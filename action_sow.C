// action_sow.C

#include "action.h"
#include "daisy.h"
#include "column.h"
#include "crop.h"		// We need to initialize the Crop library.
#include "syntax.h"
#include "alist.h"
#include "common.h"
#include <iostream.h>

class ActionSow : public Action
{
  const AttributeList& crop;

public:
  void doIt (Daisy& daisy)
  {
    cout << " [Sowing " << crop.name ("type") << " at";
    
    ColumnList& cl = daisy.columns;
    for (ColumnList::iterator i = cl.begin (); i != cl.end (); i++)
      { 
	if (match (**i))
	  {
	    (*i)->sow (crop); 
	    cout << " " << (*i)->name;
	  }
      }
    cout << "]";
  }
  // Create and Destroy.
private:
  friend class ActionSowSyntax;
  static Action& make (const AttributeList& al, const Action *const p)
  { return *new ActionSow (al, p); }
  ActionSow (const AttributeList& al, const Action *const p)
  : Action (p),
    crop (al.alist ("crop"))
  { }
public:
  ~ActionSow ()
  { }
};

// Add the ActionSow syntax to the syntax table.
static struct ActionSowSyntax
{
  ActionSowSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    syntax.add ("crop", Crop::library (), Syntax::Const);
    syntax.order ("crop");
    Action::add_type ("sow", alist, syntax, &ActionSow::make);
  }
} ActionSow_syntax;

