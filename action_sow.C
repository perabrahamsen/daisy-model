// action_sow.C

#include "action.h"
#include "daisy.h"
#include "field.h"
#include "crop.h"

struct ActionSow : public Action
{
  const AttributeList& crop;

  void doIt (Daisy& daisy)
    { 
      COUT << " [Sowing " << crop.name ("type") << "]\n";      
      daisy.field.sow (crop); 
    }

  ActionSow (const AttributeList& al)
    : Action (al),
      crop (al.alist ("crop"))
    { }
};

// Add the ActionSow syntax to the syntax table.
static struct ActionSowSyntax
{
  static Action& make (const AttributeList& al)
  { return *new ActionSow (al); }

  ActionSowSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "Sow a crop on the field.");
    syntax.add ("crop", Librarian<Crop>::library (), "Crop to sow.");
    syntax.order ("crop");
    Librarian<Action>::add_type ("sow", alist, syntax, &make);
  }
} ActionSow_syntax;

