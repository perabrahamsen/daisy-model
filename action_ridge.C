// action_ridge.C

#include "action.h"
#include "daisy.h"
#include "field.h"
#include "ridge.h"

struct ActionRidge : public Action
{
  const AttributeList& ridge;

  void doIt (Daisy& daisy)
    { 
      COUT << " [Ridging]\n";      
      daisy.field.ridge (ridge); 
    }

  ActionRidge (const AttributeList& al)
    : Action (al),
      ridge (al.alist ("ridge"))
    { }
};

#ifdef BORLAND_TEMPLATES
template class add_submodule_sequence<Ridge>;
#endif

// Add the ActionRidge syntax to the syntax table.
static struct ActionRidgeSyntax
{
  static Action& make (const AttributeList& al)
  { return *new ActionRidge (al); }

  ActionRidgeSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "Ridge a ridge on the field.");
    add_submodule<Ridge> ("ridge", syntax, alist, Syntax::Const,
			  "Ridge parameters");
    syntax.order ("ridge");
    Librarian<Action>::add_type ("ridge", alist, syntax, &make);
  }
} ActionRidge_syntax;

