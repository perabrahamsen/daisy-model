// action_sow.C

#include "action.h"
#include "daisy.h"
#include "field.h"
#include "chemical.h"

struct ActionSpray : public Action
{
  const string chemical;
  const double amount;

  void doIt (Daisy& daisy)
    {
      COUT << " [Spraying " << chemical << "]\n";
      daisy.field.spray (chemical, amount); 
    }

  ActionSpray (const AttributeList& al)
    : Action (al),
      chemical (al.name ("chemical")),
      amount (al.number ("amount"))
    { }
};

// Add the ActionSpray syntax to the syntax table.
static struct ActionSpraySyntax
{
  static Action& make (const AttributeList& al)
  { return *new ActionSpray (al); }

  static bool check_alist (const AttributeList& al, Treelog& err)
    {
      bool ok = true;
      const string chemical = al.name ("chemical");
      non_negative (al.number ("amount"), "amount", ok, err);

      const Library& library = Librarian<Chemical>::library ();
      if (!library.check (chemical))
	{
	  err.entry (string ("Unknown chemical '") + chemical + "'");
	  ok = false;
	}
      else
	{
	  const Syntax& syntax = library.syntax (chemical);
	  const AttributeList& alist = library.lookup (chemical);
	  if (!syntax.check (alist, err))
	    {
	      err.entry (string ("Incomplete chemical '") + chemical + "'");
	      ok = false;
	    }
	}
      return ok;
    }
  ActionSpraySyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    syntax.add_check (check_alist);
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "\
Spray a chemical (typically a pesticide) on the field.");
    syntax.add ("chemical", Syntax::String, Syntax::Const,
		"Name of pesticide to spray.");
    syntax.add ("amount", "g/ha", Syntax::Const,
		"Amount of pesticide to spray.");
    syntax.order ("chemical", "amount");
    Librarian<Action>::add_type ("spray", alist, syntax, &make);
  }
} ActionSpray_syntax;

