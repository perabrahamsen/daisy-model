// action_sow.C

#include "action.h"
#include "daisy.h"
#include "frame.h"
#include "column.h"
#include "chemical.h"

struct ActionSpray : public Action
{
  const string chemical;
  const double amount;

  void doIt (const Frame& frame, Daisy& daisy)
    {
      COUT << " [Spraying " << chemical << " at";

      ColumnList& cl = daisy.columns;
      for (ColumnList::iterator i = cl.begin (); i != cl.end (); i++)
	{ 
	  if (frame.match_column (**i))
	    {
	      (*i)->spray (chemical, amount); 
	      COUT << " " << (*i)->name;
	    }
	}
      COUT << "]\n";
    }

  bool check (Daisy&) const
    { return true; }

  ActionSpray (const AttributeList& al)
    : Action (al.name ("type")),
      chemical (al.name ("chemical")),
      amount (al.number ("amount"))
    { }
};

// Add the ActionSpray syntax to the syntax table.
static struct ActionSpraySyntax
{
  static Action& make (const AttributeList& al)
  { return *new ActionSpray (al); }

  static bool check_alist (const AttributeList& al)
    {
      bool ok = true;
      const string chemical = al.name ("chemical");
      non_negative (al.number ("amount"), "amount", ok);

      const Library& library = Librarian<Chemical>::library ();
      if (!library.check (chemical))
	{
	  CERR << "Unknown chemical `" << chemical << "'\n";
	  ok = false;
	}
      else
	{
	  const Syntax& syntax = library.syntax (chemical);
	  const AttributeList& alist = library.lookup (chemical);
	  if (!syntax.check (alist))
	    {
	      CERR << "Incomplete chemical `" << chemical << "'\n";
	      ok = false;
	    }
	}
      return ok;
    }
  ActionSpraySyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    syntax.add ("chemical", Syntax::String, Syntax::Const,
		"Name of pesticide to spray.");
    syntax.add ("amount", "g/ha", Syntax::Const,
		"Amount of pesticide to spray.");
    syntax.order ("chemical", "amount");
    Librarian<Action>::add_type ("spray", alist, syntax, &make);
  }
} ActionSpray_syntax;

