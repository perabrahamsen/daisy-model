// action_sow.C

#include "action.h"
#include "daisy.h"
#include "frame.h"
#include "column.h"
// We need to initialize the Crop library.
#include "crop.h"

struct ActionSow : public Action
{
  const AttributeList& crop;

  void doIt (const Frame& frame, Daisy& daisy)
    {
      cout << " [Sowing " << crop.name ("type") << " at";

      ColumnList& cl = daisy.columns;
      for (ColumnList::iterator i = cl.begin (); i != cl.end (); i++)
	{ 
	  if (frame.match_column (**i))
	    {
	      (*i)->sow (crop); 
	      cout << " " << (*i)->name;
	    }
	}
      cout << "]\n";
    }

  bool check (Daisy&) const
    { 
      const string name = crop.name ("type");

      // Does it exists?
      const Library& library = Librarian<Crop>::library ();
      if (!library.check (name))
	{
	  cerr << "Cannot sow unknown crop `" << name << "'\n";
	  return false;
	}

      // Is it complete?
      const Syntax& syntax = library.syntax (name);
      if (!syntax.check (crop, name))
	{
	  cerr << "Cannot sow incomplete crop `" << name << "'\n";
	  return false;
	}

      return true;
    }

  ActionSow (const AttributeList& al)
    : Action (al.name ("type")),
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
    syntax.add ("crop", Librarian<Crop>::library (), Syntax::Const);
    syntax.order ("crop");
    Librarian<Action>::add_type ("sow", alist, syntax, &make);
  }
} ActionSow_syntax;

