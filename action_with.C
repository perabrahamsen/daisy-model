// action_with.C --- restrict actions to a specific columns

#include "action.h"
#include "daisy.h"
#include "syntax.h"
#include "alist.h"
#include "field.h"

struct ActionWithColumn : public Action
{
  const string column;
  vector<Action*>& actions;

public:
  void doIt (Daisy& daisy)
    { 
      Field::Restrict restriction (daisy.field, column);
      for (vector<Action*>::iterator i = actions.begin ();
	   i != actions.end ();
	   i++)
	{
	  (*i)->doIt (daisy);
	}
    }
  bool check (const Daisy& daisy, Treelog& err) const
    { 
      Treelog::Open nest (err, string ("with") + column);
      bool ok = true;
      for (vector<const Action*>::const_iterator i = actions.begin ();
	   i != actions.end ();
	   i++)
	{
	  if (!(*i)->check (daisy, err))
	    ok = false;
	}
      if (!daisy.field.find (column))
	{
	  err.entry (string ("No column '") + column + "'");
	  ok = false;
	}
      return ok;
    }

  ActionWithColumn (const AttributeList& al)
    : Action (al),
      column (al.name ("column")),
      actions (map_create<Action> (al.alist_sequence ("actions")))
    { }
public:
  ~ActionWithColumn ()
    { }
};

static struct ActionWithColumnSyntax
{
  static Action& make (const AttributeList& al)
    { return *new ActionWithColumn (al); }

  ActionWithColumnSyntax ()
    { 
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      alist.add ("description", "Perform actions on a specific column.");
      syntax.add ("column", Syntax::String, Syntax::Const, 
		  "Name of column to perform actions on.");
      syntax.add ("actions", Librarian<Action>::library (), Syntax::Sequence,
		  "Actions to perform on the specified column.");
      syntax.order ("column", "actions");
      Librarian<Action>::add_type ("with-column", alist, syntax, &make);
    }
} ActionWithColumn_syntax;
