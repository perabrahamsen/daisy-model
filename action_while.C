// action_while.C

#include "action.h"
#include "log.h"

struct ActionWhile : public Action
{
  const vector<Action*>& actions;

  void doIt (Daisy& daisy)
    { 
      for (unsigned int i = 0; i < actions.size (); i++)
	actions[i]->doIt (daisy);
    }

  bool done (const Daisy& daisy) const
    {
      assert (actions.size () != 0U);
      return (actions[0]->done (daisy)); 
    }

  void output (Log& log) const
    { 
      output_list (actions, "actions", log, 
		   Librarian<Action>::library ());
    }

  bool check (const Daisy& daisy, Treelog& err) const
    { 
      Treelog::Open nest (err, "while");
      bool ok = true;
      for (vector<Action*>::const_iterator i = actions.begin ();
	   i != actions.end ();
	   i++)
	{
	  if (!(*i)->check (daisy, err))
	    ok = false;
	}
      return ok;
    }

  ActionWhile (const AttributeList& al)
    : Action (al),
      actions (map_create<Action> (al.alist_sequence ("actions")))
    { }

  ~ActionWhile ()
    { 
      sequence_delete (actions.begin (), actions.end ());
      delete &actions;
    }
};

static struct ActionWhileSyntax
{
  static Action& make (const AttributeList& al)
    { return *new ActionWhile (al); }

  static bool check_alist (const AttributeList& al, Treelog& err)
    {
      bool ok = true;

      if (al.size ("actions") < 1)
	{
	  err.entry ("You must specify at least one action");
	  ok = false;
	}
      return ok;
    }

  ActionWhileSyntax ()
    {
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      syntax.add_check (check_alist);
      alist.add ("description", "\
Perform all the specified actions in the sequence listed, but in the\n\
same timestep.  The `while' action is done when the first action in the\n\
list is done.");
      syntax.add ("actions", Librarian<Action>::library (), Syntax::Sequence,
		  "List of actions to perform.");
      syntax.order ("actions");
      Librarian<Action>::add_type ("while", alist, syntax, &make);
    }
} ActionWhile_syntax;
