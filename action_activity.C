// action_activity.C

#include "action.h"
#include "log.h"

struct ActionActivity : public Action
{
  vector<Action*>& actions;

  void doIt (Daisy& daisy)
    { 
      if (actions.size () == 0U)
	return;

      Action* action = actions.front ();
      action->doIt (daisy);

      if (action->done (daisy))
	{
	  delete action;
	  actions.erase (actions.begin ());
	}
    }

  bool done (const Daisy&) const
    { return (actions.size () == 0U); }

  void output (Log& log) const
    { 
      output_list (actions, "actions", log, 
		   Librarian<Action>::library ());
    }

  bool check (const Daisy& daisy, Treelog& err) const
    { 
      bool ok = true;
      for (vector<const Action*>::const_iterator i = actions.begin ();
	   i != actions.end ();
	   i++)
	{
	  if (!(*i)->check (daisy, err))
	    ok = false;
	}
      return ok;
    }

  ActionActivity (const AttributeList& al)
    : Action (al),
      actions (map_create<Action> (al.alist_sequence ("actions")))
    { }

  ~ActionActivity ()
    { 
      sequence_delete (actions.begin (), actions.end ());
      delete &actions;
    }
};

static struct ActionActivitySyntax
{
  static Action& make (const AttributeList& al)
    { return *new ActionActivity (al); }

  ActionActivitySyntax ()
    {
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      alist.add ("description", "\
Perform all the specified actions in the sequence listed.  Each\n\
action is performed until done.  At most one action can be performed\n\
at each time step.");
      syntax.add ("actions", Librarian<Action>::library (), Syntax::Sequence,
		  "Sequence of actions to perform.");
      alist.add ("actions", vector<AttributeList*> ());
      syntax.order ("actions");
      Librarian<Action>::add_type ("activity", alist, syntax, &make);
    }
} ActionActivity_syntax;
