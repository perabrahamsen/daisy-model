// action_repeat.C

#include "action.h"
#include "log.h"

struct ActionRepeat : public Action
{
  const AttributeList repeat;
  Action* action;

  void doIt (Daisy& daisy)
  { 
    if (action && action->done (daisy))
      {
	delete action;
	action = NULL;
      }
    if (action == NULL)
      action = &Librarian<Action>::create (repeat);
    action->doIt (daisy);
  }

  bool done (const Daisy&) const
  { 
    return false;
  }

  void output (Log& log) const
  { 
    output_derived (*action, "do", log);
  }

  bool check (const Daisy& daisy, Treelog& err) const
  { 
    if (action)
      return action->check (daisy, err);
    else
      return true;
  }

  static AttributeList add_do (const AttributeList& al)
  {
    AttributeList alist (al);
    if (!alist.check ("do"))
      alist.add ("do", alist.alist ("repeat"));
    return alist;
  }

  ActionRepeat (const AttributeList& al)
    : Action (add_do (al)),
      repeat (al.alist ("repeat")),
      action (&Librarian<Action>::create (al.check ("do") 
					  ? al.alist ("do")
					  : repeat))
  { }

  ~ActionRepeat ()
  { 
    if (action)
      delete action;
  }
};

static struct ActionRepeatSyntax
{
  static Action& make (const AttributeList& al)
  { return *new ActionRepeat (al); }

  ActionRepeatSyntax ()
    {
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      alist.add ("description", "\
Perform all the specified action.  When done, repeat the action.\n\
The action may take several timesteps.");
      syntax.add ("repeat", Librarian<Action>::library (),
		  Syntax::Const, Syntax::Singleton,
		  "Action to perform repeatedly.");
      syntax.add ("do", Librarian<Action>::library (), 
		  Syntax::OptionalState, Syntax::Singleton,
		  "Action currently being performed.");
      syntax.order ("repeat");
      Librarian<Action>::add_type ("repeat", alist, syntax, &make);
    }
} ActionRepeat_syntax;
