// action_message.C

#include "action.h"
#include "condition.h"
#include "log.h"
#include "daisy.h"

struct ActionAssert : public Action
{
  Condition& condition;
  const string message;

  void doIt (Daisy& daisy)
    { 
      if (!condition.match (daisy))
	throw (message);
    }

  void output (Log& log) const
    { output_derived (condition, "condition", log); }

  ActionAssert (const AttributeList& al)
    : Action (al),
      condition (Librarian<Condition>::create (al.alist ("condition"))),
      message (al.name ("message"))
    { }

  ~ActionAssert ()
    { delete &condition; }
};

struct ActionMessage : public Action
{
  const string message;

  void doIt (Daisy&)
    { 
      COUT << message << "\n";
    }

  ActionMessage (const AttributeList& al)
    : Action (al),
      message (al.name ("message"))
    { }

  ~ActionMessage ()
    { }
};

struct ActionWarning : public Action
{
  const string message;

  void doIt (Daisy&)
    { 
      COUT << message << "\n";
    }

  ActionWarning (const AttributeList& al)
    : Action (al),
      message (al.name ("message"))
    { }

  ~ActionWarning ()
    { }
};

struct ActionError : public Action
{
  const string message;

  void doIt (Daisy&)
    { throw (message); }

  ActionError (const AttributeList& al)
    : Action (al),
      message (al.name ("message"))
    { }

  ~ActionError ()
    { }
};

static struct ActionMessageSyntax
{
  static Action& make_assert (const AttributeList& al)
    { return *new ActionAssert (al); }
  static Action& make_message (const AttributeList& al)
    { return *new ActionMessage (al); }
  static Action& make_warning (const AttributeList& al)
    { return *new ActionWarning (al); }
  static Action& make_error (const AttributeList& al)
    { return *new ActionError (al); }

  ActionMessageSyntax ()
    {
      {
	Syntax& syntax = *new Syntax ();
	AttributeList& alist = *new AttributeList ();
	alist.add ("description", "\
Assert that condition is true, if not, stop the simulation.");
	syntax.add ("condition", Librarian<Condition>::library (), 
		    "Condition to check.");
	syntax.order ("condition");
	syntax.add ("message", Syntax::String, Syntax::Const,
		    "Error message to give iff assertion fails.");
	alist.add ("message", "Required condition not fulfiled");
	Librarian<Action>::add_type ("assert", alist, syntax, &make_assert);
      }
      {
	Syntax& syntax = *new Syntax ();
	AttributeList& alist = *new AttributeList ();
	alist.add ("description", "\
Write a message to the user.");
	syntax.add ("message", Syntax::String, Syntax::Const,
		    "Message to give to the user.");
	syntax.order ("message");

	Librarian<Action>::add_type ("message", alist, syntax, &make_message);
      }
      {
	Syntax& syntax = *new Syntax ();
	AttributeList& alist = *new AttributeList ();
	alist.add ("description", "\
Write a warning to the user.");
	syntax.add ("message", Syntax::String, Syntax::Const,
		    "Warning to give to the user.");
	syntax.order ("message");

	Librarian<Action>::add_type ("warning", alist, syntax, &make_warning);
      }
      {
	Syntax& syntax = *new Syntax ();
	AttributeList& alist = *new AttributeList ();
	alist.add ("description", "\
Write a error message to the user and stop the simulation.");
	syntax.add ("message", Syntax::String, Syntax::Const,
		    "Error message to give.");
	syntax.order ("message");
	Librarian<Action>::add_type ("error", alist, syntax, &make_error);
      }
    }
} ActionMessage_syntax;
