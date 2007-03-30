// action_message.C
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
//
// This file is part of Daisy.
// 
// Daisy is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser Public License as published by
// the Free Software Foundation; either version 2.1 of the License, or
// (at your option) any later version.
// 
// Daisy is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser Public License for more details.
// 
// You should have received a copy of the GNU Lesser Public License
// along with Daisy; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA


#include "action.h"
#include "block.h"
#include "condition.h"
#include "log.h"
#include "daisy.h"

using namespace std;

struct ActionAssert : public Action
{
  auto_ptr<Condition> condition;
  const string message;

  void tick (const Daisy& daisy, Treelog& out)
  { condition->tick (daisy, out); }

  void doIt (Daisy& daisy, Treelog& msg)
  { 
    if (!condition->match (daisy, msg))
      throw (message);
  }

  void output (Log& log) const
  { output_derived (condition, "condition", log); }

  ActionAssert (Block& al)
    : Action (al),
      condition (Librarian<Condition>::build_item (al, "condition")),
      message (al.name ("message"))
  { }

  ~ActionAssert ()
  { }
};

struct ActionMessage : public Action
{
  const string message;

  void doIt (Daisy&, Treelog& out)
  { 
    out.message (message);
  }

  ActionMessage (Block& al)
    : Action (al),
      message (al.name ("message"))
  { }

  ~ActionMessage ()
  { }
};

struct ActionWarning : public Action
{
  const string message;

  void doIt (Daisy&, Treelog& out)
  { 
    out.warning (message);
  }

  ActionWarning (Block& al)
    : Action (al),
      message (al.name ("message"))
  { }

  ~ActionWarning ()
  { }
};

struct ActionError : public Action
{
  const string message;

  void doIt (Daisy&, Treelog&)
  { throw (message); }

  ActionError (Block& al)
    : Action (al),
      message (al.name ("message"))
  { }

  ~ActionError ()
  { }
};

static struct ActionMessageSyntax
{
  static Model& make_assert (Block& al)
  { return *new ActionAssert (al); }
  static Model& make_message (Block& al)
  { return *new ActionMessage (al); }
  static Model& make_warning (Block& al)
  { return *new ActionWarning (al); }
  static Model& make_error (Block& al)
  { return *new ActionError (al); }

  ActionMessageSyntax ()
  {
    {
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      alist.add ("description", "\
Assert that condition is true, if not, stop the simulation.");
      syntax.add_object ("condition", Condition::component, 
                         "Condition to check.");
      syntax.order ("condition");
      syntax.add ("message", Syntax::String, Syntax::Const,
		  "Error message to give iff assertion fails.");
      alist.add ("message", "Required condition not fulfiled");
      BuildBase::add_type (Action::component, "assert", alist, syntax, &make_assert);
    }
    {
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      alist.add ("description", "\
Write a message to the user.");
      syntax.add ("message", Syntax::String, Syntax::Const,
		  "Message to give to the user.");
      syntax.order ("message");

      BuildBase::add_type (Action::component, "message", alist, syntax, &make_message);
    }
    {
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      alist.add ("description", "\
Write a warning to the user.");
      syntax.add ("message", Syntax::String, Syntax::Const,
		  "Warning to give to the user.");
      syntax.order ("message");

      BuildBase::add_type (Action::component, "warning", alist, syntax, &make_warning);
    }
    {
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      alist.add ("description", "\
Write a error message to the user and stop the simulation.");
      syntax.add ("message", Syntax::String, Syntax::Const,
		  "Error message to give.");
      syntax.order ("message");
      BuildBase::add_type (Action::component, "error", alist, syntax, &make_error);
    }
  }
} ActionMessage_syntax;
