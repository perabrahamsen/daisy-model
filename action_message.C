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

#define BUILD_DLL

#include "action.h"
#include "block.h"
#include "condition.h"
#include "log.h"
#include "daisy.h"
#include "librarian.h"
#include "treelog.h"
#include "frame.h"

struct ActionAssert : public Action
{
  std::auto_ptr<Condition> condition;
  const symbol message;

  void tick (const Daisy& daisy, const Scope& scope, Treelog& out)
  { condition->tick (daisy, scope, out); }

  void doIt (Daisy& daisy, const Scope& scope, Treelog& msg)
  { 
    if (!condition->match (daisy, scope, msg))
      throw (message);
  }

  void output (Log& log) const
  { output_derived (condition, "condition", log); }

  void initialize (const Daisy& daisy, const Scope& scope, Treelog& out)
  { condition->initialize (daisy, scope, out); }

  bool check (const Daisy& daisy, const Scope& scope, Treelog& out) const
  { return condition->check (daisy, scope, out); }

  ActionAssert (Block& al)
    : Action (al),
      condition (Librarian::build_item<Condition> (al, "condition")),
      message (al.name ("message"))
  { }

  ~ActionAssert ()
  { }
};

struct ActionMessage : public Action
{
  const symbol message;

  void doIt (Daisy&, const Scope&, Treelog& out)
  { 
    out.message (message.name ());
  }

  void tick (const Daisy&, const Scope&, Treelog&)
  { }
  void initialize (const Daisy&, const Scope&, Treelog&)
  { }
  bool check (const Daisy&, const Scope&, Treelog& err) const
  { return true; }

  ActionMessage (Block& al)
    : Action (al),
      message (al.name ("message"))
  { }

  ~ActionMessage ()
  { }
};

struct ActionWarning : public Action
{
  const symbol message;

  void doIt (Daisy&, const Scope&, Treelog& out)
  { 
    out.warning (message.name ());
  }

  void tick (const Daisy&, const Scope&, Treelog&)
  { }
  void initialize (const Daisy&, const Scope&, Treelog&)
  { }
  bool check (const Daisy&, const Scope&, Treelog& err) const
  { return true; }

  ActionWarning (Block& al)
    : Action (al),
      message (al.name ("message"))
  { }

  ~ActionWarning ()
  { }
};

struct ActionError : public Action
{
  const symbol message;

  void doIt (Daisy&, const Scope&, Treelog& out)
  { 
    out.error (message.name ());
  }

  void tick (const Daisy&, const Scope&, Treelog&)
  { }
  void initialize (const Daisy&, const Scope&, Treelog&)
  { }
  bool check (const Daisy&, const Scope&, Treelog& err) const
  { return true; }

  ActionError (Block& al)
    : Action (al),
      message (al.name ("message"))
  { }

  ~ActionError ()
  { }
};

struct ActionPanic : public Action
{
  const symbol message;

  void doIt (Daisy&, const Scope&, Treelog& msg)
  { 
    msg.touch ();
    throw message; 
  }

  void tick (const Daisy&, const Scope&, Treelog&)
  { }
  void initialize (const Daisy&, const Scope&, Treelog&)
  { }
  bool check (const Daisy&, const Scope&, Treelog& err) const
  { return true; }

  ActionPanic (Block& al)
    : Action (al),
      message (al.name ("message"))
  { }

  ~ActionPanic ()
  { }
};

static struct ActionAssertSyntax : public DeclareModel
{
  Model* make (Block& al) const
  { return new ActionAssert (al); }
  ActionAssertSyntax ()
    : DeclareModel (Action::component, "assert", "\
Assert that condition is true, if not, stop the simulation.")
  { }
  void load_frame (Frame& frame) const
  {
      frame.add_object ("condition", Condition::component, 
                         "Condition to check.");
      frame.order ("condition");
      frame.add ("message", Value::String, Value::Const,
		  "Error message to give iff assertion fails.");
      frame.add ("message", "Required condition not fulfilled");
  }
} ActionAssert_syntax;

static struct ActionMessageSyntax : public DeclareModel
{
  Model* make (Block& al) const
  { return new ActionMessage (al); }
  ActionMessageSyntax ()
    : DeclareModel (Action::component, "message", "\
Write a message to the user.")
  { }
  void load_frame (Frame& frame) const
  {
      frame.add ("message", Value::String, Value::Const,
		  "Message to give to the user.");
      frame.order ("message");
  }
} ActionMessage_syntax;

static struct ActionWarningSyntax : public DeclareModel
{
  Model* make (Block& al) const
  { return new ActionWarning (al); }
  ActionWarningSyntax ()
    : DeclareModel (Action::component, "warning", "\
Write a warning to the user.")
  { }
  void load_frame (Frame& frame) const
  {
      frame.add ("message", Value::String, Value::Const,
		  "Warning to give to the user.");
      frame.order ("message");
  }
} ActionWarning_syntax;

static struct ActionErrorSyntax : public DeclareModel
{
  Model* make (Block& al) const
  { return new ActionError (al); }
  ActionErrorSyntax ()
    : DeclareModel (Action::component, "error", "\
Write a error message to the user.")
  { }
  void load_frame (Frame& frame) const
  {
      frame.add ("message", Value::String, Value::Const,
		  "Error message to give.");
      frame.order ("message");
  }
} ActionError_syntax;

static struct ActionPanicSyntax : public DeclareModel
{
  Model* make (Block& al) const
  { return new ActionPanic (al); }
  ActionPanicSyntax ()
    : DeclareModel (Action::component, "panic", "\
Write a error message to the user and stop the simulation.")
  { }
  void load_frame (Frame& frame) const
  {
      frame.add ("message", Value::String, Value::Const,
		  "Error message to give.");
      frame.order ("message");
  }
} ActionPanic_syntax;

// action_message.C ends here.

