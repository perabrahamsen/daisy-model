// action_spray.C -- Spray a chemical on the field.
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
#include "metalib.h"
#include "library.h"
#include "block.h"
#include "daisy.h"
#include "field.h"
#include "chemical.h"
#include "check.h"
#include "librarian.h"
#include <sstream>

struct ActionSpray : public Action
{
  const symbol chemical;
  const double amount;

  void doIt (Daisy& daisy, const Scope&, Treelog& msg)
  {
    std::ostringstream tmp;
    tmp << "Spraying " << amount << " g " << chemical << "/ha";
    msg.message (tmp.str ());
    daisy.field->spray (chemical, amount, daisy.dt, msg); 
  }

  void tick (const Daisy&, const Scope&, Treelog&)
  { }
  void initialize (const Daisy&, const Scope&, Treelog&)
  { }
  bool check (const Daisy&, const Scope&, Treelog&) const
  { return true; }

  ActionSpray (Block& al)
    : Action (al),
      chemical (al.identifier ("chemical")),
      amount (al.number ("amount"))
  { }
};

// Add the ActionSpray syntax to the syntax table.
static struct ActionSpraySyntax
{
  static Model& make (Block& al)
  { return *new ActionSpray (al); }

  ActionSpraySyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "\
Spray a chemical (typically a pesticide) on the field.");
    syntax.add ("chemical", Syntax::String, Syntax::Const,
		"Name of pesticide to spray.");
    syntax.add_check ("chemical", Chemical::check_library ());
    syntax.add ("amount", "g/ha", Check::non_negative (), Syntax::Const,
		"Amount of pesticide to spray.");
    syntax.order ("chemical", "amount");
    Librarian::add_type (Action::component, "spray", alist, syntax, &make);
  }
} ActionSpray_syntax;

// action_spray.C ends here.
