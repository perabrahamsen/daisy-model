// action.C -- Manager actions
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
#include "librarian.h"

const char *const Action::component = "action";

void
Action::tick (const Daisy&, const Scope&, Treelog&)
{ }

void 
Action::output (Log&) const
{ }

bool
Action::done (const Daisy&, const Scope&, Treelog&) const
{ return true; }

void
Action::initialize (const Daisy&, const Scope&, Treelog&)
{ }

bool
Action::check (const Daisy&, const Scope&, Treelog&) const
{ return true; }

Action::Action (Block& al)
  : name (al.identifier ("type")),
    alist (al.alist ())
{ }

Action::Action (Block&, const AttributeList& al)
  : name (al.identifier ("type")),
    alist (al)
{ }

Action::~Action ()
{ }

static Librarian Action_init (Action::component, "\
The 'action' component represents management on different abstraction\n\
levels, from a single tillage operation to strategies of how to manage\n\
a farm.  Typically, but not necessarily, the high level management\n\
strategies are build by combining low level management operations.");
