// action_ridge.C
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
#include "daisy.h"
#include "field.h"
#include "ridge.h"
#include "message.h"

struct ActionRidge : public Action
{
  const AttributeList& ridge;

  void doIt (Daisy& daisy)
    { 
      COUT << " [Ridging]\n";      
      daisy.field.ridge (ridge); 
    }

  ActionRidge (const AttributeList& al)
    : Action (al),
      ridge (al.alist ("ridge"))
    { }
};

#ifdef BORLAND_TEMPLATES
template class add_submodule<Ridge>;
#endif

// Add the ActionRidge syntax to the syntax table.
static struct ActionRidgeSyntax
{
  static Action& make (const AttributeList& al)
  { return *new ActionRidge (al); }

  ActionRidgeSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "Ridge a ridge on the field.");
    add_submodule<Ridge> ("ridge", syntax, alist, Syntax::Const,
			  "Ridge parameters");
    syntax.order ("ridge");
    Librarian<Action>::add_type ("ridge", alist, syntax, &make);
  }
} ActionRidge_syntax;

