// action_sow.C
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
#include "crop.h"

using namespace std;

struct ActionSow : public Action
{
  const AttributeList& crop;

  void doIt (Daisy& daisy, Treelog& msg)
  { 
    msg.message (string ("Sowing ") + crop.name ("type"));      
    daisy.field.sow (msg, crop); 
  }

  ActionSow (const AttributeList& al)
    : Action (al),
      crop (al.alist ("crop"))
  { }
};

// Add the ActionSow syntax to the syntax table.
static struct ActionSowSyntax
{
  static Action& make (const AttributeList& al)
  { return *new ActionSow (al); }

  ActionSowSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "Sow a crop on the field.");
    syntax.add ("crop", Librarian<Crop>::library (), "Crop to sow.");
    syntax.order ("crop");
    Librarian<Action>::add_type ("sow", alist, syntax, &make);
  }
} ActionSow_syntax;

