// action_merge.C
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

struct ActionMerge : public Action
{
  const symbol combine;
  const symbol remove;

  void doIt (Daisy& daisy, Treelog& out)
  {
    out.message (" [Merging " + remove + " into " + combine + "]");
    daisy.field.merge (combine, remove);
  }
  
  ActionMerge (const AttributeList& al)
    : Action (al),
      combine (al.identifier ("combine")), 
      remove (al.identifier ("remove"))
  { }
};

static struct ActionMergeSyntax
{
  static Action& make (const AttributeList& al)
    { return *new ActionMerge (al); }
  ActionMergeSyntax ()
    { 
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      alist.add ("description", "\
Merge two columns.  After the merge, only the first column will remain,\n\
but its state will be a average of the the columns, weighted after size.");
      syntax.add ("combine", Syntax::String, Syntax::Const,
		  "Column to merge into.");
      syntax.add ("remove", Syntax::String, Syntax::Const,
		  "Column to remove after merge.");
      syntax.order ("combine", "remove");
      Librarian<Action>::add_type ("merge", alist, syntax, &make);
    }
} ActionMerge_syntax;
