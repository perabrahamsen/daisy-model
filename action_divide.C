// action_divide.C
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
#include "message.h"

struct ActionDivide : public Action
{
  const string original;
  const string copy;
  const double size;

  void doIt (Daisy& daisy)
    {
      COUT << "[Dividing " << original << " into " << copy << "]\n";
      daisy.field.divide (original, copy, size, daisy.time, daisy.weather);
    }

  ActionDivide (const AttributeList& al)
    : Action (al),
      original (al.name ("original")), 
      copy (al.name ("copy")),
      size (al.number ("size"))
    { }
};

static struct ActionDivideSyntax
{
  static Action& make (const AttributeList& al)
    { return *new ActionDivide (al); }
  ActionDivideSyntax ()
    { 
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      alist.add ("description", "\
Divide an existing column into two, thus creating a new column.\n\
The 'size' argument specifies the size of the new column, which must be\n\
smaller than the size of the original column.");
      syntax.add ("original", Syntax::String, Syntax::Const,
		  "Column to divide");
      syntax.add ("copy", Syntax::String, Syntax::Const,
		  "Name of new column.");
      syntax.add ("size", Syntax::Unknown (), Syntax::Const,
		  "Size of the partition to remove.");
      syntax.order ("original", "copy", "size");
      Librarian<Action>::add_type ("divide", alist, syntax, &make);
    }
} ActionDivide_syntax;
