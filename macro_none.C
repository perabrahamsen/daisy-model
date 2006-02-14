// macro_none.C -- No preferential flow.
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


#include "macro.h"

struct MacroNone : public Macro
{
  bool none () const
  { return true; }

  // Simulation.
  void tick (const Soil& /* soil */, 
	     unsigned int /* first */, unsigned int /* last */,
	     Surface& /* surface */,
	     const std::vector<double>& /* h_ice */,
	     const std::vector<double>& /* h */,
	     const std::vector<double>& /* Theta */,
	     std::vector<double>& /* S */,
	     std::vector<double>& /* S_p */,
	     std::vector<double>& /* q_p */,
	     Treelog&)
    { }
  void output (Log&) const
    { }

  // Create and Destroy.
  MacroNone (Block& al)
    : Macro (al)
    { }
  ~MacroNone ()
    { }
};

static struct MacroNoneSyntax
{
  static Macro&
  make (Block& al)
    { return *new MacroNone (al); }
  MacroNoneSyntax ()
    {
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      alist.add ("description", "No macropores.");
      Librarian<Macro>::add_type ("none", alist, syntax, &make);
    }
} MacroNone_syntax;
