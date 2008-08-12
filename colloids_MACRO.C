// colloids_MACRO.C -- Colloids calculated by the MACRO model 
// 
// Copyright 2008 Birgitte Gjettermann and KU
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
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.See the
// GNU Lesser Public License for more details.
// 
// You should have received a copy of the GNU Lesser Public License
// along with Daisy; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

#define BUILD_DLL

#include "colloids.h"
#include "mathlib.h"
#include <sstream>
#include "check.h"
#include "block.h"
#include "librarian.h"

struct Colloids_MACRO : public Colloids
{
  // Parameters.
  const double Do;
private:
  // Simulation.
  double colloids_ (const double D, Treelog&);

  void output (Log&) const { }

  const AttributeList& default_model ();

  // Create.
  public:
  Colloids_MACRO (Block& al)
    : Colloids (al),
      Do (al.number ("Do"))
  { }
};

double
Colloids_MACRO::colloids_ (const double /*[]*/, Treelog& msg)
{
  return 1.0;
}

static struct ColloidsMACROSyntax
{
  static Model& make (Block& al)
  { return *new Colloids_MACRO (al); }
  static void load_syntax (Syntax& syntax, AttributeList& alist)
  {
    syntax.add ("Do", "[]", Check::non_negative (), Syntax::Const,
                "Coefficient, value after MACRO (1995)");
    alist.add ("Do", 1500.);

  }  
  ColloidsMACROSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", 
	       "Colloids in flowing soil water calculated by the MACRO model.");

    load_syntax (syntax, alist);

    Librarian::add_type (Colloids::component, "MACRO", alist, syntax, &make);
  }
} ColloidsMACROsyntax;


const AttributeList& 
Colloids::default_model ()
{
  static AttributeList alist;
  
  if (!alist.check ("type"))
    {
      Syntax syntax;
      ColloidsMACROSyntax::load_syntax (syntax, alist);
      alist.add ("type", "MACRO");
    }
  return alist;
}
