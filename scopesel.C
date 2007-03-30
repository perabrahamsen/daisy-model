// scopesel.C -- Select a scope.
// 
// Copyright 2007 Per Abrahamsen and KVL.
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


#include "scopesel.h"
#include "scope.h"
#include "assertion.h"
#include "output.h"
#include "block.h"
#include "alist.h"

const char *const Scopesel::description = "\
A method to choose a scope in a Daisy simulation.";

const char *const Scopesel::component = "scopesel";

Scopesel::Scopesel ()
{ }

Scopesel::~Scopesel ()
{ }

class ScopeselName : public Scopesel
{
  // Parameters.
private: 
  const symbol name;

  // Simulation.
public:
  Scope* lookup (const Output& output, Treelog& msg) const
  { 
    int found = -1;
    for (size_t i = 0; i < output.scope_size (); i++)
      {
        const Scope& scope = output.scope (i);

        if (scope.title () == name)
          if (found < 0)
            found = i;
          else
            {
              msg.error ("Duplicate scope: '" + name.name () + "'");
              return NULL;
            }
      }
    if (found < 0)
      return NULL;
    return &output.scope (found); 
  }

  // Create.
public:
  ScopeselName (Block& al)
    : name (al.identifier ("name"))
  { }
};

static struct ScopeselNameSyntax
{
  static Model& make (Block& al)
  { return *new ScopeselName (al); }

  ScopeselNameSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "Select named scope.");
    syntax.add ("name", Syntax::String, Syntax::Const,
                "Name of scope to select.");
    syntax.order ("name");
    BuildBase::add_type (Scopesel::component, "name", alist, syntax, &make);
  }
} ScopeselName_syntax;

// scopesel.C ends here


