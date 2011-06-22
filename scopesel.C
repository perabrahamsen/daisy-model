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

#define BUILD_DLL

#include "scopesel.h"
#include "scope.h"
#include "assertion.h"
#include "block_model.h"
#include "treelog.h"
#include "librarian.h"
#include "frame.h"

const char *const Scopesel::component = "scopesel";

symbol
Scopesel::library_id () const
{
  static const symbol id (component);
  return id;
}

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
  const Scope* lookup (const std::vector<const Scope*>& scopes, 
                       Treelog& msg) const
  { 
    int found = -1;
    for (size_t i = 0; i < scopes.size (); i++)
      {
        const Scope& scope = *scopes[i];

        if (scope.title () == name)
          {
            if (found < 0)
              found = i;
            else
              {
                msg.error ("Duplicate scope: '" + name.name () + "'");
                return NULL;
              }
          }
      }
    if (found < 0)
      return NULL;
    return scopes[found]; 
  }

  // Create.
public:
  ScopeselName (const BlockModel& al)
    : name (al.name ("frame"))
  { }
};

static struct ScopeselNameSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ScopeselName (al); }

  void load_frame (Frame& frame) const
  { 
    frame.declare_string ("frame", Attribute::Const,
               "Name of scope to select.");
    frame.order ("frame");
  }

  ScopeselNameSyntax ()
    : DeclareModel (Scopesel::component, "name", "Select named scope.")
  { }
} ScopeselName_syntax;

class ScopeselNull : public Scopesel
{
  // Simulation.
public:
  Scope* lookup (const std::vector<const Scope*>&, Treelog& msg) const
  { return &Scope::null (); }

  // Create.
public:
  ScopeselNull (const BlockModel&)
  { }
};

static struct ScopeselNullSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ScopeselNull (al); }

  void load_frame (Frame&) const
  { }

  ScopeselNullSyntax ()
    : DeclareModel (Scopesel::component, "null", "Select the empty scope.")
  { }
} ScopeselNull_syntax;


static struct ScopeselInit : public DeclareComponent
{ 
  ScopeselInit ()
    : DeclareComponent (Scopesel::component, "\
A method to choose a scope in a Daisy simulation.")
  { }
} Scopesel_init;

// scopesel.C ends here


