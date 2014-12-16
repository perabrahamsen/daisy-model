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
#include "vcheck.h"
#include <map>

// The 'scopesel' component.

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

static struct ScopeselInit : public DeclareComponent
{ 
  ScopeselInit ()
    : DeclareComponent (Scopesel::component, "\
A method to choose a scope in a Daisy simulation.")
  { }
} Scopesel_init;

// The 'name' model.

struct ScopeselName : public Scopesel
{
  // Parameters.
  const symbol name;

  // Simulation.
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
  ScopeselName (const BlockModel& al)
    : name (al.name ("name"))
  { }
};

static struct ScopeselNameSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ScopeselName (al); }

  void load_frame (Frame& frame) const
  { 
    frame.declare_string ("name", Attribute::Const,
               "Name of scope to select.");
    frame.order ("name");
  }

  ScopeselNameSyntax ()
    : DeclareModel (Scopesel::component, "name", "Select named scope.")
  { }
} ScopeselName_syntax;

// The 'null' model.

struct ScopeselNull : public Scopesel
{
  // Simulation.
  const Scope* lookup (const std::vector<const Scope*>&, Treelog& msg) const
  { return &Scope::null (); }

  // Create.
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

// The 'multi' model.

struct ScopeselMulti : public Scopesel
{
  const std::vector<symbol> scope_names;

  // Scope
  mutable struct Combine : public Scope
  {
    // Data.
    std::map<symbol, const Scope*> scope_map;
    std::map<symbol, symbol> name_map;

    const Scope& scope (const symbol key) const
    {
      const std::map<symbol, const Scope*>::const_iterator i
        = scope_map.find (key);
      if (i == scope_map.end ())
        daisy_panic ("'" + key + "' nor found");
      return *(*i).second;
    }

    symbol extract_name (const symbol key) const
    {
      const std::map<symbol, symbol>::const_iterator i = name_map.find (key);
      if (i == name_map.end ())
        daisy_panic ("'" + key + "' nor found");
      return (*i).second;
    }

    // Scope interface.
    void entries (std::set<symbol>& all) const
    {
      for (std::map<symbol, symbol>::const_iterator i = name_map.begin ();
           i != name_map.end ();
           i++)
        all.insert ((*i).first);
    }
    Attribute::type lookup (const symbol key) const
    { return scope (key).lookup (extract_name (key)); }
    bool can_extract_as (const symbol key, const Attribute::type type) const
    { return scope (key).can_extract_as (extract_name (key), type); }
    symbol dimension (const symbol key) const
    { return scope (key).dimension (extract_name (key)); }
    symbol description (const symbol key) const
    { return scope (key).description (extract_name (key)); }
    int type_size (const symbol key) const
    { return scope (key).type_size (extract_name (key)); }
    bool check (const symbol key) const
    { return scope (key).check (extract_name (key)); }
    double number (const symbol key) const
    { return scope (key).number (extract_name (key)); }
    int value_size (const symbol key) const
    { return scope (key).value_size (extract_name (key)); }
    symbol name (const symbol key) const
    { return scope (key).name (extract_name (key)); }
    int integer (const symbol key) const
    { return scope (key).integer (extract_name (key)); }

    // Create and destroy.
    bool initialized_ok;

    void initialize (const std::vector<symbol>& scope_names,
                     const std::vector<const Scope*>& scopes, Treelog& msg)
    { 
      bool ok = true;
      scope_map.clear ();
      name_map.clear ();
      
      std::set<symbol> names (scope_names.begin (), scope_names.end ());
      std::set<symbol> found;
      for (size_t i = 0; i < scopes.size (); i++)
        {
          const Scope& scope = *scopes[i];
          const symbol name = scope.title ();
          std::set<symbol>::iterator this_one = names.find (name);
          if (this_one == names.end ())
            continue;

          if (found.find (name) != found.end ())
            {
              ok = false;
              msg.error ("Duplicate scope '" + name + "' ignored");
              continue;
            }
          names.erase (this_one);
          found.insert (name);
          
          std::set<symbol> entries;
          scope.entries (entries);
          for (std::set<symbol>::const_iterator j = entries.begin ();
               j != entries.end ();
               j++)
            {
              const symbol entry = *j;
              const std::string construct = name + "." + entry;
              const symbol x = construct;
              scope_map[x] = &scope;
              name_map[x] = entry;
            }
        }
      for (std::set<symbol>::const_iterator i = names.begin ();
           i != names.end ();
           i++)
        {
          ok = false;
          msg.error ("No scope named '" + *i + "' found");
        }

      if (ok)
        initialized_ok = true;
    }
    bool check ()
    { return initialized_ok; }
    Combine ()
      : initialized_ok (false)
    { }
  } combine;

  // Simulation.
  const Scope* lookup (const std::vector<const Scope*>& scopes, 
                       Treelog& msg) const
  { 
    combine.initialize (scope_names, scopes, msg);
    if (combine.check ())
      return &combine; 
    
    return NULL;
  }

  // Create.
  ScopeselMulti (const BlockModel& al)
    : scope_names (al.name_sequence ("name"))
  { }
};

static struct ScopeselMultiSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ScopeselMulti (al); }

  void load_frame (Frame& frame) const
  { 
    frame.declare_string ("name", Attribute::Const, Attribute::Variable,
               "Names of scope to select.");
    frame.set_check ("name", VCheck::unique ());
    frame.order ("name");
}

  ScopeselMultiSyntax ()
    : DeclareModel (Scopesel::component, "multi", "\
Combine multiple named scopes.\n\
\n\
The entries in the combined scope will have the form <scope>.<key>,\n\
where <scope> is the name of the scope containing the entry, and <key>\n\
is the name of the entry in <scope>.")
  { }
} ScopeselMulti_syntax;


// scopesel.C ends here


