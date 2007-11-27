// library.C
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

#include "library.h"
#include "block.h"
#include "alist.h"
#include "syntax.h"
#include "treelog.h"
#include <sstream>
#include "assertion.h"
#include "symbol.h"
#include "memutils.h"
#include <map>
#include <sstream>

struct Library::Implementation
{
  // Id.
  const symbol name;
  const char* description;

  // Types.
  typedef std::map<symbol, builder> bmap_type;
  typedef std::map<symbol, AttributeList*> alist_map;
  typedef std::map<symbol, const Syntax*> syntax_map;
  typedef std::map<symbol, std::set<symbol>/**/> ancestor_map;

  // Data (remember to update Library::clone if you change this).
  bmap_type builders;
  alist_map alists;
  syntax_map syntaxen;
  std::vector<doc_fun> doc_funs;
  ancestor_map ancestors;

  // Accessors.
  AttributeList& lookup (symbol) const;
  bool check (symbol) const;
  void add_ancestors (symbol);
  void add_base (AttributeList&, const Syntax&);
  void add (symbol, AttributeList&, const Syntax&, builder);
  const Syntax& syntax (symbol) const;
  void entries (std::vector<symbol>&) const;
  void remove (symbol);
  void clear_parsed ();
  void refile_parsed (const std::string& from, const std::string& to);
  static void load_syntax (Syntax&, AttributeList&);
  Implementation (const char* n);
  ~Implementation ();
};

AttributeList&
Library::Implementation::lookup (const symbol key) const
{ 
  alist_map::const_iterator i = alists.find (key);

  if (i == alists.end ())
    daisy_panic ("Model '" + key.name ()
                 + "' not in library '" + name.name () + "'");
  return *(*i).second;
}

bool
Library::Implementation::check (const symbol key) const
{ 
  alist_map::const_iterator i = alists.find (key);

  if (i == alists.end ())
    return false;

  return true;
}

void
Library::Implementation::add_ancestors (const symbol key)
{
  std::set<symbol> all;
  
  symbol current = key;

  while (true)
    {
      all.insert (current);

      if (!check (current))
	break;

      const AttributeList& alist = lookup (current);

      symbol next;

      if (alist.check ("type"))
	next = alist.identifier ("type");
      else if (alist.check ("base_model"))
	next = alist.identifier ("base_model");
      else
	break;

      if (next == current)
	break;

      current = next;
    }
  ancestors[key] = all;
}

void
Library::Implementation::add_base (AttributeList& value,
				   const Syntax& syntax)
{
  daisy_assert (value.check ("base_model"));
  const symbol key = value.identifier ("base_model");
  alists[key] = &value;
  syntaxen[key] = &syntax;
  add_ancestors (key);
}

void
Library::Implementation::add (const symbol key, AttributeList& value,
			      const Syntax& syntax, builder build)
{
  alists[key] = &value;
  syntaxen[key] = &syntax;
  builders[key] = build;
  add_ancestors (key);
}

const Syntax& 
Library::Implementation::syntax (const symbol key) const
{ 
  syntax_map::const_iterator i = syntaxen.find (key);

  if (i == syntaxen.end ())
    daisy_panic ("'" + key + "' not found");

  return *(*i).second;
}

void
Library::Implementation::entries (std::vector<symbol>& result) const
{
  for (syntax_map::const_iterator i = syntaxen.begin ();
       i != syntaxen.end ();
       i++)
    {
      result.push_back ((*i).first);
    }
}

void
Library::Implementation::remove (const symbol key)
{
  alists.erase (alists.find (key));
  syntaxen.erase (syntaxen.find (key));
}

void
Library::Implementation::clear_parsed ()
{
 retry:
  for (alist_map::iterator i = alists.begin (); i != alists.end (); i++)
    {
      AttributeList& alist = *((*i).second);
      if (alist.check ("parsed_from_file"))
	{
	  const symbol key = (*i).first;
	  syntax_map::iterator j = syntaxen.find (key);
	  daisy_assert (j != syntaxen.end ());
	  syntaxen.erase (j);
	  alists.erase (i);
	  delete &alist;
	  goto retry;
	}
    }
}

void
Library::Implementation::refile_parsed (const std::string& from, const std::string& to)
{
  daisy_assert (from != to);
  for (alist_map::iterator i = alists.begin (); i != alists.end (); i++)
    {
      AttributeList& alist = *((*i).second);
      if (alist.check ("parsed_from_file")
	  && alist.name ("parsed_from_file") == from)
	{
	  alist.add ("parsed_from_file", to);
	}
    }
}

Library::Implementation::Implementation (const char* n) 
  : name (symbol (n)),
    description (NULL)
{ }

Library::Implementation::~Implementation ()
{ 
  // Delete alists.
  map_delete (alists.begin (), alists.end ());

  // Delete unique syntaxen.
  std::set<const Syntax*> unique;
  for (syntax_map::iterator i = syntaxen.begin ();
       i != syntaxen.end ();
       i++)
    {
      daisy_assert ((*i).second);
      unique.insert ((*i).second);
      (*i).second = NULL;
    }
  sequence_delete (unique.begin (), unique.end ());
}

void 
Library::clear_parsed ()
{ impl->clear_parsed (); }

void 
Library::refile_parsed (const std::string& from, const std::string& to)
{ impl->refile_parsed (from, to); }

symbol
Library::name () const
{ return impl->name; }

const char*
Library::description () const
{ return impl->description; }

AttributeList&
Library::lookup (const symbol key) const
{ return impl->lookup (key); }

bool
Library::check (const symbol key) const
{ return impl->check (key); }

bool
Library::complete (const Metalib& metalib, const symbol key) const
{ 
  if (!check (key))
    return false;

  if (!syntax (key).check (metalib, lookup (key), Treelog::null ()))
    return false;

  return true;
}

void
Library::add_base (AttributeList& value, const Syntax& syntax)
{ impl->add_base (value, syntax); }

void
Library::add (const symbol key, AttributeList& value, const Syntax& syntax,
              builder build)
{ impl->add (key, value, syntax, build); }

void 
Library::add_derived (const symbol name, AttributeList& al,
		      const symbol super)
{ 
  add_derived (name, syntax (super), al, super); 
}

void
Library::add_derived (const symbol name, const Syntax& syn, AttributeList& al,
		      const symbol super)
{ 
  al.add ("type", super);
  add (name, al, syn, impl->builders[super]); 
}

const Syntax& 
Library::syntax (const symbol key) const
{ return impl->syntax (key); }

void
Library::entries (std::vector<symbol>& result) const
{ impl->entries (result); }

const std::set<symbol>& 
Library::ancestors (symbol key) const
{ return impl->ancestors[key]; }


bool 
Library::is_derived_from (const symbol a, const symbol b) const
{
  if (a == b)
    return true;

  const AttributeList& al = lookup (a);

  if (!al.check ("type") && !al.check ("base_model"))
    return false;

  const symbol type = al.check ("type") 
    ? al.identifier ("type") 
    : al.identifier ("base_model");

  if (type == b)
    return true;

  daisy_assert (check (type));

  if (type == a)
    return false;

  return is_derived_from (type, b);
}
  
const symbol
Library::base_model (const symbol parameterization) const
{
  const AttributeList& al = lookup (parameterization);

  if (al.check ("type"))
    return base_model (al.identifier ("type"));
  if (al.check ("base_model")
      && al.identifier ("base_model") != parameterization)
    return  base_model (al.identifier ("base_model"));

  return parameterization;
}

bool 
Library::has_interesting_description (const AttributeList& alist) const
{
  // A missing description is boring.
  if (!alist.check ("description"))
    return false;
  
  // The description of models are always interesting.
  if (!alist.check ("type"))
    return true;
  
  // If the model has no description, this one is interesting.
  const symbol type = alist.identifier ("type");
  if (!check (type))
    {
      daisy_bug (name () + " does not have " + type.name ());
      return false;
    }
  daisy_assert (check (type));
  const AttributeList& super = lookup (type);
  if (!super.check ("description"))
    return true;
  
  // If the model description is different, this one is interesting.
  return alist.name ("description") != super.name ("description");
}

void
Library::add_doc_fun (doc_fun fun) 
{ impl->doc_funs.push_back (fun); }

std::vector<Library::doc_fun>& 
Library::doc_funs () const
{ return impl->doc_funs; }

void
Library::remove (const symbol key)
{ impl->remove (key); }

Model* 
Library::build_raw (const symbol type, Block& block) const
{ 
  const Implementation::bmap_type::const_iterator i 
    = impl->builders.find (type);
  if  (i == impl->builders.end ())
    {
      std::ostringstream tmp;
      tmp << "No '" << type.name () << "' found in '"  << impl->name.name()
          << "' library";
      daisy_panic (tmp.str ());
    }
  return &(*i).second (block);
}

void 
Library::set_description (const char *const description)
{
  daisy_assert (!impl->description);
  impl->description = description;
}

Library* 
Library::clone () const
{ 
  Library *const lib = new Library (impl->name.name ().c_str ());
  lib->set_description (impl->description);
  lib->impl->builders = impl->builders;
  for (Implementation::alist_map::const_iterator i = impl->alists.begin ();
       i != impl->alists.end ();
       i++)
    lib->impl->alists[(*i).first] = new AttributeList (*(*i).second);
  for (Implementation::syntax_map::const_iterator i = impl->syntaxen.begin ();
       i != impl->syntaxen.end ();
       i++)
    lib->impl->syntaxen[(*i).first] = new Syntax (*(*i).second);
  lib->impl->doc_funs = impl->doc_funs;
  lib->impl->ancestors = impl->ancestors;
  return lib;
}

Library::Library (const char* name) 
  : impl (new Implementation (name))
{ }

Library::~Library ()
{ }

// library.C ends here
