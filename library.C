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


#include "library.h"
#include "alist.h"
#include "syntax.h"
#include "treelog.h"
#include "tmpstream.h"
#include <map>
#include <set>

struct Library::Implementation
{
  typedef map<string, Library*, less<string>/**/> library_map;
  static library_map* all;
  static int all_count;

  // We give each parsed object an increasing sequence number.
  static int sequence;

  const string name;
  derive_fun derive;
  const char *const description;
  typedef map<string, AttributeList*, less<string> > alist_map;
  typedef map<string, const Syntax*, less<string> > syntax_map;
  alist_map alists;
  syntax_map syntaxen;
  static void all_entries (vector<string>& libraries);
  AttributeList& lookup (const string&) const;
  bool check (const string&) const;
  void add (const string&, AttributeList&, const Syntax&);
  const Syntax& syntax (const string&) const;
  void entries (vector<string>&) const;
  void remove (const string&);
  void clear_parsed ();
  void refile_parsed (const string& from, const string& to);
  static void load_syntax (Syntax&, AttributeList&);
  Implementation (const char* n, derive_fun d, const char* des);
  ~Implementation ();
};

// map<string, Library*, less<string> >* Library::Implementation::all;
Library::Implementation::library_map* Library::Implementation::all = NULL;
int Library::Implementation::all_count = 0;
int Library::Implementation::sequence = 0;

void
Library::Implementation::all_entries (vector<string>& libraries)
{ 
  for (library_map::const_iterator i = all->begin (); 
       i != all->end ();
       i++)
    libraries.push_back ((*i).first); 
}

AttributeList&
Library::Implementation::lookup (const string& key) const
{ 
  alist_map::const_iterator i = alists.find (key);

  if (i == alists.end ())
    assert (false);

  return *(*i).second;
}

bool
Library::Implementation::check (const string& key) const
{ 
  alist_map::const_iterator i = alists.find (key);

  if (i == alists.end ())
    return false;

  return true;
}

void
Library::Implementation::add (const string& key, AttributeList& value,
			      const Syntax& syntax)
{
  alists[key] = &value;
  syntaxen[key] = &syntax;
}

const Syntax& 
Library::Implementation::syntax (const string& key) const
{ 
  syntax_map::const_iterator i = syntaxen.find (key);

  if (i == syntaxen.end ())
    assert (false);

  return *(*i).second;
}

void
Library::Implementation::entries (vector<string>& result) const
{
  for (syntax_map::const_iterator i = syntaxen.begin ();
       i != syntaxen.end ();
       i++)
    {
      result.push_back ((*i).first);
    }
}

void
Library::Implementation::remove (const string& key)
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
	  string key = (*i).first;
	  syntax_map::iterator j = syntaxen.find (key);
	  assert (j != syntaxen.end ());
	  syntaxen.erase (j);
	  alists.erase (i);
	  delete &alist;
	  goto retry;
	}
    }
}

void
Library::Implementation::refile_parsed (const string& from, const string& to)
{
  assert (from != to);
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

void
Library::Implementation::load_syntax (Syntax& syntax, AttributeList&)
{
  const string def = "def";
  for (library_map::const_iterator i = all->begin (); 
       i != all->end ();
       i++)
    { 
      const string& name = (*i).first;
      Library& library = *((*i).second);
      syntax.add_library (def + name, library);
    }
}

Library::Implementation::Implementation (const char* n, derive_fun d,
					 const char* des) 
  : name (n),
    derive (d),
    description (des)
{
  if (all == NULL)
    {
      assert (all_count == 0);
      all = new library_map ();
    }
  all_count++;
}

Library::Implementation::~Implementation ()
{ 
  // Delete alists.
  map_delete (alists.begin (), alists.end ());

  // Delete unique syntaxen.
  set<const Syntax*, less<const Syntax*>/**/> unique;
  for (syntax_map::iterator i = syntaxen.begin ();
       i != syntaxen.end ();
       i++)
    {
      assert ((*i).second);
      unique.insert ((*i).second);
      (*i).second = NULL;
    }
  sequence_delete (unique.begin (), unique.end ());
  
  // Remove from list of libraries.
  all->erase (all->find (name)); 

  // Delete list of libraries if empty.
  all_count--;
  assert (all->size () == all_count);
  if (all_count == 0)
    delete all;
  else
    assert (all_count > 0);
}

bool
Library::exist (const string& name)
{ return Implementation::all->find (name) != Implementation::all->end (); }

Library& 
Library::find (const string& name)
{ return *(*Implementation::all)[name]; }

void
Library::all (vector<string>& libraries)
{ 
  Implementation::all_entries (libraries);
}

int
Library::get_sequence ()
{ 
  Implementation::sequence++;
  // Nobody will ever need more than two billion objects --- Per 1998.
  assert (Implementation::sequence > 0);
  return Implementation::sequence;
}

const string&
Library::name () const
{ return impl.name; }

const char*
Library::description () const
{ return impl.description; }

AttributeList&
Library::lookup (const string& key) const
{ return impl.lookup (key); }

bool
Library::check (const string& key) const
{ return impl.check (key); }

void
Library::add (const string& key, AttributeList& value, const Syntax& syntax)
{ impl.add (key, value, syntax); }

void 
Library::add_derived (const string& name, AttributeList& al,
		      const string& super)
{ 
  al.add ("type", super);
  impl.derive (name, al, super); 
}

const Syntax& 
Library::syntax (const string& key) const
{ return impl.syntax (key); }

void
Library::entries (vector<string>& result) const
{ impl.entries (result); }

bool 
Library::is_derived_from (const string& a, const string& b) const
{
  const AttributeList& al = lookup (a);

  if (!al.check ("type"))
    return false;

  const string& type = al.name ("type");

  if (type == b)
    return true;

  assert (check (type));
  assert (type != a);

  return is_derived_from (type, b);
}
  
const string 
Library::base_model (const string& parameterization) const
{
  const AttributeList& al = lookup (parameterization);

  if (!al.check ("type"))
    return parameterization;

  return base_model (al.name ("type"));
}


void
Library::remove (const string& key)
{ impl.remove (key); }

void 
Library::clear_all_parsed ()
{
  vector<string> components;
  Library::all (components);

  for (unsigned int i = 0; i < components.size (); i++)
    {
      const string& component = components[i];
      const Library& library = Library::find (component);
      
      library.impl.clear_parsed ();
    }
}

void 
Library::refile_parsed (const string& from, const string& to)
{
  vector<string> components;
  Library::all (components);

  for (unsigned int i = 0; i < components.size (); i++)
    {
      const string& component = components[i];
      const Library& library = Library::find (component);
      
      library.impl.refile_parsed (from, to);
    }
}

void 
Library::load_syntax (Syntax& syntax, AttributeList& alist)
{ Implementation::load_syntax (syntax, alist); }

Library::Library (const char* name, derive_fun derive, 
		  const char* description) 
  : impl (*new Implementation (name, derive, description))
{ 
  (*Implementation::all)[name] = this; 
  assert (Implementation::all->size () == Implementation::all_count);
}

Library::~Library ()
{ 
  delete &impl; 
}
