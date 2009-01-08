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
#include "assertion.h"
#include "memutils.h"
#include "frame_model.h"
#include <map>
#include <sstream>

struct Library::Implementation
{
  // Id.
  const symbol name;
  symbol description;

  // Types.
  typedef std::map<symbol, FrameModel*> frame_map;
  typedef std::map<symbol, std::set<symbol>/**/> ancestor_map;

  // Data (remember to update Library::clone if you change this).
  frame_map frames;
  std::vector<doc_fun> doc_funs;
  ancestor_map ancestors;

  // Accessors.
  AttributeList& lookup (symbol) const;
  const FrameModel& model (symbol) const;
  Frame& frame (symbol) const;
  bool check (symbol) const;
  void add_ancestors (symbol);
  void add_base (AttributeList&, const Syntax&);
  void add_model (symbol, FrameModel*);
  const Syntax& syntax (symbol) const;
  void entries (std::vector<symbol>&) const;
  void remove (symbol);
  void clear_parsed ();
  void refile_parsed (const std::string& from, const std::string& to);
  static void load_syntax (Syntax&, AttributeList&);
  Implementation (const symbol n);
  ~Implementation ();
};

AttributeList&
Library::Implementation::lookup (const symbol key) const
{ return frame (key).alist (); }

const FrameModel&
Library::Implementation::model (const symbol key) const
{ 
  frame_map::const_iterator i = frames.find (key);

  if (i == frames.end ())
    daisy_panic ("Model '" + key.name ()
                 + "' not in library '" + name.name () + "'");
  return *(*i).second;
}

Frame&
Library::Implementation::frame (const symbol key) const
{ 
  frame_map::const_iterator i = frames.find (key);

  if (i == frames.end ())
    daisy_panic ("Frame '" + key.name ()
                 + "' not in library '" + name.name () + "'");
  return *(*i).second;
}

bool
Library::Implementation::check (const symbol key) const
{ 
  frame_map::const_iterator i = frames.find (key);

  if (i == frames.end ())
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

      const Frame& frame = this->frame (current);

      symbol next;

      if (frame.check ("type"))
	next = frame.name ("type");
      else if (frame.check ("base_model"))
	next = frame.name ("base_model");
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
  const symbol key = value.name ("base_model");
  frames[key] = new FrameModel (syntax, value);
  add_ancestors (key);
}

void
Library::Implementation::add_model (const symbol key, FrameModel *const frame)
{
  frames[key] = frame;
  add_ancestors (key);
}

const Syntax& 
Library::Implementation::syntax (const symbol key) const
{ return frame (key).syntax (); }

void
Library::Implementation::entries (std::vector<symbol>& result) const
{
  for (frame_map::const_iterator i = frames.begin ();
       i != frames.end ();
       i++)
    {
      result.push_back ((*i).first);
    }
}

void
Library::Implementation::remove (const symbol key)
{ frames.erase (frames.find (key)); }

void
Library::Implementation::clear_parsed ()
{
 retry:
  for (frame_map::iterator i = frames.begin (); i != frames.end (); i++)
    {
      Frame& frame = *((*i).second);
      if (frame.check ("parsed_from_file"))
	{
	  const symbol key = (*i).first;
	  frames.erase (i);
	  delete &frame;
	  goto retry;
	}
    }
}

void
Library::Implementation::refile_parsed (const std::string& from, const std::string& to)
{
  daisy_assert (from != to);
  for (frame_map::iterator i = frames.begin (); i != frames.end (); i++)
    {
      Frame& frame = *((*i).second);
      if (frame.check ("parsed_from_file")
	  && frame.name ("parsed_from_file") == from)
	{
	  frame.add ("parsed_from_file", to);
	}
    }
}

Library::Implementation::Implementation (const symbol n) 
  : name (symbol (n))
{ }

Library::Implementation::~Implementation ()
{ 
  // Delete frames.
  map_delete (frames.begin (), frames.end ());
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

symbol
Library::description () const
{ return impl->description; }

const FrameModel& 
Library::model (const symbol key) const
{ return impl->model (key); }

Frame& 
Library::frame (const symbol key) const
{ return impl->frame (key); }

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

  if (!frame (key).check (metalib, Treelog::null ()))
    return false;

  return true;
}

void
Library::add_base (AttributeList& value, const Syntax& syntax)
{ impl->add_base (value, syntax); }

void
Library::add_model (const symbol key, AttributeList& value, 
                    const Syntax& syntax, builder build)
{ impl->add_model (key, new FrameModel (syntax, value, build)); }

void 
Library::add_derived (const symbol name, AttributeList& al,
		      const symbol super)
{ 
  al.add ("type", super);
  daisy_assert (check (super));
  impl->add_model (name, new FrameModel (model (super), al));
}

void
Library::add_derived (const symbol name, const Syntax& syn, AttributeList& al,
		      const symbol super)
{ 
  al.add ("type", super);
  daisy_assert (check (super));
  impl->add_model (name, new FrameModel (model (super), syn, al)); 
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
    ? al.name ("type") 
    : al.name ("base_model");

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
    return base_model (al.name ("type"));
  if (al.check ("base_model")
      && al.name ("base_model") != parameterization)
    return  base_model (al.name ("base_model"));

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
  const symbol type = alist.name ("type");
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

void 
Library::set_description (const symbol description)
{ impl->description = description; }

Library* 
Library::clone () const
{ 
  Library *const lib = new Library (impl->name.name ().c_str ());
  lib->set_description (impl->description);
  for (Implementation::frame_map::const_iterator i = impl->frames.begin ();
       i != impl->frames.end ();
       i++)
    lib->impl->frames[(*i).first] = new FrameModel (*(*i).second, 
                                                    FrameModel::parent_copy);
  lib->impl->doc_funs = impl->doc_funs;
  lib->impl->ancestors = impl->ancestors;
  return lib;
}

Library::Library (const symbol name) 
  : impl (new Implementation (name))
{ }

Library::~Library ()
{ }

// library.C ends here
