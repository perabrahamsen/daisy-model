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
#include "block_model.h"
#include "treelog.h"
#include "assertion.h"
#include "memutils.h"
#include "frame_model.h"
#include "filepos.h"
#include <map>
#include <sstream>

struct Library::Implementation
{
  // Id.
  const symbol name;
  symbol description;

  // Types.
  typedef std::map<symbol, boost::shared_ptr<const FrameModel>/**/> frame_map;
  typedef std::map<symbol, std::set<symbol>/**/> ancestor_map;

  // Data (remember to update Library::clone if you change this).
  frame_map frames;
  std::vector<doc_fun> doc_funs;
  ancestor_map ancestors;

  // Accessors.
  const FrameModel& model (symbol) const;
  bool check (symbol) const;
  void add_ancestors (symbol);
  void add_model (symbol, boost::shared_ptr<const FrameModel>);
  void entries (std::vector<symbol>&) const;
  void remove (symbol);
  void clear_parsed ();
  void refile_parsed (const std::string& from, const std::string& to);
  static void load_syntax (Frame&);
  Implementation (const symbol n);
  ~Implementation ();
};

const FrameModel&
Library::Implementation::model (const symbol key) const
{ 
  frame_map::const_iterator i = frames.find (key);

  if (i == frames.end ())
    daisy_panic ("Model '" + key.name () + "' not in library '" 
                 + name.name () + "'");
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
      if (all.find (current) != all.end ())
        daisy_panic ("Cyclic inheritence '" + current 
                     + "' for '" + key + "' in library '" + name + "'");

      all.insert (current);

      if (!check (current))
	break;

      const Frame& frame = this->model (current);

      symbol next = frame.base_name ();
      
      if (next == Attribute::None ())
	break;

      daisy_assert (next != current);

      current = next;
    }
  ancestors[key] = all;
}

void
Library::Implementation::add_model (const symbol key, 
                                    boost::shared_ptr<const FrameModel> frame)
{
  if (frame->type_name () != key)
    daisy_panic ("Adding frame named " 
                 + frame->type_name () + " under the name "
                 + key + ", type " + typeid (*frame).name ());
  frames[key] = frame;
  add_ancestors (key);
}

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
{ 
  const frame_map::iterator i = frames.find (key);
  frames.erase (i); 
}

void
Library::Implementation::clear_parsed ()
{
 retry:
  for (frame_map::iterator i = frames.begin (); i != frames.end (); i++)
    {
      const Frame& frame = *((*i).second);
      if (frame.own_position () != Filepos::none ())
	{
	  // const symbol key = (*i).first;
	  frames.erase (i);
	  goto retry;
	}
    }
}

#if 0
void
Library::Implementation::refile_parsed (const std::string& from, const std::string& to)
{
  daisy_assert (from != to);
  for (frame_map::iterator i = frames.begin (); i != frames.end (); i++)
    {
      const Frame& frame = *((*i).second);
      const Filepos& pos = frame.own_position ();
      if (pos.filename () == from)
        frame.reposition (Filepos (to, pos.line (), pos.column ()));
    }
}
#endif

Library::Implementation::Implementation (const symbol n) 
  : name (symbol (n))
{ }

Library::Implementation::~Implementation ()
{ }

void 
Library::clear_parsed ()
{ impl->clear_parsed (); }

#if 0
void 
Library::refile_parsed (const std::string& from, const std::string& to)
{ impl->refile_parsed (from, to); }
#endif

symbol
Library::name () const
{ return impl->name; }

symbol
Library::description () const
{ return impl->description; }

const FrameModel& 
Library::model (const symbol key) const
{ return impl->model (key); }

bool
Library::check (const symbol key) const
{ return impl->check (key); }

bool
Library::complete (const Metalib& metalib, const symbol key) const
{ 
  if (!check (key))
    return false;

  const FrameModel& frame = model (key);

  if (!frame.check (metalib, Treelog::null ()))
    return false;

  if (!frame.buildable ())
    return false;

  return true;
}

void
Library::add_model (const symbol key, boost::shared_ptr<const FrameModel> frame)
{ impl->add_model (key, frame); }

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

  const FrameModel& frame = this->model (a);
  if (frame.type_name () != a)
    daisy_panic ("'" + a + "' in library present itself as a '" 
                 + frame.type_name () + "', maybe because it is a "
                 + typeid (frame).name ());

  const symbol type = frame.base_name ();

  if (type == Attribute::None ())
    return false;

  if (type == b)
    return true;

  if (!check (type))
    daisy_panic ("'" + a + "' is derived from non-existing '" + type 
                 + "', rather than '" + b + "'");

  if (type == a)
    daisy_panic ("'" + a + "' is derived from itself");

  return is_derived_from (type, b);
}
  
const symbol
Library::base_model (const symbol parameterization) const
{
  const FrameModel& al = this->model (parameterization);

  const symbol base = al.base_name ();
  daisy_assert (base != parameterization);
  if (base != Attribute::None ())
    return base_model (base);
  
  return parameterization;
}

bool 
Library::has_interesting_description (const Frame& frame) const
{
  // A missing description is boring.
  const symbol my_d = frame.description ();
  if (my_d == Attribute::None ())
    return false;
  
  // Top level description are always interesting.
  const symbol base_name = frame.base_name ();
  if (base_name == Attribute::None ())
    return true;
  
  // If the model has no description, this one is interesting.
  if (!check (base_name))
    {
      daisy_bug (name () + " does not have '" + base_name + "' defined");
      return true;
    }
  daisy_assert (check (base_name));
  const Frame& super = model (base_name);
  
  // If the model description is different, this one is interesting.
  return my_d != super.description ();
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
  lib->impl->frames = impl->frames;
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
