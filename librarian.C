// librarian.C --- Manage model libraries.
// 
// Copyright 2006 Per Abrahamsen and KVL.
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


#include "librarian.h"
#include "library.h"
#include "block.h"
#include "alist.h"
#include "treelog.h"
#include <sstream>

Model* 
BuildBase::build_free (Treelog& msg, const AttributeList& alist, 
                       const std::string& scope_id) const
{
  daisy_assert (alist.check ("type"));
  const symbol type = alist.identifier ("type");
  if (!lib->check (type))
    {
      std::ostringstream tmp;
      tmp << "Library '" << lib->name () << "' contains no model '"
          << type << "'";
      daisy_panic (tmp.str ());
    }
  const Syntax& syntax = lib->syntax (type);
  Block block (syntax, alist, msg, scope_id + ": " + type.name ());
  daisy_assert (syntax.check (alist, msg));
  try
    {  
      Model* result = build_raw (type, block); 
      daisy_assert (block.ok () && result);
      return result;
    }
  catch (const std::string& err)
    { block.error ("Build failed: " + err); }
  catch (const char *const err)
    { block.error ("Build failure: " + std::string (err)); }
  return NULL;
}

Model* 
BuildBase::build_cheat (const AttributeList& parent, 
                        const std::string& key) const
{ return build_free (Treelog::null (), parent.alist (key), key); }

Model* 
BuildBase::build_alist (Block& parent, const AttributeList& alist, 
                        const std::string& scope_id) const
{
  daisy_assert (alist.check ("type"));
  const symbol type = alist.identifier ("type");
  if (!lib->check (type))
    {
      std::ostringstream tmp;
      tmp << "Component '" << lib->name () << "' contains no model '"
          << type << "'";
      daisy_panic (tmp.str ());
    }
  const Syntax& syntax = lib->syntax (type);
  Block nested (parent, syntax, alist, scope_id + ": " + type.name ());
  daisy_assert (syntax.check (alist, nested.msg ()));
  try
    {  return build_raw (type, nested); }
  catch (const std::string& err)
    { nested.error ("Build failed: " + err); }
  catch (const char *const err)
    { nested.error ("Build failure: " + std::string (err)); }
  return NULL;
}

Model* 
BuildBase::build_item (Block& parent, const std::string& key) const
{ return build_alist (parent, parent.alist (key), key); }

std::vector<Model*> 
BuildBase::build_vector (Block& al, const std::string& key) const
{ 
  std::vector<Model*> t;
  const std::vector<AttributeList*>& f (al.alist_sequence (key));
  for (size_t i = 0; i < f.size (); i++)
    t.push_back (build_alist (al, *f[i], sequence_id (key, i)));
  return t;
}

std::vector<const Model*> 
BuildBase::build_vector_const (Block& al, const std::string& key) const
{ 
  std::vector<const Model*> t;
  const std::vector<AttributeList*>& f (al.alist_sequence (key));
  for (size_t i = 0; i < f.size (); i++)
    t.push_back (build_alist (al, *f[i], sequence_id (key, i)));
  return t;
}

void 
BuildBase::add_base (AttributeList& al, const Syntax& syntax) const
{ lib->add_base (al, syntax); }

void 
BuildBase::add_type (const symbol name, AttributeList& al,
                     const Syntax& syntax) const
{ lib->add (name, al, syntax); }

BuildBase::BuildBase (const char *const name, Library::derive_fun derive, 
                      const char *const description)
  : lib (new Library (name, derive, description)),
    count (0)
{ daisy_assert (lib.get ()); }

BuildBase::~BuildBase ()
{ }

// librarian.C ends here

