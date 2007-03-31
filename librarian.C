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
#include "metalib.h"
#include "intrinsics.h"
#include "block.h"
#include "alist.h"
#include "treelog.h"
#include "assertion.h"
#include <sstream>
#include <map>

void 
BuildBase::non_null (const void *const p)
{ daisy_assert (p); }

Intrinsics* BuildBase::content = 0;  

const Intrinsics& 
BuildBase::intrinsics ()
{ 
  daisy_assert (content);
  return *content;
}

Model* 
BuildBase::build_free (const char *const component, Metalib& metalib,
                       Treelog& msg, const AttributeList& alist, 
                       const std::string& scope_id)
{
  daisy_assert (alist.check ("type"));
  const symbol type = alist.identifier ("type");
  const Library& lib = metalib.library (component);

  if (!lib.check (type))
    {
      std::ostringstream tmp;
      tmp << "Library '" << lib.name () << "' contains no model '"
          << type << "'";
      daisy_panic (tmp.str ());
    }
  const Syntax& syntax = lib.syntax (type);
  Block block (metalib, msg, syntax, alist, scope_id + ": " + type.name ());
  daisy_assert (syntax.check (metalib, alist, Treelog::null ()));
  try
    { return lib.build_raw (type, block); }
  catch (const std::string& err)
    { block.error ("Build failed: " + err); }
  catch (const char *const err)
    { block.error ("Build failure: " + std::string (err)); }
  return NULL;
}

Model* 
BuildBase::build_alist (const char *const component,
                        Block& parent, const AttributeList& alist, 
                        const std::string& scope_id)
{
  daisy_assert (alist.check ("type"));
  const symbol type = alist.identifier ("type");
  const Library& lib = parent.metalib ().library (component);
  if (!lib.check (type))
    {
      std::ostringstream tmp;
      tmp << "Component '" << lib.name () << "' contains no model '"
          << type << "'";
      daisy_panic (tmp.str ());
    }
  const Syntax& syntax = lib.syntax (type);
  Block nested (parent, syntax, alist, scope_id + ": " + type.name ());
  daisy_assert (syntax.check (parent.metalib (), alist, Treelog::null ()));
  try
    {  return lib.build_raw (type, nested); }
  catch (const std::string& err)
    { nested.error ("Build failed: " + err); }
  catch (const char *const err)
    { nested.error ("Build failure: " + std::string (err)); }
  return NULL;
}

Model* 
BuildBase::build_item (const char *const component,
                       Block& parent, const std::string& key)
{ return build_alist (component, parent, parent.alist (key), key); }

std::vector<Model*> 
BuildBase::build_vector (const char *const component,
                         Block& al, const std::string& key)
{ 
  std::vector<Model*> t;
  const std::vector<AttributeList*>& f (al.alist_sequence (key));
  for (size_t i = 0; i < f.size (); i++)
    t.push_back (build_alist (component, al, *f[i], sequence_id (key, i)));
  return t;
}

std::vector<const Model*> 
BuildBase::build_vector_const (const char *const component,
                               Block& al, const std::string& key)
{ 
  std::vector<const Model*> t;
  const std::vector<AttributeList*>& f (al.alist_sequence (key));
  for (size_t i = 0; i < f.size (); i++)
    t.push_back (build_alist (component, al, *f[i], sequence_id (key, i)));
  return t;
}

Library& 
BuildBase::library (const char* component)
{
  if (!content)
    content = new Intrinsics ();

  return content->add (component);
}
 

void 
BuildBase::add_base (const char *const component,
                     AttributeList& al, const Syntax& syntax)
{ 
  library (component).add_base (al, syntax); 
  daisy_assert (!content->closed);
}

void 
BuildBase::add_type (const char *const component,
                     const symbol name, AttributeList& al,
                     const Syntax& syntax, builder build)
{
  library (component).add (name, al, syntax, build); 
  daisy_assert (!content->closed);
}

void
BuildBase::add_type (const char *const component,
                     const char *const name, AttributeList& al,
                     const Syntax& syntax, builder build)
{
  library (component).add (symbol (name), al, syntax, build); 
  daisy_assert (!content->closed);
}

void 
BuildBase::add_alias (const char *const component,
                      const symbol derived, const symbol base)
{
  Library& lib = library (component);

  daisy_assert (lib.check (base));
  daisy_assert (!lib.check (derived));
  AttributeList& alist = *new AttributeList (lib.lookup (base));
  alist.add ("description", 
             "The " + derived.name ()
             + " model is an alias for " + base.name () + ".");
  lib.add_derived (derived, alist, base);
  daisy_assert (!content->closed);
}

void 
BuildBase::add_doc_fun (const char *const component, const doc_fun fun)
{
  library (component).add_doc_fun (fun); 
  daisy_assert (!content->closed);
} 

void
BuildBase::load_syntax (Syntax& syntax, AttributeList&)
{
  const std::string def = "def";
  for (std::map<symbol, Library*>::const_iterator i = content->all.begin (); 
       i != content->all.end ();
       i++)
    { 
      const symbol name = (*i).first;
      syntax.add_library (def + name, name);
    }
}

BuildBase::BuildBase (const char *const component,
                      const char *const description)
{
  library (component).set_description (description);
  content->count++;
}

BuildBase::~BuildBase ()
{ 
  daisy_assert (content);
  daisy_assert (content->count > 0);
  content->count--;

  if (content->count == 0)
    {
      delete content;
      content = NULL;
    }
}

// librarian.C ends here
