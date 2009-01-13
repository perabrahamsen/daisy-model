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

#define BUILD_DLL

#include "librarian.h"
#include "library.h"
#include "metalib.h"
#include "intrinsics.h"
#include "block.h"
#include "alist.h"
#include "treelog_text.h"
#include "assertion.h"
#include "librarian.h"
#include "frame_model.h"
#include <sstream>
#include <map>

void 
Librarian::non_null (const void *const p)
{ daisy_assert (p); }

Intrinsics* Librarian::content = 0;  

const Intrinsics& 
Librarian::intrinsics ()
{ 
  daisy_assert (content);
  return *content;
}

Model* 
Librarian::build_free (const symbol component, Metalib& metalib,
                       Treelog& msg, const AttributeList& alist, 
                       symbol scope_id)
{
  daisy_assert (alist.check ("type"));
  const symbol type = alist.name ("type");
  const Library& lib = metalib.library (component);

  if (!lib.check (type))
    {
      std::ostringstream tmp;
      tmp << "Library '" << lib.name () << "' contains no model '"
          << type << "'";
      daisy_panic (tmp.str ());
    }
  const FrameModel frame (lib.model (type), alist);
  {
    TreelogString tlog;
    if (!frame.check (metalib, tlog))
      {
        msg.error (tlog.str ());
        return NULL;
      }
  }
  Block parent (metalib, msg, frame, scope_id + ": " + type);
  std::auto_ptr<Model> m (frame.construct (parent, type)); 
  if (!parent.ok ())
    return NULL;
  return m.release ();
}

Model* 
Librarian::build_alist (const symbol component,
                        Block& parent, const AttributeList& alist, 
                        symbol scope_id)
{
  daisy_assert (alist.check ("type"));
  const symbol type = alist.name ("type");
  const Library& lib = parent.metalib ().library (component);
  if (!lib.check (type))
    {
      std::ostringstream tmp;
      tmp << "Component '" << lib.name () << "' contains no model '"
          << type << "'";
      daisy_panic (tmp.str ());
    }
  const FrameModel frame (lib.model (type), alist);
  return frame.construct (parent, type); 
}

Model* 
Librarian::build_alist (const symbol component,
                        Block& parent, const AttributeList& alist, 
                        symbol scope_id, size_t index)
{
  daisy_assert (alist.check ("type"));
  const symbol type = alist.name ("type");
  const Library& lib = parent.metalib ().library (component);
  if (!lib.check (type))
    {
      std::ostringstream tmp;
      tmp << "Component '" << lib.name () << "' contains no model '"
          << type << "'";
      daisy_panic (tmp.str ());
    }
  const FrameModel frame (lib.model (type), alist);
  return frame.construct (parent, type); 
}

Model* 
Librarian::build_item (const symbol component,
                       Block& parent, symbol key)
{ return build_alist (component, parent, parent.alist (key), key); }

std::vector<Model*> 
Librarian::build_vector (const symbol component,
                         Block& al, symbol key)
{ 
  std::vector<Model*> t;
  const std::vector<const AttributeList*>& f (al.alist_sequence (key));
  for (size_t i = 0; i < f.size (); i++)
    t.push_back (build_alist (component, al, *f[i], key, i));
  return t;
}

std::vector<const Model*> 
Librarian::build_vector_const (const symbol component,
                               Block& al, symbol key)
{ 
  std::vector<const Model*> t;
  const std::vector<const AttributeList*>& f (al.alist_sequence (key));
  for (size_t i = 0; i < f.size (); i++)
    t.push_back (build_alist (component, al, *f[i], key, i));
  return t;
}

Library& 
Librarian::library (symbol component)
{
  if (!content)
    content = new Intrinsics ();

  return content->add (component.name ().c_str ());
}
 

void 
Librarian::add_base (const symbol component,
                     AttributeList& al, const Syntax& syntax)
{ 
  library (component).add_base (al, syntax); 
  daisy_assert (!content->closed);
}

void 
Librarian::add_type (const symbol component,
                     const symbol name, AttributeList& al,
                     const Syntax& syntax, builder build)
{
  library (component).add_model (name, al, syntax, build); 
  daisy_assert (!content->closed);
}

void 
Librarian::add_alias (const symbol component,
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
Librarian::add_doc_fun (const symbol component, const doc_fun fun)
{
  library (component).add_doc_fun (fun); 
  daisy_assert (!content->closed);
} 

void
Librarian::load_syntax (Syntax& syntax, AttributeList&)
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

void 
Librarian::declare (const symbol component, const symbol name, 
                    const Declare& declaration)
{ 
  if (!content)
    content = new Intrinsics ();

  content->declare (component, name, declaration);
  daisy_assert (!content->closed);
}

Librarian::Librarian (const symbol component, const symbol description)
{
  library (component).set_description (description.name ().c_str ());
  content->count++;
}

Librarian::~Librarian ()
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

void 
Declare::load (Frame& frame) const
{
  frame.alist ().add ("description", description);
  load_frame (frame);
  if (dynamic_cast<const DeclareBase*> (this))
    frame.alist ().add ("base_model", "Farquhar");
}

symbol 
Declare::root_name ()
{
  static const symbol name ("component");
  return name;
}

const FrameModel* 
Declare::parent_model () const
{ return NULL; }

Declare::Declare (const symbol c, const symbol name,
                  const symbol d)
  : component (c),
    description (d)
{ Librarian::declare (component, name, *this); }

Declare::~Declare ()
{ }

void 
DeclareComponent::load_frame (Frame&) const
{ }

DeclareComponent::DeclareComponent (const symbol component,
                                    const symbol description)
  : Declare (component, root_name (), description),
    librarian (component, description)
{ }

const FrameModel* 
DeclareSuper::parent_model () const
{ 
  Librarian::intrinsics ().instantiate (component, super);
  return &Librarian::intrinsics ().library (component).model (super); 
}

DeclareSuper::DeclareSuper (const symbol component,
                            const symbol name, const symbol s, 
                            const symbol description)
  : Declare (component, name, description),
    super (s)
{ }

DeclareBase::DeclareBase (const symbol component,
                            const symbol name, const symbol s, 
                            const symbol description)
  : DeclareSuper (component, name, s, description)
{ }

DeclareBase::DeclareBase (const symbol component, 
                            const symbol name, 
                            const symbol description)
  : DeclareSuper (component, name, root_name (), description)
{ }

DeclareModel::DeclareModel (const symbol component,
                            const symbol name, const symbol s, 
                            const symbol description)
  : DeclareSuper (component, name, s, description)
{ }

DeclareModel::DeclareModel (const symbol component, 
                            const symbol name, 
                            const symbol description)
  : DeclareSuper (component, name, root_name (), description)
{ }

DeclareModel::~DeclareModel ()
{ }

DeclareParam::DeclareParam (symbol component, symbol name, symbol super, 
                            symbol description)
  : DeclareSuper (component, name, super, description)
{ }

void DeclareAlias::load_frame (Frame&) const
{ }

DeclareAlias::DeclareAlias (symbol component, symbol name, symbol super)
  : DeclareParam (component, name, super, "'" + name
                  + "' is anothor name for '" + super + "'")
{ }

// librarian.C ends here
