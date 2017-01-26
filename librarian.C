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
#include "block_top.h"
#include "treelog_text.h"
#include "assertion.h"
#include "librarian.h"
#include "frame_model.h"
#include "block_model.h"
#include "block_top.h"
#include <sstream>
#include <map>

void 
Librarian::non_null (const Block& block, const void *const p)
{ 
  if (!p)
    block.error ("Build failed");
}

Intrinsics* Librarian::content = 0;  

const Intrinsics& 
Librarian::intrinsics ()
{ 
  daisy_assert (content);
  return *content;
}

void 
Librarian::submodel_instantiate (const load_syntax_t load_syntax)
{
  if (!content)
    content = new Intrinsics ();

  content->submodel_instantiate (load_syntax);
}

bool
Librarian::submodel_registered (const symbol name)
{ 
  daisy_assert (content);
  return content->submodel_registered (name); 
}


boost::shared_ptr<const FrameSubmodel> 
Librarian::submodel_frame (const symbol name)
{
  daisy_assert (content);
  return content->submodel_frame (name); 
}


boost::shared_ptr<const FrameSubmodel> 
Librarian::submodel_frame (const load_syntax_t load_syntax)
{
  daisy_assert (content);
  return content->submodel_frame (load_syntax); 
}

symbol
Librarian::submodel_name (const load_syntax_t load_syntax)
{
  daisy_assert (content);
  return content->submodel_name (load_syntax); 
}

Librarian::load_syntax_t
Librarian::submodel_load (const symbol name)
{
  daisy_assert (content);
  return content->submodel_load (name); 
}

symbol
Librarian::submodel_description (const symbol name)
{ 
  daisy_assert (content);
  return content->submodel_description (name); 
}

void 
Librarian::declare_submodel (const load_syntax_t load_syntax, 
                             const symbol name, const symbol desc)
{
  if (!content)
    content = new Intrinsics ();

  content->submodel_declare (load_syntax, name, desc);
}

void
Librarian::submodel_all (std::vector<symbol>& all)
{
  daisy_assert (content);
  return content->submodel_all (all); 
}
  
Model*
Librarian::build_frame (const symbol component,
                        const Block& parent, const FrameModel& frame, 
                        symbol scope_id)
{ return frame.construct (parent, scope_id); }

Model*
Librarian::build_frame (const symbol component,
                        const Block& parent, const FrameModel& frame, 
                        symbol scope_id, size_t index)
{
  std::ostringstream tmp;
  tmp << scope_id << "[" << index << "]";
  return frame.construct (parent, tmp.str ()); 
}

Model* 
Librarian::build_frame (const symbol component, const Metalib& metalib,
                        Treelog& msg, const FrameModel& frame,
                        const symbol scope_id)
{
  Treelog::Open nest (msg, scope_id);

  // Check.
  {
    TreelogString tlog;
    if (!frame.check (metalib, tlog))
      {
        msg.error (tlog.str ());
        return NULL;
      }
  }
  
  // Build.
  BlockTop parent (metalib, msg, frame);
  const symbol type = frame.type_name ();
  std::unique_ptr<Model> m (frame.construct (parent, type)); 
  if (!parent.ok ())
    return NULL;
  return m.release ();
}

Model* 
Librarian::build_stock (const symbol component, const Metalib& metalib,
                        Treelog& msg, const symbol type, 
                        const symbol scope_id)
{
  const Library& lib = metalib.library (component);

  if (!lib.check (type))
    {
      std::ostringstream tmp;
      tmp << "Library '" << lib.name () << "' contains no model '"
          << type << "'";
      daisy_panic (tmp.str ());
    }
  const FrameModel& frame = lib.model (type);
  return build_frame (component, metalib, msg, frame, scope_id + ": " + type);
}

Model* 
Librarian::build_item (const symbol component,
                       const Block& parent, symbol key)
{ return build_frame (component, parent, parent.model (key), key); }

std::vector<Model*> 
Librarian::build_vector (const symbol component,
                         const Block& al, symbol key)
{ 
  std::vector<Model*> t;
  const std::vector<boost::shared_ptr<const FrameModel>/**/>& f = al.model_sequence (key);
  for (size_t i = 0; i < f.size (); i++)
    t.push_back (build_frame (component, al, *f[i], key, i));
  return t;
}

std::vector<const Model*> 
Librarian::build_vector_const (const symbol component,
                               const Block& al, symbol key)
{ 
  std::vector<const Model*> t;
  const std::vector<boost::shared_ptr<const FrameModel>/**/>& f = al.model_sequence (key);
  for (size_t i = 0; i < f.size (); i++)
    t.push_back (build_frame (component, al, *f[i], key, i));

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
Librarian::add_doc_fun (const symbol component, const doc_fun fun)
{
  library (component).add_doc_fun (fun); 
  daisy_assert (!content->closed);
} 

void 
Librarian::declare (const symbol component, const symbol name, 
                    const Declare& declaration)
{ 
  if (!content)
    content = new Intrinsics ();

  content->declare_model (component, name, declaration);
  daisy_assert (!content->closed);
}

Librarian::Librarian (const symbol component, const symbol description)
{
  library (component).set_description (description.name ().c_str ());
  content->count++;
}

Librarian::~Librarian ()
{ 
  daisy_safe_assert (content);
  daisy_safe_assert (content->count > 0);
  content->count--;

  if (content->count == 0 && content)
    {
      delete content;
      content = NULL;
    }
}

class FrameDeclared : public FrameModel
{
  const Declare& declaration;
  symbol component () const
  { return declaration.component; }
  symbol type_name () const
  { return declaration.name; }
  using Frame::description;
  symbol description () const
  { return declaration.description_; }

  bool used_to_be_a_submodel () const
  { return declaration.used_to_be_a_submodel (); }

  
protected:
  FrameDeclared (const FrameDeclared& frame, parent_clone_t)
    : FrameModel (frame, parent_clone),
      declaration (frame.declaration)
  { }
public:
  explicit FrameDeclared (const Declare& declare)
    : FrameModel (*declare.parent_model (), parent_link),
      declaration (declare)
  { declare.load (*this); }
};

class FrameBuildable : public FrameDeclared
{
  const Declare::Builder& builder;
  bool buildable () const
  { return true; }
  Model* construct (const Block& context, const symbol key, 
                    const FrameModel& frame) const
  {
    BlockModel block (context, frame, key);

    if (!frame.check (context))
      return NULL;

    try
      { return builder.make (block); }
    catch (const std::string& err)
      { block.error ("Build failed: " + err); }
    catch (const char *const err)
      { block.error ("Build failure: " + std::string (err)); }
    return NULL;
  }
  explicit FrameBuildable (const FrameBuildable& frame, const parent_clone_t)
    : FrameDeclared (frame, parent_clone),
      builder (frame.builder)
  { }
public:
  FrameBuildable (const Declare& declare,
                  const Declare::Builder& build)
    : FrameDeclared (declare),
      builder (build)
  { }
  FrameBuildable& clone () const
  { return *new FrameBuildable (*this, parent_clone); }
};

Declare::Builder::~Builder ()
{ }

symbol 
Declare::root_name ()
{
  static const symbol name ("component");
  return name;
}

Declare::Declare (const symbol c, const symbol n,
                  const symbol d)
  : component (c),
    name (n),
    description_ (d)
{ Librarian::declare (component, name, *this); }

Declare::~Declare ()
{ }

bool 
Declare::used_to_be_a_submodel () const
{ return false; }

boost::shared_ptr<const FrameModel>
Declare::create_frame () const 
{ 
  boost::shared_ptr<const FrameModel> share (new FrameDeclared (*this));
  return share; 
}

void 
DeclareComponent::load (Frame& frame) const
{ load_frame (frame); }

void 
DeclareComponent::load_frame (Frame&) const
{ }

const FrameModel* 
DeclareComponent::parent_model () const
{ return &FrameModel::root (); }

DeclareComponent::DeclareComponent (const symbol component,
                                    const symbol description)
  : Declare (component, root_name (), description),
    librarian (component, description)
{ }

boost::shared_ptr<const FrameModel>
DeclareSolo::create_frame () const
{ 
  boost::shared_ptr<const FrameModel> share (new FrameBuildable (*this, *this));
  return share; 
}

DeclareSolo::DeclareSolo (const symbol component, const symbol description)
  : DeclareComponent (component, description)
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
{ 
  if (name == super)
    daisy_panic ("'" + name + "' has itself as superclass in '"
                 + component + "'");
}

void 
DeclareBase::load (Frame& frame) const
{ load_frame (frame); }

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

boost::shared_ptr<const FrameModel>
DeclareModel::create_frame () const 
{
  boost::shared_ptr<const FrameModel> share (new FrameBuildable (*this, *this));
  return share; 
}

void 
DeclareModel::load (Frame& frame) const
{ load_frame (frame); }

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

void 
DeclareParam::load (Frame& frame) const
{ load_frame (frame); }

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

DeclareSubmodel::DeclareSubmodel (const Librarian::load_syntax_t load_syntax,
                                  const symbol name, const symbol description)
{ Librarian::declare_submodel (load_syntax, name, description); }

DeclareSubmodel::~DeclareSubmodel ()
{ }

// librarian.C ends here
