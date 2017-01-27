// intrinsics.C -- The build in models of Daisy.
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

#include "intrinsics.h"
#include "assertion.h"
#include "library.h"
#include "memutils.h"
#include "frame_submodel.h"
#include "librarian.h"
#include <sstream>

std::map<symbol, Library*> 
Intrinsics::clone () const
{ 
  // Instantiate delayed declarations.
  for (declare_map_t::iterator i = delayed.begin ();
       i != delayed.end (); 
       i++)
    {
      const symbol component = (*i).first;
      Library& library = this->library (component);
      declare_lib_map_t& libd = (*i).second;
      for (declare_lib_map_t::iterator j = libd.begin ();
           j != libd.end ();
           j++)
        {
          const symbol model = (*j).first;
          std::vector<const Declare*>& decls = (*j).second;
          if (decls.size () == 0)
            continue;
          if (decls.size () != 1)
            daisy_panic ("'" + model + "' declared multiple times in "
                         + component);
          const Declare& declare = *decls[0];
          library.add_model (model, declare.create_frame ());
          decls.erase (decls.begin ());
        }
    }

  // Clear delayed.
  delayed = declare_map_t ();

  // Close for new definitions.
  closed = true;

  // Clone all libraries.
  std::map<symbol, Library*> result;
  
  for (std::map<symbol, Library*>::const_iterator i = all.begin ();
       i != all.end ();
       i++)
    result[(*i).first] = (*i).second->clone ();

  return result;
}

Library&
Intrinsics::add (const symbol component)
{
  daisy_assert (!closed);
  const std::map<symbol, Library*>::const_iterator i
    = all.find (component);
  
  if (i != all.end ())
    return *(*i).second;

  Library *const lib = new Library (component);
  all[symbol (component)] = lib;
  return *lib;
}

Library&
Intrinsics::library (const symbol component) const
{
  const std::map<symbol, Library*>::const_iterator i
    = all.find (component);
  if (i == all.end ())
    daisy_panic ("Component '" + component + "' not found");
  
  return *(*i).second;
}

void 
Intrinsics::declare_model (const symbol component, const symbol model,
                           const Declare& declaration)
{ 
  daisy_assert (!closed);
  delayed[component][model].push_back (&declaration); 
}

void 
Intrinsics::instantiate (const symbol component, const symbol model) const
{ 
  declare_map_t::iterator i = delayed.find (component);
  if (i == delayed.end ())
    {
      daisy_assert (library (component).check (model));
      return;
    }

  daisy_assert (component == (*i).first);
  Library& library = this->library (component);
  declare_lib_map_t& libd = (*i).second;
  declare_lib_map_t::iterator j = libd.find (model);
  if (j == libd.end ())
    {
      if (!library.check (model))
        daisy_panic ("Intrinsic model '" + model
                     + "' not declared in component '" + component + "'");
      return;
    }

  daisy_assert (model == (*j).first);
  std::vector<const Declare*>& decls = (*j).second;

  if (decls.size () == 0)
    {
      if (!library.check (model))
        daisy_panic ("Intrinsic model '" + model
                     + "' not found for component '" + component + "'");
      return;
    }
  else if (decls.size () > 1)
    {
      std::ostringstream tmp;
      tmp << "Multiple decls";
      for (size_t i = 0; i < decls.size (); i++)
	tmp << " (" << decls[i]->component << " " << decls[i]->name << ")";
      daisy_panic (tmp.str ());
    }
  daisy_assert (decls.size () == 1);
  const Declare& declare = *decls[0];
  library.add_model (model, declare.create_frame ());
  decls.erase (decls.begin ());
}

void 
Intrinsics::submodel_instantiate (const load_syntax_t load_syntax)
{
  if (submodel_load_frame.find (load_syntax) == submodel_load_frame.end ())
    submodel_load_frame[load_syntax].reset ();
}

bool 
Intrinsics::submodel_registered (const symbol name) const
{ return submodel_name_load.find (name) != submodel_name_load.end (); }

boost::shared_ptr<const FrameSubmodel> 
Intrinsics::submodel_frame (const symbol name)
{
  const submodel_name_load_t::const_iterator i = submodel_name_load.find (name);
  if (i == submodel_name_load.end ())
    daisy_panic ("Unable to find submodel '" + name + "'");
  return submodel_frame ((*i).second);
}

struct FrameSubmodelDeclared : public FrameSubmodel
{
  load_syntax_t load_syntax;

  symbol type_name () const
  { return Librarian::submodel_name (load_syntax); }

  FrameSubmodelDeclared& clone () const
  { return *new FrameSubmodelDeclared (*this, parent_clone); }

  FrameSubmodelDeclared (const FrameSubmodelDeclared& parent, parent_clone_t)
    : FrameSubmodel (parent, parent_clone),
      load_syntax (parent.load_syntax)
  { }
      
  FrameSubmodelDeclared (const load_syntax_t load)
    : FrameSubmodel (load),
      load_syntax (load)
  { }
};

boost::shared_ptr<const FrameSubmodel> 
Intrinsics::submodel_frame (const load_syntax_t load_syntax)
{
  submodel_instantiate (load_syntax);
  const submodel_load_frame_t::iterator i 
    = submodel_load_frame.find (load_syntax);
  daisy_assert (i != submodel_load_frame.end ());

  if (!(*i).second.get ())
    {
      boost::shared_ptr<const FrameSubmodel> 
        frame (new FrameSubmodelDeclared (load_syntax));
      (*i).second = frame;
    }

  return (*i).second;
  
}

symbol
Intrinsics::submodel_name (const load_syntax_t load_syntax)
{
  const submodel_load_name_t::const_iterator i 
    = submodel_load_name.find (load_syntax);
  if (i == submodel_load_name.end ())
    return Attribute::None ();
  return (*i).second;
}
  
Intrinsics::load_syntax_t
Intrinsics::submodel_load (const symbol name) const
{
  const submodel_name_load_t::const_iterator i = submodel_name_load.find (name);
  daisy_assert (i != submodel_name_load.end ());
  return (*i).second;
}

symbol 
Intrinsics::submodel_description (const symbol name) const
{
  const submodel_name_desc_t::const_iterator i = submodel_name_desc.find (name);
  daisy_assert (i != submodel_name_desc.end ());
  return (*i).second;
}

void
Intrinsics::submodel_declare (const load_syntax_t load_syntax, 
                              const symbol submodel, const symbol description)
{
  submodel_instantiate (load_syntax);
  
  // Bidirectional name<->load mapping.
  if (submodel_name_load.find (submodel) == submodel_name_load.end ())
    submodel_name_load[submodel] = load_syntax;
  daisy_assert (submodel_name_load[submodel] == load_syntax);
  if (submodel_load_name.find (load_syntax) == submodel_load_name.end ())
    submodel_load_name[load_syntax] = submodel;
  daisy_assert (submodel_load_name[load_syntax] == submodel);

  // Name -> Description mapping.
  if (submodel_name_desc.find (submodel) == submodel_name_desc.end ())
    submodel_name_desc[submodel] = description;
  daisy_assert (submodel_name_desc[submodel] == description);
}

void
Intrinsics::submodel_all (std::vector<symbol>& all) const
{
  for (submodel_name_load_t::const_iterator i = submodel_name_load.begin ();
       i != submodel_name_load.end ();
       i++)
    all.push_back ((*i).first);
}

Intrinsics::Intrinsics ()
  : count (1),
    closed (false)
{ }

Intrinsics::~Intrinsics ()
{ 
  daisy_safe_assert (count == 0); 
}

// intrinsics.C ends here
