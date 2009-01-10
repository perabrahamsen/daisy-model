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
#include "frame_model.h"

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
          daisy_assert (decls.size () == 1);
          const Declare& declare = *decls[0];
          library.add_model (model, *new FrameModel (declare));
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
Intrinsics::declare (const symbol component, const symbol model,
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
  daisy_assert (decls.size () == 1);
  const Declare& declare = *decls[0];
  library.add_model (model, *new FrameModel (declare));
  decls.erase (decls.begin ());
}

Intrinsics::Intrinsics ()
  : count (1),
    closed (false)
{ }

Intrinsics::~Intrinsics ()
{ 
  map_delete (all.begin (), all.end ());
  daisy_assert (count == 0); 
}


// intrinsics.C ends here
