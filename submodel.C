// submodel.C  --- A registry of submodels.
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


#include "submodel.h"
#include "common.h"
#include <map>

typedef map<string, Submodel::load_fun, less<string>/**/> submodel_map_type;

static submodel_map_type* submodel_map = NULL;

void
Submodel::all (vector<string>& entries)
{
  for (submodel_map_type::const_iterator i = submodel_map->begin ();
       i != submodel_map->end ();
       i++)
    {
      entries.push_back ((*i).first);
    }
}

void
Submodel::load_syntax (const string& model, 
		       Syntax& syntax, AttributeList& alist)
{
  submodel_map_type::const_iterator i = submodel_map->find (model);
  assert (i != submodel_map->end ());
  (*i).second (syntax, alist);
}

bool
Submodel::registered (const string& submodel)
{ return submodel_map->find (submodel) != submodel_map->end (); }

Submodel::Register::Register (const string& name, load_fun fun)
{
  if (!submodel_map)
    submodel_map = new submodel_map_type;
  (*submodel_map)[name] = fun;
}

Submodel::Register::~Register ()
{ }

Submodel::Submodel ()
{ }

Submodel::~Submodel ()
{ }
