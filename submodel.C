// submodel.C  --- A registry of submodels.

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
