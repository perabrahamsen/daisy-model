// xref.C -- Find cross references in datastructures.
// 
// Copyright 2002 Per Abrahamsen and KVL.
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


#include "xref.h"
#include "traverse.h"
#include "library.h"
#include "syntax.h"
#include "alist.h"
#include "submodel.h"
#include "assertion.h"
#include <deque>

using namespace std;

class TraverseXRef : public Traverse
{
  // We store it here.
  XRef& xref;

  // State.
  symbol current_component;
  symbol current_model;
  enum { is_model, is_parameterization, is_submodel, is_invalid = -1 } type;
  string current_submodel;
  vector<string> path;

  // Use.
private:
  void use_submodel (const string& submodel);
  void use_component (const Library& library);
  void use_model (const Library& library, symbol model);

  // Create & Destroy.
public:
  TraverseXRef (XRef&);
  ~TraverseXRef ();

private:
  // Implementation.
  bool enter_library (Library& library, symbol component);
  void leave_library ();
  bool enter_model (const Syntax&, AttributeList&, 
		    symbol component, symbol model);
  void leave_model (symbol component, symbol name);
  bool enter_submodel (const Syntax& syntax, AttributeList& alist,
  		       const AttributeList& default_alist,
  		       const string& name);
  void leave_submodel ();
  bool enter_submodel_default (const Syntax& syntax, 
			       const AttributeList& default_alist,
			       const string& name);
  void leave_submodel_default ();
  bool enter_submodel_sequence (const Syntax& syntax,
  				AttributeList& alist,
  				const AttributeList& default_alist,
  				const string& name, unsigned index);
  void leave_submodel_sequence ();
  bool enter_submodel_sequence_default (const Syntax& syntax, 
  					const AttributeList& default_alist,
  					const string& name);
  void leave_submodel_sequence_default ();
  bool enter_object (const Library&, 
		     const Syntax& syntax, AttributeList& alist,
  		     const AttributeList& default_alist,
  		     const string& name);
  void leave_object ();
  bool enter_object_sequence (const Library&, const Syntax& syntax,
  			      AttributeList& alist,
  			      const AttributeList& default_alist,
  			      const string& name, 
  			      unsigned index);
  void leave_object_sequence ();
  bool enter_parameter (const Syntax&, AttributeList& alist, 
			const AttributeList& default_alist, 
			const string& name, const string& parameter);
  void leave_parameter ();
};

void 
TraverseXRef::use_submodel (const string& submodel)
{
  daisy_assert (Submodel::registered (submodel));
  XRef::Users& moi = xref.submodels[submodel];
  
  switch (type)
    {
    case is_parameterization:
      break;
    case is_submodel:
      moi.submodels.insert (XRef::SubmodelUser (current_submodel, path));
      break;
    case is_model:
      moi.models.insert (XRef::ModelUser (current_component, current_model, 
                                          path)); 
      break;
    case is_invalid:
    default:
      daisy_notreached ();
    }
  
}

void 
TraverseXRef::use_component (const Library& library)
{
  const symbol component = library.name ();
  XRef::Users& moi = xref.components[component];
  
  switch (type)
    {
    case is_parameterization:
      break;
    case is_submodel:
      moi.submodels.insert (XRef::SubmodelUser (current_submodel, path));
      break;
    case is_model:
      moi.models.insert (XRef::ModelUser (current_component, current_model, 
                                          path)); 
      break;
    case is_invalid:
    default:
      daisy_notreached ();
    }
  
}

void 
TraverseXRef::use_model (const Library& library, const symbol model)
{
  daisy_assert (library.check (model));
  const symbol component = library.name ();
  XRef::Users& moi = xref.models[XRef::ModelUsed (component, model)];
  
  switch (type)
    {
    case is_submodel:
      moi.submodels.insert (XRef::SubmodelUser (current_submodel, path));
      break;
    case is_parameterization:
    case is_model:
      moi.models.insert (XRef::ModelUser (current_component, current_model, 
                                          path)); 
      break;
    case is_invalid:
    default:
      daisy_notreached ();
    }
  
}

bool
TraverseXRef::enter_library (Library&, symbol component)
{
  daisy_assert (type == is_invalid);
  current_component = component;
  return true;
}

void
TraverseXRef::leave_library ()
{ 
  daisy_assert (type == is_invalid);
  daisy_assert (path.empty ());
}

bool
TraverseXRef::enter_model (const Syntax&, AttributeList& alist,
			   const symbol component, const symbol name)
{
  daisy_assert (component == current_component);
  daisy_assert (type == is_invalid);
  current_model = name;
  if (alist.check ("type"))
    type = is_parameterization;
  else
    type = is_model;
  return true;
}

void
TraverseXRef::leave_model (const symbol component, const symbol name)
{ 
  daisy_assert (path.empty ());
  daisy_assert (component == current_component);
  daisy_assert (name == current_model);
  type = is_invalid;
}

bool
TraverseXRef::enter_submodel (const Syntax& syntax, AttributeList& al,
			      const AttributeList&,
			      const string& name)
{ return enter_submodel_default (syntax, al, name); }

void
TraverseXRef::leave_submodel ()
{ leave_submodel_default (); }

bool
TraverseXRef::enter_submodel_default (const Syntax&, const AttributeList& al, 
				      const string& name)
{ 
  if (type == is_invalid)
    {
      // We are traversing a top level submodels.
      daisy_assert (al.check ("submodel"));
      daisy_assert (al.name ("submodel") == name);
      daisy_assert (path.empty ());
      type = is_submodel;
      current_submodel = name;
      return true;
    }
  if (al.check ("submodel"))
    {
      // Nested submodel.  Register and stop.
      use_submodel (al.name ("submodel"));
      return false;
    }
  return true; 
}

void
TraverseXRef::leave_submodel_default ()
{ 
  if (path.empty ())
    {
      // Top level submodel.
      daisy_assert (type == is_submodel);
      type = is_invalid;
    }
}

bool
TraverseXRef::enter_submodel_sequence (const Syntax& syntax,
				       AttributeList& al,
				       const AttributeList&,
				       const string& name, unsigned)
{ return enter_submodel_default (syntax, al, name); }

void
TraverseXRef::leave_submodel_sequence ()
{ leave_submodel_default (); }

bool
TraverseXRef::enter_submodel_sequence_default (const Syntax& syntax, 
					       const AttributeList& al,
					       const string& name)
{ return enter_submodel_default (syntax, al, name); }

void
TraverseXRef::leave_submodel_sequence_default ()
{ leave_submodel_default (); }

bool
TraverseXRef::enter_object (const Library& library, 
			    const Syntax&, AttributeList& alist,
			    const AttributeList&,
			    const string&)
{
  daisy_assert (alist.check ("type"));
  use_model (library, alist.identifier ("type"));
  return false; 
}

void
TraverseXRef::leave_object ()
{ daisy_notreached (); }

bool
TraverseXRef::enter_object_sequence (const Library& library, 
				     const Syntax& syntax, 
				     AttributeList& alist,
				     const AttributeList& default_alist,
				     const string& name, unsigned)
{ return enter_object (library, syntax, alist, default_alist, name); }

void
TraverseXRef::leave_object_sequence ()
{ leave_object (); }

bool
TraverseXRef::enter_parameter (const Syntax& syntax, AttributeList& alist, 
			       const AttributeList& default_alist, 
			       const string&, const string& name)
{ 
  if (type == is_parameterization)
    {
      // Ignore inherited values.
      if (alist.subset (default_alist, syntax, name))
        return false;
    }
  else if (type == is_model && alist.check ("base_model"))
    {
      // Ignore base parameters.
      const Library& library = Library::find (current_component);
      const symbol base_model = alist.identifier ("base_model");
      if (base_model != current_model)
        {
          const Syntax& base_syntax = library.syntax (base_model);
          if (base_syntax.lookup (name) != Syntax::Error)
            {
              const AttributeList& base_alist = library.lookup (base_model);
              if (alist.subset (base_alist, syntax, name))
                return false;
            }
        }
    }
  path.push_back (name);

  if (syntax.lookup (name) == Syntax::Object)
    // We always use the component, even if it has no value, or a
    // value that is an empty sequence.
    use_component (syntax.library (name));

  return true; 
}

void 
TraverseXRef::leave_parameter ()
{ path.pop_back (); }

TraverseXRef::TraverseXRef (XRef& xr)
  : xref (xr),
    current_component ("Daisy"),
    current_model ("invalid"),
    type (is_invalid)
{
  traverse_all_libraries ();
  traverse_all_submodels ();
}

TraverseXRef::~TraverseXRef ()
{ 
  daisy_assert (type == is_invalid); 
  daisy_assert (path.empty ());
}

bool
XRef::ModelUsed::operator< (const ModelUsed& other) const
{ return component < other.component
    || (component == other.component && model < other.model); }


XRef::ModelUsed::ModelUsed (const symbol comp, const symbol mod)
  : component (comp),
    model (mod)
{ }

bool
XRef::ModelUser::operator< (const ModelUser& other) const
{ return component < other.component
    || (component == other.component && model < other.model); }


XRef::ModelUser::ModelUser (const symbol comp, const symbol mod,
			    const vector<string>& p)
  : component (comp),
    model (mod),
    path (p)
{ }

bool
XRef::SubmodelUser::operator< (const SubmodelUser& other) const
{ return submodel < other.submodel; }

XRef::SubmodelUser::SubmodelUser (const string& sub, const vector<string>& p)
  : submodel (sub),
    path (p)
{ }

XRef::SubmodelUser::SubmodelUser ()
{ }

XRef::Users::Users ()
{ }

XRef::XRef ()
{ 
  TraverseXRef traverse (*this);
}

XRef::~XRef ()
{ }

