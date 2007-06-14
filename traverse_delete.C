// traverse_delete.C -- FInd and remove dependencies in datastructures.
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

#include "traverse_delete.h"
#include "traverse.h"
#include "metalib.h"
#include "library.h"
#include "syntax.h"
#include "alist.h"
#include "assertion.h"

using namespace std;

class TraverseDelete : public Traverse
{
private:
  // What depend on this parameterization?
  const symbol dep_lib;
  const symbol dep_par;
  const symbol dep_super;

  // The parameterization we currently test.
  bool found;

  // Create & Destroy.
public:
  TraverseDelete (const Metalib&, symbol component, symbol parameterization);
  ~TraverseDelete ();

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

bool
TraverseDelete::enter_library (Library&, const symbol)
{ return true; }

void
TraverseDelete::leave_library ()
{ }

bool
TraverseDelete::enter_model (const Syntax&, AttributeList& alist,
			     const symbol component, const symbol)
{
  // Check if this model is inherited from the model we are examining.
  if (dep_lib == component
      && alist.check ("type") 
      && alist.identifier ("type") == dep_par)
    alist.add ("type", dep_super);

  return true;
}

void
TraverseDelete::leave_model (const symbol, const symbol)
{ }

bool
TraverseDelete::enter_submodel (const Syntax&, AttributeList&,
				const AttributeList&,
				const string&)
{ return true; }

void
TraverseDelete::leave_submodel ()
{ }

bool
TraverseDelete::enter_submodel_default (const Syntax&, const AttributeList&, 
					const string&)
{ return false; }

void
TraverseDelete::leave_submodel_default ()
{ daisy_notreached (); }

bool
TraverseDelete::enter_submodel_sequence (const Syntax&,
					 AttributeList&,
					 const AttributeList&,
					 const string&, unsigned)
{ return true; }

void
TraverseDelete::leave_submodel_sequence ()
{ }

bool
TraverseDelete::enter_submodel_sequence_default (const Syntax&, 
						 const AttributeList&,
						 const string&)
{ return false; }

void
TraverseDelete::leave_submodel_sequence_default ()
{ daisy_notreached (); }

bool
TraverseDelete::enter_object (const Library& library, 
			      const Syntax&, AttributeList& alist,
			      const AttributeList&,
			      const string&)
{
  daisy_assert (alist.check ("type"));
  const symbol super = alist.identifier ("type");
  if (dep_lib == library.name () && super == dep_par)
    alist.add ("type", dep_super);

  return true; 
}

void
TraverseDelete::leave_object ()
{ }

bool
TraverseDelete::enter_object_sequence (const Library& library, 
				       const Syntax& syntax, 
				       AttributeList& alist,
				       const AttributeList& default_alist,
				       const string&, unsigned)
{ return enter_object (library, syntax, alist, default_alist, "dummy"); }

void
TraverseDelete::leave_object_sequence ()
{ leave_object (); }

bool
TraverseDelete::enter_parameter (const Syntax&, AttributeList&, 
				 const AttributeList&, 
				 const string&, const string&)
{ return true; }

void 
TraverseDelete::leave_parameter ()
{ }

static symbol
find_super (const Metalib& metalib,
            const symbol component, const symbol parameterization)
{ 
  const Library& library = metalib.library (component);
  daisy_assert (library.check (parameterization));
  const AttributeList& alist = library.lookup (parameterization);
  daisy_assert (alist.check ("type"));
  const symbol super = alist.identifier ("type");
  daisy_assert (parameterization != super);
  return super;
}

TraverseDelete::TraverseDelete (const Metalib& mlib,
                                const symbol component,
				const symbol parameterization)
  : Traverse (mlib),
    dep_lib (component),
    dep_par (parameterization),
    dep_super (find_super (mlib, component, parameterization)),
    found (false)
{ }

TraverseDelete::~TraverseDelete ()
{ }

void
remove_dependencies (const Metalib& mlib,
                     symbol component, symbol parameterization)
{
  TraverseDelete depend (mlib, component, parameterization);
  depend.traverse_all_libraries ();
}

void
remove_dependencies (const Metalib& mlib,
                     symbol component, symbol parameterization, 
		     const Syntax& syntax, AttributeList& alist)
{
  TraverseDelete depend (mlib, component, parameterization);
  depend.traverse_submodel (syntax, alist, AttributeList (), "dummy");
}
