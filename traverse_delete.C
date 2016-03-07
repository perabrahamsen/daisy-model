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
#include "frame_model.h"
#include "assertion.h"

class TraverseDelete : public Traverse
{
private:
  // What depend on this parameterization?
  const symbol dep_lib;
  const symbol dep_par;
  const symbol dep_super;

  // Create & Destroy.
public:
  TraverseDelete (Metalib&, symbol component, symbol parameterization);
  ~TraverseDelete ();

private:
  // Implementation.
  bool enter_library (Library& library, symbol component);
  void leave_library ();
  bool enter_model (Frame& frame,
		    symbol component, symbol model);
  void leave_model (symbol component, symbol name);
  bool enter_submodel (Frame& frame, const Frame& default_frame,
  		       const symbol name, const symbol registered);
  void leave_submodel ();
  bool enter_submodel_default (const Frame& default_frame,
			       const symbol name,
                               const symbol registered);
  void leave_submodel_default ();
  bool enter_submodel_sequence (const Frame& frame, const Frame& default_frame,
  				const symbol name, unsigned index, 
                                const symbol registered);
  void leave_submodel_sequence ();
  bool enter_submodel_sequence_default (const Frame& default_frame,
  					const symbol name,
                                        const symbol registered);

  void leave_submodel_sequence_default ();
  bool enter_object (const Library&, 
		     const Frame& frame, const Frame& default_frame,
  		     const symbol name);
  void leave_object ();
  bool enter_object_sequence (const Library&, 
                              const Frame& frame, const Frame& default_frame,
  			      const symbol name, 
  			      unsigned index);
  void leave_object_sequence ();
  bool enter_parameter (const Frame& frame, const Frame& default_frame,
			const symbol name, const symbol parameter);
  void leave_parameter ();
};

bool
TraverseDelete::enter_library (Library&, const symbol)
{ return true; }

void
TraverseDelete::leave_library ()
{ }

bool
TraverseDelete::enter_model (Frame& frame,
			     const symbol component, const symbol)
{
  // Check if this model is inherited from the model we are examining.
  if (dep_lib == component && frame.type_name () == dep_par)
    frame.set ("type", dep_super);

  return true;
}

void
TraverseDelete::leave_model (const symbol, const symbol)
{ }

bool
TraverseDelete::enter_submodel (Frame&, const Frame&,
				const symbol, const symbol)
{ return true; }

void
TraverseDelete::leave_submodel ()
{ }

bool
TraverseDelete::enter_submodel_default (const Frame&,
					const symbol, const symbol registered)
{ return false; }

void
TraverseDelete::leave_submodel_default ()
{ daisy_notreached (); }

bool
TraverseDelete::enter_submodel_sequence (const Frame&, const Frame&,
                                         const symbol, unsigned, const symbol)
{ return true; }

void
TraverseDelete::leave_submodel_sequence ()
{ }

bool
TraverseDelete::enter_submodel_sequence_default (const Frame&,
						 const symbol, const symbol)
{ return false; }

void
TraverseDelete::leave_submodel_sequence_default ()
{ daisy_notreached (); }

bool
TraverseDelete::enter_object (const Library& library, 
                              const Frame& frame, const Frame&, 
			      const symbol)
{
  const symbol super = frame.type_name ();
  if (dep_lib == library.name () && super == dep_par)
    const_cast<Frame&> (frame).set ("type", dep_super);

  return true; 
}

void
TraverseDelete::leave_object ()
{ }

bool
TraverseDelete::enter_object_sequence (const Library& library, 
                                       const Frame& frame, 
                                       const Frame& default_frame,
				       const symbol, unsigned)
{ return enter_object (library, frame, default_frame, "dummy"); }

void
TraverseDelete::leave_object_sequence ()
{ leave_object (); }

bool
TraverseDelete::enter_parameter (const Frame&, const Frame&, 
				 const symbol, const symbol)
{ return true; }

void 
TraverseDelete::leave_parameter ()
{ }

static symbol
find_super (Metalib& metalib,
            const symbol component, const symbol parameterization)
{ 
  const Library& library = metalib.library (component);
  daisy_assert (library.check (parameterization));
  const Frame& frame = library.model (parameterization);
  const symbol super = frame.base_name ();
  daisy_assert (parameterization != super);
  return super;
}

TraverseDelete::TraverseDelete (Metalib& mlib,
                                const symbol component,
				const symbol parameterization)
  : Traverse (mlib),
    dep_lib (component),
    dep_par (parameterization),
    dep_super (find_super (mlib, component, parameterization))
{ }

TraverseDelete::~TraverseDelete ()
{ }

void
remove_dependencies (Metalib& mlib,
                     symbol component, symbol parameterization)
{
  TraverseDelete depend (mlib, component, parameterization);
  depend.traverse_all_libraries ();
}

// traverse_depend.C ends here.
