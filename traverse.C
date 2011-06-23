// traverse.C --- Base class to traverse the data structures.
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

#include "traverse.h"
#include "metalib.h"
#include "library.h"
#include "assertion.h"
#include "librarian.h"
#include "frame_submodel.h"
#include "frame_model.h"

void 
Traverse::traverse_all_libraries ()
  // Traverse through all libraries.
{
  std::vector<symbol> components;
  metalib.all (components);

  for (unsigned int i = 0; i < components.size (); i++)
    {
      const symbol component = components[i];
      traverse_library (component);
    }
}

void 
Traverse::traverse_all_submodels ()
  // Traverse through all registered submodels.
{
  std::vector<symbol> submodels;
  Librarian::submodel_all (submodels);

  for (unsigned int i = 0; i < submodels.size (); i++)
    {
      const symbol submodel = submodels[i];
      const Frame& frame = *Librarian::submodel_frame (submodel).get ();
      traverse_submodel_default (frame, submodel, submodel);
    }
}

void 
Traverse::traverse_library (const symbol component)
  // Traverse through a specific library.
{
  Library& library = metalib.library (component);

  if (enter_library (library, component))
    {
      std::vector<symbol> models;
      library.entries (models);

      for (unsigned int i = 0; i < models.size (); i++)
	{
	  const symbol model = models[i];
	  traverse_model (component, model);
	}
      leave_library ();
    }
}  

void 
Traverse::traverse_model (const symbol component, const symbol model)
  // Traverse through a specific library member.
{
  Library& library = metalib.library (component);
  FrameModel& frame = const_cast<FrameModel&> (library.model (model));

  if (enter_model (frame, component, model))
    {
      const symbol super = frame.base_name ();
      if (super != Attribute::None ())
	{
	  // Derived parameterization, has default values.
	  const Frame& default_frame = library.model (super);
	  traverse_alist (frame, default_frame, model.name ());
	}
      else
	{
	  // Buildin, no default values.
	  traverse_alist (frame, FrameModel::root (), model.name ());
	}	
      leave_model (component, model);
    }
}

void
Traverse::traverse_submodel (Frame& frame,
			     const Frame& default_frame,
			     const symbol name, const symbol registered)
  // Traverse through a submodel, typically a nested alist.
{
  if (enter_submodel (frame, default_frame, name, registered))
    {
      traverse_alist (frame, default_frame, name);
      leave_submodel ();
    }
}

void
Traverse::traverse_submodel_default (const Frame& default_frame,
				     const symbol name, const symbol registered)
  // Traverse through a submodel with no actual value, but a default
  // value.  This only happens in buildin models.
{
  if (enter_submodel_default (default_frame, name, registered))
    {
      traverse_alist (const_cast<Frame&> (default_frame),
                      FrameModel::root (), name);
      leave_submodel_default ();
    }
}

void
Traverse::traverse_submodel_sequence (const Frame& frame,
				      const Frame& default_frame,
				      const symbol name, const unsigned index,
                                      const symbol registered)
  // Traverse through a submodel, typically a nested alist.
{
  if (enter_submodel_sequence (frame, default_frame, name, index, registered))
    {
      traverse_alist (frame, default_frame, name);
      leave_submodel_sequence ();
    }
}

void
Traverse::traverse_submodel_sequence_default (const Frame& default_frame,
					      const symbol name,
                                              const symbol registered)
  // Traverse through the common default value for members of a
  // submodel sequence.
{
  if (enter_submodel_sequence_default (default_frame, name, registered))
    {
      traverse_alist (const_cast<Frame&> (default_frame), 
                      FrameModel::root (), name);
      leave_submodel_sequence_default ();
    }
}

void
Traverse::traverse_object (const Library& library, 
			   Frame& frame, const Frame& default_frame,
			   const symbol name)
  // Traverse through a object parameter value.
{
  if (enter_object (library, frame, default_frame, name))
    {
      traverse_alist (frame, default_frame, name);
      leave_object ();
    }
}

void
Traverse::traverse_object_sequence (const Library& library,
				    const Frame& frame,
				    const Frame& default_frame,
				    const symbol name, unsigned index)
  // Traverse through a object parameter value.
{
  if (enter_object_sequence (library, frame, default_frame, 
			     name, index))
    {
      traverse_alist (frame, default_frame, name);
      leave_object_sequence ();
    }
}

void
Traverse::traverse_alist (const Frame& frame,
			  const Frame& default_frame,
			  const symbol name)
  // Generic code to traverse through any kind of alist.
  // This is only a helper function for the more specific traversals,
  // such as 'traverse_model'.
{
  const std::vector<symbol>& order = frame.order ();
  for (unsigned int i = 0; i < order.size (); i++)
    traverse_parameter (frame, default_frame, name, order[i]);

  std::set<symbol> parameters;
  frame.entries (parameters);
  for (std::set<symbol>::const_iterator i = parameters.begin ();
       i != parameters.end ();
       i++)
    {
      const symbol parameter = *i;
      if (frame.order_index (parameter) < 0)
	traverse_parameter (frame, default_frame, name, parameter);
  }
}

void
Traverse::traverse_parameter (const Frame& frame,
			      const Frame& default_frame,
			      const symbol name, const symbol parameter)
  // Traverse through an alist member.  This is most interesting for
  // alist and object members, of course.
{
  if (enter_parameter (frame, default_frame, name, parameter))
    {
      const Attribute::type type = frame.lookup (parameter);
      const int size = frame.type_size (parameter);
      const bool has_value = frame.check (parameter);

      // Children.
      switch (type)
	{
	case Attribute::Submodel:
	  {
	    FrameSubmodel& entry_frame
              = const_cast<FrameSubmodel&> (frame.submodel (parameter));
            const symbol submodel = frame.submodel_name (parameter);
	    if (size == Attribute::Singleton)
	      {
		if (has_value && default_frame.check (parameter))
                  traverse_submodel (entry_frame, 
                                     default_frame.submodel (parameter), 
                                     parameter, submodel);
                else
                  traverse_submodel (entry_frame,
                                     *frame.default_frame (parameter), 
                                     parameter, submodel);
	      }
	    else
	      {
		const Frame& nested_default_frame 
                  = *frame.default_frame (parameter);

		traverse_submodel_sequence_default (nested_default_frame, 
						    parameter, submodel);
		
		if (has_value)
		  {
		    const std::vector<boost::shared_ptr<const FrameSubmodel>/**/>& sequence
		      = frame.submodel_sequence (parameter);
		    for (unsigned int i = 0; i < sequence.size (); i++)
		      traverse_submodel_sequence (*sequence[i],
						  nested_default_frame,
						  parameter, i, submodel);
		  }
	      }
	  }
	  break;
	case Attribute::Model:
	  {
	    if (has_value)
	      {
		if (size == Attribute::Singleton)
		  {
		    FrameModel& entry_frame 
                      = const_cast<FrameModel&> (frame.model (parameter));
		    const symbol type = entry_frame.type_name ();
		    const Library& library 
                      = metalib.library (frame.component (parameter));
		    const Frame& entry_default_frame = library.model (type);
		
		    traverse_object (library, entry_frame, entry_default_frame,
				     parameter);
		  }
		else
		  {
		    const std::vector<boost::shared_ptr<const FrameModel>/**/>& sequence
		      = frame.model_sequence (parameter);
		    for (unsigned int i = 0; i < sequence.size (); i++)
		      {
			const FrameModel& entry_frame = *sequence[i];
			const symbol type = entry_frame.type_name ();
			const Library& library 
                          = metalib.library (frame.component (parameter));
			const FrameModel& entry_default_frame 
                          = library.model (type);
			traverse_object_sequence (library, entry_frame, 
                                                  entry_default_frame, 
						  parameter, i);
		      }
		  }
	      }
	  }
	  break;
	default:
	  /* do nothing */
	  break;
	}
      leave_parameter ();
    }
}

Traverse::Traverse (const Metalib& mlib)
  : metalib (mlib)
{ }

Traverse::~Traverse ()
{ }
