// traverse.C --- Base class to traverse the data structures.

#include "traverse.h"
#include "library.h"
#include "syntax.h"
#include "alist.h"

void 
Traverse::traverse_all_libraries ()
  // Traverse through all libraries.
{
  vector<string> components;
  Library::all (components);

  for (unsigned int i = 0; i < components.size (); i++)
    {
      const string& component = components[i];
      traverse_library (component);
    }
}

void 
Traverse::traverse_library (const string& component)
  // Traverse through a specific library.
{
  Library& library = Library::find (component);

  if (enter_library (library, component))
    {
      vector<string> models;
      library.entries (models);

      for (unsigned int i = 0; i < models.size (); i++)
	{
	  const string& model = models[i];
	  traverse_model (component, model);
	}
      leave_library ();
    }
}  

void 
Traverse::traverse_model (const string& component, const string& model)
  // Traverse through a specific library member.
{
  Library& library = Library::find (component);
  const Syntax& syntax = library.syntax (model);
  AttributeList& alist = library.lookup (model);

  if (enter_model (syntax, alist, component, model))
    {
      if (alist.check ("type"))
	{
	  // Derived parameterization, has default values.
	  const string& super = alist.name ("type");
	  const AttributeList& default_alist = library.lookup (super);
	  traverse_alist (syntax, alist, default_alist, model);
	}
      else
	{
	  // Buildin, no default values.
	  static const AttributeList empty_alist;
	  traverse_alist (syntax, alist, empty_alist, model);
	}	
      leave_model (component, model);
    }
}

void
Traverse::traverse_submodel (const Syntax& syntax, AttributeList& alist,
			     const AttributeList& default_alist,
			     const string& name)
  // Traverse through a submodel, typically a nested alist.
{
  if (enter_submodel (syntax, alist, default_alist, name))
    {
      traverse_alist (syntax, alist, default_alist, name);
      leave_submodel ();
    }
}

void
Traverse::traverse_submodel_default (const Syntax& syntax, 
				     const AttributeList& default_alist,
				     const string& name)
  // Traverse through a submodel with no actual value, but a default
  // value.  This only happens in buildin models.
{
  if (enter_submodel_default (syntax, default_alist, name))
    {
      static AttributeList empty_alist;
      traverse_alist (syntax, const_cast<AttributeList&> (default_alist),
		      empty_alist, name);
      leave_submodel_default ();
    }
}

void
Traverse::traverse_submodel_sequence (const Syntax& syntax,
				      AttributeList& alist,
				      const AttributeList& default_alist,
				      const string& name, unsigned index)
  // Traverse through a submodel, typically a nested alist.
{
  if (enter_submodel_sequence (syntax, alist, default_alist, name, index))
    {
      traverse_alist (syntax, alist, default_alist, name);
      leave_submodel_sequence ();
    }
}

void
Traverse::traverse_submodel_sequence_default (const Syntax& syntax, 
					      const AttributeList&
					      /**/ default_alist,
					      const string& name)
  // Traverse through the common default value for members of a
  // submodel sequence.
{
  if (enter_submodel_sequence_default (syntax, default_alist, name))
    {
      static AttributeList empty_alist;
      traverse_alist (syntax, const_cast<AttributeList&> (default_alist),
		      empty_alist, name);
      leave_submodel_sequence_default ();
    }
}

void
Traverse::traverse_object (const Library& library, 
			   const Syntax& syntax, AttributeList& alist,
			   const AttributeList& default_alist,
			   const string& name)
  // Traverse through a object parameter value.
{
  if (enter_object (library, syntax, alist, default_alist, name))
    {
      traverse_alist (syntax, alist, default_alist, name);
      leave_object ();
    }
}

void
Traverse::traverse_object_sequence (const Library& library,
				    const Syntax& syntax, AttributeList& alist,
				    const AttributeList& default_alist,
				    const string& name, unsigned index)
  // Traverse through a object parameter value.
{
  if (enter_object_sequence (library, syntax, alist, default_alist, 
			     name, index))
    {
      traverse_alist (syntax, alist, default_alist, name);
      leave_object_sequence ();
    }
}

void
Traverse::traverse_alist (const Syntax& syntax, AttributeList& alist,
			  const AttributeList& default_alist,
			  const string& name)
  // Generic code to traverse through any kind of alist.
  // This is only a helper function for the more specific traversals,
  // such as `traverse_model'.
{
  const vector<string>& order = syntax.order ();
  for (unsigned int i = 0; i < order.size (); i++)
    traverse_parameter (syntax, alist, default_alist, name, order[i]);

  vector<string> parameters;
  syntax.entries (parameters);
  for (unsigned int i = 0; i < parameters.size (); i++)
    {
      const string& parameter = parameters[i];
      if (syntax.order (parameter) < 0)
	traverse_parameter (syntax, alist, default_alist, name, parameter);
    }
}

void
Traverse::traverse_parameter (const Syntax& syntax, AttributeList& alist,
			      const AttributeList& default_alist,
			      const string& name, const string&
			      parameter)
  // Traverse through an alist member.  This is most interesting for
  // alist and object members, of course.
{
  if (enter_parameter (syntax, alist, default_alist, name, parameter))
    {
      const Syntax::type type = syntax.lookup (parameter);
      const int size = syntax.size (parameter);
      const bool has_value = alist.check (parameter);

      // Children.
      switch (type)
	{
	case Syntax::AList:
	  {
	    const Syntax& entry_syntax = syntax.syntax (parameter);

	    if (size == Syntax::Singleton)
	      {
		if (has_value)
		  {
		    AttributeList& entry_alist = alist.alist (parameter);
		    if (default_alist.check (parameter))
		      traverse_submodel (entry_syntax, entry_alist, 
					 default_alist.alist (parameter), 
					 parameter);
		    else
		      traverse_submodel (entry_syntax, entry_alist,
					 syntax.default_alist (parameter), 
					 parameter);
		  }
		else 
		  traverse_submodel_default (entry_syntax, 
					     syntax.default_alist (parameter),
					     parameter);
	      }
	    else
	      {
		const AttributeList& nested_default_alist 
		  = syntax.default_alist (parameter);

		traverse_submodel_sequence_default (entry_syntax, 
						    nested_default_alist, 
						    parameter);
		
		if (has_value)
		  {
		    const vector<AttributeList*> sequence
		      = alist.alist_sequence (parameter);
		    for (unsigned int i = 0; i < sequence.size (); i++)
		      traverse_submodel_sequence (entry_syntax, *sequence[i],
						  nested_default_alist,
						  parameter, i);
		  }
	      }
	  }
	  break;
	case Syntax::Object:
	  {
	    if (has_value)
	      {
		if (size == Syntax::Singleton)
		  {
		    AttributeList& entry_alist = alist.alist (parameter);
		    assert (entry_alist.check ("type"));
		    const string& type = entry_alist.name ("type");
		    const Library& library = syntax.library (parameter);
		    const AttributeList& entry_default_alist 
		      = library.lookup (type);
		    const Syntax& entry_syntax = library.syntax (type);
		
		    traverse_object (library, entry_syntax, entry_alist, 
				     entry_default_alist,
				     parameter);
		  }
		else
		  {
		    const vector<AttributeList*> sequence
		      = alist.alist_sequence (parameter);
		    for (unsigned int i = 0; i < sequence.size (); i++)
		      {
			AttributeList& entry_alist = *sequence[i];
			assert (entry_alist.check ("type"));
			const string& type = entry_alist.name ("type");
			const Library& library = syntax.library (parameter);
			const AttributeList& entry_default_alist 
			  = library.lookup (type);
			const Syntax& entry_syntax = library.syntax (type);

			traverse_object_sequence (library, 
						  entry_syntax, entry_alist,
						  entry_default_alist, 
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

Traverse::Traverse ()
{ }

Traverse::~Traverse ()
{ }
