// traverse_delete.C -- FInd and remove dependencies in Daisy datastructures.

#include "traverse_delete.h"
#include "traverse.h"
#include "library.h"
#include "syntax.h"
#include "alist.h"

class TraverseDelete : public Traverse
{
private:
  // What depend on this parameterization?
  const string dep_lib;
  const string dep_par;
  string dep_super;

  // The parameterization we currently test.
  bool found;

  // Create & Destroy.
public:
  TraverseDelete (const string& component, const string& parameterization);
  ~TraverseDelete ();

private:
  // Implementation.
  bool enter_library (Library& library, 
		      const string& component);
  void leave_library ();
  bool enter_model (const Syntax&, AttributeList&, 
		    const string& component, const string& model);
  void leave_model (const string& component, const string& name);
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
TraverseDelete::enter_library (Library&, const string&)
{ return true; }

void
TraverseDelete::leave_library ()
{ }

bool
TraverseDelete::enter_model (const Syntax&, AttributeList& alist,
			     const string& component, const string&)
{
  // Check if this model is inherited from the model we are examining.
  if (dep_lib == component
      && alist.check ("type") 
      && alist.name ("type") == dep_par)
    alist.add ("type", dep_super);

  return true;
}

void
TraverseDelete::leave_model (const string&, const string&)
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
{ assert (false); }

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
{ assert (false); }

bool
TraverseDelete::enter_object (const Library& library, 
			      const Syntax&, AttributeList& alist,
			      const AttributeList&,
			      const string&)
{
  assert (alist.check ("type"));
  const string super = alist.name ("type");
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

TraverseDelete::TraverseDelete (const string& component,
				const string& parameterization)
  : dep_lib (component),
    dep_par (parameterization),
    found (false)
{ 
  const Library& library = Library::find (component);
  assert (library.check (parameterization));
  const AttributeList& alist = library.lookup (parameterization);
  assert (alist.check ("type"));
  dep_super = alist.name ("type");
  assert (parameterization != dep_super);
}

TraverseDelete::~TraverseDelete ()
{ }

void
remove_dependencies (const string& component, const string& parameterization)
{
  TraverseDelete depend (component, parameterization);
  depend.traverse_all_libraries ();
}

void
remove_dependencies (const string& component, const string& parameterization, 
		     const Syntax& syntax, AttributeList& alist)
{
  TraverseDelete depend (component, parameterization);
  depend.traverse_submodel (syntax, alist, AttributeList (), "dummy");
}
