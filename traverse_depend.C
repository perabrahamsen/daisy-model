// traverse_depend.C -- Find dependencies in Daisy datastructures.

#include "traverse_depend.h"
#include "traverse.h"
#include "library.h"
#include "syntax.h"
#include "alist.h"
#include "tmpstream.h"
#include "treelog.h"

class TraverseDepend : public Traverse
{
private:
  // We store it here.
  Treelog& treelog;

  // What depend on this parameterization?
  const string dep_lib;
  const string dep_par;

  // The parameterization we currently test.
  bool found;
public:
  bool found_any;

  // Create & Destroy.
public:
  TraverseDepend (const string& component, const string& parameterization, 
		  Treelog& treelog);
  ~TraverseDepend ();

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
TraverseDepend::enter_library (Library&, const string& component)
{
  treelog.open (component);
  return true;
}

void
TraverseDepend::leave_library ()
{ treelog.close (); }

bool
TraverseDepend::enter_model (const Syntax&, AttributeList& alist,
			     const string& component, const string& name)
{
  // Check if this model is inherited from the model we are examining.
  if (dep_lib == component
      && alist.check ("type") 
      && alist.name ("type") == dep_par)
    { 
      Treelog::Open nest1 (treelog, name);
      treelog.entry (dep_par + " inherited by " + name);
      found_any = true;

      // Find stuff dependend on this model.
      Treelog::Open nest2 (treelog, "Recursive dependencies");
      TraverseDepend recurse (component, name, treelog);
      recurse.traverse_all_libraries ();

      // We don't examine the content of this model then.
      return false;
    } 
  treelog.open (name);
  return true;
}

void
TraverseDepend::leave_model (const string& component, const string& name)
{ 
  if (found)
    {
      // Found, search for stuff dependend on this model.
      Treelog::Open nest (treelog, "Recursive dependencies");
      TraverseDepend recurse (component, name, treelog);
      recurse.traverse_all_libraries ();

      // Clear flag for next model.
      found = false;
    }
  treelog.close (); 
}

bool
TraverseDepend::enter_submodel (const Syntax&, AttributeList&,
				const AttributeList&,
				const string& name)
{
  treelog.open (name);
  return true; 
}

void
TraverseDepend::leave_submodel ()
{ treelog.close (); }

bool
TraverseDepend::enter_submodel_default (const Syntax&, const AttributeList&, 
					const string&)
{ return false; }

void
TraverseDepend::leave_submodel_default ()
{ assert (false); }

bool
TraverseDepend::enter_submodel_sequence (const Syntax&,
					 AttributeList&,
					 const AttributeList&,
					 const string& name, unsigned index)
{ 
  TmpStream str;
  str () << name << "[" << index << "]";
  treelog.open (str.str ());
  return true; 
}

void
TraverseDepend::leave_submodel_sequence ()
{ treelog.close (); }

bool
TraverseDepend::enter_submodel_sequence_default (const Syntax&, 
						 const AttributeList&,
						 const string&)
{ return false; }

void
TraverseDepend::leave_submodel_sequence_default ()
{ assert (false); }

bool
TraverseDepend::enter_object (const Library& library, 
			      const Syntax&, AttributeList& alist,
			      const AttributeList&,
			      const string& name)
{
  assert (alist.check ("type"));
  const string super = alist.name ("type");
  if (dep_lib == library.name () && super == dep_par)
    { 
      treelog.entry (name + " inherits " + dep_par);
      found_any = true;
      found = true;
      return false;
    }
  treelog.open (name + " (" + super + ")");
  return true; 
}

void
TraverseDepend::leave_object ()
{ treelog.close (); }

bool
TraverseDepend::enter_object_sequence (const Library& library, 
				       const Syntax& syntax, 
				       AttributeList& alist,
				       const AttributeList& default_alist,
				       const string& name, unsigned index)
{ 
  TmpStream str;
  str () << name << "[" << index << "]";

  return enter_object (library, syntax, alist, default_alist, str.str ());
}

void
TraverseDepend::leave_object_sequence ()
{ leave_object (); }

bool
TraverseDepend::enter_parameter (const Syntax&, AttributeList&, 
				 const AttributeList&, 
				 const string&, const string&)
{ return true; }

void 
TraverseDepend::leave_parameter ()
{ }

TraverseDepend::TraverseDepend (const string& component,
				const string& parameterization,
				Treelog& tlog)
  : treelog (tlog),
    dep_lib (component),
    dep_par (parameterization),
    found (false),
    found_any (false)
{ }

TraverseDepend::~TraverseDepend ()
{ }

bool
check_dependencies (const string& component, const string& parameterization, 
		    Treelog& treelog)
{
  TraverseDepend depend (component, parameterization, treelog);
  depend.traverse_all_libraries ();

  return depend.found_any;
}

bool
check_dependencies (const string& component, const string& parameterization, 
		    const Syntax& syntax, AttributeList& alist,
		    const string& name, Treelog& treelog)
{
  TraverseDepend depend (component, parameterization, treelog);
  depend.traverse_submodel (syntax, alist, AttributeList (), name);

  return depend.found_any;
}

