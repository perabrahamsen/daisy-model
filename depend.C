// depend.C -- Find dependencies in datastructures.
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


#include "depend.h"
#include "traverse.h"
#include "library.h"
#include "syntax.h"
#include "alist.h"
#include "tmpstream.h"
#include "treelog.h"
#include "assertion.h"

#include <algorithm>
#include <numeric>

class TraverseDepend : public Traverse
{
private:
  // We store it here.
  Treelog& treelog;
  dep_map& dependencies;

  // What depend on this parameterization?
  const string dep_lib;
  const string dep_par;
  
  // Find _all_ depencied?
  const bool find_all;

  // The parameterization we currently test.
  bool found;
public:
  bool found_any;

  // Create & Destroy.
public:
  TraverseDepend (const string& component, const string& parameterization, 
		  Treelog& treelog, dep_map& dependencies, bool find_all);
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
  if (found_any && !find_all)
    return false;
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

      if (find_all)
	{
	  // Find stuff dependend on this model.
	  Treelog::Open nest2 (treelog, "Recursive dependencies");
	  dependencies[component].insert (name);
	  TraverseDepend recurse (component, name, 
				  treelog, dependencies, find_all);
	  recurse.traverse_all_libraries ();
	}

      // We don't examine the content of this model then.
      return false;
    } 
  treelog.open (name);
  return true;
}

void
TraverseDepend::leave_model (const string& component, const string& name)
{ 
  if (found && find_all)
    {
      // Found, search for stuff dependend on this model.
      Treelog::Open nest (treelog, "Recursive dependencies");
      dependencies[component].insert (name);
      TraverseDepend recurse (component, name,
			      treelog, dependencies, find_all);
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
{ daisy_assert (false); }

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
{ daisy_assert (false); }

bool
TraverseDepend::enter_object (const Library& library, 
			      const Syntax&, AttributeList& alist,
			      const AttributeList&,
			      const string& name)
{
  daisy_assert (alist.check ("type"));
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
				Treelog& tlog,
				dep_map& deps, bool fa)
  : treelog (tlog),
    dependencies (deps),
    dep_lib (component),
    dep_par (parameterization),
    find_all (fa),
    found (false),
    found_any (false)
{ }

TraverseDepend::~TraverseDepend ()
{ }

bool
has_dependencies (const string& component, const string& parameterization)
{
  dep_map dependencies;
  TraverseDepend depend (component, parameterization,
			 Treelog::null (), dependencies, false);
  depend.traverse_all_libraries ();

  return depend.found_any;
}

bool
has_dependencies (const string& component, const string& parameterization, 
		  const Syntax& syntax, AttributeList& alist,
		  const string& name)
{
  dep_map dependencies;
  TraverseDepend depend (component, parameterization,
			 Treelog::null (), dependencies, false);
  depend.traverse_submodel (syntax, alist, AttributeList (), name);

  return depend.found_any;
}

bool
check_dependencies (const string& component, const string& parameterization, 
		    Treelog& treelog)
{
  dep_map dependencies;
  TraverseDepend depend (component, parameterization, 
			 treelog, dependencies, true);
  depend.traverse_all_libraries ();

  return depend.found_any;
}

bool
check_dependencies (const string& component, const string& parameterization, 
		    const Syntax& syntax, AttributeList& alist,
		    const string& name, Treelog& treelog)
{
  dep_map dependencies;
  TraverseDepend depend (component, parameterization, 
			 treelog, dependencies, true);
  depend.traverse_submodel (syntax, alist, AttributeList (), name);

  return depend.found_any;
}

bool
find_dependencies (const string& component, const string& parameterization, 
		   dep_map& dependencies)
{
  TraverseDepend depend (component, parameterization, 
			 Treelog::null (), dependencies, true);
  depend.traverse_all_libraries ();

  return depend.found_any;
}

static int
sequence_number (const string& component, const string& parameterization)
{
  const Library& library = Library::find (component);
  daisy_assert (library.check (parameterization));
  const AttributeList& alist = library.lookup (parameterization);
  if (alist.check ("parsed_sequence"))
    return alist.integer ("parsed_sequence");

  return -1;
}

struct object_desc
{
  string comp;
  string par;
  void operator= (const object_desc& other)
  { 
    comp = other.comp;
    par = other.par;
  }
  object_desc ()
  { }
  object_desc (const object_desc& other)
    : comp (other.comp),
      par (other.par)
  { }
  object_desc (const string& c, const string& p)
    : comp (c),
      par (p)
  { }
};


static bool
sort_by_sequence (const object_desc& one, const object_desc& two)
{ return sequence_number (one.comp, one.par) 
    < sequence_number (two.comp, two.par); }

void
resequence (const string& component, const string& parameterization, 
	    const dep_map& dependencies)
{ 
  // Vector with all object to resequence.
  vector<object_desc> deps;

  // Add parent.
  deps.push_back (object_desc (component, parameterization));

  // Add dependencies.
  for (dep_map::const_iterator i = dependencies.begin ();
       i != dependencies.end ();
       i++)
    {
      const string component = (*i).first;
      const string_set& pars = (*i).second;
      for (string_set::const_iterator j = pars.begin ();
	   j != pars.end ();
	   j++)
	deps.push_back (object_desc (component, *j));
    }

  // Sort them.
  sort (deps.begin (), deps.end (), sort_by_sequence);

  // Resequence them.
  {for (unsigned int i = 0; i < deps.size (); i++)
    {
      const string& component = deps[i].comp;
      const string& parameterization = deps[i].par;
       
       Library& library = Library::find (component);
       daisy_assert (library.check (parameterization));
       AttributeList& alist = library.lookup (parameterization);
       alist.add ("parsed_sequence", Library::get_sequence ());
  }}
}
