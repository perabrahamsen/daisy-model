// syntax.C
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


#include "syntax.h"
#include "alist.h"
#include "library.h"
#include "tmpstream.h"
#include "check.h"
#include "vcheck.h"
#include "assertion.h"
#include "common.h"
#include <map>
#include <algorithm>

using namespace std;

const int Syntax::Singleton = -117;
const int Syntax::Sequence = -3212;
const int Syntax::Unspecified = -666;

struct Syntax::Implementation
{
  vector<check_fun> checker;
  vector<check_list_fun> list_checker;
  vector<string> order;
  typedef map<string, type, less<string> > type_map;
  typedef map<string, category, less<string> > status_map;
  typedef map<string, const Syntax*, less<string> > syntax_map;
  typedef map<string, int, less<string> > size_map;
  typedef map<string, ::Library*, less<string> > library_map;
  typedef map<string, string, less<string> > string_map;
  typedef map<string, const AttributeList*, less<string> > alist_map;
  typedef map<string, const Check*, less<string> > check_map;
  typedef map<string, const VCheck*, less<string> > vcheck_map;

  type_map types;
  status_map status;
  syntax_map syntax;
  size_map size;
  library_map libraries;
  string_map domains;
  string_map dimensions;
  check_map num_checks;
  vcheck_map val_checks;
  string_map descriptions;
  alist_map alists;

  bool check (const AttributeList& vl, Treelog& err);
  void check (const string& key, double value) const;
  Syntax::type lookup (const string& key) const;
  int order_number (const string& name) const;
  void entries (vector<string>& result) const;
  Implementation ()
  { }
  ~Implementation ()
  {
    map_delete (syntax.begin (), syntax.end ());
    map_delete (alists.begin (), alists.end ()); 
  }
};    

bool 
Syntax::Implementation::check (const AttributeList& vl, Treelog& err)
{
  bool error = false;

  for (status_map::const_iterator i = status.begin ();
       i != status.end ();
       i++)
    {
      const string key = (*i).first;
      if (!vl.check (key))
	{
	  if (status[key] == Const || status[key] == State)
	    {
	      err.error (key + " missing");
	      error = true;
	    }
	  continue;
	}
      // We can't do a generic value check here, because we don't have
      // the Syntax.  Sigh.
      // vcheck_map::const_iterator i = val_checks.find (key);
      
      // Spcial handling of various types.
      if (types[key] == Number)
	{
	  // This should already be checked by the file parse, but you
	  // never know... better safe than sorry...  don't drink and
	  // drive...  Well, theoretically the alist could come from
	  // another source (like one of the many other parser :/), or
	  // be one of the build in ones.
	  check_map::const_iterator i = num_checks.find (key);

	  if (i != num_checks.end ())
	    {
	      const Check *const check = (*i).second;

	      if (size[key] != Singleton)
		{
		  const vector<double>& array = vl.number_sequence (key);
		  for (unsigned int i = 0; i < array.size (); i++)
		    try 
		      { check->check (array[i]); }
		    catch (const string& message)
		      {
			TmpStream str;
			str () << key << "[" << i << "]: " << message;
			err.error (str.str ());
			error = true;
		      }
		}
	      else try 
		{ check->check (vl.number (key)); }
	      catch (const string& message)
		{
		  err.error (key + ": " + message);
		  error = true;
		}
	    }
	}
      else if (types[key] == Object)
	if (size[key] != Singleton)
	  {
	    const ::Library& lib = *libraries[key];
	    const vector<AttributeList*>& seq = vl.alist_sequence (key);
	    int j_index = 0;
	    for (vector<AttributeList*>::const_iterator j = seq.begin ();
		 j != seq.end ();
		 j++)
	      {
		TmpStream tmp;
		tmp () << key << "[" << j_index << "]: ";
		j_index++;
		const AttributeList& al = **j;
		if (!al.check ("type"))
		  {
		    tmp () << "Non object found";
		    err.error (tmp.str ());
		    error = true;
		  }
		else if (al.name ("type") == "error")
		  {
		    tmp () << "Error node found";
		    error = true;
		    err.error (tmp.str ());
		  }
		else if (!lib.check (al.name ("type")))
		  {
		    tmp () << "Unknown library member '"
			   << al.name ("type") << "'";
		    err.error (tmp.str ());
		    error = true;
		  }
		else 
		  {
		    tmp () << al.name ("type");
		    Treelog::Open nest (err, tmp.str ());
		    if (!lib.syntax (al.name ("type")).impl.check (al, err))
		      error = true;
		  }
	      }
	  }
	else 
	  {
	    const ::Library& lib = *libraries[key];
	    const AttributeList& al = vl.alist (key);
	    if (!al.check ("type"))
	      {
		err.error (key + "Non object found");
		error = true;
	      }
	    else 
	      {
		Treelog::Open nest (err, key + ": " + al.name ("type"));
		if (!lib.syntax (al.name ("type")).check (al, err))
		  error = true;
	      }
	  }
      else if (types[key] == AList)
	if (size[key] != Singleton)
	  {
	    daisy_assert (vl.size (key) != Syntax::Singleton);
	    const vector<AttributeList*>& seq = vl.alist_sequence (key);
	    {
	      Treelog::Open nest (err, key);
	      for (unsigned int j = 0;
		   j < syntax[key]->impl.list_checker.size ();
		   j++)
		{
		  if (!syntax[key]->impl.list_checker[j] (seq, err))
		    {
		      error = true;
		      break;
		    }
		}
	    }
	    int j_index = 0;
	    for (vector<AttributeList*>::const_iterator j = seq.begin ();
		 j != seq.end ();
		 j++)
	      {
		TmpStream tmp;
		tmp () << key << " [" << j_index << "]";
		Treelog::Open nest (err, tmp.str ());
		j_index++;
		const AttributeList& al = **j;
		if (!syntax[key]->impl.check (al, err))
		  error = true;
	      }
	  }
	else 
	  {
	    Treelog::Open nest (err, key);
	    if (!syntax[key]->impl.check (vl.alist (key), err))
	      error = true;
	  }
    }
  if (!error)
    {
      for (unsigned int j = 0; j < checker.size (); j++)
	{
	  if (!checker[j] (vl, err))
	    {
	      error = true;
	      break;
	    }
	}
    }
  return !error;
}

void
Syntax::Implementation::check (const string& key, const double value) const
{
  check_map::const_iterator i = num_checks.find (key);

  if (i != num_checks.end ())
    (*i).second->check (value);
}


Syntax::type 
Syntax::Implementation::lookup (const string& key) const
{
  type_map::const_iterator i = types.find (key);

  if (i == types.end ())
    return Syntax::Error;
  else
    return (*i).second;
}

int
Syntax::Implementation::order_number (const string& name) const
{
  for (unsigned int j = 0; j < order.size (); j++)
    if (order[j] == name)
      return j;
  return -1;
}

void
Syntax::Implementation::entries (vector<string>& result) const
{
  // All the ordered items first.
  result = order;

  for (type_map::const_iterator i = types.begin ();
       i != types.end ();
       i++)
    {
      const string name = (*i).first;
      
      if (order_number (name) < 0)
	result.push_back (name);
    }
}

const string&
Syntax::Unknown ()
{
  static const string unknown = "<unknown>";
  return unknown;
}

const string&
Syntax::None ()
{
  static const string none = "<none>";
  return none;
}

const string&
Syntax::Fraction ()
{
  static const string fraction = "<fraction>";
  return fraction;
}

// Each syntax entry should have an associated type.

static const char * const type_names[] = 
{ "Number", "AList", "PLF", "Boolean", "String",
  "Date", "Integer", "Object", "Library", "Error", NULL };

const char* 
Syntax::type_name (type t)
{ return type_names[t]; }
    
Syntax::type
Syntax::type_number (const char* name)
{ 
  for (int i = 0; type_names[i]; i++)
    if (strcmp (name, type_names[i]) == 0)
      return static_cast<type> (i);
  return Error;
}

static const char * const category_names[] = 
{ "Const", "State", "OptionalState", "OptionalConst", "LogOnly", NULL };

const char* Syntax::category_name (category c)
{ return category_names[c]; }

int
Syntax::category_number (const char* name)
{ 
  for (int i = 0; category_names[i]; i++)
    if (strcmp (name, category_names[i]) == 0)
      return i;
  return -1;
}

bool
Syntax::check (const AttributeList& vl, Treelog& err) const
{ return impl.check (vl, err);}

void
Syntax::check (const string& key, const double value) const
{ impl.check (key, value); }

bool 
Syntax::check (const AttributeList& vl, 
		    const string& key, Treelog& err) const
{
  bool ok = true;
  Implementation::vcheck_map::const_iterator i = impl.val_checks.find (key);

  if (i != impl.val_checks.end ())
    {
      const VCheck *const vcheck = (*i).second;

      try
	{
	  vcheck->check (*this, vl, key);
	}
      catch (const string& message)
	{
	  err.error (message);
	  ok = false;;
	}
    }
  return ok;
}

Syntax::type 
Syntax::lookup (const string& key) const
{ return impl.lookup (key); }

bool
Syntax::is_const (const string& key) const
{
  daisy_assert (impl.status.find (key) != impl.status.end ());
  return (impl.status[key] == Const
	  || impl.status[key] == OptionalConst);
}

bool
Syntax::is_state (const string& key) const
{
  daisy_assert (impl.status.find (key) != impl.status.end ());
  return (impl.status[key] == State
	  || impl.status[key] == OptionalState);
}

bool
Syntax::is_optional (const string& key) const
{
  daisy_assert (impl.status.find (key) != impl.status.end ());
  return (impl.status[key] == OptionalState 
	  || impl.status[key] == OptionalConst);
}

bool
Syntax::is_log (const string& key) const
{
  daisy_assert (impl.status.find (key) != impl.status.end ());
  return impl.status[key] == LogOnly;
}

const Syntax&
Syntax::syntax (const string& key) const
{
  daisy_assert (impl.syntax.find (key) != impl.syntax.end ());
  return *impl.syntax[key];
}

::Library&
Syntax::library (const string& key) const
{
  daisy_assert (impl.libraries.find (key) != impl.libraries.end ());
  return *impl.libraries[key];
}

int
Syntax::size (const string& key) const
{
  Implementation::size_map::const_iterator i = impl.size.find (key);

  if (i == impl.size.end ())
    return -1;
  else
    return (*i).second;
}

const string&
Syntax::dimension (const string& key) const
{
  Implementation::string_map::const_iterator i = impl.dimensions.find (key);

  if (i == impl.dimensions.end ())
    return Unknown ();
  else
    return (*i).second;
}

const string&
Syntax::domain (const string& key) const
{
  Implementation::string_map::const_iterator i = impl.domains.find (key);

  if (i == impl.domains.end ())
    return Unknown ();
  else
    return (*i).second;
}

const string&
Syntax::range (const string& key) const
{ return dimension (key); }

const string&
Syntax::description (const string& key) const
{
  Implementation::string_map::const_iterator i = impl.descriptions.find (key);

  if (i == impl.descriptions.end ())
    return Unknown ();
  else
    return (*i).second;
}

bool 
Syntax::ordered () const
{
  return impl.order.size () > 0;
}

const vector<string>& 
Syntax::order () const
{
  return impl.order;
}

int
Syntax::order (const string& name) const
{
  return impl.order_number (name);
}

bool
Syntax::total_order () const
{ return impl.order.size () == impl.types.size (); }

const AttributeList& 
Syntax::default_alist (const string& key) const
{
  static const AttributeList empty_alist;

  Implementation::alist_map::const_iterator i = impl.alists.find (key);

  if (i == impl.alists.end ())
    return empty_alist;
  else
    return *((*i).second);
}

void
Syntax::add (const string& key, type t, category req, int s, const string& d)
{
  impl.size[key] = s;
  impl.types[key] = t;
  impl.status[key] = req;
  if (d != Unknown ())
    impl.descriptions[key] = d;
}

void
Syntax::add (const string& key, const string& dim, category req, int sz,
	     const string& d)
{
  add (key, Number, req, sz, d);
  if (d != Unknown ())
    impl.dimensions[key] = dim;
}

void
Syntax::add (const string& key, const string& dim, const Check& check,
	     category req, int sz, const string& d)
{
  add (key, dim, req, sz, d);
  impl.num_checks[key] = &check;
}

void 
Syntax::add_fraction (const string& key, 
		      category cat,
		      int size,
		      const string& description)
{ add (key, Fraction (), Check::fraction (), cat, size, description); } 

void 
Syntax::add_fraction (const string& key, 
		      category cat,
		      const string& description)
{ add (key, Fraction (), Check::fraction (), cat, Singleton, description); } 

void
Syntax::add (const string& key, const string& dom, const string& ran, 
	     category req, int sz, const string& d)
{
  add (key, PLF, req, sz, d);
  if (dom != Unknown ())
    impl.domains[key] = dom;
  if (ran != Unknown ())
    impl.dimensions[key] = ran;
}

void
Syntax::add (const string& key, const string& dom, const string& ran, 
	     const Check& check, category req, int sz, const string& d)
{
  add (key, dom, ran, req, sz, d);
  impl.num_checks[key] = &check;
}

void
Syntax::add (const string& key, const Syntax& s, category req, int sz,
	     const string& d)
{
  add (key, AList, req, sz, d);
  impl.syntax[key] = &s;
}

void
Syntax::add (const string& key, const Syntax& s, const AttributeList& al,
	     category req, int sz, const string& d)
{
  add (key, s, req, sz, d);
  impl.alists[key] = new AttributeList (al);
}

void 
Syntax::add (const string& key, ::Library& l, category req, int s,
	     const string& d)
{
  add (key, Object, req, s, d);
  impl.libraries[key] = &l;
}

void 
Syntax::add_library (const string& key, ::Library& l)
{
  add (key, Library, OptionalConst, None ());
  impl.libraries[key] = &l;
}

void 
Syntax::add_submodule (const string& name, AttributeList& alist,
		       Syntax::category cat, const string& description,
		       load_syntax_fun load_syntax)
{
    Syntax& s = *new Syntax ();
    AttributeList a;
    (*load_syntax) (s, a);

    // Phew.  There are basically two places one can store the alist
    // containing the default values for the variables and parameters
    // of a submodel.  The first place is as an initial value in the
    // parent alist, the other is as the 'default_alist' syntax table
    // attribute.   Neither solution works well in all cases.  
    //
    // Using the initial value will not work for optional singletons,
    // because if there is an initial value in the parent alist, the
    // submodel isn't really optional.  The initial value will ensure
    // that it is alwayes there.  The initial value will not work for
    // submodel sequences either, because we do not know the the
    // length of the sequence. 
    //
    // However, using the 'default_alist' for fully specified
    // non-optional submodels won't work either.  The problems is if
    // the user is satisfied with the default value, and don't try to
    // overwrite anything.  In that case the entry will be empty in
    // the parent alist after loading, causing an 'missing value'
    // error.  
    // 
    // Log variables doesn't have a value, so the problem does not
    // apply to them.
    // 
    // The solution is to treat the three cases separately.

    if (cat == Syntax::LogOnly)
      // Log only, ignore default value.
      add (name, s, cat, Syntax::Singleton, description);
    else if (cat == Syntax::Const || cat == Syntax::State)
      {
	// Mandatory, store in alist.
	add (name, s, cat, Syntax::Singleton, description);
	alist.add (name, a);
      }
    else
      // Optional, store as default_alist.
      add (name, s, a, cat, Syntax::Singleton, description);
}

void 
Syntax::add_submodule_sequence (const string& name, Syntax::category cat, 
				const string& description,
				load_syntax_fun load_syntax)
{
    Syntax& s = *new Syntax ();
    AttributeList a;
    (*load_syntax) (s, a);

    if (cat == Syntax::LogOnly)
      // No default value for log only variables.
      add (name, s, cat, Syntax::Sequence, description);
    else
      // With default value for sequence members.
      add (name, s, a, cat, Syntax::Sequence, description);
}

void 
Syntax::add_check (const string& name, const VCheck& vcheck)
{ 
  impl.val_checks[name] = &vcheck;
}

void 
Syntax::order (const vector<string>& order)
{
  impl.order = order;
}

void 
Syntax::order (const string& one)
{
  daisy_assert (impl.order.size () == 0);
  impl.order.push_back (one);
}

void 
Syntax::order (const string& one, const string& two)
{
  daisy_assert (impl.order.size () == 0);
  impl.order.push_back (one);
  impl.order.push_back (two);
}

void 
Syntax::order (const string& one, const string& two, const string& three)
{
  daisy_assert (impl.order.size () == 0);
  impl.order.push_back (one);
  impl.order.push_back (two);
  impl.order.push_back (three);
}

void 
Syntax::order (const string& one, const string& two, const string& three,
	       const string& four)
{
  daisy_assert (impl.order.size () == 0);
  impl.order.push_back (one);
  impl.order.push_back (two);
  impl.order.push_back (three);
  impl.order.push_back (four);
}

void 
Syntax::order (const string& one, const string& two, const string& three,
	       const string& four, const string& five)
{
  daisy_assert (impl.order.size () == 0);
  impl.order.push_back (one);
  impl.order.push_back (two);
  impl.order.push_back (three);
  impl.order.push_back (four);
  impl.order.push_back (five);
}

void
Syntax::entries (vector<string>& result) const
{
  impl.entries (result);
}

unsigned int
Syntax::entries () const
{ return impl.types.size (); }

void 
Syntax::add_check (check_fun fun)
{ impl.checker.push_back (fun); }

void 
Syntax::add_check (check_list_fun fun)
{ impl.list_checker.push_back (fun); }

Syntax::Syntax ()
  : impl (*new Implementation ())
{ }

Syntax::~Syntax ()
{ delete &impl; }
