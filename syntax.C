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

#define BUILD_DLL

#include "syntax.h"
#include "alist.h"
#include "library.h"
#include "metalib.h"
#include "symbol.h"
#include "check.h"
#include "vcheck.h"
#include "assertion.h"
#include "memutils.h"
#include <sstream>
#include <map>
#include <algorithm>

const int Syntax::Singleton = -117;
const int Syntax::Sequence = -3212;
const int Syntax::Unspecified = -666;

struct Syntax::Implementation
{
  std::vector<check_fun> checker;
  std::vector<check_object> object_checker;
  std::vector<std::string> order;
  typedef std::map<std::string, type> type_map;
  typedef std::map<std::string, category> status_map;
  typedef std::map<std::string, const Syntax*> syntax_map;
  typedef std::map<std::string, int> size_map;
  typedef std::map<std::string, symbol> library_map;
  typedef std::map<std::string, std::string> string_map;
  typedef std::map<std::string, const AttributeList*> alist_map;
  typedef std::map<std::string, const Check*> check_map;
  typedef std::map<std::string, const VCheck*> vcheck_map;

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

  bool check (const Metalib&, const AttributeList& vl, Treelog& err);
  void check (const std::string& key, double value) const;
  Syntax::type lookup (const std::string& key) const;
  int order_number (const std::string& name) const;
  void entries (std::vector<std::string>& result) const;
  Implementation ()
  { }
  Implementation (const Implementation& old)
    : checker (old.checker),
      object_checker (old.object_checker),
      order (old.order),
      types (old.types),
      status (old.status),
      size (old.size),
      libraries (old.libraries),
      domains (old.domains),
      dimensions (old.dimensions),
      num_checks (old.num_checks),
      val_checks (old.val_checks),
      descriptions (old.descriptions)
  {
    // Clone syntax.
    for (syntax_map::const_iterator i = old.syntax.begin ();
	 i != old.syntax.end (); 
	 i++)
      syntax[(*i).first] = new Syntax (*(*i).second);
    // Clone alists.
    for (alist_map::const_iterator i = old.alists.begin ();
	 i != old.alists.end (); 
	 i++)
      alists[(*i).first] = new AttributeList (*(*i).second);
  }
  ~Implementation ()
  {
    map_delete (syntax.begin (), syntax.end ());
    map_delete (alists.begin (), alists.end ()); 
  }
};    

bool 
Syntax::Implementation::check (const Metalib& metalib,
                               const AttributeList& vl, Treelog& msg)
{
  bool error = false;

  for (status_map::const_iterator i = status.begin ();
       i != status.end ();
       i++)
    {
      const std::string key = (*i).first;
      if (vl.is_reference (key))
        continue;
      else if (!vl.check (key))
	{
	  if (status[key] == Const || status[key] == State)
	    {
	      msg.error (key + " missing");
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
	  // This should already be checked by the file parser, but you
	  // never know... better safe than sorry...  don't drink and
	  // drive...  Well, theoretically the alist could come from
	  // another source (like one of the many other parsers :/), or
	  // be one of the build in ones.
	  check_map::const_iterator i = num_checks.find (key);

	  if (i != num_checks.end ())
	    {
	      const Check *const check = (*i).second;

	      if (size[key] != Singleton)
		{
		  const std::vector<double>& array = vl.number_sequence (key);
		  for (unsigned int i = 0; i < array.size (); i++)
		    try 
		      { check->check (array[i]); }
		    catch (const std::string& message)
		      {
			std::ostringstream str;
			str << key << "[" << i << "]: " << message;
			msg.error (str.str ());
			error = true;
		      }
		}
	      else try 
		{ check->check (vl.number (key)); }
	      catch (const std::string& message)
		{
		  msg.error (key + ": " + message);
		  error = true;
		}
	    }
	}
      else if (types[key] == Object)
	if (size[key] != Singleton)
	  {
	    const ::Library& lib = metalib.library (libraries[key]);
	    const std::vector<AttributeList*>& seq = vl.alist_sequence (key);
	    int j_index = 0;
	    for (std::vector<AttributeList*>::const_iterator j = seq.begin ();
		 j != seq.end ();
		 j++)
	      {
		std::ostringstream tmp;
		tmp << key << "[" << j_index << "]: ";
		j_index++;
		const AttributeList& al = **j;
		if (!al.check ("type"))
		  {
		    tmp << "Non object found";
		    msg.error (tmp.str ());
		    error = true;
		  }
		else if (!lib.check (al.identifier ("type")))
		  {
		    tmp << "Unknown library member '"
			   << al.name ("type") << "'";
		    msg.error (tmp.str ());
		    error = true;
		  }
		else 
		  {
		    tmp << al.name ("type");
		    Treelog::Open nest (msg, tmp.str ());
                    const Syntax& ssyn = lib.syntax (al.identifier ("type"));
		    if (!ssyn.impl.check (metalib, al, msg))
		      error = true;
		  }
	      }
	  }
	else 
	  {
	    const ::Library& lib = metalib.library (libraries[key]);
	    const AttributeList& al = vl.alist (key);
	    if (!al.check ("type"))
	      {
		msg.error (key + "Non object found");
		error = true;
	      }
	    else 
	      {
		Treelog::Open nest (msg, key + ": " + al.name ("type"));
		if (!lib.syntax (al.identifier ("type")).check (metalib, 
                                                                al, msg))
		  error = true;
	      }
	  }
      else if (types[key] == AList)
	if (size[key] != Singleton)
	  {
	    daisy_assert (vl.size (key) != Syntax::Singleton);
	    const std::vector<AttributeList*>& seq = vl.alist_sequence (key);
	    int j_index = 0;
	    for (std::vector<AttributeList*>::const_iterator j = seq.begin ();
		 j != seq.end ();
		 j++)
	      {
		std::ostringstream tmp;
		tmp << key << " [" << j_index << "]";
		Treelog::Open nest (msg, tmp.str ());
		j_index++;
		const AttributeList& al = **j;
		if (!syntax[key]->impl.check (metalib, al, msg))
		  error = true;
	      }
	  }
	else 
	  {
	    Treelog::Open nest (msg, key);
	    if (!syntax[key]->impl.check (metalib , vl.alist (key), msg))
	      error = true;
	  }
    }
  if (!error)
    {
      for (unsigned int j = 0; j < checker.size (); j++)
	{
	  if (!checker[j] (vl, msg))
	    {
	      error = true;
	      break;
	    }
	}
    }
  if (!error)
    {
      for (unsigned int j = 0; j < object_checker.size (); j++)
	{
	  if (!object_checker[j] (metalib, vl, msg))
	    {
	      error = true;
	      break;
	    }
	}
    }
  return !error;
}

void
Syntax::Implementation::check (const std::string& key, const double value) const
{
  check_map::const_iterator i = num_checks.find (key);

  if (i != num_checks.end ())
    (*i).second->check (value);
}


Syntax::type 
Syntax::Implementation::lookup (const std::string& key) const
{
  type_map::const_iterator i = types.find (key);

  if (i == types.end ())
    return Syntax::Error;
  else
    return (*i).second;
}

int
Syntax::Implementation::order_number (const std::string& name) const
{
  for (unsigned int j = 0; j < order.size (); j++)
    if (order[j] == name)
      return j;
  return -1;
}

void
Syntax::Implementation::entries (std::vector<std::string>& result) const
{
  // All the ordered items first.
  result = order;

  for (type_map::const_iterator i = types.begin ();
       i != types.end ();
       i++)
    {
      const std::string name = (*i).first;
      
      if (order_number (name) < 0)
	result.push_back (name);
    }
}

// Each syntax entry should have an associated type.

static const std::string type_names[] = 
{ "Number", "AList", "PLF", "Boolean", "String",
  "Integer", "Object", "Library", "Error" };

const std::string&
Syntax::type_name (type t)
{ return type_names[t]; }
    
Syntax::type
Syntax::type_number (const std::string& name)
{ 
  for (int i = 0; type_names[i] != "Error"; i++)
    if (name == type_names[i])
      return static_cast<type> (i);
  return Error;
}

symbol
Syntax::unknown ()
{
  static const symbol unknown ("<unknown>");
  return unknown; 
}

symbol
Syntax::none ()
{
  static const symbol none ("<none>");
  return none;
}

symbol
Syntax::fraction ()
{
  static const symbol fraction ("<fraction>");
  return fraction; 
}

symbol
Syntax::user ()
{
  static const symbol user ("<user>");
  return user; 
}

const std::string& 
Syntax::Unknown ()
{ return unknown ().name (); }

const std::string& 
Syntax::None ()
{ return none ().name (); }

const std::string& 
Syntax::Fraction ()
{ return fraction ().name (); }

const std::string& 
Syntax::User ()
{ return user ().name (); }

static const std::string category_names[] = 
{ "Const", "State", "OptionalState", "OptionalConst", "LogOnly"};

const std::string& Syntax::category_name (category c)
{ return category_names[c]; }

int
Syntax::category_number (const std::string& name)
{ 
  for (int i = 0; category_names[i] != "LogOnly"; i++)
    if (name == category_names[i])
      return i;
  return -1;
}

bool
Syntax::check (const Metalib& metalib, 
               const AttributeList& vl, Treelog& err) const
{ return impl.check (metalib, vl, err);}

void
Syntax::check (const std::string& key, const double value) const
{ impl.check (key, value); }

bool 
Syntax::check (const AttributeList& vl, 
               const std::string& key, Treelog& err) const
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
      catch (const std::string& message)
	{
	  err.error (message);
	  ok = false;;
	}
    }
  return ok;
}

Syntax::type 
Syntax::lookup (const std::string& key) const
{ return impl.lookup (key); }

bool
Syntax::is_const (const std::string& key) const
{
  if (impl.status.find (key) == impl.status.end ())
    return false;

  return (impl.status[key] == Const
	  || impl.status[key] == OptionalConst);
}

bool
Syntax::is_state (const std::string& key) const
{
  if (impl.status.find (key) == impl.status.end ())
    return false;

  return (impl.status[key] == State
	  || impl.status[key] == OptionalState);
}

bool
Syntax::is_optional (const std::string& key) const
{
  if (impl.status.find (key) == impl.status.end ())
    return false;

  return (impl.status[key] == OptionalState 
	  || impl.status[key] == OptionalConst);
}

bool
Syntax::is_log (const std::string& key) const
{
  if (impl.status.find (key) == impl.status.end ())
    return false;
  
  return impl.status[key] == LogOnly;
}

const Syntax&
Syntax::syntax (const std::string& key) const
{
  daisy_assert (impl.syntax.find (key) != impl.syntax.end ());
  return *impl.syntax[key];
}

::Library&
Syntax::library (const Metalib& metalib, const std::string& key) const
{
  daisy_assert (impl.libraries.find (key) != impl.libraries.end ());
  return metalib.library (impl.libraries[key]);
}

int
Syntax::size (const std::string& key) const
{
  Implementation::size_map::const_iterator i = impl.size.find (key);

  if (i == impl.size.end ())
    return -1;
  else
    return (*i).second;
}

const std::string&
Syntax::dimension (const std::string& key) const
{
  Implementation::string_map::const_iterator i = impl.dimensions.find (key);

  if (i == impl.dimensions.end ())
    return Unknown ();
  else
    return (*i).second;
}

const std::string&
Syntax::domain (const std::string& key) const
{
  Implementation::string_map::const_iterator i = impl.domains.find (key);

  if (i == impl.domains.end ())
    return Unknown ();
  else
    return (*i).second;
}

const std::string&
Syntax::range (const std::string& key) const
{ return dimension (key); }

const std::string&
Syntax::description (const std::string& key) const
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

const std::vector<std::string>& 
Syntax::order () const
{
  return impl.order;
}

int
Syntax::order (const std::string& name) const
{
  return impl.order_number (name);
}

bool
Syntax::total_order () const
{ 
  size_t non_logs = 0;
  for (Implementation::status_map::iterator i = impl.status.begin ();
       i != impl.status.end ();
       i++)
    {
      if ((*i).second != LogOnly)
	non_logs++;
    }
  return impl.order.size () == non_logs; 
}

const AttributeList& 
Syntax::default_alist (const std::string& key) const
{
  static const AttributeList empty_alist;

  Implementation::alist_map::const_iterator i = impl.alists.find (key);

  if (i == impl.alists.end ())
    return empty_alist;
  else
    return *((*i).second);
}

void
Syntax::add (const std::string& key, type t, category req, int s, const std::string& d)
{
  if (impl.size.find (key) != impl.size.end ())
    daisy_panic ("'" + key + "': already defined in syntax");
  impl.size[key] = s;
  impl.types[key] = t;
  impl.status[key] = req;
  if (d != Unknown ())
    impl.descriptions[key] = d;
}

void
Syntax::add (const std::string& key, const std::string& dim, category req, int sz,
	     const std::string& d)
{
  add (key, Number, req, sz, d);
  if (d != Unknown ())
    impl.dimensions[key] = dim;
}

void
Syntax::add (const std::string& key, const std::string& dim, const Check& check,
	     category req, int sz, const std::string& d)
{
  add (key, dim, req, sz, d);
  impl.num_checks[key] = &check;
}

void 
Syntax::add_fraction (const std::string& key, 
		      category cat,
		      int size,
		      const std::string& description)
{ add (key, Fraction (), Check::fraction (), cat, size, description); } 

void 
Syntax::add_fraction (const std::string& key, 
		      category cat,
		      const std::string& description)
{ add (key, Fraction (), Check::fraction (), cat, Singleton, description); } 

void
Syntax::add (const std::string& key, const std::string& dom, const std::string& ran, 
	     category req, int sz, const std::string& d)
{
  add (key, PLF, req, sz, d);
  if (dom != Unknown ())
    impl.domains[key] = dom;
  if (ran != Unknown ())
    impl.dimensions[key] = ran;
}

void
Syntax::add (const std::string& key, const std::string& dom, const std::string& ran, 
	     const Check& check, category req, int sz, const std::string& d)
{
  add (key, dom, ran, req, sz, d);
  impl.num_checks[key] = &check;
}

void
Syntax::add (const std::string& key, const Syntax& s, category req, int sz,
	     const std::string& d)
{
  add (key, AList, req, sz, d);
  impl.syntax[key] = &s;
}

void
Syntax::add (const std::string& key, const Syntax& s, const AttributeList& al,
	     category req, int sz, const std::string& d)
{
  add (key, s, req, sz, d);
  impl.alists[key] = new AttributeList (al);
}

void 
Syntax::add_object (const std::string& key, const char *const l,
                    category req, int s, const std::string& d)
{ add_object (key, symbol (l), req, s, d); }

void 
Syntax::add_object (const std::string& key, const symbol l,
                    category req, int s, const std::string& d)
{
  add (key, Object, req, s, d);
  impl.libraries[key] = l;
}

void 
Syntax::add_library (const std::string& key, const symbol l)
{
  add (key, Library, OptionalConst, None ());
  impl.libraries[key] = l;
}

void 
Syntax::add_submodule (const std::string& name, AttributeList& alist,
		       Syntax::category cat, const std::string& description,
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
Syntax::add_submodule_sequence (const std::string& name, Syntax::category cat, 
				const std::string& description,
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
Syntax::add_check (const std::string& name, const VCheck& vcheck)
{ 
  impl.val_checks[name] = &vcheck;
}

void 
Syntax::order (const std::vector<std::string>& order)
{
  impl.order = order;
}

void 
Syntax::order (const std::string& one)
{
  daisy_assert (impl.order.size () == 0);
  impl.order.push_back (one);
}

void 
Syntax::order (const std::string& one, const std::string& two)
{
  daisy_assert (impl.order.size () == 0);
  impl.order.push_back (one);
  impl.order.push_back (two);
}

void 
Syntax::order (const std::string& one, const std::string& two, const std::string& three)
{
  daisy_assert (impl.order.size () == 0);
  impl.order.push_back (one);
  impl.order.push_back (two);
  impl.order.push_back (three);
}

void 
Syntax::order (const std::string& one, const std::string& two, const std::string& three,
	       const std::string& four)
{
  daisy_assert (impl.order.size () == 0);
  impl.order.push_back (one);
  impl.order.push_back (two);
  impl.order.push_back (three);
  impl.order.push_back (four);
}

void 
Syntax::order (const std::string& one, const std::string& two, const std::string& three,
	       const std::string& four, const std::string& five)
{
  daisy_assert (impl.order.size () == 0);
  impl.order.push_back (one);
  impl.order.push_back (two);
  impl.order.push_back (three);
  impl.order.push_back (four);
  impl.order.push_back (five);
}

void
Syntax::entries (std::vector<std::string>& result) const
{
  impl.entries (result);
}

unsigned int
Syntax::entries () const
{ return impl.types.size () + 0U; }

void 
Syntax::add_check (check_fun fun)
{ impl.checker.push_back (fun); }

void 
Syntax::add_object_check (check_object fun)
{ impl.object_checker.push_back (fun); }

Syntax::Syntax ()
  : impl (*new Implementation ())
{ }

Syntax::Syntax (const Syntax& old)
  : impl (*new Implementation (old.impl))
{ }

Syntax::~Syntax ()
{ delete &impl; }
