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
#include "frame.h"
#include "library.h"
#include "metalib.h"
#include "check.h"
#include "vcheck.h"
#include "assertion.h"
#include "memutils.h"
#include "treelog.h"
#include <sstream>
#include <map>
#include <algorithm>

struct Syntax::Implementation
{
  std::vector<check_fun> checker;
  std::vector<check_object> object_checker;
  std::vector<symbol> order;
  typedef std::map<symbol, Value::type> type_map;
  typedef std::map<symbol, Value::category> status_map;
  typedef std::map<symbol, const Syntax*> syntax_map;
  typedef std::map<symbol, int> size_map;
  typedef std::map<symbol, symbol> library_map;
  typedef std::map<symbol, symbol> string_map;
  typedef std::map<symbol, const AttributeList*> alist_map;
  typedef std::map<symbol, const Check*> check_map;
  typedef std::map<symbol, const VCheck*> vcheck_map;

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
  void check (const symbol key, double value) const;
  Value::type lookup (const symbol key) const;
  int order_number (const symbol name) const;
  void entries (std::vector<symbol>& result) const;
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
      const symbol key = (*i).first;
      if (vl.is_reference (key))
        continue;
      else if (!vl.check (key))
	{
	  if (status[key] == Value::Const || status[key] == Value::State)
	    {
	      msg.error (key + " is missing");
	      error = true;
	    }
	  continue;
	}
      // We can't do a generic value check here, because we don't have
      // the Syntax.  Sigh.
      // vcheck_map::const_iterator i = val_checks.find (key);
      
      // Spcial handling of various types.
      if (types[key] == Value::Number)
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

	      if (size[key] != Value::Singleton)
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
      else if (types[key] == Value::Object)
	if (size[key] != Value::Singleton)
	  {
	    const ::Library& lib = metalib.library (libraries[key]);
	    const std::vector<const AttributeList*>& seq = vl.alist_sequence (key);
	    int j_index = 0;
	    for (std::vector<const AttributeList*>::const_iterator j = seq.begin ();
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
		else if (!lib.check (al.name ("type")))
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
                    const Syntax& ssyn = lib.syntax (al.name ("type"));
		    if (!ssyn.impl->check (metalib, al, msg))
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
		if (!lib.syntax (al.name ("type")).check (metalib, 
                                                                al, msg))
		  error = true;
	      }
	  }
      else if (types[key] == Value::AList)
	if (size[key] != Value::Singleton)
	  {
	    daisy_assert (vl.size (key) != Value::Singleton);
	    const std::vector<const AttributeList*>& seq = vl.alist_sequence (key);
	    int j_index = 0;
	    for (std::vector<const AttributeList*>::const_iterator j = seq.begin ();
		 j != seq.end ();
		 j++)
	      {
		std::ostringstream tmp;
		tmp << key << " [" << j_index << "]";
		Treelog::Open nest (msg, tmp.str ());
		j_index++;
		const AttributeList& al = **j;
		if (!syntax[key]->impl->check (metalib, al, msg))
		  error = true;
	      }
	  }
	else 
	  {
	    Treelog::Open nest (msg, key);
	    if (!syntax[key]->impl->check (metalib , vl.alist (key), msg))
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
Syntax::Implementation::check (const symbol key, const double value) const
{
  check_map::const_iterator i = num_checks.find (key);

  if (i != num_checks.end ())
    (*i).second->check (value);
}


Value::type 
Syntax::Implementation::lookup (const symbol key) const
{
  type_map::const_iterator i = types.find (key);

  if (i == types.end ())
    return Value::Error;
  else
    return (*i).second;
}

int
Syntax::Implementation::order_number (const symbol name) const
{
  for (unsigned int j = 0; j < order.size (); j++)
    if (order[j] == name)
      return j;
  return -1;
}

void
Syntax::Implementation::entries (std::vector<symbol>& result) const
{
  // All the ordered items first.
  for (unsigned int j = 0; j < order.size (); j++)
    result.push_back (order[j]);

  for (type_map::const_iterator i = types.begin ();
       i != types.end ();
       i++)
    {
      const symbol name = (*i).first;
      
      if (order_number (name) < 0)
	result.push_back (name);
    }
}

bool
Syntax::check (const Metalib& metalib, 
               const AttributeList& vl, Treelog& err) const
{ return impl->check (metalib, vl, err);}

void
Syntax::check (const symbol key, const double value) const
{ impl->check (key, value); }

bool 
Syntax::check (const Metalib& metalib, const AttributeList& vl, 
               const symbol key, Treelog& err) const
{
  bool ok = true;
  Implementation::vcheck_map::const_iterator i = impl->val_checks.find (key);

  if (i != impl->val_checks.end ())
    {
      const VCheck *const vcheck = (*i).second;

      try
	{
	  vcheck->check (metalib, *this, vl, key);
	}
      catch (const std::string& message)
	{
	  err.error (message);
	  ok = false;;
	}
    }
  return ok;
}

Value::type 
Syntax::lookup (const symbol key) const
{ return impl->lookup (key); }

bool
Syntax::is_const (const symbol key) const
{
  if (impl->status.find (key) == impl->status.end ())
    return false;

  return (impl->status[key] == Value::Const
	  || impl->status[key] == Value::OptionalConst);
}

bool
Syntax::is_state (const symbol key) const
{
  if (impl->status.find (key) == impl->status.end ())
    return false;

  return (impl->status[key] == Value::State
	  || impl->status[key] == Value::OptionalState);
}

bool
Syntax::is_optional (const symbol key) const
{
  if (impl->status.find (key) == impl->status.end ())
    return false;

  return (impl->status[key] == Value::OptionalState 
	  || impl->status[key] == Value::OptionalConst);
}

bool
Syntax::is_log (const symbol key) const
{
  if (impl->status.find (key) == impl->status.end ())
    return false;
  
  return impl->status[key] == Value::LogOnly;
}

const Syntax&
Syntax::syntax (const symbol key) const
{
  daisy_assert (impl->syntax.find (key) != impl->syntax.end ());
  return *impl->syntax[key];
}

::Library&
Syntax::library (const Metalib& metalib, const symbol key) const
{
  daisy_assert (impl->libraries.find (key) != impl->libraries.end ());
  return metalib.library (impl->libraries[key]);
}

symbol 
Syntax::component (const symbol key) const
{
  daisy_assert (impl->libraries.find (key) != impl->libraries.end ());
  return impl->libraries[key];
}

int
Syntax::size (const symbol key) const
{
  Implementation::size_map::const_iterator i = impl->size.find (key);

  if (i == impl->size.end ())
    return -1;
  else
    return (*i).second;
}

symbol
Syntax::dimension (const symbol key) const
{
  Implementation::string_map::const_iterator i = impl->dimensions.find (key);

  if (i == impl->dimensions.end ())
    return Value::Unknown ();
  else
    return (*i).second;
}

symbol
Syntax::domain (const symbol key) const
{
  Implementation::string_map::const_iterator i = impl->domains.find (key);

  if (i == impl->domains.end ())
    return Value::Unknown ();
  else
    return (*i).second;
}

symbol
Syntax::range (const symbol key) const
{ return dimension (key); }

symbol
Syntax::description (const symbol key) const
{
  Implementation::string_map::const_iterator i = impl->descriptions.find (key);

  if (i == impl->descriptions.end ())
    return Value::Unknown ();
  else
    return (*i).second;
}

bool 
Syntax::ordered () const
{
  return impl->order.size () > 0;
}

const std::vector<symbol>& 
Syntax::order () const
{
  return impl->order;
}

int
Syntax::order_index (const symbol name) const
{
  return impl->order_number (name);
}

bool
Syntax::total_order () const
{ 
  size_t non_logs = 0;
  for (Implementation::status_map::iterator i = impl->status.begin ();
       i != impl->status.end ();
       i++)
    {
      if ((*i).second != Value::LogOnly)
	non_logs++;
    }
  return impl->order.size () == non_logs; 
}

const AttributeList& 
Syntax::default_alist (const symbol key) const
{
  static const AttributeList empty_alist;

  Implementation::alist_map::const_iterator i = impl->alists.find (key);

  if (i == impl->alists.end ())
    return empty_alist;
  else
    return *((*i).second);
}

void
Syntax::add (const symbol key, Value::type t, Value::category req, int s, const symbol d)
{
  if (impl->size.find (key) != impl->size.end ())
    daisy_panic ("'" + key + "': already defined in syntax");
  impl->size[key] = s;
  impl->types[key] = t;
  impl->status[key] = req;
  if (d != Value::Unknown ())
    impl->descriptions[key] = d;
}

void
Syntax::add (const symbol key, const symbol dim, Value::category req, int sz,
	     const symbol d)
{
  add (key, Value::Number, req, sz, d);
  if (d != Value::Unknown ())
    impl->dimensions[key] = dim;
}

void
Syntax::add (const symbol key, const symbol dim, const Check& check,
	     Value::category req, int sz, const symbol d)
{
  add (key, dim, req, sz, d);
  impl->num_checks[key] = &check;
}

void 
Syntax::add_fraction (const symbol key, 
		      Value::category cat,
		      int size,
		      const symbol description)
{ add (key, Value::Fraction (), Check::fraction (), cat, size, description); } 

void 
Syntax::add_fraction (const symbol key, 
		      Value::category cat,
		      const symbol description)
{ add (key, Value::Fraction (), Check::fraction (), cat, Value::Singleton, description); } 

void
Syntax::add (const symbol key, const symbol dom, const symbol ran, 
	     Value::category req, int sz, const symbol d)
{
  add (key, Value::PLF, req, sz, d);
  if (dom != Value::Unknown ())
    impl->domains[key] = dom;
  if (ran != Value::Unknown ())
    impl->dimensions[key] = ran;
}

void
Syntax::add (const symbol key, const symbol dom, const symbol ran, 
	     const Check& check, Value::category req, int sz, const symbol d)
{
  add (key, dom, ran, req, sz, d);
  impl->num_checks[key] = &check;
}

void
Syntax::add (const symbol key, const Syntax& s, Value::category req, int sz,
	     const symbol d)
{
  add (key, Value::AList, req, sz, d);
  impl->syntax[key] = &s;
}

void
Syntax::add (const symbol key, const Syntax& s, const AttributeList& al,
	     Value::category req, int sz, const symbol d)
{
  add (key, s, req, sz, d);
  impl->alists[key] = new AttributeList (al);
}

void 
Syntax::add_object (const symbol key, const char *const l,
                    Value::category req, int s, const symbol d)
{ add_object (key, symbol (l), req, s, d); }

void 
Syntax::add_object (const symbol key, const symbol l,
                    Value::category req, int s, const symbol d)
{
  add (key, Value::Object, req, s, d);
  impl->libraries[key] = l;
}

void 
Syntax::add_library (const symbol key, const symbol l)
{
  add (key, Value::Library, Value::OptionalConst, Value::None ());
  impl->libraries[key] = l;
}

void 
Syntax::add_submodule_sequence (const symbol name, Value::category cat, 
				const symbol description,
				load_syntax_fun load_syntax)
{   
    const Frame frame (load_syntax);
    const Syntax& s = *new Syntax (frame.syntax ());
    const AttributeList& a = frame.alist ();

    if (cat == Value::LogOnly)
      // No default value for log only variables.
      add (name, s, cat, Value::Sequence, description);
    else
      // With default value for sequence members.
      add (name, s, a, cat, Value::Sequence, description);
}

void 
Syntax::add_check (const symbol name, const VCheck& vcheck)
{ 
  impl->val_checks[name] = &vcheck;
}

void 
Syntax::order (const std::vector<symbol>& order)
{
  impl->order = order;
}

void 
Syntax::order (const symbol one)
{
  daisy_assert (impl->order.size () == 0);
  impl->order.push_back (one);
}

void 
Syntax::order (const symbol one, const symbol two)
{
  daisy_assert (impl->order.size () == 0);
  impl->order.push_back (one);
  impl->order.push_back (two);
}

void 
Syntax::order (const symbol one, const symbol two, const symbol three)
{
  daisy_assert (impl->order.size () == 0);
  impl->order.push_back (one);
  impl->order.push_back (two);
  impl->order.push_back (three);
}

void 
Syntax::order (const symbol one, const symbol two, const symbol three,
	       const symbol four)
{
  daisy_assert (impl->order.size () == 0);
  impl->order.push_back (one);
  impl->order.push_back (two);
  impl->order.push_back (three);
  impl->order.push_back (four);
}

void 
Syntax::order (const symbol one, const symbol two, const symbol three,
	       const symbol four, const symbol five)
{
  daisy_assert (impl->order.size () == 0);
  impl->order.push_back (one);
  impl->order.push_back (two);
  impl->order.push_back (three);
  impl->order.push_back (four);
  impl->order.push_back (five);
}

void
Syntax::entries (std::vector<symbol>& result) const
{
  impl->entries (result);
}

unsigned int
Syntax::entries () const
{ return impl->types.size () + 0U; }

void 
Syntax::add_check (check_fun fun)
{ impl->checker.push_back (fun); }

void 
Syntax::add_object_check (check_object fun)
{ impl->object_checker.push_back (fun); }

Syntax::Syntax ()
  : impl (new Implementation ())
{ }

Syntax::Syntax (const Syntax& old)
  : impl (new Implementation (*old.impl))
{ }

Syntax::~Syntax ()
{ }

// syntax.C ends her.
