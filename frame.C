// frame.C -- Names with typed values.
// 
// Copyright 2008 Per Abrahamsen and KVL.
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

#include "frame.h"
#include "frame_model.h"
#include "syntax.h"
#include "block.h"
#include "assertion.h"
#include "librarian.h"
#include "intrinsics.h"
#include "library.h"
#include "memutils.h"
#include "alist.h"
#include <vector>
#include <set>
#include <sstream>

struct Frame::Implementation
{
  static int counter;
  int count;
  Syntax syntax;
  AttributeList alist;
  typedef std::set<const Frame*> child_set;
  mutable child_set children;

  Implementation (const Implementation& old)
    : count (counter),
      syntax (old.syntax),
      alist (old.alist)
      
  { counter++; }
  Implementation (int old_count, child_set old_children)
    : count (old_count),
      children (old_children)
  { }
  Implementation ()
    : count (counter)
  { counter++; }
};

int
Frame::Implementation::counter = 0;

const Frame* 
Frame::parent () const
{ return NULL; }

static void describe_frame (const Frame& frame, std::ostream& out)
{
  out << frame.impl->count;
  if (frame.check ("type"))
    out << " type = " << frame.name ("type");
  if (frame.check ("base model"))
    out << " base_model = " << frame.name ("base_model");
  if (frame.check ("submodel"))
    out << " submodel = " << frame.name ("submodel");
}

void 
Frame::register_child (const Frame* child) const
{ 
  daisy_assert (child != this);
  if (impl->children.find (child) != impl->children.end ())
    {
      std::ostringstream tmp;
      tmp << "Dual definition of frame ";
      describe_frame (*child, tmp);
      tmp << " in ";
      describe_frame (*this, tmp);
      daisy_warning (tmp.str ());
    }
  else
    impl->children.insert (child); 
}

void 
Frame::unregister_child (const Frame* child) const
{
  const Implementation::child_set::const_iterator i 
    = impl->children.find (child);
  daisy_assert (i != impl->children.end ());
  impl->children.erase (i);
  const Implementation::child_set::const_iterator j 
    = impl->children.find (child);
  daisy_assert (j == impl->children.end ());
}

void 
Frame::reparent_children (const Frame* new_parent) const
{
  for (Implementation::child_set::const_iterator i = impl->children.begin ();
       i != impl->children.end ();
       i++)
    {
      (*i)->replace_parent (new_parent);
      if (new_parent)
        new_parent->register_child (*i);
    }
  impl->children.erase (impl->children.begin (), impl->children.end ());
}

void
Frame::replace_parent (const Frame*) const
{ daisy_assert (!parent ()); }

AttributeList& 
Frame::alist () const
{ return impl->alist; }

void 
Frame::entries (std::set<symbol>& e) const
{ 
  if (parent ())
    parent ()->entries (e);
  impl->syntax.entries (e); 
}

bool 
Frame::check (Block& block) const
{ return check (block.metalib (), block.msg ()); }

bool 
Frame::check (Metalib& metalib, Treelog& msg) const
{ return impl->syntax.check (metalib, *this, msg); }

void 
Frame::check (const symbol key, double value) const
{ 
  if (parent () && impl->syntax.lookup (key) == Value::Error)
    parent ()->check (key, value); 
  else
    impl->syntax.check (key, value); 
}

bool 
Frame::check (Metalib& metalib, 
              const symbol key, Treelog& msg) const
{ return impl->syntax.check (metalib, *this, key, msg); }

bool 
Frame::is_const (const symbol key) const
{
  if (parent () && impl->syntax.lookup (key) == Value::Error)
    return parent ()->is_const (key);
  else
    return impl->syntax.is_const (key);
}

bool 
Frame::is_optional (const symbol key) const
{
  if (parent () && impl->syntax.lookup (key) == Value::Error)
    return parent ()->is_optional (key);
  else
    return impl->syntax.is_optional (key);
}

bool 
Frame::is_log (const symbol key) const
{
  if (parent () && impl->syntax.lookup (key) == Value::Error)
    return parent ()->is_log (key);
  else
    return impl->syntax.is_log (key);
}

bool 
Frame::is_state (const symbol key) const
{
  if (parent () && impl->syntax.lookup (key) == Value::Error)
    return parent ()->is_state (key);
  else
    return impl->syntax.is_state (key);
}

Value::type 
Frame::lookup (const symbol key) const
{
  if (parent () && impl->syntax.lookup (key) == Value::Error)
    return parent ()->lookup (key);
  else
    return impl->syntax.lookup (key);
}

::Library& 
Frame::library (Metalib& metalib, const symbol key) const
{
  if (parent () && impl->syntax.lookup (key) == Value::Error)
    return parent ()->library (metalib, key);
  else
    return impl->syntax.library (metalib, key);
}

int  
Frame::type_size (const symbol key) const
{
  if (parent () && impl->syntax.lookup (key) == Value::Error)
    return parent ()->type_size (key);
  else
    return impl->syntax.size (key);
}

symbol 
Frame::dimension (const symbol key) const
{
  if (parent () && impl->syntax.lookup (key) == Value::Error)
    return parent ()->dimension (key);
  else
    return impl->syntax.dimension (key);
}

symbol 
Frame::domain (const symbol key) const
{
  if (parent () && impl->syntax.lookup (key) == Value::Error)
    return parent ()->domain (key);
  else
    return impl->syntax.domain (key);
}

symbol 
Frame::range (const symbol key) const
{
  if (parent () && impl->syntax.lookup (key) == Value::Error)
    return parent ()->range (key);
  else
    return impl->syntax.range (key);
}

symbol 
Frame::description (const symbol key) const
{
  if (parent () && impl->syntax.lookup (key) == Value::Error)
    return parent ()->description (key);
  else
    return impl->syntax.description (key);
}

const Frame& 
Frame::default_frame (const symbol key) const
{
  if (parent () && impl->syntax.lookup (key) == Value::Error)
    return parent ()->default_frame (key);
  else
    return impl->syntax.default_frame (key);
}

symbol 
Frame::submodel_name (const symbol key) const
{ 
  if (parent () && impl->syntax.lookup (key) == Value::Error)
    return parent ()->submodel_name (key);
  else
    return impl->syntax.submodel_name (key);
}

void 
Frame::add (const symbol key,	// Generic.
	    Value::type t, 
	    Value::category cat,
	    int size,
	    const symbol description)
{ impl->syntax.add (key, t, cat, size, description); }


void 
Frame::add (const symbol key, // Number.
	    const symbol dim,
	    Value::category cat,
	    int size,
	    const symbol description)
{ impl->syntax.add (key, dim, cat, size, description); }


void 
Frame::add (const symbol key,
	    const symbol dim,
	    const Check& check,
	    Value::category cat,
	    int size,
	    const symbol description)
{ impl->syntax.add (key, dim, check, cat, size, description); }


void 
Frame::add_fraction (const symbol key, 
		     Value::category cat,
		     int size,
		     const symbol description)
{ impl->syntax.add_fraction (key, cat, size, description); }


void 
Frame::add_fraction (const symbol key, 
		     Value::category cat,
		     const symbol description)
{ impl->syntax.add_fraction (key, cat, description); }


void 
Frame::add (const symbol key, // PLF.
	    const symbol domain,
	    const symbol range,
	    Value::category cat,
	    int size,
	    const symbol description)
{ impl->syntax.add (key, domain, range, cat, size, description); }

void 
Frame::add (const symbol key,
	    const symbol domain,
	    const symbol range,
	    const Check& check,
	    Value::category cat,
	    int size,
	    const symbol description)
{ impl->syntax.add (key, domain, range, check, cat, size, description); }

void 
Frame::add_object (const symbol key, const symbol lib,
                   Value::category cat, int size, const symbol description)
{ impl->syntax.add_object (key, lib, cat, size, description); }

void 
Frame::add_library (const symbol key, symbol lib)
{ impl->syntax.add_library (key, lib); }


void 
Frame::add_submodule (const symbol name, 
		      Value::category cat, const symbol description,
		      load_syntax_t load_syntax)
{
  impl->syntax.add (name, load_syntax, cat, Value::Singleton, description);
  if (cat == Value::Const || cat == Value::State)
    // TODO: Move this to Frame::alist (name) (must return const first).
    impl->alist.add (name, impl->syntax.default_frame (name));
}

void 
Frame::add_submodule_sequence (const symbol name, Value::category cat, 
			       const symbol description,
			       load_syntax_t load_syntax)
{
  impl->syntax.add (name, load_syntax, cat, Value::Sequence, description);
}

void 
Frame::add_check (const symbol name, const VCheck& vcheck)
{ impl->syntax.add_check (name, vcheck); }


void 
Frame::order (const std::vector<symbol>& v)
{ impl->syntax.order (v); }

void 
Frame::order (const symbol a)
{ impl->syntax.order (a); }

void 
Frame::order (const symbol a, const symbol b)
{ impl->syntax.order (a, b); }

void 
Frame::order (const symbol a, const symbol b, const symbol c)
{ impl->syntax.order (a, b, c); }

void 
Frame::order (const symbol a, const symbol b, const symbol c,
	      const symbol d)
{ impl->syntax.order (a, b, c, d); }

void 
Frame::order (const symbol a, const symbol b, const symbol c,
	      const symbol d, const symbol e)
{ impl->syntax.order (a, b, c, d, e); }


bool 
Frame::ordered () const
{ return impl->syntax.ordered (); }

const std::vector<symbol>& 
Frame::order () const
{ return impl->syntax.order (); }

int 
Frame::order_index (const symbol key) const
{
  if (parent () && impl->syntax.lookup (key) == Value::Error)
    return parent ()->order_index (key);
  else
    return impl->syntax.order_index (key);
}

bool 
Frame::total_order () const
{ return impl->syntax.total_order (); }

void 
Frame::add_check (check_fun fun)
{ impl->syntax.add_check (fun); }

bool 
Frame::check (const symbol key) const
{ 
  return impl->alist.check (key)
    || (parent () && parent ()->check (key));
}

// Is this frame a subset of 'other'?
bool 
Frame::subset (Metalib& metalib, const Frame& other) const
{
  // Find syntax entries.
  std::set<symbol> all;
  entries (all);

  // Loop over them.
  for (std::set<symbol>::const_iterator i = all.begin (); i != all.end (); i++)
    if (!subset (metalib, other, *i))
      return false;

  return true;
}

// Is the element 'key' in this alist a subset of the corresponding other entry.
bool 
Frame::subset (Metalib& metalib, const Frame& other,
               const symbol key) const
{
  // Find frame defining my value of key.
  const Frame* me = this;
  while (!me->impl->alist.check (key))
    {
      daisy_assert (me != me->parent ());
      me = me->parent ();
      if (!me)
        // Missing value is always a subset.
        return true;
    }

  // Find frame defining other value of key.
  const Frame* him = &other;
  while (!him->impl->alist.check (key))
    {
      daisy_assert (him != him->parent ());
      him = him->parent ();
      if (!him)
        // Missing value cannot be a superset of a value.
        return false;
    }

  // Both values exist, perform test.
  return me->impl->alist.subset (metalib, him->impl->alist, key);
}

int 
Frame::value_size (const symbol key) const
{
  if (parent () && !impl->alist.check (key))
    return parent ()->value_size (key);
  else
    return impl->alist.size (key);
}

void
Frame::add_reference (const symbol key, const symbol val)
{ impl->alist.add_reference (key, val); }

bool 
Frame::is_reference (const symbol key) const
{ 
  if (parent () && !impl->alist.check (key))
    return parent ()->is_reference (key);
  else
    return impl->alist.is_reference (key);
}

symbol
Frame::get_reference (const symbol key) const
{
  if (parent () && !impl->alist.check (key))
    return parent ()->get_reference (key);
  else
    return impl->alist.get_reference (key);
}

double 
Frame::number (const symbol key) const
{ 
  if (parent () && !impl->alist.check (key))
    return parent ()->number (key);
  else
    return impl->alist.number (key);
}

double 
Frame::number (const symbol key, double default_value) const
{ 
  if (parent () && !impl->alist.check (key))
    return parent ()->number (key, default_value);
  else
    return impl->alist.number (key, default_value);
}

symbol
Frame::name (const symbol key) const
{ 
  if (parent () && !impl->alist.check (key))
    return parent ()->name (key);
  else
    return impl->alist.name (key);
}

symbol
Frame::name (const symbol key, 
             const symbol default_value) const
{ 
  if (parent () && !impl->alist.check (key))
    return parent ()->name (key, default_value);
  else
    return impl->alist.name (key, default_value);
}

bool 
Frame::flag (const symbol key) const
{ 
  if (parent () && !impl->alist.check (key))
    return parent ()->flag (key);
  else
    return impl->alist.flag (key);
}

bool 
Frame::flag (const symbol key, bool default_value) const
{ 
  if (parent () && !impl->alist.check (key))
    return parent ()->flag (key, default_value);
  else
    return impl->alist.flag (key, default_value);
}

const PLF& 
Frame::plf (const symbol key) const
{ 
  if (parent () && !impl->alist.check (key))
    return parent ()->plf (key);
  else
    return impl->alist.plf (key);
}

AttributeList& 
Frame::alist (const symbol key) const
{ 
  if (parent () && !impl->alist.check (key))
    return parent ()->alist (key);
  else
    return impl->alist.alist (key);
}

const Frame& 
Frame::frame (const symbol key) const
{ 
  if (type_size (key) != Value::Singleton)
    return default_frame (key);
  else if (impl->alist.check (key))
    return impl->alist.frame (key);
  else if (parent () && parent ()->check (key))
    return parent ()->frame (key);
  else
    return default_frame (key);
}

const FrameModel&
Frame::model (const symbol key) const
{
  const Frame& frame = this->frame (key);
  return dynamic_cast<const FrameModel&> (frame);
}

int 
Frame::integer (const symbol key) const
{ 
  if (parent () && !impl->alist.check (key))
    return parent ()->integer (key);
  else
    return impl->alist.integer (key);
}

int 
Frame::integer (const symbol key, int default_value) const
{ 
  if (parent () && !impl->alist.check (key))
    return parent ()->integer (key, default_value);
  else
    return impl->alist.integer (key, default_value);
}

const std::vector<double>& 
Frame::number_sequence (const symbol key) const
{ 
  if (parent () && !impl->alist.check (key))
    return parent ()->number_sequence (key);
  else
    return impl->alist.number_sequence (key);
}

const std::vector<symbol>&
Frame::name_sequence (const symbol key) const
{ 
  if (parent () && !impl->alist.check (key))
    return parent ()->name_sequence (key);
  else
    return impl->alist.name_sequence (key);
}

const std::vector<bool>& 
Frame::flag_sequence (const symbol key) const
{ 
  if (parent () && !impl->alist.check (key))
    return parent ()->flag_sequence (key);
  else
    return impl->alist.flag_sequence (key);
}

const std::vector<int>& 
Frame::integer_sequence (const symbol key) const
{ 
  if (parent () && !impl->alist.check (key))
    return parent ()->integer_sequence (key);
  else
    return impl->alist.integer_sequence (key);
}

const std::vector<const Frame*>& 
Frame::frame_sequence (const symbol key) const
{ 
  if (parent () && !impl->alist.check (key))
    return parent ()->frame_sequence (key);
  else
    return impl->alist.frame_sequence (key);
}

const std::vector<const PLF*>& 
Frame::plf_sequence (const symbol key) const
{ 
  if (parent () && !impl->alist.check (key))
    return parent ()->plf_sequence (key);
  else
    return impl->alist.plf_sequence (key);
}

void 
Frame::verify (const symbol key, const Value::type want, 
               const int value_size)
{ 
  Value::type has = lookup (key);
  if (has != want)
    daisy_panic ("'" + key + "' is " + Value::type_name (has) 
                 + ", should be " + Value::type_name (want));
  int type_size = this->type_size (key);
  if (type_size == Value::Sequence)
    {
      if (value_size == Value::Singleton)
        daisy_panic ("'" + key + "' is a singleton, should be a sequence");
    }
  else if (type_size != value_size)
    daisy_panic ("Size of value does not match type for '" + key + "'");
}

void 
Frame::add (const symbol key, double value)
{ 
  verify (key, Value::Number);
  impl->alist.add (key, value); 
}

void 
Frame::add (const symbol key, double value, const symbol dim)
{
  verify (key, Value::Number);
  impl->alist.add (key, value, dim); 
}

void 
Frame::add (const symbol key, const symbol name)
{
  if (lookup (key) == Value::Object)
    {
      verify (key, Value::Object);
      const symbol component = impl->syntax.component (key);
      const Intrinsics& intrinsics = Librarian::intrinsics ();
      intrinsics.instantiate (component, name);
      const FrameModel& old = intrinsics.library (component).model (name);
      FrameModel child (old, parent_copy);
      child.alist ().add ("type", name);
      impl->alist.add (key, child);
      return;
    }
  verify (key, Value::String);
  impl->alist.add (key, name); 
}

void 
Frame::add (const symbol key, const char *const name)
{ add (key, symbol (name)); }

void 
Frame::add (const symbol key, bool value)
{
  verify (key, Value::Boolean);
  impl->alist.add (key, value); 
}

void 
Frame::add (const symbol key, int value)
{
  verify (key, Value::Integer);
  impl->alist.add (key, value); 
}

void 
Frame::add (const symbol key, const Frame& value)
{
  if (lookup (key) == Value::Object)
    verify (key, Value::Object);
  else
    verify (key, Value::AList);
    
  impl->alist.add (key, value);
}

void 
Frame::add (const symbol key, const PLF& value)
{
  verify (key, Value::PLF);
  impl->alist.add (key, value); 
}

void 
Frame::add (const symbol key, const std::vector<double>& value)
{
  verify (key, Value::Number, value.size ());
  impl->alist.add (key, value); 
}

void 
Frame::add (const symbol key, const std::vector<symbol>& value)
{
  if (lookup (key) == Value::Object)
    {
      verify (key, Value::Object, value.size ());
      const symbol component = impl->syntax.component (key);
      const Intrinsics& intrinsics = Librarian::intrinsics ();
      auto_vector<const Frame*> frames;
      for (size_t i = 0; i < value.size (); i++)
        {
          const symbol name = value[i];
          intrinsics.instantiate (component, name);
          Frame& frame = intrinsics.library (component).model (name).clone ();
          frame.alist ().add ("type", name);
          frames.push_back (&frame);
        }
      impl->alist.add (key, frames);
      return;
    }
  verify (key, Value::String, value.size ());
  impl->alist.add (key, value); 
}

void 
Frame::add_strings (const symbol key)
{
  std::vector<symbol> all;
  add (key, all);
}

void 
Frame::add_strings (const symbol key, const symbol a)
{
  std::vector<symbol> all;
  all.push_back (a);
  add (key, all);
}

void 
Frame::add_strings (const symbol key,
                    const symbol a, const symbol b)
{
  std::vector<symbol> all;
  all.push_back (a);
  all.push_back (b);
  add (key, all);
}

void 
Frame::add_strings (const symbol key,
                    const symbol a, const symbol b, const symbol c)
{
  std::vector<symbol> all;
  all.push_back (a);
  all.push_back (b);
  all.push_back (c);
  add (key, all);
}

void 
Frame::add (const symbol key, const std::vector<bool>& value)
{
  verify (key, Value::Boolean, value.size ());
  impl->alist.add (key, value); 
}

void 
Frame::add (const symbol key, const std::vector<int>& value)
{
  verify (key, Value::Integer, value.size ());
  impl->alist.add (key, value); 
}

void 
Frame::add (const symbol key, const std::vector<const Frame*>& value)
{
  if (lookup (key) == Value::AList)
    verify (key, Value::AList, value.size ());
  else
    verify (key, Value::Object, value.size ());
  impl->alist.add (key, value); 
}

void 
Frame::add (const symbol key, const std::vector<const PLF*>& value)
{
  verify (key, Value::PLF, value.size ());
  impl->alist.add (key, value); 
}

void 
Frame::add_empty (const symbol key)
{
  switch (lookup (key))
    {
    case Value::Number:
      impl->alist.add (key, std::vector<double> ());
      break;
    case Value::AList:
    case Value::Object:
      impl->alist.add (key, std::vector<const Frame*> ());
      break;
    case Value::PLF:
      impl->alist.add (key, std::vector<const PLF*> ());
      break;
    case Value::Boolean:
      impl->alist.add (key, std::vector<bool> ());
      break;
    case Value::String:
      impl->alist.add (key, std::vector<symbol> ());
      break;
    case Value::Integer:
      impl->alist.add (key, std::vector<int> ());
      break;
    case Value::Library:
    case Value::Error:
    default:
      daisy_notreached ();
    }
}

Frame::Frame (const Frame& old)
  : impl (new Implementation (*old.impl))
{ }

Frame::Frame ()
  : impl (new Implementation ())
{ }

Frame::Frame (const Frame *const old)
  : impl (old ? new Implementation (*old->impl) : new Implementation ())
{ }

void
Frame::overwrite_values (const Frame& other)
{ 
  if (this == &other)
    return;

  impl->alist = other.impl->alist;
}

void 
Frame::reset ()
{ impl.reset (new Implementation (impl->count, impl->children)); }

Frame::~Frame ()
{ 
#if 0
  const size_t size = impl->children.size ();
  std::ostringstream tmp;
  if (size != 0)
    {
      describe_frame (*this, tmp);
      for (Implementation::child_set::const_iterator i
             = impl->children.begin ();
           i != impl->children.end ();
           i++)
        {
          tmp << "\n" << "child" << ": ";
          describe_frame (**i, tmp);
        }
      daisy_warning (tmp.str ());
    }
#endif
}

// frame.C ends here.
