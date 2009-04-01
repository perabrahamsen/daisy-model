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
#include "frame_submodel.h"
#include "syntax.h"
#include "block.h"
#include "assertion.h"
#include "librarian.h"
#include "intrinsics.h"
#include "library.h"
#include "memutils.h"
#include "alist.h"
#include "filepos.h"
#include "metalib.h"
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

symbol 
Frame::type_name () const
{
  if (parent ())
    return parent ()->type_name ();

  return Value::None ();
}

symbol 
Frame::base_name () const
{
  const symbol my_name = this->type_name ();

  for (const Frame* f = this->parent (); f != NULL; f = f->parent ())
    {
      const symbol base_name = f->type_name ();
      if (base_name != my_name)
        return base_name;
    }
  return Value::None ();
}

symbol 
Frame::description () const
{
  static const symbol desc ("description");

  if (check (desc) && lookup (desc) == Value::String 
      && type_size (desc) == Value::Singleton)
    return name (desc);

  if (parent ())
    return parent ()->description ();

  return Value::None ();
}

const Filepos& 
Frame::own_position () const
{ 
  return Filepos::none (); 
}

const Filepos& 
Frame::inherited_position () const
{ 
  if (own_position () != Filepos::none ())
    return own_position ();

  if (parent ())
    return parent ()->inherited_position ();
  
  return Filepos::none (); 
}

void
Frame::reposition (const Filepos& pos)
{ daisy_notreached (); }


int 
Frame::sequence_id () const
{
  if (parent ())
    return parent ()->sequence_id ();
  
  return -1;
}

bool 
Frame::used_to_be_a_submodel () const
{ 
  if (parent ())
    return parent ()->used_to_be_a_submodel ();

  return false;
}

const Frame* 
Frame::parent () const
{ return NULL; }

void 
Frame::register_child (const Frame* child) const
{ 
  daisy_assert (child != this);
  daisy_assert (impl->children.find (child) == impl->children.end ());
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

void 
Frame::entries (std::set<symbol>& e) const
{ 
  if (parent ())
    parent ()->entries (e);
  impl->syntax.entries (e); 
}

bool 
Frame::check (Block& block) const
{ return check (block.metalib (), *this, block.msg ()); }

bool 
Frame::check (Metalib& metalib, Treelog& msg) const
{ return check (metalib, *this, msg); }

bool 
Frame::check (Metalib& metalib, const Frame& frame, Treelog& msg) const
{ 
  bool ok = true;
  if (!impl->syntax.check (metalib, frame, msg))
    ok = false;
  if (parent () && !parent ()->check (metalib, frame, msg))
    ok = false;
  return ok;
}

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
{ return check (metalib, *this, key, msg); }

bool 
Frame::check (Metalib& metalib, const Frame& frame,
              const symbol key, Treelog& msg) const
{ 
  if (parent () && impl->syntax.lookup (key) == Value::Error)
    return parent ()->check (metalib, frame, key, msg);
  else
    return impl->syntax.check (metalib, frame, key, msg); 
}

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

symbol
Frame::component (const symbol key) const
{
  if (parent () && impl->syntax.lookup (key) == Value::Error)
    return parent ()->component (key);
  else
    return impl->syntax.component (key);
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

const FrameSubmodel& 
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
Frame::declare (const symbol key,	// Generic.
                Value::type t, 
                Value::category cat,
                int size,
                const symbol description)
{ impl->syntax.declare (key, t, cat, size, description); }


void 
Frame::declare (const symbol key, // Number.
	    const symbol dim,
	    Value::category cat,
	    int size,
	    const symbol description)
{ impl->syntax.declare (key, dim, cat, size, description); }


void 
Frame::declare (const symbol key,
	    const symbol dim,
	    const Check& check,
	    Value::category cat,
	    int size,
	    const symbol description)
{ impl->syntax.declare (key, dim, check, cat, size, description); }


void 
Frame::declare_fraction (const symbol key, 
		     Value::category cat,
		     int size,
		     const symbol description)
{ impl->syntax.declare_fraction (key, cat, size, description); }


void 
Frame::declare_fraction (const symbol key, 
		     Value::category cat,
		     const symbol description)
{ impl->syntax.declare_fraction (key, cat, description); }


void 
Frame::declare (const symbol key, // PLF.
	    const symbol domain,
	    const symbol range,
	    Value::category cat,
	    int size,
	    const symbol description)
{ impl->syntax.declare (key, domain, range, cat, size, description); }

void 
Frame::declare (const symbol key,
	    const symbol domain,
	    const symbol range,
	    const Check& check,
	    Value::category cat,
	    int size,
	    const symbol description)
{ impl->syntax.declare (key, domain, range, check, cat, size, description); }

void 
Frame::declare_object (const symbol key, const symbol lib,
                   Value::category cat, int size, const symbol description)
{ impl->syntax.declare_object (key, lib, cat, size, description); }

void 
Frame::declare_submodule (const symbol name, 
		      Value::category cat, const symbol description,
		      load_syntax_t load_syntax)
{
  impl->syntax.declare (name, load_syntax, cat, Value::Singleton, description);
  if (cat == Value::Const || cat == Value::State)
    // TODO: Move this to Frame::alist (name) (must return const first).
    impl->alist.set (name, impl->syntax.default_frame (name));
}

void 
Frame::declare_submodule_sequence (const symbol name, Value::category cat, 
			       const symbol description,
			       load_syntax_t load_syntax)
{
  impl->syntax.declare (name, load_syntax, cat, Value::Sequence, description);
}

void 
Frame::set_check (const symbol name, const VCheck& vcheck)
{ impl->syntax.set_check (name, vcheck); }


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
{ 
  if (impl->syntax.ordered ())
    return true;
  else if (parent ())
    return parent ()->ordered ();
  else
    return false;
}

const std::vector<symbol>& 
Frame::order () const
{ 
  if (impl->syntax.ordered ())
    return impl->syntax.order ();
  else if (parent ())
    return parent ()->order ();
  
  return impl->syntax.order ();
}

int 
Frame::order_index (const symbol key) const
{
  const std::vector<symbol>& order = this->order ();
  for (int i = 0; i < order.size (); i++)
    if (order[i] == key)
      return i;
  return -1;
}

bool 
Frame::total_order () const
{ 
  if (!impl->syntax.total_order ())
    return false;
  else if (!parent ())
    return true;
  else
    return parent ()->total_order ();
}

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

  // Can only compare same type of attribute.
  const Value::type my_type = me->lookup (key);
  const Value::type his_type = him->lookup (key);
  daisy_assert (my_type == his_type);
  
  switch (my_type)
    {
    case Value::AList:
      // TODO: We should test that the submodel is the same.
      break;
    case Value::Object:
      {
        // Can only compare objects from the same library.
        const symbol my_component = me->component (key);
        const symbol his_component = him->component (key);
        daisy_assert (my_component == his_component);
        const Library& library = metalib.library (my_component);
        if (type_size (key) == Value::Singleton)
          {
            const symbol my_name = me->model (key).type_name ();
            const symbol his_name = him->model (key).type_name ();
            if (!library.is_derived_from (my_name, his_name))
              // Subsets must be derived from supersets.
              return false;
          }
        else
          {
            const int my_size = me->value_size (key);
            const int his_size = him->value_size (key);
            const int size = std::min (my_size, his_size);
            daisy_assert (size >= 0);
            const std::vector<const FrameModel*>& mine 
              = me->model_sequence (key);
            const std::vector<const FrameModel*>& his 
              = him->model_sequence (key);
            for (size_t i = 0; i < size; i++)
              {
                const symbol my_name = mine[i]->type_name ();
                const symbol his_name = his[i]->type_name ();
                if (!library.is_derived_from (my_name, his_name))
                  // Subsets must be derived from supersets.
                  return false;
              }
          }
      }
      break;
    default:
      /* Do nothing */;
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
Frame::set_reference (const symbol key, const symbol val)
{ impl->alist.set_reference (key, val); }

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

const FrameModel&
Frame::model (const symbol key) const
{
  verify (key, Value::Object);

  if (parent () && !impl->alist.check (key))
    return parent ()->model (key);
  else
    return impl->alist.model (key);
}

const FrameSubmodel&
Frame::submodel (const symbol key) const
{
  if (type_size (key) != Value::Singleton)
    return default_frame (key);
  verify (key, Value::AList);
  if (impl->alist.check (key))
    return impl->alist.submodel (key);
  else if (parent () && parent ()->check (key))
    return parent ()->submodel (key);
  else
    return default_frame (key);
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

const std::vector<const FrameModel*>& 
Frame::model_sequence (const symbol key) const
{ 
  if (parent () && !impl->alist.check (key))
    return parent ()->model_sequence (key);
  else
    return impl->alist.model_sequence (key);
}

const std::vector<const FrameSubmodel*>& 
Frame::submodel_sequence (const symbol key) const
{ 
  if (parent () && !impl->alist.check (key))
    return parent ()->submodel_sequence (key);
  else
    return impl->alist.submodel_sequence (key);
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
               const int value_size) const
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
Frame::set (const symbol key, double value)
{ 
  verify (key, Value::Number);
  impl->alist.set (key, value); 
}

void 
Frame::set (const symbol key, double value, const symbol dim)
{
  verify (key, Value::Number);
  impl->alist.set (key, value, dim); 
}

void 
Frame::set (const symbol key, const symbol name)
{
  if (lookup (key) == Value::Object)
    {
      verify (key, Value::Object);
      const symbol component = this->component (key);
      const Intrinsics& intrinsics = Librarian::intrinsics ();
      intrinsics.instantiate (component, name);
      const FrameModel& old = intrinsics.library (component).model (name);
      FrameModel child (old, parent_link);
      impl->alist.set (key, child);
      return;
    }
  verify (key, Value::String);
  impl->alist.set (key, name); 
}

void 
Frame::set (const symbol key, const char *const name)
{ set (key, symbol (name)); }

void 
Frame::set (const symbol key, bool value)
{
  verify (key, Value::Boolean);
  impl->alist.set (key, value); 
}

void 
Frame::set (const symbol key, int value)
{
  verify (key, Value::Integer);
  impl->alist.set (key, value); 
}

void 
Frame::set (const symbol key, const FrameModel& value)
{
  verify (key, Value::Object);
  impl->alist.set (key, value);
}

void 
Frame::set (const symbol key, const FrameSubmodel& value)
{
  verify (key, Value::AList);
  impl->alist.set (key, value);
}

void 
Frame::set (const symbol key, const PLF& value)
{
  verify (key, Value::PLF);
  impl->alist.set (key, value); 
}

void 
Frame::set (const symbol key, const std::vector<double>& value)
{
  verify (key, Value::Number, value.size ());
  impl->alist.set (key, value); 
}

void 
Frame::set (const symbol key, const std::vector<symbol>& value)
{
  if (lookup (key) == Value::Object)
    {
      verify (key, Value::Object, value.size ());
      const symbol component = this->component (key);
      const Intrinsics& intrinsics = Librarian::intrinsics ();
      auto_vector<const FrameModel*> frames;
      for (size_t i = 0; i < value.size (); i++)
        {
          const symbol name = value[i];
          intrinsics.instantiate (component, name);
          const FrameModel& old = intrinsics.library (component).model (name);
          frames.push_back (new FrameModel (old, parent_link));
        }
      impl->alist.set (key, frames);
      return;
    }
  verify (key, Value::String, value.size ());
  impl->alist.set (key, value); 
}

void 
Frame::set_strings (const symbol key)
{
  std::vector<symbol> all;
  set (key, all);
}

void 
Frame::set_strings (const symbol key, const symbol a)
{
  std::vector<symbol> all;
  all.push_back (a);
  set (key, all);
}

void 
Frame::set_strings (const symbol key,
                    const symbol a, const symbol b)
{
  std::vector<symbol> all;
  all.push_back (a);
  all.push_back (b);
  set (key, all);
}

void 
Frame::set_strings (const symbol key,
                    const symbol a, const symbol b, const symbol c)
{
  std::vector<symbol> all;
  all.push_back (a);
  all.push_back (b);
  all.push_back (c);
  set (key, all);
}

void 
Frame::set (const symbol key, const std::vector<bool>& value)
{
  verify (key, Value::Boolean, value.size ());
  impl->alist.set (key, value); 
}

void 
Frame::set (const symbol key, const std::vector<int>& value)
{
  verify (key, Value::Integer, value.size ());
  impl->alist.set (key, value); 
}

void 
Frame::set (const symbol key, const std::vector<const FrameModel*>& value)
{
  verify (key, Value::Object, value.size ());
  impl->alist.set (key, value); 
}

void 
Frame::set (const symbol key, const std::vector<const FrameSubmodel*>& value)
{
  verify (key, Value::AList, value.size ());
  impl->alist.set (key, value); 
}

void 
Frame::set (const symbol key, const std::vector<const PLF*>& value)
{
  verify (key, Value::PLF, value.size ());
  impl->alist.set (key, value); 
}

void 
Frame::set_empty (const symbol key)
{
  switch (lookup (key))
    {
    case Value::Number:
      impl->alist.set (key, std::vector<double> ());
      break;
    case Value::Object:
      impl->alist.set (key, std::vector<const FrameModel*> ());
      break;
    case Value::AList:
      impl->alist.set (key, std::vector<const FrameSubmodel*> ());
      break;
    case Value::PLF:
      impl->alist.set (key, std::vector<const PLF*> ());
      break;
    case Value::Boolean:
      impl->alist.set (key, std::vector<bool> ());
      break;
    case Value::String:
      impl->alist.set (key, std::vector<symbol> ());
      break;
    case Value::Integer:
      impl->alist.set (key, std::vector<int> ());
      break;
    case Value::Scalar:
    case Value::Reference:
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

#if 0
static void describe_frame (const Frame& frame, std::ostream& out)
{
  out << typeid (frame).name () << " " << frame.impl->count << " " 
      << frame.type_name ();
}
#endif

Frame::~Frame ()
{ 
  const size_t size = impl->children.size ();
  if (size != 0)
    {
#if 0
      std::ostringstream tmp;
      tmp << "Reparenting children of ";
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
#endif
      reparent_children (parent ());
    }
}

// frame.C ends here.
