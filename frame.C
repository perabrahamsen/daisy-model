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
#include "block.h"
#include "assertion.h"
#include "librarian.h"
#include "intrinsics.h"
#include "library.h"
#include "memutils.h"
#include "alist.h"
#include "filepos.h"
#include "metalib.h"
#include "type.h"
#include "val.h"
#include "check.h"
#include "vcheck.h"
#include "treelog.h"
#include "plf.h"
#include <vector>
#include <set>
#include <sstream>
#include <boost/shared_ptr.hpp>

struct Frame::Implementation
{
  // Hierarchy.
  static int counter;
  int count;
  typedef std::set<const Frame*> child_set;
  mutable child_set children;

  // Syntax.
  typedef std::map<symbol, boost::shared_ptr<const Type>/**/> type_map;
  type_map types;
  typedef std::map<symbol, const VCheck*> vcheck_map;
  vcheck_map val_checks;
  std::vector<check_fun> checker;
  std::vector<symbol> order;

  // Value.
  typedef std::map<symbol, boost::shared_ptr<const Val>/**/> val_map;
  val_map values;
  AttributeList alist;

  void declare_type (const symbol key, const Type* type);
  const Type& get_type (symbol key) const;
  bool has_type (symbol key) const;
  const Type& find_type (const Frame&, symbol key) const;
  void entries (std::set<symbol>&) const;
  bool check (const Metalib& metalib, const Frame& frame,
              const Type&, const symbol key, Treelog& msg) const;
  bool check (const Metalib& metalib, const Frame& frame, Treelog& msg) const;

  Implementation (const Implementation& old)
    : count (counter),
      types (old.types),
      val_checks (old.val_checks),
      checker (old.checker),
      order (old.order),
      values (old.values),
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

void
Frame::Implementation::declare_type (const symbol key, const Type* type)
{
  types[key].reset (type);
}

const Type& 
Frame::Implementation::get_type (const symbol key) const
{
  type_map::const_iterator i = types.find (key);
  daisy_assert (i != types.end ());
  return *(*i).second;
}

bool 
Frame::Implementation::has_type (const symbol key) const
{
  type_map::const_iterator i = types.find (key);
  return i != types.end ();
}

const Type& 
Frame::Implementation::find_type (const Frame& frame, const symbol key) const
{
  if (has_type (key))
    return get_type (key);
  daisy_assert (frame.parent ());
  const Frame& parent = *frame.parent ();
  return parent.impl->find_type (parent, key);
}

void 
Frame::Implementation::entries (std::set<symbol>& all) const
{
  for (type_map::const_iterator i = types.begin ();
       i != types.end ();
       i++)
    all.insert ((*i).first);
}


bool 
Frame::Implementation::check (const Metalib& metalib, const Frame& frame,
                              const Type& type,
                              const symbol key, Treelog& msg) const
{ 
  // Has value?
  if (!frame.check (key))
    {
      if (type.is_mandatory ())
        {
          msg.error (key + " is missing");
          return false;
        }
      return true;
    }

  // Now, the real checks.
  bool ok = true;

  // Genenic check.
  vcheck_map::const_iterator i = val_checks.find (key);
  if (i != val_checks.end ())
    {
      if (!(*i).second->verify (metalib, frame, key, msg))
        ok = false;;
    }
      
  // Spcial handling of various types.
  switch (type.type ())
    {
    case Value::Number:
      // This should already be checked by the file parser, but you
      // never know... Well, theoretically the alist could come from
      // another source (like one of the many other parsers :/), or
      // be one of the build in ones.
      if (type.size () != Value::Singleton)
        {
          const std::vector<double>& array = frame.number_sequence (key);
          for (size_t i = 0; i < array.size (); i++)
            {
              std::ostringstream tmp;
              tmp << key << "[" << i << "]";
              Treelog::Open nest (msg, tmp.str ());
              if (!type.verify (array[i], msg))
                ok = false;
            }
        }
      else 
        {
          Treelog::Open nest (msg, key);
          if (!type.verify (frame.number (key), msg))
            ok = false;
        }
      break;

    case Value::Object:
      if (type.size () != Value::Singleton)
        {
          const ::Library& lib = metalib.library (type.component ());
          const std::vector<boost::shared_ptr<const FrameModel>/**/>& seq 
            = frame.model_sequence (key);
          int j_index = 0;
          for (std::vector<boost::shared_ptr<const FrameModel>/**/>::const_iterator j
                 = seq.begin ();
               j != seq.end ();
               j++)
            {
              std::ostringstream tmp;
              tmp << key << "[" << j_index << "]: ";
              j_index++;
              const FrameModel& al = **j;
              if (!lib.check (al.type_name ()))
                {
                  tmp << "Unknown library member '"
                      << al.type_name () << "'";
                  msg.error (tmp.str ());
                  ok = false;
                }
              else 
                {
                  tmp << al.type_name ();
                  Treelog::Open nest (msg, tmp.str ());
                  if (!al.check (metalib, msg))
                    ok = false;
                }
            }
        }
      else 
        {
          const FrameModel& al = frame.model (key);
          Treelog::Open nest (msg, key + ": " + al.type_name ());
          if (!al.check (metalib, msg))
            ok = false;
        }
      break;

    case Value::AList:
      if (type.size () != Value::Singleton)
        {
          daisy_assert (frame.type_size (key) != Value::Singleton);
          const std::vector<boost::shared_ptr<const FrameSubmodel>/**/>& seq 
            = frame.submodel_sequence (key);
          int j_index = 0;
          for (std::vector<boost::shared_ptr<const FrameSubmodel>/**/>::const_iterator j
                 = seq.begin ();
               j != seq.end ();
               j++)
            {
              std::ostringstream tmp;
              tmp << key << " [" << j_index << "]";
              Treelog::Open nest (msg, tmp.str ());
              j_index++;
              const FrameSubmodel& al = **j;
              if (!al.check (metalib, msg))
                ok = false;
            }
        }
      else 
        {
          Treelog::Open nest (msg, key);
          const FrameSubmodel& al = frame.submodel (key);
          if (!al.check (metalib, msg))
            ok = false;
        }
      break;
    default:
      /* Do nothing */;
    }
  return ok;
}

bool 
Frame::Implementation::check (const Metalib& metalib, const Frame& frame,
                              Treelog& msg) const
{
  bool ok = true;

  for (type_map::const_iterator i = types.begin ();
       i != types.end ();
       i++)
    {
      const symbol key = (*i).first;
      const Type& type = *(*i).second;
      if (frame.is_reference (key))
        continue;
      else if (!check (metalib, frame, type, key, msg))
        ok = false;
    }

  if (!ok)
    return false;

  for (size_t j = 0; j < checker.size (); j++)
    if (!checker[j] (metalib, frame, msg))
      return false;

  return true;
}

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
  impl->entries (e); 
}

bool 
Frame::check (Block& block) const
{ return check (block.metalib (), *this, block.msg ()); }

bool 
Frame::check (const Metalib& metalib, Treelog& msg) const
{ return check (metalib, *this, msg); }

bool 
Frame::check (const Metalib& metalib, const Frame& frame, Treelog& msg) const
{ 
  bool ok = true;
  if (!impl->check (metalib, frame, msg))
    ok = false;
  else if (parent () && !parent ()->check (metalib, frame, msg))
    ok = false;
  return ok;
}

bool
Frame::verify (const symbol key, const double value, Treelog& msg) const
{ 
  if (impl->has_type (key))
    return impl->get_type (key).verify (value, msg);
  if (parent () )
    return parent ()->verify (key, value, msg);
  
  daisy_panic ("'" + key + "' not found in " + type_name ());
}

bool 
Frame::check (const Metalib& metalib, 
              const symbol key, Treelog& msg) const
{ return check (metalib, *this, key, msg); }

bool 
Frame::check (const Metalib& metalib, const Frame& frame,
              const symbol key, Treelog& msg) const
{ 
  if (lookup (key) == Value::Error)
    {
      msg.error ("'" + key + "': not defined");
      return false;
    }

  const Type& type = impl->find_type (frame, key);
  bool ok = true;

  if (!impl->check (metalib, frame, type, key, msg))
    ok = false;
  else if (parent ()
      && parent ()->lookup (key) != Value::Error
      && !parent ()->check (metalib, frame, key, msg))
    ok = false;

  return ok;
}

bool 
Frame::is_const (const symbol key) const
{
  if (impl->has_type (key))
    return impl->get_type (key).is_const ();
  if (parent () )
    return parent ()->is_const (key);
  
  daisy_panic ("'" + key + "' not found in " + type_name ());
}

bool 
Frame::is_optional (const symbol key) const
{
  if (impl->has_type (key))
    return impl->get_type (key).is_optional ();
  if (parent () )
    return parent ()->is_optional (key);
 
  daisy_panic ("'" + key + "' not found in " + type_name ());
}

bool 
Frame::is_log (const symbol key) const
{
  if (impl->has_type (key))
    return impl->get_type (key).is_log ();
  if (parent () )
    return parent ()->is_log (key);
  
  daisy_panic ("'" + key + "' not found in " + type_name ());
}

bool 
Frame::is_state (const symbol key) const
{
  if (impl->has_type (key))
    return impl->get_type (key).is_state ();
  if (parent () )
    return parent ()->is_state (key);
  
  daisy_panic ("'" + key + "' not found in " + type_name ());
}

Value::type 
Frame::lookup (const symbol key) const
{
  if (impl->has_type (key))
    return impl->get_type (key).type ();
  if (parent () )
    return parent ()->lookup (key);

  return Value::Error;
}

symbol
Frame::component (const symbol key) const
{
  if (impl->has_type (key))
    return impl->get_type (key).component ();
  if (parent () )
    return parent ()->component (key);

  daisy_panic ("'" + key + "' not found in " + type_name ());
}

int  
Frame::type_size (const symbol key) const
{
  if (impl->has_type (key))
    return impl->get_type (key).size ();
  if (parent () )
    return parent ()->type_size (key);

  daisy_panic ("'" + key + "' not found in " + type_name ());
}

symbol 
Frame::dimension (const symbol key) const
{
  if (impl->has_type (key))
    return impl->get_type (key).dimension ();
  if (parent () )
    return parent ()->dimension (key);

  daisy_panic ("'" + key + "' not found in " + type_name ());
}

symbol 
Frame::domain (const symbol key) const
{
  if (impl->has_type (key))
    return impl->get_type (key).domain ();
  if (parent () )
    return parent ()->domain (key);

  daisy_panic ("'" + key + "' not found in " + type_name ());
}

symbol 
Frame::range (const symbol key) const
{
  if (impl->has_type (key))
    return impl->get_type (key).range ();
  if (parent () )
    return parent ()->range (key);

  daisy_panic ("'" + key + "' not found in " + type_name ());
}

symbol 
Frame::description (const symbol key) const
{
  if (impl->has_type (key))
    return impl->get_type (key).description ();
  if (parent () )
    return parent ()->description (key);

  daisy_panic ("'" + key + "' not found in " + type_name ());
}

const FrameSubmodel& 
Frame::default_frame (const symbol key) const
{
  if (impl->has_type (key))
    {
      const load_syntax_t load_syntax = impl->get_type (key).load_syntax ();
      return Librarian::submodel_frame (load_syntax);
    }
  if (parent () )
    return parent ()->default_frame (key);

  daisy_panic ("'" + key + "' not found in " + type_name ());
}

symbol 
Frame::submodel_name (const symbol key) const
{ 
  if (impl->has_type (key))
    {
      const load_syntax_t load_syntax = impl->get_type (key).load_syntax ();
      return Librarian::submodel_name (load_syntax);
    }
  if (parent () )
    return parent ()->submodel_name (key);

  daisy_panic ("'" + key + "' not found in " + type_name ());
}

void 
Frame::declare_boolean (const symbol key,	// Boolean.
                        Value::category cat,
                        int size,
                        const symbol description)
{ 
  impl->declare_type (key, new TypeBoolean (cat, size, description));
}

void 
Frame::declare_integer (const symbol key,	// Integer.
                        Value::category cat,
                        int size,
                        const symbol description)
{
  impl->declare_type (key, new TypeInteger (cat, size, description));
}

void 
Frame::declare_string (const symbol key,	// String.
                       Value::category cat,
                       int size,
                       const symbol description)
{
  impl->declare_type (key, new TypeString (cat, size, description));
}

void 
Frame::declare (const symbol key, // Number.
                const symbol dim,
                Value::category cat,
                int size,
                const symbol description)
{
  impl->declare_type (key, new TypeNumber (cat, size, dim, Check::none (),
                                           description));
}


void 
Frame::declare (const symbol key,
                const symbol dim,
                const Check& check,
                Value::category cat,
                int size,
                const symbol description)
{ 
  impl->declare_type (key, new TypeNumber (cat, size, dim, check, description));
}


void 
Frame::declare_fraction (const symbol key, 
                         Value::category cat,
                         int size,
                         const symbol description)
{
  impl->declare_type (key, new TypeNumber (cat, size, Value::Fraction (), 
                                           Check::fraction (), description));
}


void 
Frame::declare_fraction (const symbol key, 
                         Value::category cat,
                         const symbol description)
{
  impl->declare_type (key, new TypeNumber (cat, Value::Singleton,
                                           Value::Fraction (), 
                                           Check::fraction (), description));
}


void 
Frame::declare (const symbol key, // PLF.
                const symbol domain,
                const symbol range,
                Value::category cat,
                int size,
                const symbol description)
{
  impl->declare_type (key, new TypePLF (cat, size, domain, range, 
                                        Check::none (), description));
}

void 
Frame::declare (const symbol key,
                const symbol domain,
                const symbol range,
                const Check& check,
                Value::category cat,
                int size,
                const symbol description)
{
  impl->declare_type (key, new TypePLF (cat, size, domain, range, 
                                        check, description));
}

void 
Frame::declare_object (const symbol key, const symbol lib,
                       Value::category cat, int size, const symbol description)
{
  impl->declare_type (key, new TypeObject (cat, size, lib, description));
}

void 
Frame::declare_submodule (const symbol key, 
                          Value::category cat, const symbol description,
                          load_syntax_t load_syntax)
{
  impl->declare_type (key, new TypeAList (cat, Value::Singleton, 
                                          load_syntax, description));
#if 1
  if (cat == Value::Const || cat == Value::State)
    {
      // TODO: Move this to Frame::alist (name) (must return const first).
      boost::shared_ptr<const FrameSubmodel> child 
        (&default_frame (key).clone ());
      impl->alist.set (key, *child);
      impl->values[key].reset (new ValAList (child));
    }
#endif
}

void 
Frame::declare_submodule_sequence (const symbol key, Value::category cat, 
                                   const symbol description,
                                   load_syntax_t load_syntax)
{
  impl->declare_type (key, new TypeAList (cat, Value::Variable, 
                                          load_syntax, description));
}

void 
Frame::set_check (const symbol key, const VCheck& vcheck)
{
  impl->val_checks[key] = &vcheck;
}

void 
Frame::order (const std::vector<symbol>& v)
{ impl->order = v; }

void 
Frame::order (const symbol one)
{
  daisy_assert (impl->order.size () == 0);
  impl->order.push_back (one);
}

void 
Frame::order (const symbol one, const symbol two)
{
  daisy_assert (impl->order.size () == 0);
  impl->order.push_back (one);
  impl->order.push_back (two);
}

void 
Frame::order (const symbol one, const symbol two, const symbol three)
{
  daisy_assert (impl->order.size () == 0);
  impl->order.push_back (one);
  impl->order.push_back (two);
  impl->order.push_back (three);
}

void 
Frame::order (const symbol one, const symbol two, const symbol three,
              const symbol four)
{
  daisy_assert (impl->order.size () == 0);
  impl->order.push_back (one);
  impl->order.push_back (two);
  impl->order.push_back (three);
  impl->order.push_back (four);
}

void 
Frame::order (const symbol one, const symbol two, const symbol three,
              const symbol four, const symbol five)
{
  daisy_assert (impl->order.size () == 0);
  impl->order.push_back (one);
  impl->order.push_back (two);
  impl->order.push_back (three);
  impl->order.push_back (four);
  impl->order.push_back (five);
}

bool 
Frame::ordered () const
{ 
  if (impl->order.size () > 0)
    return true;
  else if (parent ())
    return parent ()->ordered ();
  else
    return false;
}

const std::vector<symbol>& 
Frame::order () const
{ 
  if (impl->order.size () > 0)
    return impl->order;
  else if (parent ())
    return parent ()->order ();
  
  return impl->order;
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
  std::set<symbol> all;
  entries (all);
  int non_logs = all.size ();
  for (std::set<symbol>::const_iterator i = all.begin ();
       i != all.end ();
       i++)
    {
      const symbol key = *i;
      if (is_log (key))
        non_logs--;
    }
  daisy_assert (non_logs >= 0);
  return non_logs == order ().size ();
}

void 
Frame::add_check (check_fun fun)
{ 
  impl->checker.push_back (fun);
}

bool 
Frame::check (const symbol key) const
{ 
  return impl->alist.check (key)
    || (parent () && parent ()->check (key));
}

// Is this frame a subset of 'other'?
bool 
Frame::subset (const Metalib& metalib, const Frame& other) const
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
Frame::subset (const Metalib& metalib, const Frame& other,
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
            const std::vector<boost::shared_ptr<const FrameModel>/**/>& mine 
              = me->model_sequence (key);
            const std::vector<boost::shared_ptr<const FrameModel>/**/>& his 
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
{ 
  impl->alist.set_reference (key, val); 
  impl->values[key].reset (new ValReference (val));
}

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

const std::vector<boost::shared_ptr<const FrameModel>/**/>& 
Frame::model_sequence (const symbol key) const
{ 
  if (parent () && !impl->alist.check (key))
    return parent ()->model_sequence (key);
  else
    return impl->alist.model_sequence (key);
}

const std::vector<boost::shared_ptr<const FrameSubmodel>/**/>& 
Frame::submodel_sequence (const symbol key) const
{ 
  if (parent () && !impl->alist.check (key))
    return parent ()->submodel_sequence (key);
  else
    return impl->alist.submodel_sequence (key);
}

const std::vector<boost::shared_ptr<const PLF>/**/>& 
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
  if (Value::flexible_size (type_size))
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
  impl->values[key].reset (new ValNumber (value));
}

void 
Frame::set (const symbol key, double value, const symbol dim)
{
  verify (key, Value::Number);
  impl->alist.set (key, value, dim); 
  impl->values[key].reset (new ValScalar (value, dim));
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
      boost::shared_ptr<const FrameModel> child (&old.clone ());
      impl->alist.set (key, *child);
      impl->values[key].reset (new ValObject (child));
      return;
    }
  verify (key, Value::String);
  impl->alist.set (key, name); 
  impl->values[key].reset (new ValString (name));
}

void 
Frame::set (const symbol key, const char *const name)
{
  set (key, symbol (name)); 
  impl->values[key].reset (new ValString (name));
}

void 
Frame::set (const symbol key, bool value)
{
  verify (key, Value::Boolean);
  impl->alist.set (key, value); 
  impl->values[key].reset (new ValBoolean (value));
}

void 
Frame::set (const symbol key, int value)
{
  verify (key, Value::Integer);
  impl->alist.set (key, value); 
  impl->values[key].reset (new ValInteger (value));
}

void 
Frame::set (const symbol key, const FrameModel& value)
{
  verify (key, Value::Object);
  impl->alist.set (key, value);
  boost::shared_ptr<const FrameModel> child (&value.clone ());
  impl->values[key].reset (new ValObject (child));
}

void 
Frame::set (const symbol key, const FrameSubmodel& value)
{
  verify (key, Value::AList);
  boost::shared_ptr<const FrameSubmodel> child (&value.clone ());
  impl->alist.set (key, *child);
  impl->values[key].reset (new ValAList (child));
}

void 
Frame::set (const symbol key, const PLF& value)
{
  verify (key, Value::PLF);
  boost::shared_ptr<const PLF> child (new PLF (value));
  impl->alist.set (key, *child); 
  impl->values[key].reset (new ValPLF (child));
}

void 
Frame::set (const symbol key, const std::vector<double>& value)
{
  verify (key, Value::Number, value.size ());
  impl->alist.set (key, value); 
  impl->values[key].reset (new ValNumberSeq (value));
}

void 
Frame::set (const symbol key, const std::vector<symbol>& value)
{
  if (lookup (key) == Value::Object)
    {
      verify (key, Value::Object, value.size ());
      const symbol component = this->component (key);
      const Intrinsics& intrinsics = Librarian::intrinsics ();
      std::vector<boost::shared_ptr<const FrameModel>/**/> frames;
      for (size_t i = 0; i < value.size (); i++)
        {
          const symbol name = value[i];
          intrinsics.instantiate (component, name);
          const FrameModel& old = intrinsics.library (component).model (name);
          boost::shared_ptr<const FrameModel> 
            link (new FrameModel (old, parent_link));
          frames.push_back (link);
        }
      impl->alist.set (key, frames);
      impl->values[key].reset (new ValObjectSeq (frames));
      return;
    }
  verify (key, Value::String, value.size ());
  impl->alist.set (key, value); 
  impl->values[key].reset (new ValStringSeq (value));
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
  impl->values[key].reset (new ValBooleanSeq (value));
}

void 
Frame::set (const symbol key, const std::vector<int>& value)
{
  verify (key, Value::Integer, value.size ());
  impl->alist.set (key, value); 
  impl->values[key].reset (new ValIntegerSeq (value));
}

void 
Frame::set (const symbol key, const std::vector<boost::shared_ptr<const FrameModel>/**/>& value)
{
  verify (key, Value::Object, value.size ());
  impl->alist.set (key, value); 
  impl->values[key].reset (new ValObjectSeq (value));
}

void 
Frame::set (const symbol key, const std::vector<boost::shared_ptr<const FrameSubmodel>/**/>& value)
{
  verify (key, Value::AList, value.size ());
  impl->alist.set (key, value); 
  impl->values[key].reset (new ValAListSeq (value));
}

void 
Frame::set (const symbol key, const std::vector<boost::shared_ptr<const PLF>/**/>& value)
{
  verify (key, Value::PLF, value.size ());
  impl->alist.set (key, value); 
  impl->values[key].reset (new ValPLFSeq (value));
}

void 
Frame::set_empty (const symbol key)
{
  switch (lookup (key))
    {
    case Value::Number:
      impl->alist.set (key, std::vector<double> ());
      impl->values[key].reset (new ValNumberSeq (std::vector<double> ()));
      break;
    case Value::Object:
      impl->alist.set (key, std::vector<boost::shared_ptr<const FrameModel>/**/> ());
      impl->values[key].reset (new ValObjectSeq (std::vector<boost::shared_ptr<const FrameModel>/**/> ()));
      break;
    case Value::AList:
      impl->alist.set (key, std::vector<boost::shared_ptr<const FrameSubmodel>/**/> ());
      impl->values[key].reset (new ValAListSeq (std::vector<boost::shared_ptr<const FrameSubmodel>/**/> ()));
      break;
    case Value::PLF:
      impl->alist.set (key, std::vector<boost::shared_ptr<const PLF>/**/> ());
      impl->values[key].reset (new ValPLFSeq (std::vector<boost::shared_ptr<const PLF>/**/> ()));
      break;
    case Value::Boolean:
      impl->alist.set (key, std::vector<bool> ());
      impl->values[key].reset (new ValBooleanSeq (std::vector<bool> ()));
      break;
    case Value::String:
      impl->alist.set (key, std::vector<symbol> ());
      impl->values[key].reset (new ValStringSeq (std::vector<symbol> ()));
      break;
    case Value::Integer:
      impl->alist.set (key, std::vector<int> ());
      impl->values[key].reset (new ValIntegerSeq (std::vector<int> ()));
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

void
Frame::overwrite_values (const Frame& other)
{ 
  if (this == &other)
    return;

  impl->alist = other.impl->alist;
  impl->values = other.impl->values;
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
