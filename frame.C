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
#include "block_model.h"
#include "assertion.h"
#include "librarian.h"
#include "intrinsics.h"
#include "library.h"
#include "memutils.h"
#include "filepos.h"
#include "metalib.h"
#include "type.h"
#include "value.h"
#include "check.h"
#include "vcheck.h"
#include "treelog.h"
#include "plf.h"
#include "mathlib.h"
#include "function.h"
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
  void declare_type (const symbol key, const Type* type);
  const Type& get_type (symbol key) const;
  bool has_type (symbol key) const;
  const Type& find_type (const Frame&, symbol key) const;
  void entries (std::set<symbol>&) const;

  // Value.
  typedef std::map<symbol, boost::shared_ptr<const Value>/**/> value_map;
  value_map values;
  void set_value (const symbol key, const Value* value);
  const Value& get_value (symbol key) const;
  bool has_value (symbol key) const;
  const Value& find_value (const Frame&, symbol key) const;
  void set_model (const symbol component,
		  const symbol key, const symbol name);

  // Check.
  bool check (const Metalib& metalib, const Frame& frame,
              const Type&, const symbol key, Treelog& msg) const;
  bool check (const Metalib& metalib, const Frame& frame, Treelog& msg) const;

  Implementation (const Implementation& old)
    : count (counter),
      types (old.types),
      val_checks (old.val_checks),
      checker (old.checker),
      order (old.order),
      values (old.values)
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

void
Frame::Implementation::set_value (const symbol key, const Value* value)
{
  values[key].reset (value);
}

const Value& 
Frame::Implementation::get_value (const symbol key) const
{
  value_map::const_iterator i = values.find (key);
  if (i == values.end ())
    daisy_panic ("'" + key + "': no value");
  return *(*i).second;
}

bool 
Frame::Implementation::has_value (const symbol key) const
{
  value_map::const_iterator i = values.find (key);
  return i != values.end ();
}

const Value& 
Frame::Implementation::find_value (const Frame& frame, const symbol key) const
{
  if (has_value (key))
    return get_value (key);
  daisy_assert (frame.parent ());
  const Frame& parent = *frame.parent ();
  return parent.impl->find_value (parent, key);
}

void
Frame::Implementation::set_model (const symbol component,
				  const symbol key, const symbol name)
{
  const Intrinsics& intrinsics = Librarian::intrinsics ();
  intrinsics.instantiate (component, name);
  const FrameModel& old = intrinsics.library (component).model (name);
  boost::shared_ptr<const FrameModel>
    child (new FrameModel (old, Frame::parent_link));
  set_value (key, new ValueModel (child));
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
    case Attribute::Number:
      // This should already be checked by the file parser, but you
      // never know... Well, theoretically the alist could come from
      // another source (like one of the many other parsers :/), or
      // be one of the build in ones.
      if (type.size () != Attribute::Singleton)
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

    case Attribute::Model:
      if (type.size () != Attribute::Singleton)
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

    case Attribute::Submodel:
      if (type.size () != Attribute::Singleton)
        {
          daisy_assert (frame.type_size (key) != Attribute::Singleton);
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

  const FrameModel* model = dynamic_cast<const FrameModel*> (&frame);
  if (model && !model->buildable ())
    {
      msg.error ("'" + frame.type_name () 
                 + "' is a base model, for internal use only");
      ok = false;
    }

  for (type_map::const_iterator i = types.begin ();
       i != types.end ();
       i++)
    {
      const symbol key = (*i).first;
      const Type& type = *(*i).second;
      if (!frame.is_reference (key)
          && !check (metalib, frame, type, key, msg))
        ok = false;
    }

  if (!ok)
    return false;

  if (!frame.has_references ())
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

  return Attribute::None ();
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
  return Attribute::None ();
}

symbol 
Frame::description () const
{
  static const symbol desc ("description");

  if (check (desc) && lookup (desc) == Attribute::String 
      && type_size (desc) == Attribute::Singleton)
    return name (desc);

  if (parent ())
    return parent ()->description ();

  return Attribute::None ();
}

bool 
Frame::has_references () const
{
  std::set<symbol> all;
  entries (all);
  
  for (std::set<symbol>::const_iterator i = all.begin ();
       i != all.end ();
       i++)
    {
      const symbol key = *i;
      if (is_reference (key))
        return true;;
      if (!check (key))
        continue;
      
      Attribute::type type = lookup (key);
      int size = type_size (key);
      switch (type)
        {
        case Attribute::Submodel:
          if (size == Attribute::Singleton)
            {
              if (submodel (key).has_references ())
                return true;
            }
          else
            {
              const std::vector<boost::shared_ptr<const FrameSubmodel> >& models
                = submodel_sequence (key);
              for (size_t i = 0; i < models.size (); i++)
                if (models[i]->has_references ())
                  return true;
            }
          break;
        case Attribute::Model:
          if (size == Attribute::Singleton)
            {
              if (model (key).has_references ())
                return true;
            }
          else
            {
              const std::vector<boost::shared_ptr<const FrameModel> >& models
                = model_sequence (key);
              for (size_t i = 0; i < models.size (); i++)
                if (models[i]->has_references ())
                  return true;
            }
          break;
        default:
          break;
        }
    }
  return false;
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
  daisy_safe_assert (i != impl->children.end ());
  if (i == impl->children.end ())
    return;
  impl->children.erase (i);
  const Implementation::child_set::const_iterator j 
    = impl->children.find (child);
  daisy_safe_assert (j == impl->children.end ());
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
Frame::check (const Block& block) const
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
  if (lookup (key) == Attribute::Error)
    {
      msg.error ("'" + key + "': not defined");
      return false;
    }

  const Type& type = impl->find_type (frame, key);
  bool ok = true;

  if (!impl->check (metalib, frame, type, key, msg))
    ok = false;
  else if (parent ()
      && parent ()->lookup (key) != Attribute::Error
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
  
  daisy_warning ("'" + key + "' not found in " + type_name ());
  return false;
}

bool 
Frame::is_optional (const symbol key) const
{
  if (impl->has_type (key))
    return impl->get_type (key).is_optional ();
  if (parent () )
    return parent ()->is_optional (key);
 
  daisy_warning ("'" + key + "' not found in " + type_name ());
  return false;
}

bool 
Frame::is_log (const symbol key) const
{
  if (impl->has_type (key))
    return impl->get_type (key).is_log ();
  if (parent () )
    return parent ()->is_log (key);
  
  daisy_warning ("'" + key + "' not found in " + type_name ());
  return false;
}

bool 
Frame::is_state (const symbol key) const
{
  if (impl->has_type (key))
    return impl->get_type (key).is_state ();
  if (parent () )
    return parent ()->is_state (key);
  
  daisy_warning ("'" + key + "' not found in " + type_name ());
  return false;
}

Attribute::type 
Frame::lookup (const symbol key) const
{
  if (impl->has_type (key))
    return impl->get_type (key).type ();
  if (parent () )
    return parent ()->lookup (key);

  return Attribute::Error;
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

bool
Frame::is_text (const symbol key) const
{
  if (impl->has_type (key))
    return impl->get_type (key).is_text ();
  if (parent () )
    return parent ()->is_text (key);

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

const std::vector<symbol>& 
Frame::type_cite (const symbol key) const
{
  if (impl->has_type (key))
    return impl->get_type (key).cite ();
  if (parent () )
    return parent ()->type_cite (key);

  daisy_panic ("'" + key + "' not found in " + type_name ());
}


boost::shared_ptr<const FrameSubmodel>
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
                        Attribute::category cat,
                        int size,
                        const symbol description)
{ 
  impl->declare_type (key, new TypeBoolean (cat, size, description));
}

void 
Frame::declare_integer (const symbol key,	// Integer.
                        Attribute::category cat,
                        int size,
                        const symbol description)
{
  impl->declare_type (key, new TypeInteger (cat, size, description));
}

void 
Frame::declare_string (const symbol key,	// String.
                       Attribute::category cat,
                       int size,
                       const symbol description)
{
  impl->declare_type (key, new TypeString (cat, size, description));
}

void 
Frame::declare_text (const symbol key,	// String (Text area)
                     Attribute::category cat,
                     int size,
                     const symbol description)
{
  impl->declare_type (key, new TypeText (cat, size, description));
}

void 
Frame::declare (const symbol key, // Number.
                const symbol dim,
                Attribute::category cat,
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
                Attribute::category cat,
                int size,
                const symbol description)
{ 
  impl->declare_type (key, new TypeNumber (cat, size, dim, check, description));
}


void 
Frame::declare_fraction (const symbol key, 
                         Attribute::category cat,
                         int size,
                         const symbol description)
{
  impl->declare_type (key, new TypeNumber (cat, size, Attribute::Fraction (), 
                                           Check::fraction (), description));
}


void 
Frame::declare_fraction (const symbol key, 
                         Attribute::category cat,
                         const symbol description)
{
  impl->declare_type (key, new TypeNumber (cat, Attribute::Singleton,
                                           Attribute::Fraction (), 
                                           Check::fraction (), description));
}


void 
Frame::declare_number_cited (symbol key, // Number.
                             symbol dim,
                             const Check& check,
                             Attribute::category cat,
                             int size,
                             symbol description,
                             const std::vector<symbol>& citations)
{
  impl->declare_type (key, new TypeNumberCite (cat, size, dim, 
                                               check, description, citations));
  
}
void 
Frame::declare_number_cited (symbol key, // Number.
                             symbol dim,
                             const Check& check,
                             Attribute::category cat,
                             int size,
                             symbol description,
                             symbol citation)
{
  std::vector<symbol> citations;
  citations.push_back (citation);
  declare_number_cited (key, dim, check, cat, size, description, citations);
}

void 
Frame::declare (const symbol key, // PLF.
                const symbol domain,
                const symbol range,
                Attribute::category cat,
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
                Attribute::category cat,
                int size,
                const symbol description)
{
  impl->declare_type (key, new TypePLF (cat, size, domain, range, 
                                        check, description));
}

void 
Frame::declare_object (const symbol key, const symbol lib,
                       Attribute::category cat, int size, const symbol description)
{
  impl->declare_type (key, new TypeModel (cat, size, lib, description));
}

void 
Frame::declare_function (const symbol key,
			 const symbol domain,
			 const symbol range,
			 const symbol description)
{
  impl->declare_type (key, new TypeFunction (domain, range, description));
}

void 
Frame::declare_submodule (const symbol key, 
                          Attribute::category cat, const symbol description,
                          load_syntax_t load_syntax)
{
  impl->declare_type (key, new TypeSubmodel (cat, Attribute::Singleton, 
                                          load_syntax, description));

  if (cat == Attribute::Const || cat == Attribute::State)
    impl->set_value (key, new ValueSubmodel 
                     (Librarian::submodel_frame (load_syntax)));
}

void 
Frame::declare_submodule_sequence (const symbol key, Attribute::category cat, 
                                   const symbol description,
                                   load_syntax_t load_syntax)
{
  impl->declare_type (key, new TypeSubmodel (cat, Attribute::Variable, 
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
  if (impl->has_value (key))
    return !impl->get_value (key).is_reference ();
  if (parent ())
    return parent ()->check (key);
  return false;
}

// Is this frame a subset of 'other'?
bool 
Frame::subset_elements (const Metalib& metalib, const Frame& other) const
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
  if (!this->check (key))
    // Missing value is always a subset.
    return true;

  if (!other.check (key))
    // Missing value cannot be a superset of a value.
    return false;

  const Attribute::type type = this->lookup (key);
  if (type != other.lookup (key))
    // Subsets must be same type.
    return false;
  
  // Find values.
  const Value& mine = impl->find_value (*this, key);
  const Value& his = other.impl->find_value (other, key);
  
  const int size = mine.size ();
  if (size != his.size ())
    // Subsets must be same size.
    return false;

  if (size == Attribute::Singleton)
    switch (type)
      {
      case Attribute::Number:
	return iszero (mine.number () - his.number ());
      case Attribute::Boolean:
	return mine.flag () == his.flag ();
      case Attribute::Integer:
	return mine.integer () == his.integer ();
      case Attribute::Model:
      case Attribute::Function:
        return mine.model ()->subset (metalib, *his.model ());
      case Attribute::Submodel:
        return mine.submodel ()->subset (metalib, *his.submodel ());
      case Attribute::PLF:
	return *mine.plf () == *his.plf ();
      case Attribute::Reference:
      case Attribute::String:
	return mine.name () == his.name ();
      case Attribute::Scalar:
        return iszero (mine.number () - his.number ())
          && mine.name () == his.name ();
      case Attribute::Error:
      default:
	daisy_notreached ();
      }
  else
    switch (type)
      {
      case Attribute::Number:
        for (size_t i = 0; i < size; i++)
          if (!iszero (mine.number_sequence ()[i] - his.number_sequence ()[i]))
            return false;
        return true;
      case Attribute::Boolean:
	return mine.flag_sequence () == his.flag_sequence ();
      case Attribute::Integer:
	return mine.integer_sequence () == his.integer_sequence ();
      case Attribute::Model:
      case Attribute::Function:
	{
	  const std::vector<boost::shared_ptr<const FrameModel>/**/>& value 
            = mine.model_sequence ();
	  const std::vector<boost::shared_ptr<const FrameModel>/**/>& other 
            = his.model_sequence ();

          for (size_t i = 0; i < size; i++)
            if (!value[i]->subset (metalib, *other[i]))
              return false;

	  return true;
	}
      case Attribute::Submodel:
	{
	  const std::vector<boost::shared_ptr<const FrameSubmodel>/**/>& value
            = mine.submodel_sequence ();
	  const std::vector<boost::shared_ptr<const FrameSubmodel>/**/>& other 
            = his.submodel_sequence ();

          for (size_t i = 0; i < size; i++)
            if (!value[i]->subset (metalib, *other[i]))
              return false;

	  return true;
	}
      case Attribute::PLF:
	return mine.plf_sequence () == his.plf_sequence ();
      case Attribute::String:
	return mine.name_sequence () == his.name_sequence ();
      case Attribute::Reference:
	return mine.name () == his.name ();
      case Attribute::Scalar:
      case Attribute::Error:
      default:
	daisy_notreached ();
      }
}

int 
Frame::value_size (const symbol key) const
{
  if (impl->has_value (key))
    return impl->get_value (key).size ();
  if (parent ())
    return parent ()->value_size (key);

  daisy_warning ("'" + key + "' has no value in '" + type_name () + "'");
  return -1;
}

symbol 
Frame::value_description (const symbol key) const
{
  if (impl->has_value (key))
    return impl->get_value (key).description ();
  if (parent ())
    return parent ()->value_description (key);

  return Attribute::None ();
}

const std::vector<symbol>& 
Frame::value_cite (const symbol key) const
{
  if (impl->has_value (key))
    return impl->get_value (key).cite ();
  if (parent ())
    return parent ()->value_cite (key);
  
  static const std::vector<symbol> empty;
  return empty;
}

void
Frame::set_reference (const symbol key, const symbol val)
{ 
  impl->set_value (key, new ValueReference (val));
}

bool 
Frame::is_reference (const symbol key) const
{ 
  if (impl->has_value (key))
    return impl->get_value (key).is_reference ();
  if (parent ())
    return parent ()->is_reference (key);

  return false;
}

symbol
Frame::get_reference (const symbol key) const
{
  if (impl->has_value (key))
    return impl->get_value (key).name ();
  if (parent ())
    return parent ()->get_reference (key);

  daisy_panic ("'" + key + "' not found in " + type_name ());
}

double 
Frame::number (const symbol key) const
{ 
  if (impl->has_value (key))
    return impl->get_value (key).number ();
  if (parent ())
    return parent ()->number (key);

  daisy_panic ("'" + key + "' not found in " + type_name ());
}

double 
Frame::number (const symbol key, double default_value) const
{ 
  if (impl->has_value (key))
    return impl->get_value (key).number ();
  if (parent ())
    return parent ()->number (key, default_value);

  return default_value;
}

symbol
Frame::name (const symbol key) const
{ 
  if (impl->has_value (key))
    return impl->get_value (key).name ();
  if (parent ())
    return parent ()->name (key);

  daisy_panic ("'" + key + "' not found in " + type_name ());
}

symbol
Frame::name (const symbol key, 
             const symbol default_value) const
{ 
  if (impl->has_value (key))
    return impl->get_value (key).name ();
  if (parent ())
    return parent ()->name (key, default_value);

  return default_value;
}

bool 
Frame::flag (const symbol key) const
{ 
  if (impl->has_value (key))
    return impl->get_value (key).flag ();
  if (parent ())
    return parent ()->flag (key);

  daisy_panic ("'" + key + "' not found in " + type_name ());
}

bool 
Frame::flag (const symbol key, bool default_value) const
{ 
  if (impl->has_value (key))
    return impl->get_value (key).flag ();
  if (parent ())
    return parent ()->flag (key, default_value);

  return default_value;
}

const PLF& 
Frame::plf (const symbol key) const
{ return *plf_ptr (key); }

boost::shared_ptr<const PLF>
Frame::plf_ptr (const symbol key) const
{ 
  if (impl->has_value (key))
    return impl->get_value (key).plf ();
  if (parent ())
    return parent ()->plf_ptr (key);

  daisy_panic ("'" + key + "' not found in " + type_name ());
}

const FrameModel&
Frame::model (const symbol key) const
{ return *model_ptr (key); }

boost::shared_ptr<const FrameModel>
Frame::model_ptr (const symbol key) const
{
  verify (key, Attribute::Model);

  if (impl->has_value (key))
    return impl->get_value (key).model ();
  if (parent ())
    return parent ()->model_ptr (key);

  daisy_panic ("'" + key + "' not found in " + type_name ());
}

const FrameSubmodel&
Frame::submodel (const symbol key) const
{ return *submodel_ptr (key); }

boost::shared_ptr<const FrameSubmodel>
Frame::submodel_ptr (const symbol key) const
{
  if (type_size (key) != Attribute::Singleton)
    return default_frame (key);
  verify (key, Attribute::Submodel);
  if (impl->has_value (key))
    return impl->get_value (key).submodel ();
  else if (parent () && parent ()->check (key))
    return parent ()->submodel_ptr (key);
  else
    return default_frame (key);
}

int 
Frame::integer (const symbol key) const
{ 
  if (impl->has_value (key))
    return impl->get_value (key).integer ();
  if (parent ())
    return parent ()->integer (key);

  daisy_panic ("'" + key + "' not found in " + type_name ());
}

int 
Frame::integer (const symbol key, int default_value) const
{ 
  if (impl->has_value (key))
    return impl->get_value (key).integer ();
  if (parent ())
    return parent ()->integer (key, default_value);

  return default_value;
}

const std::vector<double>& 
Frame::number_sequence (const symbol key) const
{ 
  if (impl->has_value (key))
    return impl->get_value (key).number_sequence ();
  if (parent ())
    return parent ()->number_sequence (key);

  daisy_panic ("'" + key + "' not found in " + type_name ());
}

const std::vector<symbol>&
Frame::name_sequence (const symbol key) const
{ 
  if (impl->has_value (key))
    return impl->get_value (key).name_sequence ();
  if (parent ())
    return parent ()->name_sequence (key);

  daisy_panic ("'" + key + "' not found in " + type_name ());
}

const std::vector<bool>& 
Frame::flag_sequence (const symbol key) const
{ 
  if (impl->has_value (key))
    return impl->get_value (key).flag_sequence ();
  if (parent ())
    return parent ()->flag_sequence (key);

  daisy_panic ("'" + key + "' not found in " + type_name ());
}

const std::vector<int>& 
Frame::integer_sequence (const symbol key) const
{ 
  if (impl->has_value (key))
    return impl->get_value (key).integer_sequence ();
  if (parent ())
    return parent ()->integer_sequence (key);

  daisy_panic ("'" + key + "' not found in " + type_name ());
}

const std::vector<boost::shared_ptr<const FrameModel>/**/>& 
Frame::model_sequence (const symbol key) const
{ 
  if (impl->has_value (key))
    return impl->get_value (key).model_sequence ();
  if (parent ())
    return parent ()->model_sequence (key);

  daisy_panic ("'" + key + "' not found in " + type_name ());
}

const std::vector<boost::shared_ptr<const FrameSubmodel>/**/>& 
Frame::submodel_sequence (const symbol key) const
{ 
  if (impl->has_value (key))
    return impl->get_value (key).submodel_sequence ();
  if (parent ())
    return parent ()->submodel_sequence (key);

  daisy_panic ("'" + key + "' not found in " + type_name ());
}

const std::vector<boost::shared_ptr<const PLF>/**/>& 
Frame::plf_sequence (const symbol key) const
{ 
  if (impl->has_value (key))
    return impl->get_value (key).plf_sequence ();
  if (parent ())
    return parent ()->plf_sequence (key);

  daisy_panic ("'" + key + "' not found in " + type_name ());
}

void 
Frame::verify (const symbol key, const Attribute::type want, 
               const int value_size) const
{ 
  Attribute::type has = lookup (key);
  if (has == Attribute::Function && want == Attribute::Model)
    has = Attribute::Model;
  if (has != want)
    daisy_panic ("'" + key + "' is " + Attribute::type_name (has) 
                 + ", should be " + Attribute::type_name (want));
  int type_size = this->type_size (key);
  if (Attribute::flexible_size (type_size))
    {
      if (value_size == Attribute::Singleton)
        daisy_panic ("'" + key + "' is a singleton, should be a sequence");
    }
  else if (type_size != value_size)
    daisy_panic ("Size of value does not match type for '" + key + "'");
}

void 
Frame::set (const symbol key, double value)
{ 
  verify (key, Attribute::Number);
  impl->set_value (key, new ValueNumber (value));
}

void 
Frame::set (const symbol key, double value, const symbol dim)
{
  verify (key, Attribute::Number);
  impl->set_value (key, new ValueScalar (value, dim));
}

void 
Frame::set (const symbol key, const symbol name)
{
  if (lookup (key) == Attribute::Model)
    {
      verify (key, Attribute::Model);
      impl->set_model (this->component (key), key, name);
      return;
    }
  if (lookup (key) == Attribute::Function)
    {
      verify (key, Attribute::Function);
      static const symbol function_name (Function::component);
      impl->set_model (function_name, key, name);
      return;
    }
  verify (key, Attribute::String);
  impl->set_value (key, new ValueString (name));
}

void 
Frame::set (const symbol key, const char *const name)
{ set (key, symbol (name)); } 

void 
Frame::set (const symbol key, bool value)
{
  verify (key, Attribute::Boolean);
  impl->set_value (key, new ValueBoolean (value));
}

void 
Frame::set (const symbol key, int value)
{
  verify (key, Attribute::Integer);
  impl->set_value (key, new ValueInteger (value));
}

void 
Frame::set (const symbol key, const FrameModel& value)
{
  boost::shared_ptr<const FrameModel> child (&value.clone ());
  set (key, child);
}

void 
Frame::set (const symbol key, boost::shared_ptr<FrameModel> value)
{

  boost::shared_ptr<const FrameModel> child (value);
  impl->set_value (key, new ValueModel (child));
}

void 
Frame::set (const symbol key, boost::shared_ptr<const FrameModel> value)
{
  verify (key, Attribute::Model); // TODO: Or Attribute::Function
  impl->set_value (key, new ValueModel (value));
}

void 
Frame::set (const symbol key, const FrameSubmodel& value)
{
  boost::shared_ptr<const FrameSubmodel> child (&value.clone ());
  set (key, child);
}

void 
Frame::set (const symbol key, boost::shared_ptr<FrameSubmodel> value)
{
  boost::shared_ptr<const FrameSubmodel> child (value);
  set (key, child);
}

void 
Frame::set (const symbol key, boost::shared_ptr<const FrameSubmodel> value)
{
  verify (key, Attribute::Submodel);
  impl->set_value (key, new ValueSubmodel (value));
}

void 
Frame::set (const symbol key, const PLF& value)
{
  verify (key, Attribute::PLF);
  boost::shared_ptr<const PLF> child (new PLF (value));
  impl->set_value (key, new ValuePLF (child));
}

void 
Frame::set (const symbol key, const std::vector<double>& value)
{
  verify (key, Attribute::Number, value.size ());
  impl->set_value (key, new ValueNumberSeq (value));
}

void 
Frame::set (const symbol key, const std::vector<symbol>& value)
{
  if (lookup (key) == Attribute::Model)
    {
      verify (key, Attribute::Model, value.size ());
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
      impl->set_value (key, new ValueModelSeq (frames));
      return;
    }
  verify (key, Attribute::String, value.size ());
  impl->set_value (key, new ValueStringSeq (value));
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
  verify (key, Attribute::Boolean, value.size ());
  impl->set_value (key, new ValueBooleanSeq (value));
}

void 
Frame::set (const symbol key, const std::vector<int>& value)
{
  verify (key, Attribute::Integer, value.size ());
  impl->set_value (key, new ValueIntegerSeq (value));
}

void 
Frame::set (const symbol key, const std::vector<boost::shared_ptr<const FrameModel>/**/>& value)
{
  verify (key, Attribute::Model, value.size ());
  impl->set_value (key, new ValueModelSeq (value));
}

void 
Frame::set (const symbol key, const std::vector<boost::shared_ptr<const FrameSubmodel>/**/>& value)
{
  verify (key, Attribute::Submodel, value.size ());
  impl->set_value (key, new ValueSubmodelSeq (value));
}

void 
Frame::set (const symbol key, const std::vector<boost::shared_ptr<const PLF>/**/>& value)
{
  verify (key, Attribute::PLF, value.size ());
  impl->set_value (key, new ValuePLFSeq (value));
}

void 
Frame::set_empty (const symbol key)
{
  switch (lookup (key))
    {
    case Attribute::Number:
      impl->set_value (key, new ValueNumberSeq (std::vector<double> ()));
      break;
    case Attribute::Model:
    case Attribute::Function:
      impl->set_value (key, new ValueModelSeq (std::vector<boost::shared_ptr<const FrameModel>/**/> ()));
      break;
    case Attribute::Submodel:
      impl->set_value (key, new ValueSubmodelSeq (std::vector<boost::shared_ptr<const FrameSubmodel>/**/> ()));
      break;
    case Attribute::PLF:
      impl->set_value (key, new ValuePLFSeq (std::vector<boost::shared_ptr<const PLF>/**/> ()));
      break;
    case Attribute::Boolean:
      impl->set_value (key, new ValueBooleanSeq (std::vector<bool> ()));
      break;
    case Attribute::String:
      impl->set_value (key, new ValueStringSeq (std::vector<symbol> ()));
      break;
    case Attribute::Integer:
      impl->set_value (key, new ValueIntegerSeq (std::vector<int> ()));
      break;
    case Attribute::Scalar:
    case Attribute::Reference:
    case Attribute::Error:
    default:
      daisy_notreached ();
    }
}

void 
Frame::set_described (const symbol key, const double value, const symbol desc)
{ 
  verify (key, Attribute::Number);
  impl->set_value (key, new ValueNumberDescription (value, desc));
}

void 
Frame::set_described (const symbol key, const PLF& value, const symbol desc)
{ 
  verify (key, Attribute::PLF);
  boost::shared_ptr<const PLF> child (new PLF (value));
  impl->set_value (key, new ValuePLFDescription (child, desc));
}

void 
Frame::set_cited (const symbol key, double const value, const symbol desc,
                  const std::vector<symbol>& citations)
{
  verify (key, Attribute::Number);
  impl->set_value (key, new ValueNumberCite (value, desc, citations));
}

void 
Frame::set_cited (const symbol key, const PLF& value, const symbol desc,
                  const std::vector<symbol>& citations)
{
  verify (key, Attribute::PLF);
  boost::shared_ptr<const PLF> child (new PLF (value));
  impl->set_value (key, new ValuePLFCite (child, desc, citations));
}

void 
Frame::set_cited (symbol key, double value, symbol desc,
                  symbol citation)
{
  std::vector<symbol> citations;
  citations.push_back (citation);
  set_cited (key, value, desc, citations);
}

void 
Frame::set_cited (symbol key, const PLF& value, symbol desc,
                  symbol citation)
{
  std::vector<symbol> citations;
  citations.push_back (citation);
  set_cited (key, value, desc, citations);
}

Frame::Frame (const Frame& old)
  : WScope (),
    impl (new Implementation (*old.impl))
{ }

Frame::Frame ()
  : impl (new Implementation ())
{ }

void
Frame::overwrite_values (const Frame& other)
{ 
  if (this == &other)
    return;

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
