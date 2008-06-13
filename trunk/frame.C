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
#include "alist.h"

struct Frame::Implementation
{
  const Frame *const parent;
  Syntax syntax;
  AttributeList alist;

  Implementation ()
    : parent (NULL)
  { }
  Implementation (const Frame& frame)
    : parent (&frame)
  { }
};

const AttributeList& 
Frame::alist () const
{ return impl->alist; }

const Syntax& 
Frame::syntax () const
{ return impl->syntax; }


void 
Frame::entries (std::vector<std::string>& e) const
{ impl->syntax.entries (e); }

unsigned int 
Frame::entries () const
{ return impl->syntax.entries (); }

bool 
Frame::check (const Metalib& metalib, Treelog& err) const
{ return impl->syntax.check (metalib, alist (), err); }

void 
Frame::check (const std::string& key, double value) const
{ 
  if (impl->parent && impl->syntax.lookup (key) == Syntax::Error)
    impl->parent->check (key, value); 
  else
    impl->syntax.check (key, value); 
}

bool 
Frame::check (const Metalib& metalib, 
              const std::string& key, Treelog& msg) const
{ return impl->syntax.check (metalib, alist (), key, msg); }

bool 
Frame::is_const (const std::string& key) const
{
  if (impl->parent && impl->syntax.lookup (key) == Syntax::Error)
    return impl->parent->is_const (key);
  else
    return impl->syntax.is_const (key);
}

bool 
Frame::is_optional (const std::string& key) const
{
  if (impl->parent && impl->syntax.lookup (key) == Syntax::Error)
    return impl->parent->is_optional (key);
  else
    return impl->syntax.is_optional (key);
}

bool 
Frame::is_log (const std::string& key) const
{
  if (impl->parent && impl->syntax.lookup (key) == Syntax::Error)
    return impl->parent->is_log (key);
  else
    return impl->syntax.is_log (key);
}

bool 
Frame::is_state (const std::string& key) const
{
  if (impl->parent && impl->syntax.lookup (key) == Syntax::Error)
    return impl->parent->is_state (key);
  else
    return impl->syntax.is_state (key);
}

Syntax::type 
Frame::lookup (const std::string& key) const
{
  if (impl->parent && impl->syntax.lookup (key) == Syntax::Error)
    return impl->parent->lookup (key);
  else
    return impl->syntax.lookup (key);
}

const Syntax& 
Frame::syntax (const std::string& key) const
{
  if (impl->parent && impl->syntax.lookup (key) == Syntax::Error)
    return impl->parent->syntax (key);
  else
    return impl->syntax.syntax (key);
}

::Library& 
Frame::library (const Metalib& metalib, const std::string& key) const
{
  if (impl->parent && impl->syntax.lookup (key) == Syntax::Error)
    return impl->parent->library (metalib, key);
  else
    return impl->syntax.library (metalib, key);
}

int  
Frame::size (const std::string& key) const
{
  if (impl->parent && impl->syntax.lookup (key) == Syntax::Error)
    return impl->parent->size (key);
  else
    return impl->syntax.size (key);
}

const std::string& 
Frame::dimension (const std::string& key) const
{
  if (impl->parent && impl->syntax.lookup (key) == Syntax::Error)
    return impl->parent->dimension (key);
  else
    return impl->syntax.dimension (key);
}

const std::string& 
Frame::domain (const std::string& key) const
{
  if (impl->parent && impl->syntax.lookup (key) == Syntax::Error)
    return impl->parent->domain (key);
  else
    return impl->syntax.domain (key);
}

const std::string& 
Frame::range (const std::string& key) const
{
  if (impl->parent && impl->syntax.lookup (key) == Syntax::Error)
    return impl->parent->range (key);
  else
    return impl->syntax.range (key);
}

const std::string& 
Frame::description (const std::string& key) const
{
  if (impl->parent && impl->syntax.lookup (key) == Syntax::Error)
    return impl->parent->description (key);
  else
    return impl->syntax.description (key);
}

const AttributeList& 
Frame::default_alist (const std::string& key) const
{
  if (impl->parent && impl->syntax.lookup (key) == Syntax::Error)
    return impl->parent->default_alist (key);
  else
    return impl->syntax.default_alist (key);
}

void 
Frame::add (const std::string& key,	// Generic.
	    Syntax::type t, 
	    Syntax::category cat,
	    int size,
	    const std::string& description)
{ impl->syntax.add (key, t, cat, size, description); }


void 
Frame::add (const std::string& key, // Number.
	    const std::string& dim,
	    Syntax::category cat,
	    int size,
	    const std::string& description)
{ impl->syntax.add (key, dim, cat, size, description); }


void 
Frame::add (const std::string& key,
	    const std::string& dim,
	    const Check& check,
	    Syntax::category cat,
	    int size,
	    const std::string& description)
{ impl->syntax.add (key, dim, check, cat, size, description); }


void 
Frame::add_fraction (const std::string& key, 
		     Syntax::category cat,
		     int size,
		     const std::string& description)
{ impl->syntax.add_fraction (key, cat, size, description); }


void 
Frame::add_fraction (const std::string& key, 
		     Syntax::category cat,
		     const std::string& description)
{ impl->syntax.add_fraction (key, cat, description); }


void 
Frame::add (const std::string& key, // PLF.
	    const std::string& domain,
	    const std::string& range,
	    Syntax::category cat,
	    int size,
	    const std::string& description)
{ impl->syntax.add (key, domain, range, cat, size, description); }


void 
Frame::add (const std::string& key,
	    const std::string& domain,
	    const std::string& range,
	    const Check& check,
	    Syntax::category cat,
	    int size,
	    const std::string& description)
{ impl->syntax.add (key, domain, range, check, cat, size, description); }


void 
Frame::add (const std::string& key, const Syntax& syntax,
	    Syntax::category cat, int size, 
	    const std::string& description)
{ impl->syntax.add (key, syntax, cat,size, description); }

void 
Frame::add (const std::string& key, 
            const Syntax& syntax, const AttributeList& alist,	
	    // Alist sequence with default element.
	    Syntax::category cat, int size, const std::string& description)
{ impl->syntax.add (key, syntax, alist, cat, size, description); }


void 
Frame::add_object (const std::string& key, const char* lib,
                   Syntax::category cat, int size, const std::string& description)
{ impl->syntax.add_object (key, lib, cat, size, description); }

void 
Frame::add_library (const std::string& key, symbol lib)
{ impl->syntax.add_library (key, lib); }


void 
Frame::add_submodule (const std::string& name, AttributeList& alist,
		      Syntax::category cat, const std::string& description,
		      Syntax::load_syntax_fun load_syntax)
{ impl->syntax.add_submodule (name, alist, cat, description, load_syntax); }

void 
Frame::add_submodule_sequence (const std::string& name, Syntax::category cat, 
			       const std::string& description,
			       Syntax::load_syntax_fun load_syntax)
{ impl->syntax.add_submodule_sequence (name, cat, description, load_syntax); }


void 
Frame::add_check (const std::string& name, const VCheck& vcheck)
{ impl->syntax.add_check (name, vcheck); }


void 
Frame::order (const std::vector<std::string>& v)
{ impl->syntax.order (v); }

void 
Frame::order (const std::string& a)
{ impl->syntax.order (a); }

void 
Frame::order (const std::string& a, const std::string& b)
{ impl->syntax.order (a, b); }

void 
Frame::order (const std::string& a, const std::string& b, const std::string& c)
{ impl->syntax.order (a, b, c); }

void 
Frame::order (const std::string& a, const std::string& b, const std::string& c,
	      const std::string& d)
{ impl->syntax.order (a, b, c, d); }

void 
Frame::order (const std::string& a, const std::string& b, const std::string& c,
	      const std::string& d, const std::string& e)
{ impl->syntax.order (a, b, c, d, e); }


bool 
Frame::ordered () const
{ return impl->syntax.ordered (); }

const std::vector<std::string>& 
Frame::order () const
{ return impl->syntax.order (); }

int 
Frame::order_index (const std::string& key) const
{
  if (impl->parent && impl->syntax.lookup (key) == Syntax::Error)
    return impl->parent->order_index (key);
  else
    return impl->syntax.order_index (key);
}

bool 
Frame::total_order () const
{ return impl->syntax.total_order (); }

void 
Frame::add_check (Syntax::check_fun fun)
{ impl->syntax.add_check (fun); }

void 
Frame::add_object_check (Syntax::check_object fun)
{ impl->syntax.add_object_check (fun); }

bool 
Frame::check (const std::string& key) const
{ 
  return impl->alist.check (key)
    || (impl->parent && impl->parent->check (key));
}

bool 
Frame::check (const symbol key) const
{ 
  return impl->alist.check (key)
    || (impl->parent && impl->parent->check (key));
}

double 
Frame::number (const std::string& key) const
{ 
  if (impl->parent && !impl->alist.check (key))
    return impl->parent->number (key);
  else
    return impl->alist.number (key);
}

double 
Frame::number (const std::string& key, double default_value) const
{ 
  if (impl->parent && !impl->alist.check (key))
    return impl->parent->number (key, default_value);
  else
    return impl->alist.number (key, default_value);
}

const std::string 
Frame::name (const std::string& key) const
{ 
  if (impl->parent && !impl->alist.check (key))
    return impl->parent->name (key);
  else
    return impl->alist.name (key);
}

const std::string 
Frame::name (const std::string& key, 
             const std::string& default_value) const
{ 
  if (impl->parent && !impl->alist.check (key))
    return impl->parent->name (key, default_value);
  else
    return impl->alist.name (key, default_value);
}

symbol 
Frame::identifier (const std::string& key) const
{ 
  if (impl->parent && !impl->alist.check (key))
    return impl->parent->identifier (key);
  else
    return impl->alist.identifier (key);
}

symbol 
Frame::identifier (const symbol key) const
{ 
  if (impl->parent && !impl->alist.check (key))
    return impl->parent->identifier (key);
  else
    return impl->alist.identifier (key);
}

bool 
Frame::flag (const std::string& key) const
{ 
  if (impl->parent && !impl->alist.check (key))
    return impl->parent->flag (key);
  else
    return impl->alist.flag (key);
}

bool 
Frame::flag (const std::string& key, bool default_value) const
{ 
  if (impl->parent && !impl->alist.check (key))
    return impl->parent->flag (key, default_value);
  else
    return impl->alist.flag (key, default_value);
}

const PLF& 
Frame::plf (const std::string& key) const
{ 
  if (impl->parent && !impl->alist.check (key))
    return impl->parent->plf (key);
  else
    return impl->alist.plf (key);
}

AttributeList& 
Frame::alist (const std::string& key) const
{ 
  if (impl->parent && !impl->alist.check (key))
    return impl->parent->alist (key);
  else
    return impl->alist.alist (key);
}

int 
Frame::integer (const std::string& key) const
{ 
  if (impl->parent && !impl->alist.check (key))
    return impl->parent->integer (key);
  else
    return impl->alist.integer (key);
}

int 
Frame::integer (const std::string& key, int default_value) const
{ 
  if (impl->parent && !impl->alist.check (key))
    return impl->parent->integer (key, default_value);
  else
    return impl->alist.integer (key, default_value);
}

const std::vector<double>& 
Frame::number_sequence (const std::string& key) const
{ 
  if (impl->parent && !impl->alist.check (key))
    return impl->parent->number_sequence (key);
  else
    return impl->alist.number_sequence (key);
}

const std::vector<symbol> 
Frame::identifier_sequence (const std::string& key) const
{ 
  if (impl->parent && !impl->alist.check (key))
    return impl->parent->identifier_sequence (key);
  else
    return impl->alist.identifier_sequence (key);
}

std::vector<std::string> 
Frame::name_sequence (const std::string& key) const
{ 
  if (impl->parent && !impl->alist.check (key))
    return impl->parent->name_sequence (key);
  else
    return impl->alist.name_sequence (key);
}

const std::vector<bool>& 
Frame::flag_sequence (const std::string& key) const
{ 
  if (impl->parent && !impl->alist.check (key))
    return impl->parent->flag_sequence (key);
  else
    return impl->alist.flag_sequence (key);
}

const std::vector<int>& 
Frame::integer_sequence (const std::string& key) const
{ 
  if (impl->parent && !impl->alist.check (key))
    return impl->parent->integer_sequence (key);
  else
    return impl->alist.integer_sequence (key);
}

const std::vector<const PLF*>& 
Frame::plf_sequence (const std::string& key) const
{ 
  if (impl->parent && !impl->alist.check (key))
    return impl->parent->plf_sequence (key);
  else
    return impl->alist.plf_sequence (key);
}

const std::vector<const AttributeList*>& 
Frame::alist_sequence (const std::string& key) const
{ 
  if (impl->parent && !impl->alist.check (key))
    return impl->parent->alist_sequence (key);
  else
    return impl->alist.alist_sequence (key);
}

void 
Frame::add (const std::string& key, double value)
{ impl->alist.add (key, value); }

void 
Frame::add (const std::string& key, double value, const std::string& dim)
{ impl->alist.add (key, value, dim); }

void 
Frame::add (const std::string& key, const char* name)
{ impl->alist.add (key, name); }

void 
Frame::add (const std::string& key, const std::string& name)
{ impl->alist.add (key, name); }

void 
Frame::add (const std::string& key, symbol name)
{ impl->alist.add (key, name); }

void 
Frame::add (const std::string& key, bool value)
{ impl->alist.add (key, value); }

void 
Frame::add (const std::string& key, int value)
{ impl->alist.add (key, value); }

void 
Frame::add (const std::string& key, const AttributeList& value)
{ impl->alist.add (key, value); }

void 
Frame::add (const std::string& key, const PLF& value)
{ impl->alist.add (key, value); }

void 
Frame::add (const std::string& key, const std::vector<double>& value)
{ impl->alist.add (key, value); }

void 
Frame::add (const std::string& key, const std::vector<symbol>& value)
{ impl->alist.add (key, value); }

void 
Frame::add_strings (const std::string& key, const std::string& a)
{ impl->alist.add_strings (key, a); }

void 
Frame::add_strings (const std::string& key,
                    const std::string& a, const std::string& b)
{ impl->alist.add_strings (key, a, b); }

void 
Frame::add_strings (const std::string& key,
                    const std::string& a, const std::string& b,
                    const std::string& c)
{ impl->alist.add_strings (key, a, b, c); }

void 
Frame::add (const std::string& key, const std::vector<bool>& value)
{ impl->alist.add (key, value); }

void 
Frame::add (const std::string& key, const std::vector<int>& value)
{ impl->alist.add (key, value); }

void 
Frame::add (const std::string& key, 
            const std::vector<const AttributeList*>& value)
{ impl->alist.add (key, value); }

void 
Frame::add (const std::string& key, const std::vector<const PLF*>& value)
{ impl->alist.add (key, value); }

Frame::Frame (const Frame& parent, parent_relationship_t)
  : impl (new Implementation (parent))
{ }

Frame::Frame ()
  : impl (new Implementation ())
{ }

Frame::~Frame ()
{ }

// frame.C ends here.
