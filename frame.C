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
  Syntax syntax;
  AttributeList alist;

  Implementation (const Implementation& old)
    : syntax (old.syntax),
      alist (old.alist)
  { }
  Implementation ()
  { }
  Implementation (const Syntax& s, const AttributeList& a)
    : syntax (s),
      alist (a)
  { }
};

AttributeList& 
Frame::alist () const
{ return impl->alist; }

const Syntax& 
Frame::syntax () const
{ return impl->syntax; }


void 
Frame::entries (std::vector<symbol>& e) const
{ impl->syntax.entries (e); }

unsigned int 
Frame::entries () const
{ return impl->syntax.entries (); }

bool 
Frame::check (const Metalib& metalib, Treelog& err) const
{ return impl->syntax.check (metalib, alist (), err); }

void 
Frame::check (const symbol key, double value) const
{ 
  if (parent () && impl->syntax.lookup (key) == Value::Error)
    parent ()->check (key, value); 
  else
    impl->syntax.check (key, value); 
}

bool 
Frame::check (const Metalib& metalib, 
              const symbol key, Treelog& msg) const
{ return impl->syntax.check (metalib, alist (), key, msg); }

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

const Syntax& 
Frame::syntax (const symbol key) const
{
  if (parent () && impl->syntax.lookup (key) == Value::Error)
    return parent ()->syntax (key);
  else
    return impl->syntax.syntax (key);
}

::Library& 
Frame::library (const Metalib& metalib, const symbol key) const
{
  if (parent () && impl->syntax.lookup (key) == Value::Error)
    return parent ()->library (metalib, key);
  else
    return impl->syntax.library (metalib, key);
}

int  
Frame::size (const symbol key) const
{
  if (parent () && impl->syntax.lookup (key) == Value::Error)
    return parent ()->size (key);
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

const AttributeList& 
Frame::default_alist (const symbol key) const
{
  if (parent () && impl->syntax.lookup (key) == Value::Error)
    return parent ()->default_alist (key);
  else
    return impl->syntax.default_alist (key);
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
Frame::add (const symbol key, const Syntax& syntax,
	    Value::category cat, int size, 
	    const symbol description)
{ impl->syntax.add (key, syntax, cat,size, description); }

void 
Frame::add (const symbol key, 
            const Syntax& syntax, const AttributeList& alist,	
	    // Alist sequence with default element.
	    Value::category cat, int size, const symbol description)
{ impl->syntax.add (key, syntax, alist, cat, size, description); }


void 
Frame::add_object (const symbol key, const char* lib,
                   Value::category cat, int size, const symbol description)
{ impl->syntax.add_object (key, lib, cat, size, description); }

void 
Frame::add_library (const symbol key, symbol lib)
{ impl->syntax.add_library (key, lib); }


void 
Frame::add_submodule (const symbol name, AttributeList& alist,
		      Value::category cat, const symbol description,
		      Syntax::load_syntax_fun load_syntax)
{ impl->syntax.add_submodule (name, alist, cat, description, load_syntax); }

void 
Frame::add_submodule_sequence (const symbol name, Value::category cat, 
			       const symbol description,
			       Syntax::load_syntax_fun load_syntax)
{ impl->syntax.add_submodule_sequence (name, cat, description, load_syntax); }


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
Frame::add_check (Syntax::check_fun fun)
{ impl->syntax.add_check (fun); }

void 
Frame::add_object_check (Syntax::check_object fun)
{ impl->syntax.add_object_check (fun); }

bool 
Frame::check (const symbol key) const
{ 
  return impl->alist.check (key)
    || (parent () && parent ()->check (key));
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

const std::vector<const PLF*>& 
Frame::plf_sequence (const symbol key) const
{ 
  if (parent () && !impl->alist.check (key))
    return parent ()->plf_sequence (key);
  else
    return impl->alist.plf_sequence (key);
}

const std::vector<const AttributeList*>& 
Frame::alist_sequence (const symbol key) const
{ 
  if (parent () && !impl->alist.check (key))
    return parent ()->alist_sequence (key);
  else
    return impl->alist.alist_sequence (key);
}

void 
Frame::add (const symbol key, double value)
{ impl->alist.add (key, value); }

void 
Frame::add (const symbol key, double value, const symbol dim)
{ impl->alist.add (key, value, dim); }

void 
Frame::add (const symbol key, const char* name)
{ impl->alist.add (key, name); }

void 
Frame::add (const symbol key, const symbol name)
{ impl->alist.add (key, name); }

void 
Frame::add (const symbol key, bool value)
{ impl->alist.add (key, value); }

void 
Frame::add (const symbol key, int value)
{ impl->alist.add (key, value); }

void 
Frame::add (const symbol key, const AttributeList& value)
{ impl->alist.add (key, value); }

void 
Frame::add (const symbol key, const PLF& value)
{ impl->alist.add (key, value); }

void 
Frame::add (const symbol key, const std::vector<double>& value)
{ impl->alist.add (key, value); }

void 
Frame::add (const symbol key, const std::vector<symbol>& value)
{ impl->alist.add (key, value); }

void 
Frame::add_strings (const symbol key, const symbol a)
{ impl->alist.add_strings (key, a); }

void 
Frame::add_strings (const symbol key,
                    const symbol a, const symbol b)
{ impl->alist.add_strings (key, a, b); }

void 
Frame::add_strings (const symbol key,
                    const symbol a, const symbol b,
                    const symbol c)
{ impl->alist.add_strings (key, a, b, c); }

void 
Frame::add (const symbol key, const std::vector<bool>& value)
{ impl->alist.add (key, value); }

void 
Frame::add (const symbol key, const std::vector<int>& value)
{ impl->alist.add (key, value); }

void 
Frame::add (const symbol key, 
            const std::vector<const AttributeList*>& value)
{ impl->alist.add (key, value); }

void 
Frame::add (const symbol key, const std::vector<const PLF*>& value)
{ impl->alist.add (key, value); }

Frame::Frame (const Frame& old)
  : impl (new Implementation (*old.impl))
{ }

Frame::Frame ()
  : impl (new Implementation ())
{ }

Frame::Frame (const Syntax& s, const AttributeList& a)
  : impl (new Implementation (s, a))
{ }

Frame::~Frame ()
{ }

// frame.C ends here.
