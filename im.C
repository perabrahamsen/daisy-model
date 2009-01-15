// im.C -- Inorganic matter.  Track chemicals.
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
// Copyright 2007 Per Abrahamsen and KVL.
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

#include "im.h"
#include "chemical.h"
#include "units.h"
#include "unit.h"
#include "am.h"
#include "log.h"
#include "block.h"
#include "alist.h"
#include "syntax.h"
#include "frame.h"
#include "check.h"
#include "assertion.h"
#include "frame.h"
#include <cmath>

symbol 
Scalar::dimension () const
{ return unit_.native_name (); }

symbol
IM::storage_unit ()
{
  static const symbol unit ("g/cm^2");
  return unit;
}

symbol
IM::mass_unit ()
{
  static const symbol unit ("g");
  return unit;
}

symbol
IM::flux_unit ()
{
  static const symbol unit ("g/cm^2/h");
  return unit;
}

symbol
IM::solute_unit ()
{
  static const symbol unit ("g/cm^2/mm");
  return unit;
}

const Unit& 
IM::unit () const
{
  daisy_assert (unit_);
  return *unit_;
}

double 
IM::get_value (const symbol chem, const Unit& other) const
{
  daisy_assert (unit_);
  return Units::unit_convert (*unit_, other, get_value_raw (chem)); 
}

void 
IM::set_value (const symbol chem, const Unit& other, const double value)
{
  daisy_assert (unit_);
  set_value_raw (chem, Units::unit_convert (other, *unit_, value)); 
}

void 
IM::add_value (const symbol chem, const Unit& other, const double value)
{
  set_value (chem, other, value + get_value (chem, other)); 
}

double 
IM::get_value_raw (const symbol chem) const
{
  std::map<symbol, double>::const_iterator i = content.find (chem);

  if (i != content.end ())
    return (*i).second;

  return 0.0;
}

void 
IM::set_value_raw (const symbol chem, const double value)
{ content[chem] = value; }

void
IM::output (Log& log) const
{
  for (std::map<symbol, double>::const_iterator i = content.begin (); 
       i != content.end ();
       i++)
    {
      const symbol name = (*i).first;
      if (!log.check_interior (name))
	continue;

      Log::Named named (log, name);
      output_variable (name, log);
      const double value = (*i).second;
      output_variable (value, log);
    }
}

void 
IM::rebase (const Unit& other)
{
  daisy_assert (unit_);
  for (std::map<symbol, double>::iterator i = content.begin (); 
       i != content.end ();
       i++)
    (*i).second = Units::unit_convert (*unit_, other, (*i).second);
  unit_ = &other;
}

void
IM::operator += (const IM& n)
{ 
  daisy_assert (unit_);
  daisy_assert (n.unit_);

  for (std::map<symbol, double>::const_iterator i = n.content.begin (); 
       i != n.content.end ();
       i++)
    content[(*i).first] += Units::unit_convert (*n.unit_, *unit_, (*i).second);
}

IM
IM::operator+ (const IM& im) const
{
  IM result (*this);
  result += im;
  return result;
}

void
IM::multiply_assign (const Scalar& s, const Unit& u)
{
  daisy_assert (unit_);
  for (std::map<symbol, double>::iterator i = content.begin (); 
       i != content.end ();
       i++)
    (*i).second = Units::multiply (*unit_, s.unit (),
                                   (*i).second * s.value (), u);
  unit_ = &u;
}

IM
IM::multiply (const Scalar& s, const Unit& u) const
{
  IM result (*this);
  result.multiply_assign (s, u);
  return result;
}

IM& 
IM::operator= (const IM& im)
{
  unit_ = im.unit_;
  content = im.content;
  return *this;
}

void
IM::clear ()
{
  for (std::map<symbol, double>::iterator i = content.begin (); 
       i != content.end ();
       i++)
    (*i).second = 0.0;
}

IM::IM (Block& parent, const char *const key)
{
  // Find dimension.
  const Frame& parent_frame = parent.find_frame (key);
  const Syntax& syntax = parent_frame.syntax (key);
  unit_ = &(parent.units ().get_unit (symbol (syntax.dimension ("value"))));
  
  // Find content.
  const std::vector<const AttributeList*>& alists = parent.alist_sequence (key);
  for (size_t i = 0; i < alists.size (); i++)
    {
      const AttributeList& al = *alists[i];
      content[al.name ("name")] = al.number ("value");
    }
}

IM::IM ()
  : unit_ (NULL)
{ }

IM::IM (const IM& im)
  : unit_ (im.unit_),
    content (im.content)
{ }

IM::IM (const Unit& u)
  : unit_ (&u)
{ }

IM::IM (const Unit& u, const IM& im)
  : unit_ (&u)
{
  for (const_iterator i = im.begin (); i != im.end (); i++)
    content[*i] = im.get_value (*i, u);
}

IM::~IM ()
{ daisy_assert (unit_);  }

void
IM::add_syntax (Frame& parent_frame,
		Value::category cat, 
		const char *const key,
		const symbol dimension,
		const char *const description)
{
  Syntax& child_syntax = *new Syntax ();
  child_syntax.add ("name", Value::String, cat, 
		    "Name of chemical.");
  child_syntax.add_check ("name", Chemical::check_library ());
  child_syntax.add ("value", dimension.name (), Check::non_negative (), cat, 
		    "Value for chemical.");
  child_syntax.order ("name", "value");
  parent_frame.add (key, child_syntax, cat, Value::Sequence, description);
  parent_frame.add (key, std::vector<const AttributeList*> ());
}

// im.C ends here.
