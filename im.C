// im.C
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
#include "am.h"
#include "log.h"
#include "block.h"
#include "alist.h"
#include "syntax.h"
#include "check.h"
#include "submodel.h"
#include "assertion.h"
#include <cmath>

symbol
IM::storage_unit ()
{
  static const symbol unit ("g/cm^2");
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

double 
IM::get_value (const symbol chem, const symbol dim) const
{
  return Units::convert (dimension, dim, get_value_raw (chem)); 
}

void 
IM::set_value (const symbol chem, const symbol dim, const double value)
{
  set_value_raw (chem, Units::convert (dim, dimension, value)); 
}

void 
IM::add_value (const symbol chem, const symbol dim, const double value)
{
  set_value (chem, dim, value + get_value (chem, dim)); 
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
IM::rebase (const symbol dim)
{
  for (std::map<symbol, double>::iterator i = content.begin (); 
       i != content.end ();
       i++)
    (*i).second = Units::convert (dimension, dim, (*i).second);
  dimension = dim;
}

void 
IM::rebase (const char *const dim)
{ rebase (symbol (dim)); }

void
IM::operator += (const IM& n)
{ 
  daisy_assert (dimension != Syntax::unknown ());

  for (std::map<symbol, double>::const_iterator i = n.content.begin (); 
       i != n.content.end ();
       i++)
    content[(*i).first] += Units::convert (n.dimension, dimension, (*i).second);
}

IM
IM::operator+ (const IM& im) const
{
  IM result (*this);
  result += im;
  return result;
}

void
IM::operator*= (const Scalar& s)
{
  dimension = Units::multiply (dimension, s.dimension ());
  for (std::map<symbol, double>::iterator i = content.begin (); 
       i != content.end ();
       i++)
    (*i).second *= s.value ();
}

IM
IM::operator* (const Scalar& s) const
{
  IM result (*this);
  result *= s;
  return result;
}

IM& 
IM::operator= (const IM& im)
{
  dimension = im.dimension;
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
  const Syntax& parent_syntax = parent.find_syntax (key);
  const Syntax& syntax = parent_syntax.syntax (key);
  dimension = symbol (syntax.dimension ("value"));
  
  // Find content.
  const std::vector<const AttributeList*>& alists = parent.alist_sequence (key);
  for (size_t i = 0; i < alists.size (); i++)
    {
      const AttributeList& al = *alists[i];
      content[al.identifier ("name")] = al.number ("value");
    }
}

IM::IM ()
  : dimension (Syntax::unknown ())
{ }

IM::IM (const IM& im)
  : dimension (im.dimension),
    content (im.content)
{ daisy_assert (dimension != Syntax::unknown ()); }

IM::IM (const symbol dim)
  : dimension (dim)
{ daisy_assert (dimension != Syntax::unknown ()); }

IM::IM (const symbol dim, const IM& im)
  : dimension (dim)
{
  for (const_iterator i = im.begin (); i != im.end (); i++)
    content[*i] = im.get_value (*i, dimension);
}

IM::~IM ()
{ daisy_assert (dimension != Syntax::unknown ());  }

void
IM::add_syntax (Syntax& parent_syntax, AttributeList& parent_alist,
		Syntax::category cat, 
		const char *const key,
		const symbol dimension,
		const char *const description)
{
  Syntax& child_syntax = *new Syntax ();
  child_syntax.add ("name", Syntax::String, cat, 
		    "Name of chemical.");
  child_syntax.add_check ("name", Chemical::check_library ());
  child_syntax.add ("value", dimension.name (), Check::non_negative (), cat, 
		    "Value for chemical.");
  child_syntax.order ("name", "value");
  parent_syntax.add (key, child_syntax, cat, Syntax::Sequence, description);
  parent_alist.add (key, std::vector<const AttributeList*> ());
}

// im.C ends here.
