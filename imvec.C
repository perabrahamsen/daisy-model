// imvec.C -- Keep track of vectors of inorganic matter.
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
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

#include "imvec.h"
#include "assertion.h"
#include "log.h"
#include "chemical.h"
#include "check.h"
#include "block.h"

const std::vector<double>& 
IMvec::get_value (symbol chem) const
{ 
  map_type::const_iterator i = content.find (chem);
  daisy_assert (i != content.end ());
  return (*i).second; 
}

void
IMvec::set_value (symbol chem, const std::vector<double>& value)
{ content[chem] = value; }

void
IMvec::output (Log& log) const
{
  for (map_type::const_iterator i = content.begin (); 
       i != content.end ();
       i++)
    {
      const symbol name = (*i).first;
      if (!log.check_interior (name))
	continue;

      Log::Named named (log, name);
      output_variable (name, log);
      const std::vector<double>& value = (*i).second;
      output_variable (value, log);
    }
}

void 
IMvec::add_syntax (Syntax& parent_syntax, AttributeList& parent_alist,
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
                    Syntax::Sequence, "Value for chemical.");
  child_syntax.order ("name", "value");
  parent_syntax.add (key, child_syntax, cat, Syntax::Sequence, description);
  parent_alist.add (key, std::vector<const AttributeList*> ());
}

IMvec::IMvec (Block& parent, const char* key)
{
  const std::vector<const AttributeList*>& alists = parent.alist_sequence (key);
  for (size_t i = 0; i < alists.size (); i++)
    {
      const AttributeList& al = *alists[i];
      content[al.identifier ("name")] = al.number_sequence ("value");
    }
}

IMvec::~IMvec ()
{ }

// IMvec.C ends here.
