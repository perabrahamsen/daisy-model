// chemistry.C --- Pesticedes and other chemicals.
// 
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

#include "chemistry.h"
#include "im.h"
#include "chemical.h"
#include "treelog.h"
#include "block.h"
#include "librarian.h"
#include "vcheck.h"
#include "alist.h"

const char *const Chemistry::component = "chemistry";

symbol 
Chemistry::library_id () const
{
  static const symbol id (component);
  return id;
}

bool
Chemistry::require (const symbol chem, Treelog& msg) const
{
  if (know (chem))
    return true;
  
  msg.error ("Required chemical '" + chem.name () + "' not found");
  return false;
}

void 
Chemistry::deposit (const IM& im, double dt, Treelog& msg)
{ 
  for (IM::const_iterator i = im.begin (); i != im.end (); i++)
    {
      const symbol chem = *i;
      const double amount = im.get_value (chem, Chemical::spray_unit ());
      deposit (chem, amount, dt, msg);
    }
}

void 
Chemistry::spray (const IM& im, double dt, Treelog& msg)
{ 
  for (IM::const_iterator i = im.begin (); i != im.end (); i++)
    {
      const symbol chem = *i;
      const double amount = im.get_value (chem, Chemical::spray_unit ());
      spray (chem, amount, dt, msg);
    }
}

void 
Chemistry::incorporate (const Geometry& geo, const IM& im, 
			const double from, const double to,
			const double dt, Treelog& msg)
{
  for (IM::const_iterator i = im.begin (); i != im.end (); i++)
    {
      const symbol chem = *i;
      const double amount = im.get_value (chem, Chemical::spray_unit ());
      incorporate (geo, chem, amount, from, to, dt, msg);
    }
}

void 
Chemistry::incorporate (const Geometry& geo, const IM& im, 
			const Volume& volume,
			const double dt, Treelog& msg)
{
  for (IM::const_iterator i = im.begin (); i != im.end (); i++)
    {
      const symbol chem = *i;
      const double amount = im.get_value (chem, Chemical::spray_unit ());
      incorporate (geo, chem, amount, volume, dt, msg);
    }
}

void
Chemistry::output (Log&) const
{ }

void
Chemistry::load_syntax (Syntax& syntax, AttributeList& alist)
{ }

Chemistry::Chemistry (Block& al)
  : ModelAListed (al.alist ())
{ }

Chemistry::~Chemistry ()
{ }

static Librarian Chemistry_init (Chemistry::component, "\
Pesticides and other chemicals.");

// chemistry.C ends here.
