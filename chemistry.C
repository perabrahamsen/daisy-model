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
#include "block_model.h"
#include "librarian.h"
#include "vcheck.h"
#include "units.h"

const char *const Chemistry::component = "chemistry";

symbol 
Chemistry::library_id () const
{
  static const symbol id (component);
  return id;
}

void 
Chemistry::deposit (const IM& im, Treelog& msg)
{ 
  const Unit& unit = units.get_unit (Chemical::surface_flux_unit ());
  for (IM::const_iterator i = im.begin (); i != im.end (); i++)
    {
      const symbol chem = *i;
      const double flux = im.get_value (chem, unit);
      deposit (chem, flux, msg);
    }
}

void 
Chemistry::spray_overhead (const IM& im, Treelog& msg)
{ 
  const Unit& unit = units.get_unit (Chemical::surface_storage_unit ());
  for (IM::const_iterator i = im.begin (); i != im.end (); i++)
    {
      const symbol chem = *i;
      const double amount = im.get_value (chem, unit);
      spray_overhead (chem, amount, msg);
    }
}

void 
Chemistry::spray_surface (const IM& im, Treelog& msg)
{ 
  const Unit& unit = units.get_unit (Chemical::surface_storage_unit ());
  for (IM::const_iterator i = im.begin (); i != im.end (); i++)
    {
      const symbol chem = *i;
      const double amount = im.get_value (chem, unit);
      spray_surface (chem, amount, msg);
    }
}

void 
Chemistry::incorporate (const Geometry& geo, const IM& im, 
			const double from, const double to,
			Treelog& msg)
{
  const Unit& unit = units.get_unit (Chemical::surface_storage_unit ());
  for (IM::const_iterator i = im.begin (); i != im.end (); i++)
    {
      const symbol chem = *i;
      const double amount = im.get_value (chem, unit);
      incorporate (geo, chem, amount, from, to, msg);
    }
}

void 
Chemistry::incorporate (const Geometry& geo, const IM& im, 
			const Volume& volume, Treelog& msg)
{
  const Unit& unit = units.get_unit (Chemical::surface_storage_unit ());
  for (IM::const_iterator i = im.begin (); i != im.end (); i++)
    {
      const symbol chem = *i;
      const double amount = im.get_value (chem, unit);
      incorporate (geo, chem, amount, volume, msg);
    }
}

void
Chemistry::output (Log&) const
{ }

Chemistry::Chemistry (const BlockModel& al)
  : ModelFramed (al),
    units (al.units ())
{ }

Chemistry::~Chemistry ()
{ }

static struct ChemistryInit : public DeclareComponent 
{
  ChemistryInit ()
    : DeclareComponent (Chemistry::component, "\
Pesticides and other chemicals.")
  { }
  void load_frame (Frame& frame) const
  { Model::load_model (frame); }
} Chemistry_init;

// chemistry.C ends here.
