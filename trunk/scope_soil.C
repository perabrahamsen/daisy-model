// scope_soil.C --- Look up stuff in the soil.
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

#include "scope_soil.h"
#include "soil.h"
#include "soil_water.h"
#include "soil_heat.h"
#include "units.h"
#include "syntax.h"
#include "alist.h"
#include "assertion.h"
#include "librarian.h"
#include <sstream>

const symbol 
ScopeSoil::humus ("humus");

const symbol 
ScopeSoil::h ("h");

const symbol 
ScopeSoil::Theta ("Theta");

const symbol 
ScopeSoil::T ("T");

void 
ScopeSoil::set_cell (size_t c)
{ cell = c; }

const std::vector<symbol>& 
ScopeSoil::all_numbers () const
{ return all_numbers_; }

bool 
ScopeSoil::has_number (const symbol tag) const
{
  daisy_assert (cell >= 0);

  if (tag == humus || tag == h || tag == Theta || tag == T)
    return true;

  if (soil.has_attribute (cell, tag))
    return true;
  
  std::ostringstream tmp;
  tmp << "Attribute '" << tag << "' missing in cell " << cell;
  Assertion::warning (tmp.str ());
  
  return false;
}

double
ScopeSoil::number (const symbol tag) const
{
  daisy_assert (cell >= 0);

  if (tag == humus)
    return soil.humus (cell);
  if (tag == h)
    return soil_water.h (cell);
  if (tag == Theta)
    return soil_water.Theta (cell);
  if (tag == T)
    return soil_heat.T (cell);

  return soil.get_attribute (cell, tag);
}    

symbol 
ScopeSoil::dimension (const symbol tag) const
{
  if (tag == humus)
    return Syntax::fraction ();
  if (tag == h)
    return Units::cm ();
  if (tag == Theta)
    return Syntax::none ();

  static const symbol T_dim ("dg C");
  if (tag == T)
    return T_dim;

  daisy_assert (cell >= 0);
  return soil.get_dimension (cell, tag);
}

symbol
ScopeSoil::get_description (symbol tag) const
{
  static const symbol humus_description ("Humus fraction of dry matter.");
  if (tag == humus)
    return humus_description;

  static const symbol h_description ("Soil hydraulic pressure.");
  if (tag == h)
    return h_description;

  static const symbol Theta_description ("Soil water content.");
  if (tag == Theta)
    return Theta_description;

  static const symbol T_description ("Soil temperatur.");
  if (tag == T)
    return T_description;

  static const symbol description ("Soil attribute.");
  return description;
}

std::vector<symbol>
ScopeSoil::find_numbers (const Soil&)
{
  std::vector<symbol> result;
  result.push_back (humus);
  result.push_back (h);
  result.push_back (Theta);
  result.push_back (T);
  return result;
}

ScopeSoil::ScopeSoil (const Soil& s, const SoilWater& sw, const SoilHeat& sh)
  : Scope ("soil"),
    soil (s),
    soil_water (sw),
    soil_heat (sh),
    all_numbers_ (find_numbers (soil)),
    cell (-1)
{ }

ScopeSoil::~ScopeSoil ()
{ }

// scope_soil.C ends here.
