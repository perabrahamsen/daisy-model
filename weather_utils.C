// weather_utils.C -- Weather related utilities.
// 
// Copyright 2010 KU
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

#include "weather_utils.h"
#include "attribute.h"
#include "assertion.h"
#include "mathlib.h"
#include <map>

namespace Weatherdata
{
  symbol GlobRad ()
  { static const symbol name ("GlobRad"); return name; }

  symbol AirTemp ()
  { static const symbol name ("AirTemp"); return name; }

  symbol T_min ()
  { static const symbol name ("T_min"); return name; }

  symbol T_max ()
  { static const symbol name ("T_max"); return name; }

  symbol Precip ()
  { static const symbol name ("Precip"); return name; }

  symbol RefEvap ()
  { static const symbol name ("RefEvap"); return name; }

  symbol VapPres ()
  { static const symbol name ("VapPres"); return name; }

  symbol DiffRad ()
  { static const symbol name ("DiffRad"); return name; }

  symbol RelHum ()
  { static const symbol name ("RelHum"); return name; }

  symbol Wind ()
  { static const symbol name ("Wind"); return name; }

  struct DDT
  {
    symbol description;
    symbol dimension;
    double min_value;
    double max_value;

    DDT (const symbol de, const symbol di, const double mi, const double ma)
      : description (de),
        dimension (di),
        min_value (mi),
        max_value (ma)
    { }
    DDT ()
      : description ("dummy"),
        dimension (Attribute::Unknown ()),
        min_value (NAN),
        max_value (NAN)
    { }
  };
  
  static const struct data_description_map : public std::map<symbol, DDT>
  {
    data_description_map ()
    {
      (*this)["GlobRad"] = DDT ("", "W/m^2", 0, 1400);
      (*this)["AirTemp"] = DDT ("", "dgC", -70, 60);
      (*this)["T_min"] = DDT ("", "dgC", -70, 60);
      (*this)["T_max"] = DDT ("", "dgC", -70, 60);
      (*this)["Precip"] = DDT ("", "mm/h", 0, 300);
      (*this)["RefEvap"] = DDT ("", "mm/h", -10, 20);
      (*this)["VapPres"] = DDT ("", "Pa", 0, 5000);
      (*this)["DiffRad"] = DDT ("", "W/m^2", 0, 1400);
      (*this)["RelHum"] = DDT ("", "fraction", 0, 5000);
      (*this)["Wind"] = DDT ("", "m/s", 0, 40);
    };
  } DD;
 
  symbol unit (const symbol name)
  {
    const data_description_map::const_iterator i = DD.find (name);
    if (i == DD.end ())
      return Attribute::Unknown ();
    
    return (*i).second.dimension;
  }

  symbol description (const symbol name)
  {
    const data_description_map::const_iterator i = DD.find (name);
    daisy_assert (i != DD.end ());
    return (*i).second.description;
  }

  double min_value (const symbol name)
  {
    const data_description_map::const_iterator i = DD.find (name);
    daisy_assert (i != DD.end ());
    return (*i).second.min_value;
  }

  double max_value (const symbol name)
  {
    const data_description_map::const_iterator i = DD.find (name);
    daisy_assert (i != DD.end ());
    return (*i).second.max_value;
  }
}

// weather_utils.C ends here.
