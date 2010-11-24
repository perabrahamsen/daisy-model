// weatherdata.C -- Weather related utilities.
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

#include "weatherdata.h"
#include "attribute.h"
#include "assertion.h"
#include "mathlib.h"
#include "treelog.h"
#include "check.h"
#include "librarian.h"
#include "frame.h"
#include <sstream>
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

    struct InCheck : public Check
    {
      double min_value;
      double max_value;

      bool verify (const double value, Treelog& msg) const
      { 
        if (value < min_value || value > max_value)
          {
            std::ostringstream tmp;
            tmp << "'" << value << "' is outside the interval ["
                << min_value << ":" << max_value << "]";
            msg.warning (tmp.str ());
          }
        return true; 
      }
      
      InCheck (const double mi, const double ma)
        : min_value (mi),
          max_value (ma)
      { }
    };
    boost::shared_ptr<InCheck> in_check;

    DDT (const symbol de, const symbol di, const double mi, const double ma)
      : description (de),
        dimension (di),
        in_check (new InCheck (mi, ma))
        
    { }
    DDT ()
      : description ("dummy"),
        dimension (Attribute::Unknown ()),
        in_check ()
    { }
  };
  
  static const struct data_description_map : public std::map<symbol, DDT>
  {
    data_description_map ()
    {
      (*this)["GlobRad"] = DDT ("Global radiation", "W/m^2", 0, 1400);
      (*this)["AirTemp"] = DDT ("Air temperature", "dgC", -70, 60);
      (*this)["T_min"] = DDT ("Minimum air temperature", "dgC", -70, 60);
      (*this)["T_max"] = DDT ("Maximum air temperature", "dgC", -70, 60);
      (*this)["Precip"] = DDT ("Precipitation", "mm/h", 0, 300);
      (*this)["RefEvap"] = DDT ("Reference evapotranspiration", "mm/h", -10, 20);
      (*this)["VapPres"] = DDT ("Vapor pressure", "Pa", 0, 5000);
      (*this)["DiffRad"] = DDT ("Diffuse radiation", "W/m^2", 0, 1400);
      (*this)["RelHum"] = DDT ("Relative humidity", "fraction", 0, 5000);
      (*this)["Wind"] = DDT ("Wind speed", "m/s", 0, 40);
    };
  } DD;
 
  symbol dimension (const symbol name)
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
    daisy_assert ((*i).second.in_check.get ());
    return (*i).second.in_check->min_value;
  }

  double max_value (const symbol name)
  {
    const data_description_map::const_iterator i = DD.find (name);
    daisy_assert (i != DD.end ());
    daisy_assert ((*i).second.in_check.get ());
    return (*i).second.in_check->max_value;
  }

  void load_syntax (Frame& frame)
  {
    for (data_description_map::const_iterator i = DD.begin ();
         i != DD.end ();
         i++)
      {
        const symbol key = (*i).first;
        const Check *const check = (*i).second.in_check.get ();
        if (check)
          frame.declare (key, dimension (key), *check, Attribute::OptionalConst,
                         description (key));
        else
          frame.declare (key, dimension (key), Attribute::OptionalConst,
                         description (key));
      }
  }
}

static DeclareSubmodel weatherdata_submodel (Weatherdata::load_syntax,
                                             "Weatherdata", "\
Weather data.");

// weatherdata.C ends here.
