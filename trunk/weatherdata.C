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
#include "time.h"
#include "units.h"
#include "attribute.h"
#include "assertion.h"
#include "mathlib.h"
#include "treelog.h"
#include "check.h"
#include "vcheck.h"
#include "librarian.h"
#include "frame.h"
#include <sstream>
#include <map>

namespace Weatherdata
{
  symbol reference_name ()
  { static const symbol name ("reference"); return name; }

  symbol field_name ()
  { static const symbol name ("field"); return name; }

  surface_t symbol2surface (const symbol key)
  {
    if (key == reference_name ())
      return reference;
    if (key == field_name ())
      return field;
    daisy_notreached ();
  }

  symbol surface2symbol (surface_t surface)
  {
    switch (surface)
      {
      case reference:
        return reference_name ();
      case field:
        return field_name ();
      default:
        daisy_notreached ();
      }
  }

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

  // Stationary symbols.
  symbol Latitude ()
  { static const symbol name ("Latitude"); return name; }
  
  symbol Longitude ()
  { static const symbol name ("Longitude"); return name; }
  
  symbol Elevation ()
  { static const symbol name ("Elevation"); return name; }
  
  symbol TimeZone ()
  { static const symbol name ("TimeZone"); return name; }
  
  symbol ScreenHeight ()
  { static const symbol name ("ScreenHeight"); return name; }
  
  symbol TAverage ()
  { static const symbol name ("TAverage"); return name; }
  
  symbol TAmplitude ()
  { static const symbol name ("TAmplitude"); return name; }
  
  symbol MaxTDay ()
  { static const symbol name ("MaxTDay"); return name; }
  
  symbol NH4WetDep ()
  { static const symbol name ("NH4WetDep"); return name; }
  
  symbol NH4DryDep ()
  { static const symbol name ("NH4DryDep"); return name; }
  
  symbol NO3WetDep ()
  { static const symbol name ("NO3WetDep"); return name; }
  
  symbol NO3DryDep ()
  { static const symbol name ("NO3DryDep"); return name; }
  
  symbol Deposition ()
  { static const symbol name ("Deposition"); return name; }
  
  symbol DepDry ()
  { static const symbol name ("DepDry"); return name; }
  
  symbol DepDryNH4 ()
  { static const symbol name ("DepDryNH4"); return name; }
  
  symbol DepWetNH4 ()
  { static const symbol name ("DepWetNH4"); return name; }
  
  symbol PAverage ()
  { static const symbol name ("PAverage"); return name; }
  
  symbol Station ()
  { static const symbol name ("Station"); return name; }
  
  symbol Note ()
  { static const symbol name ("Note"); return name; }
  
  symbol Surface ()
  { static const symbol name ("Surface"); return name; }
  
  symbol PrecipCorrect ()
  { static const symbol name ("PrecipCorrect"); return name; }
  
  symbol PrecipScale ()
  { static const symbol name ("PrecipScale"); return name; }
  
  symbol TempOffset ()
  { static const symbol name ("TempOffset"); return name; }
  
  symbol Begin ()
  { static const symbol name ("Begin"); return name; }
  
  symbol End ()
  { static const symbol name ("End"); return name; }
  
  symbol Timestep ()
  { static const symbol name ("Timestep"); return name; }
  

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
      (*this)["GlobRad"] = DDT ("Global radiation.", "W/m^2", 0, 1400);
      (*this)["AirTemp"] = DDT ("Air temperature.", "dgC", -70, 60);
      (*this)["T_min"] = DDT ("Minimum air temperature.", "dgC", -70, 60);
      (*this)["T_max"] = DDT ("Maximum air temperature.", "dgC", -70, 60);
      (*this)["Precip"] = DDT ("Precipitation.", "mm/h", 0, 300);
      (*this)["RefEvap"] = DDT ("\
Reference evapotranspiration.", "mm/h", -10, 20);
      (*this)["VapPres"] = DDT ("Vapor pressure.", "Pa", 0, 5000);
      (*this)["DiffRad"] = DDT ("Diffuse radiation.", "W/m^2", 0, 1400);
      (*this)["RelHum"] = DDT ("Relative humidity.", "fraction", 0, 5000);
      (*this)["Wind"] = DDT ("Wind speed.", "m/s", 0, 40);
      (*this)["Latitude"] = DDT ("\
Location of station (north-south).", "dgNorth", -90, 90);
      (*this)["Longitude"] = DDT ("\
Location of station (east-west).", "dgEast", -360, 360);
      (*this)["Elevation"] = DDT ("\
Station altitude over sea level.", "m", 0, 10000);
      (*this)["TimeZone"] = DDT ("Time zone.", "dgEast", -360, 360);
      (*this)["ScreenHeight"] = DDT ("\
Measurement altitude over ground level.", "m", 0, 100);
      (*this)["TAverage"] = DDT ("\
Yearly average temperature.", "dgC", -10, 40);
      (*this)["TAmplitude"] = DDT ("\
Typical temperature variation over the seasons.\n\
If you fit the daily average temperature over a year to a sinus curve,\
this would be the amplitude.", "dgC", 0, 100);
      (*this)["MaxTDay"] = DDT ("\
Typical day where the temperature is highest.\n                         \
If you fit the daily average temperature over a year to a sinus curve,\
this would be maximum point.", "yday", 1, 365);
      (*this)["NH4WetDep"] = DDT ("\
NH4 concentration in precipitation.", Units::ppm (), 0, 100);
      (*this)["NH4DryDep"] = DDT ("\
Dry deposition of NH4.", "kg N/ha/y", 0, 100);
      (*this)["NO3WetDep"] = DDT ("\
NO3 concentration in precipitation.", Units::ppm (), 0, 100);
      (*this)["NO3DryDep"] = DDT ("\
Dry deposition of NO3.", "kg N/ha/y", 0, 100);
      (*this)["Deposition"] = DDT ("Total N deposition.", "kg N/ha/y", 0, 100);
      (*this)["DepDry"] = DDT ("\
Fraction of total N deposition that is dry.", Attribute::Fraction (), 0, 1);
      (*this)["DepDryNH4"] = DDT ("\
NH4 fraction of dry deposition.", Attribute::Fraction (), 0, 1);
      (*this)["DepWetNH4"] = DDT ("\
NH4 fraction of wet deposition.", Attribute::Fraction (), 0, 1);
      (*this)["PAverage"] = DDT ("\
Average precipitation.  Used for deviding precipitation into dry and wet.",
                                 "mm", 0.01, 3000);
      (*this)["Timestep"] = DDT ("Timestep for weather data.",
                                 "h", 
                                 1.0 / (60.0 * 60.0 * 1000000.0), // 1 [us]
                                 24 * 365.2425 * 1000.0);         // 1000 [y]
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

  symbol meta_key (const symbol meta)
  {
    static struct meta_map_t : public std::map<symbol, symbol>
    {
      meta_map_t ()
      {
        (*this)["Latitude"] = "GlobRad";
        (*this)["Longitude"] = "GlobRad";
        (*this)["Elevation"] = "GlobRad"; // Used for diff. rad.
        (*this)["TimeZone"] = "GlobRad";
        (*this)["ScreenHeight"] = "Wind";
        (*this)["TAverage"] = "AirTemp";
        (*this)["TAmplitude"] = "AirTemp";
        (*this)["MaxTDay"] = "AirTemp";
        (*this)["Surface"] = "GlobRad"; // Used for ref.evap.
        (*this)["PrecipCorrect"] = "Precip";
        (*this)["PrecipScale"] = "Precip";
        (*this)["TempOffset"] = "AirTemp";
      }
    } meta_map;

    const meta_map_t::const_iterator i = meta_map.find (meta);
    if (i == meta_map.end ())
      return Attribute::None ();
    return i->second;
  }

  void load_syntax (Frame& frame)
  {
    //Numbers.
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

    // Other stuf.
    frame.declare_string (Station (), Attribute::OptionalConst, "\
Name of weather station.");
    frame.declare_text (Note (), Attribute::OptionalConst, "\
Note regarding this set of data.");
    frame.declare_string (Surface (), Attribute::OptionalConst, "\
Surface type.\n\
Either 'reference' for a weather station standard of short grass,\n\
or 'field' for measurements directly at the field.");
    static VCheck::Enum surfaces (reference_name (), field_name ());
    frame.set_check (Surface (), surfaces);
    static VCheck::MultiSize multi (1, 12);
    frame.declare (PrecipCorrect (), Attribute::None (), 
                   Attribute::OptionalConst, Attribute::Variable, "\
Correction factors for precipitation.\n\
Can contain one or twelve numbers, in the later case the numbers\n\
corresponds to months.");
    frame.set_check (PrecipCorrect (), multi);
    frame.declare (PrecipScale (), Attribute::None (), 
                   Attribute::OptionalConst, Attribute::Variable, "\
Scale factors for precipitation.\n\
Can contain one or twelve numbers, in the later case the numbers\n\
corresponds to months.");
    frame.set_check (PrecipScale (), multi);
    frame.declare (TempOffset (), "dg C", 
                   Attribute::OptionalConst, Attribute::Variable, "\
Temperature offset.\n\
Can contain one or twelve numbers, in the later case the numbers\n\
corresponds to months.");
    frame.set_check (TempOffset (), multi);
    frame.declare_submodule (Begin (), Attribute::OptionalConst, "\
Beginning of weather data.", Time::load_syntax);
    frame.declare_submodule (End (), Attribute::OptionalConst, "\
End of weather data.", Time::load_syntax);
  }
}

static DeclareSubmodel weatherdata_submodel (Weatherdata::load_syntax,
                                             "Weatherdata", "\
Weather data.");

// weatherdata.C ends here.
