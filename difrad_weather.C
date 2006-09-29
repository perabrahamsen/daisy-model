// difrad_weather.C -- Diffuse radiation using weather data.
// 
// Copyright 2006 Birgitte Gjettermann and KVL
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
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.See the
// GNU Lesser Public License for more details.
// 
// You should have received a copy of the GNU Lesser Public License
// along with Daisy; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA


#include "difrad.h"
#include "weather.h"
#include "mathlib.h"
#include <sstream>

struct DifradWeather : public Difrad
{
  // Simulation.
  double value (const Time&, const Weather& weather, Treelog& msg)
  {
    const double gb = weather.hourly_global_radiation ();
    const double df = weather.hourly_diffuse_radiation ();
    if (df > gb)
      {
	std::ostringstream tmp;
	tmp << "Diffuse radiation (" << df << ") > global radiation (" 
	    << gb << ")";
	msg.warning (tmp.str ());
	return 1.0;
      }
    if(!std::isnormal (gb))
      return 0.0;
    return df/gb;
  }

  void output (Log& log) const
  {
    Difrad::output (log);
  }

  // Create.
  DifradWeather (Block& al)
    : Difrad (al)
  { }
};

static struct DifradWeatherSyntax
{
  static Difrad&
  make (Block& al)
  { return *new DifradWeather (al); }
  DifradWeatherSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", 
	       "Diffuse radiation using weather data.");
    Difrad::load_syntax (syntax, alist);
    Librarian<Difrad>::add_type ("weather", alist, syntax, &make);
  }
} DifradWeather_syntax;
