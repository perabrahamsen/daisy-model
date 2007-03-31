// weather_file.C
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
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


#include "weather_old.h"
#include "time.h"
#include "log.h"
#include "librarian.h"
#include <fstream>

struct WeatherFile : public WeatherOld
{
  // Weather file.
  Time date;
  const string file_name;
  ifstream file;
  int line;			// Current line number in weather file.

  // Values.
  double precipitation;
  double reference_evapotranspiration_;
  double air_temperature;

  // Simulation.
  void tick (const Time&, Treelog&);

  // Communication with Bioclimate.
  double daily_air_temperature (void) const // [°C]
    { return air_temperature; }
  double reference_evapotranspiration () const // [mm/h]
    { 
      if (reference_evapotranspiration_ < -1.0e11)
	return WeatherOld::reference_evapotranspiration ();
      return reference_evapotranspiration_ * day_cycle (); 
    }

  // Create and Destroy.
  WeatherFile (Block& al)
    : WeatherOld (al),
      date (42, 1, 1, 0),
      file_name (al.name ("file")),
      file (Options::find_file (al.name ("file"))),
      line (0),
      precipitation (-42.42e42),
      reference_evapotranspiration_ (-42.42e42),
      air_temperature (-42.42e42)
    { }
  ~WeatherFile ()
    { 
#if 0
      // Code guard claims the file handle is bad.
      close (file.rdbuf ()->fd ()); 
#endif
    }
};

void
WeatherFile::tick (const Time& time, Treelog& out)
{ 
  WeatherOld::tick (time, out);

  if (!(date < time))
    return;

  if (!file.good ())
    {
      std::ostringstream tmp;
      tmp << file_name << ":" << line << ": file error";
      out.error (tmp.str ());
      throw ("read error");
    }
  int year;
  int month; 
  int day;
  int end;

  double global_radiation;

  while (date < time)
    {
      file >> year >> month >> day
	   >> global_radiation >> air_temperature >> precipitation;
      end = file.get ();
      if (year < 100)
	year += 1900;
      while (file.good () && strchr (" \t", end))
	end = file.get ();
      
      if (!file.good ())
	throw ("No more climate data.");

      if (end == '\n')
	reference_evapotranspiration_ = -42.42e42;
      else
	{
	  // BCC wants this:
	  file.putback ((char) end);
	  // G++ used this:
	  // file.unget ();
	  file >> reference_evapotranspiration_;
	  while (file.good () && file.get () != '\n')
	    /* do nothing */;
	}
      date = Time (year, month, day, 23);

      daisy_assert (global_radiation >= 0 && global_radiation < 700);
      daisy_assert (air_temperature >= -70 && air_temperature < 60);
      daisy_assert (precipitation >= 0 && precipitation < 1000);
      daisy_assert (reference_evapotranspiration_ <= 20);
    }
  daisy_assert (time.year () == date.year ());
  daisy_assert (time.month () == date.month ());
  daisy_assert (time.mday () == date.mday ());

  // Update the daily values.
  put_global_radiation (global_radiation);

  // Hourly value.
  distribute (precipitation / 24.0);

  Weather::tick_after (time, out);
}

static struct WeatherFileSyntax
{
  static Model& make (Block& al)
    { return *new WeatherFile (al); }

  WeatherFileSyntax ()
    { 
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      alist.add ("description", "Read weather data from a file.\n\
Each line should have the following whitespace separated fields:\n\
year, month, day, global radiation [W/m^2], air temperature [dg C],\n\
precipitation [mm/d], and reference evapotranspiration [mm/d].  The\n\
last field is optional, it is only used if you select the 'weather'\n\
model in the 'pet' component");
      WeatherOld::load_syntax (syntax, alist);
      syntax.add ("file", Syntax::String, Syntax::Const,
		  "File to read weather data from.");
      syntax.order ("file");
      Librarian::add_type (Weather::component, "file", alist, syntax, &make);
    }
} WeatherFile_syntax;
