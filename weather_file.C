// weather_file.C

#include "weather_old.h"
#include "time.h"
#include "log.h"
#include <fstream.h>

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
  void tick (const Time&);

  // Communication with Bioclimate.
  double daily_air_temperature (void) const // [°C]
    { return air_temperature; }
  double reference_evapotranspiration () const // [mm/h]
    { 
      if (reference_evapotranspiration_ < -1.0e11)
	return WeatherOld::reference_evapotranspiration ();
      return reference_evapotranspiration_ * day_cycle (); 
    }

  // Communication with external model.
  void put_precipitation (double prec)
    { WeatherOld::distribute (prec / 24.0); }
  void put_air_temperature (double T)
    { air_temperature = T; }
  void put_reference_evapotranspiration (double ref)
    { reference_evapotranspiration_ = ref; }

  // Create and Destroy.
  WeatherFile (const AttributeList& al)
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
WeatherFile::tick (const Time& time)
{ 
  WeatherOld::tick (time);

  if (!(date < time))
    return;

  if (!file.good ())
    {
      CERR << file_name << ":" << line << ": file error";
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

      assert (global_radiation >= 0 && global_radiation < 700);
      assert (air_temperature >= -70 && air_temperature < 60);
      assert (precipitation >= 0 && precipitation < 1000);
      assert (reference_evapotranspiration_ <= 20);
    }
  assert (time.year () == date.year ());
  assert (time.month () == date.month ());
  assert (time.mday () == date.mday ());

  // Update the daily values.
  put_global_radiation (global_radiation);

  // Hourly value.
  distribute (precipitation / 24.0);

  Weather::tick_after (time);
}

static struct WeatherFileSyntax
{
  static Weather& make (const AttributeList& al)
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
      Librarian<Weather>::add_type ("file", alist, syntax, &make);
    }
} WeatherFile_syntax;
