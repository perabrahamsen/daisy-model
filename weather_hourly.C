// weather_hourly.C

#include "weather_old.h"
#include "time.h"
#include "log.h"
#include "mathlib.h"
#include <fstream.h>

struct WeatherHourly : public WeatherOld
{
  Time date;
  const string file_name;
  ifstream file;
  int line;			// Current line number in weather file.

  // Read values.
  double precipitation;
  double global_radiation;
  double air_temperature;
  double vapor_pressure_;
  double wind_;

  // Accumulated values.
  double accumulated_global_radiation;
  double accumulated_air_temperature;
  unsigned int accumulated_count;
  double daily_air_temperature_;

  // Simulation.
  void tick (const Time&);

  // Communication with Bioclimate.
  double daily_air_temperature () const
    { return daily_air_temperature_; }
  double hourly_air_temperature () const
    { return air_temperature; }
  double hourly_global_radiation () const
    { return global_radiation; }
  double vapor_pressure () const
    { return vapor_pressure_; }
  double wind () const 
    { return wind_; }

  void put_air_temperature (double T)
    { air_temperature = T; }

  void put_reference_evapotranspiration (double)
    { }

  WeatherHourly (const AttributeList& al)
    : WeatherOld (al),
      date (42, 1, 1, 0),
      file_name (al.name ("file")),
      file (Options::find_file (al.name ("file"))),
      line (0),
      precipitation (-42.42e42),
      global_radiation (-42.42e42),
      air_temperature (-42.42e42),
      vapor_pressure_ (-42.42e42),
      wind_ (-42.42e42),
      accumulated_global_radiation (0.0),
      accumulated_air_temperature (0.0),
      accumulated_count (0),
      daily_air_temperature_ (-42.42e42)
    { }

  ~WeatherHourly ()
    { close (file.rdbuf ()->fd ()); }
};

void
WeatherHourly::tick (const Time& time)
{ 
  WeatherOld::tick (time);

  if (!file.good ())
    {
      CERR << file_name << ":" << line << ": file error";
      throw ("read error");
    }
  int year;
  int month; 
  int day;
  int hour;

  while (date < time)
    {
      double cloudiness_;

      file >> year >> month >> day >> hour
	   >> global_radiation >> air_temperature >> precipitation
	   >> cloudiness_ >> vapor_pressure_ >> wind_;
      if (year < 100)
	year += 1900;
      while (file.good () && file.get () != '\n')
	/* do nothing */;

      if (!file.good ())
	throw ("No more climate data.");

      date = Time (year, month, day, hour);

      assert (global_radiation >= 0 && global_radiation < 1400);
      assert (air_temperature >= -70 && air_temperature < 60);
      assert (precipitation >= 0 && precipitation < 300);
      assert (cloudiness_ >= 0 && cloudiness_ <= 1);
      if (!approximate (cloudiness_, hourly_cloudiness ()))
	CERR << "Warning: claudiness read (" << cloudiness_ 
	     << ") != calculated (" << hourly_cloudiness () << ")\n";
      assert (vapor_pressure_ >= 0 && vapor_pressure_ <= 5000);
      assert (wind_ >= 0 && wind_ <= 40);

      accumulated_global_radiation += global_radiation;
      accumulated_air_temperature += air_temperature;
      accumulated_count++;	// BUGLET: We ignore missing values.

      if (hour == 0)
	{
	  put_global_radiation (accumulated_global_radiation 
				/ accumulated_count);
	  daily_air_temperature_ 
	    = accumulated_air_temperature / accumulated_count;
	  accumulated_global_radiation = 0.0;
	  accumulated_air_temperature = 0.0;
	  accumulated_count = 0;
	}
    }

  // Update the hourly values.
  distribute (precipitation);

  Weather::tick_after (time);
}

static struct WeatherHourlySyntax
{
  static Weather& make (const AttributeList& al)
    { return *new WeatherHourly (al); }

  WeatherHourlySyntax ()
    { 
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      alist.add ("description", "Read weather data from a file.\n\
Each line should have the following whitespace separated fields:\n\
year, month, day, hour, global radiation [W/m^2], air temperature [dg C],\n\
precipitation [mm/h], cloudiness [0-1] and vapor pressure [Pa].");
      WeatherOld::load_syntax (syntax, alist);
      syntax.add ("file", Syntax::String, Syntax::Const,
		  "File to read weather data from.");
      syntax.order ("file");
      Librarian<Weather>::add_type ("hourly", alist, syntax, &make);
    }
} WeatherHourly_syntax;
