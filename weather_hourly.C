// weather_hourly.C

#include "weather.h"
#include "time.h"
#include "syntax.h"
#include "alist.h"
#include "common.h"
#include "log.h"
#include <fstream.h>

struct WeatherHourly : public Weather
{
  Time date;
  const string file_name;
  ifstream file;
  int line;			// Current line number in weather file.

  // Read values.
  double precipitation;
  double global_radiation;
  double air_temperature;
  double cloudiness_;
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
  double cloudiness () const
    { return cloudiness_; }
  double vapor_pressure () const
    { return vapor_pressure_; }
  double wind () const 
    { return wind_; }

  WeatherHourly (const AttributeList& al)
    : Weather (al),
      date (42, 1, 1, 0),
      file_name (al.name ("file")),
      file (Options::find_file (al.name ("file"))),
      line (0),
      precipitation (-42.42e42),
      global_radiation (-42.42e42),
      air_temperature (-42.42e42),
      cloudiness_ (-42.42e42),
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
  Weather::tick (time);

  if (!file.good ())
    {
      CERR << file_name << ":" << line << ": file error";
      THROW ("read error");
    }
  int year;
  int month; 
  int day;
  int hour;

  while (date < time)
    {
      file >> year >> month >> day >> hour
	   >> global_radiation >> air_temperature >> precipitation
	   >> cloudiness_ >> vapor_pressure_ >> wind_;
      if (year < 100)
	year += 1900;
      while (file.good () && file.get () != '\n')
	/* do nothing */;

      if (!file.good ())
	THROW ("No more climate data.");

      date = Time (year, month, day, hour);

      assert (global_radiation >= 0 && global_radiation < 1400);
      assert (air_temperature >= -70 && air_temperature < 60);
      assert (precipitation >= 0 && precipitation < 300);
      assert (cloudiness_ >= 0 && cloudiness_ <= 1);
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
}

static struct WeatherHourlySyntax
{
  static Weather& make (const AttributeList& al)
    { return *new WeatherHourly (al); }

  WeatherHourlySyntax ()
    { 
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      Weather::load_syntax (syntax, alist);
      syntax.add ("file", Syntax::String, Syntax::Const);
      syntax.order ("file");
      Librarian<Weather>::add_type ("hourly", alist, syntax, &make);
    }
} WeatherHourly_syntax;
