// weather_file.C

#include "weather.h"
#include "time.h"
#include "options.h"
#include "syntax.h"
#include "alist.h"
#include "common.h"
#include "log.h"
#include <fstream.h>

struct WeatherFile : public Weather
{
  Time date;
  const string file_name;
  ifstream file;

  double precipitation;
  double reference_evapotranspiration_;
  double hourly_reference_evapotranspiration;
  double global_radiation;
  double air_temperature;
  
  int line;			// Current line number in weather file.

  // Simulation.
  void tick (const Time&);

  // Communication with Bioclimate.
  double daily_air_temperature () const;
  double daily_global_radiation () const;
  double reference_evapotranspiration () const;
  double Precipitation () const;

  // Communication with external model.
  void put_precipitation (double prec);// [mm/d]
  void put_air_temperature (double T); // [°C]
  void put_reference_evapotranspiration (double ref); // [mm/d]

  // Create and Destroy.
  WeatherFile (const AttributeList&);
  ~WeatherFile ();
};

void
WeatherFile::tick (const Time& time)
{ 
  Weather::tick (time);

  if (!file.good ())
    {
      cerr << file_name << ":" << line << ": file error";
      THROW ("read error");
    }
  int year;
  int month; 
  int day;
  char end;

  while (date < time)
    {
      file >> year >> month >> day
	   >> global_radiation >> air_temperature >> precipitation >> end;
      if (year < 100)
	year += 1900;
      while (file.good () && strchr ("\n \t", end))
	file >> end;
      
      if (!file.good ())
	THROW ("No more climate data.");

      if (end == '\n')
	reference_evapotranspiration_ = -42.42e42;
      else
	{
	  // BCC wants this:
	  file.putback (end);
	  // G++ used this:
	  // file.unget ();
	  file >> reference_evapotranspiration_;
	  while (file.good () && file.get () != '\n')
	    /* do nothing */;
	}
      date = Time (year, month, day, 23);
    }

  // Update the hourly values.
  put_reference_evapotranspiration (reference_evapotranspiration_);
  distribute (Precipitation ());
}

double
WeatherFile::daily_air_temperature (void) const // [°C]
{
  return air_temperature;
}

double
WeatherFile::daily_global_radiation () const // [W/m²]
{
  return global_radiation;
}

double
WeatherFile::reference_evapotranspiration () const // [mm/h]
{
  return hourly_reference_evapotranspiration;
}

double
WeatherFile::Precipitation () const
{
  return precipitation / 24.0;
}

void 
WeatherFile::put_precipitation (double prec)
{
  precipitation = prec;
  Weather::distribute (Precipitation ());
}

void 
WeatherFile::put_air_temperature (double T)
{
  air_temperature = T;
}

void 
WeatherFile::put_reference_evapotranspiration (double ref)
{
  reference_evapotranspiration_ = ref;

  if (reference_evapotranspiration_ < -1.0e11)
    {
      hourly_reference_evapotranspiration 
	= Weather::reference_evapotranspiration ();
    }
  else
    hourly_reference_evapotranspiration 
      = reference_evapotranspiration_ * day_cycle ();
}

WeatherFile::WeatherFile (const AttributeList& al)
  : Weather (al),
    date (42, 1, 1, 0),
    file_name (al.name ("file")),
    file (Options::find_file (al.name ("file"))),
    precipitation (-42.42e42),
    reference_evapotranspiration_ (-42.42e42),
    global_radiation (-42.42e42)
{ }

WeatherFile::~WeatherFile ()
{
  close (file.rdbuf ()->fd ());
}

static struct WeatherFileSyntax
{
  static Weather& make (const AttributeList& al)
    { return *new WeatherFile (al); }

  WeatherFileSyntax ()
    { 
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      Weather::load_syntax (syntax, alist);
      syntax.add ("file", Syntax::String, Syntax::Const);
      syntax.order ("file");
      Librarian<Weather>::add_type ("file", alist, syntax, &make);
    }
} WeatherFile_syntax;
