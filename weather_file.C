// weather_file.C

#include "weather.h"
#include "options.h"
#include "syntax.h"
#include "alist.h"
#include "common.h"
#include "log.h"
#include <algobase.h>
#include <fstream.h>
#include "mike_she.h"

class WeatherFile : public Weather
{
  Time date;
  const string file_name;
  ifstream file;
  const double T1;
  const double T2;
  const vector<double>& A;
  const vector<double>& B;

#ifdef MIKE_SHE
#define MUTABLE mutable
#else
#define MUTABLE
#endif

  MUTABLE double precipitation;
  MUTABLE double reference_evapotranspiration;
  mutable double hourly_reference_evapotranspiration;
  double global_radiation;
  mutable double hourly_global_radiation;
  MUTABLE double air_temperature;

  mutable double Prain;
  mutable double Psnow;

  int line;
  // Simulation.
public:
  void tick ();
  void output (Log&, const Filter&) const;
  double AirTemperature () const;
  double GlobalRadiation () const;
  double ReferenceEvapotranspiration () const;
  double Precipitation () const;
  double Rain () const;
  double Snow () const;

  // Create and Destroy.
private:
  friend class WeatherFileSyntax;
  static Weather& make (const Time&, const AttributeList&);
  WeatherFile (const Time&, const AttributeList&);
public:
  ~WeatherFile ();
};

void
WeatherFile::tick ()
{ 
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
	reference_evapotranspiration = -42.42e42;
      else
	{
	  file.unget ();
	  file >> reference_evapotranspiration;
	  while (file.good () && file.get () != '\n')
	    /* do nothing */;
	}
      date = Time (year, month, day, 23);
    }
}

void
WeatherFile::output (Log& log, const Filter& filter) const
{
  log.output ("Prain", filter, Prain, true);	
  log.output ("Psnow", filter, Psnow, true);
  log.output ("air_temperature", filter, air_temperature, true);
  log.output ("global_radiation", filter, global_radiation, true);
  log.output ("hourly_global_radiation", filter, 
	      hourly_global_radiation, true);
  log.output ("reference_evapotranspiration", filter, 
	      reference_evapotranspiration, true);
  log.output ("hourly_reference_evapotranspiration", filter, 
	      hourly_reference_evapotranspiration, true);
}

double
WeatherFile::AirTemperature (void) const // [C]
{
#ifdef MIKE_SHE
  air_temperature = mike_she->get_air_temperature ();
#endif
  // BUG: No variation over the day? 
  return air_temperature;
}

double
WeatherFile::GlobalRadiation () const	// [W/m²]
{
  hourly_global_radiation = DayCycle () * 24.0 * global_radiation;
  return hourly_global_radiation;
}


double
WeatherFile::ReferenceEvapotranspiration () const // [mm/h]
{
#ifdef MIKE_SHE
  reference_evapotranspiration
    = mike_she->get_reference_evapotranspiration ();
#endif
  if (reference_evapotranspiration < -1.0e11)
    {
      const double T = 273.16 + AirTemperature ();
      const double Delta = 5362.7 / pow (T, 2) * exp (26.042 - 5362.7 / T);
      return 1.05e-3 * Delta / (Delta + 66.7) * GlobalRadiation ();
    }

  hourly_reference_evapotranspiration = 
    reference_evapotranspiration * DayCycle ();

  return hourly_reference_evapotranspiration;
}

double
WeatherFile::Precipitation () const
{

#ifdef MIKE_SHE
  precipitation = mike_she->get_precipitation ();
#endif
  return precipitation / 24.0;
}

double
WeatherFile::Rain () const
{
  Prain = Precipitation () - Snow ();

  return Prain;
}

double
WeatherFile::Snow () const
{
  if (AirTemperature () < T1)
    Psnow = Precipitation ();
  else if (T2 < AirTemperature ())
    Psnow = 0.0;
  else
    Psnow = Precipitation () * (T2 - AirTemperature ()) / (T2 - T1);

  return Psnow;
}

WeatherFile::WeatherFile (const Time& t, const AttributeList& al)
  : Weather (t, al.number ("Latitude"), al.name ("type")),
    date (42, 1, 1, 0),
    file_name (al.name ("file")),
    file (Options::find_file (al.name ("file"))),
    T1 (al.number ("T1")),
    T2 (al.number ("T2")),
    A (al.number_sequence ("A")),
    B (al.number_sequence ("B")),
    precipitation (-42.42e42),
    reference_evapotranspiration (-42.42e42),
    global_radiation (-42.42e42),
    Prain (-42.42e42),
    Psnow (-42.42e42)
{ }

WeatherFile::~WeatherFile ()
{
  close (file.rdbuf ()->fd ());
}

// Add the WeatherFile syntax to the syntax table.
Weather&
WeatherFile::make (const Time& t, const AttributeList& al)
{
  return *new WeatherFile (t, al);
}

static struct WeatherFileSyntax
{
  WeatherFileSyntax ();
} WeatherFile_syntax;

WeatherFileSyntax::WeatherFileSyntax ()
{ 
  static const double A[] = 
  {
    -0.916667,
    -1.687500,
    -1.681818,
    -1.468085,
    -1.387324,
    -1.377246,
    -1.389937,
    -1.518519,
    -1.629630,
    -1.631579,
    -1.333333,
    -0.900000
  };
  static const double B[] = 
  {
    -0.166667,
    -0.437500,
    -0.454545,
    -0.382979,
    -0.338028,
    -0.335329,
    -0.389937,
    -0.407407,
    -0.358025,
    -0.236842,
    -0.166667,
    -0.200000
  };

  Syntax& syntax = *new Syntax ();
  AttributeList& alist = *new AttributeList ();
  syntax.add ("Latitude", Syntax::Number, Syntax::Const);
  alist.add ("Latitude", 56.0);
  syntax.add ("file", Syntax::String, Syntax::Const);
  syntax.add ("T1", Syntax::Number, Syntax::Const);
  alist.add ("T1", -2.0);
  syntax.add ("T2", Syntax::Number, Syntax::Const);
  alist.add ("T2", 2.0);
  syntax.add ("A", Syntax::Number, Syntax::Const, 12);
  syntax.add ("B", Syntax::Number, Syntax::Const, 12);
  vector<double>& a = *new vector<double>;
  vector<double>& b = *new vector<double>;
  for (int i = 0; i < 12; i++)
    {
      a.push_back (A[i]);
      b.push_back (B[i]);
    }
  alist.add ("A", a);
  alist.add ("B", b);
  syntax.add ("Prain", Syntax::Number, Syntax::LogOnly);
  syntax.add ("Psnow", Syntax::Number, Syntax::LogOnly);
  syntax.add ("air_temperature", Syntax::Number, Syntax::LogOnly);
  syntax.add ("global_radiation", Syntax::Number, Syntax::LogOnly);
  syntax.add ("hourly_global_radiation", Syntax::Number, Syntax::LogOnly);
  syntax.add ("reference_evapotranspiration", Syntax::Number, Syntax::LogOnly);
  syntax.add ("hourly_reference_evapotranspiration",
	      Syntax::Number, Syntax::LogOnly);
  syntax.order ("file");
  Weather::add_type ("file", alist, syntax, &WeatherFile::make);
}
