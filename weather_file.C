// weather_file.C

#include "weather.h"
#include "syntax.h"
#include "alist.h"
#include "common.h"
#include "log.h"
#include "filter.h"
#include <algobase.h>
#include <fstream.h>

class WeatherFile : public Weather
{
  Time date;
  const string file_name;
  ifstream file;
  const double T1;
  const double T2;
  const vector<double>& A;
  const vector<double>& B;

  double precipitation;
  double reference_evapotranspiration;
  double global_radiation;
  double air_temperature;

  double Prain;
  double Psnow;

  int line;
  // Simulation.
public:
  void tick ();
  void output (const string, Log&, const Filter&) const;
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
      
      if (end == '\n')
	reference_evapotranspiration = -42.42e42;
      else
	{
	  file >> reference_evapotranspiration;
	  while (file.good () && file.get () != '\n')
	    /* do nothing */;
	}
      date = Time (year, month, day, 23);
    }
    
  if (AirTemperature () < T1)
    Psnow = Precipitation ();
  else if (T2 < AirTemperature ())
    Psnow = 0.0;
  else
    Psnow = Precipitation () * (T2 - AirTemperature ()) / (T2 - T1);

  Prain = Precipitation () - Snow ();
}

void
WeatherFile::output (const string name, Log& log, const Filter& filter) const
{
  if (filter.check (name))
    {
      const Filter& f1 = filter.lookup (name);
      if (f1.check ("file"))
	{
	  const Filter& f2 = f1.lookup ("file");
	  log.open (name, "file");
	  log.output ("Prain", f2, Prain, true);	
	  log.output ("Psnow", f2, Psnow, true);
	  log.close ();
	}
    }
}

double
WeatherFile::AirTemperature (void) const // [C]
{
  // BUG: No variation over the day? 
  double t = 2.0 * M_PI / 365.0 * time.yday ();
  return (7.7 - 7.7 * cos (t) - 3.6 * sin (t));
}

double
WeatherFile::GlobalRadiation () const	// [W/m^2]
{
  /*a function of the weather*/
  static const double A0[] =
  { 17.0, 44.0, 94.0, 159.0, 214.0, 247.0, 214.0, 184.0, 115.0, 58.0, 25.0,
    13.0 };
  static const double A1[] = 
  { -31.0, -74.0, -148.0, -232.0, -291.0, -320.0, -279.0, -261.0, -177.0,
    -96.0, -45.0, -24.0 };
  static const double B1[] =
  { -7.0, -20.0, -34.0, -45.0, -53.0, -63.0, -67.0, -52.0, -30.0, -13.0, 
    -6.0, -4.0 };
  static const double A2[] = 
  { 21.0, 42.0, 68.0, 77.0, 67.0, 0.0, 0.0, 75.0, 73.0, 54.0, 32.0, 18.0 };
  static const double B2[] = 
  { 11.0, 25.0, 32.0, 29.0, 23.0, 0.0, 0.0, 29.0, 25.0, 15.0, 8.0, 7.0 };

  double t = 2.0 * M_PI / 24.0 * time.hour ();
  int m = time.month () - 1;
  double Si = (  A0[m] + A1[m] * cos (t) + B1[m] * sin (t)
		 + A2[m] * cos (2 * t) + B2[m] * sin (2 * t));
  return max (0.0, Si * (global_radiation / A0[m]));
}

double
WeatherFile::ReferenceEvapotranspiration () const // [mm/h]
{
  if (reference_evapotranspiration < 0)
    {
      const double T = 273.16 + AirTemperature ();
      const double Delta = 5362.7 / pow (T, 2) * exp (26.042 - 5362.7 / T);
      return 1.05e-3 * Delta / (Delta + 66.7) * GlobalRadiation ();
    }
  int m = time.month () - 1;
  int h = time.hour ();
  return reference_evapotranspiration 
    * (1.0
       + A[m] * cos (2.0 * M_PI / 24.0 * h)
       + B[m] * sin (2.0 * M_PI / 24.0 * h));
}

double
WeatherFile::Precipitation () const
{
  return precipitation / 24.0;
}

double
WeatherFile::Rain () const
{
  return Prain;
}

double
WeatherFile::Snow () const
{
  return Psnow;
}

WeatherFile::WeatherFile (const Time& t, const AttributeList& al)
  : Weather (t, al.number ("Latitude")),
    date (42, 1, 1, 0),
    file_name (al.name ("file")),
    file (al.name ("file").c_str()),
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
{ }

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
  syntax.order ("file");
  Weather::add_type ("file", alist, syntax, &WeatherFile::make);
}
