// weather_std.C --- Standard weather file.

#include "weather.h"
#include "lexer_data.h"
#include "time.h"
#include "mathlib.h"
#include <vector>
#include <algorithm>
#include <numeric>
#include <set>

struct WeatherStandard : public Weather
{
  // Units.
  struct unit_type
  {
    const char* from;
    const char* to;
    double factor;
  };
  static const unit_type unit_table[];
  static const int unit_table_size;
  bool has_conversion (const string& from, const string& to);
  double convert_unit (const string& from, const string& to);

  // Snow Model.
  const double T_rain;
  const double T_snow;

  // Keywords
  struct keyword_description_type
  {
    const char* name;
    const char* dim;
    double WeatherStandard::* value;
    double min;
    double max;
    bool required;
  };
  static const keyword_description_type keyword_description[];
  static const int keyword_description_size;
  int timestep;
  Time begin;
  Time end;

  // Data.
  struct data_description_type
  {
    const char* name;
    const char* dim;
    double WeatherStandard::* value;
    double WeatherStandard::* factor;
    double min;
    double max;
    bool required;
  };
  static const data_description_type data_description[];
  static const int data_description_size;
  vector<int> data_index;
  bool has_data (const string& name);
  bool has_date;
  bool has_hour;
  bool has_temperature;
  bool has_vapor_pressure;
  bool has_relative_humidity;
  bool has_wind_speed;
  bool has_reference_evapotranspiration;

  // Factors.
  double air_temperature_factor;
  double global_radiation_factor;
  double precipitation_factor;
  double vapor_pressure_factor;
  double relative_humidity_factor;
  double wind_speed_factor;
  double reference_evapotranspiration_factor;

  // Parsing.
  LexerData lex;

  // These are the last read values for today.
  Time last_time;
  double last_air_temperature;
  double last_global_radiation;
  double last_precipitation;
  double last_vapor_pressure;
  double last_relative_humidity;
  double last_wind_speed;
  double last_reference_evapotranspiration;

  // These are the first values after today.
  Time next_time;
  double next_year;
  double next_month;
  double next_day;
  double next_hour;
  double next_air_temperature;
  double next_global_radiation;
  double next_precipitation;
  double next_vapor_pressure;
  double next_relative_humidity;
  double next_wind_speed;
  double next_reference_evapotranspiration;

  // Data.
  int hour;
  double air_temperature_[24];
  double global_radiation_[24];
  double precipitation_[24];
  double vapor_pressure_[24];
  double wind_speed_[24];
  double reference_evapotranspiration_[24];

  // Daily averages.
  double daily_air_temperature_;
  double daily_global_radiation_;
  
  // Fractions this hour.
  double snow_fraction;
  double rain_fraction;

  // Simulation.
  void tick (const Time& time);
  void output (Log& log) const
    { Weather::output (log); }
  void read_line ();
  void read_new_day (const Time&);

  // Communication with Bioclimate.
  double hourly_air_temperature () const // [dg C]
    { return air_temperature_[hour]; }
  double daily_air_temperature () const // [dg C]
    { return daily_air_temperature_; }
  double hourly_global_radiation () const // [W/m2]
    { return global_radiation_[hour]; }
  double daily_global_radiation () const // [W/m2]
    { return daily_global_radiation_; }
  double reference_evapotranspiration () const // [mm/h]
    { return reference_evapotranspiration_[hour]; }
  double rain () const	// [mm/h]
    { return precipitation_[hour] * rain_fraction; }
  double snow () const	// [mm/h]
    { return precipitation_[hour] * snow_fraction; }
  double vapor_pressure () const // [Pa]
    { return vapor_pressure_[hour]; }
  double wind () const	// [m/s]
    { return wind_speed_[hour]; }

  // Communication with external model.
  void put_precipitation (double prec) // [mm/d]
    { precipitation_[hour] = prec / 24.0; }
  void put_air_temperature (double T) // [°C]
    { 
      air_temperature_[hour] = T; 
      daily_air_temperature_ = T;
    }
  void put_reference_evapotranspiration (double ref) // [mm/d]
    { reference_evapotranspiration_[hour] = ref; }
  void put_global_radiation (double radiation) // [W/m²]
    { 
      global_radiation_[hour] = radiation;
      daily_global_radiation_ = radiation; 
    }

  // Create and Destroy.
  WeatherStandard (const AttributeList&);
  ~WeatherStandard ();
  bool check (const Time& from, const Time& to) const;
};

const WeatherStandard::unit_type 
WeatherStandard::unit_table[] = 
{ { "mm/d", "mm/h", 0.0416666667 },
  { "dgWest", "dgEast", -1},
  { "dgSouth", "dgNorth", -1},
  { "%", "fraction", 0.01 } };
  
const int 
WeatherStandard::unit_table_size = 
/**/ sizeof (WeatherStandard::unit_table)
  /**/ / sizeof (WeatherStandard::unit_type); 

bool
WeatherStandard::has_conversion (const string& from, const string& to)
{
  if (from == to)
    return true;
  
  for (unsigned int i = 0; i < unit_table_size; i++)
    if (unit_table[i].from == from && unit_table[i].to == to)
      return true;
  return false;
}

double
WeatherStandard::convert_unit (const string& from, const string& to)
{
  if (from == to)
    return 1.0;
  
  for (unsigned int i = 0; i < unit_table_size; i++)
    if (unit_table[i].from == from && unit_table[i].to == to)
      return unit_table[i].factor;

  assert (false);
  return -42.42e42;
}

const WeatherStandard::keyword_description_type 
WeatherStandard::keyword_description[] =
{ { "Latitude", "dgNorth", &WeatherStandard::latitude, -90, 90, true },
  { "Longitude", "dgEast", &WeatherStandard::longitude, -360, 360, true },
  { "Elevation", "m", &WeatherStandard::elevation, 0, 10000, true },
  { "TimeZone", "dgEast", &WeatherStandard::timezone, -360, 360, true },
  { "ScreenHeight", "m", &WeatherStandard::screen_height, 0, 100, true },
  { "TAverage", "dgC", &WeatherStandard::T_average, -10, 40, true },
  { "TAmplitude", "dgC", &WeatherStandard::T_amplitude, 0, 100, true },
  { "MaxTDay", "yday", &WeatherStandard::max_Ta_yday, 1, 365, true } };

const int 
WeatherStandard::keyword_description_size 
/**/ = sizeof (WeatherStandard::keyword_description) 
  /**/ / sizeof (keyword_description_type);

const WeatherStandard::data_description_type 
WeatherStandard::data_description[] =
{ { "Year", "year", &WeatherStandard::next_year, NULL,
    1, 9999, false },
  { "Month", "month", &WeatherStandard::next_month, NULL,
    1, 12, false },
  { "Day", "mday", &WeatherStandard::next_day, NULL,
    1, 31, false },
  { "Hour", "hour", &WeatherStandard::next_hour, NULL,
    0, 23, false },
  { "GlobRad", "W/m^2", &WeatherStandard::next_global_radiation,
    &WeatherStandard::global_radiation_factor,
    0, 1400, true },
  { "AirTemp", "dgC", &WeatherStandard::next_air_temperature,
    &WeatherStandard::air_temperature_factor,
    -70, 60, false },
  { "Precip", "mm/h", &WeatherStandard::next_precipitation,
    &WeatherStandard::precipitation_factor,
    0, 300, true },
  { "RefEvap", "mm/h", &WeatherStandard::next_reference_evapotranspiration,
    &WeatherStandard::reference_evapotranspiration_factor,
    -10, 20, false },
  { "VapPres", "Pa", &WeatherStandard::next_vapor_pressure,
    &WeatherStandard::vapor_pressure_factor,
    0, 5000, false },
  { "RelHum", "fraction", &WeatherStandard::next_relative_humidity,
    &WeatherStandard::relative_humidity_factor,
    0, 5000, false },
  { "Wind", "m/s", &WeatherStandard::next_wind_speed,
    &WeatherStandard::wind_speed_factor,
    0, 40, false } };

const int 
WeatherStandard::data_description_size 
/**/ = sizeof (WeatherStandard::data_description) 
  /**/ / sizeof (data_description_type);

bool
WeatherStandard::has_data (const string& name)
{
  for (unsigned int i = 0; i < data_index.size (); i++)
    {
      int index = data_index[i];
      if (index >= 0 && name == data_description[index].name)
	return true;
    }
  return false;
}

void
WeatherStandard::tick (const Time& time)
{
  Weather::tick (time);

  hour = time.hour ();
  
  if (hour == 0)
    read_new_day (time);

  // Snow and rain fractions.
  const double T = hourly_air_temperature ();
  if (T < T_snow)
    {
      snow_fraction = 1.0;
      rain_fraction = 0.0;
    }
  else if (T_rain < T)
    {
      snow_fraction = 0.0;
      rain_fraction = 1.0;
    }
  else
    {
      snow_fraction = (T_rain - T) / (T_rain - T_snow);
      rain_fraction = 1.0 - snow_fraction;
    }
  assert (rain_fraction >= 0 && rain_fraction <= 1);
  assert (snow_fraction >= 0 && snow_fraction <= 1);
  assert (approximate (rain_fraction + snow_fraction, 1.0));

  Weather::tick_after (time);
}

void 
WeatherStandard::read_line ()
{
  // Remember old values.
  last_time = next_time;
  last_air_temperature = next_air_temperature;
  last_global_radiation = next_global_radiation;
  last_precipitation = next_precipitation;
  last_vapor_pressure = next_vapor_pressure;
  last_relative_humidity = next_relative_humidity;
  last_wind_speed = next_wind_speed;
  last_reference_evapotranspiration = next_reference_evapotranspiration;

  while (true)
    {
      // Read new values.
      for (unsigned int i = 0; i < data_index.size (); i++)
	{
	  lex.skip_space ();
	  const int index = data_index[i];
	  if (index < 0)
	    continue;
	  const double factor = data_description[index].factor
	    ? this->*(data_description[index].factor) : 1.0;
	  const double value =  factor * lex.get_number ();
	  this->*(data_description[index].value) = value;
	  if (value < data_description[index].min)
	    lex.error (string ("Column ") 
		       + data_description[index].name + " value too low");
	  else if (value > data_description[index].max)
	    lex.error (string ("Column ") 
		       + data_description[index].name + " value too hight");
	  if (!lex.good ())
	    throw ("no more climate data");
	  if (next_precipitation < 0.0)
	    next_precipitation = 0.0;
	}

      // Update time.
      if (timestep > 0)
	next_time.tick_hour (timestep);
      if (has_date)
	{
	  const int year = static_cast<int> (next_year);
	  const int month = static_cast<int> (next_month);
	  const int mday = static_cast<int> (next_day);
	  const int hour = has_hour 
	    ? static_cast<int> (next_hour) : next_time.hour ();

	  if (!Time::valid (year, month, mday, hour))
	    {
	      lex.error ("Invalid date");
	      lex.next_line ();
	      continue;
	    }
	  if (timestep > 0 && !(next_time == Time (year, month, mday, hour)))
	    {
	      lex.error ("Bad timestep");
	      next_time = Time (year, month, mday, hour);
	    }
	}
      break;
    }
  lex.next_line ();
}
void 
WeatherStandard::read_new_day (const Time& time)
{ 
  assert (time.hour () == 0);
  Time now = time;
  Time tomorrow = time;
  tomorrow.tick_day ();

  // BC5 sucks // while (next_time <= now)
  while (!(now < next_time))
    read_line ();

  while (true)
    {
      Time end = (next_time < tomorrow) ? next_time : tomorrow;

      bool long_timestep = (Time::hours_between (last_time, next_time) > 12);
      for (;now < end; now.tick_hour ())
	{
	  int hour = now.hour ();
	  if (has_temperature)
	    air_temperature_[hour] = last_air_temperature;
	  else
	    air_temperature_[hour] = T_normal (now);
	  global_radiation_[hour] = last_global_radiation;
	  if (long_timestep)
	    global_radiation_[hour] *= day_cycle (now) * 24.0;
	  precipitation_[hour] = last_precipitation;
	  if (has_vapor_pressure)
	    vapor_pressure_[hour] = last_vapor_pressure;
	  else if (has_relative_humidity)
	    vapor_pressure_[hour] 
	      = SaturationVapourPressure (air_temperature_[hour])
	      * last_relative_humidity;
	  if (has_wind_speed)
	    wind_speed_[hour] = last_wind_speed;
	  else
	    wind_speed_[hour] = 3.0;
	  if (has_reference_evapotranspiration)
	    {
	      reference_evapotranspiration_[hour] 
		= last_reference_evapotranspiration;
	      if (long_timestep)
		reference_evapotranspiration_[hour] *= day_cycle (now) * 24.0;
	    }
	  else
	    reference_evapotranspiration_[hour] 
	      = Weather::Makkink (air_temperature_[hour],
				  global_radiation_[hour]);
	}
      // BC5 sucks // if (next_time >= tomorrow)
      if (!(next_time < tomorrow))
	break;
      read_line ();
    }

  daily_global_radiation_ 
    = accumulate (&global_radiation_[0], &global_radiation_[24], 0.0) / 24.0;
  daily_air_temperature_
    = accumulate (&air_temperature_[0], &air_temperature_[24], 0.0) / 24.0;

  if (!has_vapor_pressure && !has_relative_humidity)
    {
      double T_min = *min_element (&air_temperature_[0],
				   &air_temperature_[24]);
      double T_max = *max_element (&air_temperature_[0],
				   &air_temperature_[24]);
      if (T_min == T_max)
	T_min -= 5.0;
      for (int hour = 0; hour < 24; hour++)
	vapor_pressure_[hour] = SaturationVapourPressure (T_min);
    }
}

WeatherStandard::WeatherStandard (const AttributeList& al)
  : Weather (al),
    T_rain (al.number ("T_rain")),
    T_snow (al.number ("T_snow")),
    timestep (0),
    begin (1900, 1, 1, 0),
    end (2100, 1, 1, 0),
    has_date (false),
    has_hour (false),
    has_temperature (false),
    has_vapor_pressure (false),
    has_relative_humidity (false),
    has_wind_speed (false),
    has_reference_evapotranspiration (false),
    air_temperature_factor (1.0),
    global_radiation_factor (1.0),
    precipitation_factor (1.0),
    vapor_pressure_factor (1.0),
    relative_humidity_factor (1.0),
    wind_speed_factor (1.0),
    reference_evapotranspiration_factor (1.0),
    lex (al.name ("where")),
    last_time (end),
    last_air_temperature (-42.42e42),
    last_global_radiation (-42.42e42),
    last_precipitation (-42.42e42),
    last_vapor_pressure (-42.42e42),
    last_relative_humidity (-42.42e42),
    last_wind_speed (-42.42e42),
    last_reference_evapotranspiration (-42.42e42),
    next_time (begin),
    next_year (-42.42e42),
    next_month (-42.42e42),
    next_day (-42.42e42),
    next_hour (-42.42e42),
    next_air_temperature (-42.42e42),
    next_global_radiation (-42.42e42),
    next_precipitation (42.42e42),
    next_vapor_pressure (-42.42e42),
    next_relative_humidity (-42.42e42),
    next_wind_speed (-42.42e42),
    next_reference_evapotranspiration (-42.42e42),
    hour (-42),
    daily_air_temperature_ (-42.42e42),
    daily_global_radiation_ (-42.42e42),
    snow_fraction (-42.42e42),
    rain_fraction (-42.42e42)
{  
  // Open errors?
  if (!lex.good ())
    return;

  // Read first line.
  const string type = lex.get_word ();
  if (type != "dwf-0.0")
    lex.error ("Wrong file type");
  lex.skip_line ();
  lex.next_line ();

  set<string, less<string>/**/> keywords;

  // Read keywords.
  bool last_was_note = false;
  while (lex.good () && lex.peek () != '-')
    {
      string key = lex.get_word ();

      assert (key.size () > 0);
      if (key[key.size () - 1] != ':')
	{
	  lex.error ("Keywords should end in :");
	  lex.skip_line ();
	  lex.next_line ();
	  continue;
	}
      key = key.substr (0, key.size () - 1);

      if (keywords.find (key) == keywords.end ())
	keywords.insert (key);
      else if (key != "Note")
	lex.error (string ("Duplicate keyword `") + key + "'");
      else if (!last_was_note)
	lex.error ("Only one Note: block allowed");
      
      last_was_note = false;
		   
      if (key == "Station")
	lex.skip_line ();
      else if (key == "Note")
	{
	  lex.skip_line ();
	  last_was_note = true;
	}
      else if (key == "Surface")
	{
	  lex.skip_space ();
	  const string type = lex.get_word ();
	  if (type == "reference")
	    surface = Surface::reference;
	  else if (type == "field")
	    surface = Surface::field;
	  else
	    lex.error ("Uknown surface type");
	}
      else if (key == "Begin")
	{
	  lex.skip_space ();
	  lex.read_date (begin);
	}
      else if (key == "End")
	{
	  lex.skip_space ();
	  lex.read_date (end);
	}
      else
	{
	  lex.skip_space ();
	  double val = lex.get_number ();
	  lex.skip_space ();
	  string dim = lex.get_word ();
	      
	  if (key == "NH4WetDep")
	    {
	      if (has_conversion (dim, "ppm"))
		val *= convert_unit (dim, "ppm");
	      else
		lex.error ("Unknown dimension");
	      if (val < 0.0 || val > 100.0)
		lex.error ("Unreasonable value");
	      WetDeposit.NH4 = val;
	    }
	  else if (key == "NH4DryDep")
	    {
	      if (has_conversion (dim, "kgN/year"))
		val *= convert_unit (dim, "kgN/year");
	      else
		lex.error ("Unknown dimension");
	      if (val < 0.0 || val > 100.0)
		lex.error ("Unreasonable value");
	      DryDeposit.NH4 = val;
	    }
	  else if (key == "NO3WetDep")
	    {
	      if (has_conversion (dim, "ppm"))
		val *= convert_unit (dim, "ppm");
	      else
		lex.error ("Unknown dimension");
	      if (val < 0.0 || val > 100.0)
		lex.error ("Unreasonable value");
	      WetDeposit.NO3 = val;
	    }
	  else if (key == "NO3DryDep")
	    {
	      if (has_conversion (dim, "kgN/year"))
		val *= convert_unit (dim, "kgN/year");
	      else
		lex.error ("Unknown dimension");
	      if (val < 0.0 || val > 100.0)
		lex.error ("Unreasonable value");
	      DryDeposit.NO3 = val;
	    }
	  else if (key == "Timestep")
	    {
	      if (has_conversion (dim, "hours"))
		val *= convert_unit (dim, "hours");
	      else
		lex.error ("Unknown dimension");
	      timestep = static_cast<int> (val);
	      if (timestep != val || timestep < 0.0)
		lex.error ("Timestep should be a cardinal number");
	    }
	  else
	    {
	      bool found = false;
	      for (unsigned int i = 0; i < keyword_description_size; i++)
		{
		  if (key == keyword_description[i].name)
		    {
		      if (has_conversion (dim, keyword_description[i].dim))
			val *= convert_unit (dim, keyword_description[i].dim);
		      else
			lex.error ("Unknown dimension");
		      if (val < keyword_description[i].min)
			lex.error (key + " value too low");
		      else if (val > keyword_description[i].max)
			lex.error (key + " value too high");
		      this->*(keyword_description[i].value) = val;
		      found = true;
		    }
		}
	      if (!found)
		lex.error (string ("Unknown keyword: `") + key + "'");
	    }
	}
      lex.next_line ();
    }

  // Check keywords.
  for (unsigned int i = 0; i < keyword_description_size; i++)
    if (keyword_description[i].required 
	&& keywords.find (keyword_description[i].name) == keywords.end ())
      lex.error (string ("Keyword ") 
		 + keyword_description[i].name + " missing");

  static const string required[] = 
  { "NH4WetDep", "NO3WetDep", "NH4DryDep", "NO3DryDep", "Station",
    "Surface", "Begin", "End" };
  static const int required_size = sizeof (required) / sizeof (string);
  
  for (unsigned int i = 0; i < required_size; i++)
    if (keywords.find (required[i]) == keywords.end ())
      lex.error (string ("Missing keyword `") + required[i] + "'");

  // BC5 sucks // if (begin >= end)
  if (!(begin < end))
    lex.error ("Weather data ends before they begin");

  lex.skip_hyphens ();

  // Columns
  do
    {
      const string column = lex.get_word ();
      bool found = false;
      for (unsigned int j = 0; j < data_description_size; j++)
	if (column == data_description[j].name)
	  {
	    data_index.push_back (j);
	    found = true;
	    break;
	  }
      if (!found)
	{
	  data_index.push_back (-1);
	  lex.error (string ("Unknown column ") + column);
	}
      lex.skip_space ();
    }
  while (lex.good () && lex.peek () != '\n');
  lex.next_line ();

  has_date = (has_data ("Year") && has_data ("Month") && has_data ("Day"));
  has_hour = (has_date && has_data ("Hour"));
  if (!has_date && (has_data ("Year")
		    || has_data ("Month")
		    || has_data ("Day")))
    lex.error ("You should specify all of Year, Month and Day, or none");
  if (timestep < 1 && !has_date)
    lex.error ("You must specify either a timestep or date");
  has_temperature = has_data ("AirTemp");
  has_vapor_pressure = has_data ("VapPres");
  has_relative_humidity = has_data ("RelHum");
  if (has_relative_humidity && has_vapor_pressure)
    lex.error ("You should only specify one of VapPres or RelHum");
  has_wind_speed = has_data ("Wind");
  has_reference_evapotranspiration = has_data ("RefEvap");
  for (unsigned int j = 0; j < data_description_size; j++)
    if (data_description[j].required && !has_data (data_description[j].name))
      lex.error (string ("Required data column `") 
		 + data_description[j].name + "' missing");

  // Dimensions.
  for (unsigned int i = 0; i < data_index.size (); i++)
    {
      const string dimension = lex.get_word ();
      const int index = data_index[i];
      if (has_conversion (dimension, data_description[index].dim))
	{
	  if (data_description[index].factor)
	    this->*(data_description[index].factor) 
	      = convert_unit (dimension, data_description[index].dim);
	}
      else
	lex.error ("Bad unit");

      lex.skip_space ();
    }
  lex.next_line ();

  // Time.
  next_time = begin;
  next_time.tick_hour (-timestep);
}

WeatherStandard::~WeatherStandard ()
{ }

bool
WeatherStandard::check (const Time& from, const Time& to) const
{ 
  bool ok = true;
  if (lex.error_count > 0)
    {
      CERR << lex.error_count << " parser errors encountered\n";
      ok = false;
    }
  if (from < begin)
    {
      CERR << "Simulation starts before weather data\n";
      ok = false;
    }
  if (to > end)
    {
      CERR << "Simulation ends after weather data\n";
      ok = false;
    }
  if (latitude < -66 || latitude > 66)
    {
      CERR << "Researching arctic agriculture? (latitude = "
	   << latitude << ")\n";
    }
  return ok;
}

static struct WeatherStandardSyntax
{
  static Weather& make (const AttributeList& al)
  { return *new WeatherStandard (al); }

  WeatherStandardSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "Read a Daisy Weather File.");
    Weather::load_syntax (syntax, alist);

    syntax.add ("where", Syntax::String, Syntax::Const,
		"File to read weather data from.");
    syntax.order ("where");

    // Division between Rain and Snow.
    syntax.add ("T_rain", "dg C", Syntax::Const, 
		"Above this air temperature all precipitation is rain.");
    alist.add ("T_rain", 2.0);
    syntax.add ("T_snow", "dg C", Syntax::Const,
		"Below this air temperature all precipitation is snow.");
    alist.add ("T_snow", -2.0);


    Librarian<Weather>::add_type ("default", alist, syntax, &make);
  }
} WeatherStandard_syntax;
