// program_weather.C -- Examine weather file.
// 
// Copyright 2019 Per Abrahamsen and KU.
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

#include "program.h"
#include "wsource.h"
#include "time.h"
#include "librarian.h"
#include "block_model.h"
#include "submodeler.h"
#include "time.h"
#include "assertion.h"
#include "mathlib.h"

#include <fstream>
#include <numeric>
#include <memory>
#include <sstream>
#include <vector>

struct ProgramWeather : public Program
{
  // Data.
  const symbol file;
  const std::unique_ptr<WSource> weather;
  const Time begin;
  const Time end;
  const std::vector<double> fractiles;

  // Utils.
  static void print_fractiles (const symbol file,
			       const symbol key,
			       const std::vector<double>& fractiles,
			       const std::vector<std::vector<double>>& all,
			       bool accumulated)
  {
    const std::string name = file.name () + key.name () + ".csv";
    std::ofstream out (name.c_str ());
    out << "Day," << key;
    for (double f: fractiles)
      out << "," << key << f * 100 << "%";

    daisy_assert (all.size () > 1);
    const  std::vector<double> zero (all[1].size (), 0.0);
    std::vector<double> last = zero;
    for (int day = 1; day < 366; day++)
      {
	out << "\n" << day;
	daisy_assert (day < all.size ());
	for (size_t i = 0; i < std::min (last.size (), all[day].size ()); i++)
	  last[i] += all[day][i];
	std::vector<double> data = last;
	const double avg = std::accumulate (data.begin (), data.end (), 0.0) / data.size ();
	out << "," << avg;
	std::sort (data.begin (), data.end ());
	const int end = data.size () - 1;
	daisy_assert (end >= 0);
	for (double f: fractiles)
	  out << "," << data[end * f];
	if (!accumulated)
	  last = zero;
      }
  }

  
  // Use.
  bool run (Treelog& msg)
  {
    std::vector<double> count (367, 0.0);  
    std::vector<double> T_sum  (367, 0.0);
    std::vector<double> P_sum (367, 0.0);
    std::vector<double> ET0_sum (367, 0.0);
    std::vector<double> N_sum (367, 0.0);
    std::vector<std::vector<double>> T_all  (367);
    std::vector<std::vector<double>> P_all (367);
    std::vector<std::vector<double>> ET0_all (367);
    std::vector<std::vector<double>> N_all (367);
  
    Time time = begin;
    while (time < end)
      {
	double T = 0.0;
	double P = 0.0;
	double ET0 = 0.0;

	const int old_day = time.yday ();
	const int old_hour = time.hour ();
	const int old_year = time.year ();
	for (int i = 0; i < 24; i++)
	  {
	    time.tick_hour (1);
	    weather->weather_tick (time, msg);
	    T += weather->air_temperature ();
	    P += weather->rain () + weather->snow ();
	    ET0 += weather->reference_evapotranspiration ();
	  }
	T /= 24.0;

	const double N = P - ET0;

	const int yday = time.yday ();
	daisy_assert (yday > 0 && yday < 367);
	daisy_assert (yday != old_day);
	daisy_assert (time.hour () == old_hour);
	if (time.year () != old_year)
	  {
	    std::ostringstream tmp;
	    tmp << time.year ();
	    msg.message (tmp.str ());
	  }
	count[yday] += 1.0;
	T_sum[yday] += T;
	P_sum[yday] += P;
	ET0_sum[yday] += ET0;
	N_sum[yday] += N;
	T_all[yday].push_back (T);
	P_all[yday].push_back (P);
	ET0_all[yday].push_back (ET0);
	N_all[yday].push_back (N);
      }
    // Temperature.
    double a0 = 0.0;
    double a1 = 0.0;
    double b1 = 0.0;
    // Precipitation.
    double P_yearly = 0.0;
    for (int i = 1; i < 366; i++)
      {
	daisy_assert (count[i] > 0.0);
	// Temperature.
	const double p = T_sum[i] / count[i];
	const double t = i;
	a0 += p * std::cos (2.0 * M_PI * 0.0 * t / 365.0);
	a1 += p * std::cos (2.0 * M_PI * 1.0 * t / 365.0);
	b1 += p * std::sin (2.0 * M_PI * 1.0 * t / 365.0);
	// Precipitation.
	P_yearly += P_sum[i] / count[i];
      }
    a0 *= 2.0 / 365.0;
    a1 *= 2.0 / 365.0;
    b1 *= 2.0 / 365.0;

    const double A0 = a0;
    const double A1 = std::sqrt (a1 * a1 + b1 * b1);
    double phi = std::atan2 (b1, a1);
    if (phi < 0.0)
      phi += 2.0 * M_PI;
    std::ostringstream tmp;
    tmp << "TAverage: " << 0.5 * A0 << " dgC\n"
	<< "TAmplitude: " << A1 << " dgC\n"
	<< "MaxTDay: " << std::round (phi * 365.0 / (2.0 * M_PI)) << " yday\n"
	<< "PAverage: " << (P_yearly * 365.2425 / 365.0) << " mm";
    msg.message (tmp.str ());

    if (file != Attribute::None ())
      {
	print_fractiles (file, "T", fractiles, T_all, false);
	print_fractiles (file, "P", fractiles, P_all, true);
	print_fractiles (file, "ET0", fractiles, ET0_all, true);
	print_fractiles (file, "N", fractiles, N_all, true);
      }
    return true;
  }
  
  // Create and Destroy.
  void initialize (Block& block)
  {
    weather->weather_initialize (begin, block.msg ());
  }
  
  bool check (Treelog& msg)
  {
    bool ok = true;
    if (Time::whole_days_between (begin, end) < 366)
      {
	msg.error ("Need more than one year of data");
	ok = false;
      }
    if (Time::whole_days_between (begin, end) < 365 * 10)
      msg.warning ("Should have at least 10 years of data");
    
    if (!weather->weather_check (begin, end, msg))
      ok = false;

    return ok;
  }
  
  ProgramWeather (const BlockModel& al)
    : Program (al),
      file (al.name ("file", Attribute::None ())),
      weather (Librarian::build_item<WSource> (al, "weather")),
      begin (*submodel<Time> (al, "begin")),
      end (*submodel<Time> (al, "end")),
      fractiles (al.number_sequence ("fractiles"))
  { }
  ~ProgramWeather ()
  { }
};

static struct ProgramWeatherSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ProgramWeather (al); }
  ProgramWeatherSyntax ()
    : DeclareModel (Program::component, "weather", "\
Weather data from a data file.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare_string ("file", Attribute::OptionalConst, "\
Base name for files containing daily average data.\n\
By default, create no such files");
    frame.declare_object ("weather", WSource::component, 
			  Attribute::Const, Attribute::Singleton, "\
Weather source to analyse.");
    frame.declare_submodule ("begin", Attribute::Const,
			     "First date to analyze.", Time::load_syntax);
    frame.declare_submodule ("end", Attribute::Const,
			     "Last date to analyse.", Time::load_syntax);
    frame.declare_fraction ("fractiles", Attribute::Const,  Attribute::Variable, "\
List of fractiles print in 'file'.");
    frame.set_empty ("fractiles");
  }
} ProgramWeather_syntax;

// program_weather.C ends here.
