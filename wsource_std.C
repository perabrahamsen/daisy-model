// wsource_std.C -- Standard weather data file.
// 
// Copyright 2011 KU
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

#include "wsource_table.h"
#include "librarian.h"
#include "treelog.h"
#include "timestep.h"
#include "block_model.h"
#include "submodeler.h"
#include "memutils.h"
#include <sstream>

struct WSourceStandard : public WSourceTable
{
  // Missing years.
  struct YearMap
  {
    // Types.
    struct YearInterval
    {
      // Parameters.
      const int from;
      const int to;

      // Use.
      int size () const
      { return to - from + 1; }

      // Create and Destroy.
      static bool check_alist (const Metalib&, const Frame& al, Treelog&);
      static void load_syntax (Frame&);
      YearInterval (const FrameSubmodel&);
    };

    // Parameters.
    const YearInterval from;
    const YearInterval to;

    // Use.
    bool contain (const Time& time) const;
    void map_time (Time& time) const;

    // Create and Destroy.
    static bool check_alist (const Metalib&, const Frame& al, Treelog&);
    static void load_syntax (Frame&);
    YearMap (const Block&);
  };
  const auto_vector<const YearMap*> missing_years;
  int active_map;
  Time safe_end;
  int find_map (const Time& time) const;
  void map_time (const Time& simulation_time, Time& mapped_time, 
                 Treelog& msg);
  void map_time_only (const Time& simulation_time, Time& mapped_time, 
                      Treelog& msg) const;

  void weather_tick (const Time& simulation_time, Treelog& msg)
  {
    // Handle missing years.
    Time mapped_time;
    map_time (simulation_time, mapped_time, msg);
    WSourceTable::weather_tick (mapped_time, msg); 
  }


  // Create and destroy.
  bool top_level;
  void weather_initialize (const Time& simulation_time, Treelog& msg)
  {
    std::ostringstream tmp;
    tmp << title () << ": initializing weather at "
	<< simulation_time.print ();
    Treelog::Open nest (msg, tmp.str ());
    top_level = true;
    initialize_one (msg);
    if (initialized_ok ())
      {
        Time mapped_time;
        if (data_end () == Time::null ())
          safe_end = Time::null ();
        else
          safe_end = data_end () - Timestep (2, 0, 0, 0);
        map_time (simulation_time, mapped_time, msg);
        initialize_two (mapped_time, msg);
      }
    top_level = false;
  }
  bool source_check (Treelog& msg) const
  { 
    bool ok = true;
    if (!top_level)
      {
        msg.error ("\
The '" + objid + "' model cannot be used here.  Try 'table' instead");
        ok = false;
      }
    if (!WSourceTable::source_check (msg))
      ok = false;
    return ok; 
  }
  bool weather_check (const Time& simulation_from, const Time& simulation_to,
                      Treelog& msg) const
  {
    TREELOG_MODEL (msg);

    if (!initialized_ok ())
      return false;

    if (safe_end <= data_begin ())
      {
        msg.error ("Need more weather data at end of file");
        return false;
      }
    Time mapped_from;
    map_time_only (simulation_from, mapped_from, msg);
    Time mapped_to;
    if (simulation_to == Time::null ())
      mapped_to = Time::null ();
    else
      map_time_only (simulation_to, mapped_to, msg);
    return WSourceTable::weather_check (mapped_from, mapped_to, msg);
  }


  WSourceStandard (const BlockModel& al)
    : WSourceTable (al),
      missing_years (map_submodel_const<YearMap> (al, "missing_years")),
      active_map (-1),
      top_level (false)
  { }
  ~WSourceStandard ()
  { }
};

bool
WSourceStandard::YearMap::YearInterval::check_alist (const Metalib&,
                                                     const Frame& al,
						     Treelog& err)
{
  bool ok = true;

  const int from = al.integer ("from");
  const int to = al.integer ("to");
  if (from > to)
    {
      std::ostringstream tmp;
      tmp << "Start year " << from << " comes after end year " << to;
      err.error (tmp.str ());
      ok = false;
    }
  return ok;
}

void 
WSourceStandard::YearMap::YearInterval::load_syntax (Frame& frame)
{
  frame.add_check (check_alist);
  frame.declare_integer ("from", Attribute::Const,
	      "First year of interval.");
  frame.set_check ("from", VCheck::valid_year ());
  frame.declare_integer ("to", Attribute::Const,
	      "First year of interval.");
  frame.set_check ("to", VCheck::valid_year ());
  frame.order ("from", "to");
}

WSourceStandard::YearMap::YearInterval::YearInterval (const FrameSubmodel& al)
  : from (al.integer ("from")),
    to (al.integer ("to"))
{ }
    
bool WSourceStandard::YearMap::contain (const Time& time) const
{
  const int year = time.year ();
  return from.from <= year && year <= from.to;
}

void WSourceStandard::YearMap::map_time (Time& time) const
{ time.tick_year (to.from - from.from); }

bool
WSourceStandard::YearMap::check_alist (const Metalib&, const Frame& al, Treelog& msg)
{
  bool ok = true;
  const YearInterval from (al.submodel ("from"));
  const YearInterval to (al.submodel ("to"));
  
  if (from.size () != to.size ())
    {
      std::ostringstream tmp;
      tmp << "You cannot map " << from.size () << " years to "
	     << to.size () << " years";
      msg.error (tmp.str ());
      ok = false;
    }
  return ok;
}

void 
WSourceStandard::YearMap::load_syntax (Frame& frame)
{ 
  frame.add_check (check_alist);
  frame.declare_submodule ("from", Attribute::Const, 
			"Interval of years to map from.",
			YearInterval::load_syntax);
  frame.declare_submodule ("to", Attribute::Const, 
			"Interval of years to map to.",
			YearInterval::load_syntax);
  frame.order ("from", "to");
}

WSourceStandard::YearMap::YearMap (const Block& al)
  : from (al.submodel ("from")),
    to (al.submodel ("to"))
{ }

int 
WSourceStandard::find_map (const Time& time) const
{ 
  for (int i = 0; i < missing_years.size (); i++)
    if (missing_years[i]->contain (time))
      return i;
  return -1;
}

void
WSourceStandard::map_time (const Time& simulation_time, Time& mapped_time, 
                           Treelog& msg)
{
  TREELOG_MODEL (msg);
  bool find_new_map = false;
  bool reset_file = false;
  if (active_map >= 0)
    {
      if (simulation_time.between (data_begin (), safe_end))
        {
          msg.message ("Using current data");
          active_map = -1;
          reset_file = true;
        }
      else if (!missing_years[active_map]->contain (simulation_time))
        find_new_map = true;
    }
  else if (!simulation_time.between (data_begin (), safe_end))
    find_new_map = true;

  if (find_new_map)
    {
      active_map = find_map (simulation_time);
      if (active_map >= 0)
        {
          std::ostringstream tmp;
          tmp << "Using data from [" << missing_years[active_map]->to.from
              << "-" << missing_years[active_map]->to.to << "] for years ["
              << missing_years[active_map]->from.from << "-"
              << missing_years[active_map]->from.to << "]";
          msg.message (tmp.str ());
          reset_file = true;
        }
    }

  // Now.
  mapped_time = simulation_time;
  if (active_map >= 0)
    {
      missing_years[active_map]->map_time (mapped_time);
      if (!mapped_time.between (data_begin (), safe_end))
        {
          msg.error ("No mapped weather data for " + mapped_time.print ());
          mapped_time = simulation_time;
          ok = false;
        }
    }
  if (!mapped_time.between (data_begin (), safe_end))
    {
      msg.error ("No weather data for " + simulation_time.print ());
      ok = false;
    }

  // Initialize.
  if (reset_file)
    {
      lex.rewind ();
      rewind (mapped_time, msg);
    }
}

void
WSourceStandard::map_time_only (const Time& simulation_time, Time& mapped_time, 
                                Treelog& msg) const
{
  TREELOG_MODEL (msg);
  bool find_new_map = false;
  int my_map = active_map;
  if (my_map >= 0)
    {
      if (simulation_time.between (data_begin (), safe_end))
        {
          msg.message ("Using current data");
          my_map = -1;
        }
      else if (!missing_years[my_map]->contain (simulation_time))
        find_new_map = true;
    }
  else if (!simulation_time.between (data_begin (), safe_end))
    find_new_map = true;

  if (find_new_map)
    {
      my_map = find_map (simulation_time);
      if (my_map >= 0)
        {
          std::ostringstream tmp;
          tmp << "Using data from [" << missing_years[my_map]->to.from
              << "-" << missing_years[my_map]->to.to << "] for years ["
              << missing_years[my_map]->from.from << "-"
              << missing_years[my_map]->from.to << "]";
          msg.message (tmp.str ());
        }
    }

  // Now.
  mapped_time = simulation_time;
  if (my_map >= 0)
    {
      missing_years[my_map]->map_time (mapped_time);
      if (!mapped_time.between (data_begin (), safe_end))
        {
          msg.error ("No mapped weather data for " + mapped_time.print ());
          mapped_time = simulation_time;
        }
    }
  if (!mapped_time.between (data_begin (), safe_end))
    {
      msg.error ("No weather for " + simulation_time.print ());
    }
}

static struct WSourceStandardSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new WSourceStandard (al); }
  WSourceStandardSyntax ()
    : DeclareModel (WSource::component, "default", "table",
                    "Read weather data from specific file.")
  { }
  static bool check_alist (const Metalib&, const Frame& al, Treelog& msg)
  { 
    bool ok = true;
    return ok;
  }
  void load_frame (Frame& frame) const
  { 
    frame.add_check (check_alist);
    frame.order ("file");
    frame.declare_submodule_sequence ("missing_years", Attribute::Const, "\
How to get data for dates outside the range of the weather file.\n\
\n\
The value is a list of maps.  Each map consist of two intervals, and\n\
indicates that missing data from the first interval should be read\n\
from the second interval instead.  Each interval consists of two\n\
years, the first and last year of that interval.\n\
\n\
When the simulation requests weather data from a date outside the\n\
range covered by the weather file, the model will look up each member\n\
of the list, to see if the year is covered by the first interval.  If\n\
so, it will use weather data from the same day in the corresponding\n\
year in the second interval.\n\
\n\
If a given year is covered by multiple intervals in the list, the first\n\
one will be used.",
                                      WSourceStandard::YearMap::load_syntax);
    frame.set_empty ("missing_years");
  }
} WSourceStandard_syntax;

// wsource_std.C ends here.
