// groundwater_source.C
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
// Copyright 2010 KU.
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

#include "groundwater.h"
#include "source.h"
#include "time.h"
#include "assertion.h"
#include "block_model.h"
#include "librarian.h"
#include "frame.h"
#include <boost/scoped_ptr.hpp>
#include <sstream>

class GroundwaterSource : public Groundwater
{
  // Data.
private:
  boost::scoped_ptr<Source> source;
  size_t index;
  bool is_ok;
  const double offset;
  Time previous_time;
  Time next_time;
  double previous_depth;
  double next_depth;
  double depth;

  // Groundwater.
public:
  bottom_t bottom_type () const;
  double q_bottom (size_t) const
  { daisy_notreached (); }

  // Simulation.
public:
  void tick (const Units&, const Geometry&,
             const Soil&, SoilWater&, double, const SoilHeat&,
	     const Time& time, const Scope&, Treelog& msg)
  { tick (time, msg); }
  void tick (const Time&, Treelog&);
  double table () const;

  // Create and Destroy.
public:
  void initialize (const Units& units,
                   const Geometry&, const Time& time, const Scope&, Treelog&);
  bool check (const Units&, const Geometry&, const Scope&, Treelog&) const
  { return is_ok; }
  GroundwaterSource (const BlockModel&);
  ~GroundwaterSource ();
};

Groundwater::bottom_t
GroundwaterSource::bottom_type () const
{
  if (depth > 0)	     // Positive numbers indicate flux bottom.
    return free_drainage;
  else
    return pressure;
}

void
GroundwaterSource::tick (const Time& time, Treelog& msg)
{
  TREELOG_MODEL (msg);

  if (index > source->time ().size ())
    return;
  while (next_time < time)
    {
      // Remember old value.
      previous_time = next_time;
      previous_depth = next_depth;

      // Read in new value.
      if (index == source->time ().size ())
        {
          msg.error ("Out of time");
          return;
        }
      next_time = source->time ()[index];
      
      daisy_assert (index < source->value ().size ());
      next_depth =  source->value ()[index];
      if (next_depth > 0.0)
	msg.error ("positive depth, assuming free drainage");

      index++;
    }

  // We should be somewhere in the interval.
  daisy_assert (previous_time < time || time == previous_time);
  daisy_assert (time < next_time  || time == next_time);

  // Interpolate depth values.
  const double total_interval = Time::days_between (previous_time, next_time);
  const double covered_interval = Time::days_between (previous_time, time);
  const double covered_fraction = covered_interval / total_interval;
  const double total_change = next_depth - previous_depth;
  depth = previous_depth + covered_fraction * total_change;
}

double
GroundwaterSource::table () const
{
  return depth + offset;
}

void
GroundwaterSource::initialize (const Units&,
                               const Geometry&, const Time& time, const Scope&, 
                               Treelog& msg)
{
  std::ostringstream tmp;
  tmp << name << ": " << source->name << " '" << source->title () << "'";
  Treelog::Open nest (msg, tmp.str ());
  is_ok = true;
  if (!source->load (msg))
    is_ok = false;
  else if (source->value ().size () < 1)
    {
      msg.error ("No data in plot, ignoring");
      is_ok = false;
    }
  Time prev = time;
  prev.tick_hour (-1);
  tick (prev, msg); 
  tick (time, msg); 
}

GroundwaterSource::GroundwaterSource (const BlockModel& al)
  : Groundwater (al),
    source (Librarian::build_item<Source> (al, "source")),
    index (0),
    is_ok (false),
    offset (al.number ("offset")),
    previous_time (42, 1, 1, 0),
    next_time (42, 1, 1, 0),
    previous_depth (-42.42e42),
    next_depth (-42.42e42),
    depth (-42.42e42)
{ }

GroundwaterSource::~GroundwaterSource ()
{ }

static struct GroundwaterSourceSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new GroundwaterSource (al); }
  GroundwaterSourceSyntax ()
    : DeclareModel (Groundwater::component, "source", "common", "\
Read groundwater table from a source.")
  { }
  void load_frame (Frame& frame) const
  { 
    frame.declare_object ("source", Source::component, Attribute::State, 
                          Attribute::Variable, "\
Groundwater table time series.");
    frame.order ("source");
    frame.declare ("offset", "cm", Attribute::Const,
                   "Add this to depth from source.");
    frame.set ("offset", 0.0);
  }
} GroundwaterSource_syntax;

// groundwater_source.C ends here.
