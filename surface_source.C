// surface_source.C -- Upper boundary from source.
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
// Copyright 2022 UCPH
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

#include "surface_simple.h"
#include "source.h"
#include "units.h"
#include "librarian.h"
#include "block_model.h"
#include "assertion.h"
#include "time.h"
#include "timestep.h"
#include "mathlib.h"
#include <sstream>

// The 'source_flux' model.

struct SurfaceSourceFlux : public SurfaceSimple
{
  const Units& units;
  const bool interpolate;	//  Interpolate flux values.
  const std::unique_ptr<Source> source;
  size_t index;
  enum { uninitialized, working, error } state;
  Time previous_time;
  Time next_time;
  double previous_flux;
  double next_flux;
  double flux;

  top_t top_type (const Geometry&, size_t edge) const
  { return forced_flux; }
  double q_top (const Geometry&, size_t edge, const double dt) const // [cm/h]
  { return flux; }
  double h_top (const Geometry& geo, size_t edge) const // [cm]
  {
    const double dt = 1.0;       // [h]
    return -q_top (geo, edge, dt) * dt; 
  }

  void tick (const Time& time, double dt /* [h] */,
             double PotSoilEvaporationWet, 
             double PotSoilEvaporationDry, 
             double flux_in /* [mm/h] */,
             double temp /* [dg C] */, const Geometry& geo,
             const Soil& soil, const SoilWater& soil_water,
             double soil_T /* [dg C] */,
	     Treelog& msg)
  {
    SurfaceSimple::tick (time, dt, PotSoilEvaporationDry, PotSoilEvaporationWet,
			 flux_in, temp, geo, soil, soil_water,
			 soil_T, msg);
    tick_source (time, msg);
  }

  void tick_source (const Time& time, Treelog& msg)
  {
    TREELOG_MODEL (msg);

    if (state == uninitialized)
      // Initialize
      {
	std::ostringstream tmp;
	tmp << objid << ": " << source->objid << " '" << source->title () << "'";
	Treelog::Open nest (msg, tmp.str ());
	state = working;
	if (!source->load (msg))
	  state = error;
	else if (source->value ().size () < 1)
	  {
	    msg.error ("No data in plot, ignoring");
	    state = error;
	  }
	Time prev = time;
	prev.tick_hour (-1);
	tick_source (prev, msg); 
	tick_source (time, msg); 
	
	if (!units.can_convert (source->dimension (), Units::cm_per_h (), msg))
	  state = error;
      }
    if (state == error)
      // Error state.
      return;

    if (index > source->time ().size ())
      return;
    while (next_time < time)
      {
	// Remember old value.
	previous_time = next_time;
	previous_flux = next_flux;

	// Read in new value.
	if (index == source->time ().size ())
	  {
	    msg.error ("Out of time");
	    return;
	  }
	next_time = source->time ()[index];
      
	daisy_assert (index < source->value ().size ());
	next_flux = units.convert (source->dimension (), Units::cm_per_h (),
				    source->value ()[index]);
      
	index++;
      }

    // We should be somewhere in the interval.
    daisy_assert (previous_time < time || time == previous_time);
    daisy_assert (time < next_time  || time == next_time);

    if (interpolate)
      {
	// Interpolate flux values.
	const double total_interval
	  = (next_time - previous_time).total_hours ();
	if (total_interval < 1e-6)
	  {
	    msg.error ("Bad time interval: " + previous_time.print ()
		       + " to " + next_time.print ());
	    return;                   // Reuse last flux.
	  }
	const double covered_interval = (time - previous_time).total_hours ();
	const double covered_fraction = covered_interval / total_interval;
	daisy_assert (std::isfinite (previous_flux));
	daisy_assert (std::isfinite (next_flux));
	const double total_change = next_flux - previous_flux;
	flux = previous_flux + covered_fraction * total_change;
      }
    else if (time < next_time)
      flux = previous_flux;
    else
      flux = next_flux;
  }

  // Create.
  SurfaceSourceFlux (const BlockModel& al)
    : SurfaceSimple (al),
      units (al.units ()),
      interpolate (al.flag ("interpolate")),
      source (Librarian::build_item<Source> (al, "source")),
      index (0),
      state (uninitialized),
      previous_time (42, 1, 1, 0),
      next_time (42, 1, 1, 0),
      previous_flux (0.0),
      next_flux (0.0),
      flux (0.0)
  { }
  ~SurfaceSourceFlux ()
  { }
};

static struct SurfaceSourceFluxSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new SurfaceSourceFlux (al); }

  SurfaceSourceFluxSyntax () 
    : DeclareModel (Surface::component, "source_flux", "simple", "\
Dynamic flux upper boundary for soil.")
  { }

  void load_frame (Frame& frame) const
  {
    frame.declare_object ("source", Source::component, Attribute::Const, 
                          Attribute::Singleton, "\
Flux time series.");
    frame.declare_boolean ("interpolate", Attribute::Const, "\
If true, interpolate values.  If false, use last read.");
    frame.set ("interpolate", false);
  }
} SurfaceSourceFlux_syntax;


// surface_source.C ends here.
