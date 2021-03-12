// cloudiness.C  -- Calculation of cloudiness.
// 
// Copyright 2020, 2021 KU
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

#include "cloudiness.h"
#include "block_model.h"
#include "librarian.h"
#include "weather.h"
#include "log.h"

// The 'cloudiness' component.

const char *const Cloudiness::component = "cloudiness";

symbol 
Cloudiness::library_id () const
{
  static const symbol id (component);
  return id;
}

void
Cloudiness::output (Log&) const
{ }

bool
Cloudiness::check (const Weather&, Treelog&) const
{ return true; }

Cloudiness::Cloudiness (const BlockModel& al)
  : ModelFramed (al)
{ }

Cloudiness::~Cloudiness ()
{ }

static struct CloudinessInit : public DeclareComponent 
{
  void load_frame (Frame& frame) const
  { Model::load_model (frame); }
  CloudinessInit ()
    : DeclareComponent (Cloudiness::component, "\
Calculate cloudiness from meterological data.")
  { }
} Cloudiness_init;

// The 'const' model.

struct CloudinessConst : public Cloudiness
{
  const double cloudiness;
  
  // Simulation.
  void tick (const Weather&, Treelog&)
  { }
  double value () const
  { return cloudiness; }

  // Create.
  CloudinessConst (const BlockModel& al)
    : Cloudiness (al),
      cloudiness (al.number ("cloudiness"))
  { }
};

static struct CloudinessConstSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new CloudinessConst (al); }
  CloudinessConstSyntax ()
    : DeclareModel (Cloudiness::component, "const", 
		    "The cloudiness never changes.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare_fraction ("cloudiness", Attribute::Const, "\
The cloudiness never changes.");
    frame.order ("cloudiness");
  }
} CloudinessConst_syntax;

// The 'weather' model

#if 0
struct CloudinessWeather : public Cloudiness
{
  double cloudiness; 		// []
  
  // Simulation.
  void tick (const Weather& weather, Treelog&)
  { cloudiness = weather.cloudiness (); }

  double value () const
  { return cloudiness; }

  void output (Log& log) const
  { output_variable (cloudiness, log); }
  
  // Create.
  bool check (const Weather& weather, Treelog& msg)
  {
    TREELOG_MODEL (msg);
    
    bool ok = true;
    if (!weather.has_cloudiness ())
      {
	msg.error ("No cloudiness in weather file");
	ok = false;
      }
    return ok;
  }
  CloudinessWeather (const BlockModel& al)
    : Cloudiness (al)
  { }
};

static struct CloudinessWeatherSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new CloudinessWeather (al); }
  CloudinessWeatherSyntax ()
    : DeclareModel (Cloudiness::component, "weather", 
		    "Use value from weather file.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare_fraction ("cloudiness", Attribute::LogOnly, "\
Cloudiness.");
  }
} CloudinessWeather_syntax;
#endif

// The 'clear_sky' base model

struct CloudinessClear : public Cloudiness
{
  // Parameter.
  const double a;		// []
  const double min_rad;		// [W/m^2]
  
  // Lo
  double cloudiness;		// []

  // Simulation.
  void tick (const Weather& weather, Treelog&)
  {
    const double Si = weather.global_radiation ();
    const double rad = weather.extraterrestrial_radiation ();
    if (rad > min_rad)
      {
	const double x = Si / 0.75 / rad;
	cloudiness = (a * std::min (1.0, x) + 1 - a);
      }
  }

  double value () const
  { return cloudiness; }

  void output (Log& log) const
  { output_variable (cloudiness, log); }

  // Create.
  CloudinessClear (const BlockModel& al)
    : Cloudiness (al),
      a (al.number ("a")),
      min_rad (al.number ("min_rad")),
      cloudiness (al.number ("cloudiness"))
  { }
};

static struct CloudinessClearSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new CloudinessClear (al); }
  CloudinessClearSyntax ()
    : DeclareModel (Cloudiness::component, "clear", 
		    "The traditional function used by Daisy.\n\
a = 1.00; x = Si / 0.75 / rad; cfh = (a * std::min (1.0, x) + 1 - a)")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare ("a", Attribute::None (), Attribute::Const, "\
Cloudiness coefficient.");
    frame.declare ("min_rad", "W/m^2", Attribute::Const, "\
Only calculate cloudiness when extraterrestrial radiation is above this.\n\
Otherwise, lasat value will be used.");
    frame.declare_fraction ("cloudiness", Attribute::State, "\
Cloudiness.");
      
  }
} CloudinessClear_syntax;

// cloudiness.C ends here.
