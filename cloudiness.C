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
#include "mathlib.h"
#include <sstream>

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

Cloudiness::Cloudiness (const BlockModel& al)
  : ModelDerived (al.type_name ())
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
  double index () const
  { return cloudiness; }

  // Create.
  bool check (const Weather&, Treelog& msg) const
  { return true; }
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

struct CloudinessWeather : public Cloudiness
{
  double cloudiness; 		// []
  
  // Simulation.
  void tick (const Weather& weather, Treelog&)
  { cloudiness = weather.cloudiness_index (); }

  double index () const
  { return cloudiness; }

  void output (Log& log) const
  { output_variable (cloudiness, log); }
  
  // Create.
  bool check (const Weather& weather, Treelog& msg) const
  {
    TREELOG_MODEL (msg);
    bool ok = true;
    if (!weather.has_cloudiness ())
      {
	msg.error ("No CloudinessIndex in weather file");
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
Cloudiness. 1 represent clear sky, 0 total eclipse.");
  }
} CloudinessWeather_syntax;

// The 'clear_sky' base model

struct CloudinessClear : public Cloudiness
{
  // Parameters.
  const double a;		// []
  const double min_rad;		// [W/m^2]
  const double min_sin_theta;	// [W/m^2]
  const double min_radiation_ratio; // []
  const double min_time_to_sunset;  // [h]
  const double min_time_from_sunrise;  // [h]
  
  // Log variables.
  double cloudiness;		// []
  double clear_sky_radiation;	// [W/m^2]
  
  // Simulation.
  void tick (const Weather& weather, Treelog& msg)
  {
    const double Si = weather.global_radiation ();
    const double rad = weather.extraterrestrial_radiation ();
    const double sin_theta = weather.sin_solar_elevation_angle ();
    const Time middle = weather.middle ();
    const double now = middle.day_fraction () * 24.0;
    const double sunrise = weather.sunrise ();
    const double sunset = sunrise + weather.day_length ();
    const bool too_early =(now < sunrise + min_time_from_sunrise);
    const bool too_late =(now > sunset - min_time_to_sunset);
    clear_sky_radiation = find_clear_sky_radiation (weather);
    const double Si0 = clear_sky_radiation;
    if (rad > min_rad && sin_theta > min_sin_theta
	&& !too_early && !too_late)
      {
	const double x = bound (min_radiation_ratio, Si / Si0, 1.0);
	cloudiness = a * x + 1 - a;
      }

#if 0
    std::ostringstream tmp;
    tmp << "rad = " << rad
	<< ", min_rad = " << min_rad
	<< ", sin_theta = " << sin_theta
	<< ", min_sin_theta = " << min_sin_theta
	<< ", now = " << now
	<< ", sunrise = " << sunrise
	<< ", min_time_from_sunrise = " << min_time_from_sunrise
	<< ", sunset = " << sunset
	<< ", min_time_to_sunset = " << min_time_to_sunset;
    msg.message (tmp.str ());
#endif

    // Otherwise, use value from previous timestep.
  }
  virtual double find_clear_sky_radiation (const Weather&) const = 0;
  
  double index () const
  { return cloudiness; }

  void output (Log& log) const
  {
    output_variable (cloudiness, log);
    output_variable (clear_sky_radiation, log);
  }

  // Create.
  bool check (const Weather&, Treelog& msg) const
  { return true; }
  CloudinessClear (const BlockModel& al)
    : Cloudiness (al),
      a (al.number ("a")),
      min_rad (al.number ("min_extraterrestrial_radiation")),
      min_sin_theta (sin (al.number ("min_solar_elevation_angle"))),
      min_radiation_ratio (sin (al.number ("min_radiation_ratio"))),
      min_time_to_sunset (al.number ("min_time_to_sunset")),
      min_time_from_sunrise (al.number ("min_time_from_sunrise",
					min_time_to_sunset)),
      cloudiness (al.number ("cloudiness", 0.5)),
      clear_sky_radiation (NAN)
  { }
};

static struct CloudinessClearSyntax : public DeclareBase
{
  CloudinessClearSyntax ()
    : DeclareBase (Cloudiness::component, "clear_sky", "\
Calculate based on clear sky radiation: a * Si / Si0 + 1 - a\n\
where Si is global radiation and Si0 is clear sky radiation.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare ("a", Attribute::None (), Attribute::Const, "\
Cloudiness coefficient.");
    frame.declare ("min_extraterrestrial_radiation",
		   "W/m^2", Attribute::Const, "\
Only calculate cloudiness when extraterrestrial radiation is above this.\n\
Otherwise, last value will be used.");
    frame.set ("min_extraterrestrial_radiation", 0.0);
    frame.declare ("min_global_radiation",
		   "W/m^2", Attribute::Const, "\
Only calculate cloudiness when global radiation is above this.\n\
Otherwise, last value will be used.");
    frame.set ("min_global_radiation", 0.0);
    frame.declare ("min_solar_elevation_angle", "rad", Attribute::Const, "\
Only calculate cloudiness when solar elevation angle is above this.\n\
Otherwise, last value will be used.");
    frame.set_cited ("min_solar_elevation_angle", 20.0 * M_PI / 360.0, "\
FAO56 suggest 2-3 hour before sunset, ASCE suggest 20 dg solar angle.",
		     "asce-sree");
    frame.declare_fraction ("min_radiation_ratio", Attribute::Const, "\
Lowest Si/Si0 ration for calculating cloudiness.");
    frame.set ("min_radiation_ratio", 0.3);
    frame.declare ("min_time_to_sunset", "h", Attribute::Const, "\
Don't calculate cloudiness after this time before sunset.");
    frame.set ("min_time_to_sunset", 0.0);
    frame.declare ("min_time_from_sunrise", "h", Attribute::OptionalConst, "\
Don't calculate cloudiness less than this time after sunrise.\n\
By default, equal to 'min_time_to_sunset'.");
    frame.declare_fraction ("cloudiness", Attribute::OptionalState, "\
Cloudiness.");
    frame.declare ("clear_sky_radiation", "W/m^2", Attribute::LogOnly, "\
Global radiation with clear sky conditions.");
  }
} CloudinessClear_syntax;

// The 'FAO56' model.

struct CloudinessFAO56 : public CloudinessClear
{
  double find_clear_sky_radiation (const Weather& weather) const
  {
    const double rad = weather.extraterrestrial_radiation ();
    const double z = weather.elevation ();
    return (0.75 + 2e-5 * z) * rad; // Eq 37, FAO56.
  }
  CloudinessFAO56 (const BlockModel& al)
    : CloudinessClear (al)
  { }
};
  
static struct CloudinessFAO56Syntax : public DeclareModel
{
  CloudinessFAO56Syntax ()
    : DeclareModel (Cloudiness::component, "FAO56", "clear_sky",
		    "Function of station height, Eq 37, FAO56.")
  { }
  Model* make (const BlockModel& al) const
  { return new CloudinessFAO56 (al); }
  void load_frame (Frame& frame) const
  {
    frame.set_strings ("cite", "FAO-PM");
    frame.set_cited ("a", 1.35, "Unnamed constant in equation 39.", "FAO-PM");
  }
} CloudinessFAO56_syntax;
  
// The 'Kjaersgaard' parameterization.

static struct CloudinessKjaersgaardSyntax : public DeclareParam
{
  CloudinessKjaersgaardSyntax ()
    : DeclareParam (Cloudiness::component, "Kjaersgaard", "FAO56",
		    "FAO56 calibrated for Taastrup, Denmark.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.set_strings ("cite", "kjaersgaard2007long");
    frame.set_cited ("a", 1.00, "\
Table 1, equation 6, calibrated value for Taastrup.",
		     "kjaersgaard2007long");
    frame.set ("min_solar_elevation_angle", 0.0);
    frame.set ("min_extraterrestrial_radiation", 25.0);
  }
} CloudinessKjaersgaard_syntax;

// The 'ASCE' model.

struct CloudinessASCE : public CloudinessClear
{
  // Parameters.
  const double Kt;		// Turbidity coefficient []
  
  // Log variable.
  mutable double W;		// Precipitable water in atm. [mm]
  
  double find_clear_sky_radiation (const Weather& weather) const
  {
    const double rad = weather.extraterrestrial_radiation (); // [W/m^2]
    const double P = weather.air_pressure () * 0.001;	      // [kPa]
    const double ea = weather.vapor_pressure () * 0.001;      // [kPa]
    W = 0.14 * ea * P + 2.1; // Eq D.3 [mm] Precipitable water in atm.
    const double sin_theta = weather.sin_solar_elevation_angle (); // []
    if (sin_theta < 0.0)
      return 0.0;
    const double KB		// Eq. D.2 [] Clearness index direct beam rad. 
      = 0.98 * std::exp (-0.00146 * P / Kt / sin_theta
			 -0.075 * std::pow (W / sin_theta, 0.4));
    const double KD		// Eq. D.4 [] Source index diffuse rad.
      = (KB >= 0.15)
      ? (0.35 - 0.36 * KB)
      : (0.18 + 0.82 * KB);
    return rad * (KB + KD);	// Eq. D.1
  }
  void output (Log& log) const
  {
    CloudinessClear::output (log);
    output_variable (W, log);
  }
  CloudinessASCE (const BlockModel& al)
    : CloudinessClear (al),
      Kt (al.number ("Kt")),
      W (NAN)
  { }
};
  
static struct CloudinessASCESyntax : public DeclareModel
{
  CloudinessASCESyntax ()
    : DeclareModel (Cloudiness::component, "ASCE", "clear_sky",
		    "Equation D.1 in the ASCE std ref evap report.")
  { }
  Model* make (const BlockModel& al) const
  { return new CloudinessASCE (al); }
  void load_frame (Frame& frame) const
  {
    frame.set_strings ("cite", "asce-sree");
    frame.declare_fraction ("Kt", Attribute::Const, "\
Turbidity coefficient, 1 = clean air, <= 0.5 extremely unclean.");
    frame.set ("Kt", 1.0);
    frame.declare ("W", "mm", Attribute::LogOnly, "\
Precipitable water in atmosphere.");
  }
} CloudinessASCE_syntax;

// The 'Taastrup' parameterization.

static struct CloudinessTaastrupSyntax : public DeclareParam
{
  CloudinessTaastrupSyntax ()
    : DeclareParam (Cloudiness::component, "Taastrup", "ASCE",
		    "ASCE calibrated for Taastrup, Denmark.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.set_strings ("cite", "kjaersgaard2007long");
    frame.set_cited ("a", 1.00, "\
Table 1, equation 6, calibrated value for Taastrup.",
		     "kjaersgaard2007long");
    frame.set ("min_solar_elevation_angle", 0.0);
    frame.set ("min_extraterrestrial_radiation", 25.0);
  }
} CloudinessTaastrup_syntax;

// cloudiness.C ends here.
