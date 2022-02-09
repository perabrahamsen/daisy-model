// snow.C --- Simulate snowpack on surface.
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
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

#include "snow.h"
#include "frame_submodel.h"
#include "log.h"
#include "geometry.h"
#include "soil.h"
#include "soil_water.h"
#include "soil_heat.h"
#include "movement.h"
#include "librarian.h"
#include "mathlib.h"
#include "treelog.h"
#include <sstream>

struct Snow::Implementation
{ 
  // Flux variables.
  double EvapSnowPack;			// Evaporation from snowpack [mm/h]
  double q_s;			// Leaking water [mm/h]
  double temperature;		// temperature of leaking water [dg C]

  // State variables.
  double Ssnow;			// Snow storage expressed as water [mm]
  double Swater;		// Water in snow storage [mm]
  double age;			// Age since last snow [h]
  double dZs;			// Depth of snow layer [m]

  // Parameters.
  const double mf;		// Snow pack depth melting factor [1/m]
  const double mtprime;		// Air temperature melting factor 
				// [kg/m^2/h/dg C]
  const double mrprime;		// Radiation melting factor [kg/J]
  const double m1;		// Radiation melting linear factor [kg/J]
  const double m2;		// Radiation melting exponential factor [1/h]
  const double rho_s;		// Density of newly fallen snow [kg/m^3]
  const double f_c;		// Water capacity in snow factor
  const double rho_1;		// Water collapse factor [kg/m^3]
  const double rho_2;		// Snow collapse factor [1/m]
  const double Psa;		// Absolute amount of daily snow required
				// for snow to become new.
  const double fsa;		// Relative amount of snow required
				// for snow to become new.
  const double K_snow_factor;	// Factor related to thermal conductivity
				// for snow water mix [W m^5/kg^2/dg C]
  double partial_day; 		// Time this day [h]
  double partial_snow; 		// Snow this day [mm]
  double partial_precipitation;	// Precipitation this day [mm]

  void output (Log& log) const;
  void tick (Treelog&, const Movement&,
             const Soil&, const SoilWater&, const SoilHeat&,
	     double Si, double q_h, double Prain,
	     double Psnow, double Pond, double T, double Epot, double dt);
  Implementation (const FrameSubmodel& al);
};

Snow::Implementation::Implementation (const FrameSubmodel& al)
  : EvapSnowPack (0.0),
    q_s (0.0),
    Ssnow (al.number ("Ssnow")),
    Swater (al.number ("Swater")),
    age (al.number ("age")),
    dZs (al.number ("dZs")),
    mf (al.number ("mf")),
    mtprime (al.number ("mtprime")),
    mrprime (al.number ("mrprime")),
    m1 (al.number ("m1")),
    m2 (al.number ("m2")),
    rho_s (al.number ("rho_s")),
    f_c (al.number ("f_c")),
    rho_1 (al.number ("rho_1")),
    rho_2 (al.number ("rho_2")),
    Psa (al.number ("Psa")),
    fsa (al.number ("fsa")),
    K_snow_factor (al.number ("K_snow_factor")),
    partial_day (al.number ("partial_day")),
    partial_snow (al.number ("partial_snow")),
    partial_precipitation (al.number ("partial_precipitation"))
{ }

void 
Snow::Implementation::output (Log& log) const
{
  output_variable (EvapSnowPack, log);
  output_variable (q_s, log);
  output_variable (Ssnow, log);
  output_variable (Swater, log);
  output_variable (age, log);
  output_variable (dZs, log);
  output_variable (partial_day, log);
  output_variable (partial_snow, log);
  output_variable (partial_precipitation, log);
}

void
Snow::Implementation::tick (Treelog& msg,
                            const Movement& movement,
                            const Soil& soil, const SoilWater& soil_water,
			    const SoilHeat& soil_heat,
			    const double Si, const double q_h,
			    const double Prain, const double Psnow,
			    double Pond,
			    double T, const double Epot,
                            const double dt)
{
  // Equations from jansson1980soil.
  const double Ssnow_old = Ssnow;
  Pond = std::max (Pond, 0.0);
  Ssnow += Pond;
  Swater += Pond;

  daisy_assert (Si >= 0.0);
  daisy_assert (Prain >= 0.0);
  daisy_assert (Psnow >= 0.0);
  daisy_assert (Epot >= 0.0);
  daisy_assert (T > -374 && T <= 100);

  static const double f = 1.0;	// Melting factor. [mm H2O / (kg H2O / m^2)]
  static const double rho_w = 1.0e3; // Density of water. [kg/m^3]
  static const double rho_i = 0.917e3; // Density of ice. [kg/m^3]
  static const double Lm = 3.34e5; // Snow melting heat factor. [J/kg]

  // Total precipitation. [mm/h]
  const double P = Psnow + Prain;

  // Update snow age.
  partial_day += dt;
  partial_snow += Psnow * dt;
  partial_precipitation += P * dt;
  
  if (partial_day >= 24.0)
    // New day.
    {
      // Relative amount of snow in percolation.
      const double fs = (partial_precipitation > 0)
	? partial_snow / partial_precipitation
	: 1.0;
      daisy_assert (fs >= 0.0);

      if (partial_snow > Psa && fs > fsa)
	{
	  msg.message ("Fresh snow made snow pack white again");
	  age = 0.0;
	}
      
      // Reset.
      partial_day -= 24.0;
      partial_snow = 0.0;
      partial_precipitation = 0.0;
    }

  // We evaporate as much as we can.
  EvapSnowPack = std::min (Epot, Ssnow / dt + P);

  daisy_assert (EvapSnowPack >= 0.0);
  daisy_assert (EvapSnowPack <= Epot);
  daisy_assert (EvapSnowPack <= (Ssnow / dt + P) * 1.0001);

  // Depth of snow fallen this hour. [m]
  double dZp = 0.0;
  if (Psnow > 0.0)
    {
      // Density of snow-rain mixture. [kg/m^3]
      const double rho_p = rho_w + (rho_s - rho_w) * Psnow / P;
      daisy_assert (rho_p >= 0.0);
      dZp = P * dt / (f * rho_p);
    }
  daisy_assert (dZp >= 0.0);

  // Air temperature melting factor. [kg/J] Eq 46.
  const double mt = mtprime * ((T < 0.0 && (dZs + dZp) > 0.0) 
                               ? std::min (1.0, 1.00 / ((dZs + dZp) * mf)) 
                               : 1.0);
  daisy_assert (std::isfinite (mt));
  daisy_assert (mt <= mtprime);
  
  // Radiation melting factor. [kg/J]
  const double mr = mrprime * (1 + m1 * (1 - exp (-m2 * age)));

  const double s_per_h = 3600.0; // [s/h]
  
  // Potential snow melting. [mm/h]
  const double Mprime = (mt * T + (mr * Si + q_h / Lm) * s_per_h) * f;

  // Minimal possible melting (all water freezes). [mm/h]
  const double M1 = - (Swater/dt + Prain);
  
  // Maximal possible melting (all ice melts). [mm/h]
  const double M2 = (Ssnow - Swater)/dt + Psnow;

  // Actual melting. [mm/h]
  const double M = std::min (std::max (M1, Mprime), M2);

  // Evaporation from snow pack water. [mm/h]
  double Eprime = std::min (Swater / dt + Prain + M, EvapSnowPack);
  
  // Water storage capacity of snow [mm]
  const double Scapacity = f_c * Ssnow_old;
  daisy_assert (Scapacity >= 0.0);

  // We can now calculate how much water is leaking.
  q_s = std::max (0.0, Swater + (Prain - EvapSnowPack + M) * dt - Scapacity) / dt;
  daisy_assert (q_s >= 0.0);
  
  // New snow pack storage [mm].
  double Ssnow_new = Ssnow + (Psnow + Prain - EvapSnowPack - q_s) * dt;
  if (Ssnow_new < 0.0)
    {
      if (Ssnow_new < -1.0e-7)
	{
	  Treelog::Open nest (msg, "Snow");
	  std::ostringstream tmp;
	  tmp << "Lost " << -Ssnow_new << " mm from snow pack.";
	  msg.error (tmp.str ());
	}
      Ssnow_new = 0.0;
    }
  
  // New water content in snow pack [mm].
  double Swater_new = Swater + (Prain - Eprime + M - q_s) * dt;
  if (Swater_new < 0.0)
    {
      if (Swater_new < -1.0e-7)
	{
	  Treelog::Open nest (msg, "Snow");
	  std::ostringstream tmp;
	  tmp << "Lost " << -Swater_new << " mm water from snow pack.";
	  msg.error (tmp.str ());
	}
      Swater_new = 0.0;
    }
  if (Swater_new > Ssnow_new)
    {
      if (Swater_new > Ssnow_new + 1.0e-7)
	{
	  Treelog::Open nest (msg, "Snow");
	  std::ostringstream tmp;
	  tmp << "Removed " << (Swater_new - Ssnow_new) << " mm water.";
	  msg.error (tmp.str ());
	}
      Swater_new = Ssnow_new;
    }

  // Update the snow height,
  if (Ssnow_new > 0.0)
    {
      if (dZs > 0.0)
	{
	  // Density of collapsing snow pack [kg/m^3]
	  daisy_assert (Scapacity > 0.0);
	  const double rho_c
	    = std::max (Ssnow_old / (f * dZs), 
		   rho_s + rho_1 * Swater_new / Scapacity + rho_2 * Ssnow_old);
#if 0
	  cerr << "rho_c == " << rho_c << " Ssnow _old== " << Ssnow_old
	       << " dZs == " << dZs << " Swater_new == " << Swater_new
	       << " Scapacity == " << Scapacity << "\n";
#endif
	  daisy_assert (rho_c > 0.0);
	  //daisy_assert (rho_c < rho_i * 1.01);
	  daisy_assert (approximate (rho_c, Ssnow_old / (f * dZs))
		  || approximate (rho_c, rho_s 
				  + rho_1 * Swater_new / Scapacity 
				  + rho_2 * Ssnow_old));
	  // Size of collapsed snow pack [m]
	  const double dZc = Ssnow_old / (f * rho_c) + dZp;

	  // Factor in collapsing from passing melting water.
	  if (q_s * dt > Pond)
	    dZs = dZc * Ssnow_new / (Ssnow_new - Pond + q_s * dt);
	  else
	    dZs = dZc;

	}
      else
	dZs = dZp;
    }
  else
    dZs = 0.0;

  dZs = std::min (dZs, Ssnow_new / rho_s);
  dZs = std::max (dZs, Ssnow_new / rho_i);

  // Update snow storage.
  Ssnow = Ssnow_new;
  Swater = Swater_new;

  if (Ssnow > 0.1)
    age += dt;
  else
    age = 0.0;
  
  

  // Update temperature.
  if (q_s > 1.0e-20)
    // There is water leaking through the snow pack.  
    // Assume it is 0 degrees.
    T = 0.0;
  else if (dZs > 0.01)
    {
      // Density and conductivity of snowpack.
      const double rho = Ssnow / dZs; // [kg/m^3]
      const double K_snow = K_snow_factor * rho * rho; // [W/m^2]
      
      const double T_surface 
        = movement.surface_snow_T (soil, soil_water, soil_heat, 
                                   T, K_snow, dZs);

      T = std::min (T_surface, 0.0);
      daisy_assert (T > -100.0);
      daisy_assert (T < 50.0);
    } 
  temperature = T;

  q_s -= Pond / dt;
}
  
void 
Snow::tick (Treelog& msg, const Movement& movement,
            const Soil& soil, const SoilWater& soil_water,
	    const SoilHeat& soil_heat,
	    double Si, double q_h, double Prain,
	    double Psnow, double Pond, double T, double Epot, const double dt)
{
  if (impl.Ssnow > 0.0 || Psnow > 0.0)
    impl.tick (msg, movement, soil, soil_water, soil_heat, Si, q_h,
	       Prain, Psnow,
	       Pond,
	       T, Epot, dt);
  else
    {
      impl.EvapSnowPack = 0.0;
      impl.q_s = Prain;
      impl.temperature = T;
      impl.age = 0.0;
      impl.dZs = 0.0;
      impl.partial_day = 0.0;
      impl.partial_snow = 0.0;
      impl.partial_precipitation = 0.0;
    }
}

void 
Snow::output (Log& log) const
{
  impl.output (log);
}

double 
Snow::percolation () const
{
  return impl.q_s;
}

double 
Snow::temperature () const
{
  return impl.temperature;
}

double 
Snow::evaporation () const
{
  return impl.EvapSnowPack;
}

double 
Snow::storage () const
{
  return impl.Ssnow;
}

void
Snow::load_syntax (Frame& frame)
{ 

  static const double hours_per_day = 24.0; // [h/d]

  // frame.set_strings ("cite", "snow1956", "jansson1980soil"); // Not implemented for submodels.
  frame.declare ("EvapSnowPack", "mm/h", Attribute::LogOnly, 
	      "Evaporation from snowpack.");
  frame.declare ("q_s", "mm/h", Attribute::LogOnly,
	      "Leaking water.");
  frame.declare ("Ssnow", "mm", Attribute::State,
	      "Snow storage expressed as water.");
  frame.set ("Ssnow", 0.0);
  frame.declare ("Swater", "mm", Attribute::State, 
	      "Water in snow storage.");
  frame.set ("Swater", 0.0);
  frame.declare ("age", "h", Attribute::State,
	      "Time since last snow.");
  frame.set ("age", 0.0);
  frame.declare ("dZs", "m", Attribute::State,
	      "Depth of snow layer.");
  frame.set ("dZs", 0.0);
  frame.declare ("mf", "m^-1", Attribute::Const,
	      "Snow pack depth melting factor.");
  frame.set ("mf", 10.0);
  frame.declare ("mtprime", "kg/m^2/h/dg C", Attribute::Const,
	      "Air temperature melting factor.");
  frame.set ("mtprime", 2.0 / hours_per_day);
  frame.declare ("mrprime", "kg/J", Attribute::Const,
	      "Radiation melting factor.");
  frame.set ("mrprime", 1.5e-7);
  frame.declare ("m1", Attribute::None (), Attribute::Const,
	      "Radiation melting linear.");
  frame.set ("m1", 2.0);
  frame.declare ("m2", "h^-1", Attribute::Const, 
	      "Radiation melting exponential factor.");
  frame.set ("m2", 0.1 / hours_per_day);
  frame.declare ("rho_s", "kg/m^3", Attribute::Const,
	      "Density of newly fallen snow.");
  frame.set ("rho_s", 100.0);
  frame.declare ("f_c", Attribute::None (), Attribute::Const,
	      "Water capacity in snow factor.");
  frame.set ("f_c", 0.07);
  frame.declare ("rho_1", "kg/m^3", Attribute::Const,
	      "Water collapse factor.");
  frame.set ("rho_1", 200.0);
  frame.declare ("rho_2", "m^-1", Attribute::Const, 
	      "Snow collapse factor.");
  frame.set ("rho_2", 0.5);
  frame.declare ("Psa", "mm", Attribute::Const, "\
Amount of snow in a day required for snow to become new.");
  frame.set ("Psa", 5.0);
  frame.declare_fraction ("fsa", Attribute::Const, "\
Fraction of snow in precipitation required for snow to become new.");
  frame.set ("fsa", 0.9);
  frame.declare ("K_snow_factor", "W m^5/kg^2/dg C", Attribute::Const,
	      "Factor related to thermal conductivity for snow water mix.");
  frame.set ("K_snow_factor", 2.86e-6);
  frame.declare ("partial_day", "h", Attribute::State, "\
Time since last day.\n\
Snow age is reset when at least 'Psa' is receieved within a day.");
  frame.set ("partial_day", 0.0);
  frame.declare ("partial_snow", "mm", Attribute::State, "\
Amount of snow received this day so far.\n\
Snow age is reset when at least 'Psa' is receieved within a day.");
  frame.set ("partial_snow", 0.0);
  frame.declare ("partial_precipitation", "mm", Attribute::State, "\
Amount of precipitation received this day so far.\n\
At least 'fsa' of this must be snow for snow age to be reset.");
  frame.set ("partial_precipitation", 0.0);
}
  
Snow::Snow (const FrameSubmodel& al)
  : impl (*new Implementation (al))
{ }

Snow::~Snow ()
{ delete &impl; }

static DeclareSubmodel snow_submodel (Snow::load_syntax, "Snow", "\
Simulate snow pack on surface.\n\
_Snow Hydrology_, U.S. Corps of Engineers, 1956.");

// snow.C ends here.
