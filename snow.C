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


#include "snow.h"
#include "alist.h"
#include "syntax.h"
#include "log.h"
#include "geometry.h"
#include "soil.h"
#include "soil_water.h"
#include "soil_heat.h"
#include "submodel.h"
#include "mathlib.h"
#include <sstream>

using namespace std;

struct Snow::Implementation
{ 
  // Flux variables.
  double EvapSnowPack;			// Evaporation from snowpack [mm]
  double q_s;			// Leaking water [mm]
  double temperature;		// temperature of leaking water [dg C]

  // State variables.
  double Ssnow;			// Snow storage expressed as water [mm]
  double Swater;		// Water in snow storage [mm]
  double age;			// Age since last snow [h]
  double dZs;			// Depth of snow layer [m]

  // Parameters.
  const double mf;		// Snow pack depth melting factor [1/m]
  const double mtprime;		// Air temperature melting factor 
				// [kg/m²/h C]
  const double mrprime;		// Radiation melting factor [kg/J]
  const double m1;		// Radiation melting linear factor [kg/J]
  const double m2;		// Radiation melting exponential factor [1/h]
  const double rho_s;		// Density of newly fallen snow [kg/m³]
  const double f_c;		// Water capacity in snow factor
  const double rho_1;		// Water collapse factor [kg/m³]
  const double rho_2;		// Snow collapse factor [1/m]
  const double Psa;		// Absolute amount of snow required
				// for snow to become new.
  const double fsa;		// Relative amount of snow required
				// for snow to become new.
  const double K_snow_factor;	// Factor related to thermal conductivity
				// for snow water mix [W m^5/kg^2/dg C]

  void output (Log& log) const;
  void tick (Treelog&, const Geometry& geo,
             const Soil& soil, const SoilWater& soil_water,
	     const SoilHeat& soil_heat,
	     double Si, double q_h, double Prain,
	     double Psnow, double Pond, double T, double Epot);
  Implementation (const AttributeList& al);
};

Snow::Implementation::Implementation (const AttributeList& al)
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
    K_snow_factor (al.number ("K_snow_factor"))
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
}

void
Snow::Implementation::tick (Treelog& msg,
			    const Geometry& geo, 
                            const Soil& soil, const SoilWater& soil_water,
			    const SoilHeat& soil_heat,
			    const double Si, const double q_h,
			    const double Prain, const double Psnow,
			    double Pond,
			    double T, const double Epot)
{ 
  const double Ssnow_old = Ssnow;
  Pond = max (Pond, 0.0);
  Ssnow += Pond;
  Swater += Pond;

  daisy_assert (Si >= 0.0);
  daisy_assert (Prain >= 0.0);
  daisy_assert (Psnow >= 0.0);
  daisy_assert (Epot >= 0.0);
  daisy_assert (T > -374 && T <= 100);

  static const double dt = 1.0; // Time step [h].
  static const double f = 1.0;	// Melting factor. [mm H2O / (kg H2O / m²)]
  static const double rho_w = 1.0e3; // Density of water. [kg / m³]
  static const double rho_i = 0.917e3; // Density of ice. [kg / m³]
  static const double Lm = 3.34e5; // Snow melting heat factor. [J/kg]

  // Total precipitation. [mm/h]
  const double P = Psnow + Prain;
  
  // Relative amount of snow in percolation.
  const double fs = (P > 0) ? Psnow / P : 0.0;
  daisy_assert (fs >= 0.0);

  // Check if snow has become white.
  if (Prain > 0.0)
    {
      if (Psnow > Psa && fs > fsa) 
	age = 0;
    }
  else
    {
      if (Psnow > Psa) 
	age = 0;
    }

  // We evaporate as much as we can.  
  EvapSnowPack = min (Epot, Ssnow / dt + P);

  daisy_assert (EvapSnowPack >= 0.0);
  daisy_assert (EvapSnowPack <= Epot);
  daisy_assert (EvapSnowPack <= (Ssnow / dt + P) * 1.0001);

  // Depth of snow fallen this hour. [m]
  double dZp = 0.0;
  if (Psnow > 0.0)
    {
      // Density of snow-rain mixture. [kg/m³]
      const double rho_p = rho_w + (rho_s - rho_w) * Psnow / P;
      daisy_assert (rho_p >= 0.0);
      dZp = P * dt / (f * rho_p);
    }
  daisy_assert (dZp >= 0.0);

  // Air temperature melting factor. [kg/J]
  const double mt = mtprime * ((T < 0.0) ? min (1.0, (dZs + dZp) * mf) : 1);
  
  // Radiation melting factor. [kg/J]
  const double mr = mrprime * (1 + m1 * (1 - exp (-m2 * age)));

  // Potential snow melting. [mm/h]
  const double Mprime = (mt * T + mr * Si + q_h / Lm) * f;

  // Minimal possible melting (all water freezes). [mm/h]
  const double M1 = - (Swater/dt + Prain);
  
  // Maximal possible melting (all ice melts). [mm/h]
  const double M2 = (Ssnow - Swater)/dt + Psnow;

  // Actual melting. [mm/h]
  const double M = min (max (M1, Mprime), M2);

  // Evaporation from snow pack water. [mm/h]
  double Eprime = min (Swater / dt + Prain + M, EvapSnowPack);
  
  // Water storage capacity of snow [mm]
  const double Scapacity = f_c * Ssnow_old;
  daisy_assert (Scapacity >= 0.0);

  // We can now calculate how much water is leaking.
  q_s = max (0.0, Swater + (Prain - EvapSnowPack + M) * dt - Scapacity) / dt;
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
	  // Density of collapsing snow pack [kg/m³]
	  daisy_assert (Scapacity > 0.0);
	  const double rho_c
	    = max (Ssnow_old / (f * dZs), 
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
	  if (q_s > Pond)
	    dZs = dZc * Ssnow_new / (Ssnow_new + (q_s - Pond) * dt);
	  else
	    dZs = dZc;

	}
      else
	dZs = dZp;
    }
  else
    dZs = 0.0;

  dZs = min (dZs, Ssnow_new / rho_s);
  dZs = max (dZs, Ssnow_new / rho_i);

  // Update snow storage.
  Ssnow = Ssnow_new;
  Swater = Swater_new;

  // Update snow age.
  if (Ssnow > 0.0)
    age++;

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
        = soil_heat.T_surface_snow (geo, soil, soil_water, T, K_snow, dZs);

      T = min (T_surface, 0.0);
      daisy_assert (T > -100.0 && T < 50.0);
    } 
  temperature = T;

  q_s -= Pond;
}
  
void 
Snow::tick (Treelog& msg, const Geometry& geo,
            const Soil& soil, const SoilWater& soil_water,
	    const SoilHeat& soil_heat,
	    double Si, double q_h, double Prain,
	    double Psnow, double Pond, double T, double Epot)
{
  if (impl.Ssnow > 0.0 || Psnow > 0.0)
    impl.tick (msg, geo, soil, soil_water, soil_heat, Si, q_h,
	       Prain, Psnow, Pond, T, Epot);
  else
    {
      impl.EvapSnowPack = 0.0;
      impl.q_s = Prain;
      impl.temperature = T;
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
Snow::load_syntax (Syntax& syntax, AttributeList& alist)
{ 
  alist.add ("submodel", "Snow");
  alist.add ("description", "Simulate snow pack on surface.\n\
_Snow Hydrology_, U.S. Corps of Engineers, 1956.");

  static const double hours_per_day = 24.0; // [h/d]

  syntax.add ("EvapSnowPack", "mm", Syntax::LogOnly, 
	      "Evaporation from snowpack.");
  syntax.add ("q_s", "mm", Syntax::LogOnly,
	      "Leaking water.");
  syntax.add ("Ssnow", "mm", Syntax::State,
	      "Snow storage expressed as water.");
  alist.add ("Ssnow", 0.0);
  syntax.add ("Swater", "mm", Syntax::State, 
	      "Water in snow storage.");
  alist.add ("Swater", 0.0);
  syntax.add ("age", "h", Syntax::State,
	      "Time since last snow.");
  alist.add ("age", 0.0);
  syntax.add ("dZs", "m", Syntax::State,
	      "Depth of snow layer.");
  alist.add ("dZs", 0.0);
  syntax.add ("mf", "m^-1", Syntax::Const,
	      "Snow pack depth melting factor.");
  alist.add ("mf", 10.0);
  syntax.add ("mtprime", "kg/m^2/h C", Syntax::Const,
	      "Air temperature melting factor.");
  alist.add ("mtprime", 2.0 / hours_per_day);
  syntax.add ("mrprime", "kg/J", Syntax::Const,
	      "Radiation melting factor.");
  alist.add ("mrprime", 1.5e-7);
  syntax.add ("m1", "kg/J", Syntax::Const,
	      "Radiation melting linear.");
  alist.add ("m1", 2.0);
  syntax.add ("m2", "h^-1", Syntax::Const, 
	      "Radiation melting exponential factor.");
  alist.add ("m2", 0.1 / hours_per_day);
  syntax.add ("rho_s", "kg/m^3", Syntax::Const,
	      "Density of newly fallen snow.");
  alist.add ("rho_s", 100.0);
  syntax.add ("f_c", Syntax::None (), Syntax::Const,
	      "Water capacity in snow factor.");
  alist.add ("f_c", 0.07);
  syntax.add ("rho_1", "kg/m^3", Syntax::Const,
	      "Water collapse factor.");
  alist.add ("rho_1", 200.0);
  syntax.add ("rho_2", "m^-1", Syntax::Const, 
	      "Snow collapse factor.");
  alist.add ("rho_2", 0.5);
  syntax.add ("Psa", "mm", Syntax::Const, 
	      "Absolute amount of snow required for snow to become new.");
  alist.add ("Psa", 5.0 / hours_per_day);
  syntax.add ("fsa", Syntax::None (), Syntax::Const, 
	      "Relative amount of snow required for snow to become new.");
  alist.add ("fsa", 0.9);
  syntax.add ("K_snow_factor", "W m^5/kg^2/dg C", Syntax::Const,
	      "Factor related to thermal conductivity for snow water mix.");
  alist.add ("K_snow_factor", 2.86e-6);
}
  
Snow::Snow (const AttributeList& al)
  : impl (*new Implementation (al))
{ }

Snow::~Snow ()
{ delete &impl; }

static Submodel::Register snow_submodel ("Snow", Snow::load_syntax);
