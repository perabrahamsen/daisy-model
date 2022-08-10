// fao.C --- FAO utility functions.
// 
// Copyright 1996-2002,2003 Per Abrahamsen and Søren Hansen
// Copyright 2000,2001,2003 KVL.
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

#include "fao.h"
#include "assertion.h"
#include "treelog.h"
#include "mathlib.h"
#include "librarian.h"
#include <sstream>

double 
FAO::CanopyResistance (const double LAI, const double rs_min)
{ 
  daisy_assert (LAI > 0.0);
  return (rs_min / LAI); 
}

double 
FAO::RefCanopyResistance (void)
{ return 70.0; }

double 
FAO::ZeroPlaneDisplacement (double CropHeight)
{ return (0.66 * CropHeight / 100.0); }

double 
FAO::RoughnessHeight_Momentum (double CropHeight)
{ return (0.123 * CropHeight / 100.0); }

double
FAO::RoughnessHeight_Heat (double CropHeight)
{ return (0.0123 * CropHeight / 100.0); }

double 
FAO::AerodynamicResistance (double CropHeight, double ScreenHeight,
			    double U)
{
  if (iszero (U))
    U = 0.1;

  daisy_assert (U > 0.0);

  double Zom, Zoh;

  if (CropHeight <= 0) {
    Zom = 0.01;
    Zoh = 0.001;
  } else {
    Zom = RoughnessHeight_Momentum (CropHeight);
    Zoh = RoughnessHeight_Heat (CropHeight);
  }
  const double Z = ScreenHeight - ZeroPlaneDisplacement (CropHeight);
  return (log (Z / Zom) * log (Z / Zoh) / (0.41 * 0.41 * U));
}

double 
FAO::RefAerodynamicResistance (double U2)
{ 
  if (iszero (U2))
    U2 = 0.1;

  daisy_assert (U2 > 0.0);

  return (208. / U2); 
}

double
FAO::LatentHeatVaporization (double Temp) // [J/kg]
{
  const double value = ((2.501 - 2.361e-3 * Temp) * 1.0e6);
  daisy_assert (std::isfinite (value));
  return value;
}

double
FAO::PsychrometricConstant (double AtmPressure, double Temp) // [Pa/K]
{
  daisy_assert (std::isfinite (AtmPressure));
  return (1.63e3 * AtmPressure / LatentHeatVaporization (Temp));
}

double
FAO::AirDensity (double AtmPressure, double Temp) // [kg/m3]
{
#if 1
  // Unit problem, gives approximately same density as water.
  const double Tvirtuel = 1.01 * (Temp + 273);
  return (3.486 * AtmPressure * 1e-3 /* [Pa->kPa] */ / Tvirtuel);
#else
  return Resistance::rho_a (Temp, AtmPressure);
#endif
}

double
FAO::SaturationVapourPressure (double Temp) // [Pa]
{ return (611.0 * exp (17.27 * Temp / (Temp + 237.3))); }

double
FAO::SlopeVapourPressureCurve (double Temp) // [Pa/K]
{ return (4.098E3 * SaturationVapourPressure (Temp) / pow (Temp + 237.3, 2)); }

double
FAO::AtmosphericPressure (double elevation) // [Pa]
{ return (101300. * pow ((293 - 0.0065 * elevation) / 293.0, 5.26)); }

double
FAO::CloudinessFactor_Arid (double Si, double rad)
{
  const double a = 1.35;
  const double x = Si / 0.75 / rad;
  return (a * std::min (1.0, x) + 1 - a);
}

double
FAO::CloudinessFactor_Humid (double Si, double rad)
{
  const double a = 1.00;
  const double x = Si / 0.75 / rad;
  const double cfh = (a * std::min (1.0, x) + 1 - a);
  return cfh;
}

double
FAO::Makkink (double air_temperature /* dg C */,
	      double global_radiation /* W/m^2 */) /* mm/h */
{
  // Use Makkink's equation for calculating reference_evapotranspiration.
  const double T = 273.16 + air_temperature; // dg C -> K
  const double Delta = 5362.7 / (T * T) * exp (26.042 - 5362.7 / T);
  return 1.05e-3
    * Delta / (Delta + 66.7) * global_radiation;
}


double
FAO::ETaero (double AtmPressure, double Temp, double ea, double ra,
	     double rc)
{
  // Specific heat at constant pressure.
  const double c_p = 1.013e3;   // [J/kg/K]

  // [Pa/K] = [Pa/K] + [Pa/K]
  const double x1 = SlopeVapourPressureCurve (Temp) +
    PsychrometricConstant (AtmPressure, Temp) * (1 + rc / ra);
  // [kg/m^3/K] = [kg/m^3] [J/kg/K] / [J/kg]
  const double x2 = AirDensity (AtmPressure, Temp) * c_p /
    LatentHeatVaporization (Temp); 
  // [Pa s/m] = ([Pa] - [Pa]) / [s/m]
  const double x3 = (SaturationVapourPressure (Temp) - ea) / ra;
  // [kg/m^2/s] = [K/Pa] * [kg/m^3/K] * [Pa m/s]
  return (1.0 / x1) * x2 * x3;   // [kg/m2/s]
}

// [kg/m^2/s] = [K/Pa] * [J/m^3/K] * [Pa s/m]
// [kg/m^2/s] = [K/Pa] * [kg m^2/s^2] * [/m^3/K] * [Pa s/m]
// [kg/m^2/s]

// Rad:
// [J/kg] * [kg/m^2/s] = [Pa/K] * [W/m^2] / [Pa/K]

// Aero:
// [J/kg] * [kg/m^2/s] = [kg/m^3] * [J/kg/K] * ([Pa] / [s/m]) / [Pa/K]
// [J/kg] * [kg/m^2/s] = [kg/m^3] * [J/kg/K] * [Pa] * [m/s] * [K/Pa]

// [J] = [kg m^2/s^2]
// [Pa] = [kg/m/s^2]

double
FAO::RefETaero (double AtmPressure, double Temp, double ea, double U2)
{
  double x1 = SlopeVapourPressureCurve (Temp)
    / PsychrometricConstant (AtmPressure, Temp);
  x1 = 1 / (x1 + 1 + 0.34 * U2);
  const double x2 = 0.9 / (Temp + 273) * U2
    * (SaturationVapourPressure (Temp) - ea);
  return (x1 * x2) / 86400.0; // [kg/m2/s]
}

double
FAO::ETrad (double AtmPressure, double Temp, double Rn, double G,
	    double ra, double rc)
{
  double x1 = SlopeVapourPressureCurve (Temp)
    / PsychrometricConstant (AtmPressure, Temp);
  x1 /= x1 + 1 + rc / ra;
  const double x2 = (Rn - G) / LatentHeatVaporization (Temp);
  return (x1 * x2);
}

double
FAO::PenmanMonteith (double CropHeight, double ScreenHeight, 
		     double LAI, double rs_min, double Rn,
		     double G, double Temp, double ea, double U2,
		     double AtmPressure)
{
  const double ra = AerodynamicResistance (CropHeight, ScreenHeight, U2);
  const double rc = CanopyResistance (LAI, rs_min);
  const double E1 = ETrad (AtmPressure, Temp, Rn, G, ra, rc);
  const double E2 = ETaero (AtmPressure, Temp, ea, ra, rc);
  return (E1 + E2);
}

double					   // [kg/m^2/s]
FAO::RefPenmanMonteithAllen2006 (double Rn, // [W/m^2]
				 double G,  // [W/m^2]
				 double Temp, // [dg C]
				 double ea,   // [Pa]
				 double U2, // [m/s]
				 double AtmPressure // [Pa]
				 )
{
  daisy_assert (std::isfinite (Rn));
  daisy_assert (std::isfinite (G));
  daisy_assert (std::isfinite (Temp));
  daisy_assert (std::isfinite (ea));
  daisy_assert (std::isfinite (U2));
  daisy_assert (std::isfinite (AtmPressure));
  const double s = SlopeVapourPressureCurve (Temp); // [Pa/K]
  const double gamma = PsychrometricConstant (AtmPressure, Temp); // [Pa/K]
  const double e_sat = SaturationVapourPressure (Temp);		  // [Pa]
  double E3 = 0.03525 * s * (Rn - G) +
    gamma * 0.9 / (Temp + 273) * U2 *
    (e_sat - ea);
  // FAO56: box 6: r_a = 208 / U2 [s/m]
  // FAO56: box 6: r_s = 70 [s/m]
  const double r_s = (Rn > 0) ? 50 : 200; // [s/m]
  const double r_s_per_r_a = r_s * U2 / 208.0;
  E3 /= s + gamma * (1 + r_s_per_r_a);
  const double value = E3 / 86400.0; // [kg/m^2/s]
  daisy_assert (std::isfinite (value));
  return value;
}

double					   // [kg/m^2/s]
FAO::RefPenmanMonteith (double Rn, // [W/m^2]
			double G,  // [W/m^2]
			double Temp, // [dg C]
			double ea,   // [Pa]
			double U2, // [m/s]
			double AtmPressure // [Pa]
			)
{
  const double s = SlopeVapourPressureCurve (Temp); // [Pa/K]
  daisy_assert (std::isfinite (s));
  const double gamma = PsychrometricConstant (AtmPressure, Temp); // [Pa/K]
  daisy_assert (std::isfinite (gamma));
  const double e_sat = SaturationVapourPressure (Temp);		  // [Pa]
  daisy_assert (std::isfinite (e_sat));
  double E3 = 0.03525 * s * (Rn - G) +
    gamma * 0.9 / (Temp + 273) * U2 *
    (e_sat - ea);
  daisy_assert (std::isfinite (E3));
  E3 /= s + gamma * (1 + 0.34 * U2);
  daisy_assert (std::isfinite (E3));
  const double value = E3 / 86400.0; // [kg/m^2/s]
  daisy_assert (std::isfinite (value));
#if 0
  std::ostringstream tmp;
  tmp << "Rn = " << Rn << ", G = " << G << ", Temp = " << Temp
      << ", ea = " << ea << ", U2 = " << U2 << ", AtmPressure = " << AtmPressure
      << ", s  = " << s << ", gamma = " << gamma << ", e_sat = " << e_sat
      << ", value = " << value;
  Assertion::message (tmp.str ());
#endif
  return value;
}

double
FAO::RefPenmanMonteithWet (double Rn, double G, double Temp, double ea,
			   double U2, double AtmPressure, double rb)
{
  const double ra = RefAerodynamicResistance (U2);
  const double rc = rb;       // Added, used to be 0.0, pa 2009-09-22
  const double E1 = ETrad (AtmPressure, Temp, Rn, G, ra, rc);
  const double E2 = ETaero (AtmPressure, Temp, ea, ra, rc);
  //const double E2 = RefETaero (AtmPressure, Temp, ea, U2);

#if 0
  std::ostringstream tmp;
  tmp << "Rn = " << Rn << ", G = " << G << ", Temp = " << Temp
      << ", ea = " << ea << ", U2 = " << U2 << ", AtmPressure = " << AtmPressure
      << ", ra  = " << ra << ", E1 = " << E1 << ", E2 = " << E2;
  Assertion::message (tmp.str ());
#endif

  return (E1 + E2);
}

// fao.C ends here.
