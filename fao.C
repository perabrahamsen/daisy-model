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


#include "fao.h"
#include "net_radiation.h"
#include "assertion.h"
#include "treelog.h"
#include "tmpstream.h"
#include "mathlib.h"

double 
FAO::CanopyResistance (double LAI)
{ 
  daisy_assert (LAI > 0.0);
  return (200 / LAI); 
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
FAO:: RoughnessHeight_Heat (double CropHeight)
{ return (0.0123 * CropHeight / 100.0); }

double 
FAO::AerodynamicResistance (double CropHeight, double ScreenHeight,
			    double U)
{
  if (U == 0.0)
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
  if (U2 == 0.0)
    U2 = 0.1;

  daisy_assert (U2 > 0.0);

  return (208. / U2); 
}

double
FAO::LatentHeatVaporization (double Temp) // [J/kg]
{ return ((2.501 - 2.361e-3 * Temp) * 1.0e6); }

double
FAO::PsychrometricConstant (double AtmPressure, double Temp) // [Pa/K]
{ return (1.63e3 * AtmPressure / LatentHeatVaporization (Temp)); }

double
FAO::AirDensity (double AtmPressure, double Temp) // [kg/m3]
{
  const double Tvirtuel = 1.01 * (Temp + 273);
  return (3.486 * AtmPressure / Tvirtuel);
}

double
FAO::SaturationVapourPressure (double Temp) // [Pa]
{ return (611.0 * exp (17.27 * Temp / (Temp + 237.3))); }

double
FAO::SlopeVapourPressureCurve (double Temp) // [Pa/K]
{ return (4.098E3 * SaturationVapourPressure (Temp) / pow (Temp + 237.3, 2)); }

double
FAO::AtmosphericPressure (double elevation) // [Pa]
{ return (101300. * pow ((293 - 0.0065 * elevation) / 293, 5.26)); }

double
FAO::CloudinessFactor_Arid (double Si, double rad)
{
  const double a = 1.35;
  const double x = Si / 0.75 / rad;
  return (a * min (1.0, x) + 1 - a);
}

double
FAO::CloudinessFactor_Humid (double Si, double rad)
{
  const double a = 1.00;
  const double x = Si / 0.75 / rad;
  const double cfh = (a * min (1.0, x) + 1 - a);
  return cfh;
}

double
FAO::RefNetRadiation (double Si, double rad,
		      double Temp, double ea, Treelog& out)
{
  static NetRadiation* net_radiation = NULL;
  if (net_radiation == NULL)
    {
      Syntax syntax;
      AttributeList alist;
      alist.add ("type", "brunt");
      net_radiation = &Librarian<NetRadiation>::create (alist);
    }

  const double albedo = 0.23;
  net_radiation->tick (CloudinessFactor_Arid (Si, rad),
		       Temp, ea, Si, albedo, out);
  return net_radiation->net_radiation ();
}

double
FAO::Makkink (double air_temperature /* dg C */,
	      double global_radiation /* W/m^2 */) /* mm/h */
{
  // Use Makkink's equation for calculating reference_evapotranspiration.
  const double T = 273.16 + air_temperature; // dg C -> K
  const double Delta = 5362.7 / pow (T, 2.0) * exp (26.042 - 5362.7 / T);
  return 1.05e-3
    * Delta / (Delta + 66.7) * global_radiation;
}


double
FAO::ETaero (double AtmPressure, double Temp, double ea, double ra,
	     double rc)
{
  const double x1 = SlopeVapourPressureCurve (Temp) +
    PsychrometricConstant (AtmPressure, Temp) * (1 + rc / ra);
  const double x2 = AirDensity (AtmPressure, Temp) * 1.013 /
    LatentHeatVaporization (Temp);
  const double x3 = (SaturationVapourPressure (Temp) - ea) / ra;
  return (1.0 / x1) * x2 * x3;   // [kg/m2/s]
}

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
		     double LAI, double Rn,
		     double G, double Temp, double ea, double U2,
		     double AtmPressure)
{
  const double ra = AerodynamicResistance (CropHeight, 
						       ScreenHeight, U2);
  const double rc = CanopyResistance (LAI);
  const double E1 = ETrad (AtmPressure, Temp, Rn, G, ra, rc);
  const double E2 = ETaero (AtmPressure, Temp, ea, ra, rc);
  return (E1 + E2);
}

double
FAO::RefPenmanMonteith (double Rn, double G, double Temp, double ea,
			double U2, double AtmPressure, Treelog& out)
{
  double E3 = 0.03525 * SlopeVapourPressureCurve (Temp) * (Rn - G) +
    PsychrometricConstant (AtmPressure, Temp)
    * 0.9 / (Temp + 273) * U2 *
    (SaturationVapourPressure (Temp) - ea);
  E3 /= SlopeVapourPressureCurve (Temp) +
    PsychrometricConstant (AtmPressure, Temp) * (1 + 0.34 * U2);
#if 1
  if (Rn>750.0)
    {
      TmpStream tmp;
      tmp () << "Rn          " << Rn << "\n"
	     << "G           " << G << "\n"
	     << "Temp        " << Temp << "\n"
	     << "es          " 
	     << SaturationVapourPressure (Temp) << "\n"
	     << "ea          " << ea << "\n"
	     << "U2          " << U2 << "\n"
	     << "AtmPressure " << AtmPressure << "\n"
	     << "Delta       "
	     << SlopeVapourPressureCurve (Temp) << "\n"
	     << "Gamma       "
	     << PsychrometricConstant (AtmPressure, Temp) << "\n"
	     << "Ep (mm/d)   " << E3;
      out.warning (tmp.str ());
    }
#endif
  return E3 / 86400.0; // [kg/m2/s]
}

double
FAO::RefPenmanMonteithWet (double Rn, double G, double Temp, double ea,
			   double U2, double AtmPressure)
{
  const double ra = RefAerodynamicResistance (U2);
  const double rc = 0.0;
  const double E1 = ETrad (AtmPressure, Temp, Rn, G, ra, rc);
  const double E2 = ETaero (AtmPressure, Temp, ea, ra, rc);
  return (E1 + E2);
}

