// fao.h --- FAO utility functions.
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


#ifndef FAO_H
#define FAO_H

struct Treelog;

namespace FAO
{ 
  // Bioclimate.
  double CanopyResistance (double LAI /* [m^2/m^2] */,
                           double rs_min /* [s/m] */); // [s/m]
  double RefCanopyResistance (void); // [s/m]
  double ZeroPlaneDisplacement (double CropHeight /* [m] */); // [m]
  double RoughnessHeight_Momentum (double CropHeight /* [m] */); // [m]
  double RoughnessHeight_Heat (double CropHeight /* [m] */); // [m]
  double AerodynamicResistance (double CropHeight /* [m] */,
				double ScreenHeight /* [m] */,
				double U /* [m/s] */); // [s/m]
  double RefAerodynamicResistance (double U2 /* [m/s] */); // [s/m]

  // Weather.
  double LatentHeatVaporization (double Temp /* [dg C] */); // [J/kg]
  double PsychrometricConstant (double AtmPressure /* [Pa] */,
				double Temp /* [dg C] */); // [Pa/K]
  double AirDensity (double AtmPressure /* [Pa] */,
		     double Temp /* [dg C] */); // [kg/m3]
  double SaturationVapourPressure (double Temp /* [dg C] */); // [Pa]
  double SlopeVapourPressureCurve (double Temp /* [dg C] */); // [Pa/K]
  double AtmosphericPressure (double elevation /* [m] */); // [Pa]
  double CloudinessFactor_Arid (double Si /* [W/m2] */, 
				double rad /* [W/m2] */);
  double CloudinessFactor_Humid (double Si /* [W/m2] */, 
				 double rad /* [W/m2] */);

  double RefNetRadiation (double Si /* [W/m2] */, 
			  double rad /* [W/m2] */,
			  double Temp /* [dg C] */,
			  double ea /* [Pa] */,
			  Treelog&);// [W/m2]
  double Makkink (double air_temperature /* [dg C] */,
		  double global_radiation /* [W/m^2] */); /* [mm/h] */

  // Penman-Monteith.
  double ETaero (double AtmPressure /* [Pa] */,
		 double Temp /* [dg C] */,
		 double ea /* [Pa] */,
		 double ra /* [s/m] */,
		 double rc); // [kg/m2/s]
  double RefETaero (double AtmPressure /* [Pa] */,
		    double Temp /* [dg C] */,
		    double ea /* [Pa] */,
		    double U2);// [kg/m2/s]
  double ETrad (double AtmPressure /* [Pa] */,
		double Temp /* [dg C] */,
		double Rn /* [W/m2] */,
		double G /* [W/m2] */,
		double ra /* [s/m] */,
		double rc); // [kg/m2/s]
  double PenmanMonteith (double CropHeight /* [m] */,
			 double ScreenHeight /* [m] */,
			 double LAI /* [m^2/m^2] */,
                         double rs_min /* [s/m] */,
			 double Rn /* [W/m2] */,
			 double G /* [W/m2] */,
			 double Temp /* [dg C] */,
			 double ea /* [Pa] */,
			 double U2 /* [m/s] */,
			 double AtmPressure); // [kg/m2/s]
  double RefPenmanMonteith (double Rn /* [W/m2/] */,
			    double G /* [W/m2] */,
			    double Temp /* [dg C] */,
			    double ea /* [Pa] */,
			    double U2 /* [m/s] */,
			    double AtmPressure); // [kg/m2/s]
  double RefPenmanMonteithWet (double Rn /* [W/m2] */,
			       double G /* [W/m2] */,
			       double Temp /* [dg C] */,
			       double ea /* [Pa] */,
			       double U2 /* [m/s] */,
			       double AtmPressure); // [kg/m2/s]


}

#endif // FAO_H
