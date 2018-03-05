// resistance.h --- Resistance utility functions.
// 
// 2008 Copyright Per Abrahamsen, Birgitte Gjettermann, and Søren Hansen and KU
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


#ifndef RESISTANCE_H
#define RESISTANCE_H

class Treelog;

namespace Resistance
{
  // Reference : Rasmus Houborg thesis, 2006: 
  //             Inferences of key Environmental and Vegetation Biophysical 
  //             Controls for use in Regional-scale SVAT Modelling
  //             Using Terra and Agua MODIS and Weather Prediction Data.
  //             Institute of Geography, University of Copenhagen.
  
  const double TK = 273.15;      //Constant to convert celcius to Kelvin []
  const double v = 0.00001327;   // Molecular viscosity [m^2 s^-1]
  const double d_heat = 0.00001869; // Diffusivity of heat [m^2 s^-1]
  const double d_CO2  = 0.00001381; // Diffusivity of CO2 [m^2 s^-1]
  const double d_H2O  = 0.00002178; // Diffusivity of H2O [m^2 s^-1]
  const double g = 9.82;         // Gravitational acceleration [m s^-2]
  const double ku = 0.5;         // Parameter that describes the vertical variation 
                                 // of wind speed within the canopy
  const double default_z_0b = 0.0006;    // Bare soil roughness height for momentum [m]
  const double k = 0.41;         // Von Karman's constant []
  const double h_soil = 0.05;    // Height above the soil surface where the effect
                                 // of soil roughness is minimal [m]
  const double m_a = 29.0;       // Molecular weight of air [g mol^-1]
  const double R = 8.3143;       // Universal molecular gass constant [J mol^-1 K^-1]

  // Unit convertsions.  [mol/m^2/s] <-> [m/s]
  double molly2ms (const double T, const double P, const double value);
  double ms2molly (const double T, const double P, const double value);

  // Function to correct diffusivities for temperature and pressure
  double Cl(double T_air /*[dg C]*/, const double Ptot /* [Pa] */);// []  
                     
  // Boundary conductance for a leaf due to free convection 
  double gbf_heat (const double C1/*[]*/, const double T_air /*[dg C]*/, 
                   const double T_l_sun /*[dg C]*/, const double wl /*[m]*/);// [m s¯1]
  double gbf_CO2 (const double gbf_H2O /*[m s¯1]*/, const double Cl /*[]*/); // [m s¯1]
  double gbf_H2O_amph(const double gbf_heat /*[m s¯1]*/, 
                      const double Cl /*[]*/);//[m s¯1]
  double gbf_H2O_hypo(const double gbf_heat /*[m s¯1]*/, 
                      const double Cl /*[]*/); // [m s¯1]
// Boundary conductance for a leaf due to forced convection 
  double gbu_heat (const double Uz /*surface wind speed [m s^-1]*/, 
                   const double wl /*[m]*/, const double LAI /*[]*/); // [m s¯1]
  double gbu_CO2 (const double gbu_H2O /*[m s¯1]*/, const double Cl /*[]*/); // [m s¯1]
  double gbu_H2O_amph (const double gbu_heat /*[m s¯1]*/, 
                       const double Cl /*[]*/); // [m s¯1]
  double gbu_H2O_hypo (const double gbu_heat /*[m s¯1]*/,
                       const double Cl /*[]*/); // [m s¯1

// Boundary conductance of the sunlit and shaded canopy fraction due to free convection
  double gbf_fraction (const double gbf_j /*[m s¯1]*/, 
                       const double LAI_fraction /*[]*/); // [m s¯1]

// Boundary conductance of the sunlit canopy fraction due to forced convection
  double gbu_sun (const double gbu_j /*[m s¯1]*/, const double kb /*[]*/, 
                  const double LAI /*[]*/); // [m s¯1]

// Boundary conductance of the shadow canopy fraction due to forced convection
  double gbu_shadow (const double gbu_j /*[m s¯1]*/, const double kb /*[]*/,
                     const double LAI /*[]*/); // [m s¯1]

// Boundary conductance of the sunlit canopy fraction due to free convection
  double gbf_sun (const double gbf_j /*[m s¯1]*/, const double LAI_sun /*[]*/);
  
// Boundary conductance of the shadow canopy fraction due to free convection
  double gbf_shadow (const double gbf_j /*[m s¯1]*/, 
                     const double LAI_shadow /*[m^2 m^-2]*/);
  
  // Boundary conductance of the sunlit and shadow canopy fraction 
  double gb_fraction (const double gbu_j /*[m s¯1]*/, 
                      const double gbf_j /*[m s¯1]*/); // [m s¯19
  
  // Total conductance (leaf boundary + atmospheric) of the sunlit and shadow 
// canopy fraction 
  double gc_fraction (const double gb_j /*[m s¯1]*/, 
                        const double ra /* aerodynamic resistance [s m¯1]*/); //[m s¯1]

//----------------------------------------------------
// Atmospheric aerodynamic resistance
//----------------------------------------------------
// Roughness lenght 
  double z_0h (const double z_0 /*roughness lenght [m]*/); //[m]
  double z_0 (const double z_0b /* Bare soil roughness height for momentum [m] */,
              const double h_veg /* vegetation heighr [m]*/, 
              const double c_drag /* drag force [m^2 m^-2]*/,
              const double d /* zero-plane displacement height [m]*/,
              const double LAI /*[m^2 m^-2]*/); //[m]
  // Zero-plane displacement height
  double d (const double h_veg /* vegetation heighr [m]*/, 
            const double c_drag /* drag force [m^2 m^-2]*/,
            const double LAI /*[m^2 m^-2]*/); //[m]
  
  // Atmospheric stability indicator
  double N (const double z /* reference height above canopy [m]*/, 
            const double d /* displacement height [m]*/,
            const double T_0 /* land surface temp [dg C]*/, 
            const double T_a /* air temp [dg C]*/, 
            const double U_z /* surface wind speed [m s^-1]*/); //[]
  
  // Aerodynamic resistance between canopy source height and reference 
  // height above the canopy (F5)
  double r_a (const double z /* reference height above canopy [m]*/, 
              const double z_0 /* Roughness lenght for momentum transport [m]*/, 
              const double z_0h /* Roughness lenght for sensible heat transfer[m]*/,
              const double d /* displacement height [m]*/,
              const double N /* atm stability indicator []*/, 
              const double U_z /* surface wind speed [m s^-1]*/); //[s m^-1]
  
//----------------------------------------------------
// Soil aerodynamic resistance (Norman et al., 1995 cf. Rasmus Houborg)
//----------------------------------------------------
  // Wind speed at the top of the canopy (L1)
  double U_c (const double z_r /* reference height above canopy [m]*/, 
              const double z_0 /* Roughness lenght for momentum transport [m]*/, 
              const double d /* displacement height [m]*/,
              const double U_z /* surface wind speed [m s^-1]*/, 
              const double T_0 /* land surface temp [dg C]*/, 
              const double T_a /* air temp [dg C]*/,       
              const double h_veg /* vegetation heighr [m]*/,      
              const double r_a /* Aerodynamic resistance [s m^-1]*/, 
              const double rho_a /* air density [kg m^-3]*/); // [m s^-1]

  // Mean leaf size (L2)
  double l_m (const double w_l /* leaf width [m]*/); // [m]
  
  // Wind speed above the soil surface [m s^-1] (L3)
  double U_s (const double l_m /* mean leaf size [m]*/,
              const double h_veg /* vegetation height [m]*/, 
              const double LAI /*[m^2 m^-2]*/,
              const double U_c /* Wind speed at the top of the canopy [m s^-1]*/);
  
  // The aerodynamic resistance of the boundary layer at the soil 
  // surface beneath the canopy [s m^-1](L4)
  double r_a_soil (const double U_s /* Wind speed above the soil surface [m s^-1]*/);

//----------------------------------------------------
// Temperatures
//----------------------------------------------------
 
  // Land surface temperature (O1)
  // Normann et al., 1995 cf. Rasmus Houborg 2006
  double T_0 (const double T_c /* canopy temperature [dg C]*/, 
              const double T_soil /* soil temperature [dg C]*/,
              const double kb /* extinction coefficient []*/,
              const double LAI /*[m^2 m^-2]*/); // [dg C]
  
  // Canopy temperature (K1)
  // Ball (1988) & Collatz et al (1991) cf. Rasmus Houborg 2006
  double T_c (const double T_l_sun /* leaf sun temperature [dg C]*/, 
              const double T_l_sha /* leaf shadow temperature [dg C]*/,
              const double kb /* extinction coefficient []*/,
              const double LAI /*[m^2 m^-2]*/); // [dg C]

//--------------------------------------
// and left overs - miscelaneours 
//--------------------------------------

// air density 
  double rho_a (const double T_a, const double Ptot); // [kg m^-3]

}
#endif // RESISTANCE_H
