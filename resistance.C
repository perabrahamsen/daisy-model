// resistance.C --- RESISTANCE utility functions.
// 
// Copyright 2008 Per Abrahamsen, Birgitte Gjettermann, and Søren Hansen and KU
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

#include "resistance.h"
#include "alist.h"
#include "syntax.h"
#include "net_radiation.h"
#include "assertion.h"
#include "treelog.h"
#include "mathlib.h"
#include "librarian.h"

namespace Resistance
{ 
  // Reference : Rasmus Houborg thesis, 2006: 
  //             Inferences of key Environmental and Vegetation Biophysical 
  //             Controls for use in Regional-scale SVAT Modelling
  //             Using Terra and Agua MODIS and Weather Prediction Data.
  //             Institute of Geography, University of Copenhagen.
  
  const double P_surf = 101300; //Surface atmospheric pressure [Pa]
  const double TK = 273.15;  //Constant to convert celcius to Kelvin []
  const double v = 0.00001327; // Molecular viscosity [m^2 s^-1]
  const double d_heat = 0.00001869; // Diffusivity of heat [m^2 s^-1]
  const double d_CO2  = 0.00001381; // Diffusivity of CO2 [m^2 s^-1]
  const double d_H2O  = 0.00002178; // Diffusivity of H2O [m^2 s^-1]
  const double g = 9.82; // Gravitational acceleration [m s^-2]
  const double ku = 0.5; // Parameter that describes the vertical variation 
                         // of wind speed within the canopy
  const double z_0b = 0.0006; // Bare soil roughness height for momentum [m]
  const double k = 0.41; // Von Karman's constant
}
//----------------------------------------------------
// Boundary layer conductance
//----------------------------------------------------
// Function to correct diffusivities for temperature and pressure (G1)
double 
Resistance::Cl (const double T_a)
{ 
  const double Cl = (1013.0 /(0.01 * P_surf)) 
    * pow(((T_a + TK)/(TK + 0.01)), 1.81); 
  return Cl; // []
}

// Boundary conductance for a leaf due to free convection for heat (G2)
double 
Resistance::gbf_heat (const double Cl/*[]*/, const double T_a /*[dg C]*/, 
                      const double T_l_sun /*[dg C]*/, const double wl /*[m]*/) 
{
  const double gbf_heat = d_heat * Cl / wl
    * pow((g * pow(wl,3)/sqr(v * Cl) * (T_l_sun - T_a))/(T_a + TK),0.25);
  return gbf_heat;// [m s¯1]
}

// Boundary conductance for a leaf due to free convection for CO2 (G3)
double 
Resistance::gbf_CO2 (const double gbf_H2O /*[m s¯1]*/, const double Cl /*[]*/)
{
  const double gbf_CO2 = gbf_H2O * (d_CO2 * Cl)/(d_H2O * Cl); 
  return gbf_CO2; // [m s¯1]
} 

// Boundary conductance for a leaf due to free convection for H2O (G4)
// For amphistomatous leaves
double 
Resistance::gbf_H2O_amph(const double gbf_heat /*[m s¯1]*/, const double Cl /*[]*/)
{
  const double gbf_H2O = gbf_heat * (d_H2O * Cl)/(d_heat * Cl);
  return gbf_H2O; // [m s¯1]
} 
// For hypostomatous leaves (G4)
double 
Resistance::gbf_H2O_hypo(const double gbf_heat /*[m s¯1]*/, const double Cl /*[]*/ )
{
  const double gbf_H2O = 0.5 * gbf_H2O_amph(gbf_heat, Cl);
  return gbf_H2O; // [m s¯1]
} 

// Boundary conductance for a leaf due to forced convection for heat (G5)
double 
Resistance::gbu_heat (const double U_z /*surface wind speed [m s^-1]*/, 
                      const double wl /*[m]*/, const double LAI /*[]*/) 
{
  const double gbu_heat = 2 * 0.003 * pow((U_z * exp(-ku * LAI)/wl), 0.5); 
  return gbu_heat; // [m s¯1]
}

// Boundary conductance for a leaf due to forced convection for CO2 (G7)
double 
Resistance::gbu_CO2 (const double gbu_H2O /*[m s¯1]*/, const double Cl /*[]*/)
{
  const double gbu_CO2 = gbu_H2O * (d_CO2 * Cl)/(d_H2O * Cl);  
  return gbu_CO2; // [m s¯1]
}

// Boundary conductance for a leaf due to forced convection for H2O (G6)
// For amphistomatous leaves
double 
Resistance::gbu_H2O_amph (const double gbu_heat /*[m s¯1]*/, const double Cl /*[]*/)
{
  const double gbu_H2O = gbu_heat * (d_H2O * Cl)/(d_heat * Cl);  
  return gbu_H2O; // [m s¯1]
}
// For hypostomatous leaves (G6)
double 
Resistance::gbu_H2O_hypo (const double gbu_heat /*[m s¯1]*/, const double Cl /*[]*/)
{
  const double gbu_H2O = 0.5 * gbu_H2O_amph(gbu_heat, Cl);
  return gbu_H2O; // [m s¯1]
}

// Boundary conductance of the sunlit and shaded canopy fraction due to
// free convection for heat, H2O, and CO2 (j = heat, H2O, and CO2) (G8)
double 
Resistance::gbf_fraction (const double gbf_j /*[m s¯1]*/, 
                          const double LAI_fraction /*[]*/)
{
  const double gbf_fraction = gbf_j * LAI_fraction;
  return gbf_fraction; // [m s¯1]
}

// Boundary conductance of the sunlit canopy fraction due to forced
// convection for heat, H2O, and CO2 (j = heat, H2O, and CO2) (G9)
double 
Resistance::gbu_sun (const double gbu_j /*[m s¯1]*/, const double LAI /*[]*/,
                     const double kb /*extinction coefficient []*/)
{
  const double gbu_sun = gbu_j 
    * (1 - exp( - (0.5 * ku + kb) * LAI))/(0.5 * ku + kb);
  return gbu_sun; // [m s¯1]
}

// Boundary conductance of the shadow canopy fraction due to forced
// convection for heat, H2O, and CO2 (j = heat, H2O, and CO2) (G9)
double 
Resistance::gbu_shadow (const double kb, const double gbu_j /*[m s¯1]*/, 
                        const double LAI /*[]*/)
{
  const double gbu_shadow = gbu_j 
    * (1 - exp(-(0.5 * ku * LAI))/(0.5 * ku) 
       - (1 - exp(-(0.5 * ku + kb) * LAI))/(0.5 * ku + kb)) ;
  return gbu_shadow; // [m s¯1]
}

// Boundary conductance of the sunlit and shadow canopy fraction for 
// heat, H2O, and CO2 (j = heat, H2O, and CO2) (G10)
double 
Resistance::gb_fraction (const double gbu_j /*[m s¯1]*/, 
                         const double gbf_j /*[m s¯1]*/)
{
  const double gb_fraction = gbu_j + gbf_j;
  return gb_fraction; // [m s¯1]
}

// Total conductance (leaf boundary + atmospheric) of the sunlit and shadow 
// canopy fraction for heat, H2O, and CO2 (j = heat, H2O, and CO2) (G11)
double 
Resistance::gc_fraction (const double gb_j /*[m s¯1]*/, 
                         const double ra /* aerodynamic resistance (F5) [s m¯1]*/)
{
  const double gb_fraction = 1/((1/gb_j) + ra);
  return gb_fraction; // [m s¯1]
}

//----------------------------------------------------
// Atmospheric aerodynamic resistance
//----------------------------------------------------

// Roughness lenght for sensible heat transfer (F1)
double 
Resistance::z_0h (const double z_0 /*roughness lenght [m]*/)
{
  const double z_0h = z_0 / 7.0;
  return z_0h; // [m]
}

// Roughness lenght for momentum transport (F2)
double 
Resistance::z_0 (const double h_veg /* vegetation heighr [m]*/, 
                 const double c_drag /* drag force [m^2 m^-2]*/,
                 const double d /* zero-plane displacement height [m]*/,
                 const double LAI /*[m^2 m^-2]*/)
{
  double z_0;
  if (LAI < 3)
    z_0 = z_0b + 0.3 * h_veg * sqrt(c_drag * LAI);
  else 
    z_0 = 0.3 * h_veg * (1 - d/h_veg);
  return z_0; // [m]
}

// Zero-plane displacement height [m] (F3)
double 
Resistance::d (const double h_veg /* vegetation heighr [m]*/, 
               const double c_drag /* drag force [m^2 m^-2]*/,
               const double LAI /*[m^2 m^-2]*/)
{
  const double d = 1.1 * h_veg * log(1. + pow(c_drag * LAI, 0.25));
  return d; // [m]
}

// Atmospheric stability indicator (F4)
double 
Resistance::N (const double z /* reference height above canopy [m]*/, 
               const double d /* displacement height [m]*/,
               const double T_0 /* land surface temp [dg C]*/, 
               const double T_a /* air temp [dg C]*/, 
               const double U_z /* surface wind speed [m s^-1]*/)
{
  const double N = 5. * (z - d) * g * (T_0 - T_a)/((T_a + TK) * sqr(U_z));
  return N; // []
}

// Aerodynamic resistance between canopy source height and reference 
// height above the canopy (F5)
double 
Resistance::r_a (const double z /* reference height above canopy [m]*/, 
                 const double z_0 /* Roughness lenght for momentum transport [m]*/, 
                 const double z_0h /* Roughness lenght for sensible heat transfer[m]*/,
                 const double d /* displacement height [m]*/,
                 const double N /* atm stability indicator []*/, 
                 const double U_z /* surface wind speed [m s^-1]*/)
{
  const double A = 1.0; //????????????????????????????
  const double B = log((z - d)/z_0h) + 2. * N * log((z - d)/z_0); 
  const double C = N * sqr(log ((z - d)/z_0));
  const double S = (B - sqrt(sqr (B) - 4 * A * C))/(2 * A);

  double r_a;
  if (N <= 0)
    r_a = (log((z - d)/z_0) - S) * (log((z - d)/z_0h) - S) * 1/(sqr (k) * U_z);
  else
    r_a = (log((z - d)/z_0) * log((1 - d)/z_0h))
      / (exp (log(sqrt(k)) + log(U_z) + 0.75 * log(1 + N)));
  return r_a; // [s m^-1]
}

//----------------------------------------------------
// Soil aerodynamic resistance (Norman et al., 1995 cf. Rasmus Houborg)
//----------------------------------------------------

// Wind speed at the top of the canopy (L1)
double 
Resistance::U_c (const double z_r /* reference height above canopy [m]*/, 
                 const double z_0 /* Roughness lenght for momentum transport [m]*/, 
                 const double d /* displacement height [m]*/,
                 const double U_z /* surface wind speed [m s^-1]*/, 
                 const double T_0 /* land surface temp [dg C]*/, 
                 const double T_a /* air temp [dg C]*/, 
                 const double h_veg /* vegetation heighr [m]*/,            
                 const double r_a /* Aerodynamic resistance [s m^-1]*/, 
                 const double rho_a /* air density [kg m^-3]*/)
{

  const double u = (U_z * k) / (log((z_r - d)/z_0));
  const double L_mo = - (r_a * rho_a * pow(u,3) * (T_a + TK))
                         / (g * k * (T_0 - T_a));

  const double y = 1 - 15 * pow((z_r - d)/L_mo,-0.25);
  const double psi = 2 * log((1 + y)/ 2) + log((1 + sqr(y))/2) 
    - 2/(tan(y)) + M_PI/2.;

  double U_c;
  if (z_r/L_mo >= 0)
    U_c = U_z * (log(h_veg - d)/z_0)/(log((z_r - d)/z_0) + 4.7 * (z_r - d)/L_mo);
  else
    U_c = U_z * (log(h_veg - d)/z_0)/(log((z_r - d)/z_0) - psi);
  return U_c;
}



// resistance.C ends here.
