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
  // Reference : Rasmus Houborg thesis, 2006
  
  const double P_surf = 101300; //Surface atmospheric pressure [Pa]
  const double TK = 273.15;  //Constant to convert celcius to Kelvin []
  const double v = 0.00001327; // Molecular viscosity [m^2 s^-1]
  const double d_heat = 0.00001869; // Diffusivity of heat [m^2 s^-1]
  const double d_CO2  = 0.00001381; // Diffusivity of CO2 [m^2 s^-1]
  const double d_H2O  = 0.00002178; // Diffusivity of H2O [m^2 s^-1]
  const double g = 9.82; // Gravitational acceleration [m s^-2]
  const double ku = 0.5; // Parameter that describes the vertical variation 
                         // of wind speed within the canopy

}

// Function to correct diffusivities for temperature and pressure (G1)
double 
Resistance::Cl (const double T_air)
{ 
  const double Cl = (1013.0 /(0.01 * P_surf)) 
    * pow(((T_air + TK)/(TK + 0.01)), 1.81); 
  return Cl; // []
}

// Boundary conductance for a leaf due to free convection for heat (G2)
double 
Resistance::gbf_heat (const double Cl/*[]*/, const double T_air /*[dg C]*/, 
                      const double T_l_sun /*[dg C]*/, const double wl /*[m]*/) 
{
  const double gbf_heat = d_heat * Cl / wl
    * pow((g * pow(wl,3)/sqr(v * Cl) * (T_l_sun - T_air))/(T_air + TK),0.25);
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
Resistance::gbu_heat (const double Uz /*surface wind speed [m s^-1]*/, 
                      const double wl /*[m]*/, const double LAI /*[]*/) 
{
  const double gbu_heat = 2 * 0.003 * pow((Uz * exp(-ku * LAI)/wl), 0.5); 
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

// resistance.C ends here.
