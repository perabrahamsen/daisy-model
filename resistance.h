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

struct Treelog;

namespace Resistance
{ 
  // Reference : Rasmus Houborg thesis, 2006

  // Function to correct diffusivities for temperature and pressure
  double Cl(double T_air /*[dg C]*/);// []  
                     
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
                       const double LAI_fraction /*[]*/); // [m s¯1

// Boundary conductance of the sunlit canopy fraction due to forced convection
  double gbu_sun (const double gbu_j /*[m s¯1]*/, const double LAI /*[]*/,
                  const double kb /*extinction coefficient []*/); // [m s¯1]

// Boundary conductance of the shadow canopy fraction due to forced convection
  double gbu_shadow (const double kb, const double gbu_j /*[m s¯1]*/, 
                     const double LAI /*[]*/); // [m s¯1

// Boundary conductance of the sunlit and shadow canopy fraction 
  double gb_fraction (const double gbu_j /*[m s¯1]*/, 
                      const double gbf_j /*[m s¯1]*/); // [m s¯19

// Total conductance (leaf boundary + atmospheric) of the sunlit and shadow 
// canopy fraction 
    double gc_fraction (const double gb_j /*[m s¯1]*/, 
                        const double ra /* aerodynamic resistance [s m¯1]*/); //[m s¯1]

}

#endif // RESISTANCE_H
