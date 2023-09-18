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
#include "net_radiation.h"
#include "assertion.h"
#include "treelog.h"
#include "mathlib.h"
#include "librarian.h"
#include <sstream>

  // Unit convertsions.  [mol/m^2/s] <-> [m/s]
double 
Resistance::molly2ms (const double T, const double P, const double value)
{
  return value * (R * (T + TK)) / P; //[m s^-1] 
}

double 
Resistance::ms2molly (const double T, const double P, const double value)
{                
  return value * P / (R * (T + TK));
}

//----------------------------------------------------
// Boundary layer conductance
//----------------------------------------------------
// Function to correct diffusivities for temperature and pressure (G1)
double 
Resistance::Cl (const double T_a, const double Ptot)
{ 
  const double Cl = (1013.0 /(0.01 * Ptot)) 
    * pow(((T_a + TK)/(TK + 0.01)), 1.81); 
  return Cl; // []
}

// Boundary conductance for a leaf due to free convection for heat (G2)
double 
Resistance::gbf_heat (const double Cl/*[]*/, const double T_a /*[dg C]*/, 
                      const double T_l_sun /*[dg C]*/, 
                      const double w_l /* leaf width [m]*/) 
{
  daisy_assert (std::isnormal (Cl));
  daisy_assert (std::isnormal (w_l));
 double gbf_heat;
 if (T_l_sun - T_a <= 0)
   gbf_heat = 0.0;
 else 
   gbf_heat = d_heat * Cl / w_l
     * pow((g * pow(w_l, 3.)/sqr(v * Cl) * (T_l_sun - T_a))/(T_a + TK), 0.25);

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
                      const double w_l /* leaf width [m]*/, 
                      const double LAI /*[m^2 m^-2]*/) 
{
  const double gbu_heat = 2. * 0.003 * pow((U_z * exp(-ku * LAI)/w_l), 0.5); 
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
// For amphistomatous leaves (possesing stomata on both surfaces)
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
Resistance::gbu_sun (const double gbu_j /*[m s¯1]*/,
                     const double kb /*extinction coefficient []*/,
                     const double LAI /*[]*/)
{
  const double gbu_sun = gbu_j 
    * (1. - exp( - (0.5 * ku + kb) * LAI))/(0.5 * ku + kb);
  return gbu_sun; // [m s¯1]
}

// Boundary conductance of the shadow canopy fraction due to forced
// convection for heat, H2O, and CO2 (j = heat, H2O, and CO2) (G9)
double 
Resistance::gbu_shadow (const double gbu_j /*[m s¯1]*/,
                        const double kb,  
                        const double LAI /*[m^2 m^-2]*/)
{
  const double gbu_shadow = gbu_j 
    * ((1. - exp(-(0.5 * ku * LAI)))/(0.5 * ku) 
       - (1. - exp(-(0.5 * ku + kb) * LAI))/(0.5 * ku + kb)) ;
  return gbu_shadow; // [m s¯1]
}
// Boundary conductance of the sunlit canopy fraction due to free
// convection for heat, H2O, and CO2 (j = heat, H2O, and CO2) (G9)
double 
Resistance::gbf_sun (const double gbf_j /*[m s¯1]*/, const double LAI_sun /*[]*/)
{
  const double gbf_sun = gbf_j * LAI_sun; 
  return gbf_sun; // [m s¯1]
}

// Boundary conductance of the shadow canopy fraction due to free
// convection for heat, H2O, and CO2 (j = heat, H2O, and CO2) (G9)
double 
Resistance::gbf_shadow (const double gbf_j /*[m s¯1]*/, 
                        const double LAI_shadow /*[m^2 m^-2]*/)
{
  const double gbf_shadow = gbf_j * LAI_shadow;
  return gbf_shadow; // [m s¯1]
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
  const double gb_fraction = 1./((1./gb_j) + ra);
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
Resistance::z_0 (const double z_0b /* Bare soil roughness height for momentum [m] */,
                 const double h_veg /* vegetation height [m]*/, 
                 const double c_drag /* drag force [m^2 m^-2]*/,
                 const double d /* zero-plane displacement height [m]*/,
                 const double LAI /*[m^2 m^-2]*/)
{
  double z_0;
  if (LAI < 3.)
    z_0 = z_0b + 0.3 * h_veg * sqrt(c_drag * LAI);
  else 
    // z_0b added for LAI >= 3 by pa 2009-07-17
    z_0 = z_0b + 0.3 * (h_veg - d);
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
  daisy_assert (U_z > 0.0);
  const double N = 5. * (z - d) * g * (T_0 - T_a)/((T_a + TK) * sqr(U_z));
  return N; // []
}

// Aerodynamic resistance between canopy source height and reference 
// height above the canopy (F5). 
// Also described in Choudhury et al., 1986. Agricult. & Forest Met. 37:75-88
double 
Resistance::r_a (const double z /* reference height above canopy [m]*/, 
                 const double z_0 /* Roughness lenght for momentum transport [m]*/, 
                 const double z_0h /* Roughness lenght for sensible heat transfer[m]*/,
                 const double d /* displacement height [m]*/,
                 const double N /* atm stability indicator []*/, 
                 const double U_z /* surface wind speed [m s^-1]*/)
{
  daisy_assert (z_0 > 0.0);
  daisy_assert (z > d);
  daisy_assert (z_0h > 0.0);
  daisy_assert ((sqr (k) * U_z) > 0.0);
  
  const double A = 1.0 + N; //[]
  const double B = log((z - d)/z_0h) + 2. * N * log((z - d)/z_0); 
  const double C = N * sqr(log ((z - d)/z_0));
  const double D = sqr (B) - 4. * A * C;
  double S; 
  if (D < 0.0 || std::fabs (A) < 1e-4)
    S = -5.0;
  else 
    S = std::max ((B - sqrt(D))/(2. * A), -5.0);

  double r_a;
  if (N <= 0.0)
    r_a = (log((z - d)/z_0) - S) * (log((z - d)/z_0h) - S) * 1./(sqr (k) * U_z);
  else
    r_a = (log((z - d)/z_0) * log((z - d)/z_0h))
      / (sqr(k) * U_z * pow(1. + N, 3./4.));

#if 0
  std::ostringstream tmp;
  tmp << "z_0 = " << z_0 << ", z = " << z << ", z0_h " << z_0h << ", d = " << d 
      << ", N = " << N << ", U_z = " << U_z << ", S = " << S 
      << ", r_a = " << r_a;
  Assertion::message (tmp.str ());
#endif

  return r_a; // [s m^-1]
}

//----------------------------------------------------
// Soil aerodynamic resistance (Norman et al., 1995? cf. Rasmus Houborg)
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
  daisy_assert (z_0 > 0.0);
  daisy_assert (U_z > 0.0);
  daisy_assert (z_r > d);
  daisy_assert (h_veg > d);
  daisy_assert (log((z_r - d)/z_0) > 0.0);

  const double u = (U_z * k) / (log((z_r - d)/z_0));
  double L_mo;
  double U_c = -42.42e42;

  if ((T_0 - T_a) < 0.1) 
    {
      if(log((h_veg - d)/z_0)/(log((z_r - d)/z_0)) < 0.0)
        U_c = U_z; 
      else
        U_c = U_z * (log((h_veg - d)/z_0)/(log((z_r - d)/z_0)));
      daisy_assert(U_c >= 0.0);
    }
  else  
    {
      L_mo = - (r_a * pow(u,3.) * (T_a + TK)) / (g * k * (T_0 - T_a));
      daisy_assert (T_0 > T_a);
      daisy_assert (std::isnormal (L_mo));

      const double y = pow(1. - 16. * (z_r - d)/L_mo,-0.25); // []
      const double psi = log(sqr((1. + y)/ 2.) * (1. + sqr(y))/2.) 
        - 2./(tan(y)) + M_PI/2.;//[]

      if (L_mo >= 0.0)
        U_c = U_z * (log((h_veg - d)/z_0)
                     /(log((z_r - d)/z_0) + 4.7 * (z_r - d)/L_mo));
      else
        U_c = U_z * (log((h_veg - d)/z_0)
                     /(log((z_r - d)/z_0) - psi));

      //      daisy_assert (U_c > 0.0);
    }
  //  daisy_assert (U_c > 0.0);

  return U_c; // [m s^-1]
}

// Mean leaf size (L2)
double 
Resistance::l_m (const double w_l /* leaf width [m]*/)
{
  const double a1 = w_l;       // leaf radius along the major axis (ellipse) [m]
  const double b1 = w_l * 0.5; // leaf radius along the minor axis (ellipse) [m]

  const double l_m = (4. * M_PI * a1 * b1)
    / (2. * M_PI * sqrt(0.5 *(sqr (a1) + sqr (b1)))) ;

  return l_m; // [m]
}

double 
Resistance::U_s (const double l_m /* mean leaf size [m]*/,
                 const double h_veg /* vegetation height [m]*/, 
                 const double LAI /*[m^2 m^-2]*/,
                 const double U_c /* Wind speed at the top of the canopy [m s^-1]*/)
{
  const double a_0 = 0.28 * pow(LAI, 2./3.) * pow(h_veg, 1./3.) * pow(l_m, -1./3.);
  const double U_s = U_c * exp(- a_0 * (1. - h_soil/h_veg));
  daisy_assert (U_s > 0.0);
  return U_s; //[m s^-1]
}

// The aerodynamic resistance of the boundary layer at the soil 
// surface beneath the canopy (L4)
double 
Resistance::r_a_soil (const double U_s /* Wind speed above the soil surface [m s^-1]*/)
{
  daisy_assert (U_s > 0.0);
  const double r_a_soil = 1.0 /(0.004 + 0.012 * U_s);
  return r_a_soil; // [s m^-1]
}

//----------------------------------------------------
// Temperature 
//----------------------------------------------------

// Land surface temperature (O1)
// Normann et al., 1995 cf. Rasmus Houborg 2006
double 
Resistance::T_0 (const double T_c /* canopy temperature [dg C]*/, 
                 const double T_soil /* soil temperature [dg C]*/,
                 const double kb /* extinction coefficient []*/,
                 const double LAI /*[m^2 m^-2]*/) 
{
  
  const double f_veg = 1. - exp(-kb * LAI);

  const double T_0 = pow ((f_veg * pow(T_c + TK, 4.) 
                           + (1. - f_veg) * pow(T_soil + TK, 4.)), 0.25) - TK;
  return T_0; // [dg C]
}

// Canopy temperature (K1)
// Ball (1988) & Collatz et al (1991) cf. Rasmus Houborg 2006
double 
Resistance::T_c (const double T_l_sun /* leaf sun temperature [dg C]*/, 
                 const double T_l_sha /* leaf shadow temperature [dg C]*/,
                 const double kb /* extinction coefficient []*/,
                 const double LAI /*[m^2 m^-2]*/) 
{
  const double f_sun = (1. - exp(-kb * LAI)/kb)/LAI;

  const double T_c = f_sun * T_l_sun + (1. - f_sun) * T_l_sha; 
  return T_c; // [dg C]
}


//--------------------------------------
// and left overs - miscelaneours 
//--------------------------------------
// air density 
double
Resistance:: rho_a (const double T_a /*[dg C]*/, const double Ptot)
{
  const double rho_a = (Ptot * 1.0E-3 * m_a)/(R * (T_a + TK));
  return rho_a; // [kg m^-3]
}

// resistance.C ends here.
