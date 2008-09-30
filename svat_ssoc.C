// svat_ssoc.C -- SVAT model Sun Shade Open Canopy.
// 
// Copyright 2008 Per Abrahamsen, Birgitte Gjettermann and Søren Hansen
// Copyright 2008 KU.
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

#include "svat.h"
#include "syntax.h"
#include "alist.h"
#include "block.h"
#include "librarian.h"
#include "resistance.h"
#include "fao.h"
#include "soil_heat.h"
#include "bioclimate.h"
#include "soil.h"
#include "surface.h"
#include "geometry.h"
#include "weather.h"
#include "vegetation.h"
#include "assertion.h"
#include "log.h"
#include "treelog.h"
#include "mathlib.h"
#include "librarian.h"
#include <sstream>

struct SVAT_SSOC : public SVAT
{
  // Constants.
  static const double epsilon; // Emmision of Longwave rad == SurEmiss [] 
  static const double sigma;   // Stefan Boltzman constant [W m^-2 K^-4]
  static const double TK;      // Conversion from dg C to K.
  static const double c_p;     // Specific heat of air [J kg^-1 K^-1]

  //Parameters
  const bool hypostomatous;    // True for hypostomatous leaves;

  // Driving variables.
  // - Upper boundary
  double T_a;            // Air surface temperature [K]
  double z_r;            // Reference height above canopy == screen height [m]
  double RH;             // Relative humidity []
  double U_z;            // Surface wind speed [m s^-1]
  double kb;             // Extinction coefficient []

  // - Lower boundary 
  double z0;             // Depth of top cell [m]
  double k_h;            // Heat conductivity in soil [?]  
  double T_z0;           // Soil temperature in top cell [K]
  double E_soil;         // Evaporation of water from soil [kg/m2/s]

  // - Canopy
  bool has_LAI;          // True if there is a vegetation.
  double h_veg;          // Vegetation height [m] 
  double w_l;            // Leaf width [m] 
  double R_abs_soil;     // Absorbed radiation in soil [W m^-2] 
  double R_abs_sun;      // Absorbed radiation in sunlit leaves [W m^-2]
  double R_abs_shadow;   // Absorbed radiation in shadow leaves [W m^-2]
  double cover;          // Vegetation cover (fraction) []
  double LAI;            // Leaf area index [m^2 m^-2]
  double sun_LAI_fraction_total; //Sunlit fraction of LAI []
  double c_drag;         // Drag force [m^2 m^-2]

  // Intermediates.
  double lambda;         // Latent heat of vaporization in atmosphere [J/kg]
  double rho_a;          // Air density [kg m^-3]
  double gamma;          // Psychrometric constant [Pa/K]
  double e_sat_air;      // Saturated water vapour pressure at air temp [Pa]
  double e_a;            // Actual vapour pressure of water in the atmosphere [Pa]
  double s;              // Slope of water vapour pressure curve [Pa/K]

  // Output.
  // - Temperatures * 4
  double T_s;          // Soil surface temperature [K]
  double T_0;          // Surface temperature (large scale) [K]
  double T_c;          // Canopy-point temperature [K]
  double T_sun;        // Temperature of sunlit leaves [K]
  double T_shadow;     // Temperature of shadow leaves [K]

  // - Conductivities * 4
  double g_a;          // Heat conductance of atmosphere [m s^-1]
  double g_H_s_c;      // Heat conductance from soil surface to canopy point [m s^-1]
  double g_H_sun_c;    // Heat conductance from sunlit leaves to canopy point [m s^-1]
  double g_W_sun_c;    // Water conductance from sunlit leaves to canopy point [m s^-1]
  double g_H_shadow_c; // Heat conductance from shadow leaves to canopy point [m s^-1]
  double g_W_shadow_c; // Water conductance from shadow leaves to canopy point [m s^-1]

  // - vapour pressure in canopy
  double e_c;          // Actual vapour pressure of water in the canopy-point [Pa]

  // - Fluxes
  double H_soil;   // Sensible heat flux from the soil [W m^-2] 
  double H_sun;    // Sensible heat flux from the sunlit leaves to the canopy point
                   // [W m^-2] 
  double H_shadow; // Sensible heat flux from the shadow leaves to canopy point
                   // [W m^-2] 
  double H_c_a;    // Sensible heat flux from the canopy point to free atmosphere
                   // [W m^-2] 
  double LE_sun;   // Latent heat flux from the sunlit leaves to the canopy point
                   // [W m^-2] 
  double LE_shadow;// Latent heat flux from the shadow leaves to the canopy point
                   // [W m^-2] 
  double  LE_atm;  // Latent heat flux from the canopy point to the free atmosphere
                   // [W m^-2]   

  // - Transpiration
  double  E_trans; // Leaf transpiration [kg m^-2 s^-1]

  // Inter-intermediates variables
  double G_R;       // Radiation "conductivity" [W m^-2 K^-1]
  double G_H_a;     // Heat "conductivity" from soil to free atmosphere [W m^-2 K^-1]
  double G_W_a;     // Water "conductivity" from soil to free atmosphere [m s^-1]
  double G_H_s_c;   // Heat "conductance" from soil to canopy point [W m^-2 K^-1]
  double G_H_sun_c; // Heat "conductance" from sun leaves to canopy point [W m^-2 K^-1
  double G_W_sun_c; // Water "conductance" from sun leaves to canopy point [m s^-1]
  double G_H_shadow_c; // Heat "conductance" from shadow leaves to canopy point 
                       // [W m^-2 K^-1]
  double G_W_shadow_c; // Water "conductance" from shadow leaves to canopy point 
                       //[m s^-1]
  double R_eq_abs_soil;   // Equilibrium net-radiation absorbed by the soil [W m^-2]
  double R_eq_abs_sun;    // Equilibrium net-radiation absorbed by the sunlit leaves
                          // [W m^-2]
  double R_eq_abs_shadow; // Equilibrium net-radiation absorbed by the shadow leaves
                          // [W m^-2]

  bool initialized_soil; 
  bool initialized_canopy;
  
  // Simulation.
  void tick (const Weather&, const Vegetation&,
	     const Surface&, const Geometry&, const Soil&, const SoilHeat&,
	     const SoilWater&, const Pet&, const Bioclimate&, Treelog&); 

  void calculate_conductances (const double gs /* stomata cond. [m/s]*/, Treelog& msg);
  
  void calculate_temperatures (Treelog& msg);

  void calculate_fluxes ();

  void solve(const double gs /* stomata cond. [m/s]*/, Treelog& msg);

  double production_stress () const
  { return -1; }

  double transpiration () const
  { return E_trans * 3600.; } // [kg m^-2 s^-1]->[mm h^-1]

  double CanopyTemperature () const
  { return T_c - TK; }  // [dg C]

  double SunLeafTemperature () const
  { return T_sun - TK; }  // [dg C]

  double ShadowLeafTemperature () const
  { return T_shadow - TK; }  // [dg C]

  void output(Log& log) const;

  // Create.
  static void load_syntax (Syntax& syntax, AttributeList& alist);
  SVAT_SSOC (Block& al);
 ~SVAT_SSOC ()
  { }

};

const double SVAT_SSOC::epsilon = 0.98;  //Emmision of Longwave rad == SurEmiss []
const double SVAT_SSOC::sigma = 5.67e-8; //Stefan Boltzman constant [W/m^2/K^4]
const double SVAT_SSOC::TK = 273.15;     //Conversion from dg C to K.
const double SVAT_SSOC::c_p = 1010.;     //Specific heat of air.[J/kg/K^1]

// Simulation.
void
SVAT_SSOC::tick (const Weather& weather, const Vegetation& vegetation,
                 const Surface& surface, const Geometry& geo, const Soil& soil, 
                 const SoilHeat& soil_heat, const SoilWater&, const Pet&, 
                 const Bioclimate& bio, Treelog& msg)
{
  TREELOG_MODEL (msg);

  RH = weather.relative_humidity (); // []
  T_a = weather.air_temperature () + TK; // [K]
  z_r = weather.screen_height (); // [m]
  U_z = bio.wind_speed_field();   // [m s^-1]
  LAI = bio.LAI();              // [m^2 m^-2]
  sun_LAI_fraction_total = bio.sun_LAI_fraction_total ();// []
  has_LAI = (LAI > 0.0);
  h_veg = vegetation.height () / 100.; // [m]  
  w_l = vegetation.leaf_width () / 100.; // [m] 
  rho_a = Resistance:: rho_a(T_a - TK); //[kg m^-3]
  gamma = FAO::PsychrometricConstant (Resistance::P_surf, T_a - TK); // [Pa/K]
  e_sat_air = FAO::SaturationVapourPressure (T_a - TK); // [Pa]
  e_a = e_sat_air * RH;             // [Pa]; 
  s = FAO::SlopeVapourPressureCurve (T_a - TK); // [Pa/K]
  lambda = FAO::LatentHeatVaporization (T_a - TK); // [J/kg]
  T_z0 = soil_heat.T_top() + TK;     // [K]
  E_soil = surface.evap_soil_surface() / 3600.; // [mm/h]->[kg m^-2 s^-1]
  k_h = geo.content_hood(soil_heat, &SoilHeat::conductivity, Geometry::cell_above)
    * 1e-7 * 3600.0 / 100.0; // [erg/cm/h/dg C] -> [W/m/K]
  z0 = - geo.content_hood(geo, &Geometry::cell_z, Geometry::cell_above)/100.; //[m]
  R_abs_soil = bio.rad_abs_soil ();           // [W m^-2] 
  R_abs_sun = bio.rad_abs_sun_canopy ();      // [W m^-2]
  R_abs_shadow = bio.rad_abs_shadow_canopy ();// [W m^-2]
  cover = vegetation.cover (); //[]

  c_drag = 0.07;
  kb = 0.50 / bio.sin_beta();
  if(kb > 8.0) 
    kb = 8.0;
  if(kb < 0.0)
    kb = 8.0;
  
  if (!initialized_soil) 
    {
      initialized_soil = true;
      T_c = T_a; //[K]
      T_s = T_a;
      T_sun = T_shadow = T_0 = T_c;
      e_c = e_a;
    }

  if(has_LAI)
    {
      if (!initialized_canopy) 
        {
          initialized_canopy = true;
          T_c = T_a; //[K]
          T_sun = T_shadow = T_0 = T_c;
          e_c = e_a;
        }
    }
  else initialized_canopy = false;
}

void 
SVAT_SSOC::calculate_conductances (const double g_s /* stomata cond. [m/s]*/, Treelog& msg)
{
  // Function to correct diffusivities for temperature and pressure
  const double Cl = Resistance::Cl(T_a - TK);
  // Mean leaf size
  const double l_m = Resistance::l_m (w_l);
  // Land surface temperature (large scale, from outer space)
  T_0 = Resistance:: T_0 (T_c - TK, T_s - TK, kb, LAI) + TK; //[K] 
  // Zero-plane displacement height [m] (F3)
  const double d = Resistance::d (h_veg, c_drag, LAI);
  // Atmospheric stability indicator (F4)
  if (U_z <= 0.0) U_z = 0.1;
  //  const double N = Resistance::N (z_r, d, T_0 - TK, T_a - TK, U_z);
  const double N = Resistance::N (z_r, d, T_s - TK, T_a - TK, U_z);
  // Roughness lenght for momentum transport (F2)
  const double z_0 = Resistance::z_0 (h_veg, c_drag, d, LAI);
  // Roughness lenght for sensible heat transfer (F1)
  const double z_0h = Resistance::z_0h (z_0);

  // Aerodynamic heat resistance between canopy and reference height
  const double r_a = Resistance:: r_a(z_r, z_0, z_0h, d, N, U_z); //[s m^-1]
 
  // -------------------------------------------------------                   
  // CANOPY POINT TO ATMOSPHERE
  // -------------------------------------------------------
  // Heat conductance in the atmosphere (between canopy and reference height)
  g_a = 1. / r_a; //[m s^-1]
  
  if(!has_LAI)
    return;
  
  // Wind speed at the top of the canopy 
  //  double U_c = Resistance:: U_c(z_r, z_0, d, U_z, T_0 - TK, 
  //                            T_a - TK, h_veg, r_a, rho_a);
  double U_c = Resistance:: U_c(z_r, z_0, d, U_z, T_s - TK, 
                                T_a - TK, h_veg, r_a, rho_a);
  if (U_c == 0.0) U_c = 0.1;
  
  // Wind speed above the soil surface 
  const double U_s = Resistance:: U_s(l_m, h_veg, LAI, U_c); //[m s ^-1]
  
  // -------------------------------------------------------                   
  // SOIL TO CANOPY POINT
  // -------------------------------------------------------
  // Heat conductance from soil surface to canopy point
  const double r_a_soil = Resistance::r_a_soil (U_s); // [s m^-1]    
  g_H_s_c = 1./r_a_soil;         // [m s^-1]    

  // -------------------------------------------------------                   
  // LEAF TO CANOPY POINT
  // -------------------------------------------------------
  // Heat conductance from leaf to canopy point:
  const double gbu_heat = Resistance::gbu_heat (U_z, w_l, LAI);
  const double gbf_heat = Resistance::gbf_heat(Cl, T_a - TK, T_sun - TK, w_l);
 
  // H2O conductance from leaf to canopy point
  double gbu_H2O, gbf_H2O;

  if (hypostomatous == true)
    {
      // Hypostomatous leaves:
      gbu_H2O = Resistance::gbu_H2O_hypo(gbu_heat, Cl);
      gbf_H2O = Resistance::gbf_H2O_hypo(gbf_heat, Cl);
    }
  else 
    { 
      // Amphistomatous leaves (possesing stomata on both surfaces):
      gbu_H2O = Resistance::gbu_H2O_amph(gbu_heat, Cl);
      gbf_H2O = Resistance::gbf_H2O_amph(gbf_heat, Cl); 
    }

  //Sunlit fraction --------------------------------------------------
  const double LAI_sun = LAI * sun_LAI_fraction_total;
  const double gbu_sun_heat = Resistance::gbu_sun(gbu_heat, LAI, kb);
  const double gbf_sun_heat = Resistance::gbf_sun(gbf_heat, LAI_sun);
  // Heat cunductance from sunlit leaves to canopy point
  g_H_sun_c = gbu_sun_heat + gbf_sun_heat; 

  const double gbu_sun_H2O = Resistance::gbu_sun (gbu_H2O, LAI, kb);
  const double gbf_sun_H2O = Resistance::gbf_sun(gbf_H2O, LAI_sun);
  // Water conductance from sunlit leaves to canopy point
  // - sum of boundary and stomata
  const double r_W_sun_c = 1./(gbu_sun_H2O + gbf_sun_H2O) 
                         + 1./(g_s * sun_LAI_fraction_total); 
  g_W_sun_c = 1./r_W_sun_c;

  //Shadow fraction --------------------------------------------------
  const double LAI_shadow = LAI * (1. - sun_LAI_fraction_total);
  const double gbu_shadow_heat = Resistance::gbu_shadow (gbu_heat, LAI, kb);
  const double gbf_shadow_heat = Resistance::gbf_shadow (gbf_heat, LAI_shadow);
  // Heat conductance from shadow leaves to canopy point
  g_H_shadow_c = gbu_shadow_heat + gbf_shadow_heat;

  const double gbu_shadow_H2O = Resistance::gbu_shadow (gbu_H2O, LAI, kb);
  const double gbf_shadow_H2O = Resistance::gbf_shadow (gbf_H2O, LAI_shadow);
  // Water conductance from shadow leaves to canopy point 
  // - sum of boundary and stomata
  const double r_W_shadow_c = 1./(gbu_shadow_H2O + gbf_shadow_H2O)
                            + 1./(g_s * (1.-sun_LAI_fraction_total));
  g_W_shadow_c =1./r_W_shadow_c;

  /*
  std::ostringstream tmp;
  tmp << "gbu_heat = "<< gbu_heat << ", gbf_heat  = " << gbf_heat << "\n"
      << "gbf_shadow_H2O  = "<< gbf_shadow_H2O 
      << ", gbu_shadow_H2O = " << gbu_shadow_H2O << "\n"
      << "g_W_sun_c = " << g_W_sun_c  << "\n"
      << "gbf_sun_H2O  = "<<  gbf_sun_H2O << ", gbu_sun_H2O = " << gbu_sun_H2O << "\n";
  msg.message (tmp.str ());
  */
}

void 
SVAT_SSOC:: calculate_temperatures(Treelog& msg)
{
  // Intermediates variable ------------------------------

  // Radiation "conductivity"
  G_R = 4. * epsilon * sigma * pow(T_a, 3.); //[W m^-2 K^-1]
  // Sensible heat "conductance" from soil to atmosphere
  G_H_a = c_p * rho_a * g_a;            //[W m^-2 K^-1] 
  // Latent heat "conductance" from soil to atmosphere
  G_W_a =  c_p * rho_a * g_a / gamma;  //[m s^-1] 
  
  if (has_LAI) // canopy and soil 
    {
      // Sensible heat "conductance" between soil and canopy point
      G_H_s_c =  c_p * rho_a * g_H_s_c;       //[W m^-2 K^-1] 
      // Sensible heat "conductance" between sunlit leaves and canopy point
      G_H_sun_c =  c_p * rho_a * g_H_sun_c;   //[W m^-2 K^-1] 
      // Sensible heat "conductance" between shadow leaves and canopy point
      G_H_shadow_c =  c_p * rho_a * g_H_shadow_c;   //[W m^-2 K^-1] 
      
      // Latent heat "conductance" between sunlit leaves and canopy point
      G_W_sun_c =  c_p * rho_a * g_W_sun_c / gamma;  //[m s^-1] 
      G_W_shadow_c =  c_p * rho_a * g_W_shadow_c / gamma;  //[m s^-1] 
    }

  // Black-body emmission:
  const double BB = epsilon * sigma * pow(T_a, 4.); //[W m^-2]
  
  // "Temperature equilibrium" net-radiation absorbed by the soil
  R_eq_abs_soil = R_abs_soil - BB * (1. - cover);           //[W m^-2]
  // Equilibrium net-radiation absorbed by the sunlit leaves
  R_eq_abs_sun = R_abs_sun - BB * cover * sun_LAI_fraction_total; //[W m^-2]
  // Equilibrium net-radiation absorbed by the shadow leaves
  R_eq_abs_shadow = R_abs_shadow - BB * cover * (1.- sun_LAI_fraction_total);//[W m^-2]

  if (has_LAI) // canopy and soil 
    {
      // inter-inter-intermediate variables
      const double a_soil = ((R_eq_abs_soil + G_R * (T_a)
                              + k_h/z0 * (T_z0) - lambda * E_soil) 
                             / (G_R + G_H_s_c + k_h/z0)); //[K]
      
      const double a_soil_c = G_H_s_c / (G_R + G_H_s_c + k_h/z0); // []
      
      const double a_can = ((G_H_a * T_a + G_H_s_c * a_soil 
                             + G_H_sun_c * (T_sun))
                            /(G_H_a + G_H_s_c * (1. - a_soil_c)
                              + G_H_sun_c + G_H_shadow_c));//[K]
      
      const double a_can_sun = G_H_sun_c / (G_H_a + G_H_s_c * (1. - a_soil_c) 
                                            + G_H_sun_c + G_H_shadow_c); // []
      
      const double a_can_shadow = G_H_shadow_c / (G_H_a + G_H_s_c * (1. - a_soil_c) 
                                                  + G_H_sun_c + G_H_shadow_c); // []

      const double b_can = ((G_W_a * e_a /*[W m^-2]*/ + lambda * E_soil /*[W m^-2]*/
                             + G_W_sun_c * (e_sat_air - s * (T_a)) /*[W m^-2]*/
                             + G_W_shadow_c * (e_sat_air - s * (T_a)))
                            /(G_W_a + G_W_sun_c + G_W_shadow_c /*[m s^-1]*/)); 
                           //[W s m^-3]
      
      const double b_can_sun = ((G_W_sun_c /*[m s^-1]*/ * s /*[W s m^-3 K^-1]*/)
                                /(G_W_a + G_W_sun_c + G_W_shadow_c)); //[W s m^-3 K^-1]

      const double b_can_shadow = ((G_W_shadow_c * s)
                                   /(G_W_a + G_W_sun_c + G_W_shadow_c));//[W s m^-3 K^-1]
      
      const double c_sun = ((R_eq_abs_sun + G_R * (T_a) + G_H_sun_c * a_can 
                             + G_W_sun_c * (s * (T_a) - e_sat_air + b_can))
                            /*[W m^-2]*/
                            / (G_R + G_H_sun_c * (1. - a_can_sun)
                               + G_W_sun_c * (s - b_can_sun)))/*[W m^-2 K^-1]*/; // [K]
      
      const double c_sun_shadow = ((G_H_sun_c * a_can_shadow 
                                    + G_W_sun_c * b_can_shadow)/*[W m^-2 K^-1]*/
                                   /(G_R + G_H_sun_c * (1. - a_can_sun)
                                     + G_W_sun_c * (s - b_can_sun))/*[W m^-2 K^-1]*/);
                                   //[]
      
      const double c_shadow = ((R_eq_abs_shadow + G_R * (T_a) + G_H_shadow_c * a_can 
                                + G_W_shadow_c * (s * (T_a) - e_sat_air + b_can)) 
                               / (G_R + G_H_shadow_c * (1. - a_can_shadow)
                                  + G_W_shadow_c * (s - b_can_shadow))); //[K]
      
      const double c_shadow_sun = ((G_H_sun_c * a_can_shadow 
                                    + G_W_sun_c * b_can_shadow)
                                   /(G_R + G_H_shadow_c * (1. - a_can_shadow)
                                     + G_W_shadow_c * (s - b_can_shadow)));//[]
      
      // -------------------------------------------
      // Temperature of sunlit leaves 
      // -------------------------------------------
      T_sun = ((c_sun + c_sun_shadow * c_shadow) 
               / (1. - c_sun_shadow * c_shadow_sun)); //[K]
      
      // -------------------------------------------
      // Temperature of shadow leaves 
      // -------------------------------------------
      T_shadow = (c_shadow + c_shadow_sun * (T_sun)); //[K]
      //T_shadow = (T_sun - c_sun)/c_sun_shadow; //[K]

      //daisy_assert (T_sun >= T_shadow);
      
      // -------------------------------------------
      // Canopy-point temperature
      // -------------------------------------------
      T_c = (a_can + a_can_sun * (T_sun) + a_can_shadow * (T_shadow)); //[K]

      // -------------------------------------------
      // Soil surface temperature
      // -------------------------------------------
      T_s =(a_soil + a_soil_c * (T_c));  //[K]

      // -------------------------------------------
      // Canopy vapour pressure 
      // -------------------------------------------
      e_c = b_can + b_can_sun * T_sun + b_can_shadow * T_shadow; //[Pa]
     
      std::ostringstream tmp;
      tmp //<< "a_soil  = "<< a_soil << ", a_soil_c = " << a_soil_c << "\n"
        << "a_can  = "<< a_can << ", a_can_sun = " << a_can_sun << "\n";
        //<< "T_sun  = "<< T_sun << ", T_shadow = " << T_shadow << "\n"
        //<< "T_z0 = "<< T_z0 << ", c_shadow = " << c_shadow << "\n"
        //<< "G_H_s_c  = "<< G_H_s_c  << ", k_h/z0 = " << k_h/z0 << "\n"
        //<< "a_can_shadow  = "<< a_can_shadow <<  "\n";
      
      msg.message (tmp.str ());
     
    }

  else // bare soil
    T_s =((R_eq_abs_soil + G_R * (T_a) + G_H_a * (T_a) 
           + k_h / z0 * (T_z0) - (lambda * E_soil))
          / (G_R + G_H_a +  k_h / z0));  //[K]

} 

void
SVAT_SSOC:: calculate_fluxes()
{
  // Sensible heat flux from the bare soil
  H_soil = G_H_a * (T_s - T_a) ;         //[W m^-2] 
  E_trans = 0.0; //Bare soil. No vegetation and leaf transpiration  [kg m^-2 s^-1]

  if (has_LAI)
    { 
      // Sensible heat flux from the soil (overwriting)
      H_soil =  G_H_s_c * (T_s - T_c);      //[W m^-2] 
      // Sensible heat flux from the sunlit leaves to the canopy point
      H_sun =  G_H_sun_c * (T_sun - T_c);   //[W m^-2] 
      // Sensible heat flux from the shadow leaves to the canopy point
      H_shadow =  G_H_shadow_c * (T_shadow - T_c) ; //[W m^-2] 
      // Sensible heat flux from the canopy point to free atmosphere
      H_c_a = G_H_a * (T_c - T_a);      //[W m^-2] 
      
      const double e_sat_sun = FAO::SaturationVapourPressure (T_sun - TK); // [Pa]
      const double e_sat_shadow = FAO::SaturationVapourPressure (T_shadow - TK); // [Pa]
      // Latent heat flux from the sunlit leaves to the canopy point
      //LE_sun =  G_W_sun_c * (e_sat_sun - e_c);         // [W m^-2]
      LE_sun =  G_W_sun_c * ((s * (T_sun - T_a)) + (e_sat_air - e_c));  // [W m^-2]
      // Latent heat flux from the shadow leaves to the canopy point
      //LE_shadow = G_W_shadow_c * (e_sat_shadow - e_c); // [W m^-2]
      LE_shadow = G_W_shadow_c * ((s * (T_shadow - T_a)) + (e_sat_air - e_c));//[W m^-2]
      // Latent heat flux from the canopy point to the free atmosphere
      LE_atm = G_W_a * (e_c - e_a);                   // [W m^-2]
      
      // Transpiration
      E_trans = (LE_sun + LE_shadow) / lambda;        //[kg m^-2 s^-1]==[mm/s]
    }
}


void
SVAT_SSOC::solve(const double gs /* stomata cond. [m/s]*/, Treelog& msg )
{
  TREELOG_MODEL (msg);
  std::ostringstream tmp;
  const double maxTdiff = 0.1; //[K]
  const double maxEdiff = 1.; //[Pa]
  const double max_iteration = 150.;

  for (int i = 0; i<max_iteration; i++)
    {    
      const double old_T_c = T_c;
      const double old_T_s = T_s;
      const double old_T_sun = T_sun;
      const double old_T_shadow = T_shadow;
      const double old_e_c = e_c; 

      calculate_conductances(gs, msg);
      calculate_temperatures(msg);
 
      /*    
      tmp << "old_T_s  = " << old_T_s << ", T_s = " << T_s << "\n"
          << "T_a = " << T_a << "\n"
          << "old_T_c  = "<< old_T_c << ", Tc = " << T_c << "\n"
          << "old_T_sun  = "<< old_T_sun << ", T_sun = " << T_sun << "\n"  
          << "old_T_shadow  = "<< old_T_shadow << ", T_shadow = " << T_shadow << "\n"
          << "old_e_c = " << old_e_c << ", e_c = " << e_c  << "\n"
          << "i = " << i << "\n";
      */
      if(std::fabs(old_T_c - T_c) < maxTdiff 
         && std::fabs(old_T_s - T_s) < maxTdiff 
         && std::fabs(old_T_sun - T_sun) < maxTdiff 
         && std::fabs(old_T_shadow - T_shadow) < maxTdiff 
         && std::fabs( old_e_c - e_c) < maxEdiff)
        goto success;
    } 
  msg.error("Too many iterations.");
 success:
  
  tmp << "T_a  = "<< T_a << ", T_s = " << T_s << "\n"
      << "T_sun = " << T_sun << ", T_shadow = " << T_shadow << "\n"
      << "T_c  = "<< T_c << ", e_c = " << e_c << "\n";
  msg.message (tmp.str ());
  
  calculate_fluxes();
}

void
SVAT_SSOC::output(Log& log) const
{
  if (initialized_soil)
    {
      output_variable (T_s, log);
      output_variable (T_0, log);
      output_variable (g_a, log);  
      output_variable (e_a, log);
      output_variable (e_sat_air, log);
      output_variable (s, log);
      output_variable (R_abs_soil, log);
      output_variable (R_eq_abs_soil, log);
      output_variable (H_soil, log);
    }
  if (has_LAI)
    {
      output_variable (T_sun, log);
      output_variable (T_shadow, log);
      output_variable (T_c, log);
      output_variable (g_H_s_c, log); 
      output_variable (g_H_sun_c, log);  
      output_variable (g_W_sun_c, log);
      output_variable (g_H_shadow_c, log);
      output_variable (g_W_shadow_c, log);      
      output_variable (R_abs_sun, log);
      output_variable (R_abs_shadow, log);
      output_variable (R_eq_abs_sun, log);
      output_variable (R_eq_abs_shadow, log);
      output_variable (cover, log);
      output_variable (sun_LAI_fraction_total, log);
      output_variable (LAI, log);
      output_variable (H_sun, log);
      output_variable (H_shadow, log);
      output_variable (H_c_a, log);
      output_variable (LE_sun, log);
      output_variable (LE_shadow, log);
      output_variable (LE_atm, log);
      output_variable (E_trans, log);
      output_variable (e_c, log);
    }
}

void 
SVAT_SSOC::load_syntax (Syntax& syntax, AttributeList& alist)
{
  SVAT::load_syntax (syntax, alist);

  syntax.add ("hypostomatous", Syntax::Boolean, Syntax::Const,
              "True for hypostomatous leaves. \n\
False for amphistomatous leaves (possesing stomata on both surfaces).");
  alist.add ("hypostomatous", true);
  
  // For log.
  syntax.add ("T_s", "K", Syntax::LogOnly, "Soil surface temperature.");
  syntax.add ("T_0", "K", Syntax::LogOnly, "Surface temperature (large scale).");
  syntax.add ("T_c", "K", Syntax::LogOnly, "Canopy-point temperature.");
  syntax.add ("T_sun", "K", Syntax::LogOnly, "Temperature of sunlit leaves.");
  syntax.add ("T_shadow", "K", Syntax::LogOnly, "Temperature of shadow leaves.");
  syntax.add ("g_a", "m s^-1", Syntax::LogOnly, 
              "Heat conductance in the atmosphere - from canopy point \n\
to reference height (screen height).");
  syntax.add ("g_H_s_c", "m s^-1", Syntax::LogOnly, 
              "Heat conductance from soil surface to canopy point.");
  syntax.add ("g_H_sun_c", "m s^-1", Syntax::LogOnly, 
              "Heat conductance from sunlit leaves to canopy point.");
  syntax.add ("g_W_sun_c", "m s^-1", Syntax::LogOnly, 
              "Water conductance from sunlit leaves to canopy point.");
  syntax.add ("g_H_shadow_c", "m s^-1", Syntax::LogOnly,
              "Heat conductance from shadow leaves to canopy point.");
  syntax.add ("g_W_shadow_c", "m s^-1", Syntax::LogOnly,
              "Water conductance from shadow leaves to canopy point.");
  syntax.add ("e_a", "Pa", Syntax::LogOnly, 
              "Vapour pressure of water in the atmosphere.");  
  syntax.add ("e_sat_air", "Pa", Syntax::LogOnly, 
              "Saturated vapour pressure of water in the air.");  
  syntax.add ("s", "Pa K^-1", Syntax::LogOnly, 
              "Slope of water vapour pressure curve.");  
  syntax.add ("e_c", "Pa", Syntax::LogOnly, 
              "Vapour pressure of water in the canopy.");
  syntax.add ("R_abs_soil", "W m^-2", Syntax::LogOnly, "Absorbed radiation in soil.");
  syntax.add ("R_eq_abs_soil", "W m^-2", Syntax::LogOnly, 
              "Absorbed radiation in soil at equilibrium.");
  syntax.add ("R_abs_sun", "W m^-2", Syntax::LogOnly, 
              "Absorbed radiation in sunlit leaves.");
  syntax.add ("R_eq_abs_sun", "W m^-2", Syntax::LogOnly, 
              "Absorbed radiation in sunlit leaves at equilibrium.");
  syntax.add ("R_abs_shadow", "W m^-2", Syntax::LogOnly, 
              "Absorbed radiation in shadow leaves.");
  syntax.add ("R_eq_abs_shadow", "W m^-2", Syntax::LogOnly, 
              "Absorbed radiation in shadow leaves at equilibrium."); 
  syntax.add ("LAI", "m^2 m^-2", Syntax::LogOnly, "Leaf area index.");
  syntax.add ("sun_LAI_fraction_total","", Syntax::LogOnly, 
              "Sunlit fraction of leaf area in the canopy.");
  syntax.add ("H_soil", "W m^-2", Syntax::LogOnly, 
              "Sensible heat flux from the soil.");
  syntax.add ("H_sun", "W m^-2", Syntax::LogOnly, 
              "Sensible heat flux from the sunlit leaves to the canopy point.");
  syntax.add ("H_shadow", "W m^-2", Syntax::LogOnly, 
              "Sensible heat flux from the shadow leaves to canopy point.");
  syntax.add ("H_c_a", "W m^-2", Syntax::LogOnly, 
              "Sensible heat flux from the canopy point to free atmosphere.");
  syntax.add ("LE_sun", "W m^-2", Syntax::LogOnly, 
              "Latent heat flux from the sunlit leaves to the canopy point.");
  syntax.add ("LE_shadow", "W m^-2", Syntax::LogOnly, 
              "Latent heat flux from the shadow leaves to the canopy point.");
  syntax.add ("LE_atm", "W m^-2", Syntax::LogOnly, 
              "Latent heat flux from the canopy point to the free atmosphere.");
  syntax.add ("E_trans", "kg m^-2 s^-1", Syntax::LogOnly, "Leaf transpiration.");

  //  syntax.add ("", "", Syntax::LogOnly, ".");
}

SVAT_SSOC::SVAT_SSOC (Block& al)
  : SVAT (al), 
    hypostomatous (al.flag ("hypostomatous")),
    T_a (-42.42e42),
    z_r (-42.42e42),
    RH (-42.42e42),
    U_z (-42.42e42),
    kb (-42.42e42),
    z0 (-42.42e42),
    k_h (-42.42e42),
    T_z0 (-42.42e42),
    E_soil (-42.42e42),
    h_veg (-42.42e42),
    w_l (-42.42e42),
    R_abs_soil(-42.42e42),
    R_abs_sun(-42.42e42),
    R_abs_shadow(-42.42e42),
    LAI(-42.42e42),
    sun_LAI_fraction_total(-42.42e42),
    c_drag(-42.42e42),
    lambda (-42.42e42),
    rho_a (-42.42e42),
    gamma (-42.42e42),
    e_sat_air (-42.42e42),
    e_a (-42.42e42),
    s (-42.42e42),
    T_s (-42.42e42),
    T_0 (-42.42e42),
    T_c (-42.42e42),
    T_sun (-42.42e42),
    T_shadow (-42.42e42),
    g_a (-42.42e42),
    g_H_s_c (-42.42e42),
    g_H_sun_c (-42.42e42),
    g_W_sun_c (-42.42e42),
    g_H_shadow_c (-42.42e42),
    g_W_shadow_c (-42.42e42),
    e_c (-42.42e42),
    H_soil (-42.42e42),
    H_sun (-42.42e42),
    H_shadow (-42.42e42),
    H_c_a (-42.42e42),
    LE_sun (-42.42e42),
    LE_shadow (-42.42e42),
    LE_atm (-42.42e42),
    E_trans (-42.42e42),
    G_R (-42.42e42),
    G_H_a (-42.42e42),
    G_W_a (-42.42e42),
    G_H_s_c (-42.42e42),
    G_H_sun_c (-42.42e42),
    G_W_sun_c (-42.42e42),
    G_H_shadow_c (-42.42e42),
    G_W_shadow_c (-42.42e42),
    R_eq_abs_soil (-42.42e42),
    R_eq_abs_sun (-42.42e42),
    R_eq_abs_shadow (-42.42e42),
    initialized_soil (false), 
    initialized_canopy (false) 
{ }

static struct SVAT_SSOCSyntax
{
  static Model& make (Block& al)
  { return *new SVAT_SSOC (al); }
  SVAT_SSOCSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    SVAT_SSOC::load_syntax (syntax, alist);
    Librarian::add_type (SVAT::component, "SSOC", alist, syntax, &make);
  }
} SVAT_ssoc_syntax;
