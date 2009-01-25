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
#include "block.h"
#include "librarian.h"
#include "resistance.h"
#include "fao.h"
#include "soil_heat.h"
#include "bioclimate.h"
#include "soil.h"
#include "geometry.h"
#include "weather.h"
#include "vegetation.h"
#include "assertion.h"
#include "log.h"
#include "treelog.h"
#include "mathlib.h"
#include "librarian.h"
#include "solver.h"
#include "frame.h"
#include <memory>
#include <sstream>

struct SVAT_SSOC : public SVAT
{
  const std::auto_ptr<Solver> solver;

  // Constants.
  static const double epsilon; // Emmision of Longwave rad == SurEmiss [] 
  static const double sigma;   // Stefan Boltzman constant [W m^-2 K^-4]
  static const double TK;      // Conversion from dg C to K.
  static const double c_p;     // Specific heat of air [J kg^-1 K^-1]

  //Parameters
  const bool hypostomatous;    // True for hypostomatous leaves;
  const double maxTdiff;        // Largest temperature difference for conv. [K]
  const double maxEdiff;        // Largest humidity difference for converg. [Pa]
  const double max_iteration;   // Max number of iterations before giving up c.

  // Driving variables.
  // - Upper boundary
  double T_a;            // Air surface temperature [K]
  double z_r;            // Reference height above canopy == screen height [m]
  double RH;             // Relative humidity []
  double U_z;            // Surface wind speed [m s^-1]
  double canopy_ea;      // Evaporation of intercepted water [mm/h]
  double kb;             // Extinction coefficeint
  bool has_light;        // True if there is light.

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
  double g_H_leaf_c; // Heat conductance from leaves to canopy point (night time)
  double g_W_leaf_c; // Water conductance from leaves to canopy point (night time)

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
  double LE_atm;  // Latent heat flux from the canopy point to the free atmosphere
                   // [W m^-2]   

  // - Transpiration
  double E_trans; // Leaf transpiration [kg m^-2 s^-1]

  // Inter-intermediates variables
  double G_R_soil;  // Radiation "conductivity" from the soil [W m^-2 K^-1]
  double G_R_sun;   // Radiation "conductivity" from sun leaves [W m^-2 K^-1]
  double G_R_shadow;// Radiation "conductivity" from shadow leaves [W m^-2 K^-1]
  double G_R_leaf;// Radiation "conductivity" from shadow leaves [W m^-2 K^-1]
  double G_H_a;     // Heat "conductivity" from soil to free atmosphere [W m^-2 K^-1]
  double G_W_a;     // Water "conductivity" from soil to free atmosphere [m s^-1]
  double G_H_s_c;   // Heat "conductance" from soil to canopy point [W m^-2 K^-1]
  double G_H_sun_c; // Heat "conductance" from sun leaves to canopy point [W m^-2 K^-1
  double G_W_sun_c; // Water "conductance" from sun leaves to canopy point [m s^-1]
  double G_H_shadow_c; // Heat "conductance" from shadow leaves to canopy point 
                       // [W m^-2 K^-1]
  double G_W_shadow_c; // Water "conductance" from shadow leaves to canopy point 
                       //[m s^-1]
  double G_H_leaf_c; // Heat "conductance" from leaves to canopy point (night time)
  double G_W_leaf_c; // Water "conductance" from leaves to canopy point (night time)
  double R_eq_abs_soil;   // Equilibrium net-radiation absorbed by the soil [W m^-2]
  double R_eq_abs_sun;    // Equilibrium net-radiation absorbed by the sunlit leaves
                          // [W m^-2]
  double R_eq_abs_shadow; // Equilibrium net-radiation absorbed by the shadow leaves
                          // [W m^-2]

  bool initialized_soil; 
  bool initialized_canopy;
  size_t converge_fail;
  size_t converge_total;

  // Simulation.
  void tick (const Weather&, const Vegetation&,
	     const Geometry&, const Soil&, const SoilHeat&,
	     const SoilWater&, const Bioclimate&, Treelog&); 

  void calculate_conductances (const double gs /* stomata cond. [m/s]*/, Treelog& msg);
  
  void calculate_temperatures (Treelog& msg);

  void calculate_fluxes ();

  void solve(const double gs /* stomata cond. [m/s]*/, Treelog& msg);

  double production_stress () const
  { return -1; }

  double transpiration () const
  {
    const double svat_ea = E_trans * 3600.; // [kg m^-2 s^-1]->[mm h^-1]
    return std::max (0.0, svat_ea - canopy_ea); 
  } 

  double CanopyTemperature () const
  { return T_c - TK; }  // [dg C]

  double SunLeafTemperature () const
  { return T_sun - TK; }  // [dg C]

  double ShadowLeafTemperature () const
  { return T_shadow - TK; }  // [dg C]

  void output(Log& log) const;

  // Create.
  SVAT_SSOC (Block& al);
  void summarize (Treelog& msg) const
  { 
    if (converge_fail > 0)
      {
        TREELOG_MODEL (msg);
        daisy_assert (converge_total > 0);
        std::ostringstream tmp;
        tmp << "Convergence failed " << converge_fail << " times out of "
            << converge_total << "; or "
            << (100.0 * converge_fail / (converge_total + 0.0)) << "%";
        msg.warning (tmp.str ());
        msg.message ("See 'daisy.log' for details");
      }
  }
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
                 const Geometry& geo, const Soil& soil, 
                 const SoilHeat& soil_heat, const SoilWater&, 
                 const Bioclimate& bio, Treelog& msg)
{
  TREELOG_MODEL (msg);

  RH = weather.relative_humidity (); // []
  T_a = weather.air_temperature () + TK; // [K]
  z_r = weather.screen_height (); // [m]
  U_z = bio.wind_speed_field();   // [m s^-1]
  canopy_ea = bio.canopy_ea ();
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
  E_soil =bio.soil_surface_ea () / 3600.; // [mm/h]->[kg m^-2 s^-1]
  k_h = geo.content_hood(soil_heat, &SoilHeat::conductivity, Geometry::cell_above)
    * 1e-7 * 3600.0 / 100.0; // [erg/cm/h/dg C] -> [W/m/K]
  z0 = - geo.content_hood(geo, &Geometry::cell_z, Geometry::cell_above)/100.; //[m]
  R_abs_soil = bio.rad_abs_soil ();           // [W m^-2] 
  R_abs_sun = bio.rad_abs_sun_canopy ();      // [W m^-2]
  R_abs_shadow = bio.rad_abs_shadow_canopy ();// [W m^-2]
  cover = vegetation.cover (); //[]
  c_drag = 0.07;

  const double sin_beta = bio.sin_beta ();
  const double min_sin_beta = bio.min_sin_beta ();
  has_light = (sin_beta > min_sin_beta);
  if (has_light)
    kb = 0.50 / sin_beta;
  else
    kb = 0.60;
      

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
          T_s = T_c;
          T_sun = T_shadow = T_0 = T_c;
          e_c = e_a;
        }
    }
  else initialized_canopy = false;

#if 1 // PACHANGES
  T_c = T_a; //[K]
  T_sun = T_shadow = T_s = T_0 = T_c;
  e_c = e_a;
#endif
}

void 
SVAT_SSOC::calculate_conductances (const double g_s /* stomata cond. [m/s]*/, Treelog& msg)
{
  // Function to correct diffusivities for temperature and pressure
  const double Cl = Resistance::Cl(T_a - TK);
  // Mean leaf size
  const double l_m = Resistance::l_m (w_l);
  // Zero-plane displacement height [m] (F3)
  const double d = Resistance::d (h_veg, c_drag, LAI);
  // Atmospheric stability indicator (F4)
  if (U_z <= 0.0) U_z = 0.1;
  const double N = Resistance::N (z_r, d, T_c - TK, T_a - TK, U_z);
  // const double N = Resistance::N (z_r, d, T_0 - TK, T_a - TK, U_z);
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

  /*
  std::ostringstream tmp;
  tmp << "g_a = "<< g_a << ", r_a  = " << r_a << "\n"
      << "U_z  = "<< U_z  << ", N = " << N << "\n"
      << "g_W_sun_c = " << g_W_sun_c  << "\n";
  msg.message (tmp.str ());
  
  */
  if(!has_LAI)
    return;
  
  // Wind speed at the top of the canopy 
  //  double U_c = Resistance:: U_c(z_r, z_0, d, U_z, T_0 - TK, 
  //                            T_a - TK, h_veg, r_a, rho_a);
  double U_c = Resistance:: U_c(z_r, z_0, d, U_z, T_c - TK, 
                                T_a - TK, h_veg, r_a, rho_a);
  if (U_c <= 0.0) U_c = 0.1;
  
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
  if(has_light)
    {
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
    }

  if(!has_light)
    {
      //Only one (shadow) fraction --------------------------------------------------
      const double gbu_leaf_heat = Resistance::gbu_shadow(gbu_heat, LAI, kb);
      const double gbf_leaf_heat = Resistance::gbf_shadow(gbf_heat, LAI);
      // Heat cunductance from sunlit leaves to canopy point
      g_H_leaf_c = gbu_leaf_heat + gbf_leaf_heat; 
      
      const double gbu_leaf_H2O = Resistance::gbu_shadow (gbu_H2O, LAI, kb);
      const double gbf_leaf_H2O = Resistance::gbf_shadow(gbf_H2O, LAI);
      // Water conductance from leaves to canopy point
      // - sum of boundary and stomata
      const double r_W_leaf_c = 1./(gbu_leaf_H2O + gbf_leaf_H2O); 
      g_W_leaf_c = 1./r_W_leaf_c;
    }
  
}

void 
SVAT_SSOC:: calculate_temperatures(Treelog& msg)
{
  // Intermediates variable ------------------------------

  // Radiation "conductivity"
  G_R_soil = 4. * epsilon * sigma * pow(T_a, 3.) * (1. - cover); //[W m^-2 K^-1]
 
  // Sensible heat "conductance" from soil to atmosphere
  G_H_a = c_p * rho_a * g_a;            //[W m^-2 K^-1] 
  // Latent heat "conductance" from soil to atmosphere
  G_W_a =  c_p * rho_a * g_a / gamma;  //[m s^-1] 

  // Black-body emmission:
  const double BB = epsilon * sigma * pow(T_a , 4.); //[W m^-2]
  // "Temperature equilibrium" net-radiation absorbed by the soil
  R_eq_abs_soil = R_abs_soil - BB * (1. - cover);           //[W m^-2]

  
  if (has_LAI && has_light) // canopy and soil during daytime
    {
      // Radiation "conductivity"
      G_R_sun = 4. * epsilon * sigma * pow(T_a, 3.) * cover 
        * sun_LAI_fraction_total; //[W m^-2 K^-1]
      G_R_shadow = 4. * epsilon * sigma * pow(T_a, 3.) * cover
        * (1. - sun_LAI_fraction_total); //[W m^-2 K^-1]
      
      // Equilibrium net-radiation absorbed by the sunlit leaves
      R_eq_abs_sun = R_abs_sun - BB * cover * sun_LAI_fraction_total; //[W m^-2]
      // Equilibrium net-radiation absorbed by the shadow leaves
      R_eq_abs_shadow = R_abs_shadow - BB * cover * (1.- sun_LAI_fraction_total);
                        //[W m^-2]
   
      // Sensible heat "conductance" between soil and canopy point
      G_H_s_c =  c_p * rho_a * g_H_s_c;// * (1. - cover);       //[W m^-2 K^-1] 
      // Sensible heat "conductance" between sunlit leaves and canopy point
      G_H_sun_c =  c_p * rho_a * g_H_sun_c;// * cover *  sun_LAI_fraction_total;   //[W m^-2 K^-1] 
      // Sensible heat "conductance" between shadow leaves and canopy point
      G_H_shadow_c =  c_p * rho_a * g_H_shadow_c;// * cover *  (1.-sun_LAI_fraction_total);   //[W m^-2 K^-1] 
      
      // Latent heat "conductance" between sunlit leaves and canopy point
      G_W_sun_c =  c_p * rho_a * g_W_sun_c / gamma;// * cover *  sun_LAI_fraction_total;  //[m s^-1] 
      G_W_shadow_c =  c_p * rho_a * g_W_shadow_c / gamma;// * cover *  (1.-sun_LAI_fraction_total);  //[m s^-1] 
    
      // We have an equation system with five unknowns, and five equations.

      // We assign a number and an equation to each unknown.
      enum { iT_s, iT_c, ie_c, iT_sun, iT_shadow, isize };

      // A x = b
      Solver::Matrix A (isize); 
      Solver::Vector b (isize);
      Solver::Vector x (isize);

      // 1. The soil equation.
      // 
      // R_eq_abs_soil - G_R_soil * (T_s - T_a)
      // = (k_h / z0) * (T_s - T_z0) + G_H_s_c * (T_s - T_c) + lambda * E_soil
      // 
      // <=>
      //
      // R_eq_abs_soil + G_R_soil * T_a + (k_h / z0) * T_z0 - lambda * E_soil
      // = (G_R_soil + (k_h / z0) + G_H_s_c) * T_s
      // - G_H_s_c * T_c

      b (iT_s) = R_eq_abs_soil + G_R_soil * T_a 
               + (k_h / z0) * T_z0 - lambda * E_soil;
      A (iT_s, iT_s) = G_R_soil + (k_h / z0) + G_H_s_c;
      A (iT_s, iT_c) = - G_H_s_c;
        
      // 2. The canopy equation.
      // 
      // H_atm = H_soil + H_sun + H_shadow 
      // 
      // <=>
      //
      // G_H_a * (T_c - T_a) 
      // = G_H_s_c * (T_s - T_c) + G_H_sun_c * (T_sun - T_c)
      // + G_H_shadow_c * (T_shadow - T_c)
      //
      // - G_H_a * T_a
      // = - (G_H_a + G_H_s_c + G_H_sun_c + G_H_shadow_c) * T_c 
      // + G_H_s_c * T_s + G_H_sun_c * T_sun + G_H_shadow_c * T_shadow
      
      b (iT_c) = - G_H_a * T_a;
      A (iT_c, iT_s) = G_H_s_c;
      A (iT_c, iT_c) = - (G_H_a + G_H_s_c + G_H_sun_c + G_H_shadow_c);
      A (iT_c, iT_sun) = G_H_sun_c;
      A (iT_c, iT_shadow) = G_H_shadow_c;

      // 3. The vapour pressure equation.
      // 
      // LE_atm  = LE_soil + LE_sun + LE_shadow
      // 
      // <=>
      //
      // G_W_a * (e_c - e_a) 
      // = lambda * E_soil 
      // + G_W_sun_c * (s * (T_sun - T_a) + (e_sat_air - e_c))
      // + G_W_shadow_c * (s * (T_shadow - T_a) + (e_sat_air - e_c))
      //
      // <=>
      //
      // (G_W_sun_c + G_W_shadow_c) * (s * T_a - e_sat_air)
      // - G_W_a * e_a - lambda * E_soil 
      // = G_W_sun_c * s * T_sun + G_W_shadow_c * s * T_shadow
      // - (G_W_a + G_W_sun_c + G_W_shadow_c) * e_c
      
      b (ie_c) = (G_W_sun_c + G_W_shadow_c) * (s * T_a - e_sat_air)
        - G_W_a * e_a - lambda * E_soil;
      A (ie_c, ie_c) = - (G_W_a + G_W_sun_c + G_W_shadow_c);
      A (ie_c, iT_sun) = G_W_sun_c * s;
      A (ie_c, iT_shadow) = G_W_shadow_c * s;

      // 4. The sunlit leaves equation.
      // 
      // R_abs_sun = H_sun_c + lambda * E_sun_c
      // 
      // R_eq_abs_sun - G_R_sun * (T_sun - T_a) 
      // = G_H_sun_c * (T_sun - T_c) 
      // + G_W_sun_c * (s * (T_sun - T_a) + (e_sat_air - e_c))
      // 
      // <=>
      //
      // R_eq_abs_sun + (G_R_sun + G_W_sun_c * s) * T_a - G_W_sun_c * e_sat_air
      // = (G_R_sun + G_H_sun_c + G_W_sun_c * s) * T_sun 
      // - G_H_sun_c * T_c - G_W_sun_c * e_c
      
      b (iT_sun) = R_eq_abs_sun + (G_R_sun + G_W_sun_c * s) * T_a 
                 - G_W_sun_c * e_sat_air;
      A (iT_sun, iT_c) = - G_H_sun_c;
      A (iT_sun, ie_c) = - G_W_sun_c;
      A (iT_sun, iT_sun) = (G_R_sun + G_H_sun_c + G_W_sun_c * s);

      // 5. The shadow leaves equation.
      // 
      // R_abs_shadow = H_shadow_c + lambda * E_shadow_c
      // 
      // R_eq_abs_shadow - G_R_shadow * (T_shadow - T_a) 
      // = G_H_shadow_c * (T_shadow - T_c) 
      // + G_W_shadow_c * (s * (T_shadow - T_a) + (e_sat_air - e_c))
      // 
      // <=>
      //
      // R_eq_abs_shadow + (G_R_shadow + G_W_shadow_c * s) * T_a - G_W_shadow_c * e_sat_air
      // = (G_R_shadow + G_H_shadow_c + G_W_shadow_c * s) * T_shadow 
      // - G_H_shadow_c * T_c - G_W_shadow_c * e_c
      
      b (iT_shadow) = R_eq_abs_shadow + (G_R_shadow + G_W_shadow_c * s) * T_a 
                 - G_W_shadow_c * e_sat_air;
      A (iT_shadow, iT_c) = - G_H_shadow_c;
      A (iT_shadow, ie_c) = - G_W_shadow_c;
      A (iT_shadow, iT_shadow) = (G_R_shadow + G_H_shadow_c + G_W_shadow_c * s);

      // Solve and extract.
      solver->solve (A, b, x);
      T_s = x[iT_s];
      T_c = x[iT_c];
      T_sun = x[iT_sun];
      T_shadow = x[iT_shadow];
      e_c = x[ie_c];
    }

  else if (has_LAI && !has_light) // canopy and soil during night time
    {
      // Radiation "conductivity"
      G_R_leaf = 4. * epsilon * sigma * pow(T_a, 3.) * cover; //[W m^-2 K^-1]

      // Equilibrium net-radiation absorbed by the leaves
      R_eq_abs_shadow = R_abs_shadow + R_abs_sun  - BB * cover; //[W m^-2]
   
      // Sensible heat "conductance" between soil and canopy point
      G_H_s_c =  c_p * rho_a * g_H_s_c; //*(1.- cover);       //[W m^-2 K^-1] 
      // Sensible heat "conductance" between leaves and canopy point
      G_H_leaf_c =  c_p * rho_a * g_H_leaf_c;// * cover * (1.-sun_LAI_fraction_total); 
      //[W m^-2 K^-1] 
      
      // Latent heat "conductance" between sunlit leaves and canopy point
      G_W_leaf_c =  c_p * rho_a * g_W_leaf_c / gamma;// * cover *  (1.-sun_LAI_fraction_total);  //[m s^-1] 

      // inter-inter-intermediate variables
      const double a_soil = ((R_eq_abs_soil + G_R_soil * (T_a - T_a)
                              + k_h/z0 * (T_z0 - T_a) - lambda * E_soil) 
                             / (G_R_soil + G_H_s_c + k_h/z0)); //[K]
      
      const double a_soil_c = G_H_s_c / (G_R_soil + G_H_s_c + k_h/z0); // []
      
      const double a_can = ((G_H_a * (T_a - T_a) + G_H_s_c * a_soil)
                            /(G_H_a + G_H_s_c * (1. - a_soil_c) + G_H_leaf_c));//[K]
      
      const double a_can_leaf = G_H_leaf_c / (G_H_a + G_H_s_c * (1. - a_soil_c) 
                                              + G_H_leaf_c); // []
      
      const double b_can = ((G_W_a * (e_a - e_a) + lambda * E_soil 
                             + G_W_leaf_c * ((e_sat_air - e_a) - s * (T_a - T_a)))
                            /(G_W_a + G_W_leaf_c)); //[W s m^-3]
      
      const double b_can_leaf = ((G_W_leaf_c /*[m s^-1]*/ * s /*[W s m^-3 K^-1]*/)
                                 /(G_W_a + G_W_leaf_c)); //[W s m^-3 K^-1]

      const double T_leaf = ((R_eq_abs_shadow + G_R_leaf * (T_a - T_a) 
                              + G_H_leaf_c * a_can 
                              + G_W_leaf_c * (s * (T_a - T_a) - (e_sat_air - e_a) 
                                              + b_can)) /*[W m^-2]*/
                             / (G_R_leaf + G_H_leaf_c * (1. - a_can_leaf)
                                + G_W_leaf_c * (s - b_can_leaf))/*[W m^-2 K^-1]*/
                             + T_a);//[K]

      // -------------------------------------------
      // Temperature of leaves 
      // -------------------------------------------
      T_sun = T_shadow = T_leaf; //[K]
      
      // -------------------------------------------
      // Canopy-point temperature
      // -------------------------------------------
      T_c = a_can + a_can_leaf * (T_leaf - T_a) + T_a; //[K]

      // -------------------------------------------
      // Soil surface temperature
      // -------------------------------------------
      T_s = a_soil + a_soil_c * (T_c - T_a) + T_a;  //[K]

      // -------------------------------------------
      // Canopy vapour pressure 
      // -------------------------------------------
      e_c = b_can + b_can_leaf * (T_leaf - T_a) + e_a; //[Pa]

    }

  else // bare soil
    {
      T_s =((R_eq_abs_soil + G_R_soil * (T_a - T_a) + G_H_a * (T_a - T_a) 
             + k_h / z0 * (T_z0 - T_a) - (lambda * E_soil))
            / (G_R_soil + G_H_a +  k_h / z0))
        + T_a;  //[K]
      T_c = T_s;
    }
} 

void
SVAT_SSOC:: calculate_fluxes()
{
  // Sensible heat flux from the bare soil
  H_soil = G_H_a * (T_s - T_a) ;         //[W m^-2] 
  E_trans = 0.0; //Bare soil. No vegetation and leaf transpiration  [kg m^-2 s^-1]

  if (!has_LAI)
    {
      T_0 = Resistance:: T_0 (T_a - TK, T_s - TK, kb, LAI) + TK; //[K] 
      return;
    }
  
  // Land surface temperature (large scale, from outer space)
  T_0 = Resistance:: T_0 (T_c - TK, T_s - TK, kb, LAI) + TK; //[K] 
  
  if (has_light)
    { 
      // Sensible heat flux from the soil (overwriting)
      H_soil =  G_H_s_c * (T_s - T_c);      //[W m^-2] 
      // Sensible heat flux from the sunlit leaves to the canopy point
      H_sun =  G_H_sun_c * (T_sun - T_c); //[W m^-2] 
      // Sensible heat flux from the shadow leaves to the canopy point
      H_shadow =  G_H_shadow_c * (T_shadow - T_c); //[W m^-2] 
      // Sensible heat flux from the canopy point to free atmosphere
      H_c_a = G_H_a * (T_c - T_a);      //[W m^-2] 
      
      // Latent heat flux from the sunlit leaves to the canopy point
      LE_sun =  G_W_sun_c * ((s * (T_sun - T_a)) + (e_sat_air - e_c));  // [W m^-2]
      // Latent heat flux from the shadow leaves to the canopy point
      LE_shadow = G_W_shadow_c * ((s * (T_shadow - T_a)) + (e_sat_air - e_c));//[W m^-2]
      // Latent heat flux from the canopy point to the free atmosphere
      LE_atm = G_W_a * (e_c - e_a);                   // [W m^-2]
      
      // Transpiration
      E_trans = (LE_sun + LE_shadow) / lambda;        //[kg m^-2 s^-1]==[mm/s]
    }
  else // !has_light
    { 
      // Sensible heat flux from the soil (overwriting)
      H_soil =  G_H_s_c * (T_s - T_c);      //[W m^-2] 
      // Sensible heat flux from the leaves to the canopy point
      H_shadow = G_H_leaf_c * (T_shadow - T_c);   //[W m^-2] 
      H_sun = 0.0;
      // Sensible heat flux from the canopy point to free atmosphere
      H_c_a = G_H_a * (T_c - T_a);      //[W m^-2] 
      
      // Latent heat flux from the sunlit leaves to the canopy point
      LE_shadow =  G_W_leaf_c * ((s * (T_shadow - T_a)) + (e_sat_air - e_c));//[W m^-2]
                 
      LE_sun = 0.0;
      // Latent heat flux from the canopy point to the free atmosphere
      LE_atm = G_W_a * (e_c - e_a);                   // [W m^-2]
      
      // Transpiration
      E_trans = (LE_shadow) / lambda;        //[kg m^-2 s^-1]==[mm/s]
    }
}


void
SVAT_SSOC::solve(const double gs /* stomata cond. [m/s]*/, Treelog& msg )
{
  TREELOG_MODEL (msg);
  std::ostringstream tmp;

  for (int i = 0; i<max_iteration; i++)
    {    
      const double old_T_c = T_c;
      const double old_T_s = T_s;
      const double old_T_sun = T_sun;
      const double old_T_shadow = T_shadow;
      const double old_e_c = e_c; 

      calculate_conductances(gs, msg);
      calculate_temperatures(msg);
      
      if(std::fabs(old_T_c - T_c) < maxTdiff 
         && std::fabs(old_T_s - T_s) < maxTdiff 
         && std::fabs(old_T_sun - T_sun) < maxTdiff 
         && std::fabs(old_T_shadow - T_shadow) < maxTdiff 
         && std::fabs( old_e_c - e_c) < maxEdiff)
        goto success;
    } 
  msg.debug ("Too many iterations.");
  initialized_soil = has_LAI = false; // Prevent log.
  T_c = T_sun = T_shadow = T_s = T_a;
  calculate_conductances(gs, msg);
  converge_fail++;
 success:
  converge_total++;
  calculate_fluxes();
}

void
SVAT_SSOC::output(Log& log) const
{
  if (initialized_soil)
    {
      output_variable (gamma, log);
      output_variable (lambda, log);
      output_variable (rho_a, log);
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
      output_variable (G_W_sun_c, log);
      output_variable (g_H_shadow_c, log);
      output_variable (g_W_shadow_c, log);      
      output_variable (R_abs_sun, log);
      output_variable (R_abs_shadow, log);
      if (has_light)
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
      output_value (E_trans * 3600., "E_trans", log);
      output_variable (e_c, log);
    }
}

SVAT_SSOC::SVAT_SSOC (Block& al)
  : SVAT (al), 
    solver (Librarian::build_item<Solver> (al, "solver")),
    hypostomatous (al.flag ("hypostomatous")),
    maxTdiff (al.number ("maxTdiff")),
    maxEdiff (al.number ("maxEdiff")),
    max_iteration (al.integer ("max_iteration")),
    T_a (-42.42e42),
    z_r (-42.42e42),
    RH (-42.42e42),
    U_z (-42.42e42),
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
    g_H_leaf_c (-42.42e42),
    g_W_leaf_c (-42.42e42),
    e_c (-42.42e42),
    H_soil (-42.42e42),
    H_sun (-42.42e42),
    H_shadow (-42.42e42),
    H_c_a (-42.42e42),
    LE_sun (-42.42e42),
    LE_shadow (-42.42e42),
    LE_atm (-42.42e42),
    E_trans (-42.42e42),
    G_R_soil (-42.42e42),
    G_R_sun (-42.42e42),
    G_R_shadow (-42.42e42),
    G_R_leaf (-42.42e42),
    G_H_a (-42.42e42),
    G_W_a (-42.42e42),
    G_H_s_c (-42.42e42),
    G_H_sun_c (-42.42e42),
    G_W_sun_c (-42.42e42),
    G_H_shadow_c (-42.42e42),
    G_W_shadow_c (-42.42e42),
    G_H_leaf_c (-42.42e42),
    G_W_leaf_c (-42.42e42),
    R_eq_abs_soil (-42.42e42),
    R_eq_abs_sun (-42.42e42),
    R_eq_abs_shadow (-42.42e42),
    initialized_soil (false), 
    initialized_canopy (false),
    converge_fail (0),
    converge_total (0)
{ }

static struct SVAT_SSOCSyntax : public DeclareModel
{
  Model* make (Block& al) const
  { return new SVAT_SSOC (al); }
  SVAT_SSOCSyntax ()
    : DeclareModel (SVAT::component, "SSOC", "Sun-Shade Open Canopy.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.add_object ("solver", Solver::component, 
                       Value::Const, Value::Singleton, "\
Model used for solving the energy balance equation system.");
    frame.add ("solver", "cxsparse");
    frame.add ("hypostomatous", Value::Boolean, Value::Const,
                "True for hypostomatous leaves. \n\
False for amphistomatous leaves (possesing stomata on both surfaces).");
    frame.add ("hypostomatous", true);

    frame.add ("maxTdiff", "K", Value::Const, "\
Largest temperature difference for convergence.");
    frame.add ("maxTdiff", 0.001);
    frame.add ("maxEdiff", "Pa", Value::Const, "\
Largest humidity difference for convergence.");
    frame.add ("maxEdiff", 0.01);
    frame.add ("max_iteration", Value::Integer, Value::Const, "\
Largest number of iterations before giving up on convergence.");
    frame.add ("max_iteration", 150);  

    // For log.
    frame.add ("lambda", "J kg^-1", Value::LogOnly, "Latent heat of vaporization in atmosphere.");
    frame.add ("rho_a", "kg m^-3", Value::LogOnly, "Air density.");
    frame.add ("gamma", "Pa K^-1", Value::LogOnly, "Psychrometric constant.");
    frame.add ("T_s", "K", Value::LogOnly, "Soil surface temperature.");
    frame.add ("T_0", "K", Value::LogOnly, "Surface temperature (large scale).");
    frame.add ("T_c", "K", Value::LogOnly, "Canopy-point temperature.");
    frame.add ("T_sun", "K", Value::LogOnly, "Temperature of sunlit leaves.");
    frame.add ("T_shadow", "K", Value::LogOnly, "Temperature of shadow leaves.");
    frame.add ("g_a", "m s^-1", Value::LogOnly, 
                "Heat conductance in the atmosphere - from canopy point \n\
to reference height (screen height).");
    frame.add ("g_H_s_c", "m s^-1", Value::LogOnly, 
                "Heat conductance from soil surface to canopy point.");
    frame.add ("g_H_sun_c", "m s^-1", Value::LogOnly, 
                "Heat conductance from sunlit leaves to canopy point.");
    frame.add ("g_W_sun_c", "m s^-1", Value::LogOnly, 
                "Water conductance from sunlit leaves to canopy point.");
    frame.add ("G_W_sun_c", "W m^-2 K^-1", Value::LogOnly, 
                "Scaled water conductance from sunlit leaves to canopy point.");
    frame.add ("g_H_shadow_c", "m s^-1", Value::LogOnly,
                "Heat conductance from shadow leaves to canopy point.");
    frame.add ("g_W_shadow_c", "m s^-1", Value::LogOnly,
                "Water conductance from shadow leaves to canopy point.");
    frame.add ("e_a", "Pa", Value::LogOnly, 
                "Vapour pressure of water in the atmosphere.");  
    frame.add ("e_sat_air", "Pa", Value::LogOnly, 
                "Saturated vapour pressure of water in the air.");  
    frame.add ("s", "Pa K^-1", Value::LogOnly, 
                "Slope of water vapour pressure curve.");  
    frame.add ("e_c", "Pa", Value::LogOnly, 
                "Vapour pressure of water in the canopy.");
    frame.add ("R_abs_soil", "W m^-2", Value::LogOnly, "Absorbed radiation in soil.");
    frame.add ("R_eq_abs_soil", "W m^-2", Value::LogOnly, 
                "Absorbed radiation in soil at equilibrium.");
    frame.add ("R_abs_sun", "W m^-2", Value::LogOnly, 
                "Absorbed radiation in sunlit leaves.");
    frame.add ("R_eq_abs_sun", "W m^-2", Value::LogOnly, 
                "Absorbed radiation in sunlit leaves at equilibrium.");
    frame.add ("R_abs_shadow", "W m^-2", Value::LogOnly, 
                "Absorbed radiation in shadow leaves.");
    frame.add ("R_eq_abs_shadow", "W m^-2", Value::LogOnly, 
                "Absorbed radiation in shadow leaves at equilibrium."); 
    frame.add ("LAI", "m^2 m^-2", Value::LogOnly, "Leaf area index.");
    frame.add ("sun_LAI_fraction_total","", Value::LogOnly, 
                "Sunlit fraction of leaf area in the canopy.");
    frame.add ("cover", "", Value::LogOnly, "Vegetation cover.");
    frame.add ("H_soil", "W m^-2", Value::LogOnly, 
                "Sensible heat flux from the soil.");
    frame.add ("H_sun", "W m^-2", Value::LogOnly, 
                "Sensible heat flux from the sunlit leaves to the canopy point.");
    frame.add ("H_shadow", "W m^-2", Value::LogOnly, 
                "Sensible heat flux from the shadow leaves to canopy point.");
    frame.add ("H_c_a", "W m^-2", Value::LogOnly, 
                "Sensible heat flux from the canopy point to free atmosphere.");
    frame.add ("LE_sun", "W m^-2", Value::LogOnly, 
                "Latent heat flux from the sunlit leaves to the canopy point.");
    frame.add ("LE_shadow", "W m^-2", Value::LogOnly, 
                "Latent heat flux from the shadow leaves to the canopy point.");
    frame.add ("LE_atm", "W m^-2", Value::LogOnly, 
                "Latent heat flux from the canopy point to the free atmosphere.");
    frame.add ("E_trans", "mm/h", Value::LogOnly, "Leaf transpiration.");

    //  frame.add ("", "", Value::LogOnly, ".");
    }
} SVAT_ssoc_syntax;

// svat_ssoc.C ends here
