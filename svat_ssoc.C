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
#include "net_radiation.h"
#include "weather.h"
#include "vegetation.h"
#include "assertion.h"
#include "log.h"
#include "treelog.h"
#include "mathlib.h"
#include "librarian.h"

struct SVAT_SSOC : public SVAT
{
  // Parameters.
  static const double epsilon; // Emmision of Longwave rad == SurEmiss [] 
  static const double sigma;   // Stefan Boltzman constant [W m^-2 K^-4]
  static const double TK;      // Conversion from dg C to K.
  static const double c_p;     // Specific heat of air [J kg^-1 K^-1]

  // Driving variables.
  // - Upper boundary
  double z_r;            // Referebce height above canopy == screen height [m]
  double RH;             // Relative humidity []
  double U_z;            // Surface wind speed [m s^-1]

  // - Lower boundary 
  // - Canopy
  double h_veg;          // Vegetation height [m] 
  double w_l;            // leaf width [m] 
  bool hypostomatous;    // True for hypostomatous leaves;
  // Intermediate variables.
  double rho_a;          // Air density [kg m^-3]
  double gamma;          // Psychrometric constant [Pa/K]
  double e_sat;          // Saturatet water vapour pressure [Pa]

  // - Temperatures * 4
  double T_a;          // Air surface temperature [dg C]
  double T_s;          // Soil surface temperature [dg C]
  double T_c;          // Canopy-point temperature [dg C]
  double T_sun;        // Temperature of sunlit leaves [dg C]
  double T_shadow;     // Temperature of shadow leaves [dg C]
  // - Conductivities * 4
  double g_a;          // Heat conductance of atmosphere [m s^-1]
  double g_s;          // Stomatal conductance [m s^-1]
  double g_H_s_c;      // Heat conductance from soil surface to canopy point [m s^-1]
  double g_H_sun_c;    // Heat conductance from sunlit leaves to canopy point [m s^-1]
  double g_W_sun_c;    // Water conductance from sunlit leaves to canopy point [m s^-1]
  double g_H_shadow_c; // Heat conductance from shadow leaves to canopy point [m s^-1]
  double g_W_shadow_c; // Water conductance from shadow leaves to canopy point [m s^-1]

  // - Inter-intermediates variables
  double G_R;       // Radiation "conductivity" [W m^-2 K^-1]
  double G_H_s_a;   // Heat "conductivity" from soil to atmosphere (screen height)
                    // [W m^-2 K^-1]
  double G_H_a;     // Heat "conductivity" from soil to free atmosphere [W m^-2 K^-1]
  double G_W_a;     // Water "conductivity" from soil to free atmosphere [m s^-1]
  double G_H_s_c;   // Heat "conductance" from soil to canopy point [W m^-2 K^-1]
  double G_H_sun_c; // Heat "conductance" from sun leaves to canopy point [W m^-2 K^-1
  double G_W_sun_c; // Water "conductance" from sun leaves to canopy point [m s^-1]
  double G_H_shadow_c; // Heat "conductance" from shadow leaves to canopy point 
                       // [W m^-2 K^-19
  double G_W_shadow_c; // Water "conductance" from shadow leaves to canopy point 
                       //[m s^-1]
  double R_eq_abs_soil;   // Equilibrium net-radiation absorbed by the soil [W m^-2]
  double R_eq_abs_sun;    // Equilibrium net-radiation absorbed by the sunlit leaves
                          // [W m^-2]
  double R_eq_abs_shadow; // Equilibrium net-radiation absorbed by the shadow leaves
                          // [W m^-2]
  // Vapour pressure 
  double e_a;             // Vapour pressure of water in the atmosphere [Pa]
  
  // Fluxes.
  // - Heat
  // - H2O
  // - CO2
  std::auto_ptr<NetRadiation> net_radiation;

  // Simulation.
  void tick (const Weather&, const Vegetation&,
	     const Surface&, const Soil&, const SoilHeat&,
	     const SoilWater&, const Pet&,
	     double /* canopy_ea */, double /* snow_ea */,
	     double /* pond_ea */, double /* soil_ea */,
             double /* crop_ea */, double /* crop_ep */); 

  void calculate_conductances(const double c_drag /*drag force [m^2 m^-2]*/,
                              const double LAI /*[m^2 m^-2]*/,
                              const double sun_LAI_fraction_total /*[]*/,
                              const double gs_mol /* stomata cond. [mol/m^2/s]*/,
                              const double T_c /* canopy temperature [dg C]*/, 
                              const double T_soil /* soil temperature [dg C]*/, 
                              const double T_sun_last /*leaf sun temperature [dg C]*/,
                              const double kb /* extinction coefficient []*/);
  
  void find_T(const double T_a /*air temp [dg C]*/,
              const double T_s /*surface temp [dg C]*/, 
              const double T_z0/*soil temp in z0 [dg C]*/, 
              const double T_c_last /* canopy temp [dg C]*/, 
              const double T_sun_last /* sun leaves temp [dg C]*/, 
              const double T_shadow_last /* shadoe leaves temp [dg C]*/, 
              const double k_h /*heat conductivity [?]*/,  
              const double z_0 /* depth of top cell [m]*/,
              const double R_abs_soil /*[W m^-2]*/, 
              const double R_abs_sun /*[W m^-2]*/,
              const double R_abs_shadow /*[W m^-2]*/
              , const double E_soil /**/, const double lambda /*??*/, 
              const double LAI, const double s /*??*/);
  
  // løs - find r og T (rs_sun, rs_sha)
  // find flux

  double production_stress () const
  { return -1; }

  void clear ();
  void output(Log& log) const;
  static void load_syntax (Syntax& syntax, AttributeList& alist);

  // Create.
public:
  SVAT_SSOC (Block& al)
    : SVAT (al), 
      hypostomatous (al.flag ("hypostomatous)")),
      //  c_p (al.number ("c_p")),
      net_radiation (Librarian::build_item<NetRadiation> (al, "net_radiation"))
  { }
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
	     const Surface&, const Soil&, const SoilHeat&,
	     const SoilWater&, const Pet&,
	     double wind_speed_field /*[m s^-1]*/, double /* snow_ea */,
	     double /* pond_ea */, double /* soil_ea */,
             double /* crop_ea */, double /* crop_ep */)
{
  RH = weather.relative_humidity (); // Relative humidity []
  T_a = weather.hourly_air_temperature (); // air temp [dg C]
  z_r = weather.screen_height (); //reference height
  U_z = wind_speed_field;   // wind speed at reference height [m s^-1]
  h_veg = vegetation.height () / 100.; // vegetation height [m]  
  //w_l = vegetation.leaf_width () / 100.; // leaf width [m] 
  w_l = 0.03;

}

void 
SVAT_SSOC:: calculate_conductances(const double c_drag /*drag force [m^2 m^-2]*/,
                                   const double LAI /*[m^2 m^-2]*/,
                                   const double sun_LAI_fraction_total /*[]*/,
                                   const double gs_mol /* stomata cond. [mol/m^2/s]*/,
                                   const double T_c_last /* canopy temperature [dg C]*/,                                   const double T_soil /* soil temperature [dg C]*/, 
                                   const double T_sun_last /*leaf sun temp [dg C]*/,
                                   const double kb /* extinction coefficient []*/) 
{
  // Function to correct diffusivities for temperature and pressure
  const double Cl = Resistance::Cl(T_a);
  // Mean leaf size
  const double l_m = Resistance::l_m (w_l);
  // Land surface temperature (large scale, from outer space)
  const double T_0 = Resistance:: T_0 (T_c, T_soil, kb, LAI); //[dg C] 
  // Zero-plane displacement height [m] (F3)
  const double d = Resistance::d (h_veg, c_drag, LAI);
  // Atmospheric stability indicator (F4)
  const double N = Resistance::N (z_r, d, T_0, T_a, U_z);
  // Roughness lenght for momentum transport (F2)
  const double z_0 = Resistance::z_0 (h_veg, c_drag, d, LAI);
  // Roughness lenght for sensible heat transfer (F1)
  const double z_0h = Resistance::z_0h (z_0);

  // Aerodynamic heat resistance between canopy and reference height
  const double r_a = Resistance:: r_a(z_r, z_0, z_0h, d, N, U_z); //[s m^-1]
  // Wind speed at the top of the canopy 
  const double U_c = Resistance:: U_c(z_r, z_0, d, U_z, T_0, T_a, h_veg, r_a, rho_a);
  // Wind speed above the soil surface 
  const double U_s = Resistance:: U_s(l_m, h_veg, LAI, U_c); //[m s ^-1]

  // -------------------------------------------------------                   
  // CANOPY POINT TO ATMOSPHERE
  // -------------------------------------------------------
  // Heat conductance in the atmosphere (between canopy and reference height)
  g_a = 1 / r_a; //[m s^-1]

  // -------------------------------------------------------                   
  // SOIL TO CANOPY POINT
  // -------------------------------------------------------
  // Heat conductance from soil surface to canopy point
  const double r_a_soil = Resistance::r_a_soil (U_s); // [s m^-1]    
  g_H_s_c = 1/r_a_soil;         // [m s^-1]    

  // -------------------------------------------------------                   
  // LEAF TO CANOPY POINT
  // -------------------------------------------------------
  // Heat conductance from leaf to canopy point:
  const double gbu_heat = Resistance::gbu_heat (U_z, w_l, LAI);
  const double gbf_heat = Resistance::gbf_heat(Cl, T_a, T_sun_last, w_l);
 
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

  // Stomata conductance
  // Omregning af gs_mol(mol/(m2s)) til g_s (m/s) foretages ved 
  // g_s = gs_mol * (R * T)/P:
  g_s = gs_mol * (Resistance::R * T_a) / Resistance::P_surf;//[mol s^-1 m^-2]->[m s^-1]

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
  g_W_sun_c = gbu_sun_H2O + gbf_sun_H2O + g_s * sun_LAI_fraction_total; 

  //Shadow fraction --------------------------------------------------
  const double LAI_shadow = LAI * (1 - sun_LAI_fraction_total);
  const double gbu_shadow_heat = Resistance::gbu_sun(gbu_heat, LAI, kb);
  const double gbf_shadow_heat = Resistance::gbf_sun(gbf_heat, LAI_shadow);
  // Heat conductance from shadow leaves to canopy point
  g_H_shadow_c = gbu_shadow_heat + gbf_shadow_heat;

  const double gbu_shadow_H2O = Resistance::gbu_sun (gbu_H2O, LAI, kb);
  const double gbf_shadow_H2O = Resistance::gbf_sun(gbf_H2O, LAI_shadow);
  // Water conductance from shadow leaves to canopy point 
  // - sum of boundary and stomata
  g_W_shadow_c = gbu_shadow_H2O + gbf_shadow_H2O + g_s * (1-sun_LAI_fraction_total);
}

void 
SVAT_SSOC:: find_T(const double T_a /* air temp [dg C]*/,
                   const double T_s_last /* surface temp [dg C]*/, 
                   const double T_z0_last /* soil temp in z0 [dg C]*/, 
                   const double T_c_last /* canopy temp [dg C]*/, 
                   const double T_sun_last /* sun leaves temp [dg C]*/, 
                   const double T_shadow_last /* shadoe leaves temp [dg C]*/, 
                   const double k_h /* heat conductivity [?]*/,  
                   const double z0 /* depth of top cell [m]*/,
                   const double R_abs_soil /*[W m^-2]*/, 
                   const double R_abs_sun /*[W m^-2]*/,
                   const double R_abs_shadow /*[W m^-2]*/,
                   const double E_soil /**/, const double lambda /*??*/, 
                   const double LAI, const double s /*??*/)
{
  // Intermediates variable ------------------------------

  // Air density 
  rho_a = Resistance:: rho_a(T_a); //[kg m^-3]
  // Psychrometric constant 
  gamma = FAO::PsychrometricConstant (Resistance::P_surf, T_a); // [Pa/K]
  // Saturated water vapour pressure in atmosphere
  e_sat = FAO::SaturationVapourPressure (T_a);   // [Pa]

  // Radiation "conductivity"
  G_R = 4. * epsilon * sigma * pow(T_a + TK, 3.); //[W m^-2 K^-1]
  // Sensible heat "conductance" from soil to atmosphere
  G_H_s_a = c_p * rho_a * g_a;            //[W m^-2 K^-1] 
  // Sensible heat "conductance" between soil and canopy point
  G_H_s_c =  c_p * rho_a * g_H_s_c;       //[W m^-2 K^-1] 
  // Sensible heat "conductance" between sunlit leaves and canopy point
  G_H_sun_c =  c_p * rho_a * g_H_sun_c;   //[W m^-2 K^-1] 
  // Sensible heat "conductance" from canopy point to atmosphere
  G_H_a = G_H_s_a;                        //[W m^-2 K^-1] 
  // Latent heat ????? "conductance" between sunlit leaves and canopy point
  G_W_sun_c =  c_p * rho_a * g_W_sun_c / gamma;  //[m s^-1] 
  G_W_shadow_c =  c_p * rho_a * g_W_shadow_c / gamma;  //[m s^-1] 
  G_W_a =  c_p * rho_a * g_a / gamma;  //[m s^-1] 

  // Black-body emmission:
  const double BB = epsilon * sigma * pow(T_a + TK, 4.); //[W m^-2]
  // Loss of long-wave radiation
  const double R_emi_soil = BB + G_R * (T_s_last - T_a); //[W m^-2]
  const double R_emi_sun = BB + G_R * (T_sun_last - T_a); //[W m^-2]
  const double R_emi_shadow = BB + G_R * (T_shadow_last - T_a); //[W m^-2]

  // Equilibrium net-radiation absorbed by the soil
  R_eq_abs_soil = R_abs_soil - R_emi_soil;           //[W m^-2]
  // Equilibrium net-radiation absorbed by the sunlit leaves
  R_eq_abs_sun = R_abs_sun - R_emi_sun;              //[W m^-2]
  // Equilibrium net-radiation absorbed by the shadow leaves
  R_eq_abs_shadow = R_abs_shadow - R_emi_shadow;     //[W m^-2]


  if (LAI > 0) // canopy and soil 
    {
      // inter-inter-intermediate variables
      const double a_soil = ((R_eq_abs_soil + G_R * (T_a + TK)
                              + k_h/z0 * (T_z0_last + TK) - lambda * E_soil) 
                             / (G_R + G_H_s_c + k_h/z0));
      
      const double a_soil_c = G_H_s_c / (G_R + G_H_s_c + k_h/z0);
      
      const double a_can = ((G_H_a * (T_a + TK) + G_H_s_c * a_soil 
                             + G_H_sun_c * (T_sun + TK))
                            /(G_H_a + G_H_s_c * (1 - a_soil_c)
                              + G_H_sun_c + G_H_shadow_c));
      
      const double a_can_sun = G_H_sun_c / (G_H_a + G_H_s_c * (1-a_soil_c) 
                                            + G_H_sun_c + G_H_shadow_c);
      
      const double a_can_shadow = G_H_shadow_c / (G_H_a + G_H_s_c * (1-a_soil_c) 
                                                  + G_H_sun_c + G_H_shadow_c);
      
      const double b_can = ((G_W_a * e_a + lambda * E_soil 
                             + G_W_sun_c * (e_sat * (T_a + TK) - s * (T_a + TK))
                             + G_W_shadow_c * (e_sat * (T_a + TK) - s * (T_a + TK)))
                            /(G_W_a + G_W_sun_c + G_W_shadow_c));
      
      const double b_can_sun = (G_W_sun_c * s)/(G_W_a + G_W_sun_c + G_W_shadow_c);

      const double b_can_shadow = ((G_W_shadow_c * s)
                                   /(G_W_a + G_W_sun_c + G_W_shadow_c));
      
      const double c_sun = ((R_eq_abs_sun + G_R *  (T_a + TK) + G_H_sun_c * a_can 
                             + G_W_sun_c * (s * (T_a + TK) 
                                            - e_sat * (T_a + TK) + b_can))
                            / (G_R + G_H_sun_c * (1 - a_can_sun)
                               + G_W_sun_c * (s - b_can_sun)));
      
      const double c_sun_shadow = ((G_H_sun_c * a_can_shadow 
                                    + G_W_sun_c * b_can_shadow)
                                   /(G_R + G_H_sun_c * (1 - a_can_sun)
                                     + G_W_sun_c * (s - b_can_sun)));
      
      const double c_shadow = ((R_eq_abs_shadow + G_R *  (T_a + TK) 
                                + G_H_shadow_c * a_can 
                                + G_W_shadow_c * (s * (T_a + TK)
                                                  - e_sat * (T_a + TK) + b_can)) 
                               / (G_R + G_H_shadow_c * (1 - a_can_shadow)
                                  + G_W_shadow_c * (s - b_can_shadow)));
      
      const double c_shadow_sun = ((G_H_sun_c * a_can_shadow 
                                    + G_W_sun_c * b_can_shadow)
                                   /(G_R + G_H_shadow_c * (1 - a_can_shadow)
                                     + G_W_shadow_c * (s - b_can_shadow)));
      
      // -------------------------------------------
      // Temperature of sunlit leaves 
      // -------------------------------------------
      T_sun = (c_sun + c_sun_shadow * c_shadow) / (1-c_sun_shadow * c_shadow_sun) - TK;
      
      // -------------------------------------------
      // Temperature of shadow leaves 
      // -------------------------------------------
      T_shadow = (c_shadow + c_shadow_sun * (T_sun + TK)) - TK; 
      
      // -------------------------------------------
      // Canopy-point temperature
      // -------------------------------------------
      T_c = (a_can + a_can_sun * (T_sun + TK) + a_can_shadow * (T_shadow + TK)) - TK;

      // -------------------------------------------
      // Soil surface temperature
      // -------------------------------------------
      T_s =(a_soil + a_soil_c * (T_c + TK)) - TK;  
    }

  else // bare soil
    T_s =((R_eq_abs_soil + G_R * (T_a + TK) + G_H_s_a * (T_a + TK) 
           + k_h / z0 * (T_z0_last + TK) - (lambda * E_soil))
          / (G_R + G_H_s_a +  k_h / z0)) - TK;  
} 


// løs - find r og T (rs_sun, rs_sha)
// find flux

void
SVAT_SSOC::clear ()
{
  T_s = 0.0;
  g_a =  0.0;         
  g_s = 0.0;
  g_H_s_c = 0.0;
  g_H_sun_c = 0.0;
  g_W_sun_c = 0.0;
  g_H_shadow_c = 0.0;
  g_W_shadow_c = 0.0;
  R_eq_abs_soil = 0.0;
  R_eq_abs_sun = 0.0;
  R_eq_abs_shadow = 0.0;
  e_a = 0.0;
}

void
SVAT_SSOC::output(Log& log) const
{
  output_variable (T_s, log);
  output_variable (g_a, log);  
  output_variable (g_s, log); 
  output_variable (g_H_s_c, log); 
  output_variable (g_H_sun_c, log);  
  output_variable (g_W_sun_c, log);
  output_variable (g_H_shadow_c, log);
  output_variable (g_W_shadow_c, log);
  output_variable (R_eq_abs_soil, log);
  output_variable (R_eq_abs_sun, log);
  output_variable (R_eq_abs_shadow, log);
  output_variable (e_a, log);
}

void 
SVAT_SSOC::load_syntax (Syntax& syntax, AttributeList& alist)
{
  syntax.add ("hypostomatous", Syntax::Boolean, Syntax::Const,
              "True for hypostomatous leaves. \n\
False for amphistomatous leaves (possesing stomata on both surfaces).");
  alist.add ("hypostomatous", true);
  
  syntax.add_object ("net_radiation", NetRadiation::component,
                     "Net radiation.");
  alist.add ("net_radiation", NetRadiation::default_model ());

  // For log:
  syntax.add ("T_s", "dg C", Syntax::LogOnly, "Soil surface temperature.");
  syntax.add ("g_a", "m s^-1", Syntax::LogOnly, 
              "Heat conductance in the atmosphere - from canopy point \n\
to reference height (screen height).");
  syntax.add ("g_s", "m s^-1", Syntax::LogOnly, 
              "Stomatal conductance of water.");
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
  syntax.add ("R_eq_abs_soil", "W m^-2", Syntax::LogOnly, 
              "Equilibrium net-radiation absorbed by the soil.");
  syntax.add ("R_eq_abs_sun", "W m^-2", Syntax::LogOnly,
              "Equilibrium net-radiation absorbed by the sunlit leaves.");
  syntax.add ("R_eq_abs_shadow", "W m^-2", Syntax::LogOnly,
              "Equilibrium net-radiation absorbed by the shadow leaves.");
  syntax.add ("e_a", "Pa", Syntax::LogOnly, 
              "Vapour pressure of water in the atmosphere.");
  
  //  syntax.add ("", "", Syntax::LogOnly, ".");

}

static struct SVAT_SSOCSyntax
{
  static Model& make (Block& al)
  { return *new SVAT_SSOC (al); }
  SVAT_SSOCSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    SVAT::load_syntax (syntax, alist);
    Librarian::add_type (SVAT::component, "SSOC", alist, syntax, &make);
  }
} SVAT_ssoc_syntax;
