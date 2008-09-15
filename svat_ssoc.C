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
#include "net_radiation.h"
#include "assertion.h"
#include "treelog.h"
#include "mathlib.h"
#include "librarian.h"

struct SVAT_SSOC : public SVAT
{
  // Parameters.
  static const double epsilon; 
  static const double sigma; // Stefan Boltzman constant [W m^-2 K^-4]
  static const double TK;    // Conversion from dg C to K.
  static const double c_p;   // Specific heat of air [J kg^-1 K^-1]

  // Driving variables.
  // - Upper boundary
  // - Lower boundary 
  // - Canopy
  bool hypostomatous; // True for hypostomatous leaves;
  // Intermediate variables.
  double rho_a;          // Air density []

  // "Unknowns."
  // - Temperatures * 4
  double T_s;            // Soil surface temperature [dg C]
  // - Resistances * 4
  double g_a;            // Heat conductance of atmosphere [ ]
  double g_H_s_c;        // Heat conductance from soil surface to canopy point
  double g_H_snl_c;      // Heat conductance from sunlit leaves to canopy point
  double g_H_sha_c;      // Heat conductance from shadow leaves to canopy point
  double g_W_s_c;        // Water conductance from soil surface to canopy point
  double g_W_snl_c;      // Water conductance from sunlit leaves to canopy point
  double g_W_sha_c;      // Water conductance from shadow leaves to canopy point
  double g_s;            // Stomatal conductance
  
  double G_R;            // Radiation "conductivity" [W m^-2 K^-1]
  double G_H_s_a;        // Heat conductivity from soil to screen height [W m^-2 K^-1]

  double R_eq_abs_soil;  // Equilibrium net-radiation absorbed by the soil [W m^-2]
  // - ?
  
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
  void calculate_resistances(const double z_r /* ref height above canopy [m]*/, 
                             const double c_drag /*drag force [m^2 m^-2]*/,
                             const double U_z /* surface wind speed [m s^-1]*/,
                             const double w_l /* leaf width [m]*/, 
                             const double h_veg /* vegetation height [m]*/, 
                             const double LAI /*[m^2 m^-2]*/,
                             const double sun_LAI_fraction_total /*[]*/,
                             const double gs_mol /* stomata cond. [mol/m^2/s]*/,
                             const double T_a /* air temperature [dg C]*/, 
                             const double T_c /* canopy temperature [dg C]*/, 
                             const double T_soil /* soil temperature [dg C]*/, 
                             const double T_l_sun /*leaf sun temperature [dg C]*/,
                             const double kb /* extinction coefficient []*/);
  
  void find_T(const double T_a /*air temp [dg C]*/,
              const double T_s /*surface temp [dg C]*/, 
              const double T_z0/*soil temp in z0 [dg C]*/, 
              const double k_h /*heat conductivity [?]*/,  
              const double z_0 /* depth of top cell [m]*/,
              const double R_abs_total_Long_soil/*[W m^-2]*/, 
              const double R_abs_total_PAR_soil/*[W m^-2]*/,
              const double R_abs_total_NIR_soil/*[W m^-2]*/,
              const double gamma /**/, const double E_soil /**/);
  
  // løs - find r og T (rs_sun, rs_sha)
  // find flux

  double production_stress () const
  { return -1; }

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
SVAT_SSOC::tick (const Weather&, const Vegetation&,
	     const Surface&, const Soil&, const SoilHeat&,
	     const SoilWater&, const Pet&,
	     double /* canopy_ea */, double /* snow_ea */,
	     double /* pond_ea */, double /* soil_ea */,
             double /* crop_ea */, double /* crop_ep */)
{
  // indlæse driving variabler
}

void 
SVAT_SSOC:: calculate_resistances(const double z_r /* ref height above canopy [m]*/, 
                                  const double c_drag /*drag force [m^2 m^-2]*/,
                                  const double U_z /* surface wind speed [m s^-1]*/,
                                  const double w_l /* leaf width [m]*/, 
                                  const double h_veg /* vegetation height [m]*/, 
                                  const double LAI /*[m^2 m^-2]*/,
                                  const double sun_LAI_fraction_total /*[]*/,
                                  const double gs_mol /* stomata cond. [mol/m^2/s]*/,
                                  const double T_a /* air temperature [dg C]*/, 
                                  const double T_c /* canopy temperature [dg C]*/, 
                                  const double T_soil /* soil temperature [dg C]*/, 
                                  const double T_l_sun /*leaf sun temperature [dg C]*/,
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

  // Heat conductance of atmosphere
  const double r_a = Resistance:: r_a(z_r, z_0, z_0h, d, N, U_z); //[s m^-1]
  g_a = 1 / r_a; //[m s^-1]

  // Stomata conductance
  // Omregning af r_mol((m2s)/mol) ti r_s (s/m) foretages ved 
  // r_s = r_mol * P /(R * T):
  const double r_s_mol = 1 / gs_mol; // [s m^2 mol^-1]  
  const double r_s = r_s_mol * Resistance::P_surf /(Resistance::R * T_a);
  g_s = 1 / r_s; // [m s^-1]
  

  // Wind speed at the top of the canopy 
  const double U_c = Resistance:: U_c(z_r, z_0, d, U_z, T_0, T_a, h_veg, r_a, rho_a);
  // Wind speed above the soil surface 
  const double U_s = Resistance:: U_s(l_m, h_veg, LAI, U_c); //[m s ^-1]
  // Heat conductance from soil surface to canopy point
  const double r_a_soil = Resistance::r_a_soil (U_s); // [s m^-1]    
  g_H_s_c = 1/r_a_soil;         // [m s^-1]    


  // Heat conductance from leaf to canopy point:
  const double gbu_heat = Resistance::gbu_heat (U_z, w_l, LAI);
  const double gbf_heat = Resistance::gbf_heat(Cl, T_a, T_l_sun, w_l);
  const double g_H_l_c = Resistance::gb_fraction(gbu_heat, gbf_heat); // [m s¯1]
 
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

  const double gb_H2O = Resistance::gb_fraction(gbu_H2O, gbf_H2O);// [m s¯1]

  //Sunlit fraction
  const double LAI_sun = LAI * sun_LAI_fraction_total;
  const double gbu_sun_heat = Resistance::gbu_sun(gbu_heat, LAI, kb);
  const double gbf_sun_heat = Resistance::gbf_sun(gbf_heat, LAI_sun);
  const double g_H_sun_c = gbu_sun_heat + gbf_sun_heat; 

  const double gbu_sun_H2O = Resistance::gbu_sun (gbu_H2O, LAI, kb);
  const double gbf_sun_H2O = Resistance::gbf_sun(gbf_H2O, LAI_sun);
  const double g_W_sun_c = gbu_sun_H2O + gbf_sun_H2O + g_s * sun_LAI_fraction_total; 

  //Shadow fraction
  const double LAI_shadow = LAI * (1 - sun_LAI_fraction_total);
  const double gbu_shadow_heat = Resistance::gbu_sun(gbu_heat, LAI, kb);
  const double gbf_shadow_heat = Resistance::gbf_sun(gbf_heat, LAI_shadow);
  const double g_H_shadow_c = gbu_shadow_heat + gbf_shadow_heat;

  const double gbu_shadow_H2O = Resistance::gbu_sun (gbu_H2O, LAI, kb);
  const double gbf_shadow_H2O = Resistance::gbf_sun(gbf_H2O, LAI_shadow);
  const double g_W_shadow_c = gbu_shadow_H2O + gbf_shadow_H2O 
    + g_s * (1-sun_LAI_fraction_total);

 
  // Heat cunductance from sunlit leaves to canopy point
  //  const double g_H_snl_c = Resistance:: r_a();  

  // Heat conductance from shadow leaves to canopy point
  // const double g_H_sha_c = Resistance:: r_a();      
  // Water conductance from soil surface to canopy point
  // const double g_W_s_c = Resistance:: r_a();        
  // Water conductance from sunlit leaves to canopy point
  // const double g_W_snl_c = Resistance:: r_a();     
  // Water conductance from shadow leaves to canopy point 
  // g_W_sha_c = Resistance:: r_a();     
}
void 
SVAT_SSOC:: find_T(const double T_a /*air temp [dg C]*/,
                   const double T_s_last /*surface temp [dg C]*/, 
                   const double T_z0 /*soil temp in z0 [dg C]*/, 
                   const double k_h /*heat conductivity [?]*/,  
                   const double z0 /* depth of top cell [m]*/,
                   const double R_abs_total_Long_soil/*[W m^-2]*/, 
                   const double R_abs_total_PAR_soil/*[W m^-2]*/,
                   const double R_abs_total_NIR_soil/*[W m^-2]*/,
                   const double gamma /**/, const double E_soil /**/)
{
  // Air density 
  rho_a = Resistance:: rho_a(T_a); //[kg m^-3]
  
  // Radiation "conductivity"
  G_R = 4. * epsilon * sigma * pow(T_a + TK, 3.); //[W m^-2 K^-1]
  
  // Black-body emmission:
  const double BB = epsilon * sigma * pow(T_a + TK, 4.); //[W m^-2]
  
  // Loss of long-wave radiation
  const double R_emi_soil = BB + G_R * (T_s_last - T_a); //[W m^-2]
  
  // Equilibrium net-radiation absorbed by the soil
  R_eq_abs_soil = R_abs_total_Long_soil +  R_abs_total_PAR_soil
    + R_abs_total_NIR_soil - R_emi_soil;     //[W m^-2]
  
  // Intermediates variable
  G_H_s_a = c_p * rho_a * g_a; //[W m^-2 K^-1] ???????
  

  T_s =((R_eq_abs_soil + G_R * (T_a + TK) + G_H_s_a * (T_a + TK) 
         + k_h / z0 * (T_z0 + TK) - (gamma * E_soil))
        / (G_R + G_H_s_a +  k_h / z0)) - TK;  
} 

// find T
// løs - find r og T (rs_sun, rs_sha)
// find flux


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
