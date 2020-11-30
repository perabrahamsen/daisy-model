// litter_mulch.C -- Litter layer below vegetation.
// 
// Copyright 2020 KU
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

#include "litter_residue.h"
#include "rate.h"
#include "block_model.h"
#include "mathlib.h"
#include "librarian.h"
#include "check.h"
#include "log.h"
#include "organic.h"
#include "chemistry.h"
#include "mathlib.h"
#include "chemical.h"
#include "am.h"
#include "bioclimate.h"
#include "abiotic.h"
#include "aom.h"
#include "geometry.h"
#include "soil_water.h"
#include "iterative.h"
#include <sstream>

struct LitterMulch : public LitterResidue 
{
  // Parameters.
  const symbol DOC_name;	// Name of chemical representing DOC.
  const symbol DON_name;	// Name of chemical representing DON.
  const double density;		 // Density of mulch [kg DM/m^3]
  const double decompose_height; // Max height of active mulch layer [cm]
  const double soil_height;	 // Heigh of soil layer providing N [cm]
  const double Theta_res;	 // Residual water content []
  const double Theta_sat;	 // Saturated water content []
  const double h_min;		 // Min. pressure for biological activity [cm]
  const double factor_exch;	 // Water connectivity with soil []
  const double alpha;		 // Interception parameter []
  const double Dp;		 // Diffusion rate [h^-1]
  const double Si;		 // Saturation index []
  
  // Log variables.
  double height;		// Height of mulch layer [cm]
  double contact;		// Fraction in contact with soil []
  double water;			// Total water in mulch [mm]
  double Theta;			// Relative water content []
  double h;			// Mulch water potential at start of tstep [cm]
  double h1;			// estimated mulch water potential at end [cm]
  double h_soil;		// Soil water potential at soil_height [cm]
  double K_soil;		// Soil water conductivity at soil_height [cm/h]
  double E_darcy;		// Potential exchnage with soil water [cm/h]
  double h_factor;		// Water potential effect []
  double T;			// Temeprature of mulch.
  double T_factor;		// Temperature factor []
  double DOC_gen;		// Disolved organic C generation [g C/cm^2/h]
  double DON_gen;		// Disolved organic N generation [g N/cm^2/h]
  double SOC_gen;		// Stationary C generation [g C/cm^2/h]
  double SON_gen;		// Stationary N generation [g N/cm^2/h]


  double find_E_darcy (const double h) const // [cm/h]
  { return cover () * factor_exch * ((h - h_soil) / -soil_height) * K_soil; }

  double find_h (const double Theta) const // []->[cm]
  {
    if (Theta < Theta_res)
      return h_min;
    if (Theta > Theta_sat)
      return 0.0;

    return -std::pow (-h_min,
		      1.0-((Theta - Theta_res)
			   / (2.0 * Theta_sat / 3.0 - Theta_res)));
  }

  double find_h1 (const double h0, const double dt) const
  {
    // No litter.
    if (!std::isnormal (height))
      return h_min;

    const double E_darcy = find_E_darcy (h0); // [cm/h]
    const double new_water = water - E_darcy * dt * 10.0 /* [mm/cm] */;
    const double new_Theta
      = Theta_sat * new_water / water_capacity (); // []
    return find_h (new_Theta);
  }

  // Simulation.
  void tick (const Bioclimate& bioclimate,
	     const Geometry& geo, const Soil& soil,
	     const SoilWater& soil_water, const SoilHeat& soil_heat,
	     OrganicMatter& organic, Chemistry& chemistry,
	     const double dt,
	     Treelog& msg)
  {
    // Find mass and cover.
    LitterResidue::tick (bioclimate,
			 geo, soil, soil_water, soil_heat, organic, chemistry,
			 dt, msg);

    // Find height of mulch layer.
    static const double cm_per_m = 100.0;
    if (cover () > 0.0)
      height = cm_per_m * mass / density;
    else
      height = 0.0;

    // Find mass of active part of mulch layer.
    if (decompose_height >= height)
      contact = 1.0;
    else
      contact = decompose_height / height;

    // Water
    water = bioclimate.get_litter_water (); // [mm]
    if (height > 0.0)
      Theta = Theta_sat * water / water_capacity (); // [mm/mm]
    else
      Theta = 0.0;

    h = find_h (Theta);
    h_factor = Abiotic::f_h (h);

    h_soil = geo.content_height (soil_water, &SoilWater::h, soil_height);
    K_soil = geo.content_height (soil_water, &SoilWater::K_cell, soil_height);

    struct h_diff_c
    {
      const LitterMulch& m;
      const double dt;
      double operator()(const double h0) const
      { return h0 - m.find_h1 (h0, dt); }
      h_diff_c (const LitterMulch& m_, const double dt_)
	: m (m_),
	  dt (dt_)
      { }
    } h_diff (*this, dt);
    h1 = bisection (h_min, 0, h_diff);
    E_darcy = find_E_darcy (h1);
    
    // Temperature
    T = bioclimate.get_litter_temperature (); // [dg C]
    T_factor = Abiotic::f_T0 (T);

    // Combined
    const double factor = T_factor * h_factor * contact;

    // Turnover
    std::vector <AM*> am = organic.get_am ();
    std::vector<AOM*> added;

    for (auto pool: am)
      pool->append_to (added);

    sort (added.begin (), added.end (), AOM::compare_CN);
    
    DOC_gen = 0.0;		// [g C/cm^2/h]
    DON_gen = 0.0;		// [g N/cm^2/h]
    SOC_gen = 0.0;		// [g C/cm^2/h]
    SON_gen = 0.0;		// [g N/cm^2/h]

    if (!std::isnormal (dt))	// Initialization.
      return;
    
    for (auto pool: added)
      {
	const double rate = factor * pool->turnover_rate; // [h^-1]
	const double stationary = pool->SOM_fraction ();  // []
	const double dissolved = 1.0 - stationary;	  // []
	const double top_C = pool->top_C; // [g C/cm^2]

	double new_C = NAN; // [g C/cm^2]
	double loss_C = NAN; // [g C/cm^2/h]
	first_order_change (top_C, 0, rate, dt, new_C, loss_C);
	daisy_assert (loss_C >= 0.0);
	daisy_assert (new_C >= 0.0);
	DOC_gen += loss_C * dissolved;
	SOC_gen += loss_C * stationary;
	pool->top_C = new_C;
	
	const double top_N = pool->top_N; // [g N/cm^2]
	double new_N = NAN; // [g N/cm^2]
	double loss_N = NAN; // [g N/cm^2/h]
	first_order_change (top_N, 0, rate, dt, new_N, loss_N);
	daisy_assert (loss_N >= 0.0);
	daisy_assert (new_N >= 0.0);
	DON_gen += loss_N * dissolved;
	SON_gen += loss_N * stationary;
	pool->top_N = new_N;
      }
    double buffer_C = SOC_gen * dt;
    double buffer_N = SON_gen * dt;

    if (chemistry.know (DOC_name))
      {
	Chemical& DOC = chemistry.find (DOC_name);
	DOC.add_to_litter_transform_source (DOC_gen);
      }
    else
      buffer_C += DOC_gen * dt;

    if (chemistry.know (Chemical::DON ()))
      {
	Chemical& DON = chemistry.find (Chemical::DON ());
	DON.add_to_litter_transform_source (DON_gen);
      }
    else
      buffer_N += DON_gen * dt;

    daisy_assert (buffer_C >= 0.0);
    daisy_assert (buffer_N >= 0.0);
    daisy_assert (dt > 0.0);
    organic.add_to_buffer (geo, 0, soil_height, buffer_C, buffer_N);
  }

  double intercept () const	// [0-1]
  // Fraction of rain hitting the litter layer that actually enter the
  // litter layer.
  {
    if (Theta >= Theta_sat)
      return 0.0;

    return std::exp (-alpha * (Theta_sat - Theta_res) / (Theta_sat - Theta));
  }

  double diffusion_rate () const // [h^-1]
  { return (Theta < Si) ? 0.0 : Dp; }
  
  double potential_exfiltration () const // Water exchange with soil [mm/h]
  { return E_darcy * 10.0 /* [mm/cm] */; }
  
  void output (Log& log) const
  {
    LitterResidue::output (log);
    output_variable (height, log);
    output_variable (contact, log);
    output_variable (water, log);
    output_variable (Theta, log);
    output_variable (h, log);
    output_variable (h1, log);
    output_variable (h_soil, log);
    output_variable (K_soil, log);
    output_variable (E_darcy, log);
    output_variable (h_factor, log);
    output_variable (T, log);
    output_variable (T_factor, log);
    output_variable (DOC_gen, log);
    output_variable (DON_gen, log);
    output_variable (SOC_gen, log);
    output_variable (SON_gen, log);
  }

  static double find_Theta_sat (const BlockModel& al)
  {
    const double density = al.number ("density"); // [kg/m^3]
    const double water_capacity = al.number ("water_capacity"); // [L/kg]
    const double L_per_m3 = 1000.0; // [L/m^3]
    // [m^3/m^3] = [kg/m^3] * [L/kg] / [L/m^3]
    const double Theta_sat = density * water_capacity / L_per_m3; 

    Treelog& msg = al.msg ();
    Treelog::Open nest (msg, "mulch layer");
    std::ostringstream tmp;
    tmp << "Theta_sat = " << Theta_sat << " [])";
    msg.debug (tmp.str ());

    return Theta_sat;
  }
  
  // Create and Destroy.
  LitterMulch (const BlockModel& al)
    : LitterResidue (al),
      DOC_name (al.name ("DOC_name")),
      DON_name (al.name ("DON_name")),
      density (al.number ("density")),
      decompose_height (al.number ("decompose_height")),
      soil_height (al.number ("soil_height")),
      Theta_res (al.number ("Theta_res")),
      Theta_sat (find_Theta_sat (al)),
      h_min (al.number ("h_min")),
      factor_exch (al.number ("factor_exch")),
      alpha (al.number ("alpha")),
      Dp (Rate::value (al, "Dp")),
      Si (Theta_sat * al.number ("Si")),
      height (NAN),
      contact (NAN),
      water (NAN),
      Theta (NAN),
      h (NAN),
      h1 (NAN),
      h_soil (NAN),
      K_soil (NAN),
      E_darcy (NAN),
      h_factor (NAN),
      T (NAN),
      T_factor (NAN),
      DOC_gen (NAN),
      DON_gen (NAN),
      SOC_gen (NAN),
      SON_gen (NAN)
  {
    if (Theta_res > Theta_sat)
      al.msg ().error ("Theta_res > Theta_sat");
    if (Theta_res > Si)
      al.msg ().error ("Theta_res > Si");
    if (Si > Theta_sat)
      al.msg ().error ("Si > Theta_sat");
  }
  ~LitterMulch ()
  { }
};

static struct LitterMulchSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new LitterMulch (al); }
  LitterMulchSyntax ()
    : DeclareModel (Litter::component, "mulch", "residue", "\
A decomposing mulch layer.\n\
\n\
The bottom of the mulch layer may decompose based on the conditions\n\
in the top of soil.")
  { }
  void load_frame (Frame& frame) const
  { 
    frame.set_strings ("cite", "findeling2007modelling");
    frame.declare_string ("DOC_name", Attribute::Const, "\
Name of compound representing dissolved organic carbon.");
    frame.set ("DOC_name", Chemical::DOC ());
    frame.declare_string ("DON_name", Attribute::Const, "\
Name of compound representing dissolved organic nitrogen.");
    frame.set ("DON_name", Chemical::DON ());
    frame.declare ("density", "kg DM/m^3", Check::positive (),
		   Attribute::Const, "\
Density of mulch layer.");
    frame.declare ("decompose_height", "cm", Check::positive (),
		   Attribute::Const, "\
Height of muclh layer considered in contact with the soil.\n\
Only this part of the mulch layer will decompose");
    frame.declare ("soil_height", "cm", Check::negative (),
		   Attribute::Const, "\
Height of soil layer (a negative number) contributing to decay.");
    frame.declare ("Theta_res", Attribute::None (), Check::non_negative (),
		   Attribute::Const, "\
Water content where biological activity stops");
    frame.declare ("h_min", "cm", Check::negative (),
		   Attribute::Const, "\
Water pressure where biological activity stops.");
    frame.set ("h_min", pF2h (6.5));
    frame.declare ("factor_exch", Attribute::None (), Check::non_negative (),
		   Attribute::Const, "\
Limiting factor when calculating Darcy exchange between mulch and soil.\n\
It is intended to emulate poor contact between the two media.");
    frame.declare ("alpha", Attribute::None (), Check::non_negative (),
		   Attribute::Const, "Interception parameter.\n\
The fraction of water hitting the litter will be determined by:\n\
\n\
  exp (-alpha (Theta_sat - Theta_res) / (Theta_sat - Theta))\n\
\n\
If alpha is 0 (default), all water hitting the canopy will be intercepted.");
    frame.set ("alpha", 0.0);

    Rate::declare (frame, "Dp", "Diffusion rate to wash off water.\n\
The wash off water is water that hit the mulch cover, but is not intercepted.");
    frame.declare_fraction ("Si", Attribute::Const, "\
Water content where diffusion to wash off begins relative to Theta_sat.\n\
Theta_sat = 100 %");
    
    // Log variables.
    frame.declare ("height", "cm", Attribute::LogOnly, "\
Total height of mulch layer.");
    frame.declare_fraction ("contact", Attribute::LogOnly, "\
Fraction of mulch layer in contact with soil.\n\
DOM in this part of the mulch is degraded.");
    frame.declare ("water", "mm", Attribute::LogOnly, "\
Total water in mulch.");
    frame.declare ("Theta", Attribute::None (), Attribute::LogOnly, "\
Relative water content.");
    frame.declare ("h", "cm", Attribute::LogOnly, "\
Mulch water potential at stat of timestep.");
    frame.declare ("h1", "cm", Attribute::LogOnly, "\
Estimated mulch water potential at end of timestep.");
    frame.declare ("h_soil", "cm", Attribute::LogOnly, "\
Soil water potential.");
    frame.declare ("K_soil", "cm/h", Attribute::LogOnly, "\
Soil water conductivity.");
    frame.declare ("E_darcy", "cm/h", Attribute::LogOnly, "\
Potential mulch-soil water exchange as calculated by modified Darcy.\n\
Positive down.");
    frame.declare ("h_factor", Attribute::None (), Attribute::LogOnly, "\
Water potential effect on turnover.");
    frame.declare ("T", "dg C", Attribute::LogOnly, "\
Temperature of water in mulch.");
    frame.declare ("T_factor", Attribute::None (), Attribute::LogOnly, "\
Temperature effect on turnover.");
    frame.declare ("DOC_gen", "g/cm^2/h", Attribute::LogOnly, "\
Dissolved organic carbon generated from turnover.");
    frame.declare ("DON_gen", "g/cm^2/h", Attribute::LogOnly, "\
Dissolved organic nitrogen generated from turnover.");
    frame.declare ("SOC_gen", "g/cm^2/h", Attribute::LogOnly, "\
Stationary organic carbon generated from turnover.");
    frame.declare ("SON_gen", "g/cm^2/h", Attribute::LogOnly, "\
Stationary organic nitrogen generated from turnover.");
  }
} LitterMulch_syntax;

// litter.C ends here.
