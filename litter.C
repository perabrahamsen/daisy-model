// litter.C -- Litter lay below vegetation.
// 
// Copyright 2010 KU
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

#include "litter.h"
#include "block_model.h"
#include "mathlib.h"
#include "librarian.h"
#include "check.h"
#include "log.h"
#include "geometry.h"
#include "soil.h"
#include "soil_water.h"
#include "soil_heat.h"
#include "organic.h"
#include "chemistry.h"
#include "mathlib.h"
#include "chemical.h"
#include "am.h"
#include "bioclimate.h"
#include "abiotic.h"
#include "aom.h"
#include <sstream>

// The 'litter' component.

const char *const Litter::component = "litter";

symbol 
Litter::library_id () const
{
  static const symbol id (component);
  return id;
}

void
Litter::output (Log& log) const
{
  output_value (cover (), "cover", log);
}

Litter::Litter  (const BlockModel& al)
  : ModelDerived (al.type_name ())
{ }

Litter::~Litter ()
{ }

static struct LitterInit : public DeclareComponent 
{
  LitterInit ()
    : DeclareComponent (Litter::component, "\
Litter, surface residuals, or mulch below canopy.")
  { }
  void load_frame (Frame& frame) const
  { 
    Model::load_model (frame); 
    frame.declare_fraction ("cover", Attribute::LogOnly, "\
Fraction of surface area covered by litter.");
  }
} Litter_init;

// The 'none' model.

struct LitterNone : public Litter
{
  // Simulation.
  void tick (const Bioclimate&, const Geometry& geo, const Soil& soil,
	     const SoilWater& soil_water, const SoilHeat& soil_heat,
	     OrganicMatter& organic, Chemistry& chemistry,
	     const double dt,
	     Treelog& msg)
  { }
  double cover () const
  { return 0.0; }
  double vapor_flux_factor () const
  { return 1.0; }
  double water_capacity () const
  { return 0.0; }
  double albedo () const
  { return -1.0; }

  // Create and Destroy.
  LitterNone (const BlockModel& al)
    : Litter (al)
  { }
  ~LitterNone ()
  { }
};

static struct LitterNoneSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new LitterNone (al); }
  LitterNoneSyntax ()
    : DeclareModel (Litter::component, "none", "\
The effect of surface residuals is ignored by the model.")
  { }
  void load_frame (Frame&) const
  { }
} LitterNone_syntax;

// The 'permanent' model.

struct LitterPermanent : public Litter
{
  // Parameters.
  const double vapor_flux_factor_;
  const double interception_capacity;
  const double albedo_;

  // Simulation.
  void tick (const Bioclimate&, const Geometry& geo, const Soil& soil,
	     const SoilWater& soil_water, const SoilHeat& soil_heat,	    
	     OrganicMatter& organic, Chemistry& chemistry,
	     const double dt,
	     Treelog& msg)
  { }
  double cover () const
  { return 1.0; }
  double vapor_flux_factor () const
  { return vapor_flux_factor_; }
  double water_capacity () const
  { return interception_capacity; }
  double albedo () const
  { return albedo_; }

  // Create and Destroy.
  LitterPermanent (const BlockModel& al)
    : Litter (al),
      vapor_flux_factor_ (al.number ("vapor_flux_factor")),
      interception_capacity (al.number ("interception_capacity")),
      albedo_ (al.number ("albedo", -1.0))
  { }
  ~LitterPermanent ()
  { }
};

static struct LitterPermanentSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new LitterPermanent (al); }
  LitterPermanentSyntax ()
    : DeclareModel (Litter::component, "permanent", "\
A permanent litter layer cover the ground, as for example in a forest.")
  { }
  void load_frame (Frame& frame) const
  { 
    frame.declare_fraction ("vapor_flux_factor", Attribute::Const, "\
Reduction factor for potential evaporation below litter.");
    frame.set ("vapor_flux_factor", 1.0);
    frame.declare ("interception_capacity", "mm", Attribute::Const,
                   "Storage capacity of litter.");
    frame.declare ("albedo", Attribute::None (), Check::positive (),
                   Attribute::OptionalConst, "Reflection factor.\n\
By default, the surface albedo will be used.");
  }
} LitterPermanent_syntax;

// The 'residue' model.

struct LitterResidue : public Litter
{
  // Parameters.
  const double water_capacity_;	      // Max water in litter DM [L/kg]
  const double vapor_flux_factor_;    // Ep-reduction []
  const double specific_AI;	      // Spec. litter area [m^2/kg DM]
  const double extinction_coefficent; // Beers law for cover []
  const double albedo_;
  
  // Log variables.
  double mass;                    // Surface residuals [kg DM/m^2]
  double cover_;		  // Surface residuals [m^2 mulch/m^2 soil]

  // Simulation.
  void output (Log& log) const
  {
    Litter::output (log);
    output_variable (mass, log);
  }
  void tick (const Bioclimate& bioclimate,
	     const Geometry& geo, const Soil& soil,
	     const SoilWater& soil_water, const SoilHeat& soil_heat,
	     OrganicMatter& organic, Chemistry& chemistry,
	     const double dt,
	     Treelog& msg)
  {
    mass = organic.top_DM ();
    const double MAI = mass * specific_AI; 
    cover_ = 1.0 - exp (- MAI * extinction_coefficent);
  }
  double cover () const
  { return cover_; }
  double vapor_flux_factor () const
  { return vapor_flux_factor_; }
  double water_capacity () const
  { return mass * water_capacity_; }
  double albedo () const
  { return albedo_; }

  // Create and Destroy.
  LitterResidue (const BlockModel& al)
    : Litter (al),
      water_capacity_ (al.number ("water_capacity")),
      vapor_flux_factor_ (al.number ("vapor_flux_factor")),
      specific_AI (al.number ("specific_AI")),
      extinction_coefficent (al.number ("extinction_coefficent")),
      albedo_ (al.number ("albedo", -1.0)),
      mass (NAN),
      cover_ (NAN)
  { }
  ~LitterResidue ()
  { }
};

static struct LitterResidueSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new LitterResidue (al); }
  LitterResidueSyntax ()
    : DeclareModel (Litter::component, "residue", "\
A dynamic litter layer based on applied fertilizer and crop residuals.\n\
\n\
A 'mulch area index' is calculated from the surface organic\n\
matericals, and from that a mulch cover is calculated based on Beer's law\n\
similarily to how the crop cover is calculated from the leaf area\n\
index.")
  { }
  void load_frame (Frame& frame) const
  { 
    frame.set_strings ("cite", "scopel2004");

    frame.declare ("water_capacity", "L/kg", Check::non_negative (),
		   Attribute::Const, "\
Water holding capacity of surface residulas.");
    frame.declare_fraction ("vapor_flux_factor", Attribute::Const, "\
Reduction factor for potential evaporation below litter.\n\
Only area covered by residue is affected.");
    frame.set ("vapor_flux_factor", 0.0);
    frame.declare ("specific_AI", "m^2/kg DM", Check::non_negative (),
		   Attribute::Const, "\
Area covered per litter mass.");
    frame.declare ("extinction_coefficent", Attribute::None (),
		   Check::positive (), Attribute::Const, "\
Beer's law extinction coefficient for litter.");
    frame.declare ("albedo", Attribute::None (), Check::positive (),
                   Attribute::OptionalConst, "Reflection factor.\n\
By default, the surface albedo will be used.");

    frame.declare ("mass", "kg DM/m^2", Attribute::LogOnly, "\
Total mass of mulch layer.");
  }
} LitterResidue_syntax;

// The 'Millet' parameterization.
  
static struct LitterMilletsyntax : public DeclareParam
{ 
  LitterMilletsyntax ()
    : DeclareParam (Litter::component, "Millet", "residue", "\
Millet crop residues in Planaltina.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.set_strings ("cite", "macena2003");

    frame.set ("water_capacity", 3.2);
    frame.set ("specific_AI", 39.);
    frame.set ("extinction_coefficent", 0.45);
  }
} LitterMillet_syntax;

// The 'Maize' parameterization.

static struct LitterMaizesyntax : public DeclareParam
{ 
  LitterMaizesyntax ()
    : DeclareParam (Litter::component, "Maize", "residue", "\
Maize crop residues in La Tinaja.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.set_strings ("cite", "scopel1998");

    frame.set ("water_capacity", 3.8);
    frame.set ("specific_AI", 37.);
    frame.set ("extinction_coefficent", 0.80);
  }
} LitterMaize_syntax;

// The 'mulch' model.

struct LitterMulch : public LitterResidue 
{
  // Parameters.
  const double density;		 // Density of mulch [kg DM/m^3]
  const double decompose_height; // Max height of active mulch layer [cm]
  const double soil_height;	 // Heigh of soil layer providing N [cm]
  const double Theta_res;	 // Residual water content []
  const double Theta_sat;	 // Saturated water content []
  const double h_min;		 // Min. pressure for biological activity [cm]
  
  // Log variables.
  double height;		// Height of mulch layer [cm]
  double decompose_mass;	// In contact with soil [kg/m^2]
  double water;			// Total water in mulch [cm]
  double Theta;			// Relative water content []
  double h;			// Mulch water potential [cm]
  double h_factor;		// Water potential effect []
  double T;			// Temeprature of mulch.
  double T_factor;		// Temperature factor []
  double DOC_gen;		// Disolved organic C generation [g C/cm^2/h]
  double DON_gen;		// Disolved organic N generation [g N/cm^2/h]
  double SOC_gen;		// Stationary C generation [g C/cm^2/h]
  double SON_gen;		// Stationary N generation [g N/cm^2/h]
  
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
      height = cm_per_m * mass / cover () / density;
    else
      height = 0.0;

    // Find mass of active part of mulch layer.
    if (decompose_height >= height)
      decompose_mass = mass;
    else
      decompose_mass = mass * decompose_height / height;

    // Water
    water = bioclimate.get_litter_water () * 0.1; // [mm] -> [cm]
    if (height > 0.0)
      Theta = water / height;			  // []
    else
      Theta = 0.0;

    if (Theta < Theta_res)
      h = h_min;
    else if (Theta > Theta_sat)
      h = 0.0;
    else
      h = -std::pow (-h_min,
		     1.0-((Theta - Theta_res)
			  / (2.0 * Theta_sat / 3.0 - Theta_res)));
    h_factor = Abiotic::f_h (h);

    // Temperature
    T = bioclimate.get_litter_temperature (); // [dg C]
    T_factor = Abiotic::f_T0 (T);

    // Combined
    const double factor = T_factor * h_factor;

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
    for (auto pool: added)
      {
	const double rate = factor * pool->turnover_rate; // [h^-1]
	const double stationary = pool->SOM_fraction ();  // []
	const double dissolved = 1.0 - stationary;	  // []
	const double top_C = pool->top_C; // [g C/cm^2]

	double new_C = NAN; // [g C/cm^2]
	double loss_C = NAN; // [g C/cm^2/h]
	first_order_change (top_C, 0, rate, dt, new_C, loss_C);
	DOC_gen += loss_C * dissolved;
	SOC_gen += loss_C * stationary;
	pool->top_C = new_C;
	
	const double top_N = pool->top_N; // [g N/cm^2]
	double new_N = NAN; // [g N/cm^2]
	double loss_N = NAN; // [g N/cm^2/h]
	first_order_change (top_N, 0, rate, dt, new_N, loss_N);
	DON_gen += loss_N * dissolved;
	SON_gen += loss_N * stationary;
	pool->top_N = new_N;
      }
    double buffer_C = SOC_gen * dt;
    double buffer_N = SON_gen * dt;

    if (chemistry.know (Chemical::DOC ()))
      {
	Chemical& DOC = chemistry.find (Chemical::DOC ());
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
      buffer_C += DON_gen * dt;
    
    organic.add_to_buffer (geo, 0, soil_height, buffer_C, buffer_N);
  }
  void output (Log& log) const
  {
    LitterResidue::output (log);
    output_variable (height, log);
    output_variable (decompose_mass, log);
    output_variable (water, log);
    output_variable (Theta, log);
    output_variable (h, log);
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
      density (al.number ("density")),
      decompose_height (al.number ("decompose_height")),
      soil_height (al.number ("soil_height")),
      Theta_res (al.number ("Theta_res")),
      Theta_sat (find_Theta_sat (al)),
      h_min (al.number ("h_min")),
      height (NAN),
      decompose_mass (NAN),
      water (NAN),
      Theta (NAN),
      h (NAN),
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

    // Log variables.
    frame.declare ("height", "cm", Attribute::LogOnly, "\
Total height of mulch layer.");
    frame.declare ("decompose_mass", "kg DM/m^2", Attribute::LogOnly, "\
Mass of mulch layer that has contact with soil layer.");
    frame.declare ("water", "cm", Attribute::LogOnly, "\
Total water in mulch.");
    frame.declare ("Theta", Attribute::None (), Attribute::LogOnly, "\
Relative water content.");
    frame.declare ("h", "cm", Attribute::LogOnly, "\
Mulch water potential.");
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
