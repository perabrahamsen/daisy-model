// crop_simple.C --- Forced grow crop model.
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
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

#include "crop.h"
#include "root_system.h"
#include "canopy_simple.h"
#include "log.h"
#include "time.h"
#include "bioclimate.h"
#include "plf.h"
#include "soil_water.h"
#include "geometry.h"
#include "soil.h"
#include "aom.h"
#include "organic.h"
#include "soil_heat.h"
#include "am.h"
#include "harvest.h"
#include "submodeler.h"
#include "mathlib.h"
#include "check.h"
#include "librarian.h"
#include "treelog.h"
#include "frame.h"

// Dimensional conversion.
static const double m2_per_cm2 = 0.0001;

class CropSimple : public Crop
{
  const Metalib& metalib;
public:
  // Canopy.
  const PLF LAIvsT;		// LAI as a function of time.
  double forced_LAI;		// Minimum LAI to use until exceeded by LAIvsTS
  const std::unique_ptr<CanopySimple> canopy;
  const double height_max;	// Max height of canopy [cm]

  // Temperature sum and day.
  double T_sum;			// Temperature sum since sowing [dg C d]
  double day;			// Days since swoing [d]
  const bool use_T_sum;		// True if T is T_sum rather than day.
  double T;			// Either T_sum or day
  const double T_emergence;	// T at emergence
  const double T_flowering;	// T at flowering
  const double T_ripe;		// T when crop is ripe
  bool dead;			// True after harvest.

  // Springtime reset for winter crops.
  const int spring_mm;		// Month where T is zeroed.
  const int spring_dd;		// Day where T is zeroed.
  const double spring_LAI;	// LAI to use after T is zeroed.

  // Root.
  std::unique_ptr<RootSystem> root_system;
  const double WRoot;		// Root dry matter weight [g DM/m^2]
  const double NRoot;		// Root nitrogen weight [g N/m^2]
  const std::vector<boost::shared_ptr<const FrameModel>/**/>& root_am; // Root AM parameters.

  // Nitrogen.
  const double N_potential;	// Potential N content at harvest. [g N/m^2]
  double N_demand;		// Current potential N content. [g N/m^2]
  double N_actual;		// Current N content. [g N/m^2]
  const double N_b;		// N uptake form factor.
  const double N_c;		// N uptake form factor.

  // Communication with Bioclimate.
public:
  double rs_min () const	// Minimum transpiration resistance.
  { return canopy->rs_min; }
  double rs_max () const	// Maximum transpiration resistance.
  { return canopy->rs_max; }
  double leaf_width () const
  { return canopy->leaf_width (DS ()); }
  double height () const	// Crop height [cm]
  { return canopy->Height; }
  double LAI () const
  { return canopy->CAI; }
  const PLF& LAIvsH () const
  { return canopy->LAIvsH; }
  double PARext () const
  { return canopy->PARext; }
  double PARref () const
  { return canopy->PARref; }
  double NIRext () const
  { return canopy->PARext; }
  double NIRref () const
  { return canopy->PARref; }
  double EPext () const
  { return canopy->EPext; }
  double IntcpCap () const	// Interception Capacity.
  { return canopy->IntcpCap; }
  double EpFacDry () const		// Convertion to potential evapotransp.
  { return canopy->EpFactorDry (DS ()); }
  double EpFacWet () const		// Convertion to potential evapotransp.
  { return canopy->EpFactorWet (DS ()); }
  void CanopyStructure ();
  void CropCAI ();
  double ActualWaterUptake (const Units&, double Ept, const Geometry& geo,
                            const Soil&, const SoilWater&,
			    double EvapInterception,
                            double dt, Treelog&);
  void force_production_stress  (double pstress);

  // Simulation.
public:
  void tick (const Metalib&, const Time& time, const Bioclimate&, double ForcedCAI,
             const Geometry& geo, const Soil&, const SoilHeat&,
             SoilWater&, Chemistry&, OrganicMatter&,
             double& residuals_DM,
             double& residuals_N_top, double& residuals_C_top,
             std::vector<double>& residuals_N_soil,
             std::vector<double>& residuals_C_soil,
             double dt,
             Treelog&);
  void emerge ()
  {
    if (use_T_sum)
      T_sum = std::max (T_sum, T_emergence - 0.1);
    else
      day = std::max (day, T_emergence - 0.1);

  }
  const Harvest& harvest (symbol column_name,
			  const Time&, const Geometry&, 
			  double stub_length, double stem_harvest,
			  double leaf_harvest, double sorg_harvest,
			  bool kill_off, std::vector<AM*>& residuals,
			  double& residuals_DM,
			  double& residuals_N_top,
			  double& residuals_C_top,
			  std::vector<double>& residuals_N_soil,
			  std::vector<double>& residuals_C_soil,
                          const bool,
			  Treelog&);
  double sorg_height () const 
  { return 100.0; }
  void output (Log&) const;

  double DS () const;
  double DM (double height) const;
  double SOrg_DM () const;
  double N_fixated () const
  { return 0.0; }
  double total_N () const;
  double total_C () const;
  const std::vector<double>& root_density () const
  { return root_system->Density; }

  // Create and Destroy.
public:
  void initialize (const Metalib& metalib, 
                   const Units&, const Geometry& geo, 
                   double row_width, double row_pos, double seed,
                   OrganicMatter&, double SoilLimit, const Time&, Treelog&);
  void initialize (const Metalib& metalib, 
                   const Units&, const Geometry& geo, 
                   OrganicMatter&, double SoilLimit, const Time&, Treelog&);
  bool check (const Units& units, const Geometry& geo, Treelog&) const;
  CropSimple (const BlockModel& vl);
  ~CropSimple ();
};

void
CropSimple::CanopyStructure ()
{
  // Uniform vertical distribution of leafs.
  canopy->LAIvsH.clear ();
  canopy->LAIvsH.add (0.0, 0.0);
  canopy->LAIvsH.add (canopy->Height, canopy->CAI);
}

void
CropSimple::CropCAI ()
{
  if (use_T_sum)
    T = T_sum;
  else
    T = day;

  canopy->CAI = LAIvsT (T);

  if (canopy->CAI < forced_LAI)
    canopy->CAI = forced_LAI;
  else if (canopy->CAI > forced_LAI)
    forced_LAI = 0.0;
}

double
CropSimple::ActualWaterUptake (const Units& units, double Ept,
                               const Geometry& geo,
			       const Soil& soil, const SoilWater& soil_water,
			       const double EvapInterception, 
                               const double dt, Treelog& msg)
{
  return root_system->water_uptake (units, Ept, geo, 
                                    soil, soil_water, EvapInterception,
                                    dt, msg);
}

void 
CropSimple::force_production_stress  (double pstress)
{ root_system->production_stress = pstress; }

void
CropSimple::tick (const Metalib&, const Time& time, const Bioclimate& bioclimate, 
                  const double ForcedCAI,
                  const Geometry& geo, const Soil& soil,
		  const SoilHeat& soil_heat,
		  SoilWater& soil_water, Chemistry& chemistry,
		  OrganicMatter& /* organic_matter */,
		  double&, double&, double&,
                  std::vector<double>&, std::vector<double>&,
                  const double dt, Treelog& msg)
{
  TREELOG_MODEL (msg);

  static bool ForcedCAI_warned = false;
  if (!ForcedCAI_warned && ForcedCAI >= 0.0)
    {
      ForcedCAI_warned = true;
      msg.warning ("ForcedLAI does not work with the 'simple' crop model");
    }

  // Growth
  if (time.month () == spring_mm
      && time.mday () == spring_dd 
      && time.hour () == 0)
    {
      T_sum = 0.0;
      day = 0.0;
      T = 0.0;
      forced_LAI = spring_LAI;
    }

  // Update average soil temperature.
  const double day_fraction = bioclimate.day_fraction (dt);
  const double T_soil 
    = geo.content_height (soil_heat, &SoilHeat::T, -root_system->Depth);
  root_system->tick_dynamic (T_soil, day_fraction, soil_water, dt);
  
  // Air temperature based growth.
  const double T_air = bioclimate.daily_air_temperature ();
  if (time.hour () == 0  && T_air > 0.0)
    {
      const double old_T = T;

      T_sum += T_air;
      day += 1.0;
      CropCAI ();
      
      if (T < T_emergence)
	/* do nothing */;
      else if (T < T_flowering)
	{
	  if (old_T < T_emergence)
	    msg.message ("Emerging");

	  const double T_growth = T_flowering - T_emergence;
	  const double this_far = (T - T_emergence) / T_growth;
	  
	  canopy->Height = height_max * this_far;
	  root_system->tick_daily (geo, soil, soil_water, 
                                   WRoot * this_far, true, 
                                   DS (), msg);
	}
      else if (old_T < T_flowering)
	{
	  msg.message ("Flowering");
	  root_system->tick_daily (geo, soil, soil_water, 
                                   WRoot, true, DS (), msg);
	}
      else if (T < T_ripe)
	/* do nothing */;
      else if (old_T < T_ripe)
	msg.message ("Ripe");
    }

  // Nitrogen uptake.
  if (T > T_emergence)
    {
      N_demand = N_potential / (1.0 + ((N_potential - N_b) / N_b)
				* exp (- N_c * (T - T_emergence)));
      N_actual += root_system->nitrogen_uptake (geo, soil, soil_water, 
                                                chemistry, 0.0, 0.0,
                                                N_demand - N_actual,
                                                dt);
    }
}

const Harvest&
CropSimple::harvest (const symbol column_name,
		     const Time& time,
		     const Geometry& geo,
		     double /* stub_length */,
		     double /* stem_harvest */,
		     double /* leaf_harvest */,
		     double /* sorg_harvest */,
		     bool /* kill_off */,
		     std::vector<AM*>& residuals,
		     double& residuals_DM,
		     double& /* residuals_N_top */,
		     double& /* residuals_C_top */,
		     std::vector<double>& residuals_N_soil,
		     std::vector<double>& residuals_C_soil,
                     const bool,
		     Treelog& msg)
{
  dead = true;

  // Residuals.
  if (T > T_emergence)
    {
      const double T_growth = T_flowering - T_emergence;
      const double this_far = std::min (1.0, (T - T_emergence) / T_growth);

      static const symbol root_symbol ("root");
      AM& am = AM::create (metalib, geo, time, root_am, objid, root_symbol,
                           AM::Unlocked, msg);
      daisy_assert (geo.total_soil (root_system->Density) > 0.0);
      am.add_surface (geo, 
                      this_far * WRoot * 0.420 * m2_per_cm2,
                      this_far * NRoot * m2_per_cm2,
                      root_system->Density);
      residuals.push_back (&am);
      residuals_DM += this_far * WRoot;
      geo.add_surface (residuals_N_soil, root_system->Density,
                       this_far * NRoot * m2_per_cm2);
      geo.add_surface (residuals_C_soil, root_system->Density, 
                       this_far * WRoot * 0.420 * m2_per_cm2);
    }

  // Water and nitrogen stress.
  double wsd = -1.0;
  double nsd = -1.0;

  return *new Harvest (column_name, time, objid,
		       0.0, 0.0, 0.0, 
		       0.0, 0.0, 0.0, 
		       0.0, 0.0, 0.0, 
		       0.0, N_actual - NRoot, 0.0,
		       wsd, nsd, -1.0);
}

void
CropSimple::output (Log& log) const
{
  output_variable (forced_LAI, log);
  output_submodule (*canopy, "Canopy", log);
  output_variable (T_sum, log);
  output_variable (day, log);
  output_submodule (*root_system, "Root", log);
  output_variable (N_demand, log);
  output_variable (N_actual, log);
}

double
CropSimple::DS () const
{ 
  if (dead)
    return DSremove;
  else if (T < T_emergence)
    return T / T_emergence - 1.0;
  else if (T < T_flowering)
    return (T - T_emergence) / (T_flowering - T_emergence);
  else if (T < T_ripe)
    return (T - T_flowering) / (T_ripe - T_flowering);
  else
    return 2.0;
}

double
CropSimple::DM (double) const	
{ throw ("Can't take DM of simple crop model"); }

double
CropSimple::SOrg_DM () const	
{ throw ("Can't estimate the dry matter with simple crop model"); }

double
CropSimple::total_N () const
{
  // kg/ha -> g/m^2
  const double conv = 1000.0 / (100.0 * 100.0);
  return N_actual / conv;
}

double
CropSimple::total_C () const
{ 
  return 0.0; 
}

void
CropSimple::initialize (const Metalib& metalib, 
                        const Units& units, const Geometry& geo, 
                        const double row_width, 
                        const double row_pos, 
                        const double seed,
                        OrganicMatter&, double /* SoilLimit */,
                        const Time&, Treelog& msg)
{
  TREELOG_MODEL (msg);
  if (seed >= 0)
    msg.warning ("Seed ignored by simple crop model");
  root_system->initialize (metalib, geo, row_width, row_pos, msg);
  CropCAI ();
}

void
CropSimple::initialize (const Metalib& metalib, 
                        const Units& units, const Geometry& geo, 
                        OrganicMatter&, double /* SoilLimit */, 
                        const Time&, Treelog& msg)
{
  root_system->initialize (units, geo, msg);
  CropCAI ();
}

bool
CropSimple::check (const Units& units, const Geometry& geo, Treelog& msg) const
{
  bool ok = true;
  if (!root_system->check (units, geo, msg))
    ok = false;
  return ok;
}

CropSimple::CropSimple (const BlockModel& al)
  : Crop (al),
    metalib (al.metalib ()),
    LAIvsT (al.check ("LAIvsTS") ? al.plf ("LAIvsTS") : al.plf ("LAIvsDay")),
    forced_LAI (al.number ("forced_LAI")),
    canopy (submodel<CanopySimple> (al, "Canopy")),
    height_max (al.number ("height_max")),
    T_sum (al.number ("T_sum")),
    day (al.number ("day")),
    use_T_sum (al.check ("LAIvsTS")),
    T (use_T_sum ? T_sum : day),
    T_emergence (LAIvsT.first_interesting ()),
    T_flowering (LAIvsT.max_at ()),
    T_ripe (LAIvsT.last_interesting ()),
    dead (false),
    spring_mm (al.integer_sequence ("spring")[0]),
    spring_dd (al.integer_sequence ("spring")[1]),
    spring_LAI (al.number ("spring_LAI")),
    root_system (submodel<RootSystem> (al, "Root")),
    WRoot (al.number ("root_DM") * 100.0), // [Mg DM / ha] -> [g DM / m^2]
    NRoot (al.number ("root_N") * 0.1),	// [kg N / ha] -> [g N / m^2]
    root_am (al.model_sequence ("root_am")),
    N_potential (al.number ("potential_N") * 0.1),	// [kg N / ha] -> [g N / m^2]
    N_demand (0.0),
    N_actual (al.number ("N_actual")),
    N_b (al.number ("N_b") * 0.1),
    N_c (log ((N_potential - N_b) 
	      / ((1.0 / al.number ("N_flowering") - 1.0) * N_b))
	 / (T_flowering - T_emergence))
{ }

CropSimple::~CropSimple ()
{ delete &canopy; }

static struct CropSimpleSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
 { return new CropSimple (al); }

  static bool check_alist (const Metalib&, const Frame& al, Treelog& err)
  { 
    bool ok = true;
    if (!al.check ("LAIvsTS") && !al.check ("LAIvsDay"))
      {
	err.entry ("Must specify either 'LAIvsTS' or 'LAIvsDay'");
	ok = false;
      }
    else if (al.check ("LAIvsTS") && al.check ("LAIvsDay"))
      {
	err.entry ("Cannot specify both 'LAIvsTS' or 'LAIvsDay'");
	ok = false;
      }
    else
      {
	const PLF& plf = al.check ("LAIvsTS")
	  ? al.plf ("LAIvsTS") : al.plf ("LAIvsDay");
	try 
	  {
	    plf.first_interesting ();
	    plf.last_interesting ();
	    plf.max_at ();
	  }
	catch (...)
	  {
	    err.entry (std::string ("'")
		       + (al.check ("LAIvsTS") ? "LAIvsTS" : "LAIvsDay")
		       + "' has bogus value");
	    ok = false;
	  }
      }

    return ok;
  }
  
  CropSimpleSyntax ()
    : DeclareModel (Crop::component, "simple", "Forced growth crop model.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.add_check (check_alist);
    frame.declare ("LAIvsTS", "dg C d", "m^2/m^2", Attribute::OptionalConst, 
		"LAI as a function of T_sum");
    frame.declare ("LAIvsDay", "d", "m^2/m^2", Attribute::OptionalConst, 
		"LAI as a function of number of days since sowing.");
    frame.declare ("forced_LAI", "m^2/m^2", Check::non_negative (), 
		Attribute::State, "\
Minimum LAI, automatically cleared when exceeded by 'LAIvsTS'.");
    frame.set ("forced_LAI", 0.0);
    frame.declare_submodule("Canopy", Attribute::State, "Canopy.",
			 CanopySimple::load_syntax);
    frame.declare ("height_max", "cm", Check::non_negative (), Attribute::Const, 
		"Maximum height of plant, reached when flowering.");
    frame.set ("height_max", 80.0);
    frame.declare ("T_sum", "dg C d", Check::non_negative (), Attribute::State, 
		"Temperature sum since sowing (or spring).");
    frame.set ("T_sum", 0.0);
    frame.declare ("day", "d", Check::non_negative (), Attribute::State, 
		"Number of days since sowing (or spring).");
    frame.set ("day", 0.0);
    frame.declare_integer ("spring", Attribute::Const, 2,
		"Zero 'T_sum' at this month and day.");
    std::vector<int> spring_time;
    spring_time.push_back (3);
    spring_time.push_back (1);
    frame.set ("spring", spring_time);
    frame.declare ("spring_LAI", "m^2/m^2", Check::non_negative (), Attribute::Const, 
		"Set 'forced_LAI' to this after spring clearence of 'T_sum'.");
    frame.set ("spring_LAI", 0.1);
    frame.declare_submodule("Root", Attribute::State, "Root system.",
			 RootSystem::load_syntax);
    frame.declare ("root_DM", "Mg DM/ha", Check::non_negative (), Attribute::Const, 
		"Fully developed root drymatter.");
    frame.set ("root_DM", 2.0);
    frame.declare ("root_N", "kg N/ha", Check::non_negative (), Attribute::Const,
		"Fully developed root N content.");
    frame.set ("root_N", 20.0);
    frame.declare_object ("root_am", AOM::component, 
                      Attribute::Const, Attribute::Variable, "\
Root AM parameters.");
    frame.set_check ("root_am", AM::check_om_pools ());
    frame.set ("root_am", AM::default_AM ());
    frame.declare ("potential_N", "kg N/ha", Check::non_negative (), Attribute::Const,
		"Potential N content at harvest.");
    frame.declare ("N_demand", "g N/m^2", Attribute::LogOnly,
		"Current potential N content.");
    frame.declare ("N_actual", "g N/m^2", Check::non_negative (), Attribute::State,
		"N uptake until now.");
    frame.set ("N_actual", 0.0);
    frame.declare ("N_b", "kg N/ha", Check::non_negative (), Attribute::Const,
		"N uptake form parameter.");
    frame.set ("N_b", 10.0);
    frame.declare_fraction ("N_flowering", Attribute::Const, "\
Fraction of potential N uptake reached at flowering.");
    frame.set ("N_flowering", 0.9);
  }
} simple_crop_syntax;

// crop_simple.C ends here.
