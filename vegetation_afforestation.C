// vegetation_afforestation.C
// 
// Copyright 1996-2003 Per Abrahamsen and Søren Hansen
// Copyright 2000-2003 KVL.
// Copyright 2013 KU.
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
#include "vegetation.h"
#include "plf.h"
#include "mathlib.h"
#include "log.h"
#include "root_system.h"
#include "canopy_simple.h"
#include "time.h"
#include "geometry.h"
#include "soil.h"
#include "crop.h"
#include "am.h"
#include "aom.h"
#include "organic.h"
#include "submodeler.h"
#include "check.h"
#include "librarian.h"
#include "frame_submodel.h"
#include "bioclimate.h"
#include "soil_heat.h"
#include "block_model.h"
#include <boost/scoped_ptr.hpp>
#include <sstream>
#include <deque>

struct VegetationAfforestation : public Vegetation
{
  // Reference time for yearly growth.
  const Time planting_time;
  

  // *** HERTIL ***

  // Canopy.
  class YearlyLAI
  {
    /* const */ std::vector<int> years;
    /* const */ std::vector<PLF> LAIvsDAY;

    // use.
  public:
    double operator() (int year, int yday);
    
    // Create;
    static void load_syntax (Frame&);
    YearlyLAI (const std::vector<boost::shared_ptr<const FrameSubmodel>/**/>& als);
  } yearly_LAI;
  const PLF LAIvsDAY;		// LAI as a function of time.
  const double LAIfactor;       // Multiply by this. []
  boost::scoped_ptr<CanopySimple> canopy;
  double cover_;		// Fraction of soil covered by crops [0-1]
  PLF HvsLAI_;			// Height with LAI below [f: R -> cm]

  // Nitrogen.
  const double N_per_LAI;	// Pot N content as function of LAI [g N/m^2]
  const double DM_per_LAI;	// DM as function of LAI [Mg DM/ha]
  double N_demand;		// Current potential N content. [g N/m^2]
  double N_actual;		// Current N content. [g N/m^2]
  AM* AM_litter;                // Dead plant matter.
  double N_uptake;		// N uptake this hour. [g N/m^2/h]
  double N_litter;		// N litter this hour. [g N/m^2/h]
  const std::vector<boost::shared_ptr<const FrameModel>/**/>& litter_am; // Litter AM parameters.
  // Root.
  boost::scoped_ptr<RootSystem> root_system;
  const double WRoot;		// Root dry matter weight [g DM/m^2]

  // Radiation.
  const double albedo_;		// Another reflection factor.

  // Queries.
  double rs_min () const	// Minimum transpiration resistance.
  { return canopy->rs_min; }
  double rs_max () const	// Maximum transpiration resistance.
  { return canopy->rs_max; }
  double shadow_stomata_conductance () const
  { return 1.0 / rs_min (); }
  double sunlit_stomata_conductance () const
  { return 1.0 / rs_min (); }
  double LAI () const
  { return canopy->CAI; }
  double height () const
  { return canopy->Height; }
  double leaf_width () const
  { return canopy->leaf_width (1.0  /* arbitrary */); }
  double cover () const
  { 
    daisy_assert (cover_ >= 0.0);
    return cover_; 
  }
  const PLF& LAIvsH () const
  { return canopy->LAIvsH; }
  const PLF& HvsLAI () const
  { return HvsLAI_; }
  double ACExt_PAR () const
  { return canopy->PARext; }
  double ACRef_PAR () const
  { return canopy->PARref; }
  double ACExt_NIR () const
  { return canopy->PARext; }
  double ACRef_NIR () const
  { return canopy->PARref; }
  double ARExt () const
  { return canopy->EPext; }
  double EpFactorDry () const
  { return canopy->EpFactorDry (1.0 /* arbitrary */); }
  double EpFactorWet () const
  { return canopy->EpFactorWet (1.0 /* arbitrary */); }
  double albedo () const
  { return albedo_; }
  double interception_capacity () const
  { return canopy->IntcpCap * LAI (); }


  // Individual crop queries.
  double DS_by_name (symbol) const
  {   return Crop::DSremove; }
  double DM_by_name (symbol, double) const
  { return 0.0; }
  double SOrg_DM_by_name (symbol) const
  { return 0.0; }
  std::string crop_names () const
  { return objid.name (); }

  const std::vector<double>& root_density () const
  { return root_system->Density; }
  const std::vector<double>& root_density (symbol crop) const
  { 
    static const std::vector<double> empty;
    return empty;
  }

  // Simulation.
  void reset_canopy_structure (const Time& time);
  double transpiration (const Units&, double potential_transpiration,
			double canopy_evaporation,
                        const Geometry& geo,
			const Soil& soil, const SoilWater& soil_water, 
			double dt, Treelog&);
  void find_stomata_conductance (const Units&, const Time&, 
                                 const Bioclimate&, double, Treelog&)
  { }
  void tick (const Metalib&, const Time& time, const Bioclimate&, 
             const Geometry& geo, const Soil&, const SoilHeat&,
             SoilWater&, Chemistry&, OrganicMatter&,
             double& residuals_DM,
             double& residuals_N_top, double& residuals_C_top,
             std::vector<double>& residuals_N_soil,
             std::vector<double>& residuals_C_soil,
             double dt,
             Treelog&);
  void force_production_stress  (double)
  { }
  void emerge (const symbol, Treelog&)
  { }
  void kill_all (const Metalib&, symbol, const Time&, const Geometry&,
		 std::vector<AM*>&, double&, double&, double&, 
		 std::vector<double>&, std::vector<double>&, Treelog&)
  { }
  void harvest (const Metalib&, symbol, symbol,
		const Time&, const Geometry&, 
		double, double, double, double, 
		std::vector<const Harvest*>&,
                double&,
		double&, double&, double&,
                std::vector<AM*>&, 
		double&, double&, double&,
                std::vector<double>&, std::vector<double>&,
                const bool,
		Treelog&)
  { }
  void pluck (const Metalib&, symbol, symbol,
              const Time&, const Geometry&, 
              double, double, double,
              std::vector<const Harvest*>&,
              double&, double&, double&,
              std::vector<AM*>&, 
              double&, double&, double&,
              std::vector<double>&, std::vector<double>&,
              Treelog&)
  { }
  void sow (const Metalib&, const FrameModel&, 
            const double, const double, const double,
            const Geometry&, OrganicMatter&, const double, 
            double&, double&, const Time&, Treelog&)
  { throw "Can't sow on afforestation vegetation"; }
  void output (Log&) const;

  // Create and destroy.
  void initialize (const Metalib& metalib, 
                   const Units&, const Time& time, const Geometry& geo,
                   const Soil& soil, OrganicMatter&, 
                   Treelog&);
  bool check (const Units&, const Geometry&, Treelog&) const;
  VegetationAfforestation (const BlockModel&);
  ~VegetationAfforestation ();
};

double 
VegetationAfforestation::YearlyLAI::operator() (int year, int yday)
{
  for (unsigned int i = 0; i < years.size (); i++)
    {
      if (years[i] == year)
	{
	  if (yday < LAIvsDAY[i].x (0))
	    return -1.0;

	  if (yday > LAIvsDAY[i].x (LAIvsDAY[i].size () - 1))
	    return -1.0;

	  return LAIvsDAY[i](yday);
	}
    }
  return -1.0;
}
    
void 
VegetationAfforestation::YearlyLAI::load_syntax (Frame& frame)
{
  frame.declare_integer ("year", Attribute::Const, "\
Year for which to use yearly LAI measurements.");
  frame.declare ("LAIvsDAY", "m^2/m^2", "yday", Attribute::OptionalConst, 
		"LAI as a function of Julian day.\n\
\n\
The default LAI will be used before the first day you specify and\n\
after the last specified day.  Default LAI will also be used\n\
whenever 'LAIvsDAY' becomes negative.");
  frame.order ("year", "LAIvsDAY");
}

VegetationAfforestation::YearlyLAI::YearlyLAI
/**/ (const std::vector<boost::shared_ptr<const FrameSubmodel>/**/>& als)
{
  for (unsigned int i = 0; i < als.size (); i++)
    {
      years.push_back (als[i]->integer ("year"));
      LAIvsDAY.push_back (als[i]->plf ("LAIvsDAY"));
    }
}

void
VegetationAfforestation::reset_canopy_structure (const Time& time)
{
  canopy->CAI = yearly_LAI (time.year (), time.yday ());
  if (canopy->CAI < 0.0)
    canopy->CAI = LAIvsDAY (time.yday ());
  canopy->CAI *= LAIfactor;
  cover_ =  1.0 - exp (-(canopy->EPext * canopy->CAI));
  daisy_assert (cover_ >= 0.0);
  daisy_assert (cover_ <= 1.0);
  canopy->LAIvsH.clear ();
  canopy->LAIvsH.add (0.0, 0.0);
  canopy->LAIvsH.add (canopy->Height, canopy->CAI);
  HvsLAI_ = canopy->LAIvsH.inverse ();
}
void
VegetationAfforestation::tick (const Metalib& metalib,
                           const Time& time, const Bioclimate& bioclimate, 
                           const Geometry& geo, const Soil& soil, 
                           const SoilHeat& soil_heat,
			   SoilWater& soil_water, Chemistry& chemistry,
			   OrganicMatter& organic_matter,
			   double& residuals_DM,
			   double& residuals_N_top, double& residuals_C_top,
			   std::vector<double>& /* residuals_N_soil */,
			   std::vector<double>& /* residuals_C_soil */,
                           const double dt, Treelog& msg)
{
  // Canopy.
  const double old_LAI = canopy->CAI;
  reset_canopy_structure (time);

  // Root system.
  const double day_fraction = bioclimate.day_fraction (dt);
  const double T_soil 
    = geo.content_height (soil_heat, &SoilHeat::T, -root_system->Depth);
  root_system->tick_dynamic (T_soil, day_fraction, soil_water, dt);

  // Nitrogen uptake.
  N_demand = canopy->CAI * N_per_LAI;
  if (N_actual < -1e10)
    N_actual = N_demand;	// Initialization.
  else
    daisy_assert (N_actual >= 0.0);
  N_uptake = root_system->nitrogen_uptake (geo, soil, soil_water, 
                                           chemistry, 0.0, 0.0,
                                           N_demand - N_actual, dt);
  
  if (canopy->CAI < old_LAI)
    {
      // Litter.
      static const double C_per_DM = 0.420;
      static const double g_per_Mg = 1.0e6;
      static const double ha_per_cm2 = 1.0e-8;
      static const double m2_per_cm2 = 1.0e-4;
      
      const double dLAI = old_LAI - canopy->CAI;
      const double DM = dLAI * DM_per_LAI * g_per_Mg 
        *ha_per_cm2 / m2_per_cm2; // [g DM/m^2]
      const double C = DM * C_per_DM; // [g C/m^2]

      N_litter = N_actual * (dLAI / old_LAI);
      if (!AM_litter)
        {
          static const symbol vegetation_symbol ("vegetation");
          static const symbol dead_symbol ("dead");

          AM_litter = &AM::create (metalib, geo, time, litter_am,
                                   vegetation_symbol, dead_symbol,
                                   AM::Locked, msg);
          organic_matter.add (*AM_litter);

        }
      AM_litter->add (C * m2_per_cm2, N_litter * m2_per_cm2);
      residuals_N_top += N_litter;

      residuals_DM += DM;
      residuals_C_top += C;
    }
  else 
    N_litter = 0.0;

  N_actual += N_uptake - N_litter;
}

double
VegetationAfforestation::transpiration (const Units& units,
                                    const double potential_transpiration,
				    const double canopy_evaporation,
                                    const Geometry& geo,
				    const Soil& soil, 
				    const SoilWater& soil_water,
				    const double dt, 
                                    Treelog& msg)
{
  if (canopy->CAI > 0.0)
    return  root_system->water_uptake (units, potential_transpiration, 
                                       geo, soil, soil_water, 
                                       canopy_evaporation, 
                                       dt, msg);
  return 0.0;
}

void
VegetationAfforestation::output (Log& log) const
{
  Vegetation::output (log);
  output_submodule (*canopy, "Canopy", log);
  output_variable (N_demand, log);
  output_variable (N_actual, log);
  output_variable (N_uptake, log);
  output_variable (N_litter, log);
  output_submodule (*root_system, "Root", log);
}

void
VegetationAfforestation::initialize (const Metalib& metalib, 
                                 const Units&, const Time& time, 
                                 const Geometry& geo,
                                 const Soil& soil, 
				 OrganicMatter& organic_matter,
                                 Treelog& msg)
{
  reset_canopy_structure (time);
  root_system->initialize (metalib, geo, 0.0, 0.0, msg);
  root_system->full_grown (geo, soil.MaxRootingHeight (), WRoot, msg);

  static const symbol vegetation_symbol ("vegetation");
  static const symbol litter_symbol ("litter");
  AM_litter = organic_matter.find_am (vegetation_symbol, litter_symbol);
}

bool 
VegetationAfforestation::check (const Units& units,
                            const Geometry& geo, Treelog& msg) const 
{ 
  bool ok = true;
  if (!root_system->check (units, geo, msg))
    ok = false;
  return ok;
}

VegetationAfforestation::VegetationAfforestation (const BlockModel& al)
  : Vegetation (al),
    planting_time (al.submodel ("planting_time")),
    // *** HERTIL ***
    yearly_LAI (al.submodel_sequence ("YearlyLAI")),
    LAIvsDAY (al.plf ("LAIvsDAY")),
    LAIfactor (al.number ("LAIfactor")),
    canopy (submodel<CanopySimple> (al, "Canopy")),
    cover_ (-42.42e42),
    N_per_LAI (al.number ("N_per_LAI") * 0.1), // [kg N / ha] -> [g N / m^2]
    DM_per_LAI (al.number ("DM_per_LAI")),
    N_demand (0.0),
    N_actual (al.number ("N_actual", -42.42e42)),
    AM_litter (NULL),
    N_uptake (0.0),
    N_litter (0.0),
    litter_am (al.model_sequence ("litter_am")),
    root_system (submodel<RootSystem> (al, "Root")),
    WRoot (al.number ("root_DM") * 100.0), // [Mg DM / ha] -> [g DM / m^2]
    albedo_ (al.number ("Albedo"))
{
  canopy->Height = al.number ("Height");
}

VegetationAfforestation::~VegetationAfforestation ()
{ }

static struct VegetationAfforestationSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new VegetationAfforestation (al); }

  VegetationAfforestationSyntax ()
    : DeclareModel (Vegetation::component, "afforestation", "\
A growing forest.")
  { }

  void load_frame (Frame& frame) const
  {
    frame.declare_submodule ("planting_time", Attribute::State, "\
Reference time for yearly growth parameters.", Time::load_syntax);
    frame.declare ("canopy_height", "y", "cm", 
                   Check::non_negative (), Attribute::Const, "\
Forest height as a function of years after planting.");
    frame.declare ("root_depth", "y", "cm", 
                   Check::non_negative (), Attribute::Const, "\
Depth of effective root zone as a function of years after planting.");
    frame.declare ("LAI_shape", "d", Attribute::None (), 
                   Check::positive (), Attribute::Const, 
                   "LAI factor as a function of Julian day.\n\
The value `1' represents `LAI_min' and the value `5' represents 'LAI_max'\n\
for the specific year.");
    frame.declare ("LAI_min", "y", "m^2/m^2", 
                   Check::non_negative (), Attribute::Const, "\
Yearly minimum LAI as a function of years after planting.");
    frame.declare ("LAI_max", "y", "m^2/m^2", 
                   Check::positive (), Attribute::Const, "\
Yearly maximum LAI as a function of years after planting.");
    frame.declare ("N_nonleaves", "y", "kg N/ha", Attribute::Const, "\
Nitrogen not accounted for by seasonal LAI variation.\n\
This is a function of years after planting.");
    frame.declare ("N_per_LAI", "kg N/ha",
                   Check::positive (), Attribute::Const,
                   "N content as function of LAI.");
    frame.declare ("litterfall_shape", "d", Attribute::None (), 
                   Check::non_negative (), Attribute::Const, "\
Relative speed of litterfall.\n\
\n\
The total litterfall over a year is specified by 'litterfall_total'.\n\
This parameter specifies how the the litterfall is divided over a\n\
year.  The function is integrated over a year. The litterfall in a\n\
specific period is then the integration of this parameter over the\n\
period, divided by the integration of this function over a year,\n\
multiplied with the total litterfall for that year.");
    frame.declare ("litterfall_total", "y", "Mg DM/ha", 
                   Check::non_negative (), Attribute::Const, "\
Yearly litterfall as function of years after planting.");
    frame.declare ("litterfall_C_per_DM", Attribute::None (),
                   Check::positive (), Attribute::Const,
                   "Carbon content of litter dry matter.");
    frame.declare ("litterfall_C_per_N", Attribute::None (),
                   Check::positive (), Attribute::Const,
                   "C/N ratio in litter.");

    frame.declare ("rhizodeposition_shape", "d", Attribute::None (), 
                   Check::non_negative (), Attribute::Const, "\
Relative speed of rhizodeposition.\n\
\n\
The total rhizodeposition over a year is specified by\n\
'rhizodeposition_total'.  This parameter specifies how the the\n\
rhizodeposition is divided over a year.  The function is integrated\n\
over a year. The rhizodeposition in a specific period is then the\n\
integration of this parameter over the period, divided by the\n\
integration of this function over a year, multiplied with the total\n\
rhizodeposition for that year.");
    frame.declare ("rhizodeposition_total", "y", "Mg C/ha", 
                   Check::non_negative (), Attribute::Const, "\
Yearly rhizodeposition as function of years after planting.");
    frame.declare ("rhizodeposition_C_per_N", Attribute::None (),
                   Check::positive (), Attribute::Const,
                   "C/N ratio in rhizodeposition.");

    frame.declare ("DM_per_LAI", "Mg DM/ha/LAI", Check::positive (), 
                Attribute::Const,
		"DM as function of LAI.");
    frame.set ("DM_per_LAI", 0.5);
    frame.declare ("N_demand", "g N/m^2", Attribute::LogOnly,
		"Current potential N content.");
    frame.declare ("N_actual", "g N/m^2", Attribute::OptionalState,
		"N uptake until now (default: 'N_demand').");
    frame.declare ("N_uptake", "g N/m^2/h", Attribute::LogOnly,
		"Nitrogen uptake this hour.");
    frame.declare ("N_litter", "g N/m^2/h", Attribute::LogOnly,
		"Nitrogen in litter this hour.");
    frame.declare_object ("litter_am", AOM::component, 
                      Attribute::Const, Attribute::Variable, "\
Litter AOM parameters.");
    frame.set_check ("litter_am", AM::check_om_pools ());
    frame.set ("litter_am", AM::default_AM ());
    frame.declare_submodule("Root", Attribute::State, "Root system.",
			 RootSystem::load_syntax);
    frame.declare ("root_DM", "Mg DM/ha", Check::positive (), Attribute::Const, 
		"Afforestation root drymatter.");
    frame.set ("root_DM", 2.0);
    frame.declare ("Albedo", Attribute::None (), Check::positive (), Attribute::Const, 
		"Reflection factor.");
    frame.set ("Albedo", 0.2);

    frame.declare ("LAIfactor", Attribute::None (), Check::non_negative (),
                   Attribute::Const, "\
Multiply calculated LAI with this number for quick scaling.");
    frame.set ("LAIfactor", 1.0);
    frame.declare_submodule("Canopy", Attribute::State, "Canopy.",
			 CanopySimple::load_syntax);

  }
} VegetationAfforestation_syntax;

// vegetation_afforestation.C ends here.
