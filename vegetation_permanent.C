// vegetation_permanent.C
// 
// Copyright 1996-2003 Per Abrahamsen and Søren Hansen
// Copyright 2000-2003 KVL.
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

#include "vegetation.h"
#include "plf.h"
#include "mathlib.h"
#include "log.h"
#include "litter.h"
#include "root_system.h"
#include "canopy_simple.h"
#include "time.h"
#include "soil.h"
#include "crop.h"
#include "am.h"
#include "aom.h"
#include "organic_matter.h"
#include "check.h"
#include <sstream>
#include <deque>

using namespace std;

struct VegetationPermanent : public Vegetation
{
  // Canopy.
  class YearlyLAI
  {
    /* const */ vector<int> years;
    /* const */ vector<PLF> LAIvsDAY;

    // use.
  public:
    double operator() (int year, int yday);
    
    // Create;
    static void load_syntax (Syntax&, AttributeList&);
    YearlyLAI (const vector<AttributeList*>& als);
  } yearly_LAI;
  const PLF LAIvsDAY;		// LAI as a function of time.
  CanopySimple canopy;
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
  const vector<AttributeList*>& litter_am; // Litter AM parameters.
  // Root.
  RootSystem root_system;
  const double WRoot;		// Root dry matter weight [g DM/m^2]

  // Radiation.
  const double albedo_;		// Another reflection factor.

  // Litter.
  Litter litter;

  // Queries.
  double rs_min () const	// Minimum transpiration resistance.
  { return canopy.rs_min; }
  double rs_max () const	// Maximum transpiration resistance.
  { return canopy.rs_max; }
  double LAI () const
  { return canopy.CAI; }
  double height () const
  { return canopy.Height; }
  double cover () const
  { 
    daisy_assert (cover_ >= 0.0);
    return cover_; 
  }
  const PLF& LAIvsH () const
  { return canopy.LAIvsH; }
  const PLF& HvsLAI () const
  { return HvsLAI_; }
  double ACExt () const
  { return canopy.PARext; }
  double ACRef () const
  { return canopy.PARref; }
  double ARExt () const
  { return canopy.EPext; }
  double EpFactor () const
  { return canopy.EpFactor (1.0 /* arbitrary */); }
  double albedo () const
  { return albedo_; }
  double interception_capacity () const
  { return canopy.IntcpCap * LAI (); }


  // Individual crop queries.
  double DS_by_name (symbol) const
  {   return Crop::DSremove; }
  double DM_by_name (symbol, double) const
  { return 0.0; }
  string crop_names () const
  { return name.name (); }

  // Simulation.
  void reset_canopy_structure (const Time& time);
  void tick (const Time& time,
	     const Bioclimate& bioclimate,
	     const Soil& soil,
	     OrganicMatter *const organic_matter,
	     const SoilHeat& soil_heat,
	     const SoilWater& soil_water,
	     SoilNH4 *const soil_NH4, SoilNO3 *const soil_NO3,
	     double& residuals_DM,
	     double& residuals_N_top, double& residuals_C_top,
	     vector<double>& residuals_N_soil,
	     vector<double>& residuals_C_soil,
	     Treelog&);
  double transpiration (double potential_transpiration,
			double canopy_evaporation,
			const Soil& soil, SoilWater& soil_water, 
			double day_fraction, Treelog&);
  void force_production_stress  (double)
  { }
  void kill_all (symbol, const Time&, const Geometry&, Bioclimate&,
		 vector<AM*>&, double&, double&, double&, 
		 vector<double>&, vector<double>&, Treelog&)
  { }
  void harvest (symbol, symbol,
		const Time&, const Geometry&, Bioclimate&,
		double, double, double, double, 
		vector<const Harvest*>&,
                double&,
 		vector<AM*>&, 
		double&, double&, double&,
		double&, double&, double&, vector<double>&, vector<double>&,
                const bool,
		Treelog&)
  { }
  void sow (Treelog&, const AttributeList&, const Geometry&, OrganicMatter&, 
            double& /* kg/ha */, double& /* kg/ha */)
  { throw "Can't sow on permanent vegetation"; }
  void sow (Treelog&, const AttributeList&, const Geometry&)
  { throw "Can't sow on permanent vegetation"; }
  void output (Log&) const;

  double litter_vapor_flux_factor () const
  { return litter.vapor_flux_factor; }
  double litter_water_capacity () const
  { return litter.interception_capacity; }
  double litter_albedo () const
  { return litter.albedo; }

  // Create and destroy.
  void initialize (const Time& time, const Soil& soil, OrganicMatter *const, 
                   Treelog&);
  VegetationPermanent (const AttributeList&);
  ~VegetationPermanent ();
};

double 
VegetationPermanent::YearlyLAI::operator() (int year, int yday)
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
VegetationPermanent::YearlyLAI::load_syntax (Syntax& syntax, AttributeList&)
{
  syntax.add ("year", Syntax::Integer, Syntax::Const, "\
Year for which to use yearly LAI measurements.");
  syntax.add ("LAIvsDAY", "m^2/m^2", "yday", Syntax::OptionalConst, 
		"LAI as a function of Julian day.\n\
\n\
The default LAI will be used before the first day you specify and\n\
after the last specified day.  Default LAI will also be used\n\
whenever 'LAIvsDAY' becomes negative.");
  syntax.order ("year", "LAIvsDAY");
}

VegetationPermanent::YearlyLAI::YearlyLAI (const vector<AttributeList*>& als)
{
  for (unsigned int i = 0; i < als.size (); i++)
    {
      years.push_back (als[i]->integer ("year"));
      LAIvsDAY.push_back (als[i]->plf ("LAIvsDAY"));
    }
}

void
VegetationPermanent::reset_canopy_structure (const Time& time)
{
  canopy.CAI = yearly_LAI (time.year (), time.yday ());
  if (canopy.CAI < 0.0)
    canopy.CAI = LAIvsDAY (time.yday ());
  cover_ =  1.0 - exp (-(canopy.EPext * canopy.CAI));
  daisy_assert (cover_ >= 0.0);
  daisy_assert (cover_ <= 1.0);
  canopy.LAIvsH.clear ();
  canopy.LAIvsH.add (0.0, 0.0);
  canopy.LAIvsH.add (canopy.Height, canopy.CAI);
  HvsLAI_ = canopy.LAIvsH.inverse ();
}
void
VegetationPermanent::tick (const Time& time,
			   const Bioclimate&,
			   const Soil& soil,
			   OrganicMatter *const organic_matter,
			   const SoilHeat&,
			   const SoilWater& soil_water,
			   SoilNH4 *const soil_NH4,
			   SoilNO3 *const soil_NO3,
			   double& residuals_DM,
			   double& residuals_N_top, double& residuals_C_top,
			   vector<double>& /* residuals_N_soil */,
			   vector<double>& /* residuals_C_soil */,
			   Treelog&)
{
  // Canopy.
  const double old_LAI = canopy.CAI;
  reset_canopy_structure (time);

  // Nitrogen uptake.
  if (organic_matter)
    {
      daisy_assert (soil_NH4);
      daisy_assert (soil_NO3);

      N_demand = canopy.CAI * N_per_LAI;
      if (N_actual < -1e10)
	N_actual = N_demand;	// Initialization.
      else
	daisy_assert (N_actual >= 0.0);
      N_uptake = root_system.nitrogen_uptake (soil, soil_water, 
					      *soil_NH4, 0.0, *soil_NO3, 0.0,
					      N_demand - N_actual);
    }
  
  if (canopy.CAI < old_LAI)
    {
      // Litter.
      static const double C_per_DM = 0.420;
      static const double g_per_Mg = 1.0e6;
      static const double ha_per_cm2 = 1.0e-8;
      static const double m2_per_cm2 = 1.0e-4;
      
      const double dLAI = old_LAI - canopy.CAI;
      const double DM = dLAI * DM_per_LAI * g_per_Mg 
        *ha_per_cm2 / m2_per_cm2; // [g DM/m^2]
      const double C = DM * C_per_DM; // [g C/m^2]
      if (organic_matter)
	{
	  N_litter = N_actual * (dLAI / old_LAI);
	  if (!AM_litter)
	    {
	      static const symbol vegetation_symbol ("vegetation");
	      static const symbol dead_symbol ("dead");
	      
	      AM_litter = &AM::create (soil, time, litter_am,
				    vegetation_symbol, dead_symbol,
				    AM::Locked);
	      organic_matter->add (*AM_litter);

	    }
	  AM_litter->add (C * m2_per_cm2, N_litter * m2_per_cm2);
	  residuals_N_top += N_litter;
	}
      residuals_DM += DM;
      residuals_C_top += C;
    }
  else if (organic_matter)
    N_litter = 0.0;

  if (organic_matter)
    N_actual += N_uptake - N_litter;
}

double
VegetationPermanent::transpiration (double potential_transpiration,
				    double canopy_evaporation,
				    const Soil& soil, 
				    SoilWater& soil_water,
				    double day_fraction, Treelog& msg)
{
  if (canopy.CAI > 0.0)
    return  root_system.water_uptake (potential_transpiration, 
				      soil, soil_water, 
				      canopy_evaporation, day_fraction, msg);
  return 0.0;
}

void
VegetationPermanent::output (Log& log) const
{
  Vegetation::output (log);
  output_submodule (canopy, "Canopy", log);
  output_variable (N_demand, log);
  output_variable (N_actual, log);
  output_variable (N_uptake, log);
  output_variable (N_litter, log);
  output_submodule (root_system, "Root", log);
}

void
VegetationPermanent::initialize (const Time& time, const Soil& soil, 
				 OrganicMatter *const organic_matter,
                                 Treelog& msg)
{
  reset_canopy_structure (time);
  root_system.initialize (soil.size ());
  root_system.full_grown (msg, soil, WRoot);
  if (organic_matter)
    {
      static const symbol vegetation_symbol ("vegetation");
      static const symbol litter_symbol ("litter");
      AM_litter = organic_matter->find_am (vegetation_symbol, litter_symbol);
    }
}

VegetationPermanent::VegetationPermanent (const AttributeList& al)
  : Vegetation (al),
    yearly_LAI (al.alist_sequence ("YearlyLAI")),
    LAIvsDAY (al.plf ("LAIvsDAY")),
    canopy (al.alist ("Canopy")),
    cover_ (-42.42e42),
    N_per_LAI (al.number ("N_per_LAI") * 0.1), // [kg N / ha] -> [g N / m^2]
    DM_per_LAI (al.number ("DM_per_LAI")),
    N_demand (0.0),
    N_actual (al.number ("N_actual", -42.42e42)),
    AM_litter (NULL),
    N_uptake (0.0),
    N_litter (0.0),
    litter_am (al.alist_sequence ("litter_am")),
    root_system (al.alist ("Root")),
    WRoot (al.number ("root_DM") * 100.0), // [Mg DM / ha] -> [g DM / m^2]
    albedo_ (al.number ("Albedo")),
    litter (al.alist ("Litter"))
{
  canopy.Height = al.number ("Height");
}

VegetationPermanent::~VegetationPermanent ()
{ }

static struct
VegetationPermanentSyntax
{
  static Vegetation& make (const AttributeList& al)
  { return *new VegetationPermanent (al); }

  VegetationPermanentSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    Vegetation::load_syntax (syntax, alist);
    alist.add ("description", "Permanent (non-crop) vegetation.");
    syntax.add ("LAIvsDAY", "m^2/m^2", "yday", Check::non_negative (),
                Syntax::Const, 
		"LAI as a function of Julian day.\n\
These numbers are used when there are no yearly numbers (YearlyLAI).");
    syntax.add_submodule_sequence("YearlyLAI", Syntax::Const, "\
Yearly LAI measurements.", VegetationPermanent::YearlyLAI::load_syntax);
    alist.add ("YearlyLAI", vector<AttributeList*> ());


    syntax.add_submodule("Canopy", alist, Syntax::State, "Canopy.",
			 CanopySimple::load_syntax);
    syntax.add ("Height", "cm", Check::positive (), Syntax::Const, 
		"permanent height of vegetation.");
    alist.add ("Height", 80.0);
    syntax.add ("N_per_LAI", "kg N/ha/LAI", Check::positive (), Syntax::Const,
		"N content as function of LAI.");
    alist.add ("N_per_LAI", 10.0);
    syntax.add ("DM_per_LAI", "Mg DM/ha/LAI", Check::positive (), 
                Syntax::Const,
		"DM as function of LAI.");
    alist.add ("DM_per_LAI", 0.5);
    syntax.add ("N_demand", "g N/m^2", Syntax::LogOnly,
		"Current potential N content.");
    syntax.add ("N_actual", "g N/m^2", Syntax::OptionalState,
		"N uptake until now (default: 'N_demand').");
    syntax.add ("N_uptake", "g N/m^2/h", Syntax::LogOnly,
		"Nitrogen uptake this hour.");
    syntax.add ("N_litter", "g N/m^2/h", Syntax::LogOnly,
		"Nitrogen in litter this hour.");
    syntax.add_submodule_sequence ("litter_am", Syntax::Const,
				   "Litter AOM parameters.", AOM::load_syntax);
    syntax.add_check ("litter_am", AM::check_om_pools ());
    alist.add ("litter_am", AM::default_AM ());
    syntax.add_submodule("Root", alist, Syntax::State, "Root system.",
			 RootSystem::load_syntax);
    syntax.add ("root_DM", "Mg DM/ha", Check::positive (), Syntax::Const, 
		"Permanent root drymatter.");
    alist.add ("root_DM", 2.0);
    syntax.add ("Albedo", Syntax::None (), Check::positive (), Syntax::Const, 
		"Reflection factor.");
    alist.add ("Albedo", 0.2);
    syntax.add_submodule("Litter", alist, Syntax::State, "Dead stuff.",
			 Litter::load_syntax);
    
    Librarian<Vegetation>::add_type ("permanent", alist, syntax, &make);
  }
} VegetationPermanent_syntax;
