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
#include "organic_matter.h"
#include "soil_heat.h"
#include "soil_NH4.h"
#include "soil_NO3.h"
#include "am.h"
#include "harvest.h"
#include "submodeler.h"
#include "mathlib.h"
#include "check.h"

using namespace std;

// Dimensional conversion.
static const double m2_per_cm2 = 0.0001;

// Chemical constants affecting the crop.
class CropSimple : public Crop
{
public:
  // Canopy.
  const PLF LAIvsT;		// LAI as a function of time.
  double forced_LAI;		// Minimum LAI to use until exceeded by LAIvsTS
  CanopySimple& canopy;
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
  std::auto_ptr<RootSystem> root_system;
  const double WRoot;		// Root dry matter weight [g DM/m^2]
  const double NRoot;		// Root nitrogen weight [g N/m^2]
  const vector<AttributeList*>& root_am; // Root AM parameters.

  // Nitrogen.
  const double N_potential;	// Potential N content at harvest. [g N/m^2]
  double N_demand;		// Current potential N content. [g N/m^2]
  double N_actual;		// Current N content. [g N/m^2]
  const double N_b;		// N uptake form factor.
  const double N_c;		// N uptake form factor.

  // Communication with Bioclimate.
public:
#if 0
  double water_stress () const // [0-1] (1 = full production)
  { return root_system->water_stress; }
  double nitrogen_stress () const // [0-1] (0 = no production)
  { return root_system->nitrogen_stress; }
  double rs_min () const	// Minimum transpiration resistance.
  { return canopy.rs_min; }
  double rs_max () const	// Maximum transpiration resistance.
  { return canopy.rs_max; }
#endif
  double height () const	// Crop height [cm]
  { return canopy.Height; }
  double LAI () const
  { return canopy.CAI; }
  const PLF& LAIvsH () const
  { return canopy.LAIvsH; }
  double PARext () const
  { return canopy.PARext; }
  double PARref () const
  { return canopy.PARref; }
  double EPext () const
  { return canopy.EPext; }
  double IntcpCap () const	// Interception Capacity.
  { return canopy.IntcpCap; }
  double EpFac () const		// Convertion to potential evapotransp.
  { return canopy.EpFactor (DS ()); }
  void CanopyStructure ();
  void CropCAI ();
  double ActualWaterUptake (double Ept, const Geometry& geo,
                            const Soil&, SoilWater&,
			    double EvapInterception, double day_fraction, 
			    Treelog&);
  void force_production_stress  (double pstress);

  // Simulation.
public:
  void tick (const Time& time, const Bioclimate&, const Geometry& geo,
             const Soil&,
	     OrganicMatter*,
	     const SoilHeat&, const SoilWater&, SoilNH4*, SoilNO3*, 
	     double&, double&, double&, vector<double>&, vector<double>&,
	     double ForcedCAI,
	     Treelog&);
  const Harvest& harvest (symbol column_name,
			  const Time&, const Geometry&, 
			  Bioclimate& bioclimate,
			  double stub_length, double stem_harvest,
			  double leaf_harvest, double sorg_harvest,
			  bool kill_off, vector<AM*>& residuals,
			  double& residuals_DM,
			  double& residuals_N_top,
			  double& residuals_C_top,
			  vector<double>& residuals_N_soil,
			  vector<double>& residuals_C_soil,
                          const bool,
			  Treelog&);
  double sorg_height () const 
  { return 100.0; }
  void output (Log&) const;

  double DS () const;
  double DM (double height) const;
  double total_N () const;
  double total_C () const;

  // Create and Destroy.
public:
  void initialize (Treelog&, const Geometry& geo, OrganicMatter*);
  CropSimple (Block& vl);
  ~CropSimple ();
};

void
CropSimple::CanopyStructure ()
{
  // Uniform vertical distribution of leafs.
  canopy.LAIvsH.clear ();
  canopy.LAIvsH.add (0.0, 0.0);
  canopy.LAIvsH.add (canopy.Height, canopy.CAI);
}

void
CropSimple::CropCAI ()
{
  double T;
  if (use_T_sum)
    T = T_sum;
  else
    T = day;

  canopy.CAI = LAIvsT (T);

  if (canopy.CAI < forced_LAI)
    canopy.CAI = forced_LAI;
  else if (canopy.CAI > forced_LAI)
    forced_LAI = 0.0;
}

double
CropSimple::ActualWaterUptake (double Ept,
                               const Geometry& geo,
			       const Soil& soil, SoilWater& soil_water,
			       const double EvapInterception, 
			       const double day_fraction, Treelog& msg)
{
  return root_system->water_uptake (Ept, geo, 
                                    soil, soil_water, EvapInterception,
                                    day_fraction, msg);
}

void 
CropSimple::force_production_stress  (double pstress)
{ root_system->production_stress = pstress; }

void
CropSimple::tick (const Time& time,
		  const Bioclimate& bioclimate,
                  const Geometry& geo,
		  const Soil& soil,
		  OrganicMatter* /* organic_matter */,
		  const SoilHeat& soil_heat,
		  const SoilWater& soil_water,
		  SoilNH4* soil_NH4, SoilNO3* soil_NO3, 
		  double&, double&, double&, vector<double>&, vector<double>&,
		  double ForcedCAI,
		  Treelog& msg)
{
  Treelog::Open nest (msg, name);

  static bool ForcedCAI_warned = false;
  if (!ForcedCAI_warned && ForcedCAI >= 0.0)
    {
      ForcedCAI_warned = true;
      msg.warning ("ForcedLAI does not work with the 'simple' crop model");
    }

  // Growth
  if (time.month () == spring_mm
      && time.mday () == spring_dd 
      && time.hour () == 0.0)
    {
      T_sum = 0.0;
      day = 0.0;
      T = 0.0;
      forced_LAI = spring_LAI;
    }

  // Update average soil temperature.
  const double T_soil = geo.content_at (soil_heat, &SoilHeat::T,
                                        -root_system->Depth);
  root_system->tick_hourly (time.hour (), T_soil);
  
  // Air temperature based growth.
  const double T_air = bioclimate.daily_air_temperature ();
  if (time.hour () == 0.0  && T_air > 0.0)
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
	  const double step = T_air / T_growth;
	  const double this_far = (T - T_emergence) / T_growth;
	  
	  canopy.Height = height_max * this_far;
	  root_system->tick_daily (msg, geo, soil, 
                                   WRoot * this_far, WRoot * step, 
                                   DS ());
	}
      else if (old_T < T_flowering)
	{
	  msg.message ("Flowering");
	  root_system->tick_daily (msg, geo, soil, WRoot,
                                   WRoot * (1.0 - old_T / T_flowering), DS ());
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
      if (soil_NO3)
	{
	  daisy_assert (soil_NH4);
	  N_actual += root_system->nitrogen_uptake (geo, soil, soil_water, 
                                                    *soil_NH4, 0.0, 
                                                    *soil_NO3, 0.0,
                                                    N_demand - N_actual);
	}
      else
	{
	  daisy_assert (!soil_NH4);
	  N_actual = N_demand;
	}
    }
}

const Harvest&
CropSimple::harvest (const symbol column_name,
		     const Time& time,
		     const Geometry& geo,
		     Bioclimate& bioclimate,
		     double /* stub_length */,
		     double /* stem_harvest */,
		     double /* leaf_harvest */,
		     double /* sorg_harvest */,
		     bool /* kill_off */,
		     vector<AM*>& residuals,
		     double& residuals_DM,
		     double& /* residuals_N_top */,
		     double& /* residuals_C_top */,
		     vector<double>& residuals_N_soil,
		     vector<double>& residuals_C_soil,
                     const bool,
		     Treelog&)
{
  dead = true;

  // Residuals.
  if (T > T_emergence)
    {
      const double T_growth = T_flowering - T_emergence;
      const double this_far = min (1.0, (T - T_emergence) / T_growth);

      static const symbol root_symbol ("root");
      AM& am = AM::create (geo.cell_size (), time, root_am, name, root_symbol);
      daisy_assert (geo.total (root_system->Density) > 0.0);
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

  // Yield.
  Chemicals chemicals;
  if (canopy.CAI > 0.0)
    bioclimate.harvest_chemicals (chemicals, canopy.CAI);
  
  // Water and nitrogen stress.
  double wsd = -1.0;
  double nsd = -1.0;

  return *new Harvest (column_name, time, name,
		       0.0, 0.0, 0.0, 
		       0.0, 0.0, 0.0, 
		       0.0, 0.0, 0.0, 
		       0.0, N_actual - NRoot, 0.0,
		       wsd, nsd, chemicals);
}

void
CropSimple::output (Log& log) const
{
  output_variable (forced_LAI, log);
  output_submodule (canopy, "Canopy", log);
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
CropSimple::total_N () const
{
  // kg/ha -> g/cm^2
  const double conv = (1000.0 / ((100.0 * 100.0) * (100.0 * 100.0)));
  return N_actual / conv;
}

double
CropSimple::total_C () const
{ 
  return 0.0; 
}
void
CropSimple::initialize (Treelog&, const Geometry& geo, OrganicMatter*)
{
  root_system->initialize (geo.cell_size ());
  CropCAI ();
}

CropSimple::CropSimple (Block& al)
  : Crop (al),
    LAIvsT (al.check ("LAIvsTS") ? al.plf ("LAIvsTS") : al.plf ("LAIvsDay")),
    forced_LAI (al.number ("forced_LAI")),
    canopy (*new CanopySimple (al.alist ("Canopy"))),
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
    root_am (al.alist_sequence ("root_am")),
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

static struct CropSimpleSyntax
{
  static Crop& make (Block& al)
  { return *new CropSimple (al); }

  static bool check_alist (const AttributeList& al, Treelog& err)
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
	    err.entry (string ("'")
		       + (al.check ("LAIvsTS") ? "LAIvsTS" : "LAIvsDay")
		       + "' has bogus value");
	    ok = false;
	  }
      }

    return ok;
  }
  
  CropSimpleSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    syntax.add_check (check_alist);
    syntax.add ("description", Syntax::String, Syntax::OptionalConst,
		"Description of this parameterization."); 
    alist.add ("description", "Forced growth crop model.");

    syntax.add ("LAIvsTS", "dg C d", "m^2/m^2", Syntax::OptionalConst, 
		"LAI as a function of T_sum");
    syntax.add ("LAIvsDay", "d", "m^2/m^2", Syntax::OptionalConst, 
		"LAI as a function of number of days since sowing.");
    syntax.add ("forced_LAI", "m^2/m^2", Check::non_negative (), 
		Syntax::State, "\
Minimum LAI, automatically cleared when exceeded by 'LAIvsTS'.");
    alist.add ("forced_LAI", 0.0);
    syntax.add_submodule("Canopy", alist, Syntax::State, "Canopy.",
			 CanopySimple::load_syntax);
    syntax.add ("height_max", "cm", Check::non_negative (), Syntax::Const, 
		"Maximum height of plant, reached when flowering.");
    alist.add ("height_max", 80.0);
    syntax.add ("T_sum", "dg C d", Check::non_negative (), Syntax::State, 
		"Temperature sum since sowing (or spring).");
    alist.add ("T_sum", 0.0);
    syntax.add ("day", "d", Check::non_negative (), Syntax::State, 
		"Number of days since sowing (or spring).");
    alist.add ("day", 0.0);
    syntax.add ("spring", Syntax::Integer, Syntax::Const, 2,
		"Zero 'T_sum' at this month and day.");
    vector<int> spring_time;
    spring_time.push_back (3);
    spring_time.push_back (1);
    alist.add ("spring", spring_time);
    syntax.add ("spring_LAI", "m^2/m^2", Check::non_negative (), Syntax::Const, 
		"Set 'forced_LAI' to this after spring clearence of 'T_sum'.");
    alist.add ("spring_LAI", 0.1);
    syntax.add_submodule("Root", alist, Syntax::State, "Root system.",
			 RootSystem::load_syntax);
    syntax.add ("root_DM", "Mg DM/ha", Check::non_negative (), Syntax::Const, 
		"Fully developed root drymatter.");
    alist.add ("root_DM", 2.0);
    syntax.add ("root_N", "kg N/ha", Check::non_negative (), Syntax::Const,
		"Fully developed root N content.");
    alist.add ("root_N", 20.0);
    syntax.add_submodule_sequence ("root_am", Syntax::Const, 
				   "Root AM parameters.", AOM::load_syntax);
    syntax.add_check ("root_am", AM::check_om_pools ());
    alist.add ("root_am", AM::default_AM ());
    syntax.add ("potential_N", "kg N/ha", Check::non_negative (), Syntax::Const,
		"Potential N content at harvest.");
    syntax.add ("N_demand", "g N/m^2", Syntax::LogOnly,
		"Current potential N content.");
    syntax.add ("N_actual", "g N/m^2", Check::non_negative (), Syntax::State,
		"N uptake until now.");
    alist.add ("N_actual", 0.0);
    syntax.add ("N_b", "kg N/ha", Check::non_negative (), Syntax::Const,
		"N uptake form parameter.");
    alist.add ("N_b", 10.0);
    syntax.add_fraction ("N_flowering", Syntax::Const, "\
Fraction of potential N uptake reached at flowering.");
    alist.add ("N_flowering", 0.9);

    Librarian<Crop>::add_type ("simple", alist, syntax, &make);
  }
} simple_crop_syntax;

