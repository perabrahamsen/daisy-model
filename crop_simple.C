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
#include "common.h"
#include "plf.h"
#include "soil_water.h"
#include "soil.h"
#include "om.h"
#include "organic_matter.h"
#include "soil_heat.h"
#include "soil_NH4.h"
#include "soil_NO3.h"
#include "am.h"
#include "harvest.h"
#include "mathlib.h"
#include "check.h"
#include "message.h"

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
  RootSystem& root_system;
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
  double water_stress () const // [0-1] (1 = full production)
  { return root_system.water_stress; }
  double nitrogen_stress () const // [0-1] (0 = no production)
  { return root_system.nitrogen_stress; }
  double rs_min () const	// Minimum transpiration resistance.
  { return canopy.rs_min; }
  double rs_max () const	// Maximum transpiration resistance.
  { return canopy.rs_max; }
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
  double ActualWaterUptake (double Ept, const Soil&, SoilWater&,
			    double EvapInterception);
  void force_production_stress  (double pstress);

  // Simulation.
public:
  void tick (const Time& time, const Bioclimate&, const Soil&,
	     OrganicMatter*,
	     const SoilHeat&,
	     const SoilWater&,
	     SoilNH4*,
	     SoilNO3*);
  const Harvest& harvest (const string& column_name,
			  const Time&, const Geometry&, 
			  Bioclimate& bioclimate,
			  double stub_length, double stem_harvest,
			  double leaf_harvest, double sorg_harvest,
			  bool kill_off, vector<AM*>& residuals);
  void output (Log&) const;

  double DS () const;
  double DM () const;
  double total_N () const;

  // Create and Destroy.
public:
  void initialize (const Geometry& geometry);
  CropSimple (const AttributeList& vl);
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

double
CropSimple::ActualWaterUptake (double Ept,
			       const Soil& soil, SoilWater& soil_water,
			       const double EvapInterception)
{
  return root_system.water_uptake (Ept, soil, soil_water, EvapInterception);
}

void 
CropSimple::force_production_stress  (double pstress)
{ root_system.production_stress = pstress; }

void
CropSimple::tick (const Time& time,
		  const Bioclimate& bioclimate,
		  const Soil& soil,
		  OrganicMatter* /* organic_matter */,
		  const SoilHeat& soil_heat,
		  const SoilWater& soil_water,
		  SoilNH4* soil_NH4,
		  SoilNO3* soil_NO3)
{
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

  const double T_air = bioclimate.daily_air_temperature ();
  if (time.hour () == 0.0  && T_air > 0.0)
    {
      const double old_T = T;

      T_sum += T_air;
      day += 1.0;
      if (use_T_sum)
	T = T_sum;
      else
	T = day;

      canopy.CAI = LAIvsT (T);

      if (canopy.CAI < forced_LAI)
	canopy.CAI = forced_LAI;
      else if (canopy.CAI > forced_LAI)
	forced_LAI = 0.0;
      
      if (T < T_emergence)
	/* do nothing */;
      else if (T < T_flowering)
	{
	  if (old_T < T_emergence)
	    COUT << " [" << name << " is emerging]\n";
	  const double T_growth = T_flowering - T_emergence;
	  const double step = T_air / T_growth;
	  const double this_far = (T - T_emergence) / T_growth;
	  
	  canopy.Height = height_max * this_far;
	  root_system.tick (soil, soil_heat, WRoot * this_far, WRoot * step, 
			    DS ());
	}
      else if (old_T < T_flowering)
	{
	  COUT << " [" << name << " is flowering]\n";
	  canopy.Height = height_max;
	  root_system.tick (soil, soil_heat, WRoot,
			    WRoot * (1.0 - old_T / T_flowering), DS ());
	}
      else if (T < T_ripe)
	/* do nothing */;
      else if (old_T < T_ripe)
	COUT << " [" << name << " is ripe]\n";
    }

  // Nitrogen uptake.
  if (T > T_emergence)
    {
      N_demand = N_potential / (1.0 + ((N_potential - N_b) / N_b)
				* exp (- N_c * (T - T_emergence)));
      if (soil_NO3)
	{
	  assert (soil_NH4);
	  N_actual += root_system.nitrogen_uptake (soil, soil_water, 
						   *soil_NH4, *soil_NO3,
						   N_demand - N_actual);
	}
      else
	{
	  assert (!soil_NH4);
	  N_actual = N_demand;
	}
    }
}

const Harvest&
CropSimple::harvest (const string& column_name,
		     const Time& time,
		     const Geometry& geometry,
		     Bioclimate& bioclimate,
		     double /* stub_length */,
		     double /* stem_harvest */,
		     double /* leaf_harvest */,
		     double /* sorg_harvest */,
		     bool /* kill_off */,
		     vector<AM*>& residuals)
{
  dead = true;

  // Residuals.
  if (T > T_emergence)
    {
      const double T_growth = T_flowering - T_emergence;
      const double this_far = min (1.0, (T - T_emergence) / T_growth);

      AM& am = AM::create (geometry, time, root_am, name, "root");
      assert (geometry.total (root_system.Density) > 0.0);
      am.add (geometry, 
	      this_far * WRoot * 0.420 * m2_per_cm2,
	      this_far * NRoot * m2_per_cm2,
	      root_system.Density);
      residuals.push_back (&am);
    }

  // Yield.
  Chemicals chemicals;
  if (canopy.CAI > 0.0)
    bioclimate.harvest_chemicals (chemicals, canopy.CAI);
  return *new Harvest (column_name, time, name,
		       0.0, 0.0, 0.0, 
		       0.0, 0.0, 0.0, 
		       0.0, 0.0, 0.0, 
		       0.0, N_actual - NRoot, 0.0,
		       chemicals);
}

void
CropSimple::output (Log& log) const
{
  log.output ("forced_LAI", forced_LAI);
  output_submodule (canopy, "Canopy", log);
  log.output ("T_sum", T_sum);
  log.output ("day", day);
  output_submodule (root_system, "Root", log);
  log.output ("N_demand", N_demand);
  log.output ("N_actual", N_actual);
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
CropSimple::DM () const	
{ throw ("Can't take DM of simple crop model"); }

double
CropSimple::total_N () const
{
  // kg/ha -> g/cm^2
  const double conv = (1000.0 / ((100.0 * 100.0) * (100.0 * 100.0)));
  return N_actual / conv;
}

void
CropSimple::initialize (const Geometry& geometry)
{
  root_system.initialize (geometry.size ());
}

CropSimple::CropSimple (const AttributeList& al)
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
    root_system (*new RootSystem (al.alist ("Root"))),
    WRoot (al.number ("root_DM") * 100.0), // [T DM / ha] -> [g DM / m^2]
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
{
  delete &canopy;
  delete &root_system;
}

static struct CropSimpleSyntax
{
  static Crop& make (const AttributeList& al)
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
	    (void) plf.first_interesting ();
	    (void) plf.last_interesting ();
	    (void) plf.max_at ();
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

    syntax.add ("LAIvsTS", "m^2/m^2", "dg C d", Syntax::OptionalConst, 
		"LAI as a function of T_sum");
    syntax.add ("LAIvsDay", "m^2/m^2", "d", Syntax::OptionalConst, 
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
    syntax.add ("root_DM", "T DM/ha", Check::non_negative (), Syntax::Const, 
		"Fully developed root drymatter.");
    alist.add ("root_DM", 2.0);
    syntax.add ("root_N", "kg N/ha", Check::non_negative (), Syntax::Const,
		"Fully developed root N content.");
    alist.add ("root_N", 20.0);
    syntax.add_submodule_sequence ("root_am", Syntax::Const, 
				   "Root AM parameters.", OM::load_syntax);
    alist.add ("root_am", AM::default_AOM ());
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

