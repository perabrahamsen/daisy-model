 // vegetation_permanent.C

#include "vegetation.h"
#include "plf.h"
#include "mathlib.h"
#include "log.h"
#include "root_system.h"
#include "canopy_simple.h"
#include "time.h"
#include "soil.h"
#include "crop.h"
#include "am.h"
#include "aom.h"
#include "organic_matter.h"
#include <deque>

struct VegetationPermanent : public Vegetation
{
  // Canopy.
  const PLF LAIvsDAY;		// LAI as a function of time.
  CanopySimple& canopy;
  double cover_;		// Fraction of soil covered by crops [0-1]
  PLF HvsLAI_;			// Height with LAI below [f: R -> cm]

  // Nitrogen.
  const double N_per_LAI;	// Pot N content as function of LAI [g N/m^2]
  const double DM_per_LAI;	// DM as function of LAI [T DM/ha]
  double N_demand;		// Current potential N content. [g N/m^2]
  double N_actual;		// Current N content. [g N/m^2]
  AM* litter;			// Dead plant matter.
  double N_uptake;		// N uptake this hour. [g N/m^2/h]
  double N_litter;		// N litter this hour. [g N/m^2/h]
  const vector<AttributeList*>& litter_am; // Litter AM parameters.
  // Root.
  RootSystem& root_system;
  const double WRoot;		// Root dry matter weight [g DM/m^2]

  // Radiation.
  const double albedo_;		// Another reflection factor.

  // Queries.
  double LAI () const
  { return canopy.CAI; }
  double height () const
  { return canopy.Height; }
  double cover () const
  { return cover_; }
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
  { return canopy.IntcpCap; }


  // Individual crop queries.
  double DS_by_name (const string&) const
  {   return Crop::DSremove; }
  double DM_by_name (const string&) const
  { return 0.0; }

  // Simulation.
  void tick (const Time& time,
	     const Bioclimate& bioclimate,
	     const Soil& soil,
	     OrganicMatter& organic_matter,
	     const SoilHeat& soil_heat,
	     const SoilWater& soil_water,
	     SoilNH4& soil_NH4,
	     SoilNO3& soil_NO3,
	     double& residuals_DM,
	     double& residuals_N_top, double& residuals_C_top,
	     vector<double>& residuals_N_soil,
	     vector<double>& residuals_C_soil,
	     Treelog&);
  void tick (const Time& time,
	     const Bioclimate& bioclimate,
	     const Soil& soil,
	     const SoilHeat& soil_heat,
	     const SoilWater& soil_water, 
	     double& residuals_DM,
	     double& residuals_N_top, double& residuals_C_top,
	     vector<double>& residuals_N_soil,
	     vector<double>& residuals_C_soil,
	     Treelog&);
  double transpiration (double potential_transpiration,
			double canopy_evaporation,
			const Soil& soil, SoilWater& soil_water, 
			double day_fraction, Treelog&);
  void kill_all (const string&, const Time&, const Geometry&, Bioclimate&,
		 vector<AM*>&, double&, double&, double&, 
		 vector<double>&, vector<double>&, Treelog&)
  { }
  void harvest (const string&, const string&,
		const Time&, const Geometry&, Bioclimate&,
		double, double, double, double, 
		vector<const Harvest*>&,
 		vector<AM*>&, 
		double&, double&, double&,
		double&, double&, double&, vector<double>&, vector<double>&,
		Treelog&)
  { }
  double sow (Treelog&, const AttributeList&, const Geometry&, OrganicMatter&)
  { throw "Can't sow on permanent vegetation"; }
  void sow (Treelog&, const AttributeList&, const Geometry&)
  { throw "Can't sow on permanent vegetation"; }
  void output (Log&) const;

  // Create and destroy.
  void initialize (Treelog&, const Soil& soil, OrganicMatter&);
  VegetationPermanent (const AttributeList&);
  ~VegetationPermanent ();
};

void
VegetationPermanent::tick (const Time& time,
			   const Bioclimate&,
			   const Soil& soil,
			   OrganicMatter&,
			   const SoilHeat&,
			   const SoilWater& soil_water,
			   SoilNH4& soil_NH4,
			   SoilNO3& soil_NO3,
			   double& residuals_DM,
			   double& residuals_N_top, double& residuals_C_top,
			   vector<double>& /* residuals_N_soil */,
			   vector<double>& /* residuals_C_soil */,
			   Treelog&)
{
  // Canopy.
  const double old_LAI = canopy.CAI;
  canopy.CAI = LAIvsDAY (time.yday ());
  cover_ =  1.0 - exp (-(canopy.EPext * canopy.CAI));
  canopy.LAIvsH.clear ();
  canopy.LAIvsH.add (0.0, 0.0);
  canopy.LAIvsH.add (canopy.Height, canopy.CAI);
  HvsLAI_ = canopy.LAIvsH.inverse ();

  // Nitrogen uptake.
  N_demand = canopy.CAI * N_per_LAI;
  if (N_actual < -1e10)
    N_actual = N_demand;	// Initialization.
  else
    daisy_assert (N_actual >= 0.0);
  N_uptake = root_system.nitrogen_uptake (soil, soil_water, 
					  soil_NH4, soil_NO3,
					  N_demand - N_actual);
  if (canopy.CAI < old_LAI)
    {
      // Litter.
      static const double C_per_DM = 0.420;
      static const double ha_per_cm2 = 1.0e-8;
      static const double m2_per_cm2 = 1.0e-4;
      
      const double dLAI = old_LAI - canopy.CAI;
      const double DM = dLAI * DM_per_LAI * ha_per_cm2 / m2_per_cm2;
      const double C = DM * C_per_DM;
      N_litter = N_actual * (dLAI / old_LAI);
      if (!litter)
	litter = &AM::create (soil, time, litter_am,
			      "vegetation", "dead", AM::Locked);
      litter->add (C * m2_per_cm2, N_litter * m2_per_cm2);
      residuals_DM += DM;
      residuals_N_top += N_litter;
      residuals_C_top += C;
    }
  else 
    N_litter = 0.0;

  N_actual += N_uptake - N_litter;
}

void
VegetationPermanent::tick (const Time& time,
			   const Bioclimate&,
			   const Soil&,
			   const SoilHeat&,
			   const SoilWater&,
			   double& residuals_DM,
			   double& /*residuals_N_top */,
			   double& residuals_C_top,
			   vector<double>& /* residuals_N_soil */,
			   vector<double>& /* residuals_C_soil */,
			   Treelog&)
{
  const double old_LAI = canopy.CAI;
  canopy.CAI = LAIvsDAY (time.yday ());
  cover_ =  1.0 - exp (-(canopy.EPext * canopy.CAI));
  canopy.LAIvsH.clear ();
  canopy.LAIvsH.add (0.0, 0.0);
  canopy.LAIvsH.add (canopy.Height, canopy.CAI);
  HvsLAI_ = canopy.LAIvsH.inverse ();

  if (canopy.CAI < old_LAI)
    {
      // Litter.
      static const double C_per_DM = 0.420;
      static const double ha_per_cm2 = 1.0e-8;
      static const double m2_per_cm2 = 1.0e-4;
      
      const double dLAI = old_LAI - canopy.CAI;
      const double DM = dLAI * DM_per_LAI * ha_per_cm2 / m2_per_cm2;
      const double C = DM * C_per_DM;
      residuals_DM += DM;
      residuals_C_top += C;
    }
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
  log.output ("N_demand", N_demand);
  log.output ("N_actual", N_actual);
  log.output ("N_uptake", N_uptake);
  log.output ("N_litter", N_litter);
  output_submodule (root_system, "Root", log);
}

void
VegetationPermanent::initialize (Treelog& msg, const Soil& soil, 
				 OrganicMatter& organic_matter)
{
  root_system.initialize (soil.size ());
  root_system.full_grown (msg, soil, WRoot);
  litter = organic_matter.find_am ("vegetation", "litter");
}

VegetationPermanent::VegetationPermanent (const AttributeList& al)
  : Vegetation (al),
    LAIvsDAY (al.plf ("LAIvsDAY")),
    canopy (*new CanopySimple (al.alist ("Canopy"))),
    N_per_LAI (al.number ("N_per_LAI") * 0.1), // [kg N / ha] -> [g N / m^2]
    DM_per_LAI (al.number ("DM_per_LAI")),
    N_demand (0.0),
    N_actual (al.check ("N_actual") ? al.number ("N_actual") : -42.42e42),
    litter (NULL),
    N_uptake (0.0),
    N_litter (0.0),
    litter_am (al.alist_sequence ("litter_am")),
    root_system (*new RootSystem (al.alist ("Root"))),
    WRoot (al.number ("root_DM") * 100.0), // [T DM / ha] -> [g DM / m^2]
    albedo_ (al.number ("Albedo"))
{
  canopy.Height = al.number ("Height");
}

VegetationPermanent::~VegetationPermanent ()
{ 
  delete &canopy;
  delete &root_system;
}

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
    syntax.add ("LAIvsDAY", "m^2/m^2", "yday", Syntax::Const, 
		"LAI as a function of Julian day.");
    syntax.add_submodule("Canopy", alist, Syntax::State, "Canopy.",
			 CanopySimple::load_syntax);
    syntax.add ("Height", "cm", Syntax::Const, 
		"permanent height of vegetation.");
    alist.add ("Height", 80.0);
    syntax.add ("N_per_LAI", "kg N/ha/LAI", Syntax::Const,
		"N content as function of LAI.");
    alist.add ("N_per_LAI", 10.0);
    syntax.add ("DM_per_LAI", "t DM/ha/LAI", Syntax::Const,
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
    alist.add ("litter_am", AM::default_AM ());
    syntax.add_submodule("Root", alist, Syntax::State, "Root system.",
			 RootSystem::load_syntax);
    syntax.add ("root_DM", "T DM/ha", Syntax::Const, 
		"Permanent root drymatter.");
    alist.add ("root_DM", 2.0);
    syntax.add ("Albedo", "", Syntax::Const, 
		"Reflection factor.");
    alist.add ("Albedo", 0.2);
    Librarian<Vegetation>::add_type ("permanent", alist, syntax, &make);
  }
} VegetationPermanent_syntax;
