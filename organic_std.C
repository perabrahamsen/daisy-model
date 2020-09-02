// organic_std.C -- The default model for sil organic matter.
// 
// Copyright 1996-2002 Per Abrahamsen and Søren Hansen
// Copyright 2000-2002 KVL.
// Copyright 2006 Per Abrahamsen and KVL.
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

#include "organic.h"
#include "frame_submodel.h"
#include "frame_model.h"
#include "submodeler.h"
#include "log.h"
#include "am.h"
#include "om.h"
#include "som.h"
#include "smb.h"
#include "dom.h"
#include "domsorp.h"
#include "aom.h"
#include "clayom.h"
#include "soil.h"
#include "soilph.h"
#include "geometry.h"
#include "soil_water.h"
#include "soil_heat.h"
#include "chemistry.h"
#include "chemical.h"
#include "bioincorporation.h"
#include "abiotic.h"
#include "time.h"
#include "mathlib.h"
#include "plf.h"
#include "treelog.h"
#include "check_range.h"
#include "vcheck.h"
#include "gaussj.h"
#include "memutils.h"
#include "librarian.h"
#include "library.h"
#include "metalib.h"
#include "block_model.h"
#include "column.h"
#include <algorithm>
#include <numeric>
#include <fstream>
#include <sstream>
#include <time.h>

// Convertions
static const double g_per_cm2_to_kg_per_ha = (10000.0 * 10000.0) / 1000.0;
static const double g_per_cm2_per_h_to_kg_per_ha_per_y 
/**/ = g_per_cm2_to_kg_per_ha * 24.0 * 365.0;
static const double kg_per_ha_per_y_to_g_per_cm2_per_h
/**/ = 1.0 / g_per_cm2_per_h_to_kg_per_ha_per_y;

static void
validate_am (const std::vector <AM*>& am)
{
  std::vector<AOM*> added;
  for (int i = 0; i < am.size (); i++)
    am[i]->append_to (added);
  for (int i = 0; i < added.size (); i++)
    daisy_assert (std::isfinite (added[i]->initial_C_per_N));
}

struct OrganicStandard : public OrganicMatter
{
  // Content.
  const Geometry* log_geo;      // We do some optional work when logging;
  const bool active_underground; // True, iff turnover happens below rootzone.
  std::vector<bool> active_;     // Active cells.
  const double K_NH4;		// Immobilization rate of NH4.
  const double K_NO3;		// Immobilization rate of NO3.
  std::vector<double> CO2_slow_; // CO2 produced per time step from slow pools.
  std::vector<double> CO2_fast_; // Ditto from fast pools.
  const double CO2_threshold; // Turnover rate over which CO2 is fast.
  double top_CO2;		// CO2 produced on top of soil.
  std::vector <AM*> am;		// Added Organic Matter.
  const std::vector<SMB*> smb;	// Living Organic Matter.
  const std::vector<SOM*> som;	// Soil Organic Matter.
  std::vector<double> stored_SOM_C; // For store/restore SOM
  std::vector<double> stored_SOM_N; // "management" actions.
  const std::vector<DOM*> dom;	// Dissolved Organic Matter.
  const std::vector<Domsorp*> domsorp; // SOM <-> DOM transfer.
  struct Buffer
  {
    std::vector<double> C;      // Carbon.
    std::vector<double> N;      // Nitrogen.
    const double turnover_rate;	// Absorption.
    const int where;		// Which SOM pool does it end in?
    void output (Log& log) const;
    void tick (int i, double abiotic_factor, double N_soil, double& N_used,
	       const std::vector<SOM*>&);
    void mix (const Geometry&, double from, double to);
    void swap (const Geometry&, double from, double middle, double to);
    static void load_syntax (Frame&);
    void initialize (const Geometry& geo);
    Buffer (const FrameSubmodel& al);
  } buffer;
  const PLF heat_factor;
  const PLF water_factor;
  const PLF pH_factor;
  std::vector<double> abiotic_factor;
  std::unique_ptr<ClayOM> clayom;
  const std::vector<boost::shared_ptr<const PLF>/**/> smb_tillage_factor;
  const std::vector<boost::shared_ptr<const PLF>/**/> som_tillage_factor;
  const double min_AM_C;	// Minimal amount of C in an AM. [g/m²]
  const double min_AM_N;	// Minimal amount of N in an AM. [g/m²]
  Bioincorporation bioincorporation;
  class Initialization
  {
    // Parameters.
  public:
    const double input;		// Total C input. [kg C/ha/y]
    const double end;		// End of plowing layer. [cm]
  private:
    const std::vector<double> fractions; // C destiations. []
    const std::vector<double> efficiency; // Input efficiency. []
    std::vector<double> per_lay; // C input in numeric intervals. [g C/cm^3/h]
  public:
    const double T;		// Equilibrium temperature. [dg C]
    const double h;		// Equilibrium pressure. [cm]
    const int variable_pool;	// First pool not in equilibrium.
    const int variable_pool_2;	// Second pool not in equilibrium.
    const double background_mineralization; // Desired min. from SOM and SMB.
    const int SOM_limit_where;	// Pool used for limiting.
    const std::vector<double> SOM_limit_lower; // Lower limit to SOM partit.
    const std::vector<double> SOM_limit_upper; // Upper limit to SOM partit.
    const std::vector<int> debug_equations;
    const bool debug_rows;
    const bool debug_to_screen;
    const std::string top_summary;   // File name to print top summary.

    // Use.
  public:
    bool print_equations (int lay)
    { return std::find (debug_equations.begin (), debug_equations.end (), lay)
	!=  debug_equations.end (); }

    void find_input (std::vector<double>& destination, const int lay) const;
    double find_total_input (const int lay) const;

    // Create and Destroy.
  private:
    static int find_som_1 (const std::vector<SOM*>& som);
    static int find_som_2 (const std::vector<SOM*>& som);
    static bool check_alist (const Metalib&, const Frame&, Treelog&);
  public:
    static void load_syntax (Frame&);
    Initialization (const FrameSubmodel&, const Geometry& geo,
                    const Soil& soil, 
                    const Bioincorporation& bioincorporation, 
		    const std::vector<SOM*>& som, double T_avg);
    ~Initialization ();
  };

  // Log.
  std::vector<double> NO3_source;
  std::vector<double> NH4_source;
  double fertilized_N;
  double fertilized_C;
  double tillage_N_top;
  double tillage_C_top;
  std::vector<double> tillage_N_soil;
  std::vector<double> tillage_C_soil;

  // Utilities.
  static bool aom_compare (const AOM* a, const AOM* b);
  double total_N (const Geometry& ) const;
  double total_C (const Geometry& ) const;
  template<class DAOM>
  const double* find_abiotic (const DAOM& om, // AOM & DOM
                              const Soil& soil,
                              const SoilpH& soilph,
			      const SoilWater& soil_water, 
			      const SoilHeat& soil_heat,
			      const std::vector<double>& default_value,
			      std::vector<double>& scratch) const;
  const double* find_abiotic (const OM& om, // SOM & SMB
                              const Soil& soil,
                              const SoilpH& soilph,
  			      const SoilWater& soil_water, 
			      const SoilHeat& soil_heat,
			      const std::vector<boost::shared_ptr<const PLF>/**/> tillage_factor,
                              const std::vector<double>& tillage_age,
			      const int pool,
			      const std::vector<double>& default_value,
			      bool use_clay,
			      std::vector<double>& scratch) const;

  double water_turnover_factor (double h) const;
  double pH_turnover_factor (double pH) const;
  std::vector<double> clay_turnover_factor;
  std::vector<double> soil_turnover_factor;
  void input_from_am (std::vector<double>& destination, 
		      double T, double h, double pH, const int lay) const;
  double total_input_from_am (double T, double h, double pH, 
                              const int lay) const;
  double abiotic (const OM& om, double T, double h, double pH,
		  bool use_clay, int lay) const;
  static std::vector<double> SOM_limit_normalize (const std::vector<double>& limit, 
                                                  const std::vector<double>& fractions);
  void partition (const std::vector<double>& am_input, double total_input,
                  double T, double h, double pH,
		  int lay, double total_C, 
		  int variable_pool, int variable_pool_2, 
		  double background_mineralization, bool top_soil,
		  const std::vector<double>& SOM_default_fractions,
		  const std::vector<double>& SOM_C_per_N,
		  const std::vector<double>& SOM_limit_lower,
		  const std::vector<double>& SOM_limit_upper,
		  int SOM_limit_where,
		  std::vector<double>& SOM_results,
		  std::vector<double>& SMB_results,
                  double& delta_C,
                  double& delta_N,
		  const double dry_bulk_density,
		  Treelog& msg,
		  bool print_equations, bool print_rows,
		  bool debug_to_screen) const;
  void update_pools (const std::vector<double>& SOM_results,
		     const double total_C_per_N,
		     const std::vector<double>& SOM_C_per_N_goal,
		     const std::vector<double>& SMB_results, int lay);
  std::string top_summary (const Geometry& geo,
                           const Soil&, const SoilpH&, const Initialization&,
                           const double zone_delta_N, 
                           const double zone_delta_C) const;

  // Simulation.
  void clear ();
  void monthly (const Metalib&, const Geometry&, Treelog&);
  const std::vector<bool>& active () const;
  void tick (const Geometry& geo, const Soil& soil, const SoilpH&, 
             const SoilWater&, const SoilHeat&, 
             const std::vector<double>& tillage_age,
	     Chemistry&, double dt, Treelog& msg);
  void transport (const Units&,  const Geometry&,
                  const Soil&, const SoilWater&, const SoilHeat&, Treelog&);
  const std::vector<DOM*>& fetch_dom () const;
  void output (Log&) const;
  double top_DM () const;           // [kg DM/m^2]
  double CO2 (size_t i) const;	// [g C/cm³]
  double CO2_fast (size_t i) const;	// [g C/cm³]
  void mix (const Geometry&, const Soil&, const SoilWater&,
	    double from, double to, double penetration);
  void swap (const Geometry&, const Soil&, const SoilWater&, 
	     double from, double middle, double to);
  double AOM_C (const Geometry& geo, const double from, const double to) const
  {
    const size_t all_am_size = am.size ();
    std::vector<AOM*> added;
    for (size_t i = 0; i < all_am_size; i++)
      am[i]->append_to (added);

    double C = 0.0;
    const size_t all_added_size = added.size ();
    for (size_t i = 0; i < all_added_size; i++)
      C += added[i]->soil_C (geo, from, to);
    return C;
  }
  void store_SOM ();
  void restore_SOM ();

  const std::vector <AM*> get_am () const
  { return am; }

  // Communication with external model.
  double get_smb_c_at (size_t i) const; // [g C/cm³]

  // Create and Destroy.
  int som_pools () const;
  bool check (const Units&, const Geometry&,
              const Soil&, const SoilWater&, const SoilHeat&,
	      const Chemistry&, Treelog& err) const;
  bool check_am (const FrameModel& am, Treelog& err) const;
  void add (AM&);
  void fertilize (const Metalib&, const FrameModel&, const Geometry&, 
                  const Time&, Treelog&);
  void fertilize (const Metalib&, const FrameModel&, const Geometry&,
                  double from, double to, const Time&, Treelog&);
  void fertilize (const Metalib&, const FrameModel&, const Geometry&, 
                  const Volume&, const Time&, Treelog&);
  AM* find_am (symbol sort, symbol part) const;
  void initialize (const Metalib&, 
                   const Units&, const Frame&, const Geometry& geo,
                   const Soil&, const SoilpH&, 
                   const SoilWater&, const SoilHeat&,
		   double T_avg, Treelog&);
  OrganicStandard ();
  OrganicStandard (const OrganicStandard&);
  OrganicStandard& operator= (const OrganicStandard&);
  explicit OrganicStandard (const BlockModel&);
  ~OrganicStandard ();
};

void
OrganicStandard::Buffer::output (Log& log) const
{
  output_variable (C, log);
  output_variable (N, log);
} 

void
OrganicStandard::Buffer::tick (int i, double abiotic_factor,
                               double N_soil, double& N_used,
                               const std::vector<SOM*>& som)
{
  if (iszero (C[i]))
    return;

  double rate;
  if (C[i] < 1e-15)
    rate = 1.0;
  else 
    rate = std::min (turnover_rate * abiotic_factor, 0.1);
  
  double C_use;
  double N_produce;
  double N_consume;

  OM::turnover (C[i], N[i], som[where]->goal_C_per_N (i),
		N_soil - N_used, rate, 1.0,
		C_use, N_produce, N_consume);

  // Update C.
  som[where]->C[i] += C_use;
  C[i] -= C_use;
  daisy_assert (C[i] >= 0.0);
  
  // Update N.
  N_used += (N_consume - N_produce);
  som[where]->N[i] += N_consume;
  N[i] -= N_produce;
  daisy_assert (N[i] >= 0.0);
}

void 
OrganicStandard::Buffer::mix (const Geometry& geo, 
                              double from, double to)
{
  daisy_non_negative (C);
  geo.mix (C, from, to);
  daisy_non_negative (C);
  daisy_non_negative (N);
  geo.mix (N, from, to);
  daisy_non_negative (N);
}

void
OrganicStandard::Buffer::swap (const Geometry& geo,
                               double from,
                               double middle, 
                               double to)
{
  daisy_non_negative (C);
  geo.swap (C, from, middle, to);
  daisy_non_negative (C);
  daisy_non_negative (N);
  geo.swap (N, from, middle, to);
  daisy_non_negative (N);
}

void
OrganicStandard::Buffer::load_syntax (Frame& frame)
{
  const std::vector<double> empty_vector;
  frame.declare ("C", "g C/cm^3", Check::non_negative (),
	      Attribute::State, Attribute::SoilCells,
	      "Buffer carbon content.");
  frame.set ("C", empty_vector);
  frame.declare ("N", "g N/cm^3", Check::non_negative (), Attribute::State, Attribute::SoilCells,
	      "Buffer nitrogen content.");
  frame.set ("N", empty_vector);
  frame.declare ("turnover_rate", "h^-1", Check::fraction (), Attribute::Const,
	      "Turnover rate from buffer into SOM.\n\
Ignored if you specify 'turnover_halftime'.");
  frame.set ("turnover_rate", 1.0);
  frame.declare ("turnover_halftime", "h", Check::positive (),
	      Attribute::OptionalConst, 
	      "Turnover halftime from buffer into SOM.\n\
Overrules 'turnover_rate' if specified.");
  frame.declare_integer ("where", Attribute::Const,
	      "The SOM pool to move the buffer content into.\n\
The first and slow SOM pool is numbered '0', the second and faster\n\
is numbered '1'.");
  frame.set ("where", 1);
}

void 
OrganicStandard::Initialization
::find_input (std::vector<double>& destination, const int lay) const
{
  daisy_assert (destination.size () == fractions.size ());
  daisy_assert (destination.size () >= efficiency.size ());
  for (size_t i = 0; i < destination.size (); i++)
    {
      destination[i] = per_lay[lay] * fractions[i];
      // No abiotic factor since we this is already a rate.
      if (i < efficiency.size ())
        destination[i] *= efficiency[i];
    }
  daisy_non_negative (destination);
}

double
OrganicStandard::Initialization
::find_total_input (const int lay) const
{ return per_lay[lay]; }

bool 
OrganicStandard::Initialization::
/**/ check_alist (const Metalib&, const Frame& al, Treelog& msg)
{ 
  bool ok = true;
  if (al.check ("input"))
    {
      const double root = al.number ("root");
      const double bioinc = al.number ("bioinc");
      const double input = al.number ("input");

      if (root + bioinc > input)
	{
	  msg.error ("Root and bioinc input larger than total input");
	  ok = false;
	}
    }
  return ok;
}

void
OrganicStandard::Initialization::load_syntax (Frame& frame)
{
  frame.declare ("input", "kg C/ha/y", Check::non_negative (),
	      Attribute::OptionalConst, "\
Amount of carbon added to the organic matter system.\n\
\n\
If this is unspecifed, the input rate from the inital added matter\n\
pools will be used instead.");
  frame.declare_fraction ("fractions", Attribute::Const, Attribute::Variable, "\
Desitinations for AOM input.  The first numbers corresponds to each\n\
SMB pool, while the last number correspond to the SOM buffer.\n\
This is only used if you specify the input parameter.");
  frame.set_check ("fractions", VCheck::sum_equal_1 ());
  std::vector<double> fractions;
  fractions.push_back (0.0);
  fractions.push_back (1.0);
  fractions.push_back (0.0);
  frame.set ("fractions", fractions);
  frame.declare_fraction ("efficiency", Attribute::Const, Attribute::Variable, "\
The efficiency this pool can be digested by each of the SMB pools.\n\
This is only used if you specify the input parameter.");
  std::vector<double> efficiency;
  efficiency.push_back (0.5);
  efficiency.push_back (0.5);
  frame.set ("efficiency", efficiency);
  frame.declare ("root", "kg C/ha/y", Check::non_negative (), Attribute::Const, "\
Amount of carbon added to the organic matter system from dead roots.\n\
\n\
This is part of the total amount specified by the 'input' parameter.");
  frame.set ("root", 800.0); // According to HSV simulations.
  frame.declare ("dist", "cm", Check::positive (), Attribute::Const, "\
Distance to go down in order to decrease the root density to half the\n\
original.");
  frame.set ("dist", 7.0);
  frame.declare ("end", "cm", Check::negative (),
	      Attribute::OptionalConst, "Depth of non-root input.\n\
\n\
The input will distributes uniformly down to this size, after\n\
subtracting the part of the input allocated to the 'root' parameter.\n\
\n\
By default, the end of the first horizon will be used.");
  frame.declare ("bioinc", "kg C/ha/y", Check::non_negative (), Attribute::Const, "\
Amount of carbon added to the organic matter system from bioincorporation.\n\
\n\
This is part of the total amount specified by the 'input' parameter.");
  frame.set ("bioinc", 0.0);
  frame.declare ("T", "dg C", Temperature, Attribute::OptionalConst, "\
Temperature used for equilibrium.\n\
\n\
By default, the yearly average from the weather component will be used.");
  frame.declare ("h", "cm", Check::non_positive (), Attribute::Const, "\
Pressure used for equilibrium.");
  frame.set ("h", -100.0);
  frame.declare_integer ("variable_pool", Attribute::OptionalConst, "\
If neither the C content nor 'SOM_fractions' are specified, equilibrium is\n\
assumed for all SOM pools except the one specified by this parameter.\n\
If you set this to -1 (or any number nor corresponding to a SOM pool),\n\
equilibrium will be assumed for all pools, and the humus content\n\
specified by the horizon will be ignored.\n\
Note, the numbering is zero-based, so '0' specifies SOM1.\n\
By default, the slowest active pool will be used.");
  frame.declare_integer ("variable_pool_2", Attribute::OptionalConst, "\
If 'background_mineralization' is specified, this pool is no longer\n\
assumed to be in equilibrium.\n\
Note, the numbering is zero-based, so '0' specifies SOM1.\n\
By default, the second slowest active pool will be used.");
  static RangeII min_range (-1000.0, 1000.0);
  frame.declare ("background_mineralization", "kg N/ha/y", 
	      min_range, Attribute::OptionalConst, "\
The background mineralization is the mineralization from all SMB and\n\
SOM pools, but not from the AOM pools.\n\
\n\
If neither the C content of individual pools nor 'SOM_fractions' are\n\
speecified, the SOM and SMB pools will be initialized so all pools in\n\
the top soil (above 'end', usually the first horizon) are in\n\
equilibrium except those specified by 'variable_pool' and\n\
'variable_pool_2', usually SOM1 and SOM2.  These two will be\n\
initialized so the background mineralization will be the specified number.\n\
The subsoil is not affected by this parameter.\n\
\n\
If the background mineralization is unspecified, 'variable_pool_2' will be\n\
assumed to be in equilibrium instead.");
  frame.declare_integer ("SOM_limit_where", Attribute::Const, "\
This is the SOM pool that must be within the limits specified by\n\
'SOM_limit_lower' and 'SOM_limit_upper'.  Use negative number to disable.\n\
Note, the numbering is zero-based, so '0' specifies SOM1.");
  frame.set ("SOM_limit_where", 0);
  frame.declare_fraction ("SOM_limit_lower", Attribute::Const, Attribute::Variable, "\
Lower limit for for automatic SOM partitioning.\n\
\n\
The SOM pool specified by 'SOM_limit_where' must contain at least the\n\
fraction of the total SOM content given in this list, where the first\n\
number correspond to the SOM1 fraction, the second number to SOM2,\n\
etc.  If the fraction is below the one given in this list, the SOM\n\
partitioning in this list will be used instead.\n\
\n\
If the SOM partitioning have been specified directly, either by the\n\
'SOM_fractions' horizon parameter or by specifying the C content of\n\
each pool, this parameter will be ignored.  The limit is also ignore\n\
for soil layers below 'end'.");
  frame.set_check ("SOM_limit_lower", VCheck::sum_equal_1 ());
  std::vector<double> SOM_limit_lower;
  SOM_limit_lower.push_back (0.3);
  SOM_limit_lower.push_back (0.7);
  SOM_limit_lower.push_back (0.0);
  frame.set ("SOM_limit_lower", SOM_limit_lower);
  frame.declare_fraction ("SOM_limit_upper", Attribute::Const, Attribute::Variable, "\
Upper limit for for automatic SOM partitioning.\n\
Works like 'SOM_limit_lower'.");
  frame.set_check ("SOM_limit_upper", VCheck::sum_equal_1 ());
  std::vector<double> SOM_limit_upper;
  SOM_limit_upper.push_back (0.7);
  SOM_limit_upper.push_back (0.3);
  SOM_limit_upper.push_back (0.0);
  frame.set ("SOM_limit_upper", SOM_limit_upper);
  
  frame.declare_integer ("debug_equations", Attribute::Const, 
	      Attribute::Variable, "\
Print equations used for initialization for the specified intervals.");
  frame.set_empty ("debug_equations");
  frame.declare_boolean ("debug_rows", Attribute::Const, "\
Print summari information for each row.");
  frame.set ("debug_rows", true);
  frame.declare_boolean ("debug_to_screen", Attribute::Const, "\
If true, print debug information to screen, else to the 'daisy.log' file.");
  frame.set ("debug_to_screen", false);
  frame.declare_string ("top_summary", Attribute::OptionalConst, "\
Name of file to print a summary of the organic carbon and nitrogen\n\
content in the zone down to the 'end' parameter.\n\
If unspecified, no such file will be generated, but the summary will\n\
still be found in 'daisy.log'.");
}

int
OrganicStandard::Initialization::
/**/find_som_1 (const std::vector<SOM*>& som)
{
  // Find the slowest pool and its rate.
  int slowest = -1;
  double slowest_rate = 0.1;

  for (int i = 0; i < som.size (); i++)
    {
      const double rate = som[i]->turnover_rate;
      if (rate > 1e-90 && rate < slowest_rate)
	{
	  slowest = i;
	  slowest_rate = rate;
	}
    }
  return slowest;
}

int
OrganicStandard::Initialization::
/**/find_som_2 (const std::vector<SOM*>& som)
{
  // Find the slowest pool and its rate.
  const int slowest = find_som_1 (som);
  if (slowest < 0)
    return -2;
  daisy_assert (slowest < som.size ());
  const double slowest_rate = som[slowest]->turnover_rate;

  // Fnd the second slowest pool and its rate.
  int second = -1;
  double second_rate = 0.1;

  for (int i = 0; i < som.size (); i++)
    {
      const double rate = som[i]->turnover_rate;
      if (i != slowest && rate >= slowest_rate && rate < second_rate)
	{
	  second = i;
	  second_rate = rate;
	}
    }
  return second;
}

OrganicStandard::Initialization::
/**/ Initialization (const FrameSubmodel& al, const Geometry& geo,
                     const Soil& soil, 
                     const Bioincorporation& bioincorporation,
		     const std::vector<SOM*>& som, double T_avg)
  : input (al.number ("input", -1.0)),
    end (al.number ("end", soil.end_of_first_horizon ())),
    fractions (al.number_sequence ("fractions")),
    efficiency (al.number_sequence ("efficiency")),
    T (al.number ("T", T_avg)),
    h (al.number ("h")),
    variable_pool (al.check ("variable_pool")
		   ? al.integer ("variable_pool")
		   : find_som_1 (som)),
    variable_pool_2 (al.check ("variable_pool_2")
		     ? al.integer ("variable_pool_2")
		     : find_som_2 (som)),
    background_mineralization (al.number ("background_mineralization",
                                          -42.42e42)),
    SOM_limit_where (al.integer ("SOM_limit_where")),
    SOM_limit_lower (al.number_sequence ("SOM_limit_lower")),
    SOM_limit_upper (al.number_sequence ("SOM_limit_upper")),
    debug_equations (al.integer_sequence ("debug_equations")),
    debug_rows (al.flag ("debug_rows")),
    debug_to_screen (al.flag ("debug_to_screen")),
    top_summary (al.name ("top_summary", "").name ())
{ 
  if (input < 0)
    return;

  const size_t cell_size = geo.cell_size ();

  // Per Lay
  daisy_assert (per_lay.size () == 0);
  per_lay.insert (per_lay.end (), cell_size, 0.0);

  // Parameters.
  const double root = al.number ("root");
  const double bioinc = al.number ("bioinc");

  // Add top.
  daisy_assert (input >= root + bioinc);
  const double top = input - root - bioinc;
  geo.add_surface (per_lay, 0.0, end, 
                   top * kg_per_ha_per_y_to_g_per_cm2_per_h);

  // Add roots
  const double depth = soil.MaxRootingHeight ();
  const double k = M_LN2 / al.number ("dist");
  std::vector<double> density (cell_size, 0.0);
  for (size_t i = 0; i < soil.size (); i++)
    if (geo.cell_z (i) > depth)
      density[i] = k * exp (k * geo.cell_z (i));

  geo.add_surface (per_lay, density, 
                   root * kg_per_ha_per_y_to_g_per_cm2_per_h);

  // Add bioincorporation
  bioincorporation.add (geo, per_lay,
			bioinc * kg_per_ha_per_y_to_g_per_cm2_per_h);

  // Mix roots in top.
  geo.mix (per_lay, 0.0, end);
  
  daisy_non_negative (per_lay);
}

OrganicStandard::Initialization::~Initialization ()
{ }

bool 
OrganicStandard::aom_compare (const AOM* a, const AOM* b)
{
  double A = a->initial_C_per_N;
  if (approximate (A, OM::Unspecified)
      && a->N.size () > 0 && a->C.size () > 0 && a->N[0] > 0)
    A = a->C[0] / a->N[0];
  double B = b->initial_C_per_N;
  if (approximate (B, OM::Unspecified)
      && b->N.size () > 0 && b->C.size () > 0 && b->N[0] > 0)
    B = b->C[0] / b->N[0];

  return A < B;
}

double 
OrganicStandard::total_N (const Geometry& geo) const
{
  double result = geo.total_soil (buffer.N);

  for (size_t i = 0; i < smb.size (); i++)
    result += smb[i]->soil_N (geo);
  for (size_t i = 0; i < som.size (); i++)
    result += som[i]->soil_N (geo);
  for (size_t i = 0; i < dom.size (); i++)
    result += dom[i]->soil_N (geo);
  for (int i = 0; i < am.size (); i++)
    result += am[i]->total_N (geo);
  
  return result;
}

double 
OrganicStandard::total_C (const Geometry& geo) const
{
  double result = geo.total_soil (buffer.C);

  for (size_t i = 0; i < smb.size (); i++)
    result += smb[i]->soil_C (geo);
  for (size_t i = 0; i < som.size (); i++)
    result += som[i]->soil_C (geo);
  for (size_t i = 0; i < dom.size (); i++)
    result += dom[i]->soil_C (geo);
  for (int i = 0; i < am.size (); i++)
    result += am[i]->total_C (geo);
  
  return result;
}

double
OrganicStandard::water_turnover_factor (double h) const
{
  if (water_factor.size () > 0)
    return water_factor (h);
  
  return Abiotic::f_h (h);
}

double
OrganicStandard::pH_turnover_factor (double pF) const
{
  if (pH_factor.size () > 0)
    return pH_factor (pF);
  
  return 1.0;
}

void
OrganicStandard::Buffer::initialize (const Geometry& geo)
{ 
  const size_t size = geo.cell_size ();
  // Make sure the vectors are large enough.
  while (N.size () < size)
    N.push_back (0.0);
  daisy_non_negative (N);
  while (C.size () < size)
    C.push_back (0.0);
  daisy_non_negative (C);
}

OrganicStandard::Buffer::Buffer (const FrameSubmodel& al)
  : C (al.number_sequence ("C")),
    N (al.number_sequence ("N")),
    turnover_rate (al.check ("turnover_halftime")
		   ? halftime_to_rate (al.number ("turnover_halftime"))
		   : al.number ("turnover_rate")),
    where (al.integer ("where"))
{
  daisy_non_negative (N);
  daisy_non_negative (C);
  daisy_assert (turnover_rate >= 0.0);
  daisy_assert (turnover_rate <= 1.0);
}

void
OrganicStandard::output (Log& log) const
{
  static const symbol CO2_symbol ("CO2");
  if (log.check_leaf (CO2_symbol))
    {
      std::vector<double> CO2 (CO2_slow_);
      daisy_assert (CO2.size () == CO2_fast_.size ());
      for (size_t i =0; i < CO2.size(); i++)
	CO2[i] += CO2_fast_[i];
      output_variable (CO2, log);
    }
  output_value (CO2_fast_, "CO2_fast", log);
  output_variable (top_CO2, log);
  output_lazy (top_DM (), "top_DM", log);
  static const symbol total_N_symbol ("total_N");
  static const symbol total_C_symbol ("total_C");
  static const symbol humus_symbol ("humus");
  if (log.check_leaf (total_N_symbol)
      || log.check_leaf (total_C_symbol)
      || log.check_leaf (humus_symbol))
    {
      daisy_assert (log_geo);
      const Geometry& geo = *log_geo;
      const int size = geo.cell_size ();

      std::vector<double> total_N (size, 0.0);
      std::vector<double> total_C (size, 0.0);
      for (int i = 0; i < size; i++)
	{
	  for (size_t j = 0; j < smb.size (); j++)
	    {
	      total_C[i] += smb[j]->C[i];
	      total_N[i] += smb[j]->N[i];
	    }
	  for (size_t j = 0; j < som.size (); j++)
	    {
	      total_C[i] += som[j]->C[i];
	      total_N[i] += som[j]->N[i];
	    }
	  for (size_t j = 0; j < dom.size (); j++)
	    {
	      total_C[i] += dom[j]->C_at (i);
	      total_N[i] += dom[j]->N_at (i);
	    }
	  for (int j = 0; j < am.size (); j++)
	    {
	      total_C[i] += am[j]->C_at (i);
	      total_N[i] += am[j]->N_at (i);
	    }
	  total_C[i] += buffer.C[i];
	  total_N[i] += buffer.N[i];
	}
      output_variable (total_N, log);
      output_variable (total_C, log);
      if (log.check_leaf (humus_symbol))
        {
          static const double c_fraction_in_humus = 0.587;
          std::vector<double> humus;
          for (size_t i = 0; i < total_C.size (); i++)
            humus.push_back (total_C[i] / c_fraction_in_humus);
          output_variable (humus, log);
        }
    }
  output_variable (abiotic_factor, log);
  static const symbol am_symbol ("am");
  if (log.check_interior (am_symbol))
    {
      Log::Open open (log, am_symbol);
      for (std::vector<AM*>::const_iterator item = am.begin(); 
	   item != am.end();
	   item++)
	{
	  const symbol name = (*item)->real_name ();
	  if (log.check_entry (name, AM::component))
	    {
	      Log::NamedEntry named_entry (log, name, (*item)->objid,
					   (*item)->frame ());
	      (*item)->output (log);
	    }
	}
    }
  output_list (smb, "smb", log, SMB::component);
  output_list (som, "som", log, SOM::component);
  output_variable (stored_SOM_C, log);
  output_variable (stored_SOM_N, log);
  output_ordered (dom, "dom", log);
  output_list (domsorp, "domsorp", log, Domsorp::component);
  output_submodule (buffer, "buffer", log);
  output_submodule (bioincorporation, "Bioincorporation", log);
  output_variable (NO3_source, log);
  output_variable (NH4_source, log);
  output_variable (fertilized_N, log);
  output_variable (fertilized_C, log);
  output_variable (tillage_N_top, log);
  output_variable (tillage_C_top, log);
  output_variable (tillage_N_soil, log);
  output_variable (tillage_C_soil, log);
}

bool
OrganicStandard::check (const Units& units, const Geometry& geo, 
                        const Soil& soil, 
			const SoilWater& soil_water, const SoilHeat& soil_heat, 
			const Chemistry& chemistry, Treelog& msg) const
{
  Treelog::Open nest (msg, "OrganicStandard");
  bool ok = true;
  
  if (!chemistry.know (Chemical::NO3 ()))
    msg.warning ("NO3 not tracked, assuming unlimited supply");
  if (!chemistry.know (Chemical::NH4 ()))
    msg.warning ("NH4 not tracked, assuming unlimited supply");

  for (size_t i = 0; i < am.size (); i++)
    if (!am[i]->check (msg))
      ok = false;
  for (size_t i = 0; i < domsorp.size (); i++)
    if (!domsorp[i]->check (units, geo, soil, soil_water, soil_heat, 
			    dom.size (), som.size (), msg))
      ok = false;
  if (!clayom->check (smb, msg))
    ok = false;

  return ok;
}

void 
OrganicStandard::add (AM& om)
{ 
  for (size_t i = 0; i < am.size (); i++)
    daisy_assert (&om != am[i]);
  am.push_back (&om); 
  validate_am (am);
}

void 
OrganicStandard::fertilize (const Metalib& metalib, const FrameModel& frame, 
                            const Geometry& geo, 
                            const Time& now, Treelog& msg)
{ 
  AM& om = AM::create (metalib, frame, geo, now, msg);
  fertilized_N += om.total_N (geo) / geo.surface_area (); 
  fertilized_C += om.total_C (geo) / geo.surface_area ();
  add (om);
}

void 
OrganicStandard::fertilize (const Metalib& metalib, const FrameModel& frame,
                            const Geometry& geo,
                            const double from, const double to,
                            const Time& now, Treelog& msg)
{ 
  AM& om = AM::create (metalib, frame, geo, now, msg);
  fertilized_N += om.total_N (geo) / geo.surface_area (); 
  fertilized_C += om.total_C (geo) / geo.surface_area ();
  om.mix (geo, from, to, 1.0,
          tillage_N_top, tillage_C_top,
          tillage_N_soil, tillage_C_soil);
  add (om);
}

void 
OrganicStandard::fertilize (const Metalib& metalib, const FrameModel& frame,
                            const Geometry& geo, const Volume& volume,
                            const Time& now, Treelog& msg)
{ 
  AM& om = AM::create (metalib, frame, geo, now, msg);
  fertilized_N += om.total_N (geo) / geo.surface_area (); 
  fertilized_C += om.total_C (geo) / geo.surface_area ();
  om.mix (geo, volume, 1.0,
          tillage_N_top, tillage_C_top,
          tillage_N_soil, tillage_C_soil);
  add (om);
}

void 
OrganicStandard::clear ()
{
  fertilized_N = 0.0;
  fertilized_C = 0.0;
  tillage_N_top = 0.0;
  tillage_C_top = 0.0;
  fill (tillage_N_soil.begin (), tillage_N_soil.end (), 0.0);
  fill (tillage_C_soil.begin (), tillage_C_soil.end (), 0.0);
}

void
OrganicStandard::monthly (const Metalib& metalib, const Geometry& geo,
                          Treelog& msg)
{
  static const symbol am_symbol ("am");
  static const symbol cleanup_symbol ("cleanup");
  AM* remainder = find_am (am_symbol, cleanup_symbol);
  if (!remainder)
    {
      const Library& library = metalib.library (AOM::component);
      const std::vector<symbol>& names = AM::default_AM ();
      std::vector<boost::shared_ptr<const FrameModel>/**/> alists;
      for (size_t i = 0; i < names.size (); i++)
        {
          boost::shared_ptr<const FrameModel> model (new FrameModel (library.model (names[i]), Frame::parent_link));
          alists.push_back (model);
        }
      remainder = &AM::create (metalib, geo,
                               Time (1, 1, 1, 1), alists,
			       am_symbol, cleanup_symbol, AM::Locked, msg);
      add (*remainder);
    }

  validate_am (am);
  const int am_size = am.size ();
  std::vector<AM*> new_am;
  
  for (int i = 0; i < am_size; i++)
    {
      daisy_assert (am[i]);
      bool keep;
      
      if (am[i]->locked ())
	keep = true;
      else if (iszero (min_AM_C))
	if (iszero (min_AM_N))
	  // No requirement, keep it.
	  keep = true;
	else
	  // Only require N.
	  keep = (am[i]->total_N (geo) * (100.0 * 100.0) > min_AM_N);
      else
	if (iszero (min_AM_N))
	  // Only require C.
	  keep = (am[i]->total_C (geo) * (100.0 * 100.0) > min_AM_C);
	else 
	  // Require either N or C.
	  keep = (am[i]->total_N (geo) * (100.0 * 100.0) > min_AM_N
		  || am[i]->total_C (geo) * (100.0 * 100.0) > min_AM_C);
      
      if (keep)
	new_am.push_back (am[i]);
      else
	{
	  remainder->add (geo, *am[i]);
	  delete am[i];
	}
      am[i] = NULL;
    }
  validate_am (new_am);
  am = new_am;
  validate_am (am);
}

template <class DAOM>
const double*
OrganicStandard::find_abiotic (const DAOM& om,
                               const Soil& soil,
                               const SoilpH& soilph,
                               const SoilWater& soil_water, 
                               const SoilHeat& soil_heat,
                               const std::vector<double>& 
                               /**/ default_value,
                               std::vector<double>& scratch) const

{
  const bool use_om_heat = (om.heat_factor.size () > 0);
  const bool use_om_water = (om.water_factor.size () > 0);
  
  if (!use_om_heat && !use_om_water)
    return &default_value[0];
  
  const size_t cell_size = active_.size ();
  for (size_t i = 0; i < cell_size; i++)
    {
      if (!active_[i])
        continue;
      const double T = soil_heat.T (i);
      if (use_om_heat)
	scratch[i] = om.heat_factor (T);
      else
	scratch[i] = Abiotic::f_T0 (T);

      const double h = soil_water.h (i);
      if (use_om_water)
	scratch[i] *= om.water_factor (h);
      else
	scratch[i] *= water_turnover_factor (h);

      const double pH = soilph.pH (i);
      scratch[i] *= pH_turnover_factor (pH);
    }
  return &scratch[0];
}

const double*
OrganicStandard::find_abiotic (const OM& om,
                               const Soil& soil,
                               const SoilpH& soilph,
                               const SoilWater& soil_water, 
                               const SoilHeat& soil_heat,
                               const std::vector<boost::shared_ptr<const PLF>/**/> tillage_factor,
                               const std::vector<double>& tillage_age,
                               const int pool,
                               const std::vector<double>&
                               /**/ default_value,
                               bool use_clay,
                               std::vector<double>& scratch) const
{				// SOM & SMB
  const bool use_om_heat = (om.heat_factor.size () > 0);
  const bool use_om_water = (om.water_factor.size () > 0);
  const bool use_tillage = (tillage_factor.size () > pool);

  if (!use_om_heat && !use_om_water && !use_tillage)
    return &default_value[0];
  
  const size_t cell_size = active_.size ();

  if (use_om_heat || use_om_water)
    {
      for (size_t i = 0; i < cell_size; i++)
	{
          if (!active_[i])
            continue;
	  if (use_clay)
	    scratch[i] = clay_turnover_factor[i];
	  else
	    scratch[i] = soil_turnover_factor[i];

	  const double T = soil_heat.T (i);
	  if (use_om_heat)
	    scratch[i] *= om.heat_factor (T);
	  else
	    scratch[i] *= Abiotic::f_T0 (T);

	  const double h = soil_water.h (i);
	  if (use_om_water)
	    scratch[i] *= om.water_factor (h);
	  else
	    scratch[i] *= water_turnover_factor (h);

          const double pH = soilph.pH (i);
          scratch[i] *= pH_turnover_factor (pH);
	}
    }
  else
    scratch = default_value;

  if (use_tillage)
    for (size_t i = 0; i < cell_size; i++)
      if (active_[i])
        scratch[i] *= (*tillage_factor[pool]) (tillage_age[i]);

  return &scratch[0];
}

void 
OrganicStandard::tick (const Geometry& geo,
                       const Soil& soil,
                       const SoilpH& soilph,
                       const SoilWater& soil_water, 
                       const SoilHeat& soil_heat,
                       const std::vector<double>& tillage_age,
		       Chemistry& chemistry,
                       const double dt,
                       Treelog& msg)
{
  // Extract stuff.
  Chemical *const soil_NO3 = chemistry.know (Chemical::NO3 ())
    ? &chemistry.find (Chemical::NO3 ())
    : NULL;
  Chemical *const soil_NH4 = chemistry.know (Chemical::NH4 ())
    ? &chemistry.find (Chemical::NH4 ())
    : NULL;
  const size_t cell_size = geo.cell_size ();

  // Fluxify management data.
  fertilized_N /= dt;
  fertilized_C /= dt;
  tillage_N_top /= dt;
  tillage_C_top /= dt;
  daisy_assert (tillage_N_soil.size () == cell_size);
  daisy_assert (tillage_C_soil.size () == cell_size);
  for (size_t c = 0; c < cell_size; c++)
    {
      tillage_N_soil[c] /= dt;
      tillage_C_soil[c] /= dt;
    }

  // Prepare mass balance.
  const double old_N = total_N (geo);
  const double old_C = total_C (geo);

  // Create an array of all AM pools, sorted by their C_per_N.
  validate_am (am);
  const int all_am_size = am.size ();
  std::vector<AOM*> added;
  for (int i = 0; i < all_am_size; i++)
    am[i]->append_to (added);
  sort (added.begin (), added.end (), aom_compare);
  validate_am (am);
  
  // Clear logs.
  fill (CO2_slow_.begin (), CO2_slow_.end (), 0.0);
  fill (CO2_fast_.begin (), CO2_fast_.end (), 0.0);
  fill (NO3_source.begin (), NO3_source.end (), 0.0);
  fill (NH4_source.begin (), NH4_source.end (), 0.0);
  top_CO2 = 0.0;
  for (size_t j = 0; j < dom.size (); j++)
    dom[j]->clear ();

  // Setup arrays.
  std::vector<double> N_soil (cell_size, -42.42e42);
  std::vector<double> N_used (cell_size, -42.42e42);
  std::vector<double> clay_factor (cell_size, -42.42e42);
  std::vector<double> soil_factor (cell_size, -42.42e42);
  std::vector<double> tillage_factor (cell_size, -42.42e42);
  
  for (size_t i = 0; i < cell_size; i++)
    {
      if (!active_[i])
        continue;
      const double NH4 = soil_NH4 
        ? soil_NH4->M_primary (i) * K_NH4
        : 1.0;                  // Practically unlimited 1 [g/cm^3]
      const double NO3 = soil_NO3
        ? soil_NO3->M_primary (i) * K_NO3
        : 1.0;                  // Practically unlimited 1 [g/cm^3]

      N_soil[i] = NH4 + NO3;
      N_used[i] = 0.0;

      const double h = soil_water.h (i);
      daisy_assert (std::isfinite (h));
      const double T = soil_heat.T (i);
      const double heat = Abiotic::f_T0 (T);
      const double water = water_turnover_factor (h);
      const double pH = soilph.pH (i);
      const double pH_factor =  pH_turnover_factor (pH);
      abiotic_factor[i] = heat * water * pH_factor;
      clay_factor[i] = abiotic_factor[i] * clay_turnover_factor [i];
      soil_factor[i] = abiotic_factor[i] * soil_turnover_factor [i];
    }
  
  // Main processing.
  for (size_t j = 0; j < dom.size (); j++)
    {
      const double *const abiotic 
	= find_abiotic (*dom[j], soil, soilph, 
                        soil_water, soil_heat, soil_factor, 
                        tillage_factor);
      double *const CO2 = dom[j]->turnover_rate > CO2_threshold 
	? &CO2_fast_[0] 
	: &CO2_slow_[0];
      dom[j]->turnover (active_, abiotic, &N_soil[0], &N_used[0], CO2, smb,
                        dt);
    }
  for (size_t j = 0; j < smb.size (); j++)
    {
      const bool use_clay = clayom->smb_use_clay (j);
      const std::vector<double>& default_factor = 
	use_clay ? clay_factor : soil_factor;
      const double *const abiotic 
	= find_abiotic (*smb[j], soil, soilph, soil_water, soil_heat,
			smb_tillage_factor, tillage_age, j,
			default_factor, use_clay, tillage_factor);
      double *const CO2 = smb[j]->turnover_rate > CO2_threshold 
	? &CO2_fast_[0] 
	: &CO2_slow_[0];
      smb[j]->maintain (active_, abiotic, &N_used[0], CO2, dt);
      smb[j]->tick (active_, 
                    abiotic, &N_soil[0], &N_used[0], CO2, smb, som, dom, dt);
    }
  for (size_t j = 0; j < som.size (); j++)
    {
      const bool use_clay = clayom->som_use_clay (j);
      const std::vector<double>& default_factor = 
	use_clay ? clay_factor : soil_factor;
      const double *const abiotic 
	= find_abiotic (*som[j], soil, soilph, soil_water, soil_heat,
			som_tillage_factor, tillage_age, j,
			default_factor, use_clay, tillage_factor);
      double *const CO2 = som[j]->turnover_rate > CO2_threshold 
	? &CO2_fast_[0] 
	: &CO2_slow_[0];
      som[j]->tick (active_,
                    abiotic, &N_soil[0], &N_used[0], CO2, smb, som, dom, dt);
    }

  for (size_t j = 0; j < added.size (); j++)
    {
      const double *const abiotic 
	= find_abiotic (*added[j], soil, soilph, soil_water, soil_heat,
			soil_factor, tillage_factor);
      double *const CO2 = added[j]->turnover_rate > CO2_threshold 
	? &CO2_fast_[0] 
	: &CO2_slow_[0];
      added[j]->tick (active_, abiotic, &N_soil[0], &N_used[0], &CO2[0],
		      smb, &buffer.C[0], &buffer.N[0], dom, dt);
    }

  // Update buffer.
  for (size_t i = 0; i < cell_size; i++)
    if (active_[i])
      buffer.tick (i, soil_factor[i], N_soil[i], N_used[i], som);

  // Update source.
  for (size_t i = 0; i < cell_size; i++)
    {
      if (!active_[i])
        continue;
      
      const double NH4 = soil_NH4 
        ? soil_NH4->M_primary (i) * K_NH4
        : 0.0;
      daisy_assert (NH4 >= 0.0);

      if (N_used[i] > NH4)
	{
	  NH4_source[i] = -NH4;
	  NO3_source[i] = (NH4 - N_used[i]);
	  daisy_assert (NH4_source[i] <= 0.0);
	  daisy_assert (NO3_source[i] <= 0.0);
	}
      else
	{
	  NH4_source[i] = -N_used[i];
	  NO3_source[i] = 0.0;
	}
    }
  
  // Update soil solutes.
  if (soil_NO3)
    soil_NO3->add_to_transform_source (NO3_source);
  if (soil_NH4)
    soil_NH4->add_to_transform_source (NH4_source);

  // Biological incorporation.
  const double soil_T 
    = geo.content_hood (soil_heat, &SoilHeat::T, Geometry::cell_above);
  bioincorporation.tick (geo, am, soil_T, top_CO2, dt);

  // Mass balance.
  double N_to_DOM = 0.0;
  for (int j = 0; j < dom.size (); j++)
    N_to_DOM += dom[j]->N_source (geo) * dt;
  const double new_N = total_N (geo) + N_to_DOM;
  const double delta_N = old_N - new_N;
  const double N_source = geo.total_soil (NO3_source) 
    + geo.total_soil (NH4_source);

  if (!approximate (delta_N, N_source * dt)
      && !approximate (old_N, new_N, 1e-10))
    {
      std::ostringstream tmp;
      tmp << "BUG: OrganicStandard: delta_N != NO3 + NH4 [g N]\n"
          << delta_N << " != " << geo.total_soil (NO3_source) * dt
          << " + " << geo.total_soil (NH4_source) * dt;
      if (std::isnormal (N_source))
	tmp << " (error " 
            << fabs (delta_N / (N_source) - 1.0) * 100.0 << "%)";
      msg.error (tmp.str ());
    }
  double C_to_DOM = 0.0;
  for (int j = 0; j < dom.size (); j++)
    C_to_DOM += dom[j]->C_source (geo) * dt;
  const double new_C = total_C (geo) + C_to_DOM;
  const double delta_C = old_C - new_C;
  const double C_source 
    = geo.total_soil (CO2_slow_) + geo.total_soil (CO2_fast_)
    + top_CO2 * geo.surface_area ();
  
  if (!approximate (delta_C, C_source * dt)
      && !approximate (old_C, new_C, 1e-10))
    {
      std::ostringstream tmp;
      tmp << "BUG: OrganicStandard: "
	"delta_C != soil_CO2_slow + soil_CO2_fast + top_CO2 [g C]\n"
          << delta_C << " != " << geo.total_soil (CO2_slow_) * dt << " + " 
          << geo.total_soil (CO2_fast_) * dt << " + "
          << top_CO2 * geo.surface_area () * dt;
      msg.error (tmp.str ());
    }
}
      
void 
OrganicStandard::transport (const Units& units, const Geometry& geo, 
                            const Soil& soil, 
                            const SoilWater& soil_water, 
			    const SoilHeat& soil_heat,
                            Treelog& msg)
{
  for (size_t j = 0; j < domsorp.size (); j++)
    domsorp[j]->tick (units, geo, soil, soil_water, soil_heat, dom, som, msg);
}

void 
OrganicStandard::mix (const Geometry& geo, const Soil& soil,
                      const SoilWater& soil_water,
                      const double from, const double to, 
                      const double penetration)
{
  buffer.mix (geo, from, to);
  for (size_t i = 0; i < am.size (); i++)
    am[i]->mix (geo, from, to, penetration, 
                tillage_N_top, tillage_C_top, 
                tillage_N_soil, tillage_C_soil);
  for (size_t i = 1; i < smb.size (); i++)
    smb[i]->mix (geo, from, to, 
                 tillage_N_soil, tillage_C_soil);
  for (size_t i = 0; i < som.size (); i++)
    som[i]->mix (geo, from, to, 
                 tillage_N_soil, tillage_C_soil);
  for (size_t i = 0; i < dom.size (); i++)
    dom[i]->mix (geo, soil, soil_water, from, to);

  // Leave CO2 alone.
}

void
OrganicStandard::store_SOM ()
{
  stored_SOM_C.clear ();
  stored_SOM_N.clear ();
  for (size_t i = 0; i < som.size (); i++)
    {
      stored_SOM_C.insert(stored_SOM_C.end(),
                          som[i]->C.begin(), som[i]->C.end());
      stored_SOM_N.insert(stored_SOM_N.end(),
                          som[i]->N.begin(), som[i]->N.end());
    }
}

void
OrganicStandard::restore_SOM ()
{
  const size_t stored_size = stored_SOM_C.size ();
  daisy_assert (stored_size == stored_SOM_N.size ());
  size_t stored_index = 0;
  for (auto s : som)
    {
      const size_t soil_cells = s->N.size ();
      daisy_assert (s->N.size () == soil_cells);
      daisy_assert (stored_index + soil_cells <= stored_size);
      for (size_t i = 0; i < soil_cells; i++)
        {
          s->C[i] = stored_SOM_C[stored_index + i];
          s->N[i] = stored_SOM_N[stored_index + i];
        }
      stored_index += soil_cells;
    }
  daisy_assert (stored_index == stored_size);
}

double 
OrganicStandard::get_smb_c_at (size_t i) const // [g C/cm³]
{
  double total = 0.0;
  for (size_t j = 0; j < smb.size (); j++)
    {
      if (smb[j]->C.size () > i)
        total += smb[j]->C[i];
    }
  return total;
}

void 
OrganicStandard::swap (const Geometry& geo, const Soil& soil,
                       const SoilWater& soil_water,
                       const double from, const double middle, const double to)
{
  buffer.swap (geo, from, middle, to);
  for (size_t i = 0; i < am.size (); i++)
    am[i]->swap (geo, from, middle, to, 
                 tillage_N_soil, tillage_C_soil);
  for (size_t i = 1; i < smb.size (); i++)
    smb[i]->swap (geo, from, middle, to, 
                  tillage_N_soil, tillage_C_soil);
  for (size_t i = 0; i < som.size (); i++)
    som[i]->swap (geo, from, middle, to, 
                  tillage_N_soil, tillage_C_soil);
  for (size_t i = 0; i < dom.size (); i++)
    dom[i]->swap (geo, soil, soil_water, from, middle, to);
  // Leave CO2 alone.
}

AM* 
OrganicStandard::find_am (const symbol sort,
                          const symbol part) const
{
  for (size_t i = 0; i < am.size (); i++)
    if (am[i]->locked () 
	&& am[i]->crop_name () == sort 
	&& am[i]->crop_part_name () == part)
      return am[i];
  return NULL;
}

void
OrganicStandard::input_from_am (std::vector<double>& destination,
                                double T, double h, double pH,
                                const int lay) const
{
  // Calculate the input from all aom in soil layer lay to all destinations. 
  const size_t size = destination.size ();
  daisy_assert (size == smb.size () + 1);
  const size_t som_pool = size - 1;
  fill (destination.begin (), destination.end (), 0.0);

  // Loop over all AOM pools.
  validate_am (am);
  std::vector<AOM*> added;
  for (size_t i = 0; i < am.size (); i++)
    am[i]->append_to (added);

  for (size_t i = 0; i < added.size (); i++)
    if (added[i]->C.size () > lay)
      {
	daisy_assert (added[i]->fractions.size () == size);
	const double abiotic_factor 
	  = abiotic (*added[i], T, h, pH, false, lay);
	// For SMB pools.
	for (size_t pool = 0; pool < som_pool ; pool++)
	  destination[pool] += added[i]->C[lay] 
	    * added[i]->turnover_rate 
	    * added[i]->fractions[pool]
	    * added[i]->efficiency[pool]
	    * abiotic_factor;
	
	// For SOM buffer.
	destination[som_pool] += added[i]->C[lay] 
	  * added[i]->turnover_rate 
	  * added[i]->fractions[som_pool]
	  * abiotic_factor;
      }
  validate_am (am);
}

double
OrganicStandard::total_input_from_am (double T, double h, double pH,
                                      const int lay) const
{
  const size_t som_pool = smb.size ();

  // Loop over all AOM pools.
  validate_am (am);
  std::vector<AOM*> added;
  for (size_t i = 0; i < am.size (); i++)
    am[i]->append_to (added);

  double total = 0.0;
  for (size_t i = 0; i < added.size (); i++)
    if (added[i]->C.size () > lay)
      {
	const double abiotic_factor 
	  = abiotic (*added[i], T, h, pH, false, lay);
	// For SMB pools.
	for (size_t pool = 0; pool < som_pool ; pool++)
	  total += added[i]->C[lay] 
	    * added[i]->turnover_rate 
	    * added[i]->fractions[pool]
	    * abiotic_factor;
	
	// For SOM buffer.
	total += added[i]->C[lay] 
	  * added[i]->turnover_rate 
	  * added[i]->fractions[som_pool]
	  * abiotic_factor;
      }
  validate_am (am);
  return total;
}

				     
double
OrganicStandard::abiotic (const OM& om, double T, double h, double pH,
                          bool use_clay, int lay) const
{
  return ((om.heat_factor.size () > 0) 
	  ? om.heat_factor (T) 
	  : Abiotic::f_T0 (T))
    * ((om.water_factor.size () > 0)
       ? om.water_factor (h)
       : water_turnover_factor (h))
    * pH_turnover_factor (pH)
    * (use_clay ? clay_turnover_factor[lay] : soil_turnover_factor[lay]);
}

std::vector<double> 
OrganicStandard::SOM_limit_normalize
/**/ (const std::vector<double>& limit, const std::vector<double>& fractions)
{
  // If no fractions are specified, just use limit.
  bool unspecified = true;
  for (size_t i = 0; i < fractions.size (); i++)
    if (fractions[i] >= 0.0)
      unspecified = false;
  
  if (unspecified)
    return limit;

  std::vector<double> result (limit.size (), -1.0);

  // Set the forced values.
  double added = 0.0;
  double removed = 0.0;
  int missing = 0;
  for (size_t i = 0; i < fractions.size (); i++)
    if (fractions[i] >= 0.0)
      {
        result[i] = fractions[i];
        added += fractions[i];
        removed += limit[i];
      }
    else
      missing++;

  if (approximate (removed, 1.0))
    {
      // Distribute remaining SOM uniformly.
      daisy_assert (missing > 0);
      const double value = (1.0 - added) / (missing + 0.0);
      for (size_t i = 0; i < result.size (); i++)
        if (result[i] < 0.0)
          result[i] = value;
    }
  else
    {
      // Distribute remaining SOM proportionally.
      const double factor = (1.0 - added) / (1.0 - removed);
      for (size_t i = 0; i < result.size (); i++)
        if (result[i] < 0.0)
          result[i] = limit[i] * factor;
    }

  daisy_assert (approximate (accumulate (result.begin (), result.end (), 0.0),
                             1.0));
  return result;
}
				     
void
OrganicStandard::partition (const std::vector<double>& am_input,
                            const double total_input,
                            const double T, const double h, const double pH,
                            const int lay, const double total_C,
                            const int variable_pool,
                            const int variable_pool_2,
                            const double
                            /**/ background_mineralization,
                            bool top_soil,
                            const std::vector<double>& 
                            /**/ SOM_default_fractions,
                            const std::vector<double>& SOM_C_per_N,
                            const std::vector<double>&
                            /**/ SOM_limit_lower,
                            const std::vector<double>& 
                            /**/ SOM_limit_upper,
                            const int SOM_limit_where,
                            std::vector<double>& SOM_results,
                            std::vector<double>& SMB_results,
                            double& delta_C,
                            double& delta_N,
                            const double dry_bulk_density,
                            Treelog& msg,
                            const bool print_equations,
                            const bool print_rows,
                            const bool debug_to_screen) const
{
  // Find AOM values.
  validate_am (am);
  double total_am = 0.0;
  for (size_t i = 0; i < am.size (); i++)
    total_am += am[i]->C_at (lay);

  // We know the total humus, the yearly input and has been told the
  // relative sizes of the SOM pools.  We need to calculate the SMB
  // fractions.

  // Sizes.
  const size_t smb_size = smb.size ();
  const size_t som_size = som.size ();

  // The columns are:
  //  k = SMB1 SMB2 SOM1 SOM2 dSMB1 dSMB2 dSOM1 dSOM2
  const size_t smb_column = 0;
  const size_t som_column = smb_column + smb_size;
  const size_t dsmb_column = som_column + som_size;
  const size_t dsom_column = dsmb_column + smb_size;
  const size_t number_of_equations = dsom_column + som_size;

  // Calculated C content.
  double total = -42.42e42;

  // Messages.
  std::ostringstream table_string;
  if (lay == 0)
    {
      // Tag line.
      table_string << "lay\thumus\thumus\tinput\tinput\tAOM";
      for (size_t pool = 0; pool < smb_size; pool++)
	table_string << "\tSMB" << (pool + 1);
      for (size_t pool = 0; pool < som_size; pool++)
	table_string << "\tSOM" << (pool + 1);
      for (size_t pool = 0; pool < smb_size; pool++)
	table_string << "\tdSMB" << (pool + 1) << "\tdSMB" << (pool + 1);
      for (size_t pool = 0; pool < som_size; pool++)
	table_string << "\tdSOM" << (pool + 1) << "\tdSOM" << (pool + 1);
      table_string << "\n";
      // Dimension line.
      table_string << "\tkg C/ha/cm\t%\tkg C/ha/cm/y\t%\t%";
      for (size_t pool = 0; pool < smb_size; pool++)
	table_string << "\t%";
      for (size_t pool = 0; pool < som_size; pool++)
	table_string << "\t%";
      for (size_t pool = 0; pool < smb_size; pool++)
	table_string << "\tkg C/ha/cm/y\ty^-1";
      for (size_t pool = 0; pool < som_size; pool++)
	table_string << "\tkg C/ha/cm/y\ty^-1";
      table_string << "\n";
    }
  std::ostringstream equation_string;
  equation_string << "Equations:\n";
  bool error_found;

  // We try the default fractions first.
  std::vector<double> SOM_fractions = SOM_default_fractions;

  while (true)
    {
      GaussJordan matrix (number_of_equations);
      size_t equation = 0;

      // The SMB and SOM change equations have this format:
      //
      //     dXXXn = k1 SMB1 + k2 SMB2 + k3 SOM1 + k4 SOM2 + k5 AOM1 + k6 AOM2
      // => -k5 AOM1 - k6 AOM2) = k1 SMB1 + k2 SMB2 + k3 SOM1 + k4 SOM2 - dXXX

      // The SMB change equations.
      for (size_t pool = 0; pool < smb_size; pool++)
	{
	  // Add contributions from AM pools
	  if (am_input[pool] > 1e-30)
	    matrix.set_value (equation, -am_input[pool]);
      
	  // Add contributions from SMB pools.
	  for (size_t i = 0; i < smb_size; i++)
	    {
	      const double abiotic_factor = abiotic (*smb[i], T, h, pH,
						     clayom->smb_use_clay (i),
						     lay);
	      const double out = (i == pool)
		? ((smb[pool]->turnover_rate + smb[pool]->maintenance)
		   * abiotic_factor)
		: 0.0;
	      const double in = smb[i]->turnover_rate 
		* smb[i]->fractions[pool]
		* smb[i]->efficiency[pool]
		* abiotic_factor;

	      if (fabs (in - out) > 1e-100)
		matrix.set_entry (equation, smb_column + i, in - out);
	    }

	  // Add contributions from SOM pools.
	  for (size_t i = 0; i < som_size; i++)
	    {
	      const double abiotic_factor = abiotic (*som[i], T, h, pH,
						     clayom->som_use_clay (i),
						     lay);
	      const double in = som[i]->turnover_rate 
		* som[i]->fractions[pool]
		* som[i]->efficiency[pool]
		* abiotic_factor;
	  
	      if (in > 1e-100)
		matrix.set_entry (equation, som_column + i, in);
	    }

	  // dSMBn
	  matrix.set_entry (equation, dsmb_column + pool, -1.0);

	  equation++;
	}
      daisy_assert (equation == smb_size);

      // The SOM change equations.
      for (size_t pool = 0; pool < som_size; pool++)
	{
	  // Add contributions from AOM pools.
	  if (pool == buffer.where && am_input[smb_size] > 1e-30)
	    matrix.set_value (equation, -am_input[smb_size]);
      
	  // Add contributions from SMB pools.
	  for (size_t i = 0; i < smb_size; i++)
	    {
	      const double abiotic_factor = abiotic (*smb[i], T, h, pH,
						     clayom->smb_use_clay (i),
						     lay);
	      const double in = smb[i]->turnover_rate 
		* smb[i]->fractions[smb_size + pool]
		* abiotic_factor;
	  
	      if (in > 1e-100)
		matrix.set_entry (equation, smb_column + i, in);
	    }

	  // Add contributions from SOM pools.
	  for (size_t i = 0; i < som_size; i++)
	    {
	      const double abiotic_factor = abiotic (*som[i], T, h, pH,
						     clayom->som_use_clay (i), 
						     lay);
	      const double out = (i == pool)
		? (som[pool]->turnover_rate * abiotic_factor)
		: 0.0;
	      const double in = som[i]->turnover_rate 
		* som[i]->fractions[smb_size + pool]
		* abiotic_factor;

	      if (fabs (in - out) > 1e-100)
		matrix.set_entry (equation, som_column + i, in - out);
	    }
      
	  // dSOMn
	  matrix.set_entry (equation, dsom_column + pool, -1.0);
	  equation++;
	}
      daisy_assert (equation == smb_size + som_size);

      // Additional SMB equations.
      for (size_t pool = 0; pool < smb_size; pool++)
	{
	  if (smb[pool]->C.size () > lay)
	    {
	      // The SMB value equations
	      // k = SMBn
	      matrix.set_value (equation, smb[pool]->C[lay]);
	      matrix.set_entry (equation, smb_column + pool, 1.0);
	    }
	  else
	    {
	      // The SMB equilibrium equations
	      // 0 = dSMBn
	      // matrix.set_value (equation, 0.0);
	      matrix.set_entry (equation, dsmb_column + pool, 1.0);
	    }
	  equation++;
	}

      // Additional SOM equations.
      bool use_humus_equation = false;
      bool inert_pool_found = false;
      for (size_t pool = 0; pool < som_size; pool++)
	{
	  if (som[pool]->C.size () > lay)
	    {
	      // The SOM value equations
	      // k = SOMn
	      matrix.set_value (equation, som[pool]->C[lay]);
	      matrix.set_entry (equation, som_column + pool, 1.0);
	    }
	  else if (pool == variable_pool)
	    {
	      // The humus equation:
	      //
	      //    humus = SMB1 + SMB2 + SOM1 + SOM2 + AOM
	      // => humus - AOM = SMB1 + SMB2 + SOM1 + SOM2
	      //
	      matrix.set_value (equation, total_C - total_am);
	      for (size_t i = 0; i < smb_size; i++)
		matrix.set_entry (equation, smb_column + i, 1.0);
	      for (size_t i = 0; i < som_size; i++)
		matrix.set_entry (equation, som_column + i, 1.0);
	      use_humus_equation = true;
	    }
	  else if (variable_pool >= 0
                   && SOM_fractions.size () > 0
                   && (SOM_fractions.size () <= pool
                       || SOM_fractions[pool] >= 0))
	    {
	      // The SOM fractions equations:
	      // 
	      //    SOMi = Fi SOM and SOM = SOMi + + SOMn
	      // => SOMi = Fi (SOMi + + SOMn)
	      // => SOMi = Fi SOMi + + Fi SOMn
	      // => 0 = (Fi - 1) SOMi + + Fi SOMn
	      // 
	      daisy_assert (SOM_fractions.size () <= som.size ());

	      matrix.set_value (equation, 0.0);
	      const double fraction = (pool < SOM_fractions.size ())
		? SOM_fractions[pool]
		: 0.0;
      
	      for (size_t i = 0; i < som.size (); i++)
		{
		  if (i == pool)
		    matrix.set_entry (equation, som_column + i, 
				      fraction - 1.0);
		  else if (fraction > 1e-100)
		    matrix.set_entry (equation, som_column + i, fraction);
		}
	    }
	  else if (background_mineralization > -1e10
		   && pool == variable_pool_2)
	    {
	      // The background mineralization equation.
	      //
	      // dN = dSMB1 SMB1_N/C + + dSMBn SMBn_N/C 
	      //      + dSOM1 SOM1_N/C + + dSOMn SOMn_N/C
	      matrix.set_value (equation, -background_mineralization);
	      for (size_t i = 0; i < smb_size; i++)
		{
		  double N_per_C = 0.0;
		  if (smb[i]->C.size () > lay && smb[i]->N.size () > lay)
		    {
		      if (std::isnormal (smb[i]->C[lay]))
			N_per_C = smb[i]->N[lay] / smb[i]->C[lay];
		    }
		  else 
		    {
		      daisy_assert (smb[i]->initial_C_per_N > 0.0);
		      N_per_C = 1.0 / smb[i]->initial_C_per_N;
		    }
		  matrix.set_entry (equation, dsmb_column + i, N_per_C);
		}
	      daisy_assert (SOM_C_per_N.size () == som.size ());
	      for (size_t i = 0; i < som_size; i++)
		{
		  daisy_assert (std::isnormal (SOM_C_per_N[i]));
		  matrix.set_entry (equation, dsom_column + i,
				    1.0 / SOM_C_per_N[i]);
		}
	    }
	  else if (som[pool]->turnover_rate > 1e-100)
	    {
	      // The SOM equilibrium equations
	      // 
	      // 0 = dSOMn
	      // matrix.set_value (equation, 0.0);
	      matrix.set_entry (equation, dsom_column + pool, 1.0);
	    }
	  else if (top_soil)
	    {
	      // No inert humus in the plowing layer.
	      //
	      // 0 = SOMn
	      // matrix.set_value (equation, 0.0);
	      matrix.set_entry (equation, som_column + pool, 1.0);
	    }
          else if (variable_pool < 0)
	    {
	      // No inert humus if we have to find total humus ourselves.
	      //
	      // 0 = SOMn
	      // matrix.set_value (equation, 0.0);
	      matrix.set_entry (equation, som_column + pool, 1.0);
	    }
	  else if (inert_pool_found)
	    {
	      // We have no heuristic for partioning multiple inert pools.
	      msg.warning ("\
Cannot partition between multiple inert SOM pools.\n\
Setting additional pool to zero");
	      // 0 = SOMn
	      // matrix.set_value (equation, 0.0);
	      matrix.set_entry (equation, som_column + pool, 1.0);
	    }
	  else
	    {
	      // No net mineralization below the plowing layer.
	      //
	      // 0 = dSMB1 + dSMB2 + dSOM1 + dSOM2
	      // matrix.set_value (equation, 0.0);
	      for (size_t i = 0; i < smb_size; i++)
		matrix.set_entry (equation, dsmb_column + i, 1.0);
	      for (size_t i = 0; i < som_size; i++)
		matrix.set_entry (equation, dsom_column + i, 1.0);
	      inert_pool_found = true;
	    }
	  equation++;
	}

      daisy_assert (number_of_equations == equation);

      // Print out equations.
      for (size_t row = 0; row < number_of_equations; row++)
	{
	  equation_string << matrix.get_value (row) << " =";
	  bool first = true;

	  for (size_t pool = 0; pool < smb_size; pool++)
	    {
	      const double value = matrix.get_entry (row, smb_column + pool);
	      if (std::isnormal (value))
		{
		  if (first)
		    first = false;
		  else
		    equation_string << " +";

		  equation_string << " " << value << " SMB" << (pool + 1);
		}
	    }
	  for (size_t pool = 0; pool < som_size; pool++)
	    {
	      const double value = matrix.get_entry (row, som_column + pool);
	      if (std::isnormal (value))
		{
		  if (first)
		    first = false;
		  else
		    equation_string << " +";
		  equation_string << " " << value << " SOM" << (pool + 1);
		}
	    }
	  for (size_t pool = 0; pool < smb_size; pool++)
	    {
	      const double value = matrix.get_entry (row, dsmb_column + pool);
	      if (std::isnormal (value))
		{
		  if (first)
		    first = false;
		  else
		    equation_string << " +";
		  equation_string << " " << value << " dSMB" << (pool + 1);
		}
	    }
	  for (size_t pool = 0; pool < som_size; pool++)
	    {
	      const double value = matrix.get_entry (row, dsom_column + pool);
	      if (std::isnormal (value))
		{
		  if (first)
		    first = false;
		  else
		    equation_string << " +";
		  equation_string << " " << value << " dSOM" << (pool + 1);
		}
	    }
	  equation_string << "\n";
	}

      // Solve.
      try
	{
	  matrix.solve ();
	}
      catch (const char* error)
	{
	  if (!print_equations)
	    msg.error (equation_string.str ());
	  msg.error (error);
	  throw ("Organic matter initialization failed");
	}
      catch (const std::string& error)
	{
	  if (!print_equations)
	    msg.error (equation_string.str ());
	  msg.error (error);
	  throw ("Organic matter initialization failure");
	}

      // Check mass balance.
      total = total_am;
      for (size_t i = 0; i < smb_size + som_size; i++)
	total += matrix.result (i);
      daisy_assert (!use_humus_equation || approximate (total, total_C));

      static const double c_fraction_in_humus = 0.587;

      // Messages.
      table_string
	<< lay << "\t"
	<< total * g_per_cm2_to_kg_per_ha << "\t"
	<< 100.0 * total / (c_fraction_in_humus * dry_bulk_density) << "\t"
	<< total_input * g_per_cm2_per_h_to_kg_per_ha_per_y << "\t"
	<< 100.0 * ((total_input * g_per_cm2_per_h_to_kg_per_ha_per_y) 
		    / (total * g_per_cm2_to_kg_per_ha)) << "\t"
	<< 100.0 * total_am / total;
      
      for (size_t pool = 0; pool < smb_size; pool++)
	{
	  const double value = matrix.result (smb_column + pool);
	  table_string << "\t" << 100.0 * value / total;
	}
      for (size_t pool = 0; pool < som_size; pool++)
	{
	  const double value = matrix.result (som_column + pool);
	  table_string << "\t" << 100.0 * value / total;
	}
      for (size_t pool = 0; pool < smb_size; pool++)
	{
	  const double value = matrix.result (smb_column + pool) 
	    * g_per_cm2_to_kg_per_ha;
	  const double change = matrix.result (dsmb_column + pool)
	    * g_per_cm2_per_h_to_kg_per_ha_per_y;
	  const double rate = change / value;
	  table_string << "\t" << change << "\t" << rate;
	}
      for (size_t pool = 0; pool < som_size; pool++)
	{
	  const double value = matrix.result (som_column + pool) 
	    * g_per_cm2_to_kg_per_ha;
	  const double change = matrix.result (dsom_column + pool)
	    * g_per_cm2_per_h_to_kg_per_ha_per_y;
	  const double rate = change / value;
	  table_string << "\t" << change << "\t" << rate;
	}

      // Store results.
      for (size_t pool = 0; pool < smb_size; pool++)
	SMB_results[pool] = std::max (0.0, matrix.result (smb_column + pool));
      for (size_t pool = 0; pool < som_size; pool++)
	SOM_results[pool] = std::max (0.0, matrix.result (som_column + pool));


      // Store changes.
      delta_N = 0.0;
      delta_C = 0.0;
      for (size_t pool = 0; pool < smb_size; pool++)
        {
          double N_per_C = 0.0;
          if (smb[pool]->C.size () > lay && smb[pool]->N.size () > lay)
            {
              if (std::isnormal (smb[pool]->C[lay]))
                N_per_C = smb[pool]->N[lay] / smb[pool]->C[lay];
            }
          else 
            {
              daisy_assert (smb[pool]->initial_C_per_N > 0.0);
              N_per_C = 1.0 / smb[pool]->initial_C_per_N;
            }
          const double C = matrix.result (dsmb_column + pool);
          delta_C += C;
          delta_N += C * N_per_C;
        }
      daisy_assert (SOM_C_per_N.size () == som.size ());
      for (size_t pool = 0; pool < som_size; pool++)
        {
          const double C = matrix.result (dsom_column + pool);
          delta_C += C;
          daisy_assert (std::isnormal (SOM_C_per_N[pool]));
          delta_N += C / SOM_C_per_N[pool];
        }

      // Check mass balance, ignoring negative numbers.
      error_found 
	= !approximate (total_am 
			+ accumulate (SMB_results.begin (), SMB_results.end (),
				      0.0)
			+ accumulate (SOM_results.begin (), SOM_results.end (), 
				      0.0),
			total);

      // Forced SOM fractions, don't retry.
      bool SOM_fractions_fully_specified = SOM_fractions.size () != 0;
      for (size_t i = 0; i < SOM_fractions.size (); i++)
        if (SOM_fractions[i] < 0.0)
          SOM_fractions_fully_specified = false;
      if (SOM_fractions_fully_specified)
	break;

      if (!use_humus_equation)
	// We specified the slow pool directly, don't retry.
	break;

      // Check limits
      if (SOM_limit_where >= 0 && SOM_limit_where < som_size)
	{
	  daisy_assert (SOM_limit_lower.size () == som_size);
	  daisy_assert (SOM_limit_upper.size () == som_size);

          const std::vector<double> limit_lower 
            = SOM_limit_normalize (SOM_limit_lower, SOM_fractions);
          const std::vector<double> limit_upper 
            = SOM_limit_normalize (SOM_limit_upper, SOM_fractions);
          
	  const double upper = limit_upper[SOM_limit_where];
	  const double lower = limit_lower[SOM_limit_where];
	  daisy_assert (upper >= lower);
	  double total_SOM = 0.0;
	  for (int pool = 0; pool < som_size; pool++)
	    total_SOM += matrix.result (som_column + pool);
	  const double value
	    = matrix.result (som_column + SOM_limit_where) / total_SOM;
	  if (value < lower)
	    {
	      // Too low.
	      SOM_fractions = limit_lower;
	      table_string << "\tBelow SOM" 
                           << SOM_limit_where + 1 << " limit\n";
	      continue;
	    }
	  else if (upper < value)
	    {
	      // To high.
	      SOM_fractions = limit_upper;
	      table_string << "\tAbove SOM"
                           << SOM_limit_where + 1 << " limit\n";
	      continue;
	    }
	  // Just right.
	  daisy_assert (lower <= value && value <= upper);
	}      
      if (error_found && !top_soil)
	{
	  // No forced equilibrium possible, try without.
	  top_soil = true;
	  table_string << "\tTry as top soil.\n";
	  continue;
	}
      // Done for this row.
      break;
    }

  // Messages.
  if (print_rows || error_found)
    {
      if (error_found)
	msg.error (table_string.str ());
      else if (debug_to_screen)
	msg.message (table_string.str ());
      else
	msg.debug (table_string.str ());
    }
  if (print_equations)
    {
      if (debug_to_screen)
        msg.message (equation_string.str ());
      else
        msg.debug (equation_string.str ());
    }
  if (error_found)
    {
      if (!print_equations)
	msg.error (equation_string.str ());

      std::ostringstream tmp;
      tmp << "Can't initialize organic matter from input in layer " << lay;
      throw (std::string (tmp.str ()));
    }
}

std::string
OrganicStandard::top_summary (const Geometry& geo,
                              const Soil& soil,
                              const SoilpH& soilph,
                              const Initialization& init,
                              const double zone_delta_N, 
                              const double zone_delta_C) const
{
  std::ostringstream tmp;
    
  // Max number of AOM pools.
  validate_am (am);
  size_t aom_max_size = 0;
  for (size_t pool = 0; pool < am.size (); pool++)
    { 
      std::vector<AOM*> added;
      am[pool]->append_to (added);
      aom_max_size = std::max (aom_max_size, added.size ());
    }        
  validate_am (am);
    
  // Header line.
  tmp << "\t";
  for (size_t pool = 0; pool < som.size (); pool++)
    tmp << "SOM" << pool + 1 << "\t";
  for (size_t pool = 0; pool < smb.size (); pool++)
    tmp << "SMB" << pool + 1 << "\t";
  for (size_t pool = 0; pool < aom_max_size; pool++)
    tmp << "AOM" << pool + 1 << "\t";
  for (size_t pool = 0; pool < dom.size (); pool++)
    tmp << "DOM" << pool + 1 << "\t";
  tmp << "total\n";

  // C line
  tmp << "kg C/ha\t";
  double total_C = 0.0;
  for (size_t pool = 0; pool < som.size (); pool++)
    {
      const double C = som[pool]->soil_C (geo, 0.0, init.end);
      tmp << C * g_per_cm2_to_kg_per_ha << "\t";
      total_C += C;
    }
  for (size_t pool = 0; pool < smb.size (); pool++)
    {
      const double C = smb[pool]->soil_C (geo, 0.0, init.end);
      tmp << C * g_per_cm2_to_kg_per_ha << "\t";
      total_C += C;
    }
  for (size_t pool = 0; pool < aom_max_size; pool++)
    { 
      double C = 0.0;
      validate_am (am);
      for (size_t i = 0; i < am.size (); i++)
        { 
          std::vector<AOM*> added;
          am[i]->append_to (added);
          if (pool < added.size ())
            C += added[pool]->soil_C (geo, 0.0, init.end);
        }        
      validate_am (am);
      tmp << C * g_per_cm2_to_kg_per_ha << "\t";
      total_C += C;
    }
  for (size_t pool = 0; pool < dom.size (); pool++)
    {
      const double C = dom[pool]->soil_C (geo, 0.0, init.end);
      tmp << C * g_per_cm2_to_kg_per_ha << "\t";
      total_C += C;
    }
  tmp << total_C * g_per_cm2_to_kg_per_ha << "\n";

  // N line
  tmp << "kg N/ha\t";
  double total_N = 0.0;
  for (size_t pool = 0; pool < som.size (); pool++)
    {
      const double N = som[pool]->soil_N (geo, 0.0, init.end);
      tmp << N * g_per_cm2_to_kg_per_ha << "\t";
      total_N += N;
    }
  for (size_t pool = 0; pool < smb.size (); pool++)
    {
      const double N = smb[pool]->soil_N (geo, 0.0, init.end);
      tmp << N * g_per_cm2_to_kg_per_ha << "\t";
      total_N += N;
    }
  for (size_t pool = 0; pool < aom_max_size; pool++)
    { 
      double N = 0.0;
      validate_am (am);
      for (size_t i = 0; i < am.size (); i++)
        { 
          std::vector<AOM*> added;
          am[i]->append_to (added);
          if (pool < added.size ())
            N += added[pool]->soil_N (geo, 0.0, init.end);
        }        
      validate_am (am);
      tmp << N * g_per_cm2_to_kg_per_ha << "\t";
      total_N += N;
    }
  for (size_t pool = 0; pool < dom.size (); pool++)
    {
      const double N = dom[pool]->soil_N (geo, 0.0, init.end);
      tmp << N * g_per_cm2_to_kg_per_ha << "\t";
      total_N += N;
    }
  tmp << total_N * g_per_cm2_to_kg_per_ha << "\n\n";
    
  // Parameters.
  tmp << "Depth\t" << -init.end << "\tcm\n";
  tmp << "T\t" << init.T << "\tdg C\n";
  tmp << "h\t" << init.h << "\tcm\n";

  // Clay and input.
  double clay = 0.0;
  double input = 0.0;
  double volume = 0.0;
  for (size_t lay = 0; lay < geo.cell_size (); lay++)
    {
      if (geo.cell_z (lay) < init.end)
        continue;
      
      const double v = geo.cell_volume (lay);
      volume += v;
      clay += soil.clay (lay) * v;

      const double total_input 
        = (init.input >= 0)
        ? init.find_total_input (lay)
        : total_input_from_am (init.T, init.h, soilph.pH (lay), lay);
      input += total_input * v * g_per_cm2_per_h_to_kg_per_ha_per_y;
    }
  clay /= volume;
  tmp << "clay\t" << clay * 100 << "\t%\n";
  tmp << "Specified input\t";
  if (init.input < 0.0)
    tmp << "not specified";
  else
    tmp << init.input;
  tmp << "\tkg C/ha/y\n";
  tmp << "Specified background mineralization\t";
  if (init.background_mineralization < -1e10)
    tmp << "not specified";
  else
    tmp << init.background_mineralization;
  tmp << "\tkg N/ha/y\n";

  tmp << "Zone input\t" << input << "\tkg C/ha/y\n";
  tmp << "Zone background mineralization\t" << zone_delta_N 
      << "\tkg C/ha/y\n";
  tmp << "Zone humus change\t" << zone_delta_C << "\tkg C/ha/y\n";

  // Time.
  time_t start_time = time (NULL);
  tmp << "Time\t" << ctime (&start_time);
  return tmp.str ();
}

void
OrganicStandard::update_pools 
/**/ (const std::vector<double>& SOM_results,
      const double total_C_per_N,
      const std::vector<double>& SOM_C_per_N_goal,
      const std::vector<double>& SMB_results, 
      int lay)
{
  // Update SMB pools.
  double SMB_C = 0.0;
  double SMB_N = 0.0;
  for (size_t pool = 0; pool < smb.size (); pool++)
    {
      const double value = SMB_results[pool];
      if (smb[pool]->C.size () > lay)
	daisy_assert (approximate (smb[pool]->C[lay], value));
      else
	{
	  daisy_assert (smb[pool]->C.size () == lay);
	  smb[pool]->C.push_back (value);
	}
      SMB_C += smb[pool]->C[lay];
      if (smb[pool]->N.size () == lay)
	{
	  daisy_assert (smb[pool]->initial_C_per_N > 0);
	  const double N_content = smb[pool]->C[lay]
	    / smb[pool]->initial_C_per_N;
	  smb[pool]->N.push_back (N_content);
	}
      SMB_N += smb[pool]->N[lay];
      if (smb[pool]->C_per_N_goal.size () == lay)
	{
	  daisy_assert (smb[pool]->initial_C_per_N > 0);
	  smb[pool]->C_per_N_goal.push_back (smb[pool]->initial_C_per_N);
	}
    }

  // Update SOM pools.
  daisy_assert (SOM_C_per_N_goal.size () == som.size ());
  double SOM_C = 0.0;
  for (size_t pool = 0; pool < som.size (); pool++)
    {
      const double value = SOM_results[pool];
      if (som[pool]->C.size () > lay)
	daisy_assert (approximate (som[pool]->C[lay], value));
      else
	{
	  daisy_assert (som[pool]->C.size () == lay);
	  som[pool]->C.push_back (value);
	}
      SOM_C += som[pool]->C[lay];
      if (som[pool]->C_per_N_goal.size () == lay)
	som[pool]->C_per_N_goal.push_back (SOM_C_per_N_goal[pool]);
      if (som[pool]->N.size () == lay && total_C_per_N <= 0)
	som[pool]->N.push_back (som[pool]->C[lay] 
				/ som[pool]->C_per_N_goal[lay]);
    }

  // Update SOM N.
  if (total_C_per_N > 0)
    {
      double AOM_C = 0.0;
      double AOM_N = 0.0;
      for (size_t i = 0; i < am.size (); i++)
	{
	  AOM_C += am[i]->C_at (lay);
	  AOM_N += am[i]->N_at (lay);
	}
      const double total_C = AOM_C + SMB_C + SOM_C;
      const double total_N = total_C / total_C_per_N;
      const double SOM_N = total_N - AOM_N - SMB_N;
      if (SOM_N > 0.0)
        {
          const double SOM_C_per_N = SOM_C / SOM_N;
          
          for (size_t pool = 0; pool < som.size (); pool++)
            if (som[pool]->N.size () == lay)
              som[pool]->N.push_back (som[pool]->C[lay] / SOM_C_per_N);
        }
      else for (size_t pool = 0; pool < som.size (); pool++)
             if (som[pool]->N.size () == lay)
               som[pool]->N.push_back (0.0);
    }
}

void
OrganicStandard::initialize (const Metalib& metalib, 
                             const Units& units, const Frame& al,
                             const Geometry& geo,
                             const Soil& soil, 
                             const SoilpH& soilph,
                             const SoilWater& soil_water,
                             const SoilHeat& soil_heat,
                             double T_avg, Treelog& msg)
{ 
  Treelog::Open nest (msg, "OrganicStandard");

  // Save geometry for logging.
  daisy_assert (!log_geo);
  log_geo = &geo;

  // Sizes.
  const size_t cell_size = geo.cell_size ();
  const size_t smb_size = smb.size ();
  const size_t som_size = som.size ();
  const size_t dom_size = dom.size ();

  if (active_underground)
    active_.insert (active_.end (), cell_size, true);
  else
    {
      const double limit = std::min (-100.0, soil.MaxRootingHeight ());
      for (size_t lay = 0; lay < cell_size; lay++)
        active_.push_back (geo.cell_z (lay) >= limit);
    }
  daisy_assert (active_.size () == cell_size);

  // Check horizons.
  for (size_t lay = 0; lay < cell_size; lay++)
    {
      // We just return uninitialized if there is a mismatch, it will
      // be caught by Soil::check later.
      if (soil.SOM_C_per_N (lay).size () != som_size)
	return;
      const int SOM_fractions_size = soil.SOM_fractions (lay).size ();
      if (SOM_fractions_size > 0 && SOM_fractions_size != som_size)
	return;
    }

  // Production.
  CO2_slow_.insert (CO2_slow_.end (), cell_size, 0.0);
  CO2_fast_.insert (CO2_fast_.end (), cell_size, 0.0);
  NO3_source.insert (NO3_source.end (), cell_size, 0.0);
  NH4_source.insert (NH4_source.end (), cell_size, 0.0);
  
  // Clay affect of SMB turnover and maintenance.
  clayom->set_rates (soil, smb);

  // Clay and soil.
  for (size_t i = 0; i < cell_size; i++)
    {
      const double soil_factor = soil.turnover_factor (i);
      const double clay_factor = clayom->factor (soil.clay (i));
      soil_turnover_factor.push_back (soil_factor);
      clay_turnover_factor.push_back (soil_factor * clay_factor);
    }

  abiotic_factor.insert (abiotic_factor.end (), cell_size, 1.0);

  // Initialize AM.
  for (size_t i = 0; i < am.size (); i++)
    am[i]->initialize (geo, soil.MaxRootingHeight ());
  validate_am (am);

  // Biological incorporation.
  bioincorporation.initialize (geo, soil);
  static const symbol bio_symbol ("bio");
  static const symbol incorporation_symbol ("incorporation");
  AM* bioam = find_am (bio_symbol, incorporation_symbol);
  if (bioam)
    bioincorporation.set_am (bioam);
  else
    am.push_back (bioincorporation.create_am (metalib, geo, msg)); 
  validate_am (am);

  // Warnings in case of explicit SOM or SMB initialization.
  for (size_t pool = 0; pool < som_size; pool++)
    {
      std::ostringstream tmp;
      tmp << "som[" << pool << "]";
      Treelog::Open nest (msg, tmp.str ());
      if (som[pool]->C.size () > 0 && som[pool]->C.size () < cell_size)
	msg.warning ("C partially initialized.\n\
Using humus for remaining entries");
      if (som[pool]->N.size () > 0 && som[pool]->N.size () < cell_size)
	msg.warning ("N partially initialized.\n\
Using humus for remaining entries");
    }
  for (size_t pool = 0; pool < smb_size; pool++)
    {
      std::ostringstream tmp;
      tmp << "smb[" << pool << "]";
      Treelog::Open nest (msg, tmp.str ());
      if (smb[pool]->C.size () > 0 && smb[pool]->C.size () < cell_size)
	msg.warning ("C partially initialized.\n\
Using equilibrium for remaining entries");
      if (smb[pool]->N.size () > 0 && smb[pool]->N.size () < cell_size)
	msg.warning ("N partially initialized.\n\
Using initial C per N for remaining entries");
    }

  std::vector<double> total_C (cell_size, 0.0);
  double first_humus = 0.0;

  // Initialize C from layers, when available.
  if (al.check ("initial_SOM"))
    {
      const std::vector<boost::shared_ptr<const FrameSubmodel>/**/>& layers 
        = al.submodel_sequence ("initial_SOM");
      const double soil_end = geo.bottom ();
      double last = 0.0;
      for (size_t i = 0; i < layers.size (); i++)
	{
	  double end = layers[i]->number ("end");
	  double weight = layers[i]->number ("weight"); // kg C/m²
	  daisy_assert (weight > 0);
	  daisy_assert (end < last);
	  if (end < soil_end)
	    {
	      msg.warning ("\
An 'initial_SOM' layer in OrganicStandard ends below the last cell");
	      weight *= (last - soil_end) / (last - end);
	      end = soil_end;
	      i = layers.size ();
	    }
	  const double C = weight * 1000.0 / (100.0 * 100.0); // g C / cm²
	  geo.add_surface (total_C, last, end, C);
	  last = end;
	}
      first_humus = last;
    }

  // Initialize rest from humus.
  {
    for (size_t lay = 0; lay < cell_size; lay++)
      if (geo.cell_z (lay) < first_humus)
        total_C[lay] = soil.humus_C (lay);
  }
  // Partitioning.
  Initialization init (al.submodel ("init"),
                       geo, soil, bioincorporation, som, T_avg);
		       
  double total_delta_C = 0.0;
  double total_delta_N = 0.0;
  double zone_delta_C = 0.0;
  double zone_delta_N = 0.0;
  {
    std::vector<double> SOM_results (som_size, 0.0);
    std::vector<double> SMB_results (smb_size, 0.0);

    for (size_t lay = 0; lay < cell_size; lay++)
      {
        std::vector<double> am_input (smb.size () + 1);
        if (init.input >= 0)
          init.find_input (am_input, lay);
        else
          input_from_am (am_input, init.T, init.h, soilph.pH (lay), lay);
        const double total_input 
          = (init.input >= 0)
          ? init.find_total_input (lay)
          : total_input_from_am (init.T, init.h, soilph.pH (lay), lay);
      
        const bool top_soil = geo.cell_z (lay) > init.end;
        const double background_mineralization = 
          (top_soil && init.background_mineralization > -1e10)
          ? (init.background_mineralization 
             * kg_per_ha_per_y_to_g_per_cm2_per_h / -init.end)
          : -42.42e42;
        double delta_C;
        double delta_N;
        partition (am_input, total_input, init.T, init.h, soilph.pH (lay),
                   lay, total_C[lay], init.variable_pool, init.variable_pool_2,
                   background_mineralization, top_soil,
                   soil.SOM_fractions (lay), 
                   soil.SOM_C_per_N (lay),
                   init.SOM_limit_lower, init.SOM_limit_upper, 
                   (top_soil ? init.SOM_limit_where : -1),
                   SOM_results, SMB_results,
                   delta_C, delta_N,
                   soil.dry_bulk_density (lay),
                   msg,
                   init.print_equations (lay), init.debug_rows, 
                   init.debug_to_screen);
        if (top_soil)
          {
            total_delta_C += delta_C * geo.cell_volume (lay);
            total_delta_N += delta_N * geo.cell_volume (lay);
          }

        update_pools (SOM_results, soil.C_per_N (lay), 
                      soil.SOM_C_per_N (lay), SMB_results, lay);
      }
  }

  // Initialize buffer.
  buffer.initialize (geo);

  // Initialize DOM.
  for (size_t pool = 0; pool < dom_size; pool++)
    dom[pool]->initialize (geo, soil, soil_water, msg);

  // Initialize domsorp
  for (size_t i = 0; i < domsorp.size (); i++)
    domsorp[i]->initialize (units, geo, soil, soil_water, soil_heat, msg);

  // Initial value of "stored SOM".
  daisy_assert (stored_SOM_C.size () == stored_SOM_N.size ());
  if (stored_SOM_C.size () == 0)
    store_SOM ();
  
  // Summary.
  {
    Treelog::Open nest (msg, "Total soil summary");
    std::ostringstream total;
    total << "Expected humus change: " 
          << total_delta_C * g_per_cm2_per_h_to_kg_per_ha_per_y 
          << " [kg C/ha/y], ";
    const double all_C = this->total_C (geo);
    if (std::isnormal (all_C))
      total << total_delta_C / all_C << " [y^-1]";
    else
      total << "all new";
    total << ".\nExpected background mineralization: "
          << -total_delta_N * g_per_cm2_per_h_to_kg_per_ha_per_y 
          << " [kg N/ha/y].";
    if (init.debug_to_screen)
      msg.message (total.str ());
    else
      msg.debug (total.str ());  
  }

  // Print top summary.
  {
    const std::string summary 
      = top_summary (geo, soil, soilph, init, zone_delta_N, zone_delta_C);

    Treelog::Open nest (msg, "Top soil summary");
    if (init.debug_to_screen)
      msg.message (summary);
    else
      msg.debug (summary);
    
    if (init.top_summary != "")
      {
        std::ofstream out (init.top_summary.c_str ());
        out << summary;
        if (!out.good ())
          msg.error ("Problems writing to '" + init.top_summary + "'");
      }
  }  

  // Log variable.
  tillage_N_soil.insert (tillage_N_soil.end (), cell_size, 0.0);
  tillage_C_soil.insert (tillage_C_soil.end (), cell_size, 0.0);
}

OrganicStandard::OrganicStandard (const BlockModel& al)
  : OrganicMatter (al),
    log_geo (NULL),
    active_underground (al.flag ("active_underground")),
    K_NH4 (al.number ("K_NH4")),
    K_NO3 (al.number ("K_NO3")),
    CO2_threshold (al.number ("CO2_threshold")),
    top_CO2 (0.0),
    am (Librarian::build_vector<AM> (al, "am")),
    smb (Librarian::build_vector<SMB> (al, "smb")),
    som (Librarian::build_vector<SOM> (al, "som")),
    stored_SOM_C (al.number_sequence ("stored_SOM_C")),
    stored_SOM_N (al.number_sequence ("stored_SOM_N")),
    dom (map_submodel<DOM> (al, "dom")),
    domsorp (Librarian::build_vector<Domsorp> (al, "domsorp")),
    buffer (al.submodel ("buffer")),
    heat_factor (al.plf ("heat_factor")),
    water_factor (al.plf ("water_factor")),
    pH_factor (al.plf ("pH_factor")),
    clayom (Librarian::build_item<ClayOM> (al, "ClayOM")),
    smb_tillage_factor (al.plf_sequence ("smb_tillage_factor")),
    som_tillage_factor (al.plf_sequence ("som_tillage_factor")),
    min_AM_C (al.number ("min_AM_C")),
    min_AM_N (al.number ("min_AM_N")),
    bioincorporation (al.submodel ("Bioincorporation")),
    fertilized_N (0.0),
    fertilized_C (0.0),
    tillage_N_top (0.0),
    tillage_C_top (0.0)
{ }

OrganicStandard::~OrganicStandard ()
{
  sequence_delete (am.begin (), am.end ());
  sequence_delete (smb.begin (), smb.end ());
  sequence_delete (som.begin (), som.end ());
  sequence_delete (dom.begin (), dom.end ());
  sequence_delete (domsorp.begin (), domsorp.end ());
}

const std::vector<bool>& 
OrganicStandard::active () const
{ return active_; }

const std::vector<DOM*>&
OrganicStandard::fetch_dom () const
{ return dom; }

double 
OrganicStandard::top_DM () const
{
  double result = 0.0;
  for (int i = 0; i < am.size (); i++)
    result += am[i]-> top_DM ();
  return result * 10; // [g DM/cm^2] -> [kg DM/m^2]
}

double
OrganicStandard::CO2 (size_t i) const
{ return CO2_slow_[i] + CO2_fast_[i];
}

double
OrganicStandard::CO2_fast (size_t i) const
{
  daisy_assert (CO2_fast_.size () > i);
  return CO2_fast_[i];
}

bool
OrganicStandard::check_am (const FrameModel& am, Treelog& err) const
{
  bool ok = true;
  if (ok)
    {
      const std::vector<boost::shared_ptr<const FrameModel>/**/>& om_alist = am.model_sequence ("om");
      
      for (size_t i = 0; i < om_alist.size(); i++)
	{
	  std::ostringstream tmp;
	  tmp << "[" << i << "]";
	  Treelog::Open nest (err, tmp.str ());
          std::vector<double> fractions
            = om_alist[i]->number_sequence ("fractions");
          if (fractions.size () != smb.size () + 1 + dom.size ()
              && fractions.size () != smb.size () + 1)
            {
              std::ostringstream tmp;
              tmp << "You have " << fractions.size ()
                  << " fractions but " << smb.size ()
                  << " smb, 1 som buffer and " << dom.size () 
                  << " dom";
              err.entry (tmp.str ());
              ok = false;
            }
          double sum = accumulate (fractions.begin (), fractions.end (), 0.0);
          if (fabs (sum - 1.0) > 0.0001)
            {
              std::ostringstream tmp;
              tmp << "The sum of all fractions is " << sum;
              err.entry (tmp.str ());
              ok = false;
            }
	}
    }
  return ok;
}

int 
OrganicStandard::som_pools () const
{ return som.size (); }

static bool 
check_alist (const Metalib&, const Frame& al, Treelog& err)
{
  bool ok = true;

  const std::vector<boost::shared_ptr<const FrameModel>/**/>& am_alist = al.model_sequence ("am");
  const std::vector<boost::shared_ptr<const FrameModel>/**/>& smb_alist = al.model_sequence ("smb");
  const std::vector<boost::shared_ptr<const FrameModel>/**/>& som_alist = al.model_sequence ("som");
  const std::vector<boost::shared_ptr<const FrameSubmodel>/**/>& dom_alist 
    = al.submodel_sequence ("dom");

  for (size_t j = 0; j < am_alist.size(); j++)
    {
      std::ostringstream tmp;
      tmp << "am[" << j << "]";
      Treelog::Open nest (err, tmp.str ());
      bool am_ok = true;
      if (am_ok)
	{
	  bool om_ok = true;
	  const std::vector<boost::shared_ptr<const FrameModel>/**/>& om_alist
	    = am_alist[j]->model_sequence ("om");
	  for (size_t i = 0; i < om_alist.size(); i++)
	    {
	      std::ostringstream tmp;
	      tmp << "om[" << i << "]";
	      Treelog::Open nest (err, tmp.str ());
	      std::vector<double> fractions
		= om_alist[i]->number_sequence ("fractions");
	      if (fractions.size ()
		  != smb_alist.size () + 1 + dom_alist.size ()
		  && fractions.size () != smb_alist.size () + 1)
		{
		  std::ostringstream tmp;
		  tmp << "You have " << fractions.size ()
                      << " fractions but " << smb_alist.size ()
                      << " smb, 1 buffer and " << dom_alist.size ()
                      << " dom";
		  err.error (tmp.str ());
		  om_ok = false;
		}
	      double sum
		= accumulate (fractions.begin (), fractions.end (), 0.0);
	      if (fabs (sum - 1.0) > 0.0001)
		{
		  std::ostringstream tmp;
		  tmp << "The sum of all fractions is " << sum;
		  err.error (tmp.str ());
		  om_ok = false;
		}
	      if (!om_ok)
		am_ok = false;
	    }
	}
      if (!am_ok)
	ok = false;
    }

  if (smb_alist.size() < 1)
    {
      err.error ("You nees at least one smb pool.");
      ok = false;
    }
  for (size_t i = 0; i < smb_alist.size(); i++)
    {
      std::ostringstream tmp;
      tmp << "smb[" << i << "]";
      Treelog::Open nest (err, tmp.str ());
      bool om_ok = true;
      std::vector<double> fractions = smb_alist[i]->number_sequence ("fractions");
      if (fractions.size () 
	  != smb_alist.size () + som_alist.size () + dom_alist.size ())
	{
	  std::ostringstream tmp;
	  tmp << "You have " << fractions.size () << " fractions but " 
              << smb_alist.size () << " smb, " << som_alist.size ()
              << " som and " << dom_alist.size () << " dom";
	  err.error (tmp.str ());
	  om_ok = false;
	}
      std::vector<double> efficiency = smb_alist[i]->number_sequence ("efficiency");
      if (efficiency.size () != smb_alist.size ())
	{
	  std::ostringstream tmp;
	  tmp << "You have " << efficiency.size () << " efficiency but " 
              << smb_alist.size () << " smb";
	  err.error (tmp.str ());
	  om_ok = false;
	}
      double sum = accumulate (fractions.begin (), fractions.end (), 0.0);
      if (fabs (sum - 1.0) > 0.0001)
	{
	  std::ostringstream tmp;
	  tmp << "The sum of all fractions is " << sum;
	  err.error (tmp.str ());
	  om_ok = false;
	}
      if (!(OM::get_initial_C_per_N (*smb_alist[i]) > 0))
	{
	  err.error ("C/N unspecified");
	  om_ok = false;
	}
      if (smb_alist[i]->check ("C"))
	{
	  if (!smb_alist[0]->check ("C")
	      || (smb_alist[i]->number_sequence ("C").size ()
		  != smb_alist[0]->number_sequence ("C").size ()))
	    {
	      err.error ("You must specify C for all SMB pools, or for none");
	      ok = false;
	      break; 		// Avoid duplicate errors.
	    }
	}
      if (!om_ok)
	ok = false;
    }
  
  if (som_alist.size() < 1)
    {
      err.error ("You nees at least one som pool.");
      ok = false;
    }
  for (size_t i = 0; i < som_alist.size(); i++)
    {
      std::ostringstream tmp;
      tmp << "som[" << i << "]";
      Treelog::Open nest (err, tmp.str ());
      bool om_ok = true;
      std::vector<double> efficiency = som_alist[i]->number_sequence ("efficiency");
      if (efficiency.size () != smb_alist.size ())
	{
	  std::ostringstream tmp;
	  tmp << "You have " << efficiency.size () << " efficiency but " 
              << smb_alist.size () << " smb";
	  err.error (tmp.str ());
	  om_ok = false;
	}
      std::vector<double> fractions = som_alist[i]->number_sequence ("fractions");
      if (fractions.size () 
	  != smb_alist.size () + som_alist.size () + dom_alist.size ())
	{
	  std::ostringstream tmp;
	  tmp << "You have " << fractions.size () << " fractions but " 
              << smb_alist.size () << " smb, " << som_alist.size ()
              << " som and " << dom_alist.size () << " dom";
	  err.error (tmp.str ());
	  om_ok = false;
	}
      double sum = accumulate (fractions.begin (), fractions.end (), 0.0);
      if (fabs (sum - 1.0) > 0.0001)
	{
	  std::ostringstream tmp;
	  tmp << "The sum of all fractions is " << sum;
	  err.error (tmp.str ());
	  om_ok = false;
	}
      if (som_alist[i]->check ("C"))
	{
	  if (!som_alist[0]->check ("C")
	      || (som_alist[i]->number_sequence ("C").size ()
		  != som_alist[0]->number_sequence ("C").size ()))
	    {
	      err.error ("You must specify C for all SOM pools, or for none");
	      ok = false;
	      break; 		// Avoid duplicate errors.
	    }
	}
      if (!om_ok)
	ok = false;
    }
  for (size_t i = 0; i < dom_alist.size(); i++)
    {
      std::ostringstream tmp;
      tmp << "dom[" << i << "]";
      Treelog::Open nest (err, tmp.str ());
      bool om_ok = true;
      
      std::vector<double> efficiency = dom_alist[i]->number_sequence ("efficiency");
      if (efficiency.size () != smb_alist.size ())
	{
	  std::ostringstream tmp;
	  tmp << "You have " << efficiency.size () << " efficiency but " 
              << smb_alist.size () << " smb";
	  err.error (tmp.str ());
	  om_ok = false;
	}
      std::vector<double> fractions = dom_alist[i]->number_sequence ("fractions");
      if (fractions.size () != smb_alist.size ())
	{
	  std::ostringstream tmp;
	  tmp << "You have " << fractions.size () << " fractions but " 
              << smb_alist.size () << " smb";
	  err.error (tmp.str ());
	  om_ok = false;
	}
      double sum = accumulate (fractions.begin (), fractions.end (), 0.0);
      if (fabs (sum - 1.0) > 0.0001)
	{
	  std::ostringstream tmp;
	  tmp << "The sum of all fractions is " << sum;
	  err.error (tmp.str ());
	  om_ok = false;
	}
      if (!om_ok)
	ok = false;
    }
  const FrameSubmodel& init_alist = al.submodel ("init");
  if (init_alist.number_sequence ("SOM_limit_lower").size ()
      != som_alist.size ())
    {
      std::ostringstream tmp;
      tmp << "You have " 
          << init_alist.number_sequence ("SOM_limit_lower").size () 
          << " SOM_limit_lower but " << som_alist.size () << " som";
      err.error (tmp.str ());
      ok = false;
    }
  if (init_alist.number_sequence ("SOM_limit_upper").size ()
      != som_alist.size ())
    {
      std::ostringstream tmp;
      tmp << "You have " 
          << init_alist.number_sequence ("SOM_limit_upper").size () 
          << " SOM_limit_upper but " << som_alist.size () << " som";
      err.error (tmp.str ());
      ok = false;
    }
  return ok;
}

static struct OrganicStandardSyntax : DeclareModel
{
  bool used_to_be_a_submodel () const
  { return true; }

  Model* make (const BlockModel& al) const
  { return new OrganicStandard (al); }

  OrganicStandardSyntax () 
    : DeclareModel (OrganicMatter::component, "default", "\
Mineralization and immobilization in soil.")
  { }

  static void load_layer (Frame& frame)
  {
    frame.declare ("end", "cm", Check::negative (), Attribute::Const, "\
End point of this layer (a negative number).");
    frame.declare ("weight", "kg C/m^2", Check::positive (), Attribute::Const, "\
Organic carbon content of this layer.");
    frame.order ("end", "weight");
  }

  void load_frame (Frame& frame) const
  {
    Model::load_model (frame);
    frame.set_strings ("cite", "daisy-fertilizer", "daisy-somnew");
    frame.add_check (check_alist);
    frame.declare_boolean ("active_underground", Attribute::Const, "\
Set this flag to turn on mineralization below the root zone.");
    frame.set ("active_underground", false);
    frame.declare ("K_NH4", "h^-1", Check::fraction (), Attribute::Const, 
               "Maximal immobilization rate for ammonium.");
    frame.set ("K_NH4", 0.020833); // 0.5 / 24.
    frame.declare ("K_NO3", "h^-1", Check::fraction (), Attribute::Const, 
               "Maximal immobilization rate for nitrate.");
    frame.set ("K_NO3", 0.020833); // 0.5 / 24.
    frame.declare_submodule ("Bioincorporation", Attribute::State, "\
Biological incorporation of litter.",
                         Bioincorporation::load_syntax);
    frame.declare ("NO3_source", "g N/cm^3/h", Attribute::LogOnly, Attribute::SoilCells, "\
Mineralization this time step (negative numbers mean immobilization).");
    frame.declare ("NH4_source", "g N/cm^3/h", Attribute::LogOnly, Attribute::SoilCells, "\
Mineralization this time step (negative numbers mean immobilization).");
    frame.declare ("fertilized_N", "g N/cm^2/h", Attribute::LogOnly,
               "Amount of organic bound nitrogen applied.\n\
This includes nitrogen incorporated directly in the soil.");
    frame.declare ("fertilized_C", "g C/cm^2/h", Attribute::LogOnly,
               "Amount of organic bound carbon applied.\n\
This includes carbon incorporated directly in the soil.");
    frame.declare ("tillage_N_top", "g N/m^2/h", Attribute::LogOnly,
               "Amount of nitrogen added to surface during tillage.\n\
This is a negative number.");
    frame.declare ("tillage_C_top", "g C/m^2/h", Attribute::LogOnly,
               "Amount of carbon added to surface during tillage.\n\
This is a negative number.");
    frame.declare ("tillage_N_soil", "g N/cm^3/h", 
               Attribute::LogOnly, Attribute::SoilCells,
               "Amount of nitrogen added to soil during tillage.");
    frame.declare ("tillage_C_soil", "g C/cm^3/h",
               Attribute::LogOnly, Attribute::SoilCells,
               "Amount of carbon added to surface during tillage.");
    frame.declare ("humus", "g/cm^3", Attribute::LogOnly, Attribute::SoilCells,
               "Total organic matter in the soil layer.");
    frame.declare ("total_C", "g C/cm^3", Attribute::LogOnly, Attribute::SoilCells,
               "Total organic C in the soil layer.");
    frame.declare ("total_N", "g N/cm^3", Attribute::LogOnly, Attribute::SoilCells,
               "Total organic N in the soil layer.");
    frame.declare ("CO2", "g CO2-C/cm^3/h", Attribute::LogOnly, Attribute::SoilCells,
               "CO2 evolution in soil from all pools.");
    frame.declare ("CO2_fast", "g CO2-C/cm^3/h", Attribute::LogOnly, Attribute::SoilCells,
               "CO2 evolution in soil from pools faster than 'CO2_threshold'.");
    frame.declare ("CO2_threshold", "h^-1", Check::fraction (), Attribute::Const, "\
Turnover rate above which pools will contribute to 'CO2_fast'.");
    frame.set ("CO2_threshold", 1e-4); // SMB2 and default AOM pools.
    frame.declare ("top_CO2", "g CO2-C/cm^2/h", Attribute::LogOnly,
                   "CO2 evolution at surface.");
    frame.declare ("top_DM", "kg DM/m^2", Attribute::LogOnly,
                   "Added organic dry matter on top of surface.");
    frame.declare_object ("am", AM::component, 
                      Attribute::State, Attribute::Variable, 
                      "Added organic matter pools.");
    frame.set_strings ("am", "root");
    frame.declare_submodule ("buffer", Attribute::State,
                         "Buffer between AOM pools and SOM.",
                         OrganicStandard::Buffer::load_syntax);

    // Create defaults for som and smb.
    frame.declare_object ("smb", SMB::component, Attribute::State, Attribute::Variable, "\
Soil MicroBiomass pools.\n\
Initial value will be estimated based on equilibrium with AM and SOM pools.");
    frame.set_strings ("smb", "SMB-SLOW", "SMB-FAST");
    frame.declare_object ("som", SOM::component, Attribute::State, Attribute::Variable,
                      "Soil Organic Matter pools.");
    frame.set_strings ("som", "SOM-SLOW", "SOM-FAST", "SOM-INERT");
    frame.declare_submodule_sequence ("initial_SOM", Attribute::OptionalConst, "\
Layered initialization of soil SOM content.", load_layer);
    frame.declare ("stored_SOM_C", "g C/cm^3", Check::non_negative (),
                   Attribute::State, Attribute::Variable, "\
Stored SOM C content for all pools and all cells.\n\
By default, this is equivalent to the initial C content of the SOM pools.");
    frame.set_empty ("stored_SOM_C");
    frame.declare ("stored_SOM_N", "g N/cm^3", Check::non_negative (),
                   Attribute::State, Attribute::Variable, "\
Stored SOM N content for all pools and all cells.\n\
By default, this is equivalent to the initial N content of the SOM pools.");
    frame.set_empty ("stored_SOM_N");

    frame.declare_submodule_sequence ("dom", Attribute::State, 
                                  "Dissolved Organic Matter pools.",
                                  DOM::load_syntax);
    frame.set_empty ("dom");
    frame.declare_object ("domsorp", Domsorp::component, 
                      Attribute::State, Attribute::Variable, 
                      "Interchange between DOM and SOM pools.");
    frame.set_empty ("domsorp");

    frame.declare ("heat_factor", "dg C", Attribute::None (), Check::non_negative (),
               Attribute::Const,
               "Default heat factor, used if not specified by OM pool.");
    frame.set ("heat_factor", PLF::empty ());
    frame.declare ("water_factor", "cm", Attribute::None (), Check::non_negative (),
               Attribute::Const, "\
Default water potential factor, used if not specified by OM pool.\n\
If the PLF is empty, a build-in PLF of pF will be used instead.\n\
It is 0.6 at pF < 0, 1.0 at 1.5 < pF < 2.5, and 0 at pF > 6.5.");
    frame.set ("water_factor", PLF::empty ());
    frame.declare ("pH_factor", "pH", Attribute::None (), 
                   Check::non_negative (), Attribute::Const, "\
Soil pH influence on organic matter turnover. By default, pH is ignored.");
    frame.set ("pH_factor", PLF::empty ());
    frame.declare ("abiotic_factor", Attribute::None (), 
               Attribute::LogOnly, Attribute::SoilCells,
               "Product of current heat and water factors."); 
    frame.declare_object ("ClayOM", ClayOM::component, "Clay effect model.");
    frame.set ("ClayOM", "old");
    frame.declare ("smb_tillage_factor", "d", Attribute::None (), 
               Check::non_negative (), Attribute::Const, Attribute::Variable,
               "Tillage influence on turnover rates for each SMB pool.\n\
If no value is given, tillage will have no influence.");
    frame.set_empty ("smb_tillage_factor");
    frame.declare ("som_tillage_factor", "d", Attribute::None (), 
               Check::non_negative (), Attribute::Const, Attribute::Variable,
               "Tillage influence on SOM turnover rates for each SOM pool.\n\
If no value is given, tillage will have no influence.");
    frame.set_empty ("som_tillage_factor");

    frame.declare ("min_AM_C", "g C/m^2", Check::non_negative (), Attribute::Const, 
               "Minimal amount of carbon in AOM ensuring it is not removed.");
    frame.set ("min_AM_C", 0.5);
    //  We require 5 kg C / Ha in order to keep an AM pool.
    frame.declare ("min_AM_N", "g N/m^2", Check::non_negative (), Attribute::Const, 
                   "Minimal amount of nitrogen in AOM ensuring it is not removed.");
    // We require ½ kg N / Ha in order to keep an AM pool.
    frame.set ("min_AM_N", 0.05);
    frame.declare_submodule ("init", Attribute::Const, "\
Parameters for initialization of the SOM and SMB pools.\n\
\n\
If the C content of all the pools have been specified explicitly, use\n\
those values.  Otherwise, get the total C content from either the\n\
'initial_SOM' parameter if specified, or else from the humus content\n\
specified in the soil horizons.\n\
\n\
If 'SOM_fractions' has been specified, the pools will be initialized\n\
assuming the SMB pools are in equilibrium.  Otherwise, also SOM pools\n\
expect the first will be assumed to be in equilibrium as well.",
                         OrganicStandard::Initialization::load_syntax);
  }
} OrganicStandard_syntax;

// organic_std.C ends here.
