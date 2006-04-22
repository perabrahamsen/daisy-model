// organic_matter.C
// 
// Copyright 1996-2002 Per Abrahamsen and Søren Hansen
// Copyright 2000-2002 KVL.
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

#include "organic_matter.h"
#include "syntax.h"
#include "alist.h"
#include "submodeler.h"
#include "log.h"
#include "am.h"
#include "om.h"
#include "som.h"
#include "smb.h"
#include "dom.h"
#include "aom.h"
#include "soil.h"
#include "geometry.h"
#include "soil_water.h"
#include "soil_NH4.h"
#include "soil_NO3.h"
#include "soil_heat.h"
#include "bioincorporation.h"
#include "time.h"
#include "mathlib.h"
#include "plf.h"
#include <sstream>
#include "submodel.h"
#include "treelog.h"
#include "check_range.h"
#include "vcheck.h"
#include "gaussj.h"
#include "memutils.h"
#include <algorithm>
#include <numeric>
#include <fstream>
#include <time.h>

using namespace std;

// Convertions
static const double g_per_cm2_to_kg_per_ha = (10000.0 * 10000.0) / 1000.0;
static const double g_per_cm2_per_h_to_kg_per_ha_per_y 
= g_per_cm2_to_kg_per_ha * 24.0 * 365.0;
static const double kg_per_ha_per_y_to_g_per_cm2_per_h
= 1.0 / g_per_cm2_per_h_to_kg_per_ha_per_y;

struct OrganicMatter::Implementation
{
  // Content.
  const bool active_underground; // True, iff turnover happens below rootzone.
  const bool active_groundwater; // True, iff turnover happens in groundwater.
  const double K_NH4;		// Immobilization rate of NH4.
  const double K_NO3;		// Immobilization rate of NO3.
  vector<double> CO2_slow;	// CO2 produced per time step from slow pools.
  vector<double> CO2_fast;	// Ditto from fast pools.
  const double CO2_threshold; // Turnover rate over which CO2 is fast.
  double top_CO2;		// CO2 produced on top of soil.
  vector <AM*> am;		// Added Organic Matter.
  const vector<SMB*> smb;	// Living Organic Matter.
  const vector<SOM*> som;	// Soil Organic Matter.
  const vector<DOM*> dom;	// Dissolved Organic Matter.
  const vector<Domsorp*> domsorp; // SOM <-> DOM transfer.
  struct Buffer
  {
    vector<double> C;			// Carbon.
    vector<double> N;			// Nitrogen.
    const double turnover_rate;	// Absorption.
    const int where;		// Which SOM pool does it end in?
    void output (Log& log) const;
    void tick (int i, double abiotic_factor, double N_soil, double& N_used,
	       const vector<SOM*>&);
    void mix (const Geometry&, double from, double to);
    void swap (const Geometry&, double from, double middle, double to);
    static void load_syntax (Syntax& syntax, AttributeList& alist);
    void initialize (const Geometry& geo);
    Buffer (const AttributeList& al);
  } buffer;
  const PLF heat_factor;
  const PLF water_factor;
  vector<double> abiotic_factor;
  auto_ptr<ClayOM> clayom;
  vector<double> tillage_age;
  const vector<const PLF*> smb_tillage_factor;
  const vector<const PLF*> som_tillage_factor;
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
    const vector<double> fractions; // C destiations. []
    const vector<double> efficiency; // Input efficiency. []
    vector<double> per_lay;	// C input in numeric intervals. [g C/cm^3/h]
  public:
    const double T;		// Equilibrium temperature. [dg C]
    const double h;		// Equilibrium pressure. [cm]
    const int variable_pool;	// First pool not in equilibrium.
    const int variable_pool_2;	// Second pool not in equilibrium.
    const double background_mineralization; // Desired min. from SOM and SMB.
    const int SOM_limit_where;	// Pool used for limiting.
    const vector<double> SOM_limit_lower; // Lower limit to SOM partitioning.
    const vector<double> SOM_limit_upper; // Upper limit to SOM partitioning.
    const vector<int> debug_equations;
    const bool debug_rows;
    const bool debug_to_screen;
    const string top_summary;   // File name to print top summary.

    // Use.
  public:
    bool print_equations (int lay)
    { return find (debug_equations.begin (), debug_equations.end (), lay)
	!=  debug_equations.end (); }

    void find_input (vector<double>& destination, const int lay) const;
    double find_total_input (const int lay) const;

    // Create and Destroy.
  private:
    static int find_som_1 (const vector<SOM*>& som);
    static int find_som_2 (const vector<SOM*>& som);
    static bool check_alist (const AttributeList&, Treelog&);
  public:
    static void load_syntax (Syntax&, AttributeList&);
    Initialization (const AttributeList&, const Geometry& geo,
                    const Soil& soil, 
                    const Bioincorporation& bioincorporation, 
		    const vector<SOM*>& som, double T_avg);
    ~Initialization ();
  };

  // Log.
  vector<double> NO3_source;
  vector<double> NH4_source;
  double fertilized_N;
  double fertilized_C;
  double tillage_N_top;
  double tillage_C_top;
  vector<double> tillage_N_soil;
  vector<double> tillage_C_soil;

  // Utilities.
  static bool aom_compare (const AOM* a, const AOM* b);
  double total_N (const Geometry& ) const;
  double total_C (const Geometry& ) const;

  // Simulation.
  void add (AM&);
  void fertilize (const AttributeList&, const Geometry&);
  void fertilize (const AttributeList&, const Geometry&,
                  double from, double to);
  void clear (); 
  void monthly (const Geometry&);
  template<class DAOM>
  const double* find_abiotic (const DAOM& om, // AOM & DOM
			      const int size, 
			      const SoilWater& soil_water, 
			      const SoilHeat& soil_heat,
			      const vector<double>& default_value,
			      vector<double>& scratch) const;
  const double* find_abiotic (const OM& om, // SOM & SMB
			      const int size, 
			      const SoilWater& soil_water, 
			      const SoilHeat& soil_heat,
			      const vector<const PLF*> tillage_factor,
			      const int pool,
			      const vector<double>& default_value,
			      bool use_clay,
			      vector<double>& scratch) const;
  size_t active_size (const Geometry& geo,
                      const Soil&, const SoilWater&) const;
  void tick (const Geometry& geo,
             const Soil&, const SoilWater&, const SoilHeat&,
	     SoilNO3&, SoilNH4&, Treelog& msg);
  void transport (const Soil&, const SoilWater&, Treelog&);
  void mix (const Geometry&, const Soil&, const SoilWater&, 
	    double from, double to, double penetration,
	    const Time& time);
  void swap (const Geometry&, const Soil&, const SoilWater&, 
	     double from, double middle, double to,
	     const Time& time);
  void output (Log&, const Geometry&) const;
  bool check (const Soil& soil, Treelog& err) const;

  double heat_turnover_factor (double T) const;
  double water_turnover_factor (double h) const;
  vector<double> clay_turnover_factor;
  vector<double> soil_turnover_factor;

  // Communication with external model.
  double get_smb_c_at (unsigned int i) const // g C/cm³]
    {
      double total = 0.0;
      for (unsigned int j = 0; j < smb.size (); j++)
	{
	  if (smb[j]->C.size () > i)
	    total += smb[j]->C[i];
	}
      return total;
    }
  // Create & Destroy.
  AM* find_am (symbol sort, symbol part) const;
  void input_from_am (vector<double>& destination, 
		      double T, double h, const int lay) const;
  double total_input_from_am (double T, double h, const int lay) const;
  double abiotic (const OM& om, double T, double h, 
		  bool use_clay, int lay) const;
  static vector<double> SOM_limit_normalize (const vector<double>& limit, 
                                             const vector<double>& fractions);
  void partition (const vector<double>& am_input, double total_input,
                  double T, double h, 
		  int lay, double total_C, 
		  int variable_pool, int variable_pool_2, 
		  double background_mineralization, bool top_soil,
		  const vector<double>& SOM_default_fractions,
		  const vector<double>& SOM_C_per_N,
		  const vector<double>& SOM_limit_lower,
		  const vector<double>& SOM_limit_upper,
		  int SOM_limit_where,
		  vector<double>& SOM_results,
		  vector<double>& SMB_results,
                  double& delta_C,
                  double& delta_N,
		  const double dry_bulk_density,
		  Treelog& msg,
		  bool print_equations, bool print_rows,
		  bool debug_to_screen) const;
  void update_pools (const vector<double>& SOM_results,
		     const double total_C_per_N,
		     const vector<double>& SOM_C_per_N_goal,
		     const vector<double>& SMB_results, int lay);
  string top_summary (const Geometry& geo,
                      const Soil&, const Initialization&,
                      const double zone_delta_N, 
                      const double zone_delta_C) const;
  void initialize (const AttributeList&, const Geometry& geo,
                   const Soil&, const SoilWater&,
		   double T_avg, Treelog& err);
  Implementation (Block&);
  ~Implementation ();
};

void
OrganicMatter::Implementation::Buffer::output (Log& log) const
{
  output_variable (C, log);
  output_variable (N, log);
} 

void
OrganicMatter::Implementation::Buffer::tick (int i, double abiotic_factor,
					     double N_soil, double& N_used,
					     const vector<SOM*>& som)
{
  if (C[i] == 0.0)
    return;

  double rate;
  if (C[i] < 1e-15)
    rate = 1.0;
  else 
    rate = min (turnover_rate * abiotic_factor, 0.1);
  
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
OrganicMatter::Implementation::Buffer::mix (const Geometry& geo, 
					    double from, double to)
{
  assert_non_negative (C);
  geo.mix (C, from, to);
  assert_non_negative (C);
  assert_non_negative (N);
  geo.mix (N, from, to);
  assert_non_negative (N);
}

void
OrganicMatter::Implementation::Buffer::swap (const Geometry& geo,
					     double from,
					     double middle, 
					     double to)
{
  assert_non_negative (C);
  geo.swap (C, from, middle, to);
  assert_non_negative (C);
  assert_non_negative (N);
  geo.swap (N, from, middle, to);
  assert_non_negative (N);
}

void
OrganicMatter::Implementation::Buffer::load_syntax (Syntax& syntax,
						    AttributeList& alist)
{
  const vector<double> empty_vector;
  syntax.add ("C", "g C/cm^3", Check::non_negative (),
	      Syntax::State, Syntax::Sequence,
	      "Buffer carbon content.");
  alist.add ("C", empty_vector);
  syntax.add ("N", "g N/cm^3", Check::non_negative (), Syntax::State, Syntax::Sequence,
	      "Buffer nitrogen content.");
  alist.add ("N", empty_vector);
  syntax.add ("turnover_rate", "h^-1", Check::fraction (), Syntax::Const,
	      "Turnover rate from buffer into SOM.\n\
Ignored if you specify 'turnover_halftime'.");
  alist.add ("turnover_rate", 1.0);
  syntax.add ("turnover_halftime", "h", Check::positive (),
	      Syntax::OptionalConst, 
	      "Turnover halftime from buffer into SOM.\n\
Overrules 'turnover_rate' if specified.");
  syntax.add ("where", Syntax::Integer, Syntax::Const,
	      "The SOM pool to move the buffer content into.\n\
The first and slow SOM pool is numbered '0', the second and faster\n\
is numbered '1'.");
  alist.add ("where", 1);
}

void 
OrganicMatter::Implementation::Initialization
::find_input (vector<double>& destination, const int lay) const
{
  daisy_assert (destination.size () == fractions.size ());
  daisy_assert (destination.size () >= efficiency.size ());
  for (unsigned int i = 0; i < destination.size (); i++)
    {
      destination[i] = per_lay[lay] * fractions[i];
      // No abiotic factor since we this is already a rate.
      if (i < efficiency.size ())
        destination[i] *= efficiency[i];
    }
  assert_non_negative (destination);
}

double
OrganicMatter::Implementation::Initialization
::find_total_input (const int lay) const
{ return per_lay[lay]; }

bool 
OrganicMatter::Implementation::Initialization::
/**/ check_alist (const AttributeList& al, Treelog& msg)
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
OrganicMatter::Implementation::Initialization::
/**/ load_syntax (Syntax& syntax, AttributeList& alist)
{
  syntax.add ("input", "kg C/ha/y", Check::non_negative (),
	      Syntax::OptionalConst, "\
Amount of carbon added to the organic matter system.\n\
\n\
If this is unspecifed, the input rate from the inital added matter\n\
pools will be used instead.");
  syntax.add_fraction ("fractions", Syntax::Const, Syntax::Sequence, "\
Desitinations for AOM input.  The first numbers corresponds to each\n\
SMB pool, while the last number correspond to the SOM buffer.\n\
This is only used if you specify the input parameter.");
  syntax.add_check ("fractions", VCheck::sum_equal_1 ());
  vector<double> fractions;
  fractions.push_back (0.0);
  fractions.push_back (1.0);
  fractions.push_back (0.0);
  alist.add ("fractions", fractions);
  syntax.add_fraction ("efficiency", Syntax::Const, Syntax::Sequence, "\
The efficiency this pool can be digested by each of the SMB pools.\n\
This is only used if you specify the input parameter.");
  vector<double> efficiency;
  efficiency.push_back (0.5);
  efficiency.push_back (0.5);
  alist.add ("efficiency", efficiency);
  syntax.add ("root", "kg C/ha/y", Check::non_negative (), Syntax::Const, "\
Amount of carbon added to the organic matter system from dead roots.\n\
\n\
This is part of the total amount specified by the 'input' parameter.");
  alist.add ("root", 800.0); // According to HSV simulations.
  syntax.add ("dist", "cm", Check::positive (), Syntax::Const, "\
Distance to go down in order to decrease the root density to half the\n\
original.");
  alist.add ("dist", 7.0);
  syntax.add ("end", "cm", Check::negative (),
	      Syntax::OptionalConst, "Depth of non-root input.\n\
\n\
The input will distributes uniformly down to this size, after\n\
subtracting the part of the input allocated to the 'root' parameter.\n\
\n\
By default, the end of the first horizon will be used.");
  syntax.add ("bioinc", "kg C/ha/y", Check::non_negative (), Syntax::Const, "\
Amount of carbon added to the organic matter system from bioincorporation.\n\
\n\
This is part of the total amount specified by the 'input' parameter.");
  alist.add ("bioinc", 0.0);
  syntax.add ("T", "dg C", Temperature, Syntax::OptionalConst, "\
Temperature used for equilibrium.\n\
\n\
By default, the yearly average from the weather component will be used.");
  syntax.add ("h", "cm", Check::non_positive (), Syntax::Const, "\
Pressure used for equilibrium.");
  alist.add ("h", -100.0);
  syntax.add ("variable_pool", Syntax::Integer, Syntax::OptionalConst, "\
If neither the C content nor 'SOM_fractions' are specified, equilibrium is\n\
assumed for all SOM pools except the one specified by this parameter.\n\
If you set this to -1 (or any number nor corresponding to a SOM pool),\n\
equilibrium will be assumed for all pools, and the humus content\n\
specified by the horizon will be ignored.\n\
Note, the numbering is zero-based, so '0' specifies SOM1.\n\
By default, the slowest active pool will be used.");
  syntax.add ("variable_pool_2", Syntax::Integer, Syntax::OptionalConst, "\
If 'background_mineralization' is specified, this pool is no longer\n\
assumed to be in equilibrium.\n\
Note, the numbering is zero-based, so '0' specifies SOM1.\n\
By default, the second slowest active pool will be used.");
  static RangeII min_range (-1000.0, 1000.0);
  syntax.add ("background_mineralization", "kg N/ha/y", 
	      min_range, Syntax::OptionalConst, "\
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
  syntax.add ("SOM_limit_where", Syntax::Integer, Syntax::Const, "\
This is the SOM pool that must be within the limits specified by\n\
'SOM_limit_lower' and 'SOM_limit_upper'.  Use negative number to disable.\n\
Note, the numbering is zero-based, so '0' specifies SOM1.");
  alist.add ("SOM_limit_where", 0);
  syntax.add_fraction ("SOM_limit_lower", Syntax::Const, Syntax::Sequence, "\
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
  syntax.add_check ("SOM_limit_lower", VCheck::sum_equal_1 ());
  vector<double> SOM_limit_lower;
  SOM_limit_lower.push_back (0.3);
  SOM_limit_lower.push_back (0.7);
  SOM_limit_lower.push_back (0.0);
  alist.add ("SOM_limit_lower", SOM_limit_lower);
  syntax.add_fraction ("SOM_limit_upper", Syntax::Const, Syntax::Sequence, "\
Upper limit for for automatic SOM partitioning.\n\
Works like 'SOM_limit_lower'.");
  syntax.add_check ("SOM_limit_upper", VCheck::sum_equal_1 ());
  vector<double> SOM_limit_upper;
  SOM_limit_upper.push_back (0.7);
  SOM_limit_upper.push_back (0.3);
  SOM_limit_upper.push_back (0.0);
  alist.add ("SOM_limit_upper", SOM_limit_upper);
  
  syntax.add ("debug_equations", Syntax::Integer, Syntax::Const, 
	      Syntax::Sequence, "\
Print equations used for initialization for the specified intervals.");
  alist.add ("debug_equations", vector<int> ());
  syntax.add ("debug_rows", Syntax::Boolean, Syntax::Const, "\
Print summari information for each row.");
  alist.add ("debug_rows", true);
  syntax.add ("debug_to_screen", Syntax::Boolean, Syntax::Const, "\
If true, print debug information to screen, else to the 'daisy.log' file.");
  alist.add ("debug_to_screen", false);
  syntax.add ("top_summary", Syntax::String, Syntax::OptionalConst, "\
Name of file to print a summary of the organic carbon and nitrogen\n\
content in the zone down to the 'end' parameter.\n\
If unspecified, no such file will be generated, but the summary will\n\
still be found in 'daisy.log'.");
}

int
OrganicMatter::Implementation::Initialization::
/**/find_som_1 (const vector<SOM*>& som)
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
OrganicMatter::Implementation::Initialization::
/**/find_som_2 (const vector<SOM*>& som)
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

OrganicMatter::Implementation::Initialization::
/**/ Initialization (const AttributeList& al, const Geometry& geo,
                     const Soil& soil, 
                     const Bioincorporation& bioincorporation,
		     const vector<SOM*>& som, double T_avg)
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
    top_summary (al.name ("top_summary", ""))
{ 
  if (input < 0)
    return;

  // Per Lay
  daisy_assert (per_lay.size () == 0);
  per_lay.insert (per_lay.end (), soil.size (), 0.0);

  // Parameters.
  const double root = al.number ("root");
  const double bioinc = al.number ("bioinc");

  // Add top.
  daisy_assert (input >= root + bioinc);
  const double top = input - root - bioinc;
  geo.add (per_lay, 0.0, end, top * kg_per_ha_per_y_to_g_per_cm2_per_h);

  // Add roots
  const double depth = soil.MaxRootingDepth ();
  const double k = M_LN2 / al.number ("dist");
  vector<double> density (soil.size (), 0.0);
  for (unsigned int i = 0; 
       i < soil.size () && geo.z (i) > depth;
       i++)
    {
      density[i] = k * exp (k * geo.z (i));
    }
  geo.add (per_lay, density, root * kg_per_ha_per_y_to_g_per_cm2_per_h);

  // Add bioincorporation
  bioincorporation.add (geo, per_lay,
			bioinc * kg_per_ha_per_y_to_g_per_cm2_per_h);

  // Mix roots in top.
  geo.mix (per_lay, 0.0, end);
  
  assert_non_negative (per_lay);
}

OrganicMatter::Implementation::Initialization::~Initialization ()
{ }

bool 
OrganicMatter::Implementation::aom_compare (const AOM* a, const AOM* b)
{
  double A = a->initial_C_per_N;
  if (A == OM::Unspecified
      && a->N.size () > 0 && a->C.size () > 0 && a->N[0] > 0)
    A = a->C[0] / a->N[0];
  double B = b->initial_C_per_N;
  if (B == OM::Unspecified 
      && b->N.size () > 0 && b->C.size () > 0 && b->N[0] > 0)
    B = b->C[0] / b->N[0];

  return A < B;
}

double 
OrganicMatter::Implementation::total_N (const Geometry& geo) const
{
  double result = geo.total (buffer.N);

  for (unsigned int i = 0; i < smb.size (); i++)
    result += smb[i]->soil_N (geo);
  for (unsigned int i = 0; i < som.size (); i++)
    result += som[i]->soil_N (geo);
  for (unsigned int i = 0; i < dom.size (); i++)
    result += dom[i]->soil_N (geo);
  for (int i = 0; i < am.size (); i++)
    result += am[i]->total_N (geo);
  
  return result;
}

double 
OrganicMatter::Implementation::total_C (const Geometry& geo) const
{
  double result = geo.total (buffer.C);

  for (unsigned int i = 0; i < smb.size (); i++)
    result += smb[i]->soil_C (geo);
  for (unsigned int i = 0; i < som.size (); i++)
    result += som[i]->soil_C (geo);
  for (unsigned int i = 0; i < dom.size (); i++)
    result += dom[i]->soil_C (geo);
  for (int i = 0; i < am.size (); i++)
    result += am[i]->total_C (geo);
  
  return result;
}

double 
OrganicMatter::Implementation::heat_turnover_factor (double T) const
{
  if (heat_factor.size () > 0)
    return heat_factor (T);
  if (T < 0.0)
    return 0.0;
  if (T < 20.0)
    return 0.1 * T;
  if (T < 37.0)
    return exp (0.47 - 0.027 * T + 0.00193 * T * T);
  if (T < 60.0)
    {
      // J.A. van Veen and M.J.Frissel.
      const double max_val = exp (0.47 - 0.027 * T + 0.00193 * T * T);
      return max_val * (1.0 - (T - 37.0) / (60.0 - 37.0));
    }
  return 0.0;
}

double
OrganicMatter::Implementation::water_turnover_factor (double h) const
{
  if (water_factor.size () > 0)
    return water_factor (h);

  if (h >= 0.0)
    return 0.6;

  const double pF = h2pF (h);

  if (pF <= 0.0)
    return 0.6;
  if (pF <= 1.5)
    return 0.6 + (1.0 - 0.6) * pF / 1.5;
  if (pF <= 2.5)
    return 1.0;
  if (pF <= 6.5)
    return 1.0 - (pF - 2.5) / (6.5 - 2.5);

  return 0;
}

void
OrganicMatter::Implementation::Buffer::initialize (const Geometry& geo)
{ 
  const unsigned int size = geo.node_size ();
  // Make sure the vectors are large enough.
  while (N.size () < size)
    N.push_back (0.0);
  assert_non_negative (N);
  while (C.size () < size)
    C.push_back (0.0);
  assert_non_negative (C);
}

OrganicMatter::Implementation::Buffer::Buffer (const AttributeList& al)
  : C (al.number_sequence ("C")),
    N (al.number_sequence ("N")),
    turnover_rate (al.check ("turnover_halftime")
		   ? halftime_to_rate (al.number ("turnover_halftime"))
		   : al.number ("turnover_rate")),
    where (al.integer ("where"))
{
  assert_non_negative (N);
  assert_non_negative (C);
  daisy_assert (turnover_rate >= 0.0);
  daisy_assert (turnover_rate <= 1.0);
}

void
OrganicMatter::Implementation::output (Log& log,
				       const Geometry& geo) const
{
  static const symbol CO2_symbol ("CO2");
  if (log.check_leaf (CO2_symbol))
    {
      vector<double> CO2 (CO2_slow);
      daisy_assert (CO2.size () == CO2_fast.size ());
      for (unsigned int i =0; i < CO2.size(); i++)
	CO2[i] += CO2_fast[i];
      output_variable (CO2, log);
    }
  output_variable (CO2_fast, log);
  output_variable (top_CO2, log);
  static const symbol total_N_symbol ("total_N");
  static const symbol total_C_symbol ("total_C");
  static const symbol humus_symbol ("humus");
  if (log.check_leaf (total_N_symbol)
      || log.check_leaf (total_C_symbol)
      || log.check_leaf (humus_symbol))
    {
      const int size = geo.node_size ();

      vector<double> total_N (size, 0.0);
      vector<double> total_C (size, 0.0);
      for (int i = 0; i < size; i++)
	{
	  for (unsigned int j = 0; j < smb.size (); j++)
	    {
	      total_C[i] += smb[j]->C[i];
	      total_N[i] += smb[j]->N[i];
	    }
	  for (unsigned int j = 0; j < som.size (); j++)
	    {
	      total_C[i] += som[j]->C[i];
	      total_N[i] += som[j]->N[i];
	    }
	  for (unsigned int j = 0; j < dom.size (); j++)
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
          vector<double> humus;
          for (size_t i = 0; i < total_C.size (); i++)
            humus.push_back (total_C[i] / c_fraction_in_humus);
          output_variable (humus, log);
        }
    }
  output_variable (abiotic_factor, log);
  output_variable (tillage_age, log);
  static const symbol am_symbol ("am");
  if (log.check_interior (am_symbol))
    {
      const Library& library = Librarian<AM>::library ();
      
      Log::Open open (log, am_symbol);
      for (vector<AM*>::const_iterator item = am.begin(); 
	   item != am.end();
	   item++)
	{
	  const symbol name = (*item)->real_name ();
	  if (log.check_entry (name, library))
	    {
	      Log::NamedEntry named_entry (log, name, (*item)->name,
					   (*item)->alist);
	      (*item)->output (log);
	    }
	}
    }
  output_ordered (smb, "smb", log);
  output_ordered (som, "som", log);
  output_ordered (dom, "dom", log);
  output_list (domsorp, "domsorp", log, Librarian<Domsorp>::library ());
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
OrganicMatter::Implementation::check (const Soil& soil, Treelog& err) const
{
  Treelog::Open nest (err, "OrganicMatter");
  bool ok = true;
  for (unsigned int i = 0; i < am.size (); i++)
    if (!am[i]->check (err))
      ok = false;
  for (size_t i = 0; i < domsorp.size (); i++)
    if (!domsorp[i]->check (soil, dom.size (), som.size (), err))
      ok = false;
  if (!clayom->check (smb, err))
    ok = false;
  return ok;
}

void 
OrganicMatter::Implementation::add (AM& om)
{ 
  for (unsigned int i = 0; i < am.size (); i++)
    daisy_assert (&om != am[i]);
  am.push_back (&om); 
}

void 
OrganicMatter::Implementation::fertilize (const AttributeList& al, 
                                          const Geometry& geo)
{ 
  AM& om = AM::create (al, geo);
  fertilized_N += om.total_N (geo); 
  fertilized_C += om.total_C (geo);
  add (om);
}

void 
OrganicMatter::Implementation::fertilize (const AttributeList& al,
                                          const Geometry& geo,
                                          double from, double to)
{ 
  AM& om = AM::create (al, geo);
  fertilized_N += om.total_N (geo); 
  fertilized_C += om.total_C (geo);
  om.mix (geo, from, to, 1.0,
          tillage_N_top, tillage_C_top,
          tillage_N_soil, tillage_C_soil);
  add (om);
}

void 
OrganicMatter::Implementation::clear ()
{
  fertilized_N = 0.0;
  fertilized_C = 0.0;
  tillage_N_top = 0.0;
  tillage_C_top = 0.0;
  fill (tillage_N_soil.begin (), tillage_N_soil.end (), 0.0);
  fill (tillage_C_soil.begin (), tillage_C_soil.end (), 0.0);
}

void
OrganicMatter::Implementation::monthly (const Geometry& geo)
{
  static const symbol am_symbol ("am");
  static const symbol cleanup_symbol ("cleanup");
  AM* remainder = find_am (am_symbol, cleanup_symbol);
  if (!remainder)
    {
      remainder = &AM::create (geo, Time (1, 1, 1, 1), AM::default_AM (),
			       am_symbol, cleanup_symbol, AM::Locked);
      add (*remainder);
    }

  const int am_size = am.size ();
  vector<AM*> new_am;
  
  for (int i = 0; i < am_size; i++)
    {
      daisy_assert (am[i]);

      bool keep;
      
      if (am[i]->locked ())
	keep = true;
      else if (min_AM_C == 0.0)
	if (min_AM_N == 0.0)
	  // No requirement, keep it.
	  keep = true;
	else
	  // Only require N.
	  keep = (am[i]->total_N (geo) * (100.0 * 100.0) > min_AM_N);
      else
	if (min_AM_N == 0.0)
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
  am = new_am;
}

template <class DAOM>
const double*
OrganicMatter::Implementation::find_abiotic (const DAOM& om,
					     const int size, 
					     const SoilWater& soil_water, 
					     const SoilHeat& soil_heat,
					     const vector<double>& 
					     /**/ default_value,
					     vector<double>& scratch) const

{
  const bool use_om_heat = (om.heat_factor.size () > 0);
  const bool use_om_water = (om.water_factor.size () > 0);
  
  if (!use_om_heat && !use_om_water)
    return &default_value[0];
  
  for (unsigned int i = 0; i < size; i++)
    {
      const double T = soil_heat.T (i);
      if (use_om_heat)
	scratch[i] = om.heat_factor (T);
      else
	scratch[i] = heat_turnover_factor (T);

      const double h = soil_water.h (i);
      if (use_om_water)
	scratch[i] *= om.water_factor (h);
      else
	scratch[i] *= water_turnover_factor (h);
    }
  return &scratch[0];
}

const double*
OrganicMatter::Implementation::find_abiotic (const OM& om,
					     const int size, 
					     const SoilWater& soil_water, 
					     const SoilHeat& soil_heat,
					     const vector<const PLF*> 
					     /**/ tillage_factor,
					     const int pool,
					     const vector<double>&
					     /**/ default_value,
					     bool use_clay,
					     vector<double>& scratch) const
{				// SOM & SMB
  const bool use_om_heat = (om.heat_factor.size () > 0);
  const bool use_om_water = (om.water_factor.size () > 0);
  const bool use_tillage = (tillage_factor.size () > pool);

  if (!use_om_heat && !use_om_water && !use_tillage)
    return &default_value[0];
  
  if (use_om_heat || use_om_water)
    {
      for (unsigned int i = 0; i < size; i++)
	{
	  if (use_clay)
	    scratch[i] = clay_turnover_factor[i];
	  else
	    scratch[i] = soil_turnover_factor[i];

	  const double T = soil_heat.T (i);
	  if (use_om_heat)
	    scratch[i] *= om.heat_factor (T);
	  else
	    scratch[i] *= heat_turnover_factor (T);

	  const double h = soil_water.h (i);
	  if (use_om_water)
	    scratch[i] *= om.water_factor (h);
	  else
	    scratch[i] *= water_turnover_factor (h);
	}
    }
  else
    scratch = default_value;

  if (use_tillage)
    for (unsigned int i = 0; i < size; i++)
      scratch[i] *= (*tillage_factor[pool]) (tillage_age[i]);

  return &scratch[0];
}

size_t
OrganicMatter::Implementation::active_size (const Geometry& geo,
                                            const Soil& soil, 
                                            const SoilWater& soil_water) const
{
  size_t size = soil.size ();
  if (!active_underground && geo.zplus (size - 1) < -100.0)
    size = geo.interval_plus (min (-100.0, soil.MaxRootingDepth ())) + 1;
  if (!active_groundwater)
    size = min (soil_water.first_groundwater_node (), size);
  size = min (size, soil.size ());
  
  return size;
}

void 
OrganicMatter::Implementation::tick (const Geometry& geo,
                                     const Soil& soil, 
				     const SoilWater& soil_water, 
				     const SoilHeat& soil_heat,
				     SoilNO3& soil_NO3,
				     SoilNH4& soil_NH4,
				     Treelog& msg)
{
  const double old_N = total_N (geo);
  const double old_C = total_C (geo);

  // Create an array of all AM dk:puljer, sorted by their C_per_N.
  const int all_am_size = am.size ();
  vector<AOM*> added;
  for (int i = 0; i < all_am_size; i++)
    am[i]->append_to (added);
  sort (added.begin (), added.end (), aom_compare);
  
  // Clear logs.
  fill (CO2_slow.begin (), CO2_slow.end (), 0.0);
  fill (CO2_fast.begin (), CO2_fast.end (), 0.0);
  fill (NO3_source.begin (), NO3_source.end (), 0.0);
  fill (NH4_source.begin (), NH4_source.end (), 0.0);
  top_CO2 = 0.0;
  for (unsigned int j = 0; j < dom.size (); j++)
    dom[j]->clear ();

  // Setup arrays.
  const size_t size = active_size (geo, soil, soil_water);
  
  vector<double> N_soil (size);
  vector<double> N_used (size);
  vector<double> clay_factor (size);
  vector<double> soil_factor (size);
  vector<double> tillage_factor (size);
  
  for (unsigned int i = 0; i < size; i++)
    {
      daisy_assert (soil_NH4.M_left (i) >= 0);
      const double NH4 = (soil_NH4.M_left (i) < 1e-9) // 1 mg/l
	? 0.0 : soil_NH4.M_left (i) * K_NH4;
      daisy_assert (soil_NO3.M_left (i) >= 0);
      const double NO3 = (soil_NO3.M_left (i) < 1e-9) // 1 mg/l
	? 0.0 : soil_NO3.M_left (i) * K_NO3;

      N_soil[i] = NH4 + NO3;
      N_used[i] = 0.0;

      const double h = soil_water.h (i);
      daisy_assert (isfinite (h));
      const double T = soil_heat.T (i);
      const double heat = heat_turnover_factor (T);
      const double water = water_turnover_factor (h);
      abiotic_factor[i] = heat * water;
      clay_factor[i] = abiotic_factor[i] * clay_turnover_factor [i];
      soil_factor[i] = abiotic_factor[i] * soil_turnover_factor [i];
    }
  
  // Main processing.
  for (unsigned int j = 0; j < dom.size (); j++)
    {
      const double *const abiotic 
	= find_abiotic (*dom[j], size, soil_water, soil_heat, soil_factor, 
                        tillage_factor);
      double *const CO2 = dom[j]->turnover_rate > CO2_threshold 
	? &CO2_fast[0] 
	: &CO2_slow[0];
      dom[j]->turnover (size, abiotic, &N_soil[0], &N_used[0], CO2, smb);
    }
  for (unsigned int j = 0; j < smb.size (); j++)
    {
      const bool use_clay = clayom->smb_use_clay (j);
      const vector<double>& default_factor = 
	use_clay ? clay_factor : soil_factor;
      const double *const abiotic 
	= find_abiotic (*smb[j], size, soil_water, soil_heat,
			smb_tillage_factor, j,
			default_factor, use_clay, tillage_factor);
      double *const CO2 = smb[j]->turnover_rate > CO2_threshold 
	? &CO2_fast[0] 
	: &CO2_slow[0];
      smb[j]->maintain (size, abiotic, &N_used[0], CO2);
      smb[j]->tick (size, abiotic, &N_soil[0], &N_used[0], CO2, smb, som, dom);
    }
  for (unsigned int j = 0; j < som.size (); j++)
    {
      const bool use_clay = clayom->som_use_clay (j);
      const vector<double>& default_factor = 
	use_clay ? clay_factor : soil_factor;
      const double *const abiotic 
	= find_abiotic (*som[j], size, soil_water, soil_heat,
			som_tillage_factor, j,
			default_factor, use_clay, tillage_factor);
      double *const CO2 = som[j]->turnover_rate > CO2_threshold 
	? &CO2_fast[0] 
	: &CO2_slow[0];
      som[j]->tick (size, abiotic, &N_soil[0], &N_used[0], CO2, smb, som, dom);
    }

  for (unsigned int j = 0; j < added.size (); j++)
    {
      const double *const abiotic 
	= find_abiotic (*added[j], size, soil_water, soil_heat,
			soil_factor, tillage_factor);
      double *const CO2 = added[j]->turnover_rate > CO2_threshold 
	? &CO2_fast[0] 
	: &CO2_slow[0];
      added[j]->tick (size, abiotic, &N_soil[0], &N_used[0], &CO2[0],
		      smb, &buffer.C[0], &buffer.N[0], dom);
    }

  // Update buffer.
  for (unsigned int i = 0; i < size; i++)
      buffer.tick (i, soil_factor[i], N_soil[i], N_used[i], som);

  // Update source.
  for (unsigned int i = 0; i < size; i++)
    {
      daisy_assert (N_used[i] < soil_NH4.M_left (i) + soil_NO3.M_left (i));

      const double NH4 = (soil_NH4.M_left (i) < 1e-9) // 1 mg/l
	? 0.0 : soil_NH4.M_left (i) * K_NH4;
      daisy_assert (NH4 >= 0.0);

      if (N_used[i] > NH4)
	{
	  NH4_source[i] = -NH4 / dt;
	  NO3_source[i] = (NH4 - N_used[i]) / dt;
	  daisy_assert (NH4_source[i] <= 0.0);
	  daisy_assert (NO3_source[i] <= 0.0);
	}
      else
	{
	  NH4_source[i] = -N_used[i] / dt;
	  NO3_source[i] = 0.0;
	}
    }
  
  // Update soil solutes.
  soil_NO3.add_to_source (NO3_source);
  soil_NH4.add_to_source (NH4_source);

  // Biological incorporation.
  bioincorporation.tick (geo, am, soil_heat.T (0), top_CO2);

  // Tillage time.
  for (unsigned int i = 0; i < size; i++)
    tillage_age[i] += 1.0/24.0;

  // Mass balance.
  double N_to_DOM = 0.0;
  for (int j = 0; j < dom.size (); j++)
    N_to_DOM += dom[j]->N_source (geo) * dt;
  const double new_N = total_N (geo) + N_to_DOM;
  const double delta_N = old_N - new_N;
  const double N_source = geo.total (NO3_source) + geo.total (NH4_source);

  if (!approximate (delta_N, N_source)
      && !approximate (old_N, new_N, 1e-10))
    {
      std::ostringstream tmp;
      tmp << "BUG: OrganicMatter: delta_N != NO3 + NH4[g N/cm^2]\n"
	     << delta_N << " != " << geo.total (NO3_source)
	     << " + " << geo.total (NH4_source);
      if (N_source != 0.0)
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
    = geo.total (CO2_slow) + geo.total (CO2_fast) + top_CO2;
  
  if (!approximate (delta_C, C_source)
      && !approximate (old_C, new_C, 1e-10))
    {
      std::ostringstream tmp;
      tmp << "BUG: OrganicMatter: "
	"delta_C != soil_CO2_slow + soil_CO2_fast + top_CO2 [g C/cm^2]\n"
	     << delta_C << " != " << geo.total (CO2_slow) << " + " 
	     << geo.total (CO2_fast) << " + " << top_CO2;
      msg.error (tmp.str ());
    }

  // We didn't steal it all?
  for (int i = 0; i < soil.size (); i++)
    {
      daisy_assert (soil_NO3.M_left (i) >= 0.0);
      daisy_assert (soil_NH4.M_left (i) >= 0.0);
    }
}
      
void 
OrganicMatter::Implementation::transport (const Soil& soil, 
					  const SoilWater& soil_water, 
					  Treelog& msg)
{
  for (size_t j = 0; j < domsorp.size (); j++)
    domsorp[j]->tick (soil, soil_water, dom, som, msg);
}

void 
OrganicMatter::Implementation::mix (const Geometry& geo, const Soil& soil,
				    const SoilWater& soil_water,
				    double from, double to, 
                                    double penetration,
				    const Time&)
{
  buffer.mix (geo, from, to);
  for (unsigned int i = 0; i < am.size (); i++)
    am[i]->mix (geo, from, to, penetration, 
                tillage_N_top, tillage_C_top, 
                tillage_N_soil, tillage_C_soil);
  for (unsigned int i = 1; i < smb.size (); i++)
    smb[i]->mix (geo, from, to, 
                 tillage_N_soil, tillage_C_soil);
  for (unsigned int i = 0; i < som.size (); i++)
    som[i]->mix (geo, from, to, 
                 tillage_N_soil, tillage_C_soil);
  for (unsigned int i = 0; i < dom.size (); i++)
    dom[i]->mix (geo, soil, soil_water, from, to);

  // Leave CO2 alone.

  // Reset tillage age.
  double previous = 0.0;
  for (unsigned int i = 0; i < tillage_age.size (); i++)
    {
      const double next = geo.zplus (i);
      if (previous > to && next < from)
	tillage_age[i] = 0.0;
      previous = next;
    }
}

void 
OrganicMatter::Implementation::swap (const Geometry& geo, const Soil& soil,
				     const SoilWater& soil_water,
				     double from, double middle, double to,
				     const Time&)
{
  buffer.swap (geo, from, middle, to);
  for (unsigned int i = 0; i < am.size (); i++)
    am[i]->swap (geo, from, middle, to, 
                 tillage_N_soil, tillage_C_soil);
  for (unsigned int i = 1; i < smb.size (); i++)
    smb[i]->swap (geo, from, middle, to, 
                  tillage_N_soil, tillage_C_soil);
  for (unsigned int i = 0; i < som.size (); i++)
    som[i]->swap (geo, from, middle, to, 
                  tillage_N_soil, tillage_C_soil);
  for (unsigned int i = 0; i < dom.size (); i++)
    dom[i]->swap (geo, soil, soil_water, from, middle, to);
  // Leave CO2 alone.
}

AM* 
OrganicMatter::Implementation::find_am (const symbol sort,
					const symbol part) const
{
  for (unsigned int i = 0; i < am.size (); i++)
    if (am[i]->locked () 
	&& am[i]->crop_name () == sort 
	&& am[i]->crop_part_name () == part)
      return am[i];
  return NULL;
}

void
OrganicMatter::Implementation::input_from_am (vector<double>& destination,
					      double T, double h,
					      const int lay) const
{
  // Calculate the input from all aom in soil layer lay to all destinations. 
  const unsigned int size = destination.size ();
  daisy_assert (size == smb.size () + 1);
  const unsigned int som_pool = size - 1;
  fill (destination.begin (), destination.end (), 0.0);

  // Loop over all AOM pools.
  vector<AOM*> added;
  for (unsigned int i = 0; i < am.size (); i++)
    am[i]->append_to (added);

  for (unsigned int i = 0; i < added.size (); i++)
    if (added[i]->C.size () > lay)
      {
	daisy_assert (added[i]->fractions.size () == size);
	const double abiotic_factor 
	  = abiotic (*added[i], T, h, false, lay);
	// For SMB pools.
	for (unsigned int pool = 0; pool < som_pool ; pool++)
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
}

double
OrganicMatter::Implementation::total_input_from_am (double T, double h,
                                                    const int lay) const
{
  const unsigned int som_pool = smb.size ();

  // Loop over all AOM pools.
  vector<AOM*> added;
  for (unsigned int i = 0; i < am.size (); i++)
    am[i]->append_to (added);

  double total = 0.0;
  for (unsigned int i = 0; i < added.size (); i++)
    if (added[i]->C.size () > lay)
      {
	const double abiotic_factor 
	  = abiotic (*added[i], T, h, false, lay);
	// For SMB pools.
	for (unsigned int pool = 0; pool < som_pool ; pool++)
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
  return total;
}

				     
double
OrganicMatter::Implementation::abiotic (const OM& om, double T, double h,
					bool use_clay, int lay) const
{
  return ((om.heat_factor.size () > 0) 
	  ? om.heat_factor (T) 
	  : heat_turnover_factor (T))
    * ((om.water_factor.size () > 0)
       ? om.water_factor (h)
       : water_turnover_factor (h))
    * (use_clay ? clay_turnover_factor[lay] : soil_turnover_factor[lay]);
}

vector<double> 
OrganicMatter::Implementation::SOM_limit_normalize
/**/ (const vector<double>& limit, const vector<double>& fractions)
{
  // If no fractions are specified, just use limit.
  bool unspecified = true;
  for (unsigned int i = 0; i < fractions.size (); i++)
    if (fractions[i] >= 0.0)
      unspecified = false;
  
  if (unspecified)
    return limit;

  vector<double> result (limit.size (), -1.0);

  // Set the forced values.
  double added = 0.0;
  double removed = 0.0;
  int missing = 0;
  for (unsigned int i = 0; i < fractions.size (); i++)
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
      for (unsigned int i = 0; i < result.size (); i++)
        if (result[i] < 0.0)
          result[i] = value;
    }
  else
    {
      // Distribute remaining SOM proportionally.
      const double factor = (1.0 - added) / (1.0 - removed);
      for (unsigned int i = 0; i < result.size (); i++)
        if (result[i] < 0.0)
          result[i] = limit[i] * factor;
    }

  daisy_assert (approximate (accumulate (result.begin (), result.end (), 0.0),
                             1.0));
  return result;
}
				     
void
OrganicMatter::Implementation::partition (const vector<double>& am_input,
                                          const double total_input,
					  const double T, const double h,
					  const int lay, const double total_C,
					  const int variable_pool,
					  const int variable_pool_2,
					  const double
					  /**/ background_mineralization,
					  bool top_soil,
					  const vector<double>& 
					  /**/ SOM_default_fractions,
					  const vector<double>& SOM_C_per_N,
					  const vector<double>&
					  /**/ SOM_limit_lower,
					  const vector<double>& 
					  /**/ SOM_limit_upper,
					  const int SOM_limit_where,
					  vector<double>& SOM_results,
					  vector<double>& SMB_results,
                                          double& delta_C,
                                          double& delta_N,
					  const double dry_bulk_density,
					  Treelog& msg,
					  const bool print_equations,
					  const bool print_rows,
					  const bool debug_to_screen) const
{
  // Find AOM values.
  double total_am = 0.0;
  for (unsigned int i = 0; i < am.size (); i++)
    total_am += am[i]->C_at (lay);

  // We know the total humus, the yearly input and has been told the
  // relative sizes of the SOM pools.  We need to calculate the SMB
  // fractions.

  // Sizes.
  const unsigned int smb_size = smb.size ();
  const unsigned int som_size = som.size ();

  // The columns are:
  //  k = SMB1 SMB2 SOM1 SOM2 dSMB1 dSMB2 dSOM1 dSOM2
  const unsigned int smb_column = 0;
  const unsigned int som_column = smb_column + smb_size;
  const unsigned int dsmb_column = som_column + som_size;
  const unsigned int dsom_column = dsmb_column + smb_size;
  const unsigned int number_of_equations = dsom_column + som_size;

  // Calculated C content.
  double total = -42.42e42;

  // Messages.
  std::ostringstream table_string;
  if (lay == 0)
    {
      // Tag line.
      table_string << "lay\thumus\thumus\tinput\tinput\tAOM";
      for (unsigned int pool = 0; pool < smb_size; pool++)
	table_string << "\tSMB" << (pool + 1);
      for (unsigned int pool = 0; pool < som_size; pool++)
	table_string << "\tSOM" << (pool + 1);
      for (unsigned int pool = 0; pool < smb_size; pool++)
	table_string << "\tdSMB" << (pool + 1) << "\tdSMB" << (pool + 1);
      for (unsigned int pool = 0; pool < som_size; pool++)
	table_string << "\tdSOM" << (pool + 1) << "\tdSOM" << (pool + 1);
      table_string << "\n";
      // Dimension line.
      table_string << "\tkg C/ha/cm\t%\tkg C/ha/cm/y\t%\t%";
      for (unsigned int pool = 0; pool < smb_size; pool++)
	table_string << "\t%";
      for (unsigned int pool = 0; pool < som_size; pool++)
	table_string << "\t%";
      for (unsigned int pool = 0; pool < smb_size; pool++)
	table_string << "\tkg C/ha/cm/y\ty^-1";
      for (unsigned int pool = 0; pool < som_size; pool++)
	table_string << "\tkg C/ha/cm/y\ty^-1";
      table_string << "\n";
    }
  std::ostringstream equation_string;
  equation_string << "Equations:\n";
  bool error_found;

  // We try the default fractions first.
  vector<double> SOM_fractions = SOM_default_fractions;

  while (true)
    {
      GaussJordan matrix (number_of_equations);
      unsigned int equation = 0;

      // The SMB and SOM change equations have this format:
      //
      //     dXXXn = k1 SMB1 + k2 SMB2 + k3 SOM1 + k4 SOM2 + k5 AOM1 + k6 AOM2
      // => -k5 AOM1 - k6 AOM2) = k1 SMB1 + k2 SMB2 + k3 SOM1 + k4 SOM2 - dXXX

      // The SMB change equations.
      for (unsigned int pool = 0; pool < smb_size; pool++)
	{
	  // Add contributions from AM pools
	  if (am_input[pool] > 1e-30)
	    matrix.set_value (equation, -am_input[pool]);
      
	  // Add contributions from SMB pools.
	  for (unsigned int i = 0; i < smb_size; i++)
	    {
	      const double abiotic_factor = abiotic (*smb[i], T, h,
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
	  for (unsigned int i = 0; i < som_size; i++)
	    {
	      const double abiotic_factor = abiotic (*som[i], T, h,
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
      for (unsigned int pool = 0; pool < som_size; pool++)
	{
	  // Add contributions from AOM pools.
	  if (pool == buffer.where && am_input[smb_size] > 1e-30)
	    matrix.set_value (equation, -am_input[smb_size]);
      
	  // Add contributions from SMB pools.
	  for (unsigned int i = 0; i < smb_size; i++)
	    {
	      const double abiotic_factor = abiotic (*smb[i], T, h,
						     clayom->smb_use_clay (i),
						     lay);
	      const double in = smb[i]->turnover_rate 
		* smb[i]->fractions[smb_size + pool]
		* abiotic_factor;
	  
	      if (in > 1e-100)
		matrix.set_entry (equation, smb_column + i, in);
	    }

	  // Add contributions from SOM pools.
	  for (unsigned int i = 0; i < som_size; i++)
	    {
	      const double abiotic_factor = abiotic (*som[i], T, h,
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
      for (unsigned int pool = 0; pool < smb_size; pool++)
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
      for (unsigned int pool = 0; pool < som_size; pool++)
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
	      for (unsigned int i = 0; i < smb_size; i++)
		matrix.set_entry (equation, smb_column + i, 1.0);
	      for (unsigned int i = 0; i < som_size; i++)
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
      
	      for (unsigned int i = 0; i < som.size (); i++)
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
	      for (unsigned int i = 0; i < smb_size; i++)
		{
		  double N_per_C = 0.0;
		  if (smb[i]->C.size () > lay && smb[i]->N.size () > lay)
		    {
		      if (isnormal (smb[i]->C[lay]))
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
	      for (unsigned int i = 0; i < som_size; i++)
		{
		  daisy_assert (isnormal (SOM_C_per_N[i]));
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
	      for (unsigned int i = 0; i < smb_size; i++)
		matrix.set_entry (equation, dsmb_column + i, 1.0);
	      for (unsigned int i = 0; i < som_size; i++)
		matrix.set_entry (equation, dsom_column + i, 1.0);
	      inert_pool_found = true;
	    }
	  equation++;
	}

      daisy_assert (number_of_equations == equation);

      // Print out equations.
      for (unsigned int row = 0; row < number_of_equations; row++)
	{
	  equation_string << matrix.get_value (row) << " =";
	  bool first = true;

	  for (unsigned int pool = 0; pool < smb_size; pool++)
	    {
	      const double value = matrix.get_entry (row, smb_column + pool);
	      if (value != 0.0)
		{
		  if (first)
		    first = false;
		  else
		    equation_string << " +";

		  equation_string << " " << value << " SMB" << (pool + 1);
		}
	    }
	  for (unsigned int pool = 0; pool < som_size; pool++)
	    {
	      const double value = matrix.get_entry (row, som_column + pool);
	      if (value != 0.0)
		{
		  if (first)
		    first = false;
		  else
		    equation_string << " +";
		  equation_string << " " << value << " SOM" << (pool + 1);
		}
	    }
	  for (unsigned int pool = 0; pool < smb_size; pool++)
	    {
	      const double value = matrix.get_entry (row, dsmb_column + pool);
	      if (value != 0.0)
		{
		  if (first)
		    first = false;
		  else
		    equation_string << " +";
		  equation_string << " " << value << " dSMB" << (pool + 1);
		}
	    }
	  for (unsigned int pool = 0; pool < som_size; pool++)
	    {
	      const double value = matrix.get_entry (row, dsom_column + pool);
	      if (value != 0.0)
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
      catch (const string& error)
	{
	  if (!print_equations)
	    msg.error (equation_string.str ());
	  msg.error (error);
	  throw ("Organic matter initialization failure");
	}

      // Check mass balance.
      total = total_am;
      for (unsigned int i = 0; i < smb_size + som_size; i++)
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
      
      for (unsigned int pool = 0; pool < smb_size; pool++)
	{
	  const double value = matrix.result (smb_column + pool);
	  table_string << "\t" << 100.0 * value / total;
	}
      for (unsigned int pool = 0; pool < som_size; pool++)
	{
	  const double value = matrix.result (som_column + pool);
	  table_string << "\t" << 100.0 * value / total;
	}
      for (unsigned int pool = 0; pool < smb_size; pool++)
	{
	  const double value = matrix.result (smb_column + pool) 
	    * g_per_cm2_to_kg_per_ha;
	  const double change = matrix.result (dsmb_column + pool)
	    * g_per_cm2_per_h_to_kg_per_ha_per_y;
	  const double rate = change / value;
	  table_string << "\t" << change << "\t" << rate;
	}
      for (unsigned int pool = 0; pool < som_size; pool++)
	{
	  const double value = matrix.result (som_column + pool) 
	    * g_per_cm2_to_kg_per_ha;
	  const double change = matrix.result (dsom_column + pool)
	    * g_per_cm2_per_h_to_kg_per_ha_per_y;
	  const double rate = change / value;
	  table_string << "\t" << change << "\t" << rate;
	}

      // Store results.
      for (unsigned int pool = 0; pool < smb_size; pool++)
	SMB_results[pool] = max (0.0, matrix.result (smb_column + pool));
      for (unsigned int pool = 0; pool < som_size; pool++)
	SOM_results[pool] = max (0.0, matrix.result (som_column + pool));


      // Store changes.
      delta_N = 0.0;
      delta_C = 0.0;
      for (unsigned int pool = 0; pool < smb_size; pool++)
        {
          double N_per_C = 0.0;
          if (smb[pool]->C.size () > lay && smb[pool]->N.size () > lay)
            {
              if (isnormal (smb[pool]->C[lay]))
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
      for (unsigned int pool = 0; pool < som_size; pool++)
        {
          const double C = matrix.result (dsom_column + pool);
          delta_C += C;
          daisy_assert (isnormal (SOM_C_per_N[pool]));
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
      for (unsigned int i = 0; i < SOM_fractions.size (); i++)
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

          const vector<double> limit_lower 
            = SOM_limit_normalize (SOM_limit_lower, SOM_fractions);
          const vector<double> limit_upper 
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
    if (debug_to_screen)
      msg.message (equation_string.str ());
    else
      msg.debug (equation_string.str ());
  if (error_found)
    {
      if (!print_equations)
	msg.error (equation_string.str ());

      std::ostringstream tmp;
      tmp << "Can't initialize organic matter from input in layer " << lay;
      throw (string (tmp.str ()));
    }
}

string
OrganicMatter::Implementation::top_summary (const Geometry& geo,
                                            const Soil& soil,
                                            const Initialization& init,
                                            const double zone_delta_N, 
                                            const double zone_delta_C) const
{
  std::ostringstream tmp;
    
  // Max number of AOM pools.
  size_t aom_max_size = 0;
  for (unsigned int pool = 0; pool < am.size (); pool++)
    { 
      vector<AOM*> added;
      am[pool]->append_to (added);
      aom_max_size = max (aom_max_size, added.size ());
    }        
    
  // Header line.
  tmp << "\t";
  for (unsigned int pool = 0; pool < som.size (); pool++)
    tmp << "SOM" << pool + 1 << "\t";
  for (unsigned int pool = 0; pool < smb.size (); pool++)
    tmp << "SMB" << pool + 1 << "\t";
  for (unsigned int pool = 0; pool < aom_max_size; pool++)
    tmp << "AOM" << pool + 1 << "\t";
  for (unsigned int pool = 0; pool < dom.size (); pool++)
    tmp << "DOM" << pool + 1 << "\t";
  tmp << "total\n";

  // C line
  tmp << "kg C/ha\t";
  double total_C = 0.0;
  for (unsigned int pool = 0; pool < som.size (); pool++)
    {
      const double C = som[pool]->soil_C (geo, 0.0, init.end);
      tmp << C * g_per_cm2_to_kg_per_ha << "\t";
      total_C += C;
    }
  for (unsigned int pool = 0; pool < smb.size (); pool++)
    {
      const double C = smb[pool]->soil_C (geo, 0.0, init.end);
      tmp << C * g_per_cm2_to_kg_per_ha << "\t";
      total_C += C;
    }
  for (unsigned int pool = 0; pool < aom_max_size; pool++)
    { 
      double C = 0.0;
      for (unsigned int i = 0; i < am.size (); i++)
        { 
          vector<AOM*> added;
          am[i]->append_to (added);
          if (pool < added.size ())
            C += added[pool]->soil_C (geo, 0.0, init.end);
        }        
      tmp << C * g_per_cm2_to_kg_per_ha << "\t";
      total_C += C;
    }
  for (unsigned int pool = 0; pool < dom.size (); pool++)
    {
      const double C = dom[pool]->soil_C (geo, 0.0, init.end);
      tmp << C * g_per_cm2_to_kg_per_ha << "\t";
      total_C += C;
    }
  tmp << total_C * g_per_cm2_to_kg_per_ha << "\n";

  // N line
  tmp << "kg N/ha\t";
  double total_N = 0.0;
  for (unsigned int pool = 0; pool < som.size (); pool++)
    {
      const double N = som[pool]->soil_N (geo, 0.0, init.end);
      tmp << N * g_per_cm2_to_kg_per_ha << "\t";
      total_N += N;
    }
  for (unsigned int pool = 0; pool < smb.size (); pool++)
    {
      const double N = smb[pool]->soil_N (geo, 0.0, init.end);
      tmp << N * g_per_cm2_to_kg_per_ha << "\t";
      total_N += N;
    }
  for (unsigned int pool = 0; pool < aom_max_size; pool++)
    { 
      double N = 0.0;
      for (unsigned int i = 0; i < am.size (); i++)
        { 
          vector<AOM*> added;
          am[i]->append_to (added);
          if (pool < added.size ())
            N += added[pool]->soil_N (geo, 0.0, init.end);
        }        
      tmp << N * g_per_cm2_to_kg_per_ha << "\t";
      total_N += N;
    }
  for (unsigned int pool = 0; pool < dom.size (); pool++)
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
  double last = 0.0;
  for (unsigned int lay = 0; 
       lay < geo.node_size () && geo.z (lay) > init.end;
       lay++)
    {
      const double next = max (init.end, geo.zplus (lay));
      const double dz = last - next;
      last = next;

      clay += soil.clay (lay) * dz;

      const double total_input 
        = (init.input >= 0)
        ? init.find_total_input (lay)
        : total_input_from_am (init.T, init.h, lay);
      input += total_input * dz * g_per_cm2_per_h_to_kg_per_ha_per_y;
    }
  clay /= -init.end;
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
OrganicMatter::Implementation::update_pools 
/**/ (const vector<double>& SOM_results,
      const double total_C_per_N,
      const vector<double>& SOM_C_per_N_goal,
      const vector<double>& SMB_results, 
      int lay)
{
  // Update SMB pools.
  double SMB_C = 0.0;
  double SMB_N = 0.0;
  for (unsigned int pool = 0; pool < smb.size (); pool++)
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
  for (unsigned int pool = 0; pool < som.size (); pool++)
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
      for (unsigned int i = 0; i < am.size (); i++)
	{
	  AOM_C += am[i]->C_at (lay);
	  AOM_N += am[i]->N_at (lay);
	}
      const double total_C = AOM_C + SMB_C + SOM_C;
      const double total_N = total_C / total_C_per_N;
      const double SOM_N = total_N - AOM_N - SMB_N;
      daisy_assert (SOM_N > 0.0);
      const double SOM_C_per_N = SOM_C / SOM_N;

      for (unsigned int pool = 0; pool < som.size (); pool++)
	if (som[pool]->N.size () == lay)
	  som[pool]->N.push_back (som[pool]->C[lay] / SOM_C_per_N);
    }
}

void
OrganicMatter::Implementation::initialize (const AttributeList& al,
                                           const Geometry& geo,
                                           const Soil& soil, 
					   const SoilWater& soil_water,
					   double T_avg, Treelog& err)
{ 
  Treelog::Open nest (err, "OrganicMatter");

  // Sizes.
  const unsigned int smb_size = smb.size ();
  const unsigned int som_size = som.size ();
  const unsigned int dom_size = dom.size ();

  // Check horizons.
  for (unsigned int lay = 0; lay < soil.size (); lay++)
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
  CO2_slow.insert (CO2_slow.end (), soil.size (), 0.0);
  CO2_fast.insert (CO2_fast.end (), soil.size (), 0.0);
  NO3_source.insert (NO3_source.end (), soil.size (), 0.0);
  NH4_source.insert (NH4_source.end (), soil.size (), 0.0);
  
  // Clay affect of SMB turnover and mantenance.
  clayom->set_rates (soil, smb);

  // Clay and soil.
  for (unsigned int i = 0; i < soil.size (); i++)
    {
      const double soil_factor = soil.turnover_factor (i);
      const double clay_factor = clayom->factor (soil.clay (i));
      soil_turnover_factor.push_back (soil_factor);
      clay_turnover_factor.push_back (soil_factor * clay_factor);
    }

  abiotic_factor.insert (abiotic_factor.end (), soil.size (), 1.0);
    

  // Tillage.
  tillage_age.insert (tillage_age.end (), 
		      soil.size () - tillage_age.size (), 1000000.0);

  // Initialize AM.
  for (unsigned int i = 0; i < am.size (); i++)
    am[i]->initialize (geo, soil.MaxRootingDepth ());

  // Biological incorporation.
  bioincorporation.initialize (geo, soil);
  static const symbol bio_symbol ("bio");
  static const symbol incorporation_symbol ("incorporation");
  AM* bioam = find_am (bio_symbol, incorporation_symbol);
  if (bioam)
    bioincorporation.set_am (bioam);
  else
    am.push_back (bioincorporation.create_am (geo)); 

  // Warnings in case of explicit SOM or SMB initialization.
  for (unsigned int pool = 0; pool < som_size; pool++)
    {
      std::ostringstream tmp;
      tmp << "som[" << pool << "]";
      Treelog::Open nest (err, tmp.str ());
      if (som[pool]->C.size () > 0 && som[pool]->C.size () < soil.size ())
	err.warning ("C partially initialized.\n\
Using humus for remaining entries");
      if (som[pool]->N.size () > 0 && som[pool]->N.size () < soil.size ())
	err.warning ("N partially initialized.\n\
Using humus for remaining entries");
    }
  for (unsigned int pool = 0; pool < smb_size; pool++)
    {
      std::ostringstream tmp;
      tmp << "smb[" << pool << "]";
      Treelog::Open nest (err, tmp.str ());
      if (smb[pool]->C.size () > 0 && smb[pool]->C.size () < soil.size ())
	err.warning ("C partially initialized.\n\
Using equilibrium for remaining entries");
      if (smb[pool]->N.size () > 0 && smb[pool]->N.size () < soil.size ())
	err.warning ("N partially initialized.\n\
Using initial C per N for remaining entries");
    }

  vector<double> total_C (soil.size (), 0.0);
  double first_humus = 0.0;

  // Initialize C from layers, when available.
  if (al.check ("initial_SOM"))
    {
      const vector<AttributeList*>& layers
	= al.alist_sequence ("initial_SOM");
      const double soil_end = geo.zplus (soil.size () - 1);
      double last = 0.0;
      for (unsigned int i = 0; i < layers.size (); i++)
	{
	  double end = layers[i]->number ("end");
	  double weight = layers[i]->number ("weight"); // kg C/m²
	  daisy_assert (weight > 0);
	  daisy_assert (end < last);
	  if (end < soil_end)
	    {
	      err.warning ("\
An 'initial_SOM' layer in OrganicMatter ends below the last node");
	      weight *= (last - soil_end) / (last - end);
	      end = soil_end;
	      i = layers.size ();
	    }
	  const double C = weight * 1000.0 / (100.0 * 100.0); // g C / cm²
	  geo.add (total_C, last, end, C);
	  last = end;
	}
      first_humus = last;
    }

  // Initialize rest from humus.
  {
    double last = 0.0;
    for (unsigned int lay = 0; lay < soil.size (); lay++)
      {
        const double humus_C = soil.humus_C (lay);
        const double zplus = geo.zplus (lay);
        if (zplus < first_humus)
          if (last <= first_humus)
            total_C[lay] = humus_C;
          else
            geo.add (total_C, first_humus, zplus, 
                      humus_C * (first_humus - zplus));
        last = zplus;
      }
  }
  // Partitioning.
  Initialization init (al.alist ("init"), geo, soil, bioincorporation, som, T_avg);
		       
  double total_delta_C = 0.0;
  double total_delta_N = 0.0;
  double zone_delta_C = 0.0;
  double zone_delta_N = 0.0;
  {
    vector<double> SOM_results (som_size, 0.0);
    vector<double> SMB_results (smb_size, 0.0);
    double last = 0.0;

    for (unsigned int lay = 0; lay < soil.size (); lay++)
      {
        vector<double> am_input (smb.size () + 1);
        if (init.input >= 0)
          init.find_input (am_input, lay);
        else
          input_from_am (am_input, init.T, init.h, lay);
        const double total_input 
          = (init.input >= 0)
          ? init.find_total_input (lay)
          : total_input_from_am (init.T, init.h, lay);
      
        const bool top_soil = geo.z (lay) > init.end;
        const double background_mineralization = 
          (top_soil && init.background_mineralization > -1e10)
          ? (init.background_mineralization 
             * kg_per_ha_per_y_to_g_per_cm2_per_h / -init.end)
          : -42.42e42;
        double delta_C;
        double delta_N;
        partition (am_input, total_input, init.T, init.h, 
                   lay, total_C[lay], init.variable_pool, init.variable_pool_2,
                   background_mineralization, top_soil,
                   soil.SOM_fractions (lay), 
                   soil.SOM_C_per_N (lay),
                   init.SOM_limit_lower, init.SOM_limit_upper, 
                   (top_soil ? init.SOM_limit_where : -1),
                   SOM_results, SMB_results,
                   delta_C, delta_N,
                   soil.dry_bulk_density (lay),
                   err,
                   init.print_equations (lay), init.debug_rows, 
                   init.debug_to_screen);
        total_delta_C += delta_C * geo.dz (lay);
        total_delta_N += delta_N * geo.dz (lay);
        if (top_soil)
          {
            const double next = max (init.end, geo.zplus (lay));
            const double dz = last - next;
            last = next;
            total_delta_C += delta_C * dz;
            total_delta_N += delta_N * dz;
          }
        update_pools (SOM_results, soil.C_per_N (lay), 
                      soil.SOM_C_per_N (lay), SMB_results, lay);
      }
  }

  // Summary.
  {
    Treelog::Open nest (err, "Total soil summary");
    std::ostringstream total;
    total << "Expected humus change: " 
             << total_delta_C * g_per_cm2_per_h_to_kg_per_ha_per_y 
             << " [kg C/ha/y], ";
    const double all_C = this->total_C (geo);
    if (isnormal (all_C))
      total << total_delta_C / all_C << " [y^-1]";
    else
      total << "all new";
    total << ".\nExpected background mineralization: "
             << -total_delta_N * g_per_cm2_per_h_to_kg_per_ha_per_y 
             << " [kg N/ha/y].";
    if (init.debug_to_screen)
      err.message (total.str ());
    else
      err.debug (total.str ());  
  }

  // Initialize DOM.
  for (unsigned int pool = 0; pool < dom_size; pool++)
    dom[pool]->initialize (soil, soil_water, err);

  // Initialize domsorp
  for (size_t i = 0; i < domsorp.size (); i++)
    domsorp[i]->initialize (soil, err);

  // Print top summary.
  {
    const string summary 
      = top_summary (geo, soil, init, zone_delta_N, zone_delta_C);

    Treelog::Open nest (err, "Top soil summary");
    if (init.debug_to_screen)
      err.message (summary);
    else
      err.debug (summary);
    
    if (init.top_summary != "")
      {
        ofstream out (init.top_summary.c_str ());
        out << summary;
        if (!out.good ())
          err.error ("Problems writing to '" + init.top_summary + "'");
      }
  }  

  // Initialize buffer.
  buffer.initialize (geo);

  // Log variable.
  tillage_N_soil.insert (tillage_N_soil.end (), soil.size (), 0.0);
  tillage_C_soil.insert (tillage_C_soil.end (), soil.size (), 0.0);
}

OrganicMatter::Implementation::Implementation (Block& al)
  : active_underground (al.flag ("active_underground")),
    active_groundwater (al.flag ("active_groundwater")),
    K_NH4 (al.number ("K_NH4")),
    K_NO3 (al.number ("K_NO3")),
    CO2_threshold (al.number ("CO2_threshold")),
    top_CO2 (0.0),
    am (Librarian<AM>::build_vector (al, "am")),
    smb (map_construct<SMB> (al.alist_sequence ("smb"))),
    som (map_construct<SOM> (al.alist_sequence ("som"))),
    dom (map_submodel<DOM> (al, "dom")),
    domsorp (Librarian<Domsorp>::build_vector (al, "domsorp")),
    buffer (al.alist ("buffer")),
    heat_factor (al.plf ("heat_factor")),
    water_factor (al.plf ("water_factor")),
    clayom (Librarian<ClayOM>::build_item (al, "ClayOM")),
    smb_tillage_factor (al.plf_sequence ("smb_tillage_factor")),
    som_tillage_factor (al.plf_sequence ("som_tillage_factor")),
    min_AM_C (al.number ("min_AM_C")),
    min_AM_N (al.number ("min_AM_N")),
    bioincorporation (al.alist ("Bioincorporation")),
    fertilized_N (0.0),
    fertilized_C (0.0),
    tillage_N_top (0.0),
    tillage_C_top (0.0)
{ 
  if (al.check ("tillage_age"))
    tillage_age = al.number_sequence ("tillage_age");
}

OrganicMatter::Implementation::~Implementation ()
{
  sequence_delete (am.begin (), am.end ());
  sequence_delete (smb.begin (), smb.end ());
  sequence_delete (som.begin (), som.end ());
  sequence_delete (dom.begin (), dom.end ());
  sequence_delete (domsorp.begin (), domsorp.end ());
}

void 
OrganicMatter::clear ()
{ impl->clear (); }

void 
OrganicMatter::monthly (const Geometry& geo)
{ impl->monthly (geo); }

size_t
OrganicMatter::active_size (const Geometry& geo,
                            const Soil& soil, 
                            const SoilWater& soil_water) const
{ return impl->active_size (geo, soil, soil_water); }

void 
OrganicMatter::tick (const Geometry& geo,
                     const Soil& soil, 
		     const SoilWater& soil_water, 
		     const SoilHeat& soil_heat,
		     SoilNO3& soil_NO3,
		     SoilNH4& soil_NH4,
		     Treelog& msg)
{ impl->tick (geo, soil, soil_water, soil_heat, soil_NO3, soil_NH4, msg); }

void 
OrganicMatter::transport (const Soil& soil, 
			  const SoilWater& soil_water, 
			  Treelog& msg)
{ impl->transport (soil, soil_water, msg); }

const vector<DOM*>&
OrganicMatter::dom () const
{ return impl->dom; }

void 
OrganicMatter::mix (const Geometry& geo, const Soil& soil,
		    const SoilWater& soil_water,
		    double from, double to, double penetration,
		    const Time& time)
{ impl->mix (geo, soil, soil_water, from, to, penetration, time); }

void 
OrganicMatter::swap (const Geometry& geo, const Soil& soil,
		     const SoilWater& soil_water,
		     double from, double middle, double to,
		     const Time& time)
{ impl->swap (geo, soil, soil_water, from, middle, to, time); }

double
OrganicMatter::CO2 (unsigned int i) const
{
  daisy_assert (impl->CO2_slow.size () > i);
  daisy_assert (impl->CO2_fast.size () > i);
  return impl->CO2_slow[i] + impl->CO2_fast[i];
}

double
OrganicMatter::CO2_fast (unsigned int i) const
{
  daisy_assert (impl->CO2_fast.size () > i);
  return impl->CO2_fast[i];
}

double 
OrganicMatter::get_smb_c_at (unsigned int i) const
{ return impl->get_smb_c_at (i); }

void 
OrganicMatter::output (Log& log, const Geometry& geo) const
{ impl->output (log, geo); }

bool
OrganicMatter::check_am (const AttributeList& am, Treelog& err) const
{
  bool ok = true;
  if (ok)
    {
      const vector<AttributeList*>& om_alist
	= am.alist_sequence ("om");
      
      for (unsigned int i = 0; i < om_alist.size(); i++)
	{
	  std::ostringstream tmp;
	  tmp << "[" << i << "]";
	  Treelog::Open nest (err, tmp.str ());
	  bool om_ok = true;
	  if (om_ok)
	    {
	      vector<double> fractions
		= om_alist[i]->number_sequence ("fractions");
	      if (fractions.size () != impl->smb.size () + 1 + impl->dom.size ()
		  && fractions.size () != impl->smb.size () + 1)
		{
		  std::ostringstream tmp;
		  tmp << "You have " << fractions.size ()
			 << " fractions but " << impl->smb.size ()
			 << " smb, 1 som buffer and " << impl->dom.size () 
			 << " dom";
		  err.entry (tmp.str ());
		  ok = false;
		}
	      double sum
		= accumulate (fractions.begin (), fractions.end (), 0.0);
	      if (fabs (sum - 1.0) > 0.0001)
		{
		  std::ostringstream tmp;
		  tmp << "The sum of all fractions is " << sum;
		  err.entry (tmp.str ());
		  ok = false;
		}
	    }
	  else ok = false;
	}
    }
  return ok;
}

int 
OrganicMatter::som_pools () const
{ return impl->som.size (); }

bool
OrganicMatter::check (const Soil& soil, Treelog& err) const
{ return impl->check (soil, err); }

void 
OrganicMatter::add (AM& am)
{ impl->add (am); }

void 
OrganicMatter::fertilize (const AttributeList& al,
                          const Geometry& geo)
{ impl->fertilize (al, geo); }

void 
OrganicMatter::fertilize (const AttributeList& al,
                          const Geometry& geo,
                          double from, double to)
{ impl->fertilize (al, geo, from, to); }

AM* 
OrganicMatter::find_am (const symbol sort, const symbol part) const
{ return impl->find_am (sort, part); }

void
OrganicMatter::initialize (const AttributeList& al, 
			   const Geometry& geo,
                           const Soil& soil, const SoilWater& soil_water, 
			   double T_avg, Treelog& err)
{ impl->initialize (al, geo, soil, soil_water, T_avg, err); }

OrganicMatter::OrganicMatter (Block& al)
  : impl (new Implementation (al))
{ }

OrganicMatter::~OrganicMatter ()
{ }

static bool 
check_alist (const AttributeList& al, Treelog& err)
{
  bool ok = true;

  const vector<AttributeList*>& am_alist = al.alist_sequence ("am");
  const vector<AttributeList*>& smb_alist = al.alist_sequence ("smb");
  const vector<AttributeList*>& som_alist = al.alist_sequence ("som");
  const vector<AttributeList*>& dom_alist = al.alist_sequence ("dom");

  for (unsigned int j = 0; j < am_alist.size(); j++)
    {
      std::ostringstream tmp;
      tmp << "am[" << j << "]";
      Treelog::Open nest (err, tmp.str ());
      bool am_ok = true;
      if (am_ok)
	{
	  bool om_ok = true;
	  const vector<AttributeList*>& om_alist
	    = am_alist[j]->alist_sequence ("om");
	  for (unsigned int i = 0; i < om_alist.size(); i++)
	    {
	      std::ostringstream tmp;
	      tmp << "om[" << i << "]";
	      Treelog::Open nest (err, tmp.str ());
	      vector<double> fractions
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
  for (unsigned int i = 0; i < smb_alist.size(); i++)
    {
      std::ostringstream tmp;
      tmp << "smb[" << i << "]";
      Treelog::Open nest (err, tmp.str ());
      bool om_ok = true;
      vector<double> fractions = smb_alist[i]->number_sequence ("fractions");
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
      vector<double> efficiency = smb_alist[i]->number_sequence ("efficiency");
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
  for (unsigned int i = 0; i < som_alist.size(); i++)
    {
      std::ostringstream tmp;
      tmp << "som[" << i << "]";
      Treelog::Open nest (err, tmp.str ());
      bool om_ok = true;
      vector<double> efficiency = som_alist[i]->number_sequence ("efficiency");
      if (efficiency.size () != smb_alist.size ())
	{
	  std::ostringstream tmp;
	  tmp << "You have " << efficiency.size () << " efficiency but " 
		 << smb_alist.size () << " smb";
	  err.error (tmp.str ());
	  om_ok = false;
	}
      vector<double> fractions = som_alist[i]->number_sequence ("fractions");
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
  for (unsigned int i = 0; i < dom_alist.size(); i++)
    {
      std::ostringstream tmp;
      tmp << "dom[" << i << "]";
      Treelog::Open nest (err, tmp.str ());
      bool om_ok = true;
      
      vector<double> efficiency = dom_alist[i]->number_sequence ("efficiency");
      if (efficiency.size () != smb_alist.size ())
	{
	  std::ostringstream tmp;
	  tmp << "You have " << efficiency.size () << " efficiency but " 
		 << smb_alist.size () << " smb";
	  err.error (tmp.str ());
	  om_ok = false;
	}
      vector<double> fractions = dom_alist[i]->number_sequence ("fractions");
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
  const AttributeList& init_alist = al.alist ("init");
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

void
OrganicMatter::load_syntax (Syntax& syntax, AttributeList& alist)
{ 
  alist.add ("submodel", "OrganicMatter");
  alist.add ("description", "\
Mineralization and immobilization in soil.  Hansen et.al. 1991.\n\
Recalibrated by Bruun et.al. 2002.");
  syntax.add_check (check_alist);
  syntax.add ("active_underground", Syntax::Boolean, Syntax::Const, "\
Set this flag to turn on mineralization below the root zone.");
  alist.add ("active_underground", false);
  syntax.add ("active_groundwater", Syntax::Boolean, Syntax::Const, "\
Clear this flag to turn off mineralization in groundwater.");
  alist.add ("active_groundwater", true);
  syntax.add ("K_NH4", "h^-1", Check::fraction (), Syntax::Const, 
	      "Maximal immobilization rate for ammonium.");
  alist.add ("K_NH4", 0.020833); // 0.5 / 24.
  syntax.add ("K_NO3", "h^-1", Check::fraction (), Syntax::Const, 
	      "Maximal immobilization rate for nitrate.");
  alist.add ("K_NO3", 0.020833); // 0.5 / 24.
  syntax.add_submodule ("Bioincorporation", alist, Syntax::State, "\
Biological incorporation of litter.",
			Bioincorporation::load_syntax);
  syntax.add ("NO3_source", "g N/cm^3/h", Syntax::LogOnly, Syntax::Sequence, "\
Mineralization this time step (negative numbers mean immobilization).");
  syntax.add ("NH4_source", "g N/cm^3/h", Syntax::LogOnly, Syntax::Sequence, "\
Mineralization this time step (negative numbers mean immobilization).");
  syntax.add ("fertilized_N", "g N/cm^2/h", Syntax::LogOnly,
              "Amount of organic bound nitrogen applied.\n\
This includes nitrogen incorporated directly in the soil.");
  syntax.add ("fertilized_C", "g C/cm^2/h", Syntax::LogOnly,
              "Amount of organic bound carbon applied.\n\
This includes carbon incorporated directly in the soil.");
  syntax.add ("tillage_N_top", "g N/m^2/h", Syntax::LogOnly,
              "Amount of nitrogen added to surface during tillage.\n\
This is a negative number.");
  syntax.add ("tillage_C_top", "g C/m^2/h", Syntax::LogOnly,
              "Amount of carbon added to surface during tillage.\n\
This is a negative number.");
  syntax.add ("tillage_N_soil", "g N/cm^3/h", 
              Syntax::LogOnly, Syntax::Sequence,
              "Amount of nitrogen added to soil during tillage.");
  syntax.add ("tillage_C_soil", "g C/cm^3/h",
              Syntax::LogOnly, Syntax::Sequence,
              "Amount of carbon added to surface during tillage.");
  syntax.add ("humus", "g/cm^3", Syntax::LogOnly, Syntax::Sequence,
	      "Total organic matter in the soil layer.");
  syntax.add ("total_C", "g C/cm^3", Syntax::LogOnly, Syntax::Sequence,
	      "Total organic C in the soil layer.");
  syntax.add ("total_N", "g N/cm^3", Syntax::LogOnly, Syntax::Sequence,
	      "Total organic N in the soil layer.");
  syntax.add ("CO2", "g CO_2-C/cm^3/h", Syntax::LogOnly, Syntax::Sequence,
	      "CO2 evolution in soil from all pools.");
  syntax.add ("CO2_fast", "g CO_2-C/cm^3/h", Syntax::LogOnly, Syntax::Sequence,
	      "CO2 evolution in soil from pools faster than 'CO2_threshold'.");
  syntax.add ("CO2_threshold", "h^-1", Check::fraction (), Syntax::Const, "\
Turnover rate above which pools will contribute to 'CO2_fast'.");
  alist.add ("CO2_threshold", 1e-4); // SMB2 and default AOM pools.
  syntax.add ("top_CO2", "g CO_2-C/cm^2/h", Syntax::LogOnly,
	      "CO2 evolution at surface.");
  syntax.add ("am", Librarian<AM>::library (), 
              Syntax::State, Syntax::Sequence, 
	      "Added organic matter pools.");
  vector<AttributeList*> am;
  AttributeList root (AM::default_root ());
  am.push_back (&root);
  alist.add ("am", am);
  syntax.add_submodule ("buffer", alist, Syntax::State,
			"Buffer between AOM pools and SOM.",
			Implementation::Buffer::load_syntax);

  // Create defaults for som and smb.
  Syntax smb_syntax;
  AttributeList smb_alist;
  SMB::load_syntax (smb_syntax, smb_alist);

  syntax.add_submodule_sequence ("smb", Syntax::State, "\
Soil MicroBiomass pools.\n\
Initial value will be estimated based on equilibrium with AM and SOM pools.",
				 SMB::load_syntax);
  vector<AttributeList*> SMB;
  AttributeList SMB1 (smb_alist);
  vector<double> SMB1_C_per_N;
  SMB1_C_per_N.push_back (6.7);
  SMB1.add ("C_per_N", SMB1_C_per_N);
  SMB1.add ("turnover_rate", 7.708e-6);
  vector<double> SMB1_efficiency;
  SMB1_efficiency.push_back (0.60);
  SMB1_efficiency.push_back (0.60);
  SMB1.add ("efficiency", SMB1_efficiency);
  SMB1.add ("maintenance", 7.500e-5);
  vector<double> SMB1_fractions;
  SMB1_fractions.push_back (0.0);
  SMB1_fractions.push_back (0.6);
  SMB1_fractions.push_back (0.0);
  SMB1_fractions.push_back (0.4);
  SMB1_fractions.push_back (0.0);
  SMB1.add ("fractions", SMB1_fractions);
  SMB.push_back (&SMB1);
  AttributeList SMB2 (smb_alist);
  vector<double> SMB2_C_per_N;
  SMB2_C_per_N.push_back (6.7);
  SMB2.add ("C_per_N", SMB2_C_per_N);
  SMB2.add ("turnover_rate", 4.16666666667e-4);
  vector<double> SMB2_efficiency;
  SMB2_efficiency.push_back (0.60);
  SMB2_efficiency.push_back (0.60);
  SMB2.add ("efficiency", SMB2_efficiency);
  SMB2.add ("maintenance", 4.16666666667e-4);
  vector<double> SMB2_fractions;
  SMB2_fractions.push_back (0.0);
  SMB2_fractions.push_back (0.4);
  SMB2_fractions.push_back (0.0);
  SMB2_fractions.push_back (0.6);
  SMB2_fractions.push_back (0.0);
  SMB2.add ("fractions", SMB2_fractions);
  SMB.push_back (&SMB2);
  alist.add ("smb", SMB);

  syntax.add_submodule_sequence ("som", Syntax::State, 
				 "Soil Organic Matter pools.",
				 SOM::load_syntax);
  Syntax som_syntax;
  AttributeList som_alist;
  SOM::load_syntax (som_syntax, som_alist);
  vector<AttributeList*> SOM;
  AttributeList SOM1 (som_alist);
#if 1 // SANDER_PARAMS
  SOM1.add ("turnover_rate", 4.3e-5 / 24.0 /* 1.7916667e-6 */);
#else
  SOM1.add ("turnover_rate", 2.7e-6 / 24.0 /* 1.125e-7 */);
#endif
  vector<double> SOM1_efficiency;
  SOM1_efficiency.push_back (0.40); // SMB1
  SOM1_efficiency.push_back (0.40); // SMB2
  SOM1.add ("efficiency", SOM1_efficiency);
  vector<double> SOM1_fractions;
  SOM1_fractions.push_back (1.0); // SMB1
  SOM1_fractions.push_back (0.0); // SMB2
  SOM1_fractions.push_back (0.0); // SOM1
  SOM1_fractions.push_back (0.0); // SOM2
  SOM1_fractions.push_back (0.0); // SOM3
  SOM1.add ("fractions", SOM1_fractions);
  SOM.push_back (&SOM1);
  AttributeList SOM2 (som_alist);
  SOM2.add ("turnover_rate", 1.4e-4 / 24.0 /* 5.83333333333e-6 */);
  vector<double> SOM2_efficiency;
  SOM2_efficiency.push_back (0.50); // SMB1
  SOM2_efficiency.push_back (0.50); // SMB2
  SOM2.add ("efficiency", SOM2_efficiency);
  vector<double> SOM2_fractions;
#if 1 // SANDER_PARAMS
  SOM2_fractions.push_back (0.7); // SMB1
  SOM2_fractions.push_back (0.0); // SMB2
  SOM2_fractions.push_back (0.3); // SOM1
  SOM2_fractions.push_back (0.0); // SOM2
#else
  SOM2_fractions.push_back (0.9); // SMB1
  SOM2_fractions.push_back (0.0); // SMB2
  SOM2_fractions.push_back (0.1); // SOM1
  SOM2_fractions.push_back (0.0); // SOM2
#endif
  SOM2_fractions.push_back (0.0); // SOM3
  SOM2.add ("fractions", SOM2_fractions);
  SOM.push_back (&SOM2);
  AttributeList SOM3 (som_alist);
  SOM3.add ("turnover_rate", 0.0);
  SOM3.add ("efficiency", SOM2_efficiency); // Doesn't matter.
  SOM3.add ("fractions", SOM2_fractions); // Doesn't matter.
  SOM.push_back (&SOM3);
  alist.add ("som", SOM);
  
  Syntax& layer_syntax = *new Syntax ();
  AttributeList layer_alist;
  layer_syntax.add ("end", "cm", Check::negative (), Syntax::Const,
		    "End point of this layer (a negative number).");
  layer_syntax.add ("weight", "kg C/m^2", Check::positive (),
		    Syntax::Const,
		    "organic carbon content of this layer.");
  layer_syntax.order ("end", "weight");
  syntax.add ("initial_SOM", layer_syntax, layer_alist, Syntax::OptionalConst,
	      Syntax::Sequence,
	      "Layered initialization of soil SOM content.");

  syntax.add_submodule_sequence ("dom", Syntax::State, 
				 "Dissolved Organic Matter pools.",
				 DOM::load_syntax);
  alist.add ("dom", vector<AttributeList*> ());
  syntax.add ("domsorp", Librarian<Domsorp>::library (), 
              Syntax::State, Syntax::Sequence, 
	      "Interchange between DOM and SOM pools.");
  alist.add ("domsorp", vector<AttributeList*> ());

  syntax.add ("heat_factor", "dg C", Syntax::None (), Check::non_negative (),
	      Syntax::Const,
	      "Default heat factor, used if not specified by OM pool.");
  alist.add ("heat_factor", PLF::empty ());
  syntax.add ("water_factor", "cm", Syntax::None (), Check::non_negative (),
	      Syntax::Const, "\
Default water potential factor, used if not specified by OM pool.\n\
If the PLF is empty, a build-in PLF of pF will be used instead.\n\
It is 0.6 at pF < 0, 1.0 at 1.5 < pF < 2.5, and 0 at pF > 6.5.");
  alist.add ("water_factor", PLF::empty ());
  syntax.add ("abiotic_factor", Syntax::None (), 
              Syntax::LogOnly, Syntax::Sequence,
	      "Product of current heat and water factors."); 
  syntax.add ("ClayOM", Librarian<ClayOM>::library (), "Clay effect model.");
  AttributeList clay_alist;
  clay_alist.add ("type", "old");
  PLF clay_factor;
  clay_factor.add (0.00, 1.0);
  clay_factor.add (0.25, 0.5);
  clay_factor.add (1.00, 0.5);
  clay_alist.add ("factor", clay_factor);
  alist.add ("ClayOM", clay_alist);
  syntax.add ("tillage_age", "d", Syntax::OptionalState, Syntax::Sequence,
	      "Time since the latest tillage operation was performed."); 
  syntax.add ("smb_tillage_factor", "d", Syntax::None (), 
	      Check::non_negative (), Syntax::Const, Syntax::Sequence,
	      "Tillage influence on turnover rates for each SMB pool.\n\
If no value is given, tillage will have no influence.");
  alist.add ("smb_tillage_factor", vector<const PLF*> ());
  syntax.add ("som_tillage_factor", "d", Syntax::None (), 
	      Check::non_negative (), Syntax::Const, Syntax::Sequence,
	      "Tillage influence on SOM turnover rates for each SOM pool.\n\
If no value is given, tillage will have no influence.");
  alist.add ("som_tillage_factor", vector<const PLF*> ());

  syntax.add ("min_AM_C", "g C/m^2", Check::non_negative (), Syntax::Const, 
	      "Minimal amount of carbon in AOM ensuring it is not removed.");
  alist.add ("min_AM_C", 0.5);
  //  We require 5 kg C / Ha in order to keep an AM dk:pulje.
  syntax.add ("min_AM_N", "g N/m^2", Check::non_negative (), Syntax::Const, 
	      "Minimal amount of nitrogen in AOM ensuring it is not removed.");
  // We require ½ kg N / Ha in order to keep an AM dk:pulje.
  alist.add ("min_AM_N", 0.05);
  syntax.add_submodule ("init", alist, Syntax::Const, "\
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
			OrganicMatter::Implementation
			/**/::Initialization::load_syntax);
}

static Submodel::Register 
organic_matter_submodel ("OrganicMatter", OrganicMatter::load_syntax);
