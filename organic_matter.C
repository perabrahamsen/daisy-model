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
#include "log.h"
#include "am.h"
#include "om.h"
#include "soil.h"
#include "soil_water.h"
#include "soil_NH4.h"
#include "soil_NO3.h"
#include "soil_heat.h"
#include "bioincorporation.h"
#include "time.h"
#include "mathlib.h"
#include "plf.h"
#include "tmpstream.h"
#include "submodel.h"
#include "treelog.h"
#include <algorithm>
#include <numeric>

struct OrganicMatter::Implementation
{
  // Content.
  const bool active_underground; // True, iff turnover happens below rootzone.
  const bool active_groundwater; // True, iff turnover happens in groundwater.
  const double K_NH4;		// Immobilization rate of NH4.
  const double K_NO3;		// Immobilization rate of NO3.
  vector<double> CO2;		// CO2 produced per time step.
  double top_CO2;		// CO2 produced on top of soil.
  vector <AM*> am;		// Added Organic Matter.
  const vector<OM*> smb;	// Living Organic Matter.
  const vector<OM*> som;	// Soil Organic Matter.
  struct Buffer
  {
    vector<double> C;			// Carbon.
    vector<double> N;			// Nitrogen.
    const double turnover_rate;	// Absorption.
    const int where;		// Which SOM pool does it end in?
    void output (Log& log) const;
    void tick (int i, double abiotic_factor, double N_soil, double& N_used,
	       const vector<OM*>&);
    void mix (const Geometry&, double from, double to);
    void swap (const Geometry&, double from, double middle, double to);
    static void load_syntax (Syntax& syntax, AttributeList& alist);
    void initialize (const Geometry& geometry);
    Buffer (const AttributeList& al);
  } buffer;
  const PLF heat_factor;
  const PLF water_factor;
  const PLF clay_factor;
  vector<double> tillage_age;
  const vector<const PLF*> smb_tillage_factor;
  const vector<const PLF*> som_tillage_factor;
  const double min_AM_C;	// Minimal amount of C in an AM. [g/m²]
  const double min_AM_N;	// Minimal amount of N in an AM. [g/m²]
  Bioincorporation bioincorporation;

  // Log.
  vector<double> NO3_source;
  vector<double> NH4_source;

  // Utilities.
  static bool om_compare (const OM* a, const OM* b);
  double total_N (const Geometry& ) const;
  double total_C (const Geometry& ) const;

  // Simulation.
  void add (AM& om)
  { am.push_back (&om); }
  void monthly (const Geometry& soil);
  void tick (const Soil&, const SoilWater&, const SoilHeat&,
	     SoilNO3&, SoilNH4&, Treelog& msg);
  void mix (const Geometry&, double from, double to, double penetration, 
	    const Time& time);
  void swap (const Geometry&, double from, double middle, double to, 
	     const Time& time);
  void output (Log&, const Geometry&) const;
  bool check (Treelog& err) const;

  double heat_turnover_factor (double T) const;
  double water_turnover_factor (double h) const;
  vector<double> clay_turnover_factor;

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
  AM* find_am (const string& sort, const string& part) const;
  void initialize (const AttributeList&, const Soil&, Treelog& err);
  Implementation (const AttributeList&);
  ~Implementation ();
};

void
OrganicMatter::Implementation::Buffer::output (Log& log) const
{
  log.output ("C", C);
  log.output ("N", N);
  // log.output ("turnover_rate", turnover_rate);
} 

void
OrganicMatter::Implementation::Buffer::tick (int i, double abiotic_factor,
					     double N_soil, double& N_used,
					     const vector<OM*>& som)
{
  assert (som[where]->C[i] >= 0.0);
  assert (C[i] >= 0.0);
  assert (N[i] >= 0.0);

  // assert (N_soil * 1.001 >= N_used);
  // How much can we process?
  double rate;
  if (C[i] < 1e-15)
    rate = 1.0;
  else 
    rate = min (turnover_rate * abiotic_factor, 0.1);
  double N_need = C[i] * rate / som[where]->C_per_N[i]  - N[i] * rate;

  assert (finite (rate));
  assert (finite (N_need));
  
  if (N_need > N_soil - N_used && N[i] > 0.0)
    {
      rate = (N_soil - N_used) / (C[i] / som[where]->C_per_N[i] - N[i]);
      assert (finite (rate));
      set_bound (0.0, rate, 1.0);
      N_need = C[i] * rate / som[where]->C_per_N[i]  - N[i] * rate;
      assert (finite (N_need));
    }
  // Check for NaN.
  assert (finite (rate));
  assert (finite (N_need));
  
  if (rate > 0.0)
    {
      N_used += N_need;

      // Update it.
      som[where]->C[i] += C[i] * rate;
      C[i] *= (1.0 - rate);
      N[i] *= (1.0 - rate);
    }
  assert (som[where]->C[i] >= 0.0);
  assert (C[i] >= 0.0);
  assert (N[i] >= 0.0);
}

void 
OrganicMatter::Implementation::Buffer::mix (const Geometry& geometry, 
					    double from, double to)
{
  geometry.mix (C, from, to);
  geometry.mix (N, from, to);
}

void
OrganicMatter::Implementation::Buffer::swap (const Geometry& geometry,
					     double from,
					     double middle, 
					     double to)
{
  geometry.swap (C, from, middle, to);
  geometry.swap (N, from, middle, to);
}

void
OrganicMatter::Implementation::Buffer::load_syntax (Syntax& syntax,
						    AttributeList& alist)
{
  const vector<double> empty_vector;
  syntax.add ("C", "g C/cm^3", Syntax::State, Syntax::Sequence,
	      "Buffer carbon content.");
  alist.add ("C", empty_vector);
  syntax.add ("N", "g N/cm^3", Syntax::State, Syntax::Sequence,
	      "Buffer nitrogen content.");
  alist.add ("N", empty_vector);
  syntax.add ("turnover_rate", "h^-1", Syntax::Const,
	      "Turnover rate from buffer into SOM.");
  alist.add ("turnover_rate", 1.0);
  syntax.add ("where", Syntax::Integer, Syntax::Const,
	      "The SOM pool to move the buffer content into.\n\
The first and slow SOM pool is numbered '0', the second and faster\n\
is numbered '1'.");
  alist.add ("where", 1);
}

bool 
OrganicMatter::Implementation::om_compare (const OM* a, const OM* b)
{
  return (a->C_per_N.size () == 0 ? a->initial_C_per_N : a->C_per_N[0])
    < (b->C_per_N.size () == 0 ? b->initial_C_per_N : b->C_per_N[0]);
}

double 
OrganicMatter::Implementation::total_N (const Geometry& geometry) const
{
  double result = geometry.total (buffer.N);

  for (unsigned int i = 0; i < smb.size (); i++)
    result += smb[i]->total_N (geometry);
  for (unsigned int i = 0; i < som.size (); i++)
    result += som[i]->total_N (geometry);
  for (int i = 0; i < am.size (); i++)
    result += am[i]->total_N (geometry);
  
  return result;
}

double 
OrganicMatter::Implementation::total_C (const Geometry& geometry) const
{
  double result = geometry.total (buffer.C);

  for (unsigned int i = 0; i < smb.size (); i++)
    result += smb[i]->total_C (geometry);
  for (unsigned int i = 0; i < som.size (); i++)
    result += som[i]->total_C (geometry);
  for (int i = 0; i < am.size (); i++)
    result += am[i]->total_C (geometry);
  
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

  return exp (0.47 - 0.027 * T + 0.00193 * T *T);
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
OrganicMatter::Implementation::Buffer::initialize (const Geometry& geometry)
{ 
  const unsigned int size = geometry.size ();
  // Make sure the vectors are large enough.
  while (N.size () < size)
    N.push_back (0.0);
  while (C.size () < size)
    C.push_back (0.0);
}

OrganicMatter::Implementation::Buffer::Buffer (const AttributeList& al)
  : C (al.number_sequence ("C")),
    N (al.number_sequence ("N")),
    turnover_rate (al.number ("turnover_rate")),
    where (al.integer ("where"))
{ }

void
OrganicMatter::Implementation::output (Log& log,
				       const Geometry& geometry) const
{
  log.output ("CO2", CO2);
  log.output ("top_CO2", top_CO2);
  if (log.check ("total_N") || log.check ("total_C"))
    {
      const int size = geometry.size ();

      vector<double> total_N (size, 0.0);
      vector<double> total_C (size, 0.0);
      for (int i = 0; i < size; i++)
	{
	  for (unsigned int j = 0; j < smb.size (); j++)
	    {
	      total_C[i] += smb[j]->C[i];
	      total_N[i] += smb[j]->C[i] / smb[j]->C_per_N[i];
	    }
	  for (unsigned int j = 0; j < som.size (); j++)
	    {
	      total_C[i] += som[j]->C[i];
	      total_N[i] += som[j]->C[i] / som[j]->C_per_N[i];
	    }
	  for (int j = 0; j < am.size (); j++)
	    {
	      total_C[i] += am[j]->C_at (i);
	      total_N[i] += am[j]->N_at (i);
	    }
	  total_C[i] += buffer.C[i];
	  total_N[i] += buffer.N[i];
	}
      log.output ("total_N", total_N);
      log.output ("total_C", total_C);
    }
  log.output ("tillage_age", tillage_age);
  output_list (am, "am", log, Librarian<AM>::library ());
  output_vector (smb, "smb", log);
  output_vector (som, "som", log);
  output_submodule (buffer, "buffer", log);
  if (log.check ("Bioincorporation"))
    {
      log.open ("Bioincorporation");
      bioincorporation.output (log);
      log.close ();
    }
  log.output ("NO3_source", NO3_source);
  log.output ("NH4_source", NH4_source);
}

bool
OrganicMatter::Implementation::check (Treelog& err) const
{
  bool ok = true;
  for (unsigned int i = 0; i < am.size (); i++)
    if (!am[i]->check (err))
      ok = false;
  return ok;
}

void
OrganicMatter::Implementation::monthly (const Geometry& geometry)
{
  AM* remainder = find_am ("am", "cleanup");
  if (!remainder)
    {
      remainder = &AM::create (geometry, Time (1, 1, 1, 1), AM::default_AOM (),
			       "am", "cleanup", AM::Locked);
      add (*remainder);
    }

  const int am_size = am.size ();
  vector<AM*> new_am;
  
  for (int i = 0; i < am_size; i++)
    {
      bool keep;
      
      if (am[i]->locked ())
	keep = true;
      else if (min_AM_C == 0.0)
	if (min_AM_N == 0.0)
	  // No requirement, keep it.
	  keep = true;
	else
	  // Only require N.
	  keep = (am[i]->total_N (geometry) * (100.0 * 100.0) > min_AM_N);
      else
	if (min_AM_N == 0.0)
	  // Only require C.
	  keep = (am[i]->total_C (geometry) * (100.0 * 100.0) > min_AM_C);
	else 
	  // Require either N or C.
	  keep = (am[i]->total_N (geometry) * (100.0 * 100.0) > min_AM_N
		  || am[i]->total_C (geometry) * (100.0 * 100.0) > min_AM_C);
      
      if (keep)
	new_am.push_back (am[i]);
      else
	{
#if 0
	  am[i]->pour (buffer.C, buffer.N);
#else
	  remainder->add (geometry, *am[i]);
#endif
	  delete am[i];
	}
      am[i] = NULL;
    }
  am = new_am;
}

void 
OrganicMatter::Implementation::tick (const Soil& soil, 
				     const SoilWater& soil_water, 
				     const SoilHeat& soil_heat,
				     SoilNO3& soil_NO3,
				     SoilNH4& soil_NH4,
				     Treelog& msg)
{
  const double old_N = total_N (soil);
  const double old_C = total_C (soil);

  // Create an array of all AM dk:puljer, sorted by their C_per_N.
  const int all_am_size = am.size ();
  vector<OM*> added;
  for (int i = 0; i < all_am_size; i++)
    am[i]->append_to (added);
  sort (added.begin (), added.end (), om_compare);
  
  // Clear logs.
  fill (CO2.begin (), CO2.end (), 0.0);
  fill (NO3_source.begin (), NO3_source.end (), 0.0);
  fill (NH4_source.begin (), NH4_source.end (), 0.0);
  top_CO2 = 0.0;

  // Setup arrays.
  unsigned int size = soil.size ();
  if (!active_underground && soil.zplus (size - 1) < -100.0)
    size = soil.interval_plus (min (-100.0, soil.MaxRootingDepth ())) + 1;
  if (!active_groundwater)
    size = min (soil_water.first_groundwater_node (), size);
  size = min (size, soil.size ());
  
  vector<double> N_soil (size);
  vector<double> N_used (size);
  vector<double> abiotic_factor (size);
  vector<double> clay_factor (size);
  vector<double> tillage_factor (size);
  
  for (unsigned int i = 0; i < size; i++)
    {
      assert (soil_NH4.M_left (i) >= 0);
      const double NH4 = (soil_NH4.M_left (i) < 1e-9) // 1 mg/l
	? 0.0 : soil_NH4.M_left (i) * K_NH4;
      assert (soil_NO3.M_left (i) >= 0);
      const double NO3 = (soil_NO3.M_left (i) < 1e-9) // 1 mg/l
	? 0.0 : soil_NO3.M_left (i) * K_NO3;

      N_soil[i] = NH4 + NO3;
      N_used[i] = 0.0;

      assert (finite (soil_water.h (i)));

      abiotic_factor[i] 
	= heat_turnover_factor (soil_heat.T (i)) 
	* water_turnover_factor (soil_water.h (i));
      clay_factor[i] = abiotic_factor[i] * clay_turnover_factor [i];
    }
  
  // Main processing.
  tillage_factor = clay_factor;
  if (smb_tillage_factor.size () > 0)
    for (unsigned int i = 0; i < size; i++)
      tillage_factor[i] *= (*smb_tillage_factor[0]) (tillage_age[i]) ;
  smb[0]->tick (size, &tillage_factor[0],
		&N_soil[0], &N_used[0], &CO2[0], smb, som);
  for (unsigned int j = 1; j < smb.size (); j++)
  {
    tillage_factor = abiotic_factor;
    if (smb_tillage_factor.size () > j)
      for (unsigned int i = 0; i < size; i++)
	tillage_factor[i] *= (*smb_tillage_factor[j]) (tillage_age[i]);
    smb[j]->tick (size, &tillage_factor[0],
		  &N_soil[0], &N_used[0], &CO2[0], smb, som);
  }
  for (unsigned int j = 0; j < som.size (); j++)
  {
    tillage_factor = clay_factor;
    if (som_tillage_factor.size () > j)
      for (unsigned int i = 0; i < size; i++)
	tillage_factor[i] *= (*som_tillage_factor[j]) (tillage_age[i]);
    som[j]->tick (size, &tillage_factor[0],
		  &N_soil[0], &N_used[0], &CO2[0], smb, som);
  }
  for (unsigned int j = 0; j < added.size (); j++)
    added[j]->tick (size, &abiotic_factor[0], &N_soil[0], &N_used[0], &CO2[0],
		    smb, &buffer.C[0], &buffer.N[0]);

  // Update buffer.
  for (unsigned int i = 0; i < size; i++)
      buffer.tick (i, abiotic_factor[i], N_soil[i], N_used[i], som);

  // Update source.
  for (unsigned int i = 0; i < size; i++)
    {
      assert (N_used[i] < soil_NH4.M_left (i) + soil_NO3.M_left (i));

      const double NH4 = (soil_NH4.M_left (i) < 1e-9) // 1 mg/l
	? 0.0 : soil_NH4.M_left (i) * K_NH4;
      assert (NH4 >= 0.0);

      if (N_used[i] > NH4)
	{
	  NH4_source[i] = -NH4 / dt;
	  NO3_source[i] = (NH4 - N_used[i]) / dt;
	  assert (NH4_source[i] <= 0.0);
	  assert (NO3_source[i] <= 0.0);
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
  bioincorporation.tick (soil, am, soil_heat.T (0), top_CO2);

  // Tillage time.
  for (unsigned int i = 0; i < size; i++)
    tillage_age[i] += 1.0/24.0;

  // Mass balance.
  const double new_N = total_N (soil);
  const double delta_N = old_N - new_N;
  const double N_source = soil.total (NO3_source) + soil.total (NH4_source);
  if (!approximate (delta_N, N_source) && !approximate (old_N, new_N, 1e-10))
    {
      TmpStream tmp;
      tmp () << "BUG: OrganicMatter: delta_N != NO3 + NH4 [g N/cm^2]\n"
	     << delta_N << " != " << soil.total (NO3_source)
	     << " + " << soil.total (NH4_source);
      if (N_source != 0.0)
	tmp () << " (error " 
	       << fabs (delta_N / N_source - 1.0) * 100.0 << "%)";
      msg.error (tmp.str ());
    }
  const double new_C = total_C (soil);
  const double delta_C = old_C - new_C;
  const double C_source = soil.total (CO2) + top_CO2;
  
  if (!approximate (delta_C, C_source) && !approximate (old_C, new_C, 1e-10))
    {
      TmpStream tmp;
      tmp () << "BUG: OrganicMatter: "
	"delta_C != soil_CO2 + top_CO2 [g C/cm^2]\n"
	     << delta_C << " != " 
	     << soil.total (CO2) << " + " << top_CO2;
      msg.error (tmp.str ());
    }

  // We didn't steal it all?
  for (int i = 0; i < soil.size (); i++)
    {
      assert (soil_NO3.M_left (i) >= 0.0);
      assert (soil_NH4.M_left (i) >= 0.0);
    }
}
      
void 
OrganicMatter::Implementation::mix (const Geometry& geometry,
				    double from, double to,double penetration, 
				    const Time&)
{
  buffer.mix (geometry, from, to);
  for (unsigned int i = 0; i < am.size (); i++)
    am[i]->mix (geometry, from, to, penetration);
  for (unsigned int i = 1; i < smb.size (); i++)
    smb[i]->mix (geometry, from, to, penetration);
  for (unsigned int i = 0; i < som.size (); i++)
    som[i]->mix (geometry, from, to, penetration);

  // Leave CO2 alone.

  // Reset tillage age.
  double previous = 0.0;
  for (unsigned int i = 0; i < tillage_age.size (); i++)
    {
      const double next = geometry.zplus (i);
      if (previous > to && next < from)
	tillage_age[i] = 0.0;
      previous = next;
    }
}

void 
OrganicMatter::Implementation::swap (const Geometry& geometry,
				     double from, double middle, double to, 
				     const Time&)
{
  buffer.swap (geometry, from, middle, to);
  for (unsigned int i = 0; i < am.size (); i++)
    am[i]->swap (geometry, from, middle, to);
  for (unsigned int i = 1; i < smb.size (); i++)
    smb[i]->swap (geometry, from, middle, to);
  for (unsigned int i = 0; i < som.size (); i++)
    som[i]->swap (geometry, from, middle, to);
  // Leave CO2 alone.
}

AM* 
OrganicMatter::Implementation::find_am (const string& sort,
					const string& part) const
{
  for (unsigned int i = 0; i < am.size (); i++)
    if (am[i]->locked () 
	&& am[i]->crop_name () == sort 
	&& am[i]->crop_part_name () == part)
      return am[i];
  return NULL;
}

void
OrganicMatter::Implementation::initialize (const AttributeList& al,
					   const Soil& soil, Treelog& err)
{ 
  Treelog::Open nest (err, "OrganicMatter");

  // Sizes.
  const unsigned int smb_size = smb.size ();
  const unsigned int som_size = som.size ();

  // Production.
  CO2.insert (CO2.end (), soil.size (), 0.0);
  NO3_source.insert (NO3_source.end (), soil.size (), 0.0);
  NH4_source.insert (NH4_source.end (), soil.size (), 0.0);

  // Clay.
  for (unsigned int i = 0; i < soil.size (); i++)
    clay_turnover_factor.push_back (clay_factor (soil.clay (i)));
  
  // Tillage.
  tillage_age.insert (tillage_age.end (), 
		      soil.size () - tillage_age.size (), 1000000.0);

  // Fill SMB C_per_N array with last value.
  for (unsigned int pool = 0; pool < som_size; pool++)
    {
      assert (smb[pool]->C_per_N.size () > 0);
      if (smb[pool]->C_per_N.size () > 1 
	  && smb[pool]->C_per_N.size () < soil.size ())
	err.entry ("smb C_per_N partially initialized.\n\
Filling out array with last value");
      while (smb[pool]->C_per_N.size () < soil.size ())
	smb[pool]->C_per_N.push_back 
	  (smb[pool]->C_per_N[smb[pool]->C_per_N.size () - 1]);
      assert (smb[pool]->C_per_N.size () == soil.size ());
    }

  // Initialize AM.
  for (unsigned int i = 0; i < am.size (); i++)
    am[i]->initialize (soil);

  // Initialize SOM.
  if (al.check ("initial_SOM"))
    {
      const vector<AttributeList*>& layers
	= al.alist_sequence ("initial_SOM");
      
      // Find total C in layers.
      vector<double> total (soil.size (), 0.0);
      
      const double soil_end = soil.zplus (soil.size () - 1);
      double last = 0.0;
      for (unsigned int i = 0; i < layers.size (); i++)
	{
	  double end = layers[i]->number ("end");
	  double weight = layers[i]->number ("weight"); // kg C/m²
	  assert (weight > 0);
	  assert (end < last);
	  if (end < soil_end)
	    {
	      err.entry ("\
initial_SOM layer in OrganicMatter ends below the last node");
	      weight *= (last - soil_end) / (last - end);
	      end = soil_end;
	      i = layers.size ();
	    }
	  const double C = weight * 1000.0 / (100.0 * 100.0); // g C / cm²
	  soil.add (total, last, end, C);
	  last = end;
	}

      // Distribute C in pools.
      for (unsigned int lay = 0; lay < soil.size (); lay++)
	{
	  // Examine how the C should be distributed.
	  double total_C = 0.0;
	  for (unsigned int pool = 0; pool < som_size; pool++)
	    total_C += soil.SOM_C (lay, pool);

	  // Distribute it.
	  for (unsigned int pool = 0; pool < som_size; pool++)
	    {
	      const double fraction = soil.SOM_C (lay, pool) / total_C;
	      if (som[pool]->C.size () == lay)
		som[pool]->C.push_back (total[lay] * fraction);
	      if (som[pool]->C_per_N.size () == lay)
		som[pool]->C_per_N.push_back (soil.SOM_C_per_N (lay, pool));
	    }
	}
    }
  else
    {
      for (unsigned int pool = 0; pool < som_size; pool++)
	if (som[pool]->C.size () > 0 && som[pool]->C.size () < soil.size ())
	  err.entry ("som C partially initialized.\n\
Using humus for remaining entries");
	  
      // Initialize SOM from humus in horizons.
      for (unsigned int lay = 0; lay < soil.size (); lay++)
	{
	  for (unsigned int pool = 0; pool < som_size; pool++)
	    {
	      if (som[pool]->C.size () == lay)
		som[pool]->C.push_back (soil.SOM_C (lay, pool));
	      if (som[pool]->C_per_N.size () == lay)
		som[pool]->C_per_N.push_back (soil.SOM_C_per_N (lay, pool));
	    }
	}
    }
  for (unsigned int pool = 0; pool < smb_size; pool++)
    if (smb[pool]->C.size () > 0 && smb[pool]->C.size () < soil.size ())
      err.entry ("smb C partially initialized.\n\
Using equilibrium for remaining entries");

  for (unsigned int lay = 0; lay < soil.size (); lay++)
    {
      double stolen = 0.0; // How much C was stolen by the SMB pools?
 
      // SMB C should be calculated from equilibrium.
      for (unsigned int pool = 0; pool < smb_size; pool++)
	{
	  if (smb[pool]->C.size () == lay)
	    {
	      double in = 0.0;	      // Incomming C.

	      // Add contributions from SMB pools.
	      // We ignore contributions from higher numbered SMB pools.
	      // We can do that, because that is the typical direction
	      // of the flow.
	      if (pool > 0)
		in += smb[0]->C[lay] 
		  * clay_turnover_factor[lay]
		  * smb[0]->turnover_rate 
		  * smb[0]->fractions[pool]
		  * smb[0]->efficiency[pool];

	      for (unsigned int i = 1; i < pool; i++)
		in += smb[i]->C[lay] 
		  * smb[i]->turnover_rate 
		  * smb[i]->fractions[pool]
		  * smb[i]->efficiency[pool];

	      // Add contributions from SOM pools
	      for (unsigned int i = 0; i < som_size; i++)
		in += som[i]->C[lay] 
		  * clay_turnover_factor[lay]
		  * som[i]->turnover_rate 
		  * som[i]->fractions[pool]
		  * som[i]->efficiency[pool];

	      // Add contributions from AM pools
	      vector<OM*> added;
	      for (unsigned int i = 0; i < am.size (); i++)
		am[i]->append_to (added);
	      for (unsigned int i = 0; i < added.size (); i++)
		in += added[i]->C[lay] 
		  * added[i]->turnover_rate 
		  * added[i]->fractions[pool]
		  * added[i]->efficiency[pool];

	      // Rate of outgoing C. 
	      double out_rate = smb[pool]->turnover_rate 
		+ smb[pool]->maintenance;
	      if (pool == 0)
		out_rate *= clay_turnover_factor[lay];
		
	      // content * out_rate = in  =>  content = in / out_rate;
	      smb[pool]->C.push_back (in / out_rate);
	      
	      stolen += in / out_rate;
	    }
	}
      som[0]->C[lay] -= stolen;
      assert (som[0]->C[lay] >= 0.0);
    }
  buffer.initialize (soil);
  
  // Biological incorporation.
  bioincorporation.initialize (soil);
  AM* bioam = find_am ("bio", "incorporation");
  if (bioam)
    bioincorporation.set_am (bioam);
  else
    am.push_back (bioincorporation.create_am (soil)); 
}

OrganicMatter::Implementation::Implementation (const AttributeList& al)
  : active_underground (al.flag ("active_underground")),
    active_groundwater (al.flag ("active_groundwater")),
    K_NH4 (al.number ("K_NH4")),
    K_NO3 (al.number ("K_NO3")),
    top_CO2 (0.0),
    am (map_create <AM> (al.alist_sequence ("am"))),
    smb (map_construct<OM> (al.alist_sequence ("smb"))),
    som (map_construct<OM> (al.alist_sequence ("som"))),
    buffer (al.alist ("buffer")),
    heat_factor (al.plf ("heat_factor")),
    water_factor (al.plf ("water_factor")),
    clay_factor (al.plf ("clay_factor")),
    smb_tillage_factor (al.plf_sequence ("smb_tillage_factor")),
    som_tillage_factor (al.plf_sequence ("som_tillage_factor")),
    min_AM_C (al.number ("min_AM_C")),
    min_AM_N (al.number ("min_AM_N")),
    bioincorporation (al.alist ("Bioincorporation"))
{ 
  if (al.check ("tillage_age"))
    tillage_age = al.number_sequence ("tillage_age");
}

OrganicMatter::Implementation::~Implementation ()
{
  sequence_delete (am.begin (), am.end ());
  sequence_delete (smb.begin (), smb.end ());
  sequence_delete (som.begin (), som.end ());
}

void 
OrganicMatter::monthly (const Geometry& geometry)
{
  impl.monthly (geometry); 
}

void 
OrganicMatter::tick (const Soil& soil, 
		     const SoilWater& soil_water, 
		     const SoilHeat& soil_heat,
		     SoilNO3& soil_NO3,
		     SoilNH4& soil_NH4,
		     Treelog& msg)
{
  impl.tick (soil, soil_water, soil_heat, soil_NO3, soil_NH4, msg);
}

void 
OrganicMatter::mix (const Geometry& geometry,
		    double from, double to, double penetration, 
		    const Time& time)
{
  impl.mix (geometry, from, to, penetration, time);
}

void 
OrganicMatter::swap (const Geometry& geometry,
		     double from, double middle, double to, 
		    const Time& time)
{
  impl.swap (geometry, from, middle, to, time);
}

double
OrganicMatter::CO2 (unsigned int i) const
{
  assert (impl.CO2.size () > i);
  return impl.CO2[i];
}

double 
OrganicMatter::get_smb_c_at (unsigned int i) const
{ return impl.get_smb_c_at (i); }

void 
OrganicMatter::output (Log& log, const Geometry& geometry) const
{
  impl.output (log, geometry);
}

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
	  TmpStream tmp;
	  tmp () << "[" << i << "]";
	  Treelog::Open nest (err, tmp.str ());
	  bool om_ok = true;
	  if (om_ok)
	    {
	      vector<double> fractions
		= om_alist[i]->number_sequence ("fractions");
	      if (fractions.size () != impl.smb.size () + 1)
		{
		  TmpStream tmp;
		  tmp () << "You have " << fractions.size ()
			 << " fractions but " << impl.smb.size ()
			 << " smb and one buffer";
		  err.entry (tmp.str ());
		  ok = false;
		}
	      double sum
		= accumulate (fractions.begin (), fractions.end (), 0.0);
	      if (fabs (sum - 1.0) > 0.0001)
		{
		  TmpStream tmp;
		  tmp () << "The sum of all fractions is " << sum;
		  err.entry (tmp.str ());
		  ok = false;
		}
	    }
	  else ok = false;
	}
    }
  return ok;
}

bool
OrganicMatter::check (Treelog& err) const
{
  return impl.check (err);
}

void 
OrganicMatter::add (AM& am)
{
  impl.add (am);
}

AM* 
OrganicMatter::find_am (const string& sort, const string& part) const
{ return impl.find_am (sort, part); }

void
OrganicMatter::initialize (const AttributeList& al, const Soil& soil, 
			   Treelog& err)
{ impl.initialize (al, soil, err); }

OrganicMatter::OrganicMatter (const AttributeList& al)
  : impl (*new Implementation (al))
{ }

OrganicMatter::~OrganicMatter ()
{
  delete &impl;
}

static bool 
check_alist (const AttributeList& al, Treelog& err)
{
  bool ok = true;

  const vector<AttributeList*>& am_alist = al.alist_sequence ("am");
  const vector<AttributeList*>& smb_alist = al.alist_sequence ("smb");
  const vector<AttributeList*>& som_alist = al.alist_sequence ("som");

  for (unsigned int j = 0; j < am_alist.size(); j++)
    {
      TmpStream tmp;
      tmp () << "am[" << j << "]";
      Treelog::Open nest (err, tmp.str ());
      bool am_ok = true;
      if (am_ok)
	{
	  bool om_ok = true;
	  const vector<AttributeList*>& om_alist
	    = am_alist[j]->alist_sequence ("om");
	  for (unsigned int i = 0; i < smb_alist.size(); i++)
	    {
	      TmpStream tmp;
	      tmp () << "om[" << i << "]";
	      Treelog::Open nest (err, tmp.str ());
	      vector<double> fractions
		= om_alist[i]->number_sequence ("fractions");
	      if (fractions.size () != smb_alist.size () + 1)
		{
		  TmpStream tmp;
		  tmp () << "You have " << fractions.size ()
			 << " fractions but " << smb_alist.size ()
			 << " smb and one buffer";
		  err.entry (tmp.str ());
		  om_ok = false;
		}
	      double sum
		= accumulate (fractions.begin (), fractions.end (), 0.0);
	      if (fabs (sum - 1.0) > 0.0001)
		{
		  TmpStream tmp;
		  tmp () << "The sum of all fractions is " << sum;
		  err.entry (tmp.str ());
		  om_ok = false;
		}
	      if (!om_ok)
		am_ok = false;
	    }
	}
      if (!am_ok)
	ok = false;
    }

  for (unsigned int i = 0; i < smb_alist.size(); i++)
    {
      TmpStream tmp;
      tmp () << "smb[" << i << "]";
      Treelog::Open nest (err, tmp.str ());
      bool om_ok = true;
      vector<double> fractions = smb_alist[i]->number_sequence ("fractions");
      if (fractions.size () != smb_alist.size () + som_alist.size ())
	{
	  TmpStream tmp;
	  tmp () << "You have " << fractions.size () << " fractions but " 
		 << smb_alist.size () << " smb and " << som_alist.size ()
		 << " som";
	  err.entry (tmp.str ());
	  om_ok = false;
	}
      vector<double> efficiency = smb_alist[i]->number_sequence ("efficiency");
      if (efficiency.size () != smb_alist.size ())
	{
	  TmpStream tmp;
	  tmp () << "You have " << efficiency.size () << " efficiency but " 
		 << smb_alist.size () << " smb";
	  err.entry (tmp.str ());
	  om_ok = false;
	}
      double sum = accumulate (fractions.begin (), fractions.end (), 0.0);
      if (fabs (sum - 1.0) > 0.0001)
	{
	  TmpStream tmp;
	  tmp () << "The sum of all fractions is " << sum;
	  err.entry (tmp.str ());
	  om_ok = false;
	}
      if (!om_ok)
	ok = false;
    }

  for (unsigned int i = 0; i < som_alist.size(); i++)
    {
      TmpStream tmp;
      tmp () << "som[" << i << "]";
      Treelog::Open nest (err, tmp.str ());
      bool om_ok = true;
      vector<double> efficiency = som_alist[i]->number_sequence ("efficiency");
      if (efficiency.size () != smb_alist.size ())
	{
	  TmpStream tmp;
	  tmp () << "You have " << efficiency.size () << " efficiency but " 
		 << smb_alist.size () << " smb";
	  err.entry (tmp.str ());
	  om_ok = false;
	}
      vector<double> fractions = som_alist[i]->number_sequence ("fractions");
      if (fractions.size () != smb_alist.size () + som_alist.size ())
	{
	  TmpStream tmp;
	  tmp () << "You have " << fractions.size () << " fractions but " 
		 << smb_alist.size () << " smb and " << som_alist.size ()
		 << " som";
	  err.entry (tmp.str ());
	  om_ok = false;
	}
      double sum = accumulate (fractions.begin (), fractions.end (), 0.0);
      if (fabs (sum - 1.0) > 0.0001)
	{
	  TmpStream tmp;
	  tmp () << "The sum of all fractions is " << sum;
	  err.entry (tmp.str ());
	  om_ok = false;
	}
      if (!om_ok)
	ok = false;
    }

  return ok;
}

void
OrganicMatter::load_syntax (Syntax& syntax, AttributeList& alist)
{ 
  alist.add ("submodel", "OrganicMatter");
  alist.add ("description", "\
Mineralization and immobilization in soil.  Hansen et.al. 1991.");
  syntax.add_check (check_alist);
  syntax.add ("active_underground", Syntax::Boolean, Syntax::Const, "\
Set this flag to turn on mineralization below the root zone.");
  alist.add ("active_underground", false);
  syntax.add ("active_groundwater", Syntax::Boolean, Syntax::Const, "\
Clear this flag to turn off mineralization in groundwater.");
  alist.add ("active_groundwater", true);
  syntax.add ("K_NH4", "h^-1", Syntax::Const, 
	      "Maximal immobilization rate for ammonium.");
  alist.add ("K_NH4", 0.020833); // 0.5 / 24.
  syntax.add ("K_NO3", "h^-1", Syntax::Const, 
	      "Maximal immobilization rate for nitrate.");
  alist.add ("K_NO3", 0.020833); // 0.5 / 24.
  syntax.add_submodule ("Bioincorporation", alist, Syntax::State, "\
Biological incorporation of litter.",
			Bioincorporation::load_syntax);
  syntax.add ("NO3_source", "g N/cm^3/h", Syntax::LogOnly, Syntax::Sequence, "\
Mineralization this time step (negative numbers mean immobilization).");
  syntax.add ("NH4_source", "g N/cm^3/h", Syntax::LogOnly, Syntax::Sequence, "\
Mineralization this time step (negative numbers mean immobilization).");
  syntax.add ("total_C", "g C/cm^2", Syntax::LogOnly, Syntax::Sequence,
	      "Total organic C in the soil layer.");
  syntax.add ("total_N", "g N/cm^2", Syntax::LogOnly, Syntax::Sequence,
	      "Total organic N in the soil layer.");
  syntax.add ("CO2", "g CO_2-C/cm^3/h", Syntax::LogOnly, Syntax::Sequence,
	      "CO2 evolution in soil.");
  syntax.add ("top_CO2", "g CO_2-C/cm^2/h", Syntax::LogOnly,
	      "CO2 evolution at surface.");
  syntax.add ("am", Librarian<AM>::library (), Syntax::Sequence, 
	      "Added organic matter pools.");
  vector<AttributeList*> am;
  AttributeList root (AM::default_root ());
  am.push_back (&root);
  alist.add ("am", am);
  syntax.add_submodule ("buffer", alist, Syntax::State,
			"Buffer between AOM pools and SOM.",
			Implementation::Buffer::load_syntax);

  // Create defaults for som and smb.
  Syntax om_syntax;
  AttributeList om_alist;
  OM::load_syntax (om_syntax, om_alist);

  syntax.add_submodule_sequence ("smb", Syntax::State, "\
Soil MicroBiomass pools.\n\
Initial value will be estimated based on equilibrium with AM and SOM pools.",
				 OM::load_syntax);
  vector<AttributeList*> SMB;
  AttributeList SMB1 (om_alist);
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
  SMB1.add ("fractions", SMB1_fractions);
  SMB.push_back (&SMB1);
  AttributeList SMB2 (om_alist);
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
  SMB2.add ("fractions", SMB2_fractions);
  SMB.push_back (&SMB2);
  alist.add ("smb", SMB);

  syntax.add_submodule_sequence ("som", Syntax::State, 
				 "Soil Organic Matter pools.",
				 OM::load_syntax);
  vector<AttributeList*> SOM;
  AttributeList SOM1 (om_alist);
  SOM1.add ("turnover_rate", 1.125e-7);
  vector<double> SOM1_efficiency;
  SOM1_efficiency.push_back (0.40);
  SOM1_efficiency.push_back (0.40);
  SOM1.add ("efficiency", SOM1_efficiency);
  vector<double> SOM1_fractions;
  SOM1_fractions.push_back (1.0);
  SOM1_fractions.push_back (0.0);
  SOM1_fractions.push_back (0.0);
  SOM1_fractions.push_back (0.0);
  SOM1.add ("fractions", SOM1_fractions);
  SOM.push_back (&SOM1);
  AttributeList SOM2 (om_alist);
  SOM2.add ("turnover_rate", 5.83333333333e-6);
  vector<double> SOM2_efficiency;
  SOM2_efficiency.push_back (0.50);
  SOM2_efficiency.push_back (0.50);
  SOM2.add ("efficiency", SOM2_efficiency);
  vector<double> SOM2_fractions;
  SOM2_fractions.push_back (0.9);
  SOM2_fractions.push_back (0.0);
  SOM2_fractions.push_back (0.1);
  SOM2_fractions.push_back (0.0);
  SOM2.add ("fractions", SOM2_fractions);
  SOM.push_back (&SOM2);
  alist.add ("som", SOM);
  
  Syntax& layer_syntax = *new Syntax ();
  AttributeList layer_alist;
  layer_syntax.add ("end", "cm", Syntax::Const,
		    "End point of this layer (a negative number).");
  layer_syntax.add ("weight", "kg C/m^2", Syntax::Const,
		    "organic carbon content of this layer.");
  layer_syntax.order ("end", "weight");
  syntax.add ("initial_SOM", layer_syntax, layer_alist, Syntax::OptionalConst,
	      Syntax::Sequence,
	      "Layered initialization of soil SOM content.");
  PLF empty;
  syntax.add ("heat_factor", "dg C", Syntax::None (), Syntax::Const,
	      "Heat factor.");
  alist.add ("heat_factor", empty);
  syntax.add ("water_factor", "cm", Syntax::None (), Syntax::Const,
	      "Water potential factor.");
  alist.add ("water_factor", empty);
  syntax.add ("clay_factor", Syntax::Fraction (), Syntax::None (), 
	      Syntax::Const, "Clay fraction factor.");
  PLF clay;
  clay.add (0.00, 1.0);
  clay.add (0.25, 0.5);
  clay.add (1.00, 0.5);
  alist.add ("clay_factor", clay);
  syntax.add ("tillage_age", "days", Syntax::OptionalState, Syntax::Sequence,
	      "Time since the latest tillage operation was performed."); 
  syntax.add ("smb_tillage_factor", "days", Syntax::None (), 
	      Syntax::Const, Syntax::Sequence,
	      "Tillage influence on turnover rates for each SMB pool.\n\
If no value is given, tillage will have no influence.");
  alist.add ("smb_tillage_factor", vector<const PLF*> ());
  syntax.add ("som_tillage_factor", "days", Syntax::None (), 
	      Syntax::Const, Syntax::Sequence,
	      "Tillage influence on SOM turnover rates for each SOM pool.\n\
If no value is given, tillage will have no influence.");
  alist.add ("som_tillage_factor", vector<const PLF*> ());

  syntax.add ("min_AM_C", "g C/m^2", Syntax::Const, 
	      "Minimal amount of carbon in AOM ensuring it is not removed.");
  alist.add ("min_AM_C", 0.5);
  //  We require 5 kg C / Ha in order to keep an AM dk:pulje.
  syntax.add ("min_AM_N", "g N/m^2", Syntax::Const, 
	      "Minimal amount of nitrogen in AOM ensuring it is not removed.");
  // We require ½ kg N / Ha in order to keep an AM dk:pulje.
  alist.add ("min_AM_N", 0.05);
}

static Submodel::Register 
organic_matter_submodel ("OrganicMatter", OrganicMatter::load_syntax);
