// organic_matter.C

#include "organic_matter.h"
#include "syntax.h"
#include "alist.h"
#include "log.h"
#include "filter.h"
#include "aom.h"
#include "soil.h"
#include "soil_water.h"
#include "soil_NH4.h"
#include "soil_NO3.h"
#include "soil_heat.h"
#include "mathlib.h"
#include <algorithm>


struct OrganicMatter::Implementation
{
  // Content.
  const double K_NH4;		// Absorption rate of NH4 from soil.
  const double K_NO3;		// Absorption rate of NO3 from soil.
  double CO2;			// CO2 produced per time step.
  vector <AOM*> aom;		// Added Organic Matter.
  const vector<OM*> smb;	// Living Organic Matter.
  const vector<OM*> som;	// Soil Organic Matter.
  struct Buffer
  {
    vector<double> C;			// Carbon.
    vector<double> N;			// Nitrogen.
    const double turnover_rate;	// Absorption.
    const int where;		// Which SOM dk:pulje does it end in?
    void output (Log& log, const Filter& filter) const;
    void tick (int i, double turnover_factor, double N_soil, double& N_used,
	       const vector<OM*>);
    static void load_syntax (Syntax& syntax, AttributeList& alist);
    Buffer (const AttributeList& al);
  } buffer;

  // Simulation.
  void add (AOM& om)
  { aom.push_back (&om); }
  void tick (const Soil&, const SoilWater&, const SoilHeat&, 
	     SoilNO3&, SoilNH4&);
  void output (Log& log, const Filter& filter) const;
  bool check () const;

private:
  double heat_turnover_factor (double /* T */)
  { return 1.0; }		// TODO
  double water_turnover_factor (double /* T */)
  { return 1.0; }		// TODO

  // Create & Destroy.
public:
  Implementation (const AttributeList& al);
};

void
OrganicMatter::Implementation::Buffer::output (Log& log,
					       const Filter& filter) const
{
  log.output ("C", filter, C);
  log.output ("N", filter, N);
  // log.output ("turnover_rate", filter, turnover_rate);
} 

void
OrganicMatter::Implementation::Buffer::tick (int i, double turnover_factor,
					     double N_soil, double& N_used,
					     const vector<OM*> som)
{
  // Make sure the vectors are large enough.
  while (N.size () < i + 0U)
    N.push_back (0.0);
  while (C.size () < i + 0U)
    C.push_back (0.0);

  // How much can we process?
  double rate = turnover_rate * turnover_factor;
  double N_need = C[i] * rate / som[where]->C_per_N  - N[i] * rate;

  if (N_need > N_soil - N_used)
    {
      rate = (N_soil - N_used) / (N[i] - C[i] / som[where]->C_per_N);
      N_need = C[i] * rate / som[where]->C_per_N  - N[i] * rate;
      // Check that we calculated the right rate.
      assert ((N_soil == N_used)
	      ? (abs (N_need) < 1e-10)
	      : (abs (1.0 - N_need / (N_soil - N_used)) < 0.01));
    }
  N_used += N_need;

  // Update it.
  som[where]->C[i] += C[i] * rate;
  C[i] *= (1.0 - rate);
  N[i] *= (1.0 - rate);
}

void
OrganicMatter::Implementation::Buffer::load_syntax (Syntax& syntax,
						    AttributeList& alist)
{
  syntax.add ("C", Syntax::Number, Syntax::State, Syntax::Sequence);
  alist.add ("C", *new vector<double>);
  syntax.add ("N", Syntax::Number, Syntax::State, Syntax::Sequence);
  alist.add ("N", *new vector<double>);
  syntax.add ("turnover_rate", Syntax::Number, Syntax::Const);
  alist.add ("turnover_rate", 1.0);
  syntax.add ("where", Syntax::Integer, Syntax::Const);
  alist.add ("where", 1);
}

OrganicMatter::Implementation::Buffer::Buffer (const AttributeList& al)
  : C (al.number_sequence ("C")),
    N (al.number_sequence ("N")),
    turnover_rate (al.number ("turnover_rate")),
    where (al.integer ("where"))
{ }

void
OrganicMatter::Implementation::output (Log& log, const Filter& filter) const
{
  log.output ("CO2", filter, CO2, true);
  output_list (aom, "am", log, filter);
  output_vector (smb, "smb", log, filter);
  output_vector (som, "som", log, filter);
  output_submodule (buffer, "buffer", log, filter);
}

bool
OrganicMatter::Implementation::check () const
{
  bool ok = true;
  for (unsigned int i = 0; i < aom.size (); i++)
    if (!aom[i]->check ())
      ok = false;
  if (!ok)
    cerr << "in OrganicMatter\n";
  return ok;
}

static bool om_compare (const OM* a, const OM* b)
{
  return a->C_per_N < b->C_per_N;
}

void 
OrganicMatter::Implementation::tick (const Soil& soil, 
				     const SoilWater& soil_water, 
				     const SoilHeat& soil_heat,
				     SoilNO3& soil_NO3,
				     SoilNH4& soil_NH4)
{
  CO2 = 0.0;			// Initialize for this time step.

  // Create an array of all AOM dk:puljer, sorted by their C_per_N.
  const int all_aom_size = aom.size ();
  vector<OM*> added;
  for (int i = 0; i < all_aom_size; i++)
    {
      int aom_size = aom[i]->om.size ();
      for (int j = 0; j < aom_size; j++)
	added.push_back (aom[i]->om[j]);
    }
  sort (added.begin (), added.end (), om_compare);
  
  // Soil solutes.
  vector<double> NO3_source;
  vector<double> NH4_source;

  // Main processing,
  for (int i = 0; i < soil.size (); i++)
    {
      const double NH4
	= soil_NH4.C (i) * soil_water.Theta (i) * K_NH4;
      const double NO3 
	= soil_NO3.C (i) * soil_water.Theta (i) * K_NO3;
      const double N_soil = NH4	+ NO3;

      double N_used = 0.0;

      const double turnover_factor 
	= heat_turnover_factor (soil_heat.temperature (i))
	* water_turnover_factor (soil_water.h (i));

      // Handle all the OM dk:puljer
      for (unsigned int j = 0; j < smb.size (); j++)
	smb[j]->tick (i, turnover_factor, N_soil, N_used, CO2, smb, som);
      for (unsigned int j = 0; j < som.size (); j++)
	som[j]->tick (i, turnover_factor, N_soil, N_used, CO2, smb, som);
      for (unsigned int j = 0; j < added.size (); j++)
	added[j]->tick (i, turnover_factor, N_soil, N_used, CO2,
			smb, buffer.C[i], buffer.N[i]);

      // Handle the buffer.
      buffer.tick (i, turnover_factor, N_soil, N_used, som);
      
      // Update soil solutes.
      if (N_used > NH4)
	{
	  NH4_source.push_back (- NH4);
	  NO3_source.push_back (NH4 - N_used);
	}
      else
	{
	  NH4_source.push_back (- N_used);
	  NO3_source.push_back (0.0);
	}
    }

  // Update soil solutes.
  soil_NO3.add_to_source (NO3_source);
  soil_NH4.add_to_source (NH4_source);
}
      
OrganicMatter::Implementation::Implementation (const AttributeList& al)
  : K_NH4 (al.number ("K_NH4")),
    K_NO3 (al.number ("K_NO3")),
    CO2 (0.0),
    aom (map_construct <AOM> (al.list_sequence ("am"))),
    smb (map_construct <OM> (al.list_sequence ("smb"))),
    som (map_construct <OM> (al.list_sequence ("som"))),
    buffer (al.list ("buffer"))
{ }

void 
OrganicMatter::tick (const Soil& soil, 
		     const SoilWater& soil_water, 
		     const SoilHeat& soil_heat,
		     SoilNO3& soil_NO3,
		     SoilNH4& soil_NH4)
{
  impl.tick (soil, soil_water, soil_heat, soil_NO3, soil_NH4);
}

void 
OrganicMatter::output (Log& log, const Filter& filter) const
{
  impl.output (log, filter);
}

bool
OrganicMatter::check (const AttributeList& al)
{
  bool ok = true;

  const vector<const AttributeList*>& smb_alist = al.list_sequence ("smb");
  for (unsigned int i = 0; i < smb_alist.size(); i++)
    {
      bool om_ok = true;
      ::check (*smb_alist[i], "C_per_N", om_ok);
      ::check (*smb_alist[i], "turnover_rate", om_ok);
      ::check (*smb_alist[i], "efficiency", om_ok);
      ::check (*smb_alist[i], "maintenance", om_ok);
      if (!om_ok)
	{
	  cerr << "in smb[" << i << "]\n";
	  ok = false;
	}
    }

  const vector<const AttributeList*>& som_alist = al.list_sequence ("som");
  for (unsigned int i = 0; i < som_alist.size(); i++)
    {
      bool om_ok = true;
      ::check (*som_alist[i], "C_per_N", om_ok);
      ::check (*som_alist[i], "turnover_rate", om_ok);
      ::check (*som_alist[i], "efficiency", om_ok);
      if (!om_ok)
	{
	  cerr << "in som[" << i << "]\n";
	  ok = false;
	}
    }

  if (!ok)
    cerr << "in OrganicMatter\n";

  return ok;
}

bool
OrganicMatter::check () const
{
  return impl.check ();
}

void 
OrganicMatter::add (AOM& aom)
{
  impl.add (aom);
}

OrganicMatter::OrganicMatter (const AttributeList& al)
  : impl (*new Implementation (al))
{ }

OrganicMatter::~OrganicMatter ()
{
  delete &impl;
}

void
OrganicMatter::load_syntax (Syntax& syntax, AttributeList& alist)
{ 
  syntax.add ("K_NH4", Syntax::Number, Syntax::Const);
  syntax.add ("K_NO3", Syntax::Number, Syntax::Const);
  syntax.add ("CO2", Syntax::Number, Syntax::LogOnly);
  syntax.add ("am", AOM::library (), Syntax::State, Syntax::Sequence);
  syntax.add ("smb", OM::syntax (), Syntax::State, 2);
  syntax.add ("som", OM::syntax (), Syntax::State, 2);
  add_submodule<Implementation::Buffer> ("buffer", syntax, alist);
}
