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
#include "csmp.h"
#include <algorithm>

struct OrganicMatter::Implementation
{
  // Content.
  const double K_NH4;		// Absorption rate of NH4 from soil.
  const double K_NO3;		// Absorption rate of NO3 from soil.
  vector<double> CO2;		// CO2 produced per time step.
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
	       const vector<OM*>&);
    static void load_syntax (Syntax& syntax, AttributeList& alist);
    void initialize (const Soil& soil);
    Buffer (const AttributeList& al);
  } buffer;

  // Simulation.
  void add (AOM& om)
  { aom.push_back (&om); }
  void tick (const Soil&, const SoilWater&, const SoilHeat&, 
	     SoilNO3&, SoilNH4&);
  void output (Log& log, const Filter& filter) const;
  bool check () const;

  double heat_turnover_factor (double T) const;
  double water_turnover_factor (double h) const;
  vector<double> clay_turnover_factor;

  // Create & Destroy.
  void initialize (const Soil& soil);
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
					     const vector<OM*>& som)
{
  // How much can we process?
  double rate = turnover_rate * turnover_factor;
  double N_need = C[i] * rate / som[where]->C_per_N[i]  - N[i] * rate;

  // Check for NaN.
  assert (N_need >= 0.0 || N_need <= 0.0);
  assert (rate >= 0.0 || rate <= 1.0);
  
  if (N_need > N_soil - N_used)
    {
      rate = (N_soil - N_used) / (N[i] - C[i] / som[where]->C_per_N[i]);
      N_need = C[i] * rate / som[where]->C_per_N[i]  - N[i] * rate;
      // Check that we calculated the right rate.
      assert ((N_soil == N_used)
	      ? (abs (N_need) < 1e-10)
	      : (abs (1.0 - N_need / (N_soil - N_used)) < 0.01));
    }
  N_used += N_need;

  // Check for NaN.
  assert (N_need >= 0.0 || N_need <= 0.0);
  assert (rate >= 0.0 || rate <= 1.0);

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

double 
OrganicMatter::Implementation::heat_turnover_factor (double T) const
{
  if (T < 0.0)
    return 0.0;
  if (T < 20.0)
    return 0.01 * T;

  return exp (0.47 - 0.027 * T + 0.00193 * T *T);
}

double
OrganicMatter::Implementation::water_turnover_factor (double h) const
{
  if (h > -10e-2)
    return 0.6;
  if (h > -pow (10.0, -0.5))
    return 0.6 + 0.4 * log10 (-100.0 * h) / 1.5;
  if (h > -pow (10.0, 0.5))
    return 1.0;
  if (h > -pow (10, 4.5))
    return 1.0 - log10 (-100.0 * h) / 4.0;

  return 0;
}

void
OrganicMatter::Implementation::Buffer::initialize (const Soil& soil)
{
  // Make sure the vectors are large enough.
  while (N.size () < soil.size () +0U)
    N.push_back (0.0);
  while (C.size () < soil.size () +0U)
    C.push_back (0.0);
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
  return a->C_per_N[0] < b->C_per_N[0];
}

void 
OrganicMatter::Implementation::tick (const Soil& soil, 
				     const SoilWater& soil_water, 
				     const SoilHeat& soil_heat,
				     SoilNO3& soil_NO3,
				     SoilNH4& soil_NH4)
{
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
      CO2[i] = 0.0;		// Initialize for this time step.

      const double NH4 = soil_NH4.M_left (i) * K_NH4;
      const double NO3 = soil_NO3.M_left (i) * K_NO3;
      const double N_soil = NH4	+ NO3;

      assert (NH4 >= 0.0 && NO3 >= 0.0);

      double N_used = 0.0;

      const double turnover_factor 
	= heat_turnover_factor (soil_heat.T (i)) 
	* water_turnover_factor (soil_water.pF (i));

      const double clay_factor = turnover_factor * clay_turnover_factor [i];

      // Handle all the OM dk:puljer
      smb[0]->tick (i, clay_factor, N_soil, N_used, CO2[i], smb, som);
      for (unsigned int j = 1; j < smb.size (); j++)
	smb[j]->tick (i, turnover_factor, N_soil, N_used, CO2[i], smb, som);
      for (unsigned int j = 0; j < som.size (); j++)
	som[j]->tick (i, clay_factor, N_soil, N_used, CO2[i], smb, som);
      for (unsigned int j = 0; j < added.size (); j++)
	added[j]->tick (i, turnover_factor, N_soil, N_used, CO2[i],
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
      
void 
OrganicMatter::Implementation::initialize (const Soil& soil)
{
  buffer.initialize (soil);
  for (int i = 0; i +0U < aom.size (); i++)
    aom[i]->initialize (soil);
  for (int i = 0; i +0U < smb.size (); i++)
    smb[i]->initialize (soil);
  for (int i = 0; i +0U < som.size (); i++)
    som[i]->initialize (soil);
  CO2.insert (CO2.end(), soil.size(), 0.0);
  
  for (int i = 0; i < soil.size (); i++)
    {
      const double a = 2.0;
      const double X_c_prime = 0.025;
      const double X_c = soil.clay (i);
      clay_turnover_factor.push_back (1.0 - a * (min (X_c, X_c_prime)));
    }
}

OrganicMatter::Implementation::Implementation (const AttributeList& al)
  : K_NH4 (al.number ("K_NH4")),
    K_NO3 (al.number ("K_NO3")),
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

double
OrganicMatter::CO2 (int i) const
{
  return impl.CO2[i];
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

  const vector<const AttributeList*>& am_alist = al.list_sequence ("am");
  const vector<const AttributeList*>& smb_alist = al.list_sequence ("smb");
  const vector<const AttributeList*>& som_alist = al.list_sequence ("som");

  for (unsigned int j = 0; j < am_alist.size(); j++)
    {
      bool aom_ok = true;
      ::check (*am_alist[j], "om", aom_ok);
      if (aom_ok)
	{
	  bool om_ok = true;
	  const vector<const AttributeList*>& om_alist
	    = am_alist[j]->list_sequence ("om");
	  for (unsigned int i = 0; i < smb_alist.size(); i++)
	    {
#if 0
	      ::check (*om_alist[i], "C_per_N", om_ok);
#endif
	      ::check (*om_alist[i], "turnover_rate", om_ok);
	      ::check (*om_alist[i], "efficiency", om_ok);
	      vector<double> fractions
		= om_alist[i]->number_sequence ("fractions");
	      if (fractions.size () != smb_alist.size () + 1)
		{
		  cerr << "You have " << fractions.size ()
		       << " fractions but " << smb_alist.size ()
		       << " smb and one buffer.\n";
		  om_ok = false;
		}
	      double sum
		= accumulate (fractions.begin (), fractions.end (), 0.0);
	      if (abs (sum - 1.0) > 0.0001)
		{
		  cerr << "The sum of all fractions is " << sum << "\n";
		  om_ok = false;
		}
	      if (!om_ok)
		{
		  cerr << "in om[" << i << "]\n";
		  aom_ok = false;
		}
	    }
	}
      if (!aom_ok)
	{
	  cerr << "in am[" << j << "]\n";
	  ok = false;
	}
    }

  for (unsigned int i = 0; i < smb_alist.size(); i++)
    {
      bool om_ok = true;
#if 0
      ::check (*smb_alist[i], "C_per_N", om_ok);
#endif
      ::check (*smb_alist[i], "turnover_rate", om_ok);
      ::check (*smb_alist[i], "maintenance", om_ok);
      vector<double> fractions = smb_alist[i]->number_sequence ("fractions");
      if (fractions.size () != smb_alist.size () + som_alist.size ())
	{
	  cerr << "You have " << fractions.size () << " fractions but " 
	       << smb_alist.size () << " smb and " << som_alist.size ()
	       << " som.\n";
	  om_ok = false;
	}
      vector<double> efficiency = smb_alist[i]->number_sequence ("efficiency");
      if (efficiency.size () != smb_alist.size ())
	{
	  cerr << "You have " << efficiency.size () << " efficiency but " 
	       << smb_alist.size () << " smb.\n";
	  om_ok = false;
	}
      double sum = accumulate (fractions.begin (), fractions.end (), 0.0);
      if (abs (sum - 1.0) > 0.0001)
	{
	  cerr << "The sum of all fractions is " << sum << "\n";
	  om_ok = false;
	}
      if (!om_ok)
	{
	  cerr << "in smb[" << i << "]\n";
	  ok = false;
	}
    }

  for (unsigned int i = 0; i < som_alist.size(); i++)
    {
      bool om_ok = true;
#if 0
      ::check (*som_alist[i], "C_per_N", om_ok);
#endif
      ::check (*som_alist[i], "turnover_rate", om_ok);
      vector<double> efficiency = som_alist[i]->number_sequence ("efficiency");
      if (efficiency.size () != smb_alist.size ())
	{
	  cerr << "You have " << efficiency.size () << " efficiency but " 
	       << smb_alist.size () << " smb.\n";
	  om_ok = false;
	}
      vector<double> fractions = som_alist[i]->number_sequence ("fractions");
      if (fractions.size () != smb_alist.size () + som_alist.size ())
	{
	  cerr << "You have " << fractions.size () << " fractions but " 
	       << smb_alist.size () << " smb and " << som_alist.size ()
	       << " som.\n";
	  om_ok = false;
	}
      double sum = accumulate (fractions.begin (), fractions.end (), 0.0);
      if (abs (sum - 1.0) > 0.0001)
	{
	  cerr << "The sum of all fractions is " << sum << "\n";
	  om_ok = false;
	}
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
OrganicMatter::check_am (const AttributeList& am) const
{
  bool ok = true;
  ::check (am, "om", ok);
  if (ok)
    {
      const vector<const AttributeList*>& om_alist
	= am.list_sequence ("om");
      
      for (unsigned int i = 0; i < om_alist.size(); i++)
	{
	  bool om_ok = true;
	  ::check (*om_alist[i], "fractions", ok);
	  if (om_ok)
	    {
	      vector<double> fractions
		= om_alist[i]->number_sequence ("fractions");
	      if (fractions.size () != impl.smb.size () + 1)
		{
		  cerr << "You have " << fractions.size ()
		       << " fractions but " << impl.smb.size ()
		       << " smb and one buffer.\n";
		  om_ok = false;
		}
	      double sum
		= accumulate (fractions.begin (), fractions.end (), 0.0);
	      if (abs (sum - 1.0) > 0.0001)
		{
		  cerr << "The sum of all fractions is " << sum << "\n";
		  om_ok = false;
		}
	    }
	  if (!om_ok)
	    {
	      cerr << "in om[" << i << "]\n";
	      ok = false;
	    }
	}
    }
  if (!ok)
    cerr << "in added matter `" << am.name ("type") << "'\n";
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

void 
OrganicMatter::initialize (const Soil& soil)
{
  impl.initialize (soil);
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
  syntax.add ("CO2", Syntax::Number, Syntax::LogOnly, Syntax::Sequence);
  syntax.add ("am", AOM::library (), Syntax::State, Syntax::Sequence);
  syntax.add ("smb", OM::syntax (), Syntax::State, Syntax::Sequence);
  syntax.add ("som", OM::syntax (), Syntax::State, Syntax::Sequence);
  add_submodule<Implementation::Buffer> ("buffer", syntax, alist);
  syntax.add ("heat_turnover_factor", Syntax::CSMP, Syntax::Const);
  syntax.add ("water_turnover_factor", Syntax::CSMP, Syntax::Const);
}
