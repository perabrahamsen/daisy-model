// organic_matter.C

#include "organic_matter.h"
#include "syntax.h"
#include "alist.h"
#include "log.h"
#include "filter.h"
#include "am.h"
#include "soil.h"
#include "soil_water.h"
#include "soil_NH4.h"
#include "soil_NO3.h"
#include "soil_heat.h"
#include "mathlib.h"
#include "csmp.h"
#include <algorithm>
#include <ieeefp.h>

struct OrganicMatter::Implementation
{
  // Content.
  const double K_NH4;		// Immobilization rate of NH4.
  const double K_NO3;		// Immobilization rate of NO3.
  vector<double> CO2;		// CO2 produced per time step.
  vector <AM*> am;		// Added Organic Matter.
  const vector<OM*> smb;	// Living Organic Matter.
  const vector<OM*> som;	// Soil Organic Matter.
  struct Buffer
  {
    vector<double> C;			// Carbon.
    vector<double> N;			// Nitrogen.
    const double turnover_rate;	// Absorption.
    const int where;		// Which SOM dk:pulje does it end in?
    void output (Log& log, const Filter& filter) const;
    void tick (int i, double abiotic_factor, double N_soil, double& N_used,
	       const vector<OM*>&);
    void mix (const Soil&, double from, double to);
    void swap (const Soil&, double from, double middle, double to);
    static void load_syntax (Syntax& syntax, AttributeList& alist);
    Buffer (const Soil&, const AttributeList& al);
  } buffer;
  const double min_AM_C;	// Minimal amount of C in an AM. [g/m²]
  const double min_AM_N;	// Minimal amount of N in an AM. [g/m²]

  // Log.
  vector<double> NO3_source;
  vector<double> NH4_source;

  // Simulation.
  void add (AM& om)
  { am.push_back (&om); }
  void monthly (const Soil& soil);
  void tick (const Soil&, const SoilWater&, const SoilHeat&, 
	     SoilNO3&, SoilNH4&);
  void mix (const Soil&, double from, double to, double penetration);
  void swap (const Soil& soil, double from, double middle, double to);
  void output (Log&, const Filter&, const Soil&) const;
  bool check () const;

  double heat_turnover_factor (double T) const;
  double water_turnover_factor (double h) const;
  vector<double> clay_turnover_factor;

  // Create & Destroy.
  Implementation (const Soil&, const AttributeList& al);
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
OrganicMatter::Implementation::Buffer::tick (int i, double abiotic_factor,
					     double N_soil, double& N_used,
					     const vector<OM*>& som)
{
  // assert (N_soil * 1.001 >= N_used);
  // How much can we process?
  double rate = turnover_rate * abiotic_factor;
  double N_need = C[i] * rate / som[where]->C_per_N[i]  - N[i] * rate;

  assert (finite (rate));
  assert (finite (N_need));
  
  if (N_need > N_soil - N_used && N[i] > 0.0)
    {
      rate = (N_soil - N_used) / (C[i] / som[where]->C_per_N[i] - N[i]);
      assert (finite (rate));
      N_need = C[i] * rate / som[where]->C_per_N[i]  - N[i] * rate;
      assert (finite (N_need));
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
OrganicMatter::Implementation::Buffer::mix (const Soil& soil, 
					    double from, double to)
{
  soil.mix (C, from, to);
  soil.mix (N, from, to);
}

void
OrganicMatter::Implementation::Buffer::swap (const Soil& soil,
					     double from,
					     double middle, 
					     double to)
{
  soil.swap (C, from, middle, to);
  soil.swap (N, from, middle, to);
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
    return 0.1 * T;

  return exp (0.47 - 0.027 * T + 0.00193 * T *T);
}

double
OrganicMatter::Implementation::water_turnover_factor (double h) const
{
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

OrganicMatter::Implementation::Buffer::Buffer (const Soil& soil, 
					       const AttributeList& al)
  : C (al.number_sequence ("C")),
    N (al.number_sequence ("N")),
    turnover_rate (al.number ("turnover_rate")),
    where (al.integer ("where"))
{ 
  // Make sure the vectors are large enough.
  while (N.size () < soil.size () +0U)
    N.push_back (0.0);
  while (C.size () < soil.size () +0U)
    C.push_back (0.0);
}

void
OrganicMatter::Implementation::output (Log& log, const Filter& filter,
				       const Soil& soil) const
{
  log.output ("CO2", filter, CO2, true);
  if (filter.check ("total_N", true) || filter.check ("total_C", true))
    {
      const int size = soil.size ();

      double total_N = 0.0;
      double total_C = 0.0;
      for (int i = 0; i < size; i++)
	{
	  double new_total_C = 0.0;
	  double new_total_N = 0.0;

	  for (unsigned int j = 0; j < smb.size (); j++)
	    {
	      new_total_C += smb[j]->C[i];
	      new_total_N += smb[j]->C[i] / smb[j]->C_per_N[i];
	    }
	  for (unsigned int j = 0; j < som.size (); j++)
	    {
	      new_total_C += som[j]->C[i];
	      new_total_N += som[j]->C[i] / som[j]->C_per_N[i];
	    }
	  const int all_am_size = am.size ();
	  for (int k = 0; k < all_am_size; k++)
	    {
	      const int am_size = am[i]->om.size ();
	      for (int j = 0; j < am_size; j++)
		{
		  new_total_C += am[k]->om[j]->C[i];
		  new_total_N += am[k]->om[j]->C[i] / am[k]->om[j]->C_per_N[i];
		}
	    }
	  new_total_C += buffer.C[i];
	  new_total_N += buffer.N[i];

	  total_C += new_total_C * soil.dz (i);
	  total_N += new_total_N * soil.dz (i);
	}
      log.output ("total_N", filter, total_N, true);
      log.output ("total_C", filter, total_C, true);
    }
  output_list (am, "am", log, filter);
  output_vector (smb, "smb", log, filter);
  output_vector (som, "som", log, filter);
  output_submodule (buffer, "buffer", log, filter);
  log.output ("NO3_source", filter, NO3_source);
  log.output ("NH4_source", filter, NH4_source);
}

bool
OrganicMatter::Implementation::check () const
{
  bool ok = true;
  for (unsigned int i = 0; i < am.size (); i++)
    if (!am[i]->check ())
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
OrganicMatter::Implementation::monthly (const Soil& soil)
{
  const int am_size = am.size ();
  vector<AM*> new_am;
  
  cerr << "\nThere are " << am_size << " AM dk:puljer.\n";
  
  for (int i = 0; i < am_size; i++)
    {
      bool keep;

      if (min_AM_C == 0.0)
	if (min_AM_N == 0.0)
	  // No requirement, keep it.
	  keep = true;
	else
	  // Only require N.
	  keep = (am[i]->total_N (soil) * (100.0 * 100.0) > min_AM_N);
      else
	if (min_AM_N == 0.0)
	  // Only require C.
	  keep = (am[i]->total_C (soil) * (100.0 * 100.0) > min_AM_C);
	else 
	  // Require either N or C.
	  keep = (am[i]->total_N (soil) * (100.0 * 100.0) > min_AM_N
		  || am[i]->total_C (soil) * (100.0 * 100.0) > min_AM_C);
      
      cerr << (keep ? "Keeping " : "Removing ") << am[i]->name 
	   << " (" << am[i]->creation.year ()
	   << "-" << am[i]->creation.month () 
	   << "-" << am[i]->creation.mday () << ") N = " 
	   << am[i]->total_N (soil) * (100.0 * 100.0) 
	   << ", C = "  << am[i]->total_C (soil) * (100.0 * 100.0) 
	   << ".\n";

      if (keep)
	new_am.push_back (am[i]);
      else
	{
	  am[i]->pour (buffer.C, buffer.N);
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
				     SoilNH4& soil_NH4)
{
  // Create an array of all AM dk:puljer, sorted by their C_per_N.
  const int all_am_size = am.size ();
  vector<OM*> added;
  for (int i = 0; i < all_am_size; i++)
    {
      int am_size = am[i]->om.size ();
      for (int j = 0; j < am_size; j++)
	added.push_back (am[i]->om[j]);
    }
  sort (added.begin (), added.end (), om_compare);
  
  // Setup arrays.
  const int size = soil.size ();
  vector<double> N_soil (size);
  vector<double> N_used (size);
  vector<double> abiotic_factor (size);
  vector<double> clay_factor (size);
  
  for (int i = 0; i < size; i++)
    {
      CO2[i] = 0.0;		
      NO3_source[i] = 0.0;
      NH4_source[i] = 0.0;

      const double NH4 = soil_NH4.M_left (i) * K_NH4;
      const double NO3 = soil_NO3.M_left (i) * K_NO3;
      assert (NH4 >= 0.0 && NO3 >= 0.0);

      N_soil[i] = NH4 + NO3;
      N_used[i] = 0.0;

      assert (finite (soil_water.h (i)));

      abiotic_factor[i] 
	= heat_turnover_factor (soil_heat.T (i)) 
	* water_turnover_factor (soil_water.h (i));
      clay_factor[i] = abiotic_factor[i] * clay_turnover_factor [i];
    }

  // Main processing.
  smb[0]->tick (&clay_factor[0], &N_soil[0], &N_used[0], &CO2[0], smb, som);
  for (unsigned int j = 1; j < smb.size (); j++)
    smb[j]->tick (&abiotic_factor[0], &N_soil[0], &N_used[0], &CO2[0],
		  smb, som);
  for (unsigned int j = 0; j < som.size (); j++)
    som[j]->tick (&clay_factor[0], &N_soil[0], &N_used[0], &CO2[0], smb, som);
  for (unsigned int j = 0; j < added.size (); j++)
    added[j]->tick (&abiotic_factor[0], &N_soil[0], &N_used[0], &CO2[0],
		    smb, &buffer.C[0], &buffer.N[0]);

  for (int i = 0; i < size; i++)
    {
      const double NH4 = soil_NH4.M_left (i) * K_NH4;

      buffer.tick (i, abiotic_factor[i], N_soil[i], N_used[i], som);

      if (N_used[i] > N_soil[i])
	{
	  // cerr << "\nBUG: Adding " << N_used - N_soil << " mystery N\n";
	  N_used[i] = N_soil[i];
	}

      // Update soil solutes.
      if (N_used[i] > NH4)
	{
	  NH4_source[i] = -NH4 / dt;
	  NO3_source[i] = (NH4 - N_used[i]) / dt;
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
}
      
void 
OrganicMatter::Implementation::mix (const Soil& soil,
				    double from, double to, double penetration)
{
  buffer.mix (soil, from, to);
  for (unsigned int i = 0; i < am.size (); i++)
    am[i]->mix (soil, from, to, penetration);
  for (unsigned int i = 1; i < smb.size (); i++)
    smb[i]->mix (soil, from, to, penetration);
  for (unsigned int i = 0; i < som.size (); i++)
    som[i]->mix (soil, from, to, penetration);
  // Leave CO2 alone.
}

void 
OrganicMatter::Implementation::swap (const Soil& soil,
				     double from, double middle, double to)
{
  buffer.swap (soil, from, middle, to);
  for (unsigned int i = 0; i < am.size (); i++)
    am[i]->swap (soil, from, middle, to);
  for (unsigned int i = 1; i < smb.size (); i++)
    smb[i]->swap (soil, from, middle, to);
  for (unsigned int i = 0; i < som.size (); i++)
    som[i]->swap (soil, from, middle, to);
  // Leave CO2 alone.
}

OrganicMatter::Implementation::Implementation (const Soil& soil, 
					       const AttributeList& al)
  : K_NH4 (al.number ("K_NH4")),
    K_NO3 (al.number ("K_NO3")),
    am (map_create1 <AM, const Soil&> (al.list_sequence ("am"), soil)),
    smb (map_construct1 <OM, const Soil&> (al.list_sequence ("smb"), soil)),
    som (map_construct1 <OM, const Soil&> (al.list_sequence ("som"), soil)),
    buffer (soil, al.list ("buffer")),
    min_AM_C (al.number ("min_AM_C")),
    min_AM_N (al.number ("min_AM_N"))

{ 
  // Production.
  CO2.insert (CO2.end(), soil.size(), 0.0);
  NO3_source.insert (NO3_source.end (), soil.size(), 0.0);
  NH4_source.insert (NH4_source.end (), soil.size(), 0.0);

  // Clay.
  for (int i = 0; i < soil.size (); i++)
    {
      const double a = 2.0;
      const double X_c_prime = 0.25;
      const double X_c = soil.clay (i);
      clay_turnover_factor.push_back (1.0 - a * (min (X_c, X_c_prime)));
    }
  
  // Humus.
  const vector<const AttributeList*>& som_al = al.list_sequence ("som");
  const vector<const AttributeList*>& smb_al = al.list_sequence ("smb");

  // Find total of fractions and missing C per N.
  double total = 0.0;
  int missing_C_per_N = -1;

  for (unsigned int i = 0; i < som.size (); i++)
    {
      if (som_al[i]->check ("initial_fraction"))
	total += som_al[i]->number ("initial_fraction");
      if (som[i]->C_per_N.size () == 0)
	{
	  assert (missing_C_per_N == -1);
	  missing_C_per_N = i;
	}
      else
	assert (som[i]->C_per_N.size () == soil.size () + 0U);
      assert (som[i]->C.size () == soil.size () + 0U);
    }
  for (unsigned int i = 0; i < smb.size (); i++)
    {
      if (smb_al[i]->check ("initial_fraction"))
	total += smb_al[i]->number ("initial_fraction");

      assert (smb[i]->C_per_N.size () == soil.size () + 0U);
      assert (som[i]->C.size () == soil.size () + 0U);
    }

  // If any fractions were specified, distribute soil humus.
  if (total > 0.0)
    {
      if (missing_C_per_N == -1)
	THROW ("At least one C per N must be left unspecified in OM");

      for (int l = 0; l < soil.size (); l++)
	{
	  const double C = soil.initial_C (l);
	  double N = soil.initial_N (l);

	  for (int i = 0; i +0U < som.size (); i++)
	    {
	      const double fraction 
		= som_al[i]->check ("initial_fraction")
		? som_al[i]->number ("initial_fraction")
		: 0.0;

	      som[i]->C[l] = C * fraction / total;
	      if (missing_C_per_N != i)
		N -= som[i]->C[l] / som[i]->C_per_N[l];
	    }
	  for (int i = 0; i +0U < smb.size (); i++)
	    {
	      const double fraction 
		= smb_al[i]->check ("initial_fraction")
		? smb_al[i]->number ("initial_fraction")
		: 0.0;

	      smb[i]->C[l] = C * fraction / total;
	      N -= smb[i]->C[l] / smb[i]->C_per_N[l];
	    }
	  assert (som[missing_C_per_N]->C_per_N.size () == l + 0U);
	  if (N <= 0.0)
	    THROW ("Used up all N in OrganicMatter initialization");
	  const double missing = som[missing_C_per_N]->C[l] / N;
	  som[missing_C_per_N]->C_per_N.push_back (missing);
	  assert (som[missing_C_per_N]->C_per_N[l] >= 0.0);
	}
    }
}

void 
OrganicMatter::monthly (const Soil& soil)
{
  impl.monthly (soil); 
}

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
OrganicMatter::mix (const Soil& soil,
		    double from, double to, double penetration)
{
  impl.mix (soil, from, to, penetration);
}

void 
OrganicMatter::swap (const Soil& soil, double from, double middle, double to)
{
  impl.swap (soil, from, middle, to);
}

double
OrganicMatter::CO2 (int i) const
{
  return impl.CO2[i];
}

void 
OrganicMatter::output (Log& log, const Filter& filter, const Soil& soil) const
{
  impl.output (log, filter, soil);
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
      bool am_ok = true;
      ::check (*am_alist[j], "om", am_ok);
      if (am_ok)
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
		  am_ok = false;
		}
	    }
	}
      if (!am_ok)
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
OrganicMatter::add (AM& am)
{
  impl.add (am);
}

OrganicMatter::OrganicMatter (const Soil& soil, const AttributeList& al)
  : impl (*new Implementation (soil, al))
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
  syntax.add ("NO3_source", Syntax::Number, Syntax::LogOnly, Syntax::Sequence);
  syntax.add ("NH4_source", Syntax::Number, Syntax::LogOnly, Syntax::Sequence);
  syntax.add ("total_C", Syntax::Number, Syntax::LogOnly);
  syntax.add ("total_N", Syntax::Number, Syntax::LogOnly);
  syntax.add ("CO2", Syntax::Number, Syntax::LogOnly, Syntax::Sequence);
  syntax.add ("am", AM::library (), Syntax::State, Syntax::Sequence);
  add_submodule<Implementation::Buffer> ("buffer", syntax, alist);
  add_submodule<OM> ("smb", syntax, alist, Syntax::State, Syntax::Sequence);
  add_submodule<OM> ("som", syntax, alist, Syntax::State, Syntax::Sequence);
  syntax.add ("heat_turnover_factor", Syntax::CSMP, Syntax::Const);
  syntax.add ("water_turnover_factor", Syntax::CSMP, Syntax::Const);
  syntax.add ("min_AM_C", Syntax::Number, Syntax::Const);
  alist.add ("min_AM_C", 0.0);
  syntax.add ("min_AM_N", Syntax::Number, Syntax::Const);
  // We require ½ kg N / Ha in order to keep an AM dk:pulje.
  alist.add ("min_AM_N", 0.05);
}
