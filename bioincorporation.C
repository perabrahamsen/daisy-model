// bioincorporation.C --- Biological incorporation of organic matter in soil. 

#include "bioincorporation.h"
#include "alist.h"
#include "syntax.h"
#include "log.h"
#include "soil.h"
#include "am.h"
#include "submodel.h"
#include "plf.h"
#include "time.h"
#include "om.h"
#include "mathlib.h"
#include <algorithm>

#ifdef BORLAND_TEMPLATES
template class add_submodule_sequence<OM>;
#endif

struct Bioincorporation::Implementation
{ 
  // Parameters.
  const double R_max;
  const double k_half;
  const PLF C_per_N_factor;
  const PLF T_factor;
  const double respiration;
  const PLF distribution;
  vector<double> density;
  const vector<AttributeList*>& AOM; // Stem AM parameters.
  
  // Content.
  AM* aom;

  // Log.
  double C;
  double N;

  // Utitlites.
  static bool am_compare (const AM* a, const AM* b);

  // Simulation.
  void tick (const Geometry&, vector <AM*>& am, double T, double& CO2);
  void output (Log&) const;

  // Create and destroy.
  void initialize (const Soil&);
  AM* create_am (const Geometry& geometry);
  void set_am (AM*);
  Implementation (const AttributeList& al);
};

bool 
Bioincorporation::Implementation::am_compare (const AM* a, const AM* b)
{
  const double a_top_C = a->top_C ();
  if (a_top_C > 0)
    {
      const double b_top_C = b->top_C ();
      if (b_top_C == 0.0)
	return true;
      
      const double a_top_N = a->top_N ();
      const double b_top_N = b->top_N ();
      assert (a_top_N > 0.0);
      assert (b_top_N > 0.0);
      return a_top_C / a_top_N < b_top_C / b_top_N;
    }
  return false;
}

static const double DM_to_C = 0.420; // C fraction of DM.
static const double C_to_DM = 1.0 / DM_to_C;
static const double m2_per_cm2 = 0.0001;
static const double cm2_per_m2 = 1.0 / m2_per_cm2;
static const double surface_to_soil = DM_to_C * m2_per_cm2;
static const double soil_to_surface = 1.0 / surface_to_soil;

void
Bioincorporation::Implementation::tick (const Geometry& geometry, 
					vector <AM*>& am, double T, 
					double& CO2)
{
  // No bioincorporation.
  if (R_max == 0.0)
    return;

  // Clear old log variables.
  C = 0.0;
  N = 0.0;

  // Check available bioincorporation.
  const double R_total = R_max * T_factor (T) * surface_to_soil;// [g C/cm^2/h]
  if (R_total < 1.0e-10)
    return;
  const double k_total = k_half * surface_to_soil;// [g C/cm^2]

  // Eat from each AM, lowest C/N first.
  const unsigned int am_size = am.size ();
  sort (am.begin (), am.end (), am_compare);
  double available = R_total * dt;	// [g C/cm^2]
  double last_C_per_N = 0.0;

  for (unsigned int i = 0; i < am_size; i++)
  {
    const double top_C = am[i]->top_C ();

    // No more worthwhile AOM pools.
    if (top_C < 1e-30)
      break;
    
    // Find how much to take from this AOM.
    const double top_N = am[i]->top_N ();
    assert (top_N > 0.0);
    const double C_per_N = top_C / top_N;
    if (C_per_N < last_C_per_N)
      CERR << "Bug: C/N (" << C_per_N << ") < last C/N ("
	   << last_C_per_N <<")\n";
    double speed
      = R_total * C_per_N_factor (C_per_N) * top_C / (top_C + k_total);

    // Don't take more than the bioincorporation can handle.
    if (speed * dt > available)
      speed = available / dt;

    if (speed * dt > top_C)
      {
	// Take all.
	am[i]->multiply_top (0.0);
	assert (am[i]->top_C () == 0.0);
	assert (am[i]->top_N () == 0.0);
	C += top_C * (1.0 - respiration);
	N += top_N;
      }
    else
      {
	// Take some.
	const double fraction = speed * dt / top_C;
	C += speed * dt * (1.0 - respiration);
	N += top_N * fraction;
	am[i]->multiply_top (1.0 - fraction);
	assert (approximate (am[i]->top_C (), top_C - speed * dt));
	assert (approximate (am[i]->top_N (), top_N * (1.0 - fraction)));
      }

    // No more available bioincorporation.
    available -= speed * dt;
    if (available < 1.0e-10)
      break;

    // Next pool.
    last_C_per_N = C_per_N;
  }

  // Add bioincorporation to soil.
  assert (aom);
  aom->add (geometry, C, N, density);
  
  // Update CO2.
  CO2 += respiration * C / (1.0 - respiration);

  // Update log variables.
  C *= cm2_per_m2;
  N *= cm2_per_m2;
}
  
void 
Bioincorporation::Implementation::output (Log& log) const
{ 
  if (log.check ("CO2"))
    log.output ("CO2", respiration * C / (1.0 - respiration));
  if (log.check ("DM"))
    log.output ("DM", C * C_to_DM);
  log.output ("C", C);
  log.output ("N", N);
}

void 
Bioincorporation::Implementation::initialize (const Soil& soil)
{ 
  // Calculate distribution density for all nodes.
  double last = 0.0;
  for (unsigned int i = 0;
       i < soil.size () && last > soil.MaxRootingDepth ();
       i++)
    {
      const double next = soil.zplus (i);
      const double dz = last - next;
      assert (approximate (dz, soil.dz (i)));
      const double total = distribution.integrate (next, last);
      density.push_back (total / dz);
      last = next;
    }
}

AM*
Bioincorporation::Implementation::create_am (const Geometry& geometry)
{ 
  aom = &AM::create (geometry, Time (1, 1, 1, 1), AOM,
		     "bio", "incorporation", AM::Locked); 
  return aom;
}

void 
Bioincorporation::Implementation::set_am (AM* am)
{ aom = am; }

Bioincorporation::Implementation::Implementation (const AttributeList& al)
  : R_max (al.number ("R_max")),
    k_half (al.number ("k_half")),
    C_per_N_factor (al.plf ("C_per_N_factor")),
    T_factor (al.plf ("T_factor")),
    respiration (al.number ("respiration")),
    distribution (al.plf ("distribution")), 
    AOM (al.alist_sequence ("AOM")),
    C (0.0),
    N (0.0)
{ }

void 
Bioincorporation::tick (const Geometry& geometry, vector <AM*>& am, double T,
			double& CO2)
{
  impl.tick (geometry, am, T, CO2);
}

void 
Bioincorporation::output (Log& log) const
{
  impl.output (log);
}

void 
Bioincorporation::initialize (const Soil& soil)
{ impl.initialize (soil); }

AM*
Bioincorporation::create_am (const Geometry& geometry)
{ return impl.create_am (geometry); }

void 
Bioincorporation::set_am (AM* am)
{ impl.set_am (am); }

void
Bioincorporation::load_syntax (Syntax& syntax, AttributeList& alist)
{ 
  // Submodel.
  alist.add ("submodel", "Bioincorporation");
  alist.add ("description", 
	     "Biological incorporation of organic matter in soil.");

  // Incorporation speed.
  syntax.add ("R_max", "g DM/m^2/h", Syntax::Const, 
	      "Maximal speed of incorporation.");
  alist.add ("R_max", 0.3);
  syntax.add ("k_half", "g DM/m^2", Syntax::Const, "Halflife constant.");
  alist.add ("k_half", 1.0);
  syntax.add ("speed", "g DM/m^2/h", Syntax::LogOnly, 
	      "Fraction of litter incorporated this hour.\n\
The formula is speed = (R_max * litter) / (k_half + litter).");
  syntax.add ("C_per_N_factor", "(g C/cm^2)/(g N/cm^2)", Syntax::None (),
	      Syntax::Const, "Limiting factor for high C/N ratio.");
  PLF C_per_N_factor;
  C_per_N_factor.add (40.0, 1.0);
  C_per_N_factor.add (50.0, 0.1);
  C_per_N_factor.add (120.0, 0.01);
  
  alist.add ("C_per_N_factor", C_per_N_factor);
  syntax.add ("T_factor", "dg C", Syntax::None (), Syntax::Const, 
	      "Limiting factor for low temperature.");
  PLF T_factor;
  T_factor.add (4.0, 0.0);
  T_factor.add (6.0, 1.0);
  alist.add ("T_factor", T_factor);

  // Incorporation amounts.
  syntax.add ("respiration", Syntax::Fraction (), Syntax::Const,
	      "Fraction of C lost in respiration.");
  alist.add ("respiration", 0.5);
  syntax.add ("DM", "g DM/m^2/h", Syntax::LogOnly, 
	      "DM incorporated this hour.");
  syntax.add ("C", "g C/m^2/h", Syntax::LogOnly, "C incorporated this hour.");
  syntax.add ("N", "g N/m^2/h", Syntax::LogOnly, "N incorporated this hour.");
  syntax.add ("CO2", "g C/m^2/h", Syntax::LogOnly, "C respirated this hour.");

  // Incorporation location.
  syntax.add ("distribution", "cm", Syntax::None (), Syntax::Const,
	      "Distribution of incorporated matter in the soil.\n\
\(X, Y), where X is the depth (negative numbers), and Y is the relative\n\
weight in that depth.  To get the fraction in a specific interval [a:b], we\n\
integrate the plf over that interval, and divide by the integration over\n\
the whole profile.");
  PLF distribution;
  distribution.add (-80.0, 0.0);
  distribution.add (-18.0, 100.0);
  distribution.add (0.0, 100.0);
  alist.add ("distribution", distribution);

  // Incorporated AM parameters.
  Syntax om_syntax;
  AttributeList om_alist;
  OM::load_syntax (om_syntax, om_alist);
  AttributeList& AOM1 = *new AttributeList (om_alist);
  AttributeList& AOM2 = *new AttributeList (om_alist);
  AOM1.add ("initial_fraction", 0.80);
  vector<double> CN;
  CN.push_back (60.0);
  AOM1.add ("C_per_N", CN);
  vector<double> efficiency1;
  efficiency1.push_back (0.50);
  efficiency1.push_back (0.50);
  AOM1.add ("efficiency", efficiency1);
  AOM1.add ("turnover_rate", 2.0e-4);
  vector<double> fractions1;
  fractions1.push_back (0.50);
  fractions1.push_back (0.50);
  fractions1.push_back (0.00);
  AOM1.add ("fractions", fractions1);
  vector<double> efficiency2;
  efficiency2.push_back (0.50);
  efficiency2.push_back (0.50);
  AOM2.add ("efficiency", efficiency2);
  AOM2.add ("turnover_rate", 2.0e-3);
  vector<double> fractions2;
  fractions2.push_back (0.00);
  fractions2.push_back (1.00);
  fractions2.push_back (0.00);
  AOM2.add ("fractions", fractions2);
  vector<AttributeList*> AOM;
  AOM.push_back (&AOM1);
  AOM.push_back (&AOM2);
  add_submodule_sequence<OM> ("AOM", syntax, Syntax::Const, 
			      "Incorporated AM parameters.");
  alist.add ("AOM", AOM);
}
  
Bioincorporation::Bioincorporation (const AttributeList& al)
  : impl (*new Implementation (al))
{ }

Bioincorporation::~Bioincorporation ()
{ }

static Submodel::Register bioincorporation_submodel
/**/ ("Bioincorporation", Bioincorporation::load_syntax);
