// horizon.C

#include "horizon.h"
#include "library.h"
#include "alist.h"
#include "syntax.h"
#include "csmp.h"
#include "hydraulic.h"
#include "mathlib.h"
#include "tortuosity.h"
#include <vector>
#include <map>
#include <numeric>

struct Horizon::Implementation
{
  // Content.
  const double clay;
  const double silt;
  const double fine_sand;
  const double coarse_sand;
  const double humus;
  const double quarts_in_clay;
  const double quarts_in_silt;
  const double quarts_in_sand;
  double dry_bulk_density;

  // Organic matter.
  const vector<double> SOM_C_per_N;
  const vector<double> SOM_fractions;
  double C_factor;

  // Strange things.
  const double quarts_form_factor;
  const double mineral_form_factor;

  // Heat Capacity and Conductivity.
  double C_soil;
  vector<double> K_water;
  vector<double> K_ice;
  
  // Initialize.
  // Note:  These variables are really not used after initialization.
  enum constituents
  { 
    Water, Ice, Air, Quarts, Minerals, Organic_Matter,
    Constituents_End,
    Constituents_Start = Water
  };
  double content[Constituents_End];
  double Theta_pF_high;
  const Hydraulic* hydraulic;
  void initialize (const Hydraulic&);
  double HeatCapacity ();
  double DepolationsFactor (const constituents medium, const double alfa);
  double ThermalConductivity (constituents medium);
  const int intervals;
  double rho_soil_particles ();

  static const double heat_capacity[Constituents_End];
  
  // Create.
  static double weight (const AttributeList& al, string name);
  Implementation (const AttributeList& al);
};

const double 
Horizon::Implementation::heat_capacity[Constituents_End] = // [erg / cm³ / °C]
// Ice is given as equivalent amount of water.
{ 4.2e7, 1.9e7 * (1.0 / 0.92), 1.25e4, 2.0e7, 2.0e7, 2.5e7 }; 

void 
Horizon::Implementation::initialize (const Hydraulic& hydro)
{
  hydraulic = &hydro;

  // Did we specify `dry_bulk_density'?  Else calculate it now.
  if (dry_bulk_density < 0.0)
    dry_bulk_density = rho_soil_particles () * (1.0 - hydraulic->porosity ());

  // C factor is the specific C content in horizon [g C/cm³]
  const double C_divisor 
    = accumulate (SOM_fractions.begin (), SOM_fractions.end (), 0.0);
  
  C_factor = dry_bulk_density * humus * c_fraction_in_humus;
  if (C_divisor > 0.0)
    C_factor /= C_divisor;
  else
    CERR << "Horizon: No C fractions given.\n";

  // The particles are not in a real continuous medium.  Try to correct.
  const double continuum_correction_factor = 1.25;
      
  // Quarts content in soil.
  const double quarts = clay * quarts_in_clay
    + silt * quarts_in_silt
    + (fine_sand + coarse_sand) * quarts_in_sand;

  // Above this pF heat is mostly tranfered by Air.
  Theta_pF_high = hydraulic->Theta (pF2h (4.2));
      
  // Below this pf heat is mostly transfered by Water or Ice.
  const double Theta_pF_low
    = (hydraulic->Theta (pF2h (2.0)) + Theta_pF_high) / 2.0;

  // Water that won't freeze.
  const double LiquidWater = Theta_pF_high; 
  
  // Relative content of various constituents in soil.
  content[Quarts] = quarts * (1.0 - hydraulic->Theta_sat);
  content[Minerals] = ((clay + silt + fine_sand + coarse_sand) - quarts)
    * (1.0 - hydraulic->Theta_sat);
  content[Organic_Matter] = humus * (1.0 - hydraulic->Theta_sat);

  // Find capasity of dry soil.
  content[Air] = hydraulic->porosity ();
  content[Water] = 0.0;
  content[Ice] = 0.0;
  C_soil = HeatCapacity ();

  // We calculate for water between Theta_res and Theta_sat.
  const int from = (int) floor (intervals * hydraulic->Theta_res);
  const int to = (int) ceil (intervals * hydraulic->Theta_sat);
  
  assert (0 <= from);
  assert (from < to);
  assert (to < intervals);

  // Make room, make room...
  K_water.insert (K_water.end (), intervals, 0.0);
  K_ice.insert (K_ice.end (), intervals, 0.0);

  for (int i = from; i < to; i++)
    {
      // Fill out water, ice, and air for pure water system.
      content[Water] = (i + 0.0) / (intervals + 0.0);
      content[Ice] = 0.0;
      content[Air] = hydraulic->porosity () - content[Water];

      // Calculate termal attributes for this combination.
      const double K_water_wet = ThermalConductivity (Water);
      const double K_water_dry = continuum_correction_factor
	* ThermalConductivity (Air);
      
      // Find actual conductivity in combined water and air system.
      if (content[Water] < Theta_pF_high)
	K_water[i] = K_water_dry;
      else if (content[Water] > Theta_pF_low)
	K_water[i] = K_water_wet;
      else
	K_water[i] = K_water_dry 
	  + (K_water_wet - K_water_dry)
	  * (content[Water] - Theta_pF_high)
	  / (Theta_pF_low - Theta_pF_high);
      
      // Fill out water, ice, and air for pure ice system.
      content[Water] = min (LiquidWater, (i + 0.0) / (intervals + 0.0));
      content[Ice] = max ((i + 0.0) / (intervals + 0.0) - LiquidWater, 0.0);
      content[Air] = hydraulic->Theta_sat - (content[Water] + content[Ice]);
      
      // Calculate termal attributes for this combination.
      const double K_ice_wet = ThermalConductivity (Ice);
      const double K_ice_dry = continuum_correction_factor 
	* ThermalConductivity (Air);
      
      // Find actual conductivity in combined ice and air system.
      if (content[Water] + content[Ice] < Theta_pF_high)
	K_ice[i] = K_ice_dry;
      else if (content[Water] + content[Ice] > Theta_pF_low)
	K_ice[i] = K_ice_wet;
      else
	K_ice[i] = K_ice_dry 
	  + (K_ice_wet - K_ice_dry)
	  * (content[Water] + content[Ice] - Theta_pF_high)
	  / (Theta_pF_low - Theta_pF_high);
    }
  for (int i = to; i < intervals; i++)
    {
      K_water[i] = K_water[to-1];
      K_ice[i] = K_ice[to-1];
    }
  for (int i = 0; i < from; i++)
    {
      K_water[i] = K_water[from];
      K_ice[i] = K_ice[from];
    }
}

double 
Horizon::Implementation::HeatCapacity ()
{
  double C = 0.0;
  for (int i = 0; i < Constituents_End; i++)
    C += heat_capacity[i] * content[i];
  
  return C;
}

double 
Horizon::Implementation::DepolationsFactor (const constituents medium, 
					    const double alfa)
{
  if (medium == Air)
    return 0.333 - (0.333 - 0.070) * content[Air] / (hydraulic->porosity()
						     - Theta_pF_high);

  const double a = 1 - alfa * alfa;
  
  if (alfa < 1)
    return 1.0 / (2.0 * a)
      + alfa * alfa / (4.0 * a * sqrt (a)) 
      * log ((1.0 - sqrt (a)) / (1.0 + sqrt (a)));
  if (alfa == 1.0)
    return 1.0 / 3.0;
  if (alfa > 1.0)
    return (alfa * alfa / sqrt (-a) * (M_PI_2 - atan (sqrt (-1.0 / a))) - 1.0)
      / (2.0 * -a);
  assert (false);
  return 42.42e42;
}

double 
Horizon::Implementation::ThermalConductivity (constituents medium)
{
  // Thermal conductivity of each medium.
  double thermal_conductivity[Constituents_End] =
  { 0.57e5, 2.2e5, 0.025e5, 8.8e5, 2.9e5, 0.25e5 }; // [erg/s/cm/dg C]

  // Air conductivity is modified by water vapour.
  const double vapour_conductivity = 0.040e5;
  thermal_conductivity[Air] 
    += vapour_conductivity * min (1.0, (content[Water] / Theta_pF_high));
  
  double S1 = content[medium] * thermal_conductivity[medium];
  double S2 = content[medium];
  
  for (constituents i = Constituents_Start;
       i < Constituents_End;
       // C++ enums SUCKS!
       i = constituents (i + 1))
    {
      if (i != medium && content[i] > 0.0)
	{
	  const double a = thermal_conductivity[i] 
	    / thermal_conductivity[medium] - 1.0;
	  double k = -42.42e42;
	  switch (i)
	    {
	    case Water:
	    case Ice:
	      k = (1.0 / (1.0 + a)) / 3.0;
	      break;
	    case Quarts:
	    case Minerals:
	    case Air:
	      {
		const double g = DepolationsFactor (i, 
						    (i == Quarts)
						    ? quarts_form_factor
						    : mineral_form_factor);
		k = (2.0 / (1.0 + a * g)
		     + 1.0 / (1.0 + a * (1.0 - 2.0 * g)))
		  / 3.0;
	      }
	      break;
	    case Organic_Matter:
	      {
		const double Alfa = -3.0;
		k = (1.0 / (1.0 + a / (1.0 - Alfa))
		     + 1.0 / (1.0 - a * Alfa / (1.0 - Alfa))) / 3.0;
	      }
	    break;
	    case Constituents_End:
	      abort ();
	    }
	  S1 += k * content[i] * thermal_conductivity[i];
	  S2 += k * content[i];
	}
    }
  return S1 / S2;
}

double
Horizon::Implementation::rho_soil_particles ()
{ 
  return (clay + silt + fine_sand + coarse_sand) * rho_mineral
    + humus * rho_humus; 
}

double 
Horizon::Implementation::weight (const AttributeList& al, string name)
{
  return al.number (name) / (al.number ("clay")
			     + al.number ("silt")
			     + al.number ("fine_sand")
			     + al.number ("coarse_sand")
			     + al.number ("humus"));
}

Horizon::Implementation::Implementation (const AttributeList& al)
  : clay (weight (al, "clay")),
    silt (weight (al, "silt")),
    fine_sand (weight (al, "fine_sand")),
    coarse_sand (weight (al, "coarse_sand")),
    humus (weight (al, "humus")),
    quarts_in_clay (al.number ("quarts_in_clay")),
    quarts_in_silt (al.number ("quarts_in_silt")),
    quarts_in_sand (al.number ("quarts_in_sand")),
    SOM_C_per_N (al.number_sequence ("SOM_C_per_N")),
    SOM_fractions (al.number_sequence ("SOM_fractions")),
    quarts_form_factor (al.number ("quarts_form_factor")),
    mineral_form_factor (al.number ("mineral_form_factor")),
    intervals (al.integer ("intervals"))
{ 
  if (al.check ("C_soil"))
    C_soil = al.number ("C_soil");
  if (al.check ("K_water"))
    K_water = al.number_sequence ("K_water");
  if (al.check ("K_ice"))
    K_ice = al.number_sequence ("K_ice");
  if (al.check ("dry_bulk_density"))
    dry_bulk_density = al.number ("dry_bulk_density");
  else 
    dry_bulk_density = -1.0;
}

double
Horizon::dry_bulk_density () const
{ return impl.dry_bulk_density; }

double 
Horizon::clay () const 
{ return impl.clay; }

double 
Horizon::humus () const 
{ return impl.humus; }

double 
Horizon::SOM_C (unsigned int pool) const
{
  if (pool < impl.SOM_fractions.size ())
    // Specified, cool.
    return impl.SOM_fractions[pool] * impl.C_factor;
  else if (pool == 0)
    // Else, everything in the first (slow) pool.
    return  impl.C_factor;
    
  return 0.0;
}

double 
Horizon::SOM_C_per_N (unsigned int pool) const
{
  if (pool < impl.SOM_C_per_N.size ())
    // Specied, fine.
    return impl.SOM_C_per_N[pool];
  else if (impl.SOM_C_per_N.size () > 0)
    // Used last specified number.
    return impl.SOM_C_per_N[impl.SOM_C_per_N.size () - 1];
  // Give up.  Guess.
  CERR << "Horizon: SOM: no C_per_N\n";
  return 11.0;
}

double
Horizon::heat_conductivity (double Theta, double Ice) const
{
  const int entry = int ((Theta + Ice) * impl.intervals);
  assert (entry >= 0);
  assert (entry < impl.intervals);
  return ((impl.K_ice[entry] * Ice + impl.K_water[entry] * Theta) 
	  / (Theta + Ice))
    * 3600;			// erg/s / cm / K -> erg/h / cm / K
}

double
Horizon::heat_capacity (double Theta, double Ice) const
{
  return impl.C_soil 
    + impl.heat_capacity[Implementation::Water] * Theta
    + impl.heat_capacity[Implementation::Ice] * Ice;
}

static bool
check_alist (const AttributeList& al)
{
  bool ok = true;

  if (!(al.number ("humus") > 0.0))
    {
      CERR << "humus must be a positive number\n";
      ok = false;
    }
  return ok;
}

void
Horizon::load_syntax (Syntax& syntax, AttributeList& alist)
{
  syntax.add_check (check_alist);
  syntax.add ("description", Syntax::String, Syntax::OptionalConst,
	      "Description of this particular soil type.");
  alist.add ("description",
	     "The physical properties of a particular soil type.");
  syntax.add ("hydraulic", Librarian<Hydraulic>::library (), 
	      "The hydraulic propeties of the soil.");
  syntax.add ("tortuosity", Librarian<Tortuosity>::library (), 
	      "The soil tortuosity.");
  AttributeList& tortuosity = *new AttributeList ();
  tortuosity.add ("type", "M_Q");
  alist.add ("tortuosity", tortuosity);
  syntax.add ("clay", Syntax::None (), Syntax::Const,
	      "Relative fraction of clay in soil.");
  syntax.add ("silt", Syntax::None (), Syntax::Const,
	      "Relative fraction of silt in soil.");
  syntax.add ("fine_sand", Syntax::None (), Syntax::Const,
	      "Relative fraction of fine sand in soil.");
  syntax.add ("coarse_sand", Syntax::None (), Syntax::Const,
	      "Relative fraction of coarse sand in soil.");
  syntax.add ("humus", Syntax::None (), Syntax::Const,
	      "Relative fraction of humus in soil.");
// Data adopted from Møberg et al. 1988 (Tinglev & Roskilde Soil)
  syntax.add ("quarts_in_clay", Syntax::None (), Syntax::Const,
	      "Quarts fraction in clay.");
  alist.add ("quarts_in_clay", 0.15);
  syntax.add ("quarts_in_silt", Syntax::None (), Syntax::Const,
	      "Quarts fraction in silt.");
  alist.add ("quarts_in_silt", 0.60);
  syntax.add ("quarts_in_sand", Syntax::None (), Syntax::Const,
	      "Quarts fraction in sand.");
  alist.add ("quarts_in_sand", 0.70);
  syntax.add ("dry_bulk_density", "g/cm^3", Syntax::OptionalConst,
	      "The soils dry bulk density.\n\
By default, this is calculated from the soil constituents.");
  syntax.add ("SOM_C_per_N", Syntax::None (), Syntax::Const, Syntax::Sequence,
	      "C/N ratio for each SOM pool in this soil.");
  syntax.add ("SOM_fractions",
	      Syntax::None (), Syntax::Const, Syntax::Sequence,
	      "Fraction of humus in each SOM pool, from fastest to slowest.");
  syntax.add ("quarts_form_factor", Syntax::None (), Syntax::Const,
	      "Gemetry factor used for conductivity calculation.");
  alist.add ("quarts_form_factor", 2.0);
  syntax.add ("mineral_form_factor", Syntax::None (), Syntax::Const,
	      "Gemetry factor used for conductivity calculation.");
  alist.add ("mineral_form_factor", 4.0);
  syntax.add ("intervals", Syntax::Integer, Syntax::Const, "\
Number of numeric intervals to use in the heat coductivity table.");
  alist.add ("intervals", 100);
  syntax.add ("C_soil", "erg/cm^3/dg C", Syntax::OptionalConst,
	      "The soils heat capacity.\n\
By default, this is calculated from the soil constituents.");
  syntax.add ("K_water",
	      "erg/s/cm/dg C", Syntax::OptionalConst, Syntax::Sequence,
	      "Heat conductivity table for water in soil.\n\
By default, this is calculated from the soil constituents.");
  syntax.add ("K_ice",
	      "erg/s/cm/dg C", Syntax::OptionalConst, Syntax::Sequence,
	      "Heat conductivity table for solid frozen soil.\n\
By default, this is calculated from the soil constituents.");
}

Horizon::Horizon (const AttributeList& al)
  : impl (*new Implementation (al)),
    hydraulic (Librarian<Hydraulic>::create (al.alist ("hydraulic"))),
    tortuosity (Librarian<Tortuosity>::create (al.alist ("tortuosity")))
{ 
  if (impl.K_water.size () == 0)
    {
      impl.initialize (hydraulic);
    }
}

Horizon::~Horizon ()
{ }

// Create Horizon library.
Librarian<Horizon>::Content* Librarian<Horizon>::content = NULL;

const char *const Horizon::description = "\
A `horizon' is a soil type with specific physical properties.  It is\n\
the responsibility of the `horizon' component to specify these\n\
properties.";
