// horizon.C

#include "horizon.h"
#include "library.h"
#include "alist.h"
#include "syntax.h"
#include "csmp.h"
#include "hydraulic.h"
#include <vector>
#include <map>

static const double rho_particle = 2.65;	// Weigth of soil. [g / cm³]

static Library* Horizon_library = NULL;
typedef map<string, Horizon::constructor, less<string> > Horizon_map_type;
static Horizon_map_type* Horizon_constructors;

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
  const double C_per_N;

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

  static const double heat_capacity[Constituents_End];
  
  // Create.
  static double weight (const AttributeList& al, string name);
  Implementation (const AttributeList& al);
};

const double 
Horizon::Implementation::heat_capacity[Constituents_End] = // [J / cm³ / °C]
// Ice is given as equivalent amount of water.
{ 4.2, 1.9 * (1.0 / 0.92), 1.25e-3, 2.0, 2.0, 2.5 }; 

void 
Horizon::Implementation::initialize (const Hydraulic& hydro)
{
  hydraulic = &hydro;

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
  K_water.insert (K_water.end (), to, 0.0);
  K_ice.insert (K_ice.end (), to, 0.0);

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
#if 0
  static const double density [Constituents_End] =
  { 1.0e6, 0.92e6, 1.25e3, 2.66e6, 2.65e6, 1.3e6 }; // [g / m³]
#endif
  // Thermal conductivity of each medium.
  double thermal_conductivity[Constituents_End] = 
  { 0.57e-2, 2.2e-2, 0.025e-2, 8.8e-2, 2.9e-2, 0.25e-2 };

  // Air conductivity is modified by water vapour.
  const double vapour_conductivity = 0.040e-2;
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
Horizon::Implementation::weight (const AttributeList& al, string name)
{
  return al.number (name) / (  al.number ("clay")
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
    C_per_N (al.number ("C_per_N")),
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
}

double 
Horizon::clay () const 
{ return impl.clay; }

double
Horizon::C () const
{ return rho_particle * impl.humus * (1 - hydraulic.porosity ()) * 0.587; }

double
Horizon::N () const
{ return C () / C_per_N (); }

double
Horizon::C_per_N () const
{ return impl.C_per_N; }


double
Horizon::heat_conductivity (double Theta, double Ice) const
{
  const int entry = int (Theta * impl.intervals);
  return (impl.K_ice[entry] * Ice + impl.K_water[entry] * (1.0 - Ice))
    * 3600;			// W / cm / K -> J/h / cm / K
}

double
Horizon::heat_capacity (double Theta, double Ice) const
{
  return impl.C_soil 
    + impl.heat_capacity[Implementation::Water] * Theta
    + impl.heat_capacity[Implementation::Ice] * Ice;
}

double 
Horizon::tortuosity_factor (double Water) const
{
  const double n = hydraulic.porosity ();
  return pow (Water, 7.0 / 3.0) / (n * n); // Tortuosity factor []
}

double 
Horizon::K_planar () const
{
  return 6.3e-4;
}

double 
Horizon::K_edge () const
{
  return 1.372e-5;
}

double 
Horizon::v_planar () const
{ 
  const double S_planar = 5.964e-3; // Maximum specific absorbtion [g / g clay]
  return S_planar * impl.clay * rho_particle * (1 - hydraulic.porosity ()); 
}

double 
Horizon::v_edge () const
{
  const double S_edge = 0.308e-3;	// Same for edges.
  return S_edge * impl.clay * rho_particle * (1 - hydraulic.porosity ()); 
}

const Library&
Horizon::library ()
{
  assert (Horizon_library);
  return *Horizon_library;
}

void
Horizon::add_type (const string name, 
		   const AttributeList& al, 
		   const Syntax& syntax,
		   constructor cons)
{
  assert (Horizon_library);
  Horizon_library->add (name, al, syntax);
  Horizon_constructors->insert(Horizon_map_type::value_type (name, cons));
}

void 
Horizon::derive_type (string name, const AttributeList& al, string super)
{
  add_type (name, al, library ().syntax (super),
	    (*Horizon_constructors)[super]);
}

Horizon&
Horizon::create (const AttributeList& al)
{
  assert (al.check ("type"));
  const string name = al.name ("type");
  assert (library ().check (name));
  assert (library ().syntax (name).check (al));
  return (*Horizon_constructors)[name] (al);
}

void
Horizon::load_syntax (Syntax& syntax, AttributeList& alist)
{
  syntax.add ("hydraulic", Hydraulic::library (), Syntax::Const);
  syntax.add ("clay", Syntax::Number, Syntax::Const);
  syntax.add ("silt", Syntax::Number, Syntax::Const);
  syntax.add ("fine_sand", Syntax::Number, Syntax::Const);
  syntax.add ("coarse_sand", Syntax::Number, Syntax::Const);
  syntax.add ("humus", Syntax::Number, Syntax::Const);
  syntax.add ("quarts_in_clay", Syntax::Number, Syntax::Const);
  alist.add ("quarts_in_clay", 0.05);
  syntax.add ("quarts_in_silt", Syntax::Number, Syntax::Const);
  alist.add ("quarts_in_silt", 0.20);
  syntax.add ("quarts_in_sand", Syntax::Number, Syntax::Const);
  alist.add ("quarts_in_sand", 0.60);
  syntax.add ("C_per_N", Syntax::Number, Syntax::Const);
  syntax.add ("quarts_form_factor", Syntax::Number, Syntax::Const);
  alist.add ("quarts_form_factor", 3.5);
  syntax.add ("mineral_form_factor", Syntax::Number, Syntax::Const);
  alist.add ("mineral_form_factor", 3.5);
  syntax.add ("intervals", Syntax::Integer, Syntax::Const);
  alist.add ("intervals", 100);
  syntax.add ("C_soil", Syntax::Number, Syntax::Optional);
  syntax.add ("K_water", Syntax::Number, Syntax::Optional, Syntax::Sequence);
  syntax.add ("K_ice", Syntax::Number, Syntax::Optional, Syntax::Sequence);
}

Horizon::Horizon (const AttributeList& al)
  : impl (*new Implementation (al)),
    hydraulic (Hydraulic::create (al.list ("hydraulic")))
{ 
  if (impl.K_water.size () == 0)
    {
      impl.initialize (hydraulic);
    }
}

Horizon::~Horizon ()
{ }

int Horizon_init::count;

Horizon_init::Horizon_init ()
{ 
  if (count++ == 0)
    {
      Horizon_library = new Library ("horizon");
      Horizon_constructors = new Horizon_map_type ();
    }
  assert (count > 0);
}

Horizon_init::~Horizon_init ()
{ 
  if (--count == 0)
    {
      delete Horizon_library;
      Horizon_library = NULL;
      delete Horizon_constructors;
      Horizon_constructors = NULL;
    }
  assert (count >= 0);
}

