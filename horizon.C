// horizon.C
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
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


#include "horizon.h"
#include "library.h"
#include "alist.h"
#include "syntax.h"
#include "plf.h"
#include "hydraulic.h"
#include "mathlib.h"
#include "tortuosity.h"
#include "log.h"
#include "check.h"
#include "vcheck.h"
#include <vector>
#include <map>
#include <numeric>

// Weigth of mineral particles. [g/cm^3]
static const double rho_mineral = 2.65;	
// Weight of humus. [g/cm^3]
static const double rho_humus = 1.3; 
static const double rho_water = 1.0; // [g/cm^3]
static const double rho_ice = 0.917; // [g/cm^3]
static const double c_fraction_in_humus = 0.587;

struct Horizon::Implementation
{
  // Content.
  const double clay;
  const double silt;
  const double sand;
  const double fine_sand;
  const double coarse_sand;
  const double humus;
  const double quarts_in_clay;
  const double quarts_in_silt;
  const double quarts_in_sand;
  double dry_bulk_density;

  // Organic matter.
  /* const */ vector<double> SOM_C_per_N;
  const double C_per_N;
  /* const */ vector<double> SOM_fractions;
  const double turnover_factor;

  // Strange things.
  const double quarts_form_factor;
  const double mineral_form_factor;

  // Heat Capacity and Conductivity.
  double C_soil;
  vector<double> K_water;
  vector<double> K_ice;

  // Chemistry.
  /* const */map<string, double, less<string>/**/> attributes;
  
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
  void initialize (const Hydraulic&, int som_size);
  double HeatCapacity ();
  double DepolationsFactor (const Hydraulic&, 
			    const constituents medium, const double alfa);
  double ThermalConductivity (const Hydraulic&, constituents medium);
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
Horizon::Implementation::initialize (const Hydraulic& hydraulic, int som_size)
{
  if (som_size > 0)
    {
      // Fill out SOM_fractions and SOM_C_per_N.
      if (SOM_C_per_N.size () > 0 && SOM_C_per_N.size () < som_size)
	SOM_C_per_N.insert (SOM_C_per_N.end (),
			    som_size - SOM_C_per_N.size (), 
			    SOM_C_per_N.back ());
      if (SOM_fractions.size () > 0 && SOM_fractions.size () < som_size)
	SOM_fractions.insert (SOM_fractions.end (),
			      som_size - SOM_fractions.size (), 
			      0.0);
    }

  // Already initialized.
  if (K_water.size () != 0)
    return;

  // Did we specify 'dry_bulk_density'?  Else calculate it now.
  if (dry_bulk_density < 0.0)
    dry_bulk_density = rho_soil_particles () * (1.0 - hydraulic.porosity ());

  // The particles are not in a real continuous medium.  Try to correct.
  const double continuum_correction_factor = 1.25;
      
  // Quarts content in soil.
  const double quarts = clay * quarts_in_clay
    + silt * quarts_in_silt
    + sand * quarts_in_sand;

  // Above this pF heat is mostly tranfered by Air.
  Theta_pF_high = hydraulic.Theta (pF2h (4.2));
      
  // Below this pf heat is mostly transfered by Water or Ice.
  const double Theta_pF_low
    = (hydraulic.Theta (pF2h (2.0)) + Theta_pF_high) / 2.0;

  // Water that won't freeze.
  const double LiquidWater = Theta_pF_high; 
  
  // Relative content of various constituents in soil.
  content[Quarts] = quarts * (1.0 - hydraulic.Theta_sat);
  content[Minerals] = ((clay + silt + sand) - quarts)
    * (1.0 - hydraulic.Theta_sat);
  content[Organic_Matter] = humus * (1.0 - hydraulic.Theta_sat);

  // Find capasity of dry soil.
  content[Air] = hydraulic.porosity ();
  content[Water] = 0.0;
  content[Ice] = 0.0;
  C_soil = HeatCapacity ();

  // We calculate for water between Theta_res and Theta_sat.
  const int from = double2int (floor (intervals * hydraulic.Theta_res));
  const int to = double2int (ceil (intervals * hydraulic.Theta_sat));
  
  daisy_assert (0 <= from);
  daisy_assert (from < to);
  daisy_assert (to < intervals);

  // Make room, make room...
  K_water.insert (K_water.end (), intervals, 0.0);
  K_ice.insert (K_ice.end (), intervals, 0.0);

  for (int i = from; i < to; i++)
    {
      // Fill out water, ice, and air for pure water system.
      content[Water] = (i + 0.0) / (intervals + 0.0);
      content[Ice] = 0.0;
      content[Air] = hydraulic.porosity () - content[Water];

      // Calculate termal attributes for this combination.
      const double K_water_wet = ThermalConductivity (hydraulic, Water);
      const double K_water_dry = continuum_correction_factor
	* ThermalConductivity (hydraulic, Air);
      
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
      content[Air] = hydraulic.Theta_sat - (content[Water] + content[Ice]);
      
      // Calculate termal attributes for this combination.
      const double K_ice_wet = ThermalConductivity (hydraulic, Ice);
      const double K_ice_dry = continuum_correction_factor 
	* ThermalConductivity (hydraulic, Air);
      
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
Horizon::Implementation::DepolationsFactor (const Hydraulic& hydraulic,
					    const constituents medium, 
					    const double alfa)
{
  if (medium == Air)
    return 0.333 - (0.333 - 0.070) * content[Air] / (hydraulic.porosity()
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
  daisy_assert (false);
}

double 
Horizon::Implementation::ThermalConductivity (const Hydraulic& hydraulic,
					      constituents medium)
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
		const double g = DepolationsFactor (hydraulic, i, 
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
  return (clay + silt + sand) * rho_mineral + humus * rho_humus; 
}

double 
Horizon::Implementation::weight (const AttributeList& al, string name)
{
  daisy_assert (al.check ("sand") || 
		(al.check ("fine_sand") && al.check ("coarse_sand")));
  const double sand = al.check ("sand") 
    ? al.number ("sand") 
    : (al.number ("fine_sand")
       + al.number ("coarse_sand"));
  const double total = (al.number ("clay") + al.number ("silt")
			+ sand + al.number ("humus"));
  
  if (name == "sand")
    return sand / total;

  if (name == "fine_sand" && !al.check (name))
    return -42.42e42;

  if (name == "coarse_sand" && !al.check (name))
    return -42.42e42;

  return al.number (name) / total;
}

static const vector<double> empty_sequence;

Horizon::Implementation::Implementation (const AttributeList& al)
  : clay (weight (al, "clay")),
    silt (weight (al, "silt")),
    sand (weight (al, "sand")),
    fine_sand (weight (al, "fine_sand")),
    coarse_sand (weight (al, "coarse_sand")),
    humus (weight (al, "humus")),
    quarts_in_clay (al.number ("quarts_in_clay")),
    quarts_in_silt (al.number ("quarts_in_silt")),
    quarts_in_sand (al.number ("quarts_in_sand")),
    SOM_C_per_N (al.number_sequence ("SOM_C_per_N")),
    C_per_N (al.check ("C_per_N") ? al.number ("C_per_N") : -42.42e42),
    SOM_fractions (al.check ("SOM_fractions") 
		   ? al.number_sequence ("SOM_fractions")
		   : empty_sequence),
    turnover_factor (al.number ("turnover_factor")),
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
Horizon::silt () const 
{ return impl.silt; }

double 
Horizon::sand () const 
{ return impl.sand; }

double 
Horizon::humus () const 
{ return impl.humus; }

double
Horizon::humus_C () const
{ return impl.dry_bulk_density * impl.humus * c_fraction_in_humus; }

const std::vector<double>& 
Horizon::SOM_fractions () const
{ return impl.SOM_fractions; }

const std::vector<double>& 
Horizon::SOM_C_per_N () const
{ return impl.SOM_C_per_N; }

double
Horizon::C_per_N () const
{ return impl.C_per_N; }

double
Horizon::turnover_factor () const
{ return impl.turnover_factor; }

double
Horizon::heat_conductivity (double Theta, double Ice) const
{
  const int entry = int ((Theta + Ice) * impl.intervals);
  daisy_assert (entry >= 0);
  daisy_assert (entry < impl.intervals);
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

bool
Horizon::has_attribute (const string& name) const
{ return impl.attributes.find (name) != impl.attributes.end (); }

double 
Horizon::get_attribute (const string& name) const
{ 
  daisy_assert (has_attribute (name));
  return impl.attributes[name];
}

static bool
check_alist (const AttributeList& al, Treelog& err)
{
  bool ok = true;

  if (!(al.number ("humus") > 0.0))
    {
      err.entry ("humus must be a positive number");
      ok = false;
    }
  if (al.size ("SOM_C_per_N") < 1)
    {
      err.entry ("must specify at least one SOM_C_per_N");
      ok = false;
    }
  daisy_assert (al.check ("hydraulic"));
  const AttributeList& hydraulic =al.alist ("hydraulic");
  if (hydraulic.name ("type") == "hypres"
      && !al.check ("dry_bulk_density"))
    {
      err.entry ("You must specify 'dry_bulk_density' when using the 'hypres' \
hydraulic model");
      ok = false;
    }
  const double sand = al.check ("sand") ? al.number ("sand") : -1.0;
  const double fine_sand 
    = al.check ("fine_sand") ? al.number ("fine_sand") : -1.0;
  const double coarse_sand 
    = al.check ("coarse_sand") ? al.number ("coarse_sand") : -1.0;
  if (sand >= 0.0)
    {
      if (fine_sand > sand)
	{
	  err.entry ("You can't have more fine sand than sand");
	  ok = false;
	}
      if (coarse_sand >= sand)
	{
	  err.entry ("You can't have more coarse sand than sand");
	  ok = false;
	}
      if (fine_sand >= 0.0 && coarse_sand >= 0.0
	  && !approximate (fine_sand + coarse_sand, sand))
	{
	  err.entry ("Sand is either fine or coarse");
	  ok = false;
	}
    }
  else if (fine_sand < 0.0 || coarse_sand < 0.0)
    {
      err.entry ("You must specify the total amount of sand");
      ok = false;
    }
  return ok;
}

void 
Horizon::output (Log& log) const
{ output_derived (hydraulic, "hydraulic", log); }


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
  AttributeList hydraulic_alist;
  hydraulic_alist.add ("type", "hypres");
  hydraulic_alist.add ("Theta_res", 0.0);
  hydraulic_alist.add ("Theta_sat", 0.9);
  alist.add ("hydraulic", hydraulic_alist);
  syntax.add ("tortuosity", Librarian<Tortuosity>::library (), 
	      "The soil tortuosity.");
  AttributeList tortuosity;
  tortuosity.add ("type", "M_Q");
  alist.add ("tortuosity", tortuosity);
  syntax.add ("clay", Syntax::None (), Check::non_negative (), Syntax::Const,
	      "Relative fraction of clay in soil.");
  syntax.add ("silt", Syntax::None (), Check::non_negative (), Syntax::Const,
	      "Relative fraction of silt in soil.");
  syntax.add ("fine_sand", Syntax::None (), Check::non_negative (), 
	      Syntax::OptionalConst,
	      "Relative fraction of fine sand in soil.");
  syntax.add ("coarse_sand", Syntax::None (), Check::non_negative (), 
	      Syntax::OptionalConst,
	      "Relative fraction of coarse sand in soil.");
  syntax.add ("sand", Syntax::None (), Check::non_negative (), 
	      Syntax::OptionalConst,
	      "Relative fraction of sand in soil.");
  syntax.add ("humus", Syntax::None (), Check::non_negative (), 
	      Syntax::Const,
	      "Relative fraction of humus in soil.");
  // Data adopted from Møberg et al. 1988 (Tinglev & Roskilde Soil)
  syntax.add_fraction ("quarts_in_clay", Syntax::Const,
		       "Quarts fraction in clay.");
  alist.add ("quarts_in_clay", 0.15);
  syntax.add_fraction ("quarts_in_silt", Syntax::Const,
		       "Quarts fraction in silt.");
  alist.add ("quarts_in_silt", 0.60);
  syntax.add_fraction ("quarts_in_sand", Syntax::Const,
		       "Quarts fraction in sand.");
  alist.add ("quarts_in_sand", 0.70);
  syntax.add ("dry_bulk_density", "g/cm^3", Syntax::OptionalConst,
	      "The soils dry bulk density.\n\
By default, this is calculated from the soil constituents.");
  syntax.add ("SOM_C_per_N", "g C/g N", Check::non_negative (), 
	      Syntax::Const, Syntax::Sequence,
	      "C/N ratio for each SOM pool in this soil.\n\
If 'C_per_N' is specified, this is used as a goal only.  If 'C_per_N' is\n\
unspecified, the SOM pools will be initialized with this value.");
  vector<double> SOM_C_per_N;
  SOM_C_per_N.push_back (11.0);
  SOM_C_per_N.push_back (11.0);
  SOM_C_per_N.push_back (11.0);
  alist.add ("SOM_C_per_N", SOM_C_per_N);
  syntax.add ("C_per_N", "g C/g N", Check::positive (), Syntax::OptionalConst,
	      "Total C/N ratio for this horizon.\n\
This is the combined initial C/N ratio for all organic matter pools in the\n\
horizon.  The C/N ration of the AOM and SMB pools is assumed to be known,\n\
given that this number is used to find the common C/N ration for the SOM\n\
pools.  The C/N ration for the SOM pools will then gradually move towards\n\
the values specified by 'SOM_C_per_N'.\n\
By default, the values given by 'SOM_C_per_N' will be used for\n\
initialization.");
  syntax.add_fraction ("SOM_fractions", 
		       Syntax::OptionalConst, Syntax::Sequence, "\
Fraction of humus in each SOM pool, from slowest to fastest.");
  syntax.add_check ("SOM_fractions", VCheck::sum_equal_1 ());
  syntax.add ("turnover_factor", Syntax::None (), Check::non_negative (),
	      Syntax::Const, "\
Factor multiplied to the turnover rate for all organic matter pools in\n\
this horizon.");
  alist.add ("turnover_factor", 1.0);
  syntax.add ("quarts_form_factor", Syntax::None (), Syntax::Const,
	      "Gemetry factor used for conductivity calculation.");
  alist.add ("quarts_form_factor", 2.0);
  syntax.add ("mineral_form_factor", Syntax::None (), Syntax::Const,
	      "Gemetry factor used for conductivity calculation.");
  alist.add ("mineral_form_factor", 4.0);
  syntax.add ("intervals", Syntax::Integer, Syntax::Const, "\
Number of numeric intervals to use in the heat coductivity table.");
  alist.add ("intervals", 100);
  syntax.add ("C_soil", "erg/cm^3/dg C", Check::positive (), 
	      Syntax::OptionalConst,
	      "The soils heat capacity.\n\
By default, this is calculated from the soil constituents.");
  syntax.add ("K_water",
	      "erg/s/cm/dg C", Check::positive (),
	      Syntax::OptionalConst, Syntax::Sequence,
	      "Heat conductivity table for water in soil.\n\
By default, this is calculated from the soil constituents.");
  syntax.add ("K_ice",
	      "erg/s/cm/dg C", Check::positive (),
	      Syntax::OptionalConst, Syntax::Sequence,
	      "Heat conductivity table for solid frozen soil.\n\
By default, this is calculated from the soil constituents.");

  Syntax& attSyntax = *new Syntax ();
  attSyntax.add ("key", Syntax::String, Syntax::Const,
		 "Name of attribute.");
  attSyntax.add ("value", Syntax::Unknown (), Syntax::Const,
		 "Value of attribute.");
  attSyntax.order ("key", "value");
  syntax.add ("attributes", attSyntax, Syntax::OptionalConst, Syntax::Sequence,
	      "List of additional attributes for this horizon.\n\
Intended for use with pedotransfer functions.");
}

Horizon::Horizon (const AttributeList& al)
  : impl (*new Implementation (al)),
    name (al.name ("type")),
    hydraulic (Librarian<Hydraulic>::create (al.alist ("hydraulic"))),
    tortuosity (Librarian<Tortuosity>::create (al.alist ("tortuosity")))
{ 
  if (al.check ("attributes"))
    {
      const vector<AttributeList*>& alists = al.alist_sequence ("attributes");
      for (unsigned int i = 0; i < alists.size (); i++)
	impl.attributes[alists[i]->name ("key")] = alists[i]->number ("value");
    }
}

void
Horizon::initialize (bool top_soil, int som_size, Treelog& msg)
{
  Treelog::Open nest (msg, name);
  hydraulic.initialize (clay (), silt (), sand (), humus (),
			dry_bulk_density (), top_soil, msg);
  impl.initialize (hydraulic, som_size);
}

Horizon::~Horizon ()
{ 
  delete &impl; 
  delete &hydraulic;
  delete &tortuosity;
}

// Create Horizon library.
EMPTY_TEMPLATE
Librarian<Horizon>::Content* Librarian<Horizon>::content = NULL;

const char *const Horizon::description = "\
A 'horizon' is a soil type with specific physical properties.  It is\n\
the responsibility of the 'horizon' component to specify these\n\
properties.";
