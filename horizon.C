// horizon.C --- Common code for all horizon models.
// 
// Copyright 1996-2004 Per Abrahamsen and Søren Hansen
// Copyright 2000-2004 KVL.
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
#include "horheat.h"
#include "hydraulic.h"
#include "mathlib.h"
#include "tortuosity.h"
#include "texture.h"
#include "nitrification.h"
#include "log.h"
#include "check_range.h"
#include "vcheck.h"
#include "tmpstream.h"
#include <vector>
#include <map>

// Weight of humus. [g/cm^3]
static const double rho_water = 1.0; // [g/cm^3]
static const double rho_ice = 0.917; // [g/cm^3]
static const double c_fraction_in_humus = 0.587;

struct Horizon::Implementation
{
  // Content.
  double dry_bulk_density;
  /* const */ vector<double> SOM_C_per_N;
  const double C_per_N;
  /* const */ vector<double> SOM_fractions;
  const double turnover_factor;
  const double anisotropy;
  /*const*/ map<string, double, less<string>/**/> attributes;
  /*const*/ Nitrification& nitrification;
  HorHeat hor_heat;
  
  // Initialize.
  void initialize (const Hydraulic&, const Texture& texture, double quarts, 
                   int som_size, Treelog& msg);
  
  // Create.
  Implementation (const AttributeList& al);
  ~Implementation ();
};

void 
Horizon::Implementation::initialize (const Hydraulic& hydraulic,
                                     const Texture& texture, 
                                     const double quarts,
                                     int som_size,
                                     Treelog& msg)
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

  // Did we specify 'dry_bulk_density'?  Else calculate it now.
  if (dry_bulk_density < 0.0)
    dry_bulk_density = texture.rho_soil_particles () 
      * (1.0 - hydraulic.porosity ());

  hor_heat.initialize (hydraulic, texture, quarts, msg);
}

Horizon::Implementation::Implementation (const AttributeList& al)
  : dry_bulk_density (al.number ("dry_bulk_density", -42.42e42)),
    SOM_C_per_N (al.number_sequence ("SOM_C_per_N")),
    C_per_N (al.number ("C_per_N", -42.42e42)),
    SOM_fractions (al.check ("SOM_fractions") 
		   ? al.number_sequence ("SOM_fractions")
		   : vector<double> ()),
    turnover_factor (al.number ("turnover_factor")),
    anisotropy (al.number ("anisotropy")),
    nitrification (Librarian<Nitrification>::create 
		   (al.alist ("Nitrification"))),
    hor_heat (al)
{ }

Horizon::Implementation::~Implementation ()
{
  delete &nitrification;
}

double
Horizon::dry_bulk_density () const
{ 
  daisy_assert (impl.dry_bulk_density > 0.0);
  return impl.dry_bulk_density; 
}

double 
Horizon::clay () const
{ 
  daisy_assert (fast_clay >= 0.0);
  return fast_clay; 
}

double 
Horizon::humus () const
{
  daisy_assert (fast_humus >= 0.0);
  return fast_humus; 
}

double
Horizon::humus_C () const
{ return dry_bulk_density () * humus () * c_fraction_in_humus; }

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
Horizon::quartz () const
{ 
  const double clay = texture_below ( 2.0 /*[um]*/);
  const double silt = texture_below (20.0 /*[um]*/) - clay;
  const double sand = 1.0 - clay - silt;

  // Data adopted from Møberg et al. 1988 (Tinglev & Roskilde Soil)
  return clay * 0.15 + silt * 0.6 + sand * 0.7;
}

double
Horizon::anisotropy () const
{ return impl.anisotropy; }

double
Horizon::heat_conductivity (double Theta, double Ice) const
{ return impl.hor_heat.heat_conductivity (Theta, Ice); }

double
Horizon::heat_capacity (double Theta, double Ice) const
{ return impl.hor_heat.heat_capacity (Theta, Ice); }

bool
Horizon::has_attribute (const string& name) const
{ return impl.attributes.find (name) != impl.attributes.end (); }

double 
Horizon::get_attribute (const string& name) const
{ 
  daisy_assert (has_attribute (name));
  return impl.attributes[name];
}

bool
Horizon::check_alist (const AttributeList& al, Treelog& err)
{
  bool ok = true;

  daisy_assert (al.check ("hydraulic"));
  const AttributeList& hydraulic =al.alist ("hydraulic");
  if (hydraulic.name ("type") == "hypres"
      && !al.check ("dry_bulk_density"))
    {
      err.entry ("You must specify 'dry_bulk_density' when using the 'hypres' \
hydraulic model");
      ok = false;
    }

  return ok;
}

void 
Horizon::nitrification (const double M, const double C, 
                        const double M_left,
                        const double h, const double T,
                        double& NH4, double& N2O, double& NO3) const
{ impl.nitrification.tick (M, C, M_left, h,  T, NH4, N2O, NO3); }

void 
Horizon::output (Log& log) const
{ output_derived (hydraulic, "hydraulic", log); }


static const class SOM_fractions_check_type : public VCheck
{
  void check (const Syntax& syntax, const AttributeList& alist, 
              const string& key)
    const throw (string)
  {
    daisy_assert (key == "SOM_fractions");
    daisy_assert (alist.check (key));
    daisy_assert (syntax.lookup (key) == Syntax::Number);
    daisy_assert (syntax.size (key) == Syntax::Sequence);
    vector<double> fractions = alist.number_sequence ("SOM_fractions");
    bool has_negative = false;
    double sum = 0.0;
    for (unsigned int i = 0; i < fractions.size (); i++)
      {
        if (fractions[i] < 0)
          has_negative = true;
        else
          sum += fractions[i];
      }
    if (!has_negative && !approximate (sum, 1.0))
      throw string ("sum must be 1.0");
    if (sum > 1.0 && !approximate (sum, 1.0))
      throw string ("sum must be at most 1.0");
  };
} SOM_fractions_check;

void
Horizon::load_syntax (Syntax& syntax, AttributeList& alist)
{
  syntax.add_check (check_alist);
  syntax.add ("description", Syntax::String, Syntax::OptionalConst,
	      "Description of this particular soil type.");
  syntax.add ("hydraulic", Librarian<Hydraulic>::library (), 
	      "The hydraulic propeties of the soil.");
  AttributeList hydraulic_alist;
  hydraulic_alist.add ("type", "hypres");
  alist.add ("hydraulic", hydraulic_alist);
  syntax.add ("tortuosity", Librarian<Tortuosity>::library (), 
	      "The soil tortuosity.");
  AttributeList tortuosity;
  tortuosity.add ("type", "M_Q");
  alist.add ("tortuosity", tortuosity);
  syntax.add ("anisotropy", Syntax::None (),
	      Check::non_negative (), Syntax::Const, "\
Horizontal saturated water conductivity relative to vertical saturated\n\
water conductivity.  The higher this value, the faster the water will\n\
move towards drain pipes.");
  alist.add ("anisotropy", 1.0);
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
  syntax.add_check ("SOM_C_per_N", VCheck::min_size_1 ());
  
  static const BelowOrEqual max_1 (1.0);
  syntax.add ("SOM_fractions",  Syntax::None (), max_1,
              Syntax::OptionalConst, Syntax::Sequence, "\
Fraction of humus in each SOM pool, from slowest to fastest.\n\
Negative numbers mean unspecified, let Daisy find appropriate values.");
  syntax.add_check ("SOM_fractions", SOM_fractions_check);

  syntax.add ("turnover_factor", Syntax::None (), Check::non_negative (),
	      Syntax::Const, "\
Factor multiplied to the turnover rate for all organic matter pools in\n\
this horizon.");
  alist.add ("turnover_factor", 1.0);
  syntax.add ("Nitrification", Librarian<Nitrification>::library (),
              "The soil nitrification process.");
  AttributeList nitrification_alist;
  nitrification_alist.add ("type", "soil");
  nitrification_alist.add ("k_10", 2.08333333333e-7); // 5e-6/24 [1/h]
  nitrification_alist.add ("k", 5.0e-5); // [g N/cm^3]
  nitrification_alist.add ("heat_factor", PLF::empty ());
  nitrification_alist.add ("water_factor", PLF::empty ());
  nitrification_alist.add ("N2O_fraction", 0.02);

  alist.add ("Nitrification", nitrification_alist);

  HorHeat::load_syntax (syntax, alist);

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
    fast_clay (-42.42e42),
    fast_humus (-42.42e42),
    name (al.identifier ("type")),
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
Horizon::initialize_base (bool top_soil,
                          int som_size, const Texture& texture, 
                          Treelog& msg)
{ 
  Treelog::Open nest (msg, name);
  const double clay_lim = texture_below ( 2.0 /* [um] USDA Clay */);
  fast_clay = texture.mineral () * clay_lim;
  fast_humus = texture.humus;
  hydraulic.initialize (texture, impl.dry_bulk_density, top_soil, msg);
  impl.initialize (hydraulic, texture, quartz () * texture.mineral (),
                   som_size, msg); 
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
A `horizon' is a soil type with specific physical properties.  It is\n\
the responsibility of the `horizon' component to specify these\n\
properties.";
