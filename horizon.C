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

#define BUILD_DLL

#include "horizon.h"
#include "library.h"
#include "block_model.h"
#include "frame_model.h"
#include "frame_submodel.h"
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
#include "librarian.h"
#include "secondary.h"
#include "treelog.h"
#include <sstream>
#include <vector>
#include <map>

// Weight of humus. [g/cm^3]
// static const double rho_water = 1.0; // [g/cm^3]
// static const double rho_ice = 0.917; // [g/cm^3]
static const double c_fraction_in_humus = 0.587;

struct Horizon::Implementation
{
  // Content.
  double dry_bulk_density;
  double quartz;
  /* const */ std::vector<double> SOM_C_per_N;
  const double C_per_N;
  /* const */ std::vector<double> SOM_fractions;
  const double turnover_factor;
  const double root_homogeneity;
  const double root_retardation;
  const double K_factor;
  const double anisotropy;
  typedef std::map<symbol, double> double_map;
  const double_map attributes;
  typedef std::map<symbol, symbol> symbol_map;
  const symbol_map dimensions;
  const std::unique_ptr<Nitrification> nitrification;
  const std::unique_ptr<Secondary> secondary;
  const double r_pore_min;             // Smallest pore in soil [um]
  double primary_sorption_fraction;
  HorHeat hor_heat;
  /* const */ double CEC;                   // [cmolc/kg]
  
  // Create and Detroy.
  void initialize (Hydraulic&, const Texture& texture, 
                   int som_size, bool top_soil, const double center_z, 
                   Treelog& msg);
  static double_map get_attributes
  /**/ (const std::vector<boost::shared_ptr<const FrameSubmodel>/**/>& frames);
  static symbol_map get_dimensions 
  /**/ (const std::vector<boost::shared_ptr<const FrameSubmodel>/**/>& frames);
  Implementation (const BlockModel& al);
  ~Implementation ();
};

static double 
default_CEC (const Texture& texture) // [cmolc/kg]
{
  // Source: Krogh, Lars, Henrik Breuning-Madsen, and Mogens Humlekrog
  // Greve. "Cation-exchange capacity pedotransfer functions for
  // Danish soils." Acta Agriculturae Scandinavica, Section B-Plant
  // Soil Science 50.1 (2000): 1-12.

  const double clay 
    = 100.0 * texture.fraction_of_minerals_smaller_than (2.0 /* um */); /* % */
  const double SOM = 100.0 * texture.humus; /* % */
  const double chalk = 100.0 * texture.chalk; /* % */
  
  if (SOM > 10.0)            
    // Organic soil.
    return 21.11 + 1.88 * SOM;  // [cmolc/kg]

  if (chalk > 0.1 /* % */)
    // Calcareous soil.
    return -0.04 + 2.13 * SOM + 0.42 * clay; // [cmolc/kg]

  if (false)
    // Podzolic B-horizon
    return 1.91 + 4.79 * SOM + 0.35 * clay;

  // Other soils.
  return 0.95 + 2.90 * SOM + 0.53 * clay;
}

void 
Horizon::Implementation::initialize (Hydraulic& hydraulic,
                                     const Texture& texture, 
                                     int som_size,
                                     const bool top_soil,
                                     const double center_z,
                                     Treelog& msg)
{
  if (CEC < 0.0)
    {
      CEC = default_CEC (texture);
      std::ostringstream tmp;
      tmp << "(CEC " << CEC 
          << " [cmolc/kg]) ; Estimated from clay and humus.";
      msg.debug (tmp.str ());
    }

  hydraulic.initialize (texture, dry_bulk_density, top_soil, CEC, center_z, msg);
  if (som_size > 0)
    {
      // Fill out SOM_fractions and SOM_C_per_N.
      if (SOM_C_per_N.size () > 0 && SOM_C_per_N.size () < som_size + 0U)
        SOM_C_per_N.insert (SOM_C_per_N.end (),
                            som_size - SOM_C_per_N.size (), 
                            SOM_C_per_N.back ());
      if (SOM_fractions.size () > 0 && SOM_fractions.size () < som_size + 0U)
        SOM_fractions.insert (SOM_fractions.end (),
                              som_size - SOM_fractions.size (), 
                              0.0);
    }

  // Did we specify 'dry_bulk_density'?  Else calculate it now.
  if (dry_bulk_density < 0.0)
    {
      dry_bulk_density = texture.rho_soil_particles () 
        * (1.0 - hydraulic.porosity ());
      std::ostringstream tmp;
      tmp << "(dry_bulk_density " << dry_bulk_density 
          << " [g/cm^3]) ; Estimated from porosity and texture.";
      msg.debug (tmp.str ());
    }
  
  // Did we specify 'dry_bulk_density'?  Else calculate it now.
  if (quartz < 0.0)
    {
      const double clay 
	= texture.fraction_of_minerals_smaller_than ( 2.0 /*[um]*/);
      const double silt 
	= texture.fraction_of_minerals_smaller_than (20.0 /*[um]*/) - clay;
      const double sand = 1.0 - clay - silt;
      
      // Data adopted from Møberg et al. 1988 (Tinglev & Roskilde Soil)
      quartz =  clay * 0.15 + silt * 0.6 + sand * 0.7;

      std::ostringstream tmp;
      tmp << "(quartz " << quartz << " []) ; Estimated from texture.";
      msg.debug (tmp.str ());
    }
  hor_heat.initialize (hydraulic, texture, quartz, msg);
}

Horizon::Implementation::double_map
Horizon::Implementation::get_attributes 
/**/ (const std::vector<boost::shared_ptr<const FrameSubmodel>/**/>& frames)
{ 
  double_map result; 
  for (unsigned int i = 0; i < frames.size (); i++)
    result[frames[i]->name ("key")] = frames[i]->number ("value");
  return result;
}

Horizon::Implementation::symbol_map
Horizon::Implementation::get_dimensions 
/**/ (const std::vector<boost::shared_ptr<const FrameSubmodel>/**/>& frames)
{ 
  symbol_map result; 
  for (unsigned int i = 0; i < frames.size (); i++)
    result[frames[i]->name ("key")] = frames[i]->name ("value");
  return result;
}

Horizon::Implementation::Implementation (const BlockModel& al)
  : dry_bulk_density (al.number ("dry_bulk_density", -42.42e42)),
    quartz (al.number ("quartz", -42.42e42)),
    SOM_C_per_N (al.number_sequence ("SOM_C_per_N")),
    C_per_N (al.number ("C_per_N", -42.42e42)),
    SOM_fractions (al.check ("SOM_fractions") 
                   ? al.number_sequence ("SOM_fractions")
                   : std::vector<double> ()),
    turnover_factor (al.number ("turnover_factor")),
    root_homogeneity (al.number ("root_homogeneity")),
    root_retardation (al.number ("root_retardation")),
    K_factor (al.number ("K_factor")),
    anisotropy (al.number ("anisotropy")),
    attributes (get_attributes (al.submodel_sequence ("attributes"))),
    dimensions (get_dimensions (al.submodel_sequence ("attributes"))),
    nitrification (Librarian::build_item<Nitrification> (al, "Nitrification")),
    secondary (Librarian::build_item<Secondary> (al, "secondary_domain")),
    r_pore_min (al.number ("r_pore_min")),
    primary_sorption_fraction (NAN),
    hor_heat (al.submodel ("HorHeat")),
    CEC (al.number ("CEC", -42.42e42))
{ }

Horizon::Implementation::~Implementation ()
{ }

double
Horizon::CEC () const
{
  daisy_assert (impl->CEC > 0.0);
  return impl->CEC;
}

double
Horizon::dry_bulk_density () const
{ 
  daisy_assert (impl->dry_bulk_density > 0.0);
  return impl->dry_bulk_density; 
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
{ return impl->SOM_fractions; }

const std::vector<double>& 
Horizon::SOM_C_per_N () const
{ return impl->SOM_C_per_N; }

double
Horizon::C_per_N () const
{ return impl->C_per_N; }

double
Horizon::turnover_factor () const
{ return impl->turnover_factor; }

double
Horizon::root_homogeneity () const
{ return impl->root_homogeneity; }

double
Horizon::root_retardation () const
{ return impl->root_retardation; }

double
Horizon::anisotropy () const
{ return impl->anisotropy; }

double
Horizon::heat_conductivity (double Theta, double Ice) const
{ return impl->hor_heat.heat_conductivity (Theta, Ice); }

double
Horizon::heat_capacity (double Theta, double Ice) const
{ return impl->hor_heat.heat_capacity (Theta, Ice); }

const Secondary& 
Horizon::secondary_domain () const
{ return *impl->secondary;}

double
Horizon::primary_sorption_fraction () const
{
  daisy_assert (std::isfinite (impl->primary_sorption_fraction));
  return impl->primary_sorption_fraction;
}

double 
Horizon::K_factor () const         // []
{ return impl->K_factor; }

bool
Horizon::has_attribute (const symbol name) const
{ return impl->attributes.find (name) != impl->attributes.end (); }

double 
Horizon::get_attribute (const symbol name) const
{ 
  Implementation::double_map::const_iterator i = impl->attributes.find (name);
  daisy_assert (i != impl->attributes.end ());
  return (*i).second;
}

symbol
Horizon::get_dimension (const symbol name) const
{ 
  Implementation::symbol_map::const_iterator i = impl->dimensions.find (name);
  daisy_assert (i != impl->dimensions.end ());
  return (*i).second;
}

void 
Horizon::append_attributes (std::set<symbol>& all) const
{
  for (Implementation::double_map::const_iterator i 
         = impl->attributes.begin ();
       i != impl->attributes.end ();
       i++)
    all.insert ((*i).first);
}

void 
Horizon::nitrification (const double M, const double C, 
                        const double h, const double T,
                        double& NH4, double& N2O, double& NO3) const
{ impl->nitrification->tick (M, C, h,  T, NH4, N2O, NO3); }


void 
Horizon::output (Log& log) const
{ output_object (hydraulic, "hydraulic", log); }


static const class SOM_fractions_check_type : public VCheck
{
  bool verify (const Metalib&, const Frame& frame, const symbol key,
               Treelog& msg) const
  {
    daisy_assert (key == "SOM_fractions");
    daisy_assert (frame.check (key));
    daisy_assert (frame.lookup (key) == Attribute::Number);
    daisy_assert (frame.type_size (key) == Attribute::Variable);
    std::vector<double> fractions = frame.number_sequence ("SOM_fractions");
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
      {
        msg.error ("sum must be 1.0");
        return false;
      }
    if (sum > 1.0 && !approximate (sum, 1.0))
      {
        msg.error ("sum must be at most 1.0");
        return false;
      }
    return true;
  };
 public:
  SOM_fractions_check_type ()
    { }
} SOM_fractions_check;

Horizon::Horizon (const BlockModel& al)
  : ModelDerived (al.type_name ()),
    impl (new Implementation (al)),
    fast_clay (-42.42e42),
    fast_humus (-42.42e42),
    hydraulic (Librarian::build_item<Hydraulic> (al, "hydraulic")),
    tortuosity (Librarian::build_item<Tortuosity> (al, "tortuosity"))
{ }

void 
Horizon::initialize_base (const bool top_soil,
                          const int som_size, const double center_z, 
                          const Texture& texture, Treelog& msg)
{ 
  TREELOG_MODEL (msg);
  const double clay_lim = texture_below ( 2.0 /* [um] USDA Clay */);
  fast_clay = texture.mineral () * clay_lim;
  fast_humus = texture.humus;
  impl->initialize (*hydraulic, texture,
                    som_size, top_soil, center_z, msg); 
  impl->secondary->initialize (msg);

  std::ostringstream tmp;
  const double h_lim = secondary_domain ().h_lim ();
  if (h_lim >= 0.0)
    impl->primary_sorption_fraction = 1.0;
  else
    {
      const double h_sat = 0.0;
      const double Theta_sat = hydraulic->Theta (h_sat);

      // Integrate h over Theta.
      PLF h_int;
      const double h_min = Hydraulic::r2h (impl->r_pore_min);
      const double Theta_min =hydraulic->Theta (h_min);
      h_int.add (Theta_min, 0.0);
      static const int intervals = 100;
      double delta = (Theta_sat - Theta_min) / (intervals + 0.0);
      double sum = 0.0;
      for (double Theta = Theta_min + delta; Theta < Theta_sat; Theta += delta)
        {
          double my_h = hydraulic->h (Theta);
          sum += my_h * delta;
          h_int.add (Theta, sum);
        }

      const double h_wp = -15000.0;
      const double Theta_wp = hydraulic->Theta (h_wp);
      const double Theta_lim =  hydraulic->Theta (h_lim);
      tmp << "A saturated secondary domain contain " 
          << 100.0 * (Theta_sat - Theta_lim) / (Theta_sat - Theta_wp)
          << " % of plant available water\n";
      impl->primary_sorption_fraction = h_int (Theta_lim) / h_int (Theta_sat);
      tmp << "Primary domain contains "
          << 100.0 * impl->primary_sorption_fraction
          << " % of the available sorption sites\n";
    }
  tmp << "h\th\tTheta\tK\tCw2\n"
      << "cm\tpF\t\tcm/h\tcm^-1\n";
  const double h_Sat = 0;
  tmp << h_Sat << "\t" << "\t" << hydraulic->Theta (h_Sat) 
      << "\t" << hydraulic->KT20 (h_Sat)
      << "\t" << hydraulic->Cw2 (h_Sat) << "\n";
  const double pF_Zero = 0;
  const double h_Zero = pF2h (pF_Zero);
  tmp << h_Zero << "\t" << pF_Zero << "\t" << hydraulic->Theta (h_Zero) 
      << "\t" << hydraulic->KT20 (h_Zero)
      << "\t" << hydraulic->Cw2 (h_Zero) << "\n";
  const double pF_One = 1;
  const double h_One = pF2h (pF_One);
  tmp << h_One << "\t" << pF_One << "\t" << hydraulic->Theta (h_One) 
      << "\t" << hydraulic->KT20 (h_One)
      << "\t" << hydraulic->Cw2 (h_One) << "\n";
  const double pF_FC = 2.0;
  const double h_FC = pF2h (pF_FC);
  tmp << h_FC << "\t" << pF_FC << "\t" << hydraulic->Theta (h_FC) 
      << "\t" << hydraulic->KT20 (h_FC)
      << "\t" << hydraulic->Cw2 (h_FC) << "\n";
  const double pF_Three = 3;
  const double h_Three = pF2h (pF_Three);
  tmp << h_Three << "\t" << pF_Three << "\t" << hydraulic->Theta (h_Three) 
      << "\t" << hydraulic->KT20 (h_Three)
      << "\t" << hydraulic->Cw2 (h_Three) << "\n";
  const double pF_WP = 4.2;
  const double h_WP = pF2h (pF_WP);
  tmp << h_WP << "\t" << pF_WP << "\t" << hydraulic->Theta (h_WP) 
      << "\t" << hydraulic->KT20 (h_WP)
      << "\t" << hydraulic->Cw2 (h_WP);
  msg.debug (tmp.str ());
}
  
Horizon::~Horizon ()
{ }

// Create Horizon library.
const char *const Horizon::component = "horizon";

symbol
Horizon::library_id () const
{
  static const symbol id (component);
  return id;
}

static struct HorizonInit : public DeclareComponent 
{
  HorizonInit ()
    : DeclareComponent (Horizon::component, "\
A `horizon' is a soil type with specific physical properties.  It is\n\
the responsibility of the `horizon' component to specify these\n\
properties.")
  { }
  static bool check_alist (const Metalib&, const Frame& al, Treelog& err)
  {
    bool ok = true;

    daisy_assert (al.check ("hydraulic"));
    const FrameModel& hydraulic = al.model ("hydraulic");
    if (hydraulic.type_name () == "hypres"
        && !al.check ("dry_bulk_density"))
      {
        err.entry ("\
You must specify 'dry_bulk_density' when using the 'hypres' \
hydraulic model");
        ok = false;
      }
    return ok;
  }
  static void load_attributes (Frame& frame)
  {
    frame.declare_string ("key", Attribute::Const,
                   "Name of attribute.");
    frame.declare ("value", Attribute::User (), Attribute::Const,
                   "Value of attribute.");
    frame.order ("key", "value");
  }
  void load_frame (Frame& frame) const
  {
    Model::load_model (frame);
    frame.add_check (check_alist);
    frame.declare_object ("hydraulic", Hydraulic::component, 
                          "The hydraulic propeties of the soil.");
    frame.set ("hydraulic", "hypres");
    frame.declare_object ("tortuosity", Tortuosity::component, 
                          "The soil tortuosity.");
    frame.set ("tortuosity", "linear");
    frame.declare ("anisotropy", Attribute::None (),
                   Check::non_negative (), Attribute::Const, "\
Horizontal saturated water conductivity relative to vertical saturated\n\
water conductivity.  The higher this value, the faster the water will\n\
move towards drain pipes.");
    frame.set ("anisotropy", 1.0);
    frame.declare ("dry_bulk_density", "g/cm^3", Check::positive (), 
		   Attribute::OptionalConst,
                   "The soils dry bulk density.\n\
By default, this is calculated from the soil constituents.");
    frame.declare_number_cited ("quartz", Attribute::Fraction (),
				Check::fraction (), Attribute::OptionalConst,
				Attribute::Singleton, 
				"Fraction of quartz in soil minerals.\n\
By default, this is calculated from the soil constituents.\n\
quartz = clay * 0.15 + silt * 0.6 + sand * 0.7",
				"moberg1988constituents");
    frame.declare ("SOM_C_per_N", "g C/g N", Check::non_negative (), 
                   Attribute::Const, Attribute::Variable,
                   "C/N ratio for each SOM pool in this soil.\n\
If 'C_per_N' is specified, this is used as a goal only.  If 'C_per_N' is\n\
unspecified, the SOM pools will be initialized with this value.");
    std::vector<double> SOM_C_per_N;
    SOM_C_per_N.push_back (11.0);
    SOM_C_per_N.push_back (11.0);
    SOM_C_per_N.push_back (11.0);
    frame.set ("SOM_C_per_N", SOM_C_per_N);
    frame.declare ("C_per_N", "g C/g N", Check::positive (), Attribute::OptionalConst,
                   "Total C/N ratio for this horizon.\n\
This is the combined initial C/N ratio for all organic matter pools in the\n\
horizon.  The C/N ration of the AOM and SMB pools is assumed to be known,\n\
given that this number is used to find the common C/N ration for the SOM\n\
pools.  The C/N ration for the SOM pools will then gradually move towards\n\
the values specified by 'SOM_C_per_N'.\n\
By default, the values given by 'SOM_C_per_N' will be used for\n\
initialization.");
    frame.set_check ("SOM_C_per_N", VCheck::min_size_1 ());

    static const BelowOrEqual max_1 (1.0);
    frame.declare ("SOM_fractions",  Attribute::None (), max_1,
                   Attribute::OptionalConst, Attribute::Variable, "\
Fraction of humus in each SOM pool, typically slow, fast and inert.\n\
Negative numbers mean unspecified, let Daisy find appropriate values.");
    frame.set_check ("SOM_fractions", SOM_fractions_check);

    frame.declare ("turnover_factor", Attribute::None (), Check::non_negative (),
                   Attribute::Const, "\
Factor multiplied to the turnover rate for all organic matter pools in\n\
this horizon.");
    frame.set ("turnover_factor", 1.0);
    frame.declare ("root_homogeneity", Attribute::None (), Check::non_negative (),
                   Attribute::Const, "\
Factor multiplied to the root density of all crops in this horizon.\n\
The idea is to emulate the effect of roots not being distributed\n\
evenly in the soil.");
    frame.set ("root_homogeneity", 1.0);
    frame.declare ("root_retardation",
		   Attribute::None (), Check::non_negative (),
		   Attribute::Const, "\
Factor multiplied to root penetration speed.");
    frame.set ("root_retardation", 1.0);
    frame.declare ("K_factor", Attribute::None (), Check::positive (),
                   Attribute::Const, "\
Factor multiplied to the hydraulic conductivity.");
    frame.set ("K_factor", 1.0);
    frame.declare_object ("Nitrification", Nitrification::component,
                          "The soil nitrification process.");
    frame.set ("Nitrification", "soil");

    frame.declare_object ("secondary_domain", Secondary::component,
                          "Secondary matrix domain for solute movement.");
    frame.set ("secondary_domain", "none");


    frame.declare_submodule ("HorHeat", Attribute::State, 
                         "Heat capacity and conductivity.",
                         HorHeat::load_syntax);

    frame.declare_submodule_sequence ("attributes", Attribute::OptionalConst, "\
List of additional attributes for this horizon.\n\
Intended for use with pedotransfer functions.", load_attributes);
    frame.set_empty ("attributes");
    frame.declare ("r_pore_min", "um", Attribute::Const, "\
Smallest pores in the soil.");
    frame.set ("r_pore_min", 0.1);
    frame.declare_number_cited ("CEC", "cmolc/kg", Check::positive (),
                                Attribute::OptionalConst, Attribute::Singleton,
                                "Cation-Exchange Capacity.\
By default this will be calculated from a pedotransfer function based on\
Danish cultivated soils. The pedotransfer function is known to be invalid\
for podzolic B-horizons.",
                                "krogh2000cation");
  }
} Horizon_init;

// horizon.C ends here.
