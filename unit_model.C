// unit_model.C -- The 'unit' component.
// 
// Copyright 2007, 2008 Per Abrahamsen and KVL.
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

#include "unit_model.h"
#include "check.h"
#include "librarian.h"
#include "frame.h"
#include "block_model.h"
#include "mathlib.h"
#include "convert.h"
#include "units.h"
#include "model.h"
#include "frame.h"
#include "memutils.h"
#include <sstream>

const char *const MUnit::component = "unit";

symbol
MUnit::library_id () const
{
  static const symbol id (component);
  return id;
}

const Convert*
MUnit::create_convertion (const Unit&) const
{ return NULL; }

MUnit::MUnit (const BlockModel& al, const symbol base)
  : name (al.type_name ()),
    base_name_ (base)
{ }

MUnit::~MUnit ()
{ }

static struct UnitInit : public DeclareComponent 
{
  UnitInit ()
    : DeclareComponent (MUnit::component, "\
Specify units of physical quatities.\n\
\n\
Daisy will recognise both the build-in and user defined units, and\n\
convert between different units representing the same physical\n\
dimension.  This is done by converting to and from the base (usually\n\
defined by SI) unit for that dimension.")
  { }
  void load_frame (Frame& frame) const
  { Model::load_model (frame); }
} Unit_init;

// Base model 'SI'.

struct UnitSI : public MUnit
{
  // Content.
  static const struct base_unit_type
  { 
    const symbol unit; 
    const symbol dimension;
  } base_unit[];
  static const size_t base_unit_size;

  // Create and destroy.
  static symbol find_base (const BlockModel&);
  UnitSI (const BlockModel& al)
    : MUnit (al, find_base (al))
  { }
  ~UnitSI ()
  { }
};

const UnitSI::base_unit_type 
UnitSI::base_unit[] = {
  { "m", "length" },
  { "kg", "mass" },
  { "s", "time" },
  { "A", "electric_current" },
  { "K", "thermodynamic_temperature" },
  { "mol", "amount_of_substance" },
  { "cd", "luminous_intensity" }
};

const size_t 
UnitSI::base_unit_size
/**/ = sizeof (UnitSI::base_unit) / sizeof (UnitSI::base_unit_type);
  

symbol
UnitSI::find_base (const BlockModel& al)
{
  std::ostringstream tmp;
  bool found = false;
  for (size_t i = 0; i < base_unit_size; i++)
    {
      const int dim = al.integer (base_unit[i].dimension, 0);
      if (dim == 0)
        continue;
      if (found)
        tmp << " ";
      else
        found = true;
      tmp << base_unit[i].unit;
      if (dim == 1)
        continue;
      tmp << "^" << dim;
    }
  return symbol (tmp.str ());
}

static struct UnitSISyntax : public DeclareBase
{
  UnitSISyntax ()
    : DeclareBase (MUnit::component, "SI", "\
Base parameterization for all SI based units.")
  {
  }
  void load_frame (Frame& frame) const
  {
    for (size_t i = 0; i < UnitSI::base_unit_size; i++)
      {
        const symbol unit = UnitSI::base_unit[i].unit;
        const symbol dimension = UnitSI::base_unit[i].dimension;
        frame.declare_integer (dimension, Attribute::Const, "\
Dimension, base unit [" + unit + "].");
        frame.set (dimension, 0);
      }
  }
} UnitSI_syntax;

// Model 'SIfactor'.

struct UnitSIFactor : public UnitSI
{
  const double factor;
  
  double to_base (double value) const
  { return value * factor; }
  double to_native (double value) const
  { return value / factor; }
  bool in_native (double) const
  { return true; }
  bool in_base (double) const
  { return true; }

  const Convert* create_convertion (const Unit& to) const
  { 
    const UnitSIFactor* to_si_factor = dynamic_cast<const UnitSIFactor*> (&to);
    if (!to_si_factor)
      return NULL;
    
    if (base_name () != to_si_factor->base_name ())
      return NULL;

    struct ConvertFactor : public Convert
    {
      const double factor;
      double operator()(const double value) const
      { return factor * value; }
      bool valid (const double value) const
      { return true; }
      ConvertFactor (const double f)
        : factor (f)
      { }
    };

    return new ConvertFactor (this->factor / to_si_factor->factor);
  }

  UnitSIFactor (const BlockModel& al)
    : UnitSI (al),
      factor (al.number ("factor"))
  { }
};

struct DeclareSIFactor : public DeclareParam
{
  const double factor;
  const symbol super;
  const int length;
  const int mass;
  const int time;
  const int electric_current;
  const int thermodynamic_temperature;
  const int amount_of_substance;
  const int luminous_intensity;

  DeclareSIFactor (const symbol name, const double factor_,
                   const int length_, const int mass_, const int time_,
                   const int electric_current_,
                   const int thermodynamic_temperature_,
                   const int amount_of_substance_,
                   const int luminous_intensity_,
                   const symbol description)
    : DeclareParam (MUnit::component, name, "SIfactor", description),
      factor (factor_),
      length (length_),
      mass (mass_),
      time (time_),
      electric_current (electric_current_),
      thermodynamic_temperature (thermodynamic_temperature_),
      amount_of_substance (amount_of_substance_),
      luminous_intensity (luminous_intensity_)
  { }
  void load_frame (Frame& frame) const
  {
    frame.set ("length", length);
    frame.set ("mass", mass);
    frame.set ("time", time);
    frame.set ("electric_current", electric_current);
    frame.set ("thermodynamic_temperature", thermodynamic_temperature);
    frame.set ("amount_of_substance", amount_of_substance);
    frame.set ("luminous_intensity", luminous_intensity);
    frame.set ("factor", factor);
  }
};

static struct UnitSIFactorSyntax : public DeclareModel
{
  auto_vector<const DeclareSIFactor*> declarations;

  Model* make (const BlockModel& al) const
  { return new UnitSIFactor (al); }

  void add (const symbol name, const double factor,
            const int length, const int mass, const int time,
            const int electric_current,
            const int thermodynamic_temperature,
            const int amount_of_substance, const int luminous_intensity,
            const symbol description)
  {
    declarations.push_back (new DeclareSIFactor (name, factor, length, mass,
                                                 time,electric_current,
                                                 thermodynamic_temperature,
                                                 amount_of_substance, 
                                                 luminous_intensity,
                                                 description));
  }
  UnitSIFactorSyntax ()
    : DeclareModel (MUnit::component, "SIfactor", "SI", "\
Connvert to SI base units by multiplying with a factor.")
  { 
    // Prefix constants.
    static const double p_a = 1e-18; // Atto-
    static const double p_f = 1e-15; // Femto-
    static const double p_p = 1e-12; // Pico-
    static const double p_n = 1e-9; // Nano-
    static const double p_u = 1e-6; // Micro-
    static const double p_m = 1e-3; // Milli-
    static const double p_c = 1e-2; // Centi-
    static const double p_c_2 = p_c * p_c;       // Centi- squared
    static const double p_c_3 = p_c * p_c * p_c; // Centi- cubed
    static const double p_h = 1e2;               // Hecto-
    static const double p_k = 1e3;               // Kilo-
    static const double p_M = 1e6;               // Mega-

    // Units.
    static const double u_ha = p_h * p_h;        // Hectar.
    static const double u_s = 1.0;               // Second.
    static const double u_m = 60.0;              // Minute.
    static const double u_h = u_m * 60.0;        // Hour.
    static const double u_d = u_h * 24.0;        // Day.
    static const double u_y = u_d * 365.2425;    // Year.
    static const double u_l = 1e-3;              // Liter.
    static const double u_g = 1.0 / p_k;         // Gram.

    // Unitless.
    add (Attribute::None (), 1.0, 0, 0, 0, 0, 0, 0, 0,
         "Unitless.");
    add (Attribute::Fraction (), 1.0, 0, 0, 0, 0, 0, 0, 0,
         "Unitless.");
    add ("fraction", 1.0, 0, 0, 0, 0, 0, 0, 0,
         "Unitless.");
    add ("", 1.0, 0, 0, 0, 0, 0, 0, 0,
         "Unitless.");
    add ("none", 1.0, 0, 0, 0, 0, 0, 0, 0,
         "Unitless.");
    add ("%", 0.01, 0, 0, 0, 0, 0, 0, 0,
         "Percent.");
    add ("ppm", p_u, 0, 0, 0, 0, 0, 0, 0,
         "Part per million.");
    add ("mg/g", p_m, 0, 0, 0, 0, 0, 0, 0,
         "Milligram per gram.");
    add ("g/kg", 1.0 / p_k, 0, 0, 0, 0, 0, 0, 0,
         "Gram per kilogram.");
    add ("m^3/cm^3", 1.0 / p_c_3, 0, 0, 0, 0, 0, 0, 0,
         "Kilo.");
    add ("mg N/kg dry soil", p_m / p_k, 0, 0, 0, 0, 0, 0, 0,
         "Nitrogen concentration in dry soil.");
    add ("cm^3/cm^3", p_c_3 / p_c_3,
         0, 0, 0, 0, 0, 0, 0,
         "Soil water fraction.");
    add ("cm^3 H2O/cm^3", p_c_3 / p_c_3,
         0, 0, 0, 0, 0, 0, 0,
         "Soil water fraction.");

    // Length.
    add ("m", 1.0, 1, 0, 0, 0, 0, 0, 0,
         "Meter.");
    add ("cm", p_c, 1, 0, 0, 0, 0, 0, 0,
         "Centimeter.");
    add ("mm", p_m, 1, 0, 0, 0, 0, 0, 0,
         "Millimeter.");
    add ("um", p_u, 1, 0, 0, 0, 0, 0, 0,
         "Micrometer.");
    add ("l/ha", u_l / u_ha, 1, 0, 0, 0, 0, 0, 0,
         "Liter per hectar.");

    add ("m^2", 1.0, 2, 0, 0, 0, 0, 0, 0,
         "Square meter.");
    add ("cm^2", p_c_2, 2, 0, 0, 0, 0, 0, 0,
         "Square centimeter.");
    add ("ha", u_ha, 2, 0, 0, 0, 0, 0, 0,
         "Hectare.");
         
    add ("m^3", 1.0, 3, 0, 0, 0, 0, 0, 0,
         "Cube meter.");
    add ("cm^3", p_c_3, 3, 0, 0, 0, 0, 0, 0,
         "Cube centimeter.");

    add ("m^-1", 1.0, -1, 0, 0, 0, 0, 0, 0,
         "Per meter.");
    add ("cm^-1", 1.0 / p_c, -1, 0, 0, 0, 0, 0, 0,
         "Per centimeter.");
    add ("mm^-1", 1.0 / p_c, -1, 0, 0, 0, 0, 0, 0,
         "Per millimeter.");

    add ("m^-2", 1.0, -2, 0, 0, 0, 0, 0, 0,
         "Per aquare meter.");
    add ("cm^-2", 1.0 / p_c_2, -2, 0, 0, 0, 0, 0, 0,
         "Per aquare centimeter.");
    add ("cm R/cm^3", p_c / p_c_3, -2, 0, 0, 0, 0, 0, 0,
         "Root length per cubic centimeter.");
    
    // Mass.
    add ("kg", p_k * u_g, 0, 1, 0, 0, 0, 0, 0,
         "Kilogram.");
    add ("g", u_g, 0, 1, 0, 0, 0, 0, 0,
         "Gram.");
    add ("mg", p_m * u_g, 0, 1, 0, 0, 0, 0, 0,
         "Miligram.");
    add ("ug", p_u * u_g, 0, 1, 0, 0, 0, 0, 0,
         "Microgram.");

    // Time.
    add ("s", 1.0, 0, 0, 1, 0, 0, 0, 0,
         "Second.");
    add ("seconds", 1.0, 0, 0, 1, 0, 0, 0, 0,
         "Second.");
    add ("minutes", u_m, 0, 0, 1, 0, 0, 0, 0,
         "Minute.");
    add ("h", u_h, 0, 0, 1, 0, 0, 0, 0,
         "Hour.");
    add ("hours", u_h, 0, 0, 1, 0, 0, 0, 0,
         "Hour.");
    add ("d", u_d, 0, 0, 1, 0, 0, 0, 0,
         "Day.");

    add ("s^-1", 1.0, 0, 0, -1, 0, 0, 0, 0,
         "Second.");
    add ("h^-1", 1.0 / u_h, 0, 0, -1, 0, 0, 0, 0,
         "Hour.");
    add ("d^-1", 1.0 / u_d, 0, 0, -1, 0, 0, 0, 0,
         "Day.");

    // Electric currect.
    add ("A", 1.0, 0, 0, 0, 1, 0, 0, 0,
         "Ampere.");

    // Thermodynamic temperature.
    add ("K", 1.0, 0, 0, 0, 0, 1, 0, 0,
         "Kelvin.");
    add ("K^-1", -1.0, 0, 0, 0, 0, 1, 0, 0,
         "Per kelvin.");
    add ("K^-2", -2.0, 0, 0, 0, 0, 1, 0, 0,
         "Per kelvin squared.");

    // Amount of substance.
    add ("mol", 1.0, 0, 0, 0, 0, 0, 1, 0,
         "Mole.");
    
    // Luminous intensity.
    add ("cd", 1.0, 0, 0, 0, 0, 0, 0, 1,
         "Candela.");
    
    // Speed.
    add ("m/s", 1.0, 1, 0, -1, 0, 0, 0, 0,
         "Base speed.");
    add ("m s^-1", 1.0, 1, 0, -1, 0, 0, 0, 0,
         "Base speed.");
    add ("mm/s", p_m / u_s, 1, 0, -1, 0, 0, 0, 0,
         "Percolation intensity.");
    add ("mm/h", p_m / u_h, 1, 0, -1, 0, 0, 0, 0,
         "Percolation intensity.");
    add ("mm/d", p_m / u_d, 
         1, 0, -1, 0, 0, 0, 0,
         "Percolation intensity.");
    add ("cm/h", p_c / u_h, 1, 0, -1, 0, 0, 0, 0,
         "Soil water movement.");
    add ("cm/d", p_c / u_d, 
         1, 0, -1, 0, 0, 0, 0,
         "Soil water movement.");
    add ("l/ha/h", u_l / u_ha / u_h, 1, 0, -1, 0, 0, 0, 0,
         "Liter per hectar per hour.");

    // Mass per length.
    add ("kg/m", 1e0, -1, 1, 0, 0, 0, 0, 0,
         "Base mass per length.");

    // Mass per time.
    add ("kg/s", 1e0, 0, 1, -1, 0, 0, 0, 0,
         "Base mass per time.");
    add ("g/h", u_g / u_h, 0, 1, -1, 0, 0, 0, 0,
         "Gram per hour.");
    add ("kg/h", 1.0 / u_h, 0, 1, -1, 0, 0, 0, 0,
         "Kilogram per hour.");
    
    // Mass per length flux.
    add ("kg/m/s", p_k * u_g, -1, 1, -1, 0, 0, 0, 0,
         "Base mass per length flux.");
    add ("ng/mm/h", p_n * u_g / p_m / u_h,
         -1, 1, -1, 0, 0, 0, 0,
         "Mass per length flux.");
    add ("g/cm/h", u_g / p_c / u_h,
         -1, 1, -1, 0, 0, 0, 0,
         "Mass per length flux.");
    add ("g/cm R/h", u_g / p_c / u_h,
         -1, 1, -1, 0, 0, 0, 0,
         "Gram per centimeter root per hour.");
      
    // Mass per area.
    add ("kg/m^2", p_k * u_g, -2, 1, 0, 0, 0, 0, 0,
         "Base mass per area.");
    add ("mg/m^2", p_m * u_g, -2, 1, 0, 0, 0, 0, 0,
         "Crop scale pesticide per area.");
    add ("g/m^2", u_g, -2, 1, 0, 0, 0, 0, 0,
         "Crop scale mass per area.");
    add ("kg DM/m^2", p_k * u_g, -2, 1, 0, 0, 0, 0, 0,
         "Base mass per area.");
    add ("g DM/m^2", u_g, -2, 1, 0, 0, 0, 0, 0,
         "Crop scale dry matter per area.");
    add ("g/cm^2", u_g / p_c_2, -2, 1, 0, 0, 0, 0, 0,
         "Soil scale mass per area.");
    add ("g N/cm^2", u_g / p_c_2, -2, 1, 0, 0, 0, 0, 0,
         "Soil scale nitrogen per area.");
    add ("g C/cm^2", u_g / p_c_2, -2, 1, 0, 0, 0, 0, 0,
         "Soil scale carbon per area.");
    add ("g N/m^2", u_g, -2, 1, 0, 0, 0, 0, 0,
         "Crop scale nitrogen per area.");
    add ("g C/m^2", u_g, -2, 1, 0, 0, 0, 0, 0,
         "Crop scale carbon per area.");
    add ("g/ha", u_g / u_ha, -2, 1, 0, 0, 0, 0, 0,
         "Field scale pesticide mass per area.");
    add ("g DM/ha", u_g / u_ha, -2, 1, 0, 0, 0, 0, 0,
         "Field scale crop mass per area.");
    add ("mg/ha", p_m * u_g / u_ha, -2, 1, 0, 0, 0, 0, 0,
         "Miligram per hectare");
    add ("ug/ha", p_u * u_g / u_ha, -2, 1, 0, 0, 0, 0, 0,
         "Microgram per hectare");
    add ("ug/m^2", p_u * u_g ,
         -2, 1, 0, 0, 0, 0, 0,
         "Microgram per square meter.");
    add ("g w.w./m^2", u_g, -2, 1, 0, 0, 0, 0, 0,
         "Wet weight per area.");
    add ("kg/ha", p_k * u_g / u_ha, -2, 1, 0, 0, 0, 0, 0,
         "Field scale mass per area.");
    add ("kg N/ha", p_k * u_g / u_ha, -2, 1, 0, 0, 0, 0, 0,
         "Field scale nitrogen per area.");
    add ("kg C/ha", p_k * u_g / u_ha, -2, 1, 0, 0, 0, 0, 0,
         "Field scale carbon per area.");
    add ("kg w.w./ha", p_k * u_g / u_ha,
         -2, 1, 0, 0, 0, 0, 0,
         "Wet weight per area.");
    add ("Mg/ha", p_M * u_g / u_ha, 
         -2, 1, 0, 0, 0, 0, 0,
         "Ton per hectar.");
    add ("t/ha", p_M * u_g / u_ha, 
         -2, 1, 0, 0, 0, 0, 0,
         "Ton per hectar.");
    add ("g DM/cm^2", u_g / p_c_2, 
         -2, 1, 0, 0, 0, 0, 0,
         "Gram dry matter per square centimeter.");
    add ("kg DM/ha", p_k * u_g / u_ha, 
         -2, 1, 0, 0, 0, 0, 0,
         "Kilogram dry matter per hectar.");
    add ("Mg DM/ha", p_M * u_g / u_ha, 
         -2, 1, 0, 0, 0, 0, 0,
         "Ton dry matter per hectar.");
    add ("Mg w.w./ha", p_M * u_g / u_ha, 
         -2, 1, 0, 0, 0, 0, 0,
         "Wet weight per area.");
    add ("T w.w./ha", p_M * u_g / u_ha, 
         -2, 1, 0, 0, 0, 0, 0,
         "Wet weight per area.");

      // Mass per area flux.
    add ("kg/m^2/s", p_k * u_g, -2, 1, -1, 0, 0, 0, 0,
         "Base mass per area flux.");
    add (Unit::mass_per_area_per_time ().name (),
         p_k * u_g, -2, 1, -1, 0, 0, 0, 0,
         "Base mass per area flux.");
    add ("kg m^-2 s^-1", p_k * u_g, -2, 1, -1, 0, 0, 0, 0,
         "Base mass per area flux.");
    add ("g/m^2/h", u_g / u_h, 
         -2, 1, -1, 0, 0, 0, 0,
         "Gram per square meter per hour.");
    add ("g/cm^2/h", u_g / p_c_2 / u_h, 
         -2, 1, -1, 0, 0, 0, 0,
         "Mass per area flux.");
    add ("g N/m^2/h", u_g /  u_h, 
         -2, 1, -1, 0, 0, 0, 0,
         "Nitrogen per aquare meter per hour.");
    add ("g N/cm^2/h", u_g / p_c_2 / u_h, 
         -2, 1, -1, 0, 0, 0, 0,
         "Nitrogen per aquare centimeter per hour.");
    add ("g/ha/h", u_g / u_ha / u_h,
         -2, 1, -1, 0, 0, 0, 0,
         "Pesticide application.");
    add ("mg/ha/h", p_m * u_g / u_ha / u_h,
         -2, 1, -1, 0, 0, 0, 0,
         "Pesticide application.");
    add ("mg/ha/d", p_m * u_g / u_ha / u_d,
         -2, 1, -1, 0, 0, 0, 0,
         "Pesticide application.");
    add ("ug/ha/d", p_u * u_g / u_ha / u_d,
         -2, 1, -1, 0, 0, 0, 0,
         "Pesticide application.");
    add ("ug/ha/h", p_u * u_g / u_ha / u_h,
         -2, 1, -1, 0, 0, 0, 0,
         "Pesticide application.");
    add ("g/ha/d", u_g / u_ha / u_d,
         -2, 1, -1, 0, 0, 0, 0,
         "Pesticide application.");
    add ("kg N/ha/h", p_k * u_g / u_ha / u_h,
         -2, 1, -1, 0, 0, 0, 0,
         "Field scale application and removal of nitrogen.");
    add ("kg C/ha/h", p_k * u_g / u_ha / u_h,
         -2, 1, -1, 0, 0, 0, 0,
         "Field scale application and removal of carbon.");
    add ("kg/ha/h", p_k * u_g / u_ha / u_h,
         -2, 1, -1, 0, 0, 0, 0,
         "Harvest and fertilizing.");
    add ("kg N/ha/d", p_k * u_g / u_ha / u_d,
         -2, 1, -1, 0, 0, 0, 0,
         "Field scale nitrogen.");
    add ("kg C/ha/d", p_k * u_g / u_ha / u_d,
         -2, 1, -1, 0, 0, 0, 0,
         "Field scale carbon.");
    add ("kg/ha/y", p_k * u_g / u_ha / u_y,
         -2, 1, -1, 0, 0, 0, 0,
         "Deposition.");
    add ("kg N/ha/y", p_k * u_g / u_ha / u_y,
         -2, 1, -1, 0, 0, 0, 0,
         "Deposition.");
    add ("kgN/ha/year", p_k * u_g / u_ha / u_y,
         -2, 1, -1, 0, 0, 0, 0,
         "Deposition.");
    add ("Mg DM/ha/h", p_M * u_g / u_ha / u_h,
         -2, 1, -1, 0, 0, 0, 0,
         "Harvest and fertilizing.");
    add ("ug/m^2/h", p_u * u_g / u_h,
         -2, 1, -1, 0, 0, 0, 0,
         "Microgram per square meter per hour.");

    // Mass per volume.
    add ("kg/m^3", p_k * u_g, -3, 1, 0, 0, 0, 0, 0,
         "Base mass per volume.");
    add ("kg DM/m^3", p_k * u_g, -3, 1, 0, 0, 0, 0, 0,
         "Mass per volume.");
    add ("ng/cm^3", p_n * u_g / p_c_3,
         -3, 1, 0, 0, 0, 0, 0,
         "Low solute concentration.");
    add ("ug/m^3", p_u * u_g, -3, 1, 0, 0, 0, 0, 0,
         "Mass per volume.");
    add ("g/l", u_g / u_l, -3, 1, 0, 0, 0, 0, 0,
         "Concentration.");
    add ("g/L", u_g / u_l, -3, 1, 0, 0, 0, 0, 0,
         "Concentration.");
    add ("g C/L", u_g / u_l, -3, 1, 0, 0, 0, 0, 0,
         "Concentration.");
    add ("mg/l", p_m * u_g / u_l, -3, 1, 0, 0, 0, 0, 0,
         "Concentration.");
    add ("mg/L", p_m * u_g / u_l, -3, 1, 0, 0, 0, 0, 0,
         "Concentration.");
    add ("ug/L", p_u * u_g / u_l, -3, 1, 0, 0, 0, 0, 0,
         "Concentration.");
    add ("ug/l", p_u * u_g / u_l, -3, 1, 0, 0, 0, 0, 0,
         "Low solute concentration.");
    add ("ng/L", p_n * u_g / u_l, -3, 1, 0, 0, 0, 0, 0,
         "Concentration.");
    add ("ng/l", p_n * u_g / u_l, -3, 1, 0, 0, 0, 0, 0,
         "Lower solute concentration.");
    add ("pg/L", p_p * u_g / u_l, -3, 1, 0, 0, 0, 0, 0,
         "Concentration.");
    add ("fg/L", p_f * u_g / u_l, -3, 1, 0, 0, 0, 0, 0,
         "Concentration.");
    add ("ag/L", p_a * u_g / u_l, -3, 1, 0, 0, 0, 0, 0,
         "Concentration.");
    add ("mg N/l", p_m * u_g / u_l, -3, 1, 0, 0, 0, 0, 0,
         "Nitrogen concentration.");
    add ("g N/l", u_g / u_l, -3, 1, 0, 0, 0, 0, 0,
         "Nitrogen concentration.");
    add ("g/cm^2/mm", u_g / p_c_2 / p_m,
         -3, 1, 0, 0, 0, 0, 0,
         "Irrigation and percolation concentration.");
    add ("g/cm^3", u_g / p_c_3, -3, 1, 0, 0, 0, 0, 0,
         "Solute concentration.");
    add ("g C/cm^3", u_g / p_c_3, -3, 1, 0, 0, 0, 0, 0,
         "Carbon concentration.");
    add ("g N/cm^3", u_g / p_c_3, -3, 1, 0, 0, 0, 0, 0,
         "Nitrogen concentration.");
    add ("kg/ha/mm", p_k * u_g / u_ha / p_m,
         -3, 1, 0, 0, 0, 0, 0,
         "Irrigation and percolation concentration.");
    add ("kg N/ha/mm", p_k * u_g / u_ha / p_m,
         -3, 1, 0, 0, 0, 0, 0,
         "Irrigation and percolation concentration.");
    add ("g/ha/mm", u_g / u_ha / p_m,
         -3, 1, 0, 0, 0, 0, 0,
         "gram per hectare per millimeter.");
    
    add ("g/cm^3/h", u_g / p_c_3 / u_h, -3, 1, -1, 0, 0, 0, 0,
         "Gram per cubic centimeter per hour.");
    
    // Area per time.
    add ("m^2/s", 1.0, 2, 0, -1, 0, 0, 0, 0,
         "Base area per time unit.");
    add ("cm^2/s", p_c_2 / u_s, 2, 0, -1, 0, 0, 0, 0,
         "Square centimeters per second.");
    add ("cm^2/h", p_c_2 / u_h, 2, 0, -1, 0, 0, 0, 0,
         "Square centimeters per hour.");
      
    // Volume per mass.
    add ("m^3/kg", 1e0, 3, -1, 0, 0, 0, 0, 0,
         "Base volume per mass.");
    add ("l/kg", u_l, 3, -1, 0, 0, 0, 0, 0,
         "Volume per mass.");
    add ("L/kg", u_l, 3, -1, 0, 0, 0, 0, 0,
         "Volume per mass.");
    add ("l/g", u_l / u_g, 3, -1, 0, 0, 0, 0, 0,
         "Volume per mass.");
    add ("L/g", u_l / u_g, 3, -1, 0, 0, 0, 0, 0,
         "Volume per mass.");
    add ("l/mg", u_l / (p_m * u_g), 3, -1, 0, 0, 0, 0, 0,
         "Volume per mass.");
    add ("L/mg", u_l / (p_m * u_g), 3, -1, 0, 0, 0, 0, 0,
         "Volume per mass.");
    add ("cm^3/g", p_c_3 / u_g, 3, -1, 0, 0, 0, 0, 0,
         "Volume per mass.");
    add ("ml/g", p_c_3 / u_g, 3, -1, 0, 0, 0, 0, 0,
         "Volume per mass.");
    add ("mL/g", p_c_3 / u_g, 3, -1, 0, 0, 0, 0, 0,
         "Volume per mass.");
    add ("ml/kg", p_c_3, 3, -1, 0, 0, 0, 0, 0,
         "Volume per mass.");
    add ("mL/kg", p_c_3, 3, -1, 0, 0, 0, 0, 0,
         "Volume per mass.");
    add ("cm^3/ng", p_c_3 / (p_n * u_g),
         3, -1, 0, 0, 0, 0, 0,
         "Volume per mass.");
    
    // Pressure.
    add ("Pa", 1e0, -1, 1, -2, 0, 0, 0, 0,
         "Pascal.");
    add ("hPa", p_h, -1, 1, -2, 0, 0, 0, 0,
         "Hectopascal.");
    add ("kPa", p_k, -1, 1, -2, 0, 0, 0, 0,
         "Kilopascal.");
    add ("MPa", p_M, -1, 1, -2, 0, 0, 0, 0,
         "Megapascal.");

    // Inverse Pressure.
    add ("Pa^-1", 1e0, 1, -1, 2, 0, 0, 0, 0,
         "Base inverse pressure.");
    add ("MPa^-1", 1e0 / p_M, 1, -1, 2, 0, 0, 0, 0,
         "Inverse pressure.");

    // Mass per pressure.
    add ("kg/Pa", 1.0, 1, 0, 2, 0, 0, 0, 0,
         "Base mass per pressure.");
    add ("ng/kPa", p_n * u_g / p_k, 1, 0, 2, 0, 0, 0, 0,
         "Nanogram per kilopascal.");
    add ("ng/MPa", p_n * u_g / p_M, 1, 0, 2, 0, 0, 0, 0,
         "Nanogram per megapascal.");

    // Mass per volume per pressure.
    add ("ug/L/MPa", p_u / u_l / p_M, -2, 0, 2, 0, 0, 0, 0,
         "Microgram per liter per megapascal.");
    
    // Amount of substance per area.
    add ("mol/m^2", 1.0, -2, 0, 0, 0, 0, 1, 0,
         "Mole per square meter.");
    add ("mmol/m^2", p_m, -2, 0, 0, 0, 0, 1, 0,
         "Millimole per square meter.");

    // Amount of substance flux per area.
    add ("mol/m^2/s", 1.0, -2, 0, -1, 0, 0, 1, 0,
         "Mole per square meter per second.");
    add ("mmol/m^2/s", p_m, -2, 0, -1, 0, 0, 1, 0,
         "Millimole per square meter per second.");

    // Amount of substance per mass.
    add ("mol/kg", 1.0, 0, -1, 0, 0, 0, 1, 0,
         "Mole per kilogram.");
    add ("mmol/kg", p_m, 0, -1, 0, 0, 0, 1, 0,
         "Millimole per kilogram.");

    // Mass per amount of substance.
    add ("kg/mol", 1.0, 0, 1, 0, 0, 0, -1, 0,
         "Kilogram per mole.");
    add ("g/mol", u_g, 0, 1, 0, 0, 0, -1, 0,
         "Gram per mole.");
    
    // Energy.
    add ("J", 1.0, 2, 1, -2, 0, 0, 0, 0,
         "Joule.");

    // Energy flux.
    add ("W", 1.0, 2, 1, -3, 0, 0, 0, 0,
         "Watt.");

    // Energy per area,
    add ("J/m^2", 1.0, 2, 1, -2, 0, 0, 0, 0,
         "Joule per square meter.");
    add ("kJ/m^2", p_k, 2, 1, -2, 0, 0, 0, 0,
         "Kilojoule per square meter.");
    add ("MJ/m^2", p_M, 2, 1, -2, 0, 0, 0, 0,
         "Megajoule per square meter.");
    add ("Wh/m^2", u_h, 2, 1, -2, 0, 0, 0, 0,
         "Watthours per square meter.");
    add ("kWh/m^2", p_k * u_h, 2, 1, -2, 0, 0, 0, 0,
         "Kilowatthours per square meter.");
    
         // Energy flux per area.
    add ("W/m^2", 1.0, 0, 1, -3, 0, 0, 0, 0,
         "Watt per square meter.");
    add ("W m^-2", 1.0, 0, 1, -3, 0, 0, 0, 0,
         "Watt per square meter.");
    add ("W/cm^2", 1.0 / p_c_2, 0, 1, -3, 0, 0, 0, 0,
         "Watt per square centimeter.");
    add ("J/cm^2/h", 1.0 / p_c_2 / u_h, 0, 1, -3, 0, 0, 0, 0,
         "Joule per square centimeter per hour.");
    add ("MJ/d/m^2", p_M / u_d,
         0, 1, -3, 0, 0, 0, 0,
         "Megajoule per day per square meter.");
    add ("MJ/m^2/d", p_M / u_d,
         0, 1, -3, 0, 0, 0, 0,
         "Megajoule per square meter per day.");
    add ("MJ/h/m^2", p_M / u_h,
         0, 1, -3, 0, 0, 0, 0,
         "Megajoule per hour per square meter.");
    add ("MJ/m^2/h", p_M / u_h,
         0, 1, -3, 0, 0, 0, 0,
         "Megajoule per square meter per hour.");
    add ("erg/cm^2/h", 2.778e-7,
         0, 1, -3, 0, 0, 0, 0,
         "erg per square centimer per hour.");

    // Lenght per temperature per time.
    add ("mm/dg C/d", p_m / u_d, 1, 0, -1, 0, -1, 0, 0,
         "Root growth.");
    add ("cm/dg C/d", p_c / u_d, 1, 0, -1, 0, -1, 0, 0,
         "Root growth.");
  }
  void load_frame (Frame& frame) const
  {
    static const symbol name ("SIfactor");
    
    // Add the 'SIfactor' base model.
    frame.declare ("factor", Attribute::None (), Check::non_zero (), Attribute::Const, "\
Factor to multiply with to get base unit.");
  }
} UnitSIFactor_syntax;

// Model 'pF'.

struct UnitpF : public MUnit
{
  double to_base (double value) const
  { return pF2h (value) * 100.0 /* Pa/cm */; }
  double to_native (double value) const
  { return h2pF (value / 100.0) /* cm/pa */; }
  bool in_native (double value) const
  { return value >= 0.0; }
  bool in_base (double value) const
  { return value < 0.0; }

  UnitpF (const BlockModel& al)
    : MUnit (al, Unit::pressure ())
  { }
};

static struct UnitpFSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new UnitpF (al); }

  UnitpFSyntax ()
    : DeclareModel (MUnit::component, "pF", "log10 (- cmH2O).")
  { }
  void load_frame (Frame& frame) const
  {
    // Add the 'pF' base model.
  }
} UnitpF_syntax;

// Model 'base'.

struct UnitBase : public MUnit
{
  double to_base (double value) const
  { return value; }
  double to_native (double value) const
  { return value; }
  bool in_native (double value) const
  { return true; }
  bool in_base (double value) const
  { return true; }

  UnitBase (const BlockModel& al)
    : MUnit (al, al.type_name ())
  { }
};


static struct UnitBaseSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new UnitBase (al); }
  
  UnitBaseSyntax ()
    : DeclareModel (MUnit::component, "base", "A base unit.")
  { }
  void load_frame (Frame&) const
  { }
} UnitBase_syntax;

struct DeclareBaseUnit : public DeclareParam
{
  DeclareBaseUnit (const symbol name, const symbol description)
    : DeclareParam (MUnit::component, name, "base", description)
  { }
  void load_frame (Frame&) const
  { }
};

// Angles
static DeclareBaseUnit Base_rad ("rad", "Radians");

// Add geographical coordinates.
static DeclareBaseUnit Base_dgEast ("dgEast", "Degrees East of Greenwich.");
static DeclareBaseUnit Base_dgNorth ("dgNorth", "Degrees North of Equator.");

// Soil fraction.
static DeclareBaseUnit Base_DS_fraction (Units::dry_soil_fraction (), 
                                         "Fraction of dry soil.");

// Unknown unit.
static DeclareBaseUnit Base_unknown (Attribute::Unknown (), "\
Nothing is known about the dimension of this unit.");
static DeclareBaseUnit Base_error (Units::error_symbol (), "Bogus unit.");

// Model 'factor'.

struct UnitFactor : public MUnit
{
  const double factor;

  double to_base (double value) const
  { return value * factor; }
  double to_native (double value) const
  { return value / factor; }
  bool in_native (double value) const
  { return true; }
  bool in_base (double value) const
  { return true; }

  UnitFactor (const BlockModel& al)
    : MUnit (al, al.name ("base")),
      factor (al.number ("factor"))
  { }
};

static struct UnitFactorSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new UnitFactor (al); }
  UnitFactorSyntax ()
    : DeclareModel (MUnit::component, "factor", "\
Connvert to base units by multiplying with a factor.")
  { }
  void load_frame (Frame& frame) const
  {
    // Add the 'factor' factor model.
    frame.declare_string ("base", Attribute::Const, "\
Base unit to convert to and from.");
    // TODO: Should add check that 'base' is indeed a base unit.
    frame.declare ("factor", Attribute::None (), Check::non_zero (), Attribute::Const, "\
Factor to multiply with to get base unit.");
  }
} UnitFactor_syntax;

struct DeclareBaseFactor : public DeclareParam
{
  const double factor;
  const symbol base;
  DeclareBaseFactor (const symbol name, const double factor_,
                     const symbol base_, const symbol description)
    : DeclareParam (MUnit::component, name, "factor", description),
      factor (factor_),
      base (base_)
  { }
  void load_frame (Frame& frame) const
  { 
    frame.set ("base", base);
    frame.set ("factor", factor);
  }
};

// Add angles.
static DeclareBaseFactor Base_dg ("dg", M_PI / 180.0, "rad", "\
Degrees");
static DeclareBaseFactor Base_new_dg ("new dg", M_PI / 200.0, "rad", "\
New degrees");

// Add geographical coordinates.
static DeclareBaseFactor Base_dg_West ("dgWest", -1.0, "dgEast", "\
Degrees West of Greenwich.");
static DeclareBaseFactor Base_dgSouth ("dgSouth", -1.0, "dgNorth", "\
Degrees North of Equator.");

// Add dry soil.
static DeclareBaseFactor Base_DS_ppm ("ppm dry soil", 1e-6, 
                                      Units::dry_soil_fraction (), "\
Part per million in dry soil.");

// Model 'offset'.

struct UnitOffset : public MUnit
{
  const double factor;
  const double offset;

  double to_base (double value) const
  { return value * factor + offset; }
  double to_native (double value) const
  { return (value - offset) / factor; }
  bool in_native (double value) const
  { return true; }
  bool in_base (double value) const
  { return true; }

  UnitOffset (const BlockModel& al)
    : MUnit (al, al.name ("base")),
      factor (al.number ("factor")),
      offset (al.number ("offset"))
  { }
};

static struct UnitOffsetSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new UnitOffset (al); }

  UnitOffsetSyntax ()
    : DeclareModel (MUnit::component, "offset", "\
Connvert to base units by multiplying factor, then substracting offset.")
  { }
  void load_frame (Frame& frame) const
  {
    // Add the 'SIoffset' offset model.
    frame.declare_string ("base", Attribute::Const, "\
Base unit to convert to and from.");
    // TODO: Should add check that 'base' is indeed a base unit.
    frame.declare ("factor", Attribute::None (), Check::non_zero (), Attribute::Const, "\
Factor to multiply with to get base unit.");
    frame.set ("factor", 1.0);
    frame.declare ("offset", Attribute::None (), Attribute::Const, "\
Offset to add after multiplying with factor to get base unit.");
    frame.set ("offset", 0.0);
  }
} UnitOffset_syntax;

struct DeclareBaseOffset : public DeclareParam
{
  const double factor;
  const double offset;
  const symbol base;
  DeclareBaseOffset (const symbol name, const double factor_,
                     const double offset_, const symbol base_,
                     const symbol description)
    : DeclareParam (MUnit::component, name, "offset", description),
      factor (factor_),
      offset (offset_),
      base (base_)
  { }
  void load_frame (Frame& frame) const
  { 
    frame.set ("base", base);
    frame.set ("factor", factor);
    frame.set ("offset", offset);
  }
};


static DeclareBaseOffset Base_Celcius ("dg C", 1.0, 273.15, "K", "\
degree Celcius.");
// Absolute zero is -459.67 degree Fahrenheit.
static DeclareBaseOffset Base_Fahrenheit ("dg F", 
                                          5.0/9.0, 459.67 * 5.0 / 9.0, "K", "\
degree Fahrenheit.");

// unit_model.C ends here.
