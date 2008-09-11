// unit.C -- Specify unit for scalar.
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

#include "unit.h"
#include "check.h"
#include "librarian.h"
#include "alist.h"
#include "syntax.h"
#include "block.h"
#include "mathlib.h"
#include "convert.h"
#include <sstream>

// Component 'unit'.

const char *const Unit::component = "unit";

symbol
Unit::library_id () const
{
  static const symbol id (component);
  return id;
}

symbol
Unit::pressure ()
{
  static const symbol unit ("m^-1 kg s^-2");
  return unit;
}

symbol
Unit::mass_per_volume ()
{
  static const symbol unit ("m^-3 kg");
  return unit;
}

symbol
Unit::amount_of_substance_per_area_per_time ()
{
  static const symbol unit ("m^-2 s^-1 mol");
  return unit;
}

symbol
Unit::energy_per_area_per_time ()
{
  static const symbol unit ("kg s^-3");
  return unit;
}

const Convert*
Unit::create_convertion (const Unit&) const
{ return NULL; }

Unit::Unit (Block& al, const symbol base)
  : name (al.identifier ("type")),
    base_name_ (base)
{ }

Unit::~Unit ()
{ }

static Librarian Unit_init (Unit::component, "\
Specify units of physical quatities.\n\
\n\
Daisy will recognise both the build-in and user defined units, and\n\
convert between different units representing the same physical\n\
dimension.  This is done by converting to and from the base (usually\n\
defined by SI) unit for that dimension.");

// Base model 'SI'.

struct UnitSI : public Unit
{
  // Content.
  static const struct base_unit_type
  { 
    std::string unit; 
    std::string dimension;
  } base_unit[];
  static const size_t base_unit_size;

  // Create and destroy.
  static void load_syntax (Syntax& syntax, AttributeList& alist);
  static symbol find_base (Block& al);
  UnitSI (Block& al)
    : Unit (al, find_base (al))
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
  

void
UnitSI::load_syntax (Syntax& syntax, AttributeList& alist)
{
  for (size_t i = 0; i < base_unit_size; i++)
    {
      const std::string unit = base_unit[i].unit;
      const std::string dimension = base_unit[i].dimension;
      syntax.add (dimension, Syntax::Integer, Syntax::Const, "\
Dimension, base unit [" + unit + "].");
      alist.add (dimension, 0);
    }
}

symbol
UnitSI::find_base (Block& al)
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

  UnitSIFactor (Block& al)
    : UnitSI (al),
      factor (al.number ("factor"))
  { }
};

static struct UnitSIFactorSyntax
{
  static Model& make (Block& al)
  { return *new UnitSIFactor (al); }

  static void add (const std::string& name_string, const double factor,
                   const symbol super,
                   const Syntax& super_syntax, const AttributeList& super_alist,
                   const int length, const int mass, const int time,
                   const int electric_current,
                   const int thermodynamic_temperature,
                   const int amount_of_substance, const int luminous_intensity,
                   const std::string& description)
  {
    const symbol name (name_string);

    AttributeList& alist = *new AttributeList (super_alist);
    alist.add ("type", super);
    alist.add ("length", length);
    alist.add ("mass", mass);
    alist.add ("time", time);
    alist.add ("electric_current", electric_current);
    alist.add ("thermodynamic_temperature", thermodynamic_temperature);
    alist.add ("amount_of_substance", amount_of_substance);
    alist.add ("luminous_intensity", luminous_intensity);
    alist.add ("factor", factor);
    alist.add ("description", description);
    Librarian::add_type (Unit::component, name, alist, super_syntax, &make);
  }
  UnitSIFactorSyntax ()
  {
    static const symbol name ("SIfactor");
    
    // Add the 'SIfactor' base model.
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "\
Connvert to SI base units by multiplying with a factor.");
    UnitSI::load_syntax (syntax, alist);
    syntax.add ("factor", Syntax::None (), Check::non_zero (), Syntax::Const, "\
Factor to multiply with to get base unit.");
    Librarian::add_type (Unit::component, name, alist, syntax, &make);

    // Prefix constants.
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
    static const double u_h = 60.0 * 60.0;       // Hour.
    static const double u_d = u_h * 24.0;        // Day.
    static const double u_y = u_d * 365.2425;    // Year.
    static const double u_l = 1e-3;              // Liter.
    static const double u_g = 1.0 / p_k;         // Gram.

    // Unitless.
    add (Syntax::None (), 1.0, name, syntax, alist, 0, 0, 0, 0, 0, 0, 0,
         "Unitless.");
    add (Syntax::Fraction (), 1.0, name, syntax, alist, 0, 0, 0, 0, 0, 0, 0,
         "Unitless.");
    add ("", 1.0, name, syntax, alist, 0, 0, 0, 0, 0, 0, 0,
         "Unitless.");
    add ("none", 1.0, name, syntax, alist, 0, 0, 0, 0, 0, 0, 0,
         "Unitless.");
    add ("%", 0.01, name, syntax, alist, 0, 0, 0, 0, 0, 0, 0,
         "Percent.");
    add ("ppm", p_u, name, syntax, alist, 0, 0, 0, 0, 0, 0, 0,
         "Part per million.");
    add ("mg N/kg dry soil", 1e-6, name, syntax, alist, 0, 0, 0, 0, 0, 0, 0,
         "Nitrogen concentration in dry soil.");
    add ("cm^3/cm^3", p_c_3 / p_c_3, name, syntax, alist,
         0, 0, 0, 0, 0, 0, 0,
         "Soil water fraction.");
    add ("cm^3 H2O/cm^3", p_c_3 / p_c_3, name, syntax, alist,
         0, 0, 0, 0, 0, 0, 0,
         "Soil water fraction.");

    // Length.
    add ("m", 1.0, name, syntax, alist, 1, 0, 0, 0, 0, 0, 0,
         "Meter.");
    add ("cm", p_c, name, syntax, alist, 1, 0, 0, 0, 0, 0, 0,
         "Centimeter.");
    add ("mm", p_m, name, syntax, alist, 1, 0, 0, 0, 0, 0, 0,
         "Millimeter.");

    add ("m^2", 1.0, name, syntax, alist, 2, 0, 0, 0, 0, 0, 0,
         "Square meter.");
    add ("cm^2", p_c_2, name, syntax, alist, 2, 0, 0, 0, 0, 0, 0,
         "Square centimeter.");
    add ("ha", u_ha, name, syntax, alist, 2, 0, 0, 0, 0, 0, 0,
         "Hectare.");

    add ("m^3", 1.0, name, syntax, alist, 3, 0, 0, 0, 0, 0, 0,
         "Cube meter.");
    add ("cm^3", p_c_3, name, syntax, alist, 3, 0, 0, 0, 0, 0, 0,
         "Cube centimeter.");

    add ("m^-1", 1.0, name, syntax, alist, -1, 0, 0, 0, 0, 0, 0,
         "Per meter.");
    add ("cm^-1", 1.0 / p_c, name, syntax, alist, -1, 0, 0, 0, 0, 0, 0,
         "Per centimeter.");

    add ("m^-2", 1.0, name, syntax, alist, -2, 0, 0, 0, 0, 0, 0,
         "Per aquare meter.");
    add ("cm^-2", 1.0 / p_c_2, name, syntax, alist, -2, 0, 0, 0, 0, 0, 0,
         "Per aquare centimeter.");

    // Mass.
    add ("kg", p_k * u_g, name, syntax, alist, 0, 1, 0, 0, 0, 0, 0,
         "Kilogram.");
    add ("g", u_g, name, syntax, alist, 0, 1, 0, 0, 0, 0, 0,
         "Gram.");

    // Time.
    add ("s", 1.0, name, syntax, alist, 0, 0, 1, 0, 0, 0, 0,
         "Second.");
    add ("h", u_h, name, syntax, alist, 0, 0, 1, 0, 0, 0, 0,
         "Hour.");
    add ("d", u_d, name, syntax, alist, 0, 0, 1, 0, 0, 0, 0,
         "Day.");

    add ("s^-1", 1.0, name, syntax, alist, 0, 0, -1, 0, 0, 0, 0,
         "Second.");
    add ("h^-1", 1.0 / u_h, name, syntax, alist, 0, 0, -1, 0, 0, 0, 0,
         "Hour.");
    add ("d^-1", 1.0 / u_d, name, syntax, alist, 0, 0, -1, 0, 0, 0, 0,
         "Day.");

    // Electric currect.
    add ("A", 1.0, name, syntax, alist, 0, 0, 0, 1, 0, 0, 0,
         "Ampere.");

    // Thermodynamic temperature.
    add ("K", 1.0, name, syntax, alist, 0, 0, 0, 0, 1, 0, 0,
         "Kelvin.");

    // Amount of substance.
    add ("mol", 1.0, name, syntax, alist, 0, 0, 0, 0, 0, 1, 0,
         "Mole.");
    
    // Luminous intensity.
    add ("cd", 1.0, name, syntax, alist, 0, 0, 0, 0, 0, 0, 1,
         "Candela.");
    
    // Speed.
    add ("m/s", 1.0, name, syntax, alist, 1, 0, -1, 0, 0, 0, 0,
         "Base speed.");
    add ("mm/h", p_m / u_h, name, syntax, alist, 1, 0, -1, 0, 0, 0, 0,
         "Percolation intensity.");
    add ("mm/d", p_m / u_d, name, syntax, alist, 
         1, 0, -1, 0, 0, 0, 0,
         "Percolation intensity.");
    add ("cm/h", p_c / u_h, name, syntax, alist, 1, 0, -1, 0, 0, 0, 0,
         "Soil water movement.");
    add ("cm/d", p_c / u_d, name, syntax, alist, 
         1, 0, -1, 0, 0, 0, 0,
         "Soil water movement.");

    // Mass per length.
    add ("kg/m", 1e0, name, syntax, alist, -1, 1, 0, 0, 0, 0, 0,
         "Base mass per length.");

    // Mass per length flux.
    add ("kg/m/s", p_k * u_g, name, syntax, alist, -1, 1, -1, 0, 0, 0, 0,
         "Base mass per length flux.");
    add ("ng/mm/h", p_n * u_g / p_m / u_h, name, syntax, alist,
         -1, 1, -1, 0, 0, 0, 0,
         "Mass per length flux.");
    add ("g/cm/h", u_g / p_c / u_h, name, syntax, alist,
         -1, 1, -1, 0, 0, 0, 0,
         "Base mass per length flux.");
    
    // Mass per area.
    add ("kg/m^2", p_k * u_g, name, syntax, alist, -2, 1, 0, 0, 0, 0, 0,
         "Base mass per area.");
    add ("mg/m^2", p_m * u_g, name, syntax, alist, -2, 1, 0, 0, 0, 0, 0,
         "Crop scale pesticide per area.");
    add ("g/m^2", u_g, name, syntax, alist, -2, 1, 0, 0, 0, 0, 0,
         "Crop scale mass per area.");
    add ("g DM/m^2", u_g, name, syntax, alist, -2, 1, 0, 0, 0, 0, 0,
         "Crop scale dry matter per area.");
    add ("g/cm^2", u_g / p_c_2, name, syntax, alist, -2, 1, 0, 0, 0, 0, 0,
         "Soil scale mass per area.");
    add ("g N/cm^2", u_g / p_c_2, name, syntax, alist, -2, 1, 0, 0, 0, 0, 0,
         "Soil scale nitrogen per area.");
    add ("g C/cm^2", u_g / p_c_2, name, syntax, alist, -2, 1, 0, 0, 0, 0, 0,
         "Soil scale carbon per area.");
    add ("g N/m^2", u_g, name, syntax, alist, -2, 1, 0, 0, 0, 0, 0,
         "Crop scale nitrogen per area.");
    add ("g C/m^2", u_g, name, syntax, alist, -2, 1, 0, 0, 0, 0, 0,
         "Crop scale carbon per area.");
    add ("g/ha", u_g / u_ha, name, syntax, alist, -2, 1, 0, 0, 0, 0, 0,
         "Field scale pesticide mass per area.");
    add ("g w.w./m^2", u_g, name, syntax, alist, -2, 1, 0, 0, 0, 0, 0,
         "Wet weight per area.");
    add ("kg/ha", p_k * u_g / u_ha, name, syntax, alist, -2, 1, 0, 0, 0, 0, 0,
         "Field scale mass per area.");
    add ("kg N/ha", p_k * u_g / u_ha, name, syntax, alist, -2, 1, 0, 0, 0, 0, 0,
         "Field scale nitrogen per area.");
    add ("kg C/ha", p_k * u_g / u_ha, name, syntax, alist, -2, 1, 0, 0, 0, 0, 0,
         "Field scale carbon per area.");
    add ("kg w.w./ha", p_k * u_g / u_ha, name, syntax, alist,
         -2, 1, 0, 0, 0, 0, 0,
         "Wet weight per area.");
    add ("Mg/ha", p_M * u_g / u_ha, name, syntax, alist, 
         -2, 1, 0, 0, 0, 0, 0,
         "Ton per hectar.");
    add ("t/ha", p_M * u_g / u_ha, name, syntax, alist, 
         -2, 1, 0, 0, 0, 0, 0,
         "Ton per hectar.");
    add ("Mg DM/ha", p_M * u_g / u_ha, name, syntax, alist, 
         -2, 1, 0, 0, 0, 0, 0,
         "Ton dry matter per hectar.");
    add ("Mg w.w./ha", p_M * u_g / u_ha, name, syntax, alist, 
         -2, 1, 0, 0, 0, 0, 0,
         "Wet weight per area.");
    add ("T w.w./ha", p_M * u_g / u_ha, name, syntax, alist, 
         -2, 1, 0, 0, 0, 0, 0,
         "Wet weight per area.");

    // Mass per area flux.
    add ("kg/m^2/s", p_k * u_g, name, syntax, alist, -2, 1, -1, 0, 0, 0, 0,
         "Base mass per area flux.");
    add ("g/m^2/h", u_g / u_h, name, syntax, alist, 
         -2, 1, -1, 0, 0, 0, 0,
         "Gram per square meter per hour.");
    add ("g/cm^2/h", u_g / p_c_2 / u_h, name, syntax, alist, 
         -2, 1, -1, 0, 0, 0, 0,
         "Mass per area flux.");
    add ("g N/m^2/h", u_g /  u_h, name, syntax, alist, 
         -2, 1, -1, 0, 0, 0, 0,
         "Nitrogen per aquare meter per hour.");
    add ("g N/cm^2/h", u_g / p_c_2 / u_h, name, syntax, alist, 
         -2, 1, -1, 0, 0, 0, 0,
         "Nitrogen per aquare centimeter per hour.");
    add ("g/ha/h", u_g / u_ha / u_h, name, syntax, alist,
         -2, 1, -1, 0, 0, 0, 0,
         "Pesticide application.");
    add ("kg N/ha/h", p_k * u_g / u_ha / u_h, name, syntax, alist,
         -2, 1, -1, 0, 0, 0, 0,
         "Field scale application and removal of nitrogen.");
    add ("kg C/ha/h", p_k * u_g / u_ha / u_h, name, syntax, alist,
         -2, 1, -1, 0, 0, 0, 0,
         "Field scale application and removal of carbon.");
    add ("kg/ha/h", p_k * u_g / u_ha / u_h, name, syntax, alist,
         -2, 1, -1, 0, 0, 0, 0,
         "Harvest and fertilizing.");
    add ("kg N/ha/d", p_k * u_g / u_ha / u_d, name, syntax, alist,
         -2, 1, -1, 0, 0, 0, 0,
         "Field scale nitrogen.");
    add ("kg C/ha/d", p_k * u_g / u_ha / u_d, name, syntax, alist,
         -2, 1, -1, 0, 0, 0, 0,
         "Field scale carbon.");
    add ("kg/ha/y", p_k * u_g / u_ha / u_y, name, syntax, alist,
         -2, 1, -1, 0, 0, 0, 0,
         "Deposition.");
    add ("kgN/ha/year", p_k * u_g / u_ha / u_y, name, syntax, alist,
         -2, 1, -1, 0, 0, 0, 0,
         "Deposition.");
    add ("Mg DM/ha/h", p_M * u_g / u_ha / u_h, name, syntax, alist,
         -2, 1, -1, 0, 0, 0, 0,
         "Harvest and fertilizing.");

    // Mass per volume.
    add ("kg/m^3", p_k * u_g, name, syntax, alist, -3, 1, 0, 0, 0, 0, 0,
         "Base mass per volume.");
    add ("ng/l", p_n * u_g / u_l, name, syntax, alist,
         -3, 1, 0, 0, 0, 0, 0,
         "Low solute concentration.");
    add ("ng/cm^3", p_n * u_g / p_c_3, name, syntax, alist,
         -3, 1, 0, 0, 0, 0, 0,
         "Low solute concentration.");
    add ("ug/m^3", p_k * u_g, name, syntax, alist, -3, 1, 0, 0, 0, 0, 0,
         "Mass per volume.");
    add ("mg/l", p_m * u_g / u_l, name, syntax, alist, -3, 1, 0, 0, 0, 0, 0,
         "Concentration.");
    add ("mg N/l", p_m * u_g / u_l, name, syntax, alist, -3, 1, 0, 0, 0, 0, 0,
         "Nitrogen concentration.");
    add ("g/cm^2/mm", u_g / p_c_2 / p_m, name, syntax, alist,
         -3, 1, 0, 0, 0, 0, 0,
         "Irrigation and percolation concentration.");
    add ("g/cm^3", u_g / p_c_3, name, syntax, alist, -3, 1, 0, 0, 0, 0, 0,
         "Solute concentration.");
    add ("g C/cm^3", u_g / p_c_3, name, syntax, alist, -3, 1, 0, 0, 0, 0, 0,
         "Carbon concentration.");
    add ("g N/cm^3", u_g / p_c_3, name, syntax, alist, -3, 1, 0, 0, 0, 0, 0,
         "Nitrogen concentration.");
    add ("kg/ha/mm", p_k * u_g / u_ha / p_m, name, syntax, alist,
         -3, 1, 0, 0, 0, 0, 0,
         "Irrigation and percolation concentration.");
    add ("kg N/ha/mm", p_k * u_g / u_ha / p_m, name, syntax, alist,
         -3, 1, 0, 0, 0, 0, 0,
         "Irrigation and percolation concentration.");
    
    // Volume per mass.
    add ("m^3/kg", 1e0, name, syntax, alist, 3, -1, 0, 0, 0, 0, 0,
         "Base volume per mass.");
    add ("l/kg", u_l, name, syntax, alist, 3, -1, 0, 0, 0, 0, 0,
         "Volume per mass.");
    add ("L/kg", u_l, name, syntax, alist, 3, -1, 0, 0, 0, 0, 0,
         "Volume per mass.");
    add ("cm^3/g", p_c_3 / u_g, name, syntax, alist, 3, -1, 0, 0, 0, 0, 0,
         "Volume per mass.");
    add ("cm^3/ng", p_c_3 / (p_n * u_g), name, syntax, alist,
         3, -1, 0, 0, 0, 0, 0,
         "Volume per mass.");
    
    // Pressure.
    add ("Pa", 1e0, name, syntax, alist, -1, 1, -2, 0, 0, 0, 0,
         "Pascal.");
    add ("hPa", p_h, name, syntax, alist, -1, 1, -2, 0, 0, 0, 0,
         "Hectopascal.");
    add ("kPa", p_k, name, syntax, alist, -1, 1, -2, 0, 0, 0, 0,
         "Kilopascal.");
    add ("MPa", p_M, name, syntax, alist, -1, 1, -2, 0, 0, 0, 0,
         "Megapascal.");

    // Inverse Pressure.
    add ("Pa^-1", 1e0, name, syntax, alist, 1, -1, 2, 0, 0, 0, 0,
         "Base inverse pressure.");
    add ("MPa^-1", 1e0 / p_M, name, syntax, alist, 1, -1, 2, 0, 0, 0, 0,
         "Inverse pressure.");

    // Amount of substance per area.
    add ("mol/m^2", 1.0, name, syntax, alist, -2, 0, 0, 0, 0, 1, 0,
         "Mole per square meter.");
    add ("mmol/m^2", p_m, name, syntax, alist, -2, 0, 0, 0, 0, 1, 0,
         "Millimole per square meter.");

    // Amount of substance flux per area.
    add ("mol/m^2/s", 1.0, name, syntax, alist, -2, 0, -1, 0, 0, 1, 0,
         "Mole per square meter per second.");
    add ("mmol/m^2/s", p_m, name, syntax, alist, -2, 0, -1, 0, 0, 1, 0,
         "Millimole per square meter per second.");

    // Energy.
    add ("J", 1.0, name, syntax, alist, 2, 1, -2, 0, 0, 0, 0,
         "Joule.");

    // Energy flux.
    add ("W", 1.0, name, syntax, alist, 2, 1, -3, 0, 0, 0, 0,
         "Watt.");

    // Energy flux per area.
    add ("W/m^2", 1.0, name, syntax, alist, 0, 1, -3, 0, 0, 0, 0,
         "Watt per square meter.");
    add ("MJ/d/m^2", p_M / u_d, name, syntax, alist,
         0, 1, -3, 0, 0, 0, 0,
         "Megajoule per day per square meter.");
    add ("MJ/m^2/d", p_M / u_d, name, syntax, alist,
         0, 1, -3, 0, 0, 0, 0,
         "Megajoule per square meter per day.");
  }
} UnitSIFactor_syntax;

// Model 'pF'.

struct UnitpF : public Unit
{
  double to_base (double value) const
  { return pF2h (value) * 100.0 /* Pa/cm */; }
  double to_native (double value) const
  { return h2pF (value * 100.0) /* cm/pa */; }
  bool in_native (double value) const
  { return value >= 0.0; }
  bool in_base (double value) const
  { return value < 0.0; }

  UnitpF (Block& al)
    : Unit (al, Unit::pressure ())
  { }
};

static struct UnitpFSyntax
{
  static Model& make (Block& al)
  { return *new UnitpF (al); }

  UnitpFSyntax ()
  {
    // Add the 'pF' base model.
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "log10 (- cmH2O).");
    Librarian::add_type (Unit::component, "pF", alist, syntax, &make);
  }
} UnitpF_syntax;

// Model 'base'.

struct UnitBase : public Unit
{
  double to_base (double value) const
  { return value; }
  double to_native (double value) const
  { return value; }
  bool in_native (double value) const
  { return true; }
  bool in_base (double value) const
  { return true; }

  UnitBase (Block& al)
    : Unit (al, al.identifier ("type"))
  { }
};

static struct UnitBaseSyntax
{
  static Model& make (Block& al)
  { return *new UnitBase (al); }
  
  static void add (const std::string& name_string, 
                   const symbol super,
                   const Syntax& super_syntax, const AttributeList& super_alist,
                   const std::string& description)
  {
    const symbol name (name_string);

    // Add the 'base' 
    AttributeList& alist = *new AttributeList (super_alist);
    alist.add ("type", super);
    alist.add ("description", description);
    Librarian::add_type (Unit::component, name, alist, super_syntax, &make);
  }

  UnitBaseSyntax ()
  {
    static const symbol name ("base");

    // Add the 'base' base model.
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "A base unit.");
    Librarian::add_type (Unit::component, name, alist, syntax, &make);

    // Add geographical coordinates.
    add ("dgEast", name, syntax, alist, 
         "Degrees East of Greenwich.");
    add ("dgNorth", name, syntax, alist, 
         "Degrees North of Equator.");

    // Unknown unit.
    add (Syntax::Unknown (), name, syntax, alist, 
         "Nothing is known about the dimension of this unit.");
  }
} UnitBase_syntax;

// Model 'factor'.

struct UnitFactor : public Unit
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

  UnitFactor (Block& al)
    : Unit (al, al.identifier ("base")),
      factor (al.number ("factor"))
  { }
};

static struct UnitFactorSyntax
{
  static Model& make (Block& al)
  { return *new UnitFactor (al); }
  
  static void add (const std::string& name_string, const double factor,
                   const std::string& base,
                   const symbol super,
                   const Syntax& super_syntax, const AttributeList& super_alist,
                   const std::string& description)
  {
    const symbol name (name_string);

    AttributeList& alist = *new AttributeList (super_alist);
    alist.add ("type", super);
    alist.add ("base", base);
    alist.add ("factor", factor);
    alist.add ("description", description);
    Librarian::add_type (Unit::component, name, alist, super_syntax, &make);
  }

  UnitFactorSyntax ()
  {
    static const symbol name ("factor");

    // Add the 'SIfactor' factor model.
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "\
Connvert to base units by multiplying with a factor.");
    syntax.add ("base", Syntax::String, Syntax::Const, "\
Base unit to convert to and from.");
    // TODO: Should add check that 'base' is indeed a base unit.
    syntax.add ("factor", Syntax::None (), Check::non_zero (), Syntax::Const, "\
Fcator to multiply with to get base unit.");
    Librarian::add_type (Unit::component, name, alist, syntax, &make);

    // Add geographical coordinates.
    add ("dgWest", -1.0, "dgEast", name, syntax, alist,
         "Degrees West of Greenwich.");
    add ("dgSouth", -1.0, "dgNorth", name, syntax, alist,
         "Degrees North of Equator.");
  }
} UnitFactor_syntax;

// unit.C ends here.
