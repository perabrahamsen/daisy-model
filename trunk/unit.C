// unit.C -- Specify unit for scalar.
// 
// Copyright 2007 Per Abrahamsen and KVL.
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
#include "treelog.h"
#include "metalib.h"
#include "library.h"
#include "librarian.h"
#include "alist.h"
#include "syntax.h"
#include "block.h"
#include "units.h"
#include "assertion.h"
#include "mathlib.h"
#include <sstream>

// Component 'unit'.

const char *const Unit::component = "unit";

symbol
Unit::library_id () const
{
  static const symbol id (component);
  return id;
}

int
Unit::length () const
{ return 0; }

int
Unit::mass () const
{ return 0; }

int
Unit::time () const
{ return 0; }

int
Unit::electric_current () const
{ return 0; }

int
Unit::thermodynamic_temperature () const
{ return 0; }

int
Unit::amount_of_substance () const
{ return 0; }

int
Unit::luminous_intensity () const
{ return 0; }

bool 
Unit::allow_old (const Metalib& metalib)
{
  const AttributeList& alist = metalib.alist ();
  daisy_assert (alist.check ("allow_old_units"));
  const bool allow_old_units = alist.flag ("allow_old_units");
  return allow_old_units;
}

static struct special_convert_type
{
  const symbol from;
  const symbol to;
  const double factor;
} special_convert[] = {
  // We assume length is cm H2O, and convert to hPa.
  { symbol ("m") /* cm H2O */, symbol ("m^-1 kg s^-2") /* hPa */, 10000.0 },
  // We assume mass per volume is mg solute in l H2O, and convert to ppm.
  { symbol ("m^-3 kg") /* mg/l */, symbol ("") /* ppm */, 0.001 },
};

static const size_t special_convert_size 
/**/ = sizeof (special_convert) / sizeof (special_convert_type);

bool 
Unit::compatible (const Unit& from_unit, const Unit& to_unit)
{
  const symbol from = from_unit.base_name ();
  const symbol to = to_unit.base_name ();
  if (from == to)
    return true;
  
  // Special convertions.
  for (size_t i = 0; i < special_convert_size; i++)
    if (from == special_convert[i].from && to == special_convert[i].to)
      return true; 
    else if (from == special_convert[i].to && to == special_convert[i].from)
      return true;
  
  return false;
}

double
Unit::base_convert (const symbol from, const symbol to, const double value)
{
  if (from == to)
    return value;
  
  for (size_t i = 0; i < special_convert_size; i++)
    if (from == special_convert[i].from && to == special_convert[i].to)
      return value * special_convert[i].factor;
    else if (from == special_convert[i].to && to == special_convert[i].from)
      return value / special_convert[i].factor;
  
  throw "Cannot convert base [" + from + "] to [" + to + "]";
}

bool
Unit::can_convert (Metalib& metalib, const symbol from, const symbol to, 
                   Treelog& msg)
{
  if (from == to)
    return true;

  const Unit *const from_unit = metalib.unit (from, msg);
  const Unit *const to_unit = metalib.unit (to, msg);

  // Defined?
  if (!from_unit || !to_unit)
    {
      if (!allow_old (metalib))
        {
          if (!from_unit)
            msg.message ("Original dimension [" + from + "] not known.");
          if (!to_unit)
            msg.message ("Target dimension [" + to + "] not known.");
          return false;
        }
      msg.message ("Trying old conversion.");
      return Units::can_convert (from, to);
    }

  if (Unit::compatible (*from_unit, *to_unit))
    return true;

  // Not compatible.
  std::ostringstream tmp;
  tmp << "Cannot convert [" << from 
      << "] with base [" << from_unit->base_name () << "] to [" << to
      << "] with base [" << to_unit->base_name () << "]";
  msg.message (tmp.str ());
  if (!allow_old (metalib))
    return false;

  msg.message ("Trying old conversion.");
  return Units::can_convert (from, to);
}

bool 
Unit::can_convert (Metalib& metalib, const symbol from, const symbol to)
{ return Unit::can_convert (metalib, from, to, Treelog::null ()); }

bool 
Unit::can_convert (Metalib& metalib, const symbol from, const symbol to, 
                   const double value)
{ 
  Treelog& msg = Treelog::null ();

  const Unit *const from_unit = metalib.unit (from, msg);
  const Unit *const to_unit = metalib.unit (to, msg);

  // Defined?
  if (!from_unit || !to_unit)
    {
      if (!allow_old (metalib))
        return false;
      return Units::can_convert (from, to, value);
    }

  if (!Unit::compatible (*from_unit, *to_unit))
    return false;
  if (!from_unit->in_native  (value))
    return false;
  const double base = from_unit->to_base (value);
  // We don't have to worry about [cm] and [hPa] as all values are valid.
  return to_unit->in_base (base);
}

double 
Unit::convert (Metalib& metalib, const symbol from, const symbol to, 
               const double value)
{ 
  if (from == to)
    return value;

  Treelog& msg = Treelog::null ();

  const Unit *const from_unit = metalib.unit (from, msg);
  const Unit *const to_unit = metalib.unit (to, msg);

  // Defined?
  if (!from_unit || !to_unit)
    {
      if (allow_old (metalib))
        return Units::convert (from, to, value);
      if (!from_unit)
        throw "Cannot convert from unknown dimension [" + from 
          + "] to [" + to + "]";
      throw "Cannot convert from [" + from 
        + "] to unknown dimension [" + to + "]";
    }

  if (!Unit::compatible (*from_unit, *to_unit))
    throw std::string ("Cannot convert [") + from 
      + "] with base [" + from_unit->base_name () + "] to [" + to
      + "] with base [" + to_unit->base_name () + "]";

  const double from_base = from_unit->to_base (value);
  const double to_base = Unit::base_convert (from_unit->base_name (),
                                             to_unit->base_name (), 
                                             from_base);
  const double native = to_unit->to_native (to_base);
  
#if 0
  std::ostringstream tmp;
  tmp << "Converting " << value << " [" << from << "] to " << native 
      << " [" << to << "] through " << to_base << " [" 
      << to_unit->base_name () << "]";
  Assertion::message (tmp.str ());
#endif
  return native;
}

void
Unit::find_base_name ()
{

  typedef int (Unit::*unit_fun) () const;
  static const struct base_unit
  { 
    std::string name; 
    unit_fun base;
  } base_units[] = {
    { "m", &Unit::length },
    { "kg", &Unit::mass },
    { "s", &Unit::time },
    { "A", &Unit::electric_current },
    { "K", &Unit::thermodynamic_temperature },
    { "mol", &Unit::amount_of_substance },
    { "cd", &Unit::luminous_intensity }
  };
  const size_t base_unit_size = sizeof (base_units) / sizeof (base_unit);
  
  std::ostringstream tmp;
  for (size_t i = 0; i < base_unit_size; i++)
    {
      int dim = (this->*(base_units[i].base))();
      if (dim == 0)
        continue;
      if (tmp.str () != "")
        tmp << " ";;
      tmp << base_units[i].name;
      if (dim == 1)
        continue;
      tmp << "^" << dim;
    }
  base_name_ = symbol (tmp.str ());
}

Unit::Unit (Block& al)
  : name (al.identifier ("type"))
{ }

Unit::~Unit ()
{ }

static Librarian Unit_init (Unit::component, "\
The 'unit' allows you to define convertion to and from SI base units .");

// Base model 'SI'.

struct UnitSI : public Unit
{
  // Content.
  const int length_;
  const int mass_;
  const int time_;
  const int electric_current_;
  const int thermodynamic_temperature_;
  const int amount_of_substance_;
  const int luminous_intensity_;

  // Examine.
  int length () const
  { return length_; }
  int mass () const
  { return mass_; }
  int time () const
  { return time_; }
  int electric_current () const
  { return electric_current_; }
  int thermodynamic_temperature () const
  { return thermodynamic_temperature_; }
  int amount_of_substance () const
  { return amount_of_substance_; }
  int luminous_intensity () const
  { return luminous_intensity_; }

  // Create and destroy.
  static void load_syntax (Syntax& syntax, AttributeList& alist);
  UnitSI (Block& al)
    : Unit (al),
      length_ (al.integer ("length")),
      mass_ (al.integer ("mass")),
      time_ (al.integer ("time")),
      electric_current_ (al.integer ("electric_current")),
      thermodynamic_temperature_ (al.integer ("thermodynamic_temperature")),
      amount_of_substance_ (al.integer ("amount_of_substance")),
      luminous_intensity_ (al.integer ("luminous_intensity"))
  { find_base_name (); }
  ~UnitSI ()
  { }
};

void
UnitSI::load_syntax (Syntax& syntax, AttributeList& alist)
{
  syntax.add ("length", Syntax::Integer, Syntax::Const, "\
Dimension.");
  alist.add ("length", 0);
  syntax.add ("mass", Syntax::Integer, Syntax::Const, "\
Dimension.");
  alist.add ("mass", 0);
  syntax.add ("time", Syntax::Integer, Syntax::Const, "\
Dimension.");
  alist.add ("time", 0);
  syntax.add ("electric_current", Syntax::Integer, Syntax::Const, "\
Dimension.");
  alist.add ("electric_current", 0);
  syntax.add ("thermodynamic_temperature", Syntax::Integer, Syntax::Const, "\
Dimension.");
  alist.add ("thermodynamic_temperature", 0);
  syntax.add ("amount_of_substance", Syntax::Integer, Syntax::Const, "\
Dimension.");
  alist.add ("amount_of_substance", 0);
  syntax.add ("luminous_intensity", Syntax::Integer, Syntax::Const, "\
Dimension.");
  alist.add ("luminous_intensity", 0);
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
    
    // Add the "SIfactor" base model..
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "\
Connvert to SI base units by multiplying with a factor.");
    UnitSI::load_syntax (syntax, alist);
    syntax.add ("factor", Syntax::None (), Check::non_zero (), Syntax::Const, "\
Fcator to multiply with to get base unit.");
    Librarian::add_type (Unit::component, name, alist, syntax, &make);

    // Unitless.
    add (Syntax::None (), 1.0, name, syntax, alist, 0, 0, 0, 0, 0, 0, 0,
         "Unitless.");
    add (Syntax::Fraction (), 1.0, name, syntax, alist, 0, 0, 0, 0, 0, 0, 0,
         "Unitless.");
    add ("", 1.0, name, syntax, alist, 0, 0, 0, 0, 0, 0, 0,
         "Unitless.");
    add ("%", 0.01, name, syntax, alist, 0, 0, 0, 0, 0, 0, 0,
         "Percent.");
    add ("ppm", 1.0 / 1000000.0, name, syntax, alist, 0, 0, 0, 0, 0, 0, 0,
         "Part per million.");
    
    // Length.
    add ("m", 1.0, name, syntax, alist, 1, 0, 0, 0, 0, 0, 0,
         "Meter.");
    add ("cm", 0.01, name, syntax, alist, 1, 0, 0, 0, 0, 0, 0,
         "Centimeter.");

    add ("m^2", 1.0, name, syntax, alist, 2, 0, 0, 0, 0, 0, 0,
         "Square meter.");
    add ("cm^2", 0.01 * 0.01, name, syntax, alist, 2, 0, 0, 0, 0, 0, 0,
         "Square centimeter.");
    add ("ha", 100.0 * 100.0, name, syntax, alist, 2, 0, 0, 0, 0, 0, 0,
         "Hectare.");

    add ("m^3", 1.0, name, syntax, alist, 3, 0, 0, 0, 0, 0, 0,
         "Cube meter.");
    add ("cm^3", 0.01 * 0.01 * 0.01, name, syntax, alist, 3, 0, 0, 0, 0, 0, 0,
         "Cube centimeter.");

    // Mass.
    add ("kg", 1.0, name, syntax, alist, 0, 1, 0, 0, 0, 0, 0,
         "Kilogram.");
    add ("g", 0.001, name, syntax, alist, 0, 1, 0, 0, 0, 0, 0,
         "Gram.");

    // Time.
    add ("s", 1.0, name, syntax, alist, 0, 0, 1, 0, 0, 0, 0,
         "Second.");
    add ("h", 3600.0, name, syntax, alist, 0, 0, 1, 0, 0, 0, 0,
         "Hour.");
    add ("d", 24 * 3600.0, name, syntax, alist, 0, 0, 1, 0, 0, 0, 0,
         "Day.");

    add ("s^-1", 1.0, name, syntax, alist, 0, 0, -1, 0, 0, 0, 0,
         "Second.");
    add ("h^-1", 1.0 / 3600.0, name, syntax, alist, 0, 0, -1, 0, 0, 0, 0,
         "Hour.");
    add ("d^-1", 1.0 / (24 * 3600.0), name, syntax, alist, 0, 0, -1, 0, 0, 0, 0,
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
    add ("mm/h", 0.001 / 3600.0, name, syntax, alist, 1, 0, -1, 0, 0, 0, 0,
         "Percolation intensity.");
    add ("mm/d", 0.001 / (24 * 3600.0), name, syntax, alist, 
         1, 0, -1, 0, 0, 0, 0,
         "Percolation intensity.");
    add ("cm/h", 0.01 / 3600.0, name, syntax, alist, 1, 0, -1, 0, 0, 0, 0,
         "Soil water movement.");
    add ("cm/d", 0.01 / (24 * 3600.0), name, syntax, alist, 
         1, 0, -1, 0, 0, 0, 0,
         "Soil water movement.");
    
    // Mass per area.
    add ("kg/m^2", 1e0, name, syntax, alist, -2, 1, 0, 0, 0, 0, 0,
         "Base mass per area.");
    add ("g w.w./m^2", 1e-3 / 1.0, name, syntax, alist, -2, 1, 0, 0, 0, 0, 0,
         "Wet weight per area.");
    add ("kg w.w./ha", 1e0 / 1e4, name, syntax, alist, -2, 1, 0, 0, 0, 0, 0,
         "Wet weight per area.");
    add ("Mg w.w./ha", 1e3 / 1e4, name, syntax, alist, 
         -2, 1, 0, 0, 0, 0, 0,
         "Wet weight per area.");
    add ("T w.w./ha", 1e3 / 1e4, name, syntax, alist, 
         -2, 1, 0, 0, 0, 0, 0,
         "Wet weight per area.");

    // Mass per volume.
    add ("kg/m^3", 1e0, name, syntax, alist, -3, 1, 0, 0, 0, 0, 0,
         "Base mass per volume.");
    add ("mg N/l", 1e-6 / 1e-3, name, syntax, alist, -3, 1, 0, 0, 0, 0, 0,
         "Nitrogen concentration.");
    add ("g/cm^2/mm", 1e-3 / 1e-7, name, syntax, alist, -3, 1, 0, 0, 0, 0, 0,
         "Irrigation and percolation concentration.");

    // Volume per mass.
    add ("m^3/kg", 1e0, name, syntax, alist, 3, -1, 0, 0, 0, 0, 0,
         "Base volume per mass.");
    add ("l/kg", 1e-3 / 1e0, name, syntax, alist, 3, -1, 0, 0, 0, 0, 0,
         "Volume per mass.");
    add ("L/kg", 1e-3 / 1e0, name, syntax, alist, 3, -1, 0, 0, 0, 0, 0,
         "Volume per mass.");
    add ("cm^3/g", 1e-6 / 1e-3, name, syntax, alist, 3, -1, 0, 0, 0, 0, 0,
         "Volume per mass.");

    // Pressure.
    add ("Pa", 1.0, name, syntax, alist, -1, 1, -2, 0, 0, 0, 0,
         "Pascal.");
    add ("hPa", 100.0, name, syntax, alist, -1, 1, -2, 0, 0, 0, 0,
         "Hectopascal.");
    add ("kPa", 1000.0, name, syntax, alist, -1, 1, -2, 0, 0, 0, 0,
         "Kilopascal.");
    add ("MPa", 1000000.0, name, syntax, alist, -1, 1, -2, 0, 0, 0, 0,
         "Megapascal.");
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

  int length () const
  { return -1; }
  int mass () const
  { return 1; }
  int time () const
  { return -2; }

  UnitpF (Block& al)
    : Unit (al)
  { find_base_name (); }
};

static struct UnitpFSyntax
{
  static Model& make (Block& al)
  { return *new UnitpF (al); }

  UnitpFSyntax ()
  {
    // Add the "SIfactor" base model..
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "log10 (- cmH2O).");
    Librarian::add_type (Unit::component, "pF", alist, syntax, &make);
  }
} UnitpF_syntax;

// unit.C ends here.
