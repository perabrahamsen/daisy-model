// units.C -- Unit conversions.
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

#include "units.h"
#include "unit_model.h"
#include "convert.h"
#include "oldunits.h"
#include "memutils.h"
#include "treelog.h"
#include "assertion.h"
#include "librarian.h"
#include "metalib.h"
#include "library.h"
#include "frame_model.h"
#include <sstream>

// The 'Units' Interface.

symbol
Units::error_symbol ()
{
  static const symbol unit ("<error>");
  return unit;
}

symbol
Units::h ()
{
  static const symbol unit ("h");
  return unit;
}

symbol
Units::mm ()
{
  static const symbol unit ("mm");
  return unit;
}

symbol
Units::per_mm ()
{
  static const symbol unit ("mm^-1");
  return unit;
}

symbol
Units::mm_per_h ()
{
  static const symbol unit ("mm/h");
  return unit;
}

symbol
Units::cm ()
{
  static const symbol unit ("cm");
  return unit;
}

symbol
Units::cm_per_h ()
{
  static const symbol unit ("cm/h");
  return unit;
}

symbol
Units::cm2 ()
{
  static const symbol unit ("cm^2");
  return unit;
}

symbol
Units::cm3 ()
{
  static const symbol unit ("cm^3");
  return unit;
}

symbol
Units::per_h ()
{
  static const symbol unit ("h^-1");
  return unit;
}

symbol
Units::ppm ()
{
  static const symbol unit ("ppm");
  return unit;
}

symbol
Units::dry_soil_fraction ()
{
  static const symbol unit ("dry soil fraction");
  return unit;
}

symbol
Units::dgC ()
{
  static const symbol unit ("dg C");
  return unit;
}

Units::special_convert_type
Units::special_convert[] = {
  // We assume length is cm H2O, and convert to hPa.
  { "m" /* cm H2O */, Unit::pressure () /* hPa */, 10000.0 },
#if 1 // Required by weather wet deposition.
  // We assume mass per volume is mg solute in l H2O, and convert to ppm.
  { Unit::mass_per_volume () /* mg/l */, "" /* ppm */, 0.001 },
#endif
  // We assume amount of substance is mol photons in PAR and convert to Watt.
  { Unit::amount_of_substance_per_area_per_time () /* mol/m^2/s */,
    Unit::energy_per_area_per_time () /* W/m^2 */,
    1.0 / 0.0000046 },
  // We assume mass per area per time is water, and convert to length per time.
  // [ one kg H20 is 1 l H2O / m^2, or one mm H2O ]
  { Unit::mass_per_area_per_time () /* kg/m^2/s */,
    Unit::length_per_time () /* m/s */,
    0.001 }
};

const size_t 
Units::special_convert_size 
/**/ = sizeof (Units::special_convert) / sizeof (Units::special_convert_type);

double
Units::base_convert (const symbol from, const symbol to,
                     const double value)
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
Units::compatible (const Unit& from_unit, const Unit& to_unit)
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
Units::unit_convert (const Unit& from, const Unit& to,
                     const double value)
{
  if (!compatible (from, to))
    throw std::string ("Cannot convert [") + from.native_name () 
      + "] with base [" + from.base_name () + "] to [" + to.native_name ()
      + "] with base [" + to.base_name () + "]";

  const double from_base = from.to_base (value);
  const double to_base = base_convert (from.base_name (),
                                       to.base_name (), 
                                       from_base);
  const double native = to.to_native (to_base);
  
#if 0
  std::ostringstream tmp;
  tmp << "Converting " << value << " [" << from.native_name () << "] to " << native 
      << " [" << to.native_name () << "] through " << from_base << " [" 
      << from.base_name () << "]";
  if (from.base_name () == to.base_name ())
    daisy_approximate (from_base, to_base);
  else
    tmp << " and " << to_base << " [" << to.base_name () << "]";
  Assertion::message (tmp.str ());
#endif

  return native;
}

double 
Units::multiply (const Unit& a, const Unit& b, double value, const Unit& result)
{
  const symbol ab = multiply (a.native_name (), b.native_name ());
  return Oldunits::convert (ab, result.native_name (), value);
}

symbol
Units::multiply (const symbol a, const symbol b)
{
  return Oldunits::multiply (a, b);
}

const Unit&
Units::unknown () const
{ 
  const unit_map::const_iterator i = units.find (Attribute::Unknown ()); 
  daisy_assert (i != units.end () && (*i).second);
  return *(*i).second;
}

bool
Units::is_known (const Unit& unit) const
{ return unit.native_name () != Attribute::Unknown (); }

const Unit&
Units::error () const
{ 
  const unit_map::const_iterator i = units.find (Units::error_symbol ()); 
  daisy_assert (i != units.end () && (*i).second);
  return *(*i).second;
}

bool
Units::is_error (const Unit& unit) const
{ return unit.native_name () == Units::error_symbol (); }

bool 
Units::allow_old () const
{ return allow_old_; }

bool
Units::has_unit (symbol name) const
{ return units.find (name) != units.end (); }

const Unit&
Units::get_unit (symbol name) const
{ 
  const unit_map::const_iterator i = units.find (name); 
  if (i != units.end () && (*i).second)
    return *(*i).second;
  
  return error ();
}

bool
Units::can_convert (const symbol from, const symbol to, Treelog& msg) const
{
  if (from == to)
    return true;

  // Defined?
  if (!has_unit(from) || !has_unit (to))
    {
      if (!allow_old ())
        {
          if (has_unit (from))
            msg.message ("Original dimension [" + from + "] known.");
          else
            msg.message ("Original dimension [" + from + "] not known.");
          if (has_unit (to))
            msg.message ("Target dimension [" + to + "] known.");
          else
            msg.message ("Target dimension [" + to + "] not known.");
          return false;
        }
      msg.message (std::string ("Trying old conversion of ") 
                   + (has_unit (from) ? "" : "unknown ") + "[" + from + "] to " 
                   + (has_unit (to) ? "" : "unknown ") + "[" + to + "]." );
      return Oldunits::can_convert (from, to);
    }

  const Unit& from_unit = get_unit (from);
  const Unit& to_unit = get_unit (to);

  if (compatible (from_unit, to_unit))
    return true;

  // Not compatible.
  std::ostringstream tmp;
  tmp << "Cannot convert [" << from 
      << "] with base [" << from_unit.base_name () << "] to [" << to
      << "] with base [" << to_unit.base_name () << "]";
  msg.message (tmp.str ());
  if (!allow_old ())
    return false;

  msg.message ("Trying old conversion.");
  return Oldunits::can_convert (from, to);
}

bool 
Units::can_convert (const symbol from, const symbol to) const
{
  if (from == to)
    return true;

  // Defined?
  if (!has_unit(from) || !has_unit (to))
    {
      if (!allow_old ())
        return false;
      else
        return Oldunits::can_convert (from, to);
    }
  
  const Unit& from_unit = get_unit (from);
  const Unit& to_unit = get_unit (to);

  if (compatible (from_unit, to_unit))
    return true;

  if (!allow_old ())
    return false;

  return Oldunits::can_convert (from, to);
}

bool 
Units::can_convert (const symbol from, const symbol to,
                    const double value) const
{ 
  if (from == to)
    return true;

  // Defined?
  if (!has_unit(from) || !has_unit (to))
    {
      if (!allow_old ())
        return false;
      else
        return Oldunits::can_convert (from, to, value);
    }
  
  const Unit& from_unit = get_unit (from);
  const Unit& to_unit = get_unit (to);

  if (!compatible (from_unit, to_unit))
    return false;
  if (!from_unit.in_native  (value))
    return false;
  const double base = from_unit.to_base (value);
  // We don't have to worry about [cm] and [hPa] as all values are valid.
  return to_unit.in_base (base);
}

double 
Units::convert (const symbol from, const symbol to, const double value) const
{ 
  if (from == to)
    return value;

  // Defined?
  if (!has_unit(from) || !has_unit (to))
    {
      if (allow_old ())
        return Oldunits::convert (from, to, value);
      if (!has_unit (from))
        throw "Cannot convert from unknown dimension [" + from 
          + "] to [" + to + "]";
      throw "Cannot convert from [" + from 
        + "] to unknown dimension [" + to + "]";
    }
  
  return Units::unit_convert (get_unit (from), get_unit (to), value);
}

const Convert*
Units::create_convertion (const Unit& from, const Unit& to)
{
  // Maybe 'from' knows a smart way.
  const Convert* from_convert = from.create_convertion (to);
  if (from_convert)
    return from_convert;

  // It doesn't seem that way.  Try a generic solution.
  struct ConvertGeneric : public Convert
  {
    const Unit& from;
    const Unit& to;
    double operator()(const double value) const
    { return unit_convert (from, to, value); }
    bool valid (const double value) const
    {
      if (!from.in_native (value))
        return false;
      const double from_base = from.to_base (value);
      const double to_base = base_convert (from.base_name (), to.base_name (),
                                           from_base);
      return to.in_base (to_base);
    }
    ConvertGeneric (const Unit& f, const Unit& t)
      : from (f),
        to (t)
    { }
    ~ConvertGeneric ()
    { }
  };
  
  return new ConvertGeneric (from, to);
}

const Convert& 
Units::get_convertion (const symbol from, const symbol to) const
{
  if (from == to)
    {
      static struct ConvertIdentity : public Convert
      {
        double operator()(const double value) const
        { return value; }
        bool valid (const double) const
        { return true; }
      } identity;
      return identity;
    }
  const symbol key (from.name () + " -> " + to.name ());

  // Already known.
  convert_map::const_iterator i
    = this->conversions.find (key); 
  if (i != this->conversions.end ())
    return *(*i).second;

  // Defined?
  if (!has_unit (from) || !has_unit (to))
    {
      if (allow_old ())
        {
          struct ConvertOld : Convert
          {
            const Oldunits::Convert& old;
            double operator()(const double value) const
            { return old (value); }
            bool valid (const double value) const
            { return old.valid (value); }
            ConvertOld (const Oldunits::Convert& o)
              : old (o)
            { }
          };
          const Oldunits::Convert& old
            = Oldunits::get_convertion (from.name (), to.name ());
          
          const Convert* convert = new ConvertOld (old);
          daisy_assert (convert);
          conversions[key] = convert;
          return *convert;
        }
      if (!has_unit (from))
        throw "Cannot get conversion from unknown dimension [" + from 
          + "] to [" + to + "]";
      if (!has_unit (to))
        throw "Cannot get conversion from [" + from 
          + "] to unknown dimension [" + to + "]";
    }
  
  const Unit& from_unit = get_unit (from);
  const Unit& to_unit = get_unit (to);
  if (!compatible (from_unit, to_unit))
    throw "Cannot get conversion from [" + from 
      + "] to dimension [" + to + "]";

  const Convert* convert = create_convertion (from_unit, to_unit);
  daisy_assert (convert);
  conversions[key] = convert;
  return *convert;
}

void
Units::add_unit (Metalib& metalib, const symbol name)
{
  // Do we already have it?
  unit_map::iterator i = this->units.find (name); 
  if (i != this->units.end ())
    {
      // Delete old copy.
      delete (*i).second;
      (*i).second = NULL;
    }
  
  // Is it defined?
  const Library& library = metalib.library (MUnit::component);
  if (!library.complete (metalib, name))
    return;

  // Build it.
  this->units[name] = Librarian::build_stock<MUnit> (metalib, Treelog::null (),
                                                     name, "unit");
}

void
Units::load_syntax (Frame& frame)
{
  frame.declare_boolean ("allow_old_units", Attribute::Const, "\
OBSOLETE: Set this to true to enable the old system of build-in\n\
unit conversation.");
  frame.set ("allow_old_units", true);
}

Units::Units (Metalib& metalib)
#if 1
  : allow_old_ (metalib.flag ("allow_old_units"))
#else
  : allow_old_ (true)
#endif
{ 
  const Library& library = metalib.library (MUnit::component);
  std::vector<symbol> entries;
  library.entries (entries);
  for (size_t i = 0; i < entries.size (); i++)
    add_unit (metalib, entries[i]);
}

Units::~Units ()
{ }

// unit.C ends here.
