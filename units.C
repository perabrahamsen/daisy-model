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
#include "oldunits.h"
#include "unit.h"

// The 'Convert' Interface.

Convert::Convert ()
{ }

Convert::~Convert ()
{ }

// The 'Unitc' Interface.

symbol
Unitc::h ()
{
  static const symbol unit ("h");
  return unit;
}

symbol
Unitc::mm ()
{
  static const symbol unit ("mm");
  return unit;
}

symbol
Unitc::per_mm ()
{
  static const symbol unit ("mm^-1");
  return unit;
}

symbol
Unitc::mm_per_h ()
{
  static const symbol unit ("mm/h");
  return unit;
}

symbol
Unitc::cm ()
{
  static const symbol unit ("cm");
  return unit;
}

symbol
Unitc::cm_per_h ()
{
  static const symbol unit ("cm/h");
  return unit;
}

symbol
Unitc::cm2 ()
{
  static const symbol unit ("cm^2");
  return unit;
}

symbol
Unitc::cm3 ()
{
  static const symbol unit ("cm^3");
  return unit;
}

symbol
Unitc::per_h ()
{
  static const symbol unit ("h^-1");
  return unit;
}

symbol
Unitc::ppm ()
{
  static const symbol unit ("ppm");
  return unit;
}

Unitc::special_convert_type
Unitc::special_convert[] = {
  // We assume length is cm H2O, and convert to hPa.
  { symbol ("m") /* cm H2O */, Unit::pressure () /* hPa */, 10000.0 },
  // We assume mass per volume is mg solute in l H2O, and convert to ppm.
  { Unit::mass_per_volume () /* mg/l */, symbol ("") /* ppm */, 0.001 },
  // We assume amount of substance is mol photons in PAR and convert to Watt.
  { Unit::amount_of_substance_per_area_per_time () /* mol/m^2/s */,
    Unit::energy_per_area_per_time () /* W/m^2 */,
    1.0 / 0.0000046 }
};

const size_t 
Unitc::special_convert_size 
/**/ = sizeof (Unitc::special_convert) / sizeof (Unitc::special_convert_type);

double
Unitc::base_convert (const symbol from, const symbol to,
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
Unitc::compatible (const Unit& from_unit, const Unit& to_unit)
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
Unitc::unit_convert (const Unit& from, const Unit& to,
                     const double value)
{
  if (!compatible (from, to))
    throw std::string ("Cannot convert [") + from.name 
      + "] with base [" + from.base_name () + "] to [" + to.name
      + "] with base [" + to.base_name () + "]";

  const double from_base = from.to_base (value);
  const double to_base = base_convert (from.base_name (),
                                       to.base_name (), 
                                       from_base);
  const double native = to.to_native (to_base);
  
#if 0
  std::ostringstream tmp;
  tmp << "Converting " << value << " [" << from.name << "] to " << native 
      << " [" << to.name << "] through " << from_base << " [" 
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
Unitc::multiply (const Unit& a, const Unit& b, double value, const Unit& result)
{
  const symbol ab = multiply (a.name, b.name);
  return Oldunits::convert (ab, result.name, value);
}

symbol
Unitc::multiply (const symbol a, const symbol b)
{
  return Oldunits::multiply (a, b);
}

const Convert*
Unitc::create_convertion (const Unit& from, const Unit& to)
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

Unitc::Unitc ()
{ }

Unitc::~Unitc ()
{ }

// unit.C ends here.
