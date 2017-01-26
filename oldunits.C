// oldunits.C -- Old style conversion between different dimensions.
// 
// Copyright 2002 Per Abrahamsen and KVL.
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

#include "oldunits.h"
#include "mathlib.h"
#include "memutils.h"
#include "attribute.h"
#include <map>

struct Oldunits::Content
{
  typedef std::map<symbol, boost::shared_ptr<Convert>/**/> to_type;
  typedef std::map<symbol, to_type> table_type;
  table_type table;

  static bool time_match (const symbol from, const symbol to);
  static symbol crop_time (const symbol);
  
  double convert (const symbol from, const symbol to,
		  double value) const;
  bool can_convert (const symbol from, const symbol to) const;
  bool can_convert (const symbol from, const symbol to,
		    double value) const;
  const Convert& get_convertion (const symbol from,
				 const symbol to) const;

  Content ();
  ~Content ();
};  

bool				// True iff FROM and TO have same time unit.
Oldunits::Content::time_match (const symbol from_s, const symbol to_s)
{
  const std::string& from = from_s.name ();
  const std::string& to = to_s.name ();
  const size_t from_size = from.size ();
  const size_t to_size = to.size ();

  for (int i = 1; true; i++)
    {
      if (i > from_size || i > to_size)
	return false;

      const char from_c = from[from_size - i];
      const char to_c = to[to_size - i];
      
      if (from_c != to_c)
	return false;
      
      if (from_c == '/')
	return true;
    }
}

symbol                          // Return DIM without time.
Oldunits::Content::crop_time (const symbol dim_s)
{
  const std::string& dim = dim_s.name ();
  daisy_assert (dim.size () > 0);
  size_t end;
  for (end = dim.size () - 1; dim[end] != '/'; end--)
    daisy_assert (end > 0);
  std::string result;
  for (int i = 0; i < end; i++)
    result += dim[i];
  return result;
}

double 
Oldunits::Content::convert (const symbol from, const symbol to, 
                            double value) const
{ 
  if (from == to)
    return value;

  const Oldunits::Convert& conv = get_convertion (from, to);

  if (!conv.valid (value))
    throw std::string ("invalid value");

  return conv (value);
}

bool 
Oldunits::Content::can_convert (const symbol from, const symbol to) const
{ 
  if (from == to)
    return true;

  try 
    {
      get_convertion (from, to);
    }
  catch (...)
    {
      return false;
    }
  return true;
}

bool 
Oldunits::Content::can_convert (const symbol from, const symbol to, 
                                const double value) const
{ 
  if (from == to)
    return true;

  try 
    {
      const Oldunits::Convert& conv = get_convertion (from, to);
      if (!conv.valid (value))
	return false;
    }
  catch (...)
    {
      return false;
    }
  return true;
}

static const class ConvertIdentity : public Oldunits::Convert
{
  double operator() (double value) const
  { return value; }
public:
  ConvertIdentity ()
  { }
} convert_identity;

const Oldunits::Convert&
Oldunits::Content::get_convertion (const symbol from, const symbol to) const
{ 
  if (from == to)
    return convert_identity;

  table_type::const_iterator i = table.find (from);
  if (i == table.end ())
    {
      // We check if we can convert without time.
      if (time_match (from, to))
	{
	  const symbol from_c = crop_time (from);
	  table_type::const_iterator i_c = table.find (from_c);
	  if (i_c != table.end ())
	    {
	      const symbol to_c = crop_time (to);
	      to_type::const_iterator j_c = (*i_c).second.find (to_c);
	      if (j_c != (*i_c).second.end ())
		return *(*j_c).second;
	    }
	}
      throw std::string ("Convert [") + from 
	+ "] unknown dimension, expected [" + to + "]";
    }
  to_type::const_iterator j = (*i).second.find (to);
  if (j == (*i).second.end ())
    throw std::string ("Cannot convert [") + from + "] to [" + to + "]";

  return *(*j).second;
}

Oldunits::Content::Content ()
{ }
  
Oldunits::Content::~Content ()
{ }

Oldunits::Content* Oldunits::content = NULL;
int Oldunits::count = 0;

symbol
Oldunits::h ()
{
  static const symbol unit ("h");
  return unit;
}

symbol
Oldunits::mm ()
{
  static const symbol unit ("mm");
  return unit;
}

symbol
Oldunits::per_mm ()
{
  static const symbol unit ("mm^-1");
  return unit;
}

symbol
Oldunits::mm_per_h ()
{
  static const symbol unit ("mm/h");
  return unit;
}

symbol
Oldunits::cm ()
{
  static const symbol unit ("cm");
  return unit;
}

symbol
Oldunits::cm_per_h ()
{
  static const symbol unit ("cm/h");
  return unit;
}

symbol
Oldunits::cm2 ()
{
  static const symbol unit ("cm^2");
  return unit;
}

symbol
Oldunits::cm3 ()
{
  static const symbol unit ("cm^3");
  return unit;
}

symbol
Oldunits::per_h ()
{
  static const symbol unit ("h^-1");
  return unit;
}

symbol
Oldunits::ppm ()
{
  static const symbol unit ("ppm");
  return unit;
}

bool
Oldunits::Convert::valid (double) const
{ return true; }

Oldunits::Convert::Convert ()
{ }

Oldunits::Convert::~Convert ()
{ }

struct ConvertLinear : public Oldunits::Convert
{
  const double factor;
  const double offset;
    
  double operator() (double value) const
  { return value * factor + offset; }

  ConvertLinear (double f, double o)
    : factor (f),
      offset (o)
  { }
};

void 
Oldunits::add (const symbol from, const symbol to,
               double factor, double offset)
{ 
  daisy_assert (content);
  if (!(content->table[from].find (to) == content->table[from].end ()))
    daisy_warning ("convert from [" + from + "] to [" + to + "] duplicate\n");
  else
    content->table[from][to] 
      = boost::shared_ptr<Convert> (new ConvertLinear (factor, offset));
  // Reverse conversion.
  daisy_assert (std::isnormal (factor));
  if (!(content->table[to].find (from) == content->table[to].end ()))
    daisy_warning ("convert from [" + from + "] to [" + to + "] reverse\n");
  else
    content->table[to][from] 
      = boost::shared_ptr<Convert> (new ConvertLinear (1.0 / factor, 
                                                       -offset / factor));
}

void 
Oldunits::add (const symbol from, const symbol to,
               boost::shared_ptr<Convert> convert)
{
  daisy_assert (content);
  daisy_assert (content->table[from].find (to) == content->table[from].end ());
  content->table[from][to] = convert;
}

double 
Oldunits::convert (const symbol from, const symbol to, double value)
{ 
  daisy_assert (content);
  return content->convert (from, to, value);
}

bool
Oldunits::can_convert (const symbol from, const symbol to)
{ 
  daisy_assert (content);
  return content->can_convert (from, to);
}

bool
Oldunits::can_convert (const symbol from, const symbol to, 
		    double value)
{ 
  daisy_assert (content);
  return content->can_convert (from, to, value);
}

const Oldunits::Convert&
Oldunits::get_convertion (const symbol from, const symbol to)
{
  daisy_assert (content);
  return content->get_convertion (from, to);
}

symbol
Oldunits::multiply (const symbol one, const symbol two)
{
  static const symbol empty = "";
  if (one == Attribute::None ()
      || one == Attribute::Fraction ()
      || one == empty)
    return two;
  if (two == Attribute::None ()
      || two == Attribute::Fraction ()
      || two == empty)
    return one;
  if (one == Attribute::Unknown () || two == Attribute::Unknown ())
    return Attribute::Unknown ();

  static const struct MulTable : public std::map<symbol, std::map<symbol, symbol> /**/>
  {
    MulTable ()
    {
      static const struct multiply_table
      { 
        const symbol one;
        const symbol two;
        const symbol result;
      } table[] = { 
        // select_interval.C
        { "h^-1", "cm", "cm/h" },
        { "cm^3/cm^3", "cm", "cm" },
        { "cm^3/cm^3/h", "cm", "cm/h" },
        { "g/cm^3", "cm", "g/cm^2" },
        { "g C/cm^3", "cm", "g C/cm^2" },
        { "g N/cm^3", "cm", "g N/cm^2" },
        { "g/cm^3/h", "cm", "g/cm^2/h" },
        { "g C/cm^3/h", "cm", "g C/cm^2/h" },
        { "g N/cm^3/h", "cm", "g N/cm^2/h" }, 
        { "g CO_2-C/cm^3/h", "cm", "g CO_2-C/cm^2/h"},
        // select_flow.C
        { "cm/h", "cm^2", "cm^3/h" },
        { "g/cm^2/h", "cm^2", "g/h" },   
        // select_volume.C
        { "h^-1", "cm^3", "cm^3/h" },
        { "cm^3/cm^3", "cm^3", "cm^3" },
        { "cm^3/cm^3/h", "cm^3", "cm^3/h" },
        { "g/cm^3", "cm^3", "g" },
        { "g C/cm^3", "cm^3", "g C" },
        { "g N/cm^3", "cm^3", "g N" },
        { "g/cm^3/h", "cm^3", "g/h" },
        { "g C/cm^3/h", "cm^3", "g C/h" },
        { "g N/cm^3/h", "cm^3", "g N/h" }, 
        { "g CO_2-C/cm^3/h", "cm^3", "g CO_2-C/h"},
        // im.C
        { "g/cm^2/mm", "mm/h", "g/cm^2/h"},
        { "g/cm^2/mm", "mm", "g/cm^2"},
        { "g/cm^2/h", "h", "g/cm^2"},
        { "g", "cm^-2", "g/cm^2"},
        { "mg/m^2", "mm^-1", "ppm"},
        { "g/cm^2", "h^-1", "g/cm^2/h"},
        // bioclimate.C
        { "W/m^2", "h", "Wh/m^2"},
      };
      for (unsigned int i = 0; i < sizeof (table) / sizeof (multiply_table); i++)
        {
          (*this)[table[i].one][table[i].two] = table[i].result;
          (*this)[table[i].two][table[i].one] = table[i].result;
        }
    }
  } mul_table;

  const MulTable::const_iterator i = mul_table.find (one);
  if (i != mul_table.end ())
    {
      const std::map<symbol, symbol> res_table = (*i).second;
      const std::map<symbol, symbol>::const_iterator j = res_table.find (two);
      if (j != res_table.end ())
        return (*j).second;
    }
  
  const std::string inv = "^-1";
  if (one.name () == two + inv || one + inv == two.name ())
    return Attribute::None ();

  const int size_diff = one.name ().size () - two.name ().size ();
  if (size_diff != 0)
    {
      const std::string full = size_diff > 0 ? one.name () : two.name ();
      const std::string suffix
        = "/" + (size_diff > 0 ? two.name () : one.name ());
      const int index = full.size () - suffix.size ();
      if (full.substr (index) == suffix)
        return full.substr (0, index);
    }
  return Attribute::Unknown ();
}

class Convert_pF_cm : public Oldunits::Convert
{
  bool valid (double value) const
  { return value >= 0.0; }
  double operator() (double value) const
  { return pF2h (value); }
};

class Convert_cm_pF : public Oldunits::Convert
{
  bool valid (double value) const
  { return value < 0.0; }
  double operator() (double value) const
  {
    if (value >= 0.0)
      return -1.0;
    return h2pF (value); 
  }
};

class Convert_kPa_pF : public Oldunits::Convert
{
  bool valid (double value) const
  { return value < 0.0; }
  double operator() (double value) const
  { 
    if (value >= 0.0)
      return -1.0;
    return h2pF (value * 10.0); 
  }
};

void
Oldunits::standard_conversions ()
{
  

  // Parameters.
  add ("m", "cm", 100.0);
  static boost::shared_ptr<Convert> Convert_pF_cm_ptr (new Convert_pF_cm ());
  add ("pF", "cm", Convert_pF_cm_ptr);
  static boost::shared_ptr<Convert> Convert_cm_pF_ptr (new Convert_cm_pF ());
  add ("cm", "pF", Convert_cm_pF_ptr);
  static boost::shared_ptr<Convert> Convert_kPa_pF_ptr (new Convert_kPa_pF ());
  add ("kPa", "pF", Convert_kPa_pF_ptr);
  add ("cm", "hPa", 1.0);
  add ("cm", "kPa", 0.1);
  add ("hPa", "kPa", 0.1);
  add ("cm", "Pa", 100.0);
  add ("s^-1", "h^-1", 60.0 * 60.0);
  add ("d^-1", "h^-1", 1.0/24.0);
  add ("d", "h", 24.0);
  add ("mm/d", "mm/h", 1.0/24.0);
  add ("m/s", "cm/h", 100.0 * 60.0 * 60.0);
  add ("m/s", "cm/d", 100.0 * 60.0 * 60.0 * 24.0);
  add ("m/d", "cm/h", 100.0 / 24.0);
  add ("cm/d", "cm/h", 1.0/24.0);
  add ("mm/h", "cm/h", 0.1);
  add ("mm/d", "cm/h", 0.1/24.0);
  add ("T w.w./ha", "Mg w.w./ha", 1.0);
  add ("kg w.w./ha", "g w.w./m^2", 0.1);
  add ("ppm", "mg N/l", 1.0);
  add ("ppm", "g/cm^2/mm", 1.0e-7);
  add ("L/kg", "cm^3/g", 1.0);
  add ("l/kg", "cm^3/g", 1.0);
  
  add ("g/cm^3", "kg/m^3", (100.0 * 100.0 * 100.0) / 1000.0);
  add ("g/cm^3", "mg/l", 1e6);
  add ("mol/m^2", "mmol/m^2", 1e3);
  add (Attribute::Fraction (), "mg N/kg dry soil", 1000000.0);
  add (Attribute::Fraction (), "ppm", 1000000.0);
  add (Attribute::Fraction (), "%", 100.0);
  add (Attribute::Fraction (), "", 1.0);
  add ("", "ppm", 1000000.0);
  add ("", "%", 100.0);
  add ("none", "", 1.0);
  add (Attribute::None (), "", 1.0);
  add ("cm^3/cm^3", Attribute::Fraction (), 1.0);
  add ("cm^3/cm^3", "", 1.0);
  add ("cm^3 H2O/cm^3", Attribute::Fraction (), 1.0);
  add ("cm^3 H2O/cm^3", "", 1.0);
  add ("g/cm^3", Attribute::Fraction (), 1.0);
  add ("kg/ha/y", "g/cm^2/h",
       (1000.0 / ((100.0 * 100.0) * (100.0 * 100.0))) / (365.2425 * 24.0));
  add ("g/cm^2/h", "g/ha/h", ((100.0 * 100.0) * (100.0 * 100.0)));
  add ("g/cm^2/h", "kg N/ha/h",
       ((100.0 * 100.0) * (100.0 * 100.0)) / 1000.0);
  add ("g/cm^2", "g/m^2", 100.0 * 100.0);
  add ("g/cm^2", "mg/m^2", 100.0 * 100.0 * 1000.0);
  add ("kg/ha", "mg/m^2", (1000.0 * 1000.0) / (100.0 * 100.0));
  add ("ppm", "kg/ha/mm", (100.0 * 100.0) / (1000.0 * 1000.0));
  add ("kg/ha/h", "kg N/ha/h", 1.0);
  add ("g/cm^2/h", "kg/ha/h", ((100.0 * 100.0) * (100.0 * 100.0)) / 1000.0);
  add ("g/cm^2/h", "kg C/ha/h", ((100.0 * 100.0) * (100.0 * 100.0)) / 1000.0);
  add ("g/ha", "kg/ha", 1.0 / 1000.0);
  add ("kg/ha/h", "g/ha/h", 1000.0);
  add ("cm^3/g", "cm^3/ng", 1e9);
  add ("MPa", "cm", 1e4);
  add ("MPa^-1", "cm^-1", 1e-4);
  add ("ng/mm/h", "g/cm/h", 1e-8);
  add ("ng/cm^3", "g/cm^3", 1e-9);
  add ("m^-2", "cm^-2", 1e-4);

  // Weather.
  add ("dgWest", "dgEast", -1.0);
  add ("dgSouth", "dgNorth", -1.0);
  add ("%", "fraction", 0.01);
  add ("MJ/d/m^2", "W/m^2", 1e6 / (24.0 * 60.0 * 60.0));
  add ("MJ/m^2/d", "W/m^2", 1e6 / (24.0 * 60.0 * 60.0));
  add ("mol/m^2/s", "W/m^2", 1.0 / 0.0000046); 
  add ("mmol/m^2/s", "W/m^2", 1000.0 / 0.0000046); 
  add ("kgN/year", "kgN/ha/year", 1.0); // Bug compatibility.

  // Log.
  add ("cm", "mm", 10.0);
  add ("g/cm^2", "kg/ha", 1e5);	// Pesticides.
  add ("g/cm^2", "g/ha", 1e5 * 1000);
  add ("g/m^2", "g/ha", 10000.0);
  add ("g/m^2/h", "g/ha/h", 10000.0);
  add ("g/cm^3", "ug/m^3", 1e12); 
  add ("g/cm^3", "ng/l", 1e12); 

  add ("g/cm^2", "kg N/ha", 1e5); // Inorganic N.
  add ("g N/cm^2/h", "kg N/ha/h", 1e5); 
  add ("g N/m^2", "kg N/ha", 10.0); // Crop N.
  add ("g/m^2", "kg/ha", 10.0);
  add ("g/m^2", "kg N/ha", 10.0);
  add ("g/m^2/h", "kg N/ha/h", 10.0);
  add ("g/cm^2", "kg C/ha", 1e5); // DOM.
  add ("g N/cm^2", "kg N/ha", 1e5); // Organic N.
  add ("g C/cm^2", "kg C/ha", 1e5);
  add (Attribute::Fraction (), "mg/l", 1e6);
  add ("g C/cm^2", "g/cm^2", 1.0);	// For "per dry matter" logging.
  add ("g N/cm^2", "g/cm^2", 1.0);	// For "per dry matter" logging.
  add ("g C/cm^3", "g/cm^3", 1.0);	// For "per dry matter" logging.
  add ("g N/cm^3", "g/cm^3", 1.0);	// For "per dry matter" logging.
  add ("g DM/m^2", "Mg DM/ha", 1.0e-2); // Crop production.
  add ("kg DM/ha", "Mg DM/ha", 0.001);
  add ("g/m^2", "Mg DM/ha", 1.0e-2); // harvest DM.
  add ("g/m^2/h", "Mg DM/ha/h", 1.0e-2); // harvest DM.
  add ("g CO_2-C/cm^2/h", "g CO2/m^2/h", 1.7272e4); // OM CO2
  add ("t/ha", "Mg DM/ha", 1.0); // harvest.dlf
  add ("erg/cm^3/dg C/h", "W/m/K", 2.7778e-9); // SoilHeat.
  add ("erg/cm^3/dg C", "kJ/m^3/K", 1e-4); // SoilHeat.
}

Oldunits::Oldunits ()
{ 
  if (content)
    {
      daisy_assert (count > 0);
      count++;
    }
  else
    {
      daisy_assert (count == 0);
      count = 1;
      content = new Content;
      standard_conversions ();
    }
}

Oldunits::~Oldunits ()
{
  daisy_safe_assert (content);
  daisy_safe_assert (count > 0);
  count--;
  if (count < 1)
    {
      delete content;
      content = NULL;
    }
}

// oldunits.C ends here.
