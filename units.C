// units.C -- conversion between different dimensions.
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


#include "units.h"
#include "syntax.h"
#include "mathlib.h"
#include "memutils.h"
#include <map>

using namespace std;

struct Units::Content
{
  typedef map<string, Convert*, less<string>/**/> to_type;
  typedef map<string, to_type, less<string>/**/> table_type;
  table_type table;

  static bool time_match (const string& from, const string& to);
  static const string crop_time (const string&);
  
  double convert (const string& from, const string& to, double value) const;
  bool can_convert (const string& from, const string& to) const;
  bool can_convert (const string& from, const string& to, double value) const;
  const Convert& get_convertion (const string& from, const string& to) const;

  Content ();
  ~Content ();
};  

bool				// True iff FROM and TO have same time unit.
Units::Content::time_match (const string& from, const string& to)
{
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

const string			// Return DIM without time.
Units::Content::crop_time (const string& dim)
{
  daisy_assert (dim.size () > 0);
  size_t end;
  for (end = dim.size () - 1; dim[end] != '/'; end--)
    daisy_assert (end > 0);
  string result;
  for (int i = 0; i < end; i++)
    result += dim[i];
  return result;
}

double 
Units::Content::convert (const string& from, const string& to, 
			 double value) const
{ 
  if (from == to)
    return value;

  const Units::Convert& conv = get_convertion (from, to);

  if (!conv.valid (value))
    throw string ("invalid value");

  return conv (value);
}

bool 
Units::Content::can_convert (const string& from, const string& to) const
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
Units::Content::can_convert (const string& from, const string& to, 
			     double value) const
{ 
  if (from == to)
    return true;

  try 
    {
      const Units::Convert& conv = get_convertion (from, to);
      if (!conv.valid (value))
	return false;
    }
  catch (...)
    {
      return false;
    }
  return true;
}

static const class ConvertIdentity : public Units::Convert
{
  double operator() (double value) const
  { return value; }
public:
  ConvertIdentity ()
  { }
} convert_identity;

const Units::Convert&
Units::Content::get_convertion (const string& from, const string& to) const
{ 
  if (from == to)
    return convert_identity;

  table_type::const_iterator i = table.find (from);
  if (i == table.end ())
    {
      // We check if we can convert without time.
      if (time_match (from, to))
	{
	  const string from_c = crop_time (from);
	  table_type::const_iterator i_c = table.find (from_c);
	  if (i_c != table.end ())
	    {
	      const string to_c = crop_time (to);
	      to_type::const_iterator j_c = (*i_c).second.find (to_c);
	      if (j_c != (*i_c).second.end ())
		return *(*j_c).second;
	    }
	}
      throw string ("[") + from + "] unknown dimension, expected [" + to + "]";
    }
  to_type::const_iterator j = (*i).second.find (to);
  if (j == (*i).second.end ())
    throw string ("Cannot convert [") + from + "] to [" + to + "]";

  return *(*j).second;
}

Units::Content::Content ()
{ }
  
Units::Content::~Content ()
{
  for (table_type::iterator i = table.begin ();
       i != table.end ();
       i++)
    for (to_type::iterator j = (*i).second.begin ();
	 j != (*i).second.end ();
	 j++)
      map_delete ((*i).second.begin (), (*i).second.begin ());
}

Units::Content* Units::content = NULL;
int Units::count = 0;

bool
Units::Convert::valid (double) const
{ return true; }

Units::Convert::Convert ()
{ }

Units::Convert::~Convert ()
{ }

struct ConvertLinear : public Units::Convert
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
Units::add (const string& from, const string& to, double factor, double offset)
{ 
  daisy_assert (content);
  if (!(content->table[from].find (to) == content->table[from].end ()))
    daisy_warning ("convert from [" + from + "] to [" + to + "] duplicate\n");
  else
    content->table[from][to] = new ConvertLinear (factor, offset);
  // Reverse conversion.
  daisy_assert (isnormal (factor));
  if (!(content->table[to].find (from) == content->table[to].end ()))
    daisy_warning ("convert from [" + from + "] to [" + to + "] reverse\n");
  else
    content->table[to][from] = new ConvertLinear (1.0 / factor, 
                                                  -offset / factor);
}

void 
Units::add (const string& from, const string& to, Convert& convert)
{
  daisy_assert (content);
  daisy_assert (content->table[from].find (to) == content->table[from].end ());
  content->table[from][to] = &convert;
  
}

double 
Units::convert (const string& from, const string& to, double value)
{ 
  daisy_assert (content);
  return content->convert (from, to, value);
}

bool
Units::can_convert (const string& from, const string& to)
{ 
  daisy_assert (content);
  return content->can_convert (from, to);
}

bool
Units::can_convert (const string& from, const string& to, double value)
{ 
  daisy_assert (content);
  return content->can_convert (from, to, value);
}

const Units::Convert&
Units::get_convertion (const string& from, const string& to)
{
  daisy_assert (content);
  return content->get_convertion (from, to);
}

string
Units::multiply (const string& one, const string& two)
{ 
  if (one == Syntax::None () || one == Syntax::Fraction ())
    return two;
  if (two == Syntax::None () || two == Syntax::Fraction ())
    return one;
  if (one == Syntax::Unknown () || two == Syntax::Unknown ())
    return Syntax::Unknown ();

  static const struct multiply_table
  { 
    const char* one;
    const char* two;
    const char* result;
  } table[] = { 
    { "h^-1", "cm", "cm/h" },
    { "cm^3/cm^3", "cm", "cm" },
    { "g/cm^3", "cm", "g/cm^2" },
    { "g C/cm^3", "cm", "g C/cm^2" },
    { "g N/cm^3", "cm", "g N/cm^2" },
    { "g/cm^3/h", "cm", "g/cm^2/h" },
    { "g C/cm^3/h", "cm", "g C/cm^2/h" },
    { "g N/cm^3/h", "cm", "g N/cm^2/h" }, 
    { "g CO_2-C/cm^3/h", "cm", "g CO_2-C/cm^2/h"},
  };
  
  for (unsigned int i = 0; i < sizeof (table) / sizeof (multiply_table); i++)
    if ((one == table[i].one && two == table[i].two)
	|| (two == table[i].one && one == table[i].two))
      return table[i].result;

  return Syntax::Unknown ();
}

// GCC 2.95 requires these to be defined outside a function.
static class Convert_pF_cm_ : public Units::Convert
{
  double operator() (double value) const
  { return pF2h (value); }
} Convert_pF_cm;

static class Convert_cm_pF_ : public Units::Convert
{
  double operator() (double value) const
  { return h2pF (value); }
} Convert_cm_pF;

void
Units::standard_conversions ()
{
  // Parameters.
  add ("m", "cm", 100.0);
  add ("pF", "cm", Convert_pF_cm);
  add ("cm", "pF", Convert_cm_pF);
  add ("cm", "hPa", 1.0);
  add ("cm", "kPa", 0.1);
  add ("hPa", "kPa", 0.1);
  add ("cm", "Pa", 100.0);
  add ("d^-1", "h^-1", 1.0/24.0);
  add ("d", "h", 24.0);
  add ("mm/d", "mm/h", 1.0/24.0);
  add ("m/s", "cm/h", 100.0 * 60.0 * 60.0);
  add ("cm/d", "cm/h", 1.0/24.0);
  add ("mm/h", "cm/h", 0.1);
  add ("mm/d", "cm/h", 0.1/24.0);
  add ("T w.w./ha", "Mg w.w./ha", 1.0);
  add ("ppm", "mg N/l", 1.0);
  add ("L/kg", "cm^3/g", 1.0);
  add ("l/kg", "cm^3/g", 1.0);
  add ("g/cm^3", "kg/m^3", (100.0 * 100.0 * 100.0) / 1000.0);
  add (Syntax::Fraction (), "mg N/kg dry soil", 1000000.0);
  add (Syntax::Fraction (), "ppm", 1000000.0);
  add (Syntax::Fraction (), "%", 100.0);
  add (Syntax::Fraction (), "", 1.0);
  add ("", "ppm", 1000000.0);
  add ("", "%", 100.0);
  add ("none", "", 1.0);
  add (Syntax::None (), "", 1.0);
  add ("cm^3/cm^3", Syntax::Fraction (), 1.0);
  add ("cm^3/cm^3", "", 1.0);
  add ("cm^3 H2O/cm^3", Syntax::Fraction (), 1.0);
  add ("cm^3 H2O/cm^3", "", 1.0);
  add ("g/cm^3", Syntax::Fraction (), 1.0);

  // Weather.
  add ("dgWest", "dgEast", -1.0);
  add ("dgSouth", "dgNorth", -1.0);
  add ("%", "fraction", 0.01);
  add ("MJ/d/m^2", "W/m^2", 1e6 / (24.0 * 60.0 * 60.0));
  add ("MJ/m^2/d", "W/m^2", 1e6 / (24.0 * 60.0 * 60.0));
  add ("kgN/year", "kgN/ha/year", 1.0); // Bug compatibility.

  // Log.
  add ("cm", "mm", 10.0);
  add ("g/cm^2", "kg/ha", 1e5);	// Pesticides.
  add ("g/cm^2", "kg N/ha", 1e5); // Inorganic N.
  add ("g N/cm^2/h", "kg N/ha/h", 1e5); 
  add ("g N/m^2", "kg N/ha", 10.0); // Crop N.
  add ("g/m^2", "kg N/ha", 10.0);
  add ("g/m^2/h", "kg N/ha/h", 10.0);
  add ("g/cm^2", "kg C/ha", 1e5); // DOM.
  add ("g N/cm^2", "kg N/ha", 1e5); // Organic N.
  add ("g C/cm^2", "kg C/ha", 1e5);
  add (Syntax::Fraction (), "mg/l", 1e6);
  add ("g C/cm^2", "g/cm^2", 1.0);	// For "per dry matter" logging.
  add ("g N/cm^2", "g/cm^2", 1.0);	// For "per dry matter" logging.
  add ("g/m^2/h", "Mg DM/ha/h", 1.0e-2); // harvest DM.
  add ("g CO2-C/cm^2/h", "CO2/m^2/h", 1.7272e4); // OM CO2
  add ("t/ha", "Mg DM/ha", 1.0); // harvest.dlf
}

Units::Units ()
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

Units::~Units ()
{
  daisy_assert (content);
  daisy_assert (count > 0);
  count--;
  if (count < 1)
    {
      delete content;
      content = NULL;
    }
}
