// select_interval.C --- Select a state variable.
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
// Copyright 2006 Per Abrahamsen and KVL.
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


#include "select_value.h"
#include "geometry.h"
#include "soil.h"
#include "units.h"

struct SelectInterval : public SelectValue
{
  // Content.
  const bool density;
  std::auto_ptr<Volume> volume;
  const Geometry* last_geo;
  const Soil* last_soil;
  std::vector<int> cells;
  std::vector<double> weight;

  // Bulk density convertions.
  struct BD_convert;

  std::auto_ptr<BD_convert> bd_convert;
  const Units::Convert* special_convert (const symbol has, const symbol want);

  // Output routines.
  void output_array (const std::vector<double>& array, 
		     const Geometry* geo,
                     const Soil* soil, Treelog&);

  // Print result at end of time step.
  void done (); 

  // Create and Destroy.
  symbol default_dimension (const symbol spec_dim) const;
  bool initialize (const Volume& default_volume,
                   const std::string& timestep, Treelog& msg);
  bool check_border (const Border& border, const Volume& default_volume,
                     Treelog& msg) const;
  SelectInterval (Block& al);
  ~SelectInterval ();
};

struct SelectInterval::BD_convert : public Units::Convert
{
  const Units::Convert& in;
  const Units::Convert& out;
  double bulk;
  static const symbol bulk_unit;

  // Use.
  double operator()(double value) const;
  bool valid (double value) const;
  void set_bulk (const Geometry& geo,
                 const Soil& soil, const Volume& volume);

  // Create and destroy.
  BD_convert (const symbol has, const symbol want);
};

const symbol 
SelectInterval::BD_convert::bulk_unit ("g/cm^2");

double
SelectInterval::BD_convert::operator()(double value) const
{ 
  daisy_assert (bulk > 0.0);
  return out (in (value) / bulk); 
}

bool 
SelectInterval::BD_convert::valid (double value) const
{
  daisy_assert (bulk > 0.0);
  return in.valid (value) && out.valid (in (value) / bulk);
}

void 
SelectInterval::BD_convert::set_bulk (const Geometry& geo,
                                      const Soil& soil, const Volume& volume)
{
  const size_t cell_size = geo.cell_size ();
  for (size_t i = 0; i < cell_size; i++)
    {
      const double f = geo.fraction_in_volume (i, volume);
      if (f > 1e-10)
        bulk += soil.dry_bulk_density (i) * geo.volume (i) * f;
    }
  bulk /= volume.width (geo.left (), geo.right ()); 
  bulk /= volume.depth (geo.front (), geo.back ()); 
}

SelectInterval::BD_convert::BD_convert (const symbol has, const symbol want)
  : in (Units::get_convertion (has, bulk_unit)),
    out (Units::get_convertion (Syntax::fraction (), want)),
    bulk (-42.42e42)
{ }

const Units::Convert* 
SelectInterval::special_convert (const symbol has, const symbol want)
{
  daisy_assert (!bd_convert.get ());
  if (Units::can_convert (has, BD_convert::bulk_unit)
      && Units::can_convert (Syntax::fraction (), want))
    bd_convert.reset (new BD_convert (has, want));
  return bd_convert.get ();
}

// Output routines.

void 
SelectInterval::output_array (const std::vector<double>& array, 
                              const Geometry* geo,
                              const Soil* soil, Treelog&)
{ 
  if (soil != last_soil)
    {
      last_soil = soil;

      if (bd_convert.get ())
        bd_convert->set_bulk (*geo, *soil, *volume);
    }

  if (geo != last_geo)
    {
      last_geo = geo;
      const size_t cell_size = geo->cell_size ();
      cells.clear ();
      weight.clear ();
      double total_volume = 0.0;
      for (size_t n = 0; n < cell_size; n++)
        {
          const double f 
            = geo->fraction_in_volume (n, *volume);
          if (f > 1e-10)
            {
              cells.push_back (n);
              const double cell_vol = geo->volume (n) * f;
              weight.push_back (cell_vol);
              total_volume += cell_vol;
            }
        }
      daisy_assert (std::isnormal (total_volume) || weight.size () == 0);
      if (!density)
        total_volume /= volume->height (geo->bottom (), geo->top ());
      for (size_t i = 0; i < weight.size (); i++)
        weight[i] /= total_volume;
      daisy_assert (cells.size () == weight.size ());
    }
  daisy_assert (cells.size () <= array.size ());

  double sum = 0.0;
  for (size_t i = 0; i < cells.size (); i++)
    sum += array[cells[i]] * weight[i];

  add_result (sum);
}

void 
SelectInterval::done ()
{
  if (count == 0)
    dest.missing ();
  else 
    {
      double result = value;
      switch (handle)
        {
        case Handle::average:
          result /= (count + 0.0);
          break;
        case Handle::geometric:
          result /= (count + 0.0);
          result = exp (result);
          break;
        case Handle::min:
        case Handle::max:
        case Handle::sum:
        case Handle::current:
          break;
        }            
      dest.add (convert (result));
    }
  if (!accumulate)
    count = 0;
}

symbol 
SelectInterval::default_dimension (const symbol spec_dim) const
{ 
  if (density)
    return spec_dim;
    
  return Units::multiply (spec_dim, Units::cm);
}

bool 
SelectInterval::initialize (const Volume& default_volume,
                            const std::string& timestep, Treelog& msg)
{
  bool ok = true;

  if (!Select::initialize (default_volume, timestep, msg))
    ok = false;

  if (!volume->limit (default_volume, msg))
    ok = false;

  return ok;
}

bool 
SelectInterval::check_border (const Border& border, 
                              const Volume& default_volume,
                              Treelog& msg) const
{ return volume->check_border (border, default_volume, msg); }
  
SelectInterval::SelectInterval (Block& al)
  : SelectValue (al),
    density (al.flag ("density")),
    volume (Volume::build_obsolete (al)),
    last_geo (NULL),
    last_soil (NULL),
    bd_convert (NULL)
{ }
  
SelectInterval::~SelectInterval ()
{ }

static struct SelectIntervalSyntax
{
  static Select& make (Block& al)
  { return *new SelectInterval (al); }

  SelectIntervalSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    SelectValue::load_syntax (syntax, alist);
    alist.add ("description", "Summarize specified interval.");

    syntax.add ("density", Syntax::Boolean, Syntax::Const, 
		"If true, divide value with interval height.");
    alist.add ("density", false);
    syntax.add ("volume", Librarian<Volume>::library (), 
                Syntax::Const, Syntax::Singleton,
                "Soil volume to log.");
    alist.add ("volume", Volume::infinite_box ());
    syntax.add ("from", "cm", Syntax::OptionalConst,
		"Specify height (negative) to measure from.\n\
By default, measure from the top.\n\
OBSOLETE: Use (volume box (top FROM)) instead.");
    syntax.add ("to", "cm", Syntax::OptionalConst,
		"Specify height (negative) to measure interval.\n\
By default, measure to the bottom.\n\
OBSOLETE: Use (volume box (bottom TO)) instead.");

    Librarian<Select>::add_type ("interval", alist, syntax, &make);
  }
} Select_syntax;
