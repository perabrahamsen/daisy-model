// select_interval.C --- Select a state variable.
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
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
#include "border.h"
#include "units.h"
#include "mathlib.h"

struct SelectInterval : public SelectValue
{
  // Content.
  const bool density;
  double from;
  double to;

  const Geometry* last_geo;
  const Soil* last_soil;
  std::vector<int> cells;
  std::vector<double> weight;

  // Bulk density convertions.
  struct BD_convert : public Units::Convert
  {
    const Units::Convert& in;
    const Units::Convert& out;
    double bulk;

    // Use.
    double operator()(double value) const
    { 
      daisy_assert (bulk > 0.0);
      return out (in (value) / bulk); 
    }
    bool valid (double value) const
    {
      daisy_assert (bulk > 0.0);
      return in.valid (value) && out.valid (in (value) / bulk);
    }
    void set_bulk (const Geometry& geo,
                   const Soil& soil, const double from, double to)
    {
      if (to > 0.0)
	to = geo.bottom ();
      bulk = 0.0;

      const size_t cell_size = geo.cell_size ();
      for (size_t i = 0; i < cell_size; i++)
	{
          const double f = geo.fraction_in_z_interval (i, from, to);
          if (f > 1e-10)
            bulk += soil.dry_bulk_density (i) * geo.volume (i) * f;
	}
    }
    // Create and destroy.
    BD_convert (const std::string& has, const std::string& want)
      : in (Units::get_convertion (has, "g/cm^2")),
	out (Units::get_convertion (Syntax::Fraction (), want)),
	bulk (-42.42e42)
    { }
  } *bd_convert;
  const Units::Convert* special_convert (const std::string& has,
                                         const std::string& want)
  {
    daisy_assert (!bd_convert);
    if (Units::can_convert (has, "g/cm^2")
	&& Units::can_convert (Syntax::Fraction (), want))
      bd_convert = new BD_convert (has, want);
    return bd_convert;
  }

  // Output routines.

  void output_array (const std::vector<double>& array, 
		     const Geometry* geo,
                     const Soil* soil, Treelog&)
  { 
    if (soil != last_soil)
      {
        last_soil = soil;

        if (bd_convert)
          bd_convert->set_bulk (*geo, *soil, from, to);
      }

    if (geo != last_geo)
      {
        if (to > 0.0)
          to = geo->bottom ();
        last_geo = geo;
        const size_t cell_size = geo->cell_size ();
        cells.clear ();
        weight.clear ();
        double total_volume = 0.0;
        for (size_t n = 0; n < cell_size; n++)
          {
            const double f = geo->fraction_in_z_interval (n, from, to);
            if (f > 1e-10)
              {
                cells.push_back (n);
                const double volume = geo->volume (n) * f;
                weight.push_back (volume);
                total_volume += volume;
              }
          }
        daisy_assert (std::isnormal (total_volume) || weight.size () == 0);
        if (!density)
          total_volume /= (from - to);
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

  // Print result at end of time step.
  void done ()
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

  // Create and Destroy.
  const std::string default_dimension (const std::string& spec_dim) const
  { 
    if (density)
      return spec_dim;
    
    return Units::multiply (spec_dim, "cm");
  }

  bool initialize (const std::map<symbol, symbol>& conv, 
		   double default_from, double default_to, 
		   const std::string& timestep, Treelog& msg)
  {
    bool ok = true;

    if (!Select::initialize (conv, default_from, default_to, timestep, msg))
      ok = false;

    // Set default range.
    if (default_from <= 0.0 && from > 0.0)
      from = default_from;
    if (default_to <= 0.0 && to > 0.0)
      to = default_to;

    if (from > 0.0)
      from = 0.0;
    
    return ok;
  }
  bool check_border (const Border& border, 
                     const double default_from, const double default_to,
                     Treelog& msg) const
  { 
    bool ok = true;
    if (from < 0.0 
        && !approximate (from, default_from)
        && !border.check_border (from, msg))
      ok = false;
    if (to < 0.0 
        && !approximate (to, default_to)
        && !border.check_border (to, msg))
      ok = false;
    return ok; 
  }
  SelectInterval (Block& al)
    : SelectValue (al),
      density (al.flag ("density")),
      from (al.number ("from", 1.0)),
      to (al.number ("to", 1.0)),
      bd_convert (NULL)
  { }
  ~SelectInterval ()
  { delete bd_convert; }
};

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
    syntax.add ("from", "cm", Syntax::OptionalConst,
		"Specify height (negative) to measure from.\n\
By default, measure from the top.");
    syntax.add ("to", "cm", Syntax::OptionalConst,
		"Specify height (negative) to measure interval.\n\
By default, measure to the bottom.");

    Librarian<Select>::add_type ("interval", alist, syntax, &make);
  }
} Select_syntax;
