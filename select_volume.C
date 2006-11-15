// select_volume.C --- Select a state variable in a volume.
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
#include "volume.h"
#include "geometry.h"
#include "units.h"
#include <memory>

struct SelectVolume : public SelectValue
{
  // Content.
  const bool density;
  std::auto_ptr<Volume> volume;
  const Geometry* last_geo;
  std::vector<int> cells;
  std::vector<double> weight;

  // Output routines.
  void output_array (const std::vector<double>& array, 
		     const Geometry* geo, const Soil*, Treelog&)
  { 
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
                const double vol = geo->volume (n) * f;
                weight.push_back (vol);
                total_volume += vol;
              }
          }
        if (iszero (total_volume))
          daisy_assert (weight.size () == 0);
        else if (density)
          for (size_t i = 0; i < cells.size (); i++)
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
  symbol default_dimension (const symbol spec_dim) const
  { 
    if (density)
      return spec_dim;
    
    return Units::multiply (spec_dim, Units::cm3);
  }

  bool initialize (double default_from, double default_to, 
		   const std::string& timestep, Treelog& msg)
  {
    if (default_from < 0.0 && !volume->has_top ())
      volume->limit_top (default_from);
    if (default_to < 0.0 && !volume->has_bottom ())
      volume->limit_bottom (default_to);

    return Select::initialize (default_from, default_to, timestep, msg);
  }
  bool check_border (const Border& border, 
                     const double default_from, const double default_to,
                     Treelog& msg) const
  { return volume->check_border (border, default_from, default_to, msg); }
  SelectVolume (Block& al)
    : SelectValue (al),
      density (al.flag ("density")),
      volume (Librarian<Volume>::build_item (al, "volume")),
      last_geo (NULL)
  { }
  ~SelectVolume ()
  { }
};

static struct SelectVolumeSyntax
{
  static Select& make (Block& al)
  { return *new SelectVolume (al); }

  SelectVolumeSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    SelectValue::load_syntax (syntax, alist);
    alist.add ("description", "Summarize specified volume.");

    syntax.add ("density", Syntax::Boolean, Syntax::Const, 
		"If true, divide value with volume height.");
    alist.add ("density", false);
    syntax.add ("volume", Librarian<Volume>::library (), 
                Syntax::Const, Syntax::Singleton,
                "Soil volume to log.");
    alist.add ("volume", Volume::infinite_box ());

    Librarian<Select>::add_type ("volume", alist, syntax, &make);
  }
} Select_syntax;
