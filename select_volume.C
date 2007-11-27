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

#define BUILD_DLL

#include "select_value.h"
#include "block.h"
#include "alist.h"
#include "volume.h"
#include "geometry.h"
#include "soil.h"
#include "vegetation.h"
#include "units.h"
#include "check.h"
#include "librarian.h"
#include <memory>

struct SelectVolume : public SelectValue
{
  // Content.
  const bool density_z;
  const bool density_x;
  const bool density_y;
  int dimensions () const;
  std::auto_ptr<Volume> volume;
  const Geometry* last_geo;
  const Soil* last_soil;
  std::vector<int> cells;
  std::vector<double> weight;
  const double min_root_density;
  const symbol min_root_crop;

  // Bulk density convertions.
  struct BD_convert;
  std::auto_ptr<BD_convert> bd_convert;
  const Units::Convert* special_convert (const symbol has, const symbol want);

  // Output routines.
  void output_array (const std::vector<double>& array, 
		     const Geometry* geo, const Soil* soil, const Vegetation*, 
		     Treelog&);

  // Print result at end of time step.
  void done (double dt);

  // Create and Destroy.
  symbol default_dimension (const symbol spec_dim) const;
  bool initialize (const Volume& default_volume, 
		   const std::string& timestep, Treelog& msg);
  bool check_border (const Border& border, 
                     const Volume& default_volume,
                     Treelog& msg) const;
  SelectVolume (Block& al);
  ~SelectVolume ();
};

int
SelectVolume::dimensions () const
{ return (density_z ? 1 : 0) + (density_x ? 1 : 0) + (density_y ? 1 : 0); }

struct SelectVolume::BD_convert : public Units::Convert
{
  const Units::Convert& in;
  const Units::Convert& out;
  double bulk;

  // Use.
  double operator()(double value) const;
  bool valid (double value) const;
  void set_bulk (const Geometry& geo,
                 const Soil& soil, const Volume& volume,
                 const bool density_z, const bool density_x,
                 const bool density_y);

  // Create and destroy.
  BD_convert (const symbol has, const symbol want, const symbol bulk_unit);
};

double
SelectVolume::BD_convert::operator()(double value) const
{ 
  daisy_assert (bulk > 0.0);
  return out (in (value) / bulk); 
}

bool
SelectVolume::BD_convert::valid (double value) const
{
  daisy_assert (bulk > 0.0);
  return in.valid (value) && out.valid (in (value) / bulk);
}

void
SelectVolume::BD_convert::set_bulk (const Geometry& geo,
                                    const Soil& soil, const Volume& volume,
                                    const bool density_z, const bool density_x,
                                    const bool density_y)
{
  bulk = 0.0;

  const size_t cell_size = geo.cell_size ();
  for (size_t i = 0; i < cell_size; i++)
    {
      const double f = geo.fraction_in_volume (i, volume);
      if (f > 1e-10)
        bulk += soil.dry_bulk_density (i) * geo.cell_volume (i) * f;
    }
  if (density_z)
    bulk /= volume.height (geo.bottom (), geo.top ()); 
  if (density_x)
    bulk /= volume.width (geo.left (), geo.right ()); 
  if (density_y)
    bulk /= volume.depth (geo.front (), geo.back ()); 
}

SelectVolume::BD_convert::BD_convert (const symbol has, const symbol want,
                                      const symbol bulk_unit)
  : in (Units::get_convertion (has, bulk_unit)),
    out (Units::get_convertion (Syntax::fraction (), want)),
    bulk (-42.42e42)
{ }

const Units::Convert* 
SelectVolume::special_convert (const symbol has, const symbol want)
{
  daisy_assert (!bd_convert.get ());
  static const symbol bulk_density ("g/cm^3");
  const symbol bulk_dim = default_dimension (bulk_density);
  if (Units::can_convert (has, bulk_dim)
      && Units::can_convert (Syntax::fraction (), want))
    bd_convert.reset (new BD_convert (has, want, bulk_dim));
  return bd_convert.get ();
}

// Output routines.
void 
SelectVolume::output_array (const std::vector<double>& array, 
                            const Geometry* geo, const Soil* soil, 
			    const Vegetation* vegetation, Treelog& msg)
{ 
  if (soil != last_soil)
    {
      last_soil = soil;

      if (bd_convert.get ())
        bd_convert->set_bulk (*geo, *soil, *volume, 
                              density_z, density_x, density_y);
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
              const double vol = geo->cell_volume (n) * f;
              weight.push_back (vol);
              total_volume += vol;
            }
        }
      if (iszero (total_volume))
        daisy_assert (weight.size () == 0);
      else 
        {
          if (dimensions () > 0)
            {
              if (!density_z)
                total_volume /= volume->height (geo->bottom (), geo->top ());
              if (!density_x)
                total_volume /= volume->width (geo->left (), geo->right ());
              if (!density_y)
                total_volume /= volume->depth (geo->front (), geo->back ());
              for (size_t i = 0; i < cells.size (); i++)
                weight[i] /= total_volume;
            }
        }
      daisy_assert (cells.size () == weight.size ());
    }
  daisy_assert (cells.size () <= array.size ());

  double sum = 0.0;

  if (min_root_density > 0.0)
    {
      const std::vector<double>& root_density 
	= (min_root_crop == wildcard)
	? vegetation->root_density ()
	: vegetation->root_density (min_root_crop);

      for (size_t i = 0; i < cells.size (); i++)
	{
	  const size_t cell = cells[i];

	  if (root_density.size () > cell)
	    {
	      const double density = root_density[cell];

	      if (density > 0.0)
		{
		  const double value = array[cell] * weight[i];

		  if (density < min_root_density)
		    sum += value * density / min_root_density;
		  else
		    sum += value;
		}
	    }
	}
    }
  else
    for (size_t i = 0; i < cells.size (); i++)
      sum += array[cells[i]] * weight[i];

  add_result (sum);
}

// Print result at end of time step.
void 
SelectVolume::done (const double dt)
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
        case Handle::sum:
          result *= dt;
          break;
        case Handle::min:
        case Handle::max:
        case Handle::current:
          break;
        }            
      dest.add (convert (result));
    }
  if (!accumulate)
    count = 0;
}

// Create and Destroy.
symbol 
SelectVolume::default_dimension (const symbol spec_dim) const
{ 
  switch (dimensions ())
    {
    case 0:
      return Units::multiply (spec_dim, Units::cm3 ());
    case 1:
      return Units::multiply (spec_dim, Units::cm2 ());
    case 2:
      return Units::multiply (spec_dim, Units::cm ());
    case 3:
      return spec_dim;
    default:
      daisy_panic ("Can't handle more than 3 space dimensions");
    }
}

bool 
SelectVolume::initialize (const Volume& default_volume, 
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
SelectVolume::check_border (const Border& border, 
                            const Volume& default_volume,
                            Treelog& msg) const
{ return volume->check_border (border, default_volume, msg); }
  
SelectVolume::SelectVolume (Block& al)
  : SelectValue (al),
    density_z (al.flag ("density") || al.flag ("density_z")),
    density_x (al.flag ("density") || al.flag ("density_x")),
    density_y (al.flag ("density") || al.flag ("density_y")),
    volume (Volume::build_obsolete (al)),
    last_geo (NULL),
    last_soil (NULL),
    min_root_density (al.number ("min_root_density")),
    min_root_crop (al.identifier ("min_root_crop")),
    bd_convert (NULL)
{ }
  
SelectVolume::~SelectVolume ()
{ }

static struct SelectVolumeSyntax
{
  static Model& make (Block& al)
  { return *new SelectVolume (al); }

  static void load_syntax (Syntax& syntax, AttributeList& alist)
  {
    SelectValue::load_syntax (syntax, alist);

    syntax.add ("density", Syntax::Boolean, Syntax::Const, 
		"If true, divide total content with volume.\n\
Otherwise, obey 'density_z', 'density_x', and 'density_y'.");
    alist.add ("density", false);
    syntax.add ("density_z", Syntax::Boolean, Syntax::Const, 
		"If true, divide total content with volume height.\n\
This parameter is ignored if 'density' is true.");
    syntax.add ("density_x", Syntax::Boolean, Syntax::Const, 
		"If true, divide total content with volume width.\n\
This parameter is ignored if 'density' is true.");
    syntax.add ("density_y", Syntax::Boolean, Syntax::Const, 
		"If true, divide total content with volume depth.\n\
This parameter is ignored if 'density' is true.");
    syntax.add_object ("volume", Volume::component, 
                       Syntax::Const, Syntax::Singleton,
                       "Soil volume to log.");
    alist.add ("volume", Volume::infinite_box ());

    syntax.add ("min_root_density", "cm/cm^3", Syntax::Const, "\
Minimum root density in cells.\n\
\n\
Set this paramater to a positive amount in order to log only cells\n\
within the (dynamic) root zone.  If the root density in the cell is\n\
above this amount, the full amount of the data being logged will be\n\
included.  If the root density is below, the amount included will be\n\
scaled down accordingly.  That is, if there are no roots, the data for\n\
the cell will be scaled to zero, while if there is only half the\n\
specified minimum root density, the data for the cell will be scaled\n\
to 0.5.");
    alist.add ("min_root_density", -1.0);
    syntax.add ("min_root_crop", Syntax::String, Syntax::Const, "\
Name of crop whose roots scould be used for the root density requirements.\n\
Set this to \"*\" to use all roots.");
    alist.add ("min_root_crop", "*");	// Select::wildcard may not be initialized.
  }
  void add_volume ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    load_syntax (syntax, alist);
    alist.add ("description", "Summarize specified volume.");
    alist.add ("density_z", false);
    alist.add ("density_x", false);
    alist.add ("density_y", false);
    Librarian::add_type (Select::component, "volume", alist, syntax, &make);
  }    
  void add_interval ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    load_syntax (syntax, alist);
    alist.add ("description", "Summarize specified interval.\n\
This is similar to 'volume', except for the default values of\n\
'density_x' and 'density_y', and the unqiue 'from' and 'to' parameters.");
    alist.add ("density_z", false);
    alist.add ("density_x", true);
    alist.add ("density_y", true);
    syntax.add ("from", "cm", Syntax::OptionalConst,
		"Specify height (negative) to measure from.\n\
By default, measure from the top.\n\
OBSOLETE: Use (volume box (top FROM)) instead.");
    syntax.add ("to", "cm", Syntax::OptionalConst,
		"Specify height (negative) to measure interval.\n\
By default, measure to the bottom.\n\
OBSOLETE: Use (volume box (bottom TO)) instead.");
    Librarian::add_type (Select::component, "interval", alist, syntax, &make);
  }    
  SelectVolumeSyntax ()
  { 
    add_volume ();
    add_interval ();
  }
} Select_volume_syntax;

// Here follows a hack to log the water content at fixed pressure.

struct SelectWater : public SelectVolume
{
  std::vector<double> water;
  const double h;
  const double h_ice;

  void output_array (const std::vector<double>&, 
		     const Geometry* geo, const Soil* soil,
		     const Vegetation* veg, 
		     Treelog& msg)
  {
    if (soil != last_soil || geo != last_geo)
      {
	if (!soil)
	  throw "Needs soil to log water at fixed pressure";

	water.clear ();
	while (water.size () < soil->size ())
	  water.push_back (soil->Theta (water.size (), h, h_ice));
      }
    SelectVolume::output_array (water, geo, soil, veg, msg);
  }

  SelectWater (Block& al)
    : SelectVolume (al),
      h (al.number ("h")),
      h_ice (al.number ("h_ice"))
  { }
};

static struct SelectWaterSyntax
{
  static Model& make (Block& al)
  { return *new SelectWater (al); }

  static void load_syntax (Syntax& syntax, AttributeList& alist)
  {
    SelectVolumeSyntax::load_syntax (syntax, alist);

    syntax.add ("h", "cm", Check::non_positive (), Syntax::Const, 
                "Pressure to log water content for.");
    syntax.add ("h_ice", "cm", Check::non_positive (), Syntax::Const, 
		"Pressure at which all air is out of the matrix.\n\
When there are no ice, this is 0.0.  When there are ice, the ice is\n\
presumed to occupy the large pores, so it is h (Theta_sat - X_ice).");
    alist.add ("h_ice", 0.0);
  }
  void add_volume ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    load_syntax (syntax, alist);
    alist.add ("description", "\
Summarize water content in the specified volume.");
    alist.add ("density_z", false);
    alist.add ("density_x", false);
    alist.add ("density_y", false);
    Librarian::add_type (Select::component, "water_volume", 
			 alist, syntax, &make);
  }    
  void add_interval ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    load_syntax (syntax, alist);
    alist.add ("description", "\
Summarize water content in the specified interval.\n\
This is similar to 'volume', except for the default values of\n\
'density_x' and 'density_y', and the unqiue 'from' and 'to' parameters.");
    alist.add ("density_z", false);
    alist.add ("density_x", true);
    alist.add ("density_y", true);
    syntax.add ("from", "cm", Syntax::OptionalConst,
		"Specify height (negative) to measure from.\n\
By default, measure from the top.\n\
OBSOLETE: Use (volume box (top FROM)) instead.");
    syntax.add ("to", "cm", Syntax::OptionalConst,
		"Specify height (negative) to measure interval.\n\
By default, measure to the bottom.\n\
OBSOLETE: Use (volume box (bottom TO)) instead.");
    Librarian::add_type (Select::component, "water_interval", 
			 alist, syntax, &make);
  }    
  SelectWaterSyntax ()
  { 
    add_volume ();
    add_interval ();
  }
} Select_water_syntax;

// select_volumne.C ends here
