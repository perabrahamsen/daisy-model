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
#include "bdconv.h"
#include "block.h"
#include "volume.h"
#include "geometry.h"
#include "soil.h"
#include "vegetation.h"
#include "check.h"
#include "librarian.h"
#include "frame.h"
#include "assertion.h"
#include "mathlib.h"
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
  std::auto_ptr<BD_convert> bd_convert;
  const Convert* special_convert (const Units&, 
                                  const symbol has, const symbol want);

  // Output routines.
  void output_array (const std::vector<double>& array, 
		     const Geometry* geo, const Soil* soil, const Vegetation*, 
		     Treelog&);

  // Create and Destroy.
  symbol default_dimension (const symbol spec_dim) const;
  bool initialize (const Units&, const Volume& default_volume, 
		   const symbol timestep, Treelog& msg);
  bool check_border (const Border& border, 
                     const Volume& default_volume,
                     Treelog& msg) const;
  SelectVolume (Block& al);
  ~SelectVolume ();
};

int
SelectVolume::dimensions () const
{ return (density_z ? 1 : 0) + (density_x ? 1 : 0) + (density_y ? 1 : 0); }

const Convert* 
SelectVolume::special_convert (const Units& units, 
                               const symbol has, const symbol want)
{
  daisy_assert (!bd_convert.get ());
  static const symbol bulk_density ("g/cm^3");
  const symbol bulk_dim = default_dimension (bulk_density);
  if (units.can_convert (has, bulk_dim)
      && units.can_convert (Value::Fraction (), want))
    bd_convert.reset (new BD_convert (units, has, want, bulk_dim));
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
      daisy_panic ("Can't handle more than 3 spacial dimensions");
    }
}

bool 
SelectVolume::initialize (const Units& units, const Volume& default_volume, 
                          const symbol timestep, Treelog& msg)
{
  bool ok = true;

  if (!Select::initialize (units, default_volume, timestep, msg))
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
    min_root_crop (al.name ("min_root_crop")),
    bd_convert (NULL)
{ }
  
SelectVolume::~SelectVolume ()
{ }

static struct SelectVolumeBase : public DeclareModel
{
  Model* make (Block& al) const
  { return new SelectVolume (al); }
  SelectVolumeBase ()
    : DeclareModel (Select::component, "volume_base", "value", "\
Shared parameters for volume based logs.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare ("density", Value::Boolean, Value::Const, 
		"If true, divide total content with volume.\n\
Otherwise, obey 'density_z', 'density_x', and 'density_y'.");
    frame.set ("density", false);
    frame.declare ("density_z", Value::Boolean, Value::Const, 
		"If true, divide total content with volume height.\n\
This parameter is ignored if 'density' is true.");
    frame.declare ("density_x", Value::Boolean, Value::Const, 
		"If true, divide total content with volume width.\n\
This parameter is ignored if 'density' is true.");
    frame.declare ("density_y", Value::Boolean, Value::Const, 
		"If true, divide total content with volume depth.\n\
This parameter is ignored if 'density' is true.");
    frame.declare_object ("volume", Volume::component, 
                       Value::Const, Value::Singleton,
                       "Soil volume to log.");
    frame.set ("volume", "box");

    frame.declare ("min_root_density", "cm/cm^3", Value::Const, "\
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
    frame.set ("min_root_density", -1.0);
    frame.declare ("min_root_crop", Value::String, Value::Const, "\
Name of crop whose roots scould be used for the root density requirements.\n\
Set this to \"*\" to use all roots.");
    frame.set ("min_root_crop", "*"); // Select::wildcard may not be initialized.
  }
} SelectVolume_base;

static struct SelectVolumeSyntax : public DeclareParam
{
  SelectVolumeSyntax ()
    : DeclareParam (Select::component, "volume", "volume_base", "\
Summarize specified volume.")
  { }
  void load_frame (Frame& frame) const
  { 
    frame.set ("density_z", false);
    frame.set ("density_x", false);
    frame.set ("density_y", false);
  }
} Select_volume_syntax;

static struct SelectIntervalSyntax : public DeclareParam
{
  SelectIntervalSyntax ()
    : DeclareParam (Select::component, "interval", "volume_base",
                    "Summarize specified interval.\n\
This is similar to 'volume', except for the default values of\n         \
'density_x' and 'density_y', and the unqiue 'from' and 'to' parameters.")
  { }
  void load_frame (Frame& frame) const
  { 
    frame.set ("density_z", false);
    frame.set ("density_x", true);
    frame.set ("density_y", true);
    frame.declare ("from", "cm", Value::OptionalConst,
		"Specify height (negative) to measure from.\n\
By default, measure from the top.\n\
OBSOLETE: Use (volume box (top FROM)) instead.");
    frame.declare ("to", "cm", Value::OptionalConst,
		"Specify height (negative) to measure interval.\n\
By default, measure to the bottom.\n\
OBSOLETE: Use (volume box (bottom TO)) instead.");
  }
} Select_interval_syntax;

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

static struct SelectWaterSyntax : public DeclareModel
{
  Model* make (Block& al) const
  { return new SelectWater (al); }

  SelectWaterSyntax ()
    : DeclareModel (Select::component, "water", "volume_base", "\
Shared parameters for water limited volumn logging.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare ("h", "cm", Check::non_positive (), Value::Const, 
                "Pressure to log water content for.");
    frame.declare ("h_ice", "cm", Check::non_positive (), Value::Const, 
		"Pressure at which all air is out of the matrix.\n\
When there are no ice, this is 0.0.  When there are ice, the ice is\n\
presumed to occupy the large pores, so it is h (Theta_sat - X_ice).");
    frame.set ("h_ice", 0.0);
  }
} SelectWater_syntax;
  
static struct SelectWaterVolumeParam : public DeclareParam
{
  SelectWaterVolumeParam ()
    : DeclareParam (Select::component, "water_volume", "water", "\
Summarize water content in the specified volume.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.set ("density_z", false);
    frame.set ("density_x", false);
    frame.set ("density_y", false);
 }    
} Select_water_volume_syntax;

static struct SelectWaterIntervalParam : public DeclareParam
{
  void load_frame (Frame& frame) const
  {
    frame.set ("density_z", false);
    frame.set ("density_x", true);
    frame.set ("density_y", true);
    frame.declare ("from", "cm", Value::OptionalConst,
		"Specify height (negative) to measure from.\n\
By default, measure from the top.\n\
OBSOLETE: Use (volume box (top FROM)) instead.");
    frame.declare ("to", "cm", Value::OptionalConst,
		"Specify height (negative) to measure interval.\n\
By default, measure to the bottom.\n\
OBSOLETE: Use (volume box (bottom TO)) instead.");
  }    
  SelectWaterIntervalParam ()
    : DeclareParam (Select::component, "water_interval", "water", "\
Summarize water content in the specified interval.\n\
This is similar to 'water_volume', except for the default values of\n\
'density_x' and 'density_y', and the unqiue 'from' and 'to' parameters.")
  { }
} Select_water_interval_syntax;

// select_volumne.C ends here
