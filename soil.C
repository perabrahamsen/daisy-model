// soil.C
// 
// Copyright 1996-2004 Per Abrahamsen and Søren Hansen
// Copyright 2000-2004 KVL.
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

#include "soil.h"
#include "geometry.h"
#include "hydraulic.h"
#include "tortuosity.h"
#include "groundwater.h"
#include "library.h"
#include "alist.h"
#include "syntax.h"
#include "mathlib.h"
#include "submodel.h"
#include "submodeler.h"
#include "log.h"
#include "check.h"
#include "vcheck.h"
#include "plf.h"
#include "treelog.h"
#include "memutils.h"
#include "mathlib.h"
#include <sstream>
#include <iomanip>
#include <iostream>

struct Soil::Implementation
{
  // Layers.
  struct Layer
  {
    // Content.
    const double end;
    std::auto_ptr<Horizon> horizon;

    // Simulation.
    void output (Log& log) const
    { output_derived (horizon, "horizon", log); }

    // Create and Destroy.
    static void load_syntax (Syntax& syntax, AttributeList& alist)
    { 
      alist.add ("description", "\
A location and content of a soil layer.");
      syntax.add ("end", "cm", Check::negative (), Syntax::Const,
		  "End point of this layer (a negative number).");
      syntax.add ("horizon", Librarian<Horizon>::library (), 
		  "Soil properties of this layer.");
      syntax.order ("end", "horizon");
    }
    Layer (Block& al)
      : end (al.number ("end")),
	horizon (Librarian<Horizon>::build_item (al, "horizon"))
    { }
    ~Layer ()
    { }
  };
  /* const */ std::vector<Layer*> layers;
  const size_t original_layer_size; // Size before adding aquitard, for logging.

  // Parameters
  /* const */ double MaxRootingDepth;
  const double dispersivity;
  const std::vector<double> border;

  bool has_attribute (const std::string& name, Treelog& msg) const
  { 
    bool missing = false;
    for (size_t i = 0; i < layers.size (); i++)
      if (!layers[i]->horizon->has_attribute (name))
        {
          msg.error ("Required attribute '" 
                     + name + "' is missing from the soil horizon '"
                     + layers[i]->horizon->name + "'");
          missing = true;
        }
    return !missing;
  }
  
  bool has_attribute (const std::string& name) const
  { 
    bool missing = false;
    for (size_t i = 0; i < layers.size (); i++)
      if (!layers[i]->horizon->has_attribute (name))
	missing = true;
    return !missing;
  }
  
  // Create and Destroy.
  Implementation (Block& al)
    : layers (map_submodel<Layer> (al, "horizons")),
      original_layer_size (layers.size ()),
      MaxRootingDepth (al.number ("MaxRootingDepth")),
      dispersivity (al.number ("dispersivity")),
      border (al.number_sequence ("border"))
  { }
  ~Implementation ()
  { sequence_delete (layers.begin (), layers.end ()); }
};

size_t 
Soil::size () const
{ return horizon_.size (); }

double 
Soil::K (size_t i, double h, double h_ice, double T) const
{ 
  static struct ViscosityFactor : public PLF
  {
    ViscosityFactor ()
    {
      add ( 0.0, 1002.0 / 1786.0);
      add ( 5.0, 1002.0 / 1519.0);
      add (10.0, 1002.0 / 1307.0);
      add (15.0, 1002.0 / 1139.0);
      add (20.0, 1002.0 / 1002.0);
      add (25.0, 1002.0 /  890.0);
      add (30.0, 1002.0 /  798.0);
      add (35.0, 1002.0 /  719.0);
      add (40.0, 1002.0 /  658.0);
    }
  } viscosity_factor;

  const double T_factor = viscosity_factor (T);
  if (h < h_ice)
    return horizon_[i]->hydraulic->K (h) * T_factor; 
  else
    return horizon_[i]->hydraulic->K (h_ice) * T_factor; 
}

double 
Soil::Cw1 (size_t i, double h, double h_ice) const
{ return Theta (i, h, h_ice) - Cw2 (i, h) * h; }

double
Soil::Cw2 (size_t i, double h) const
{ 
  const double answer = horizon_[i]->hydraulic->Cw2 (h); 
  if (answer > 0.0)
    return answer;
  // We divide with this.
  return 1.0e-8;
}

double Soil::Theta (size_t i, double h, double h_ice) const
{ 
  if (h < h_ice)
    return horizon_[i]->hydraulic->Theta (h);
  else
    return horizon_[i]->hydraulic->Theta (h_ice);
}

double 
Soil::Theta_res (size_t i) const
{ return horizon_[i]->hydraulic->Theta_res; }

double 
Soil::h (size_t i, double Theta) const
{ return horizon_[i]->hydraulic->h (Theta); }

double 
Soil::M (size_t i, double h) const
{ return horizon_[i]->hydraulic->M (h); }

double 
Soil::dispersivity (size_t) const
{ return impl.dispersivity; }

void
Soil::set_porosity (size_t i, double Theta)
{ horizon_[i]->hydraulic->set_porosity (Theta); }

double 
Soil::tortuosity_factor (size_t i, double Theta) const
{ return horizon_[i]->tortuosity->factor (*horizon_[i]->hydraulic, Theta); }

double 
Soil::anisotropy (size_t i) const
{ return horizon_[i]->anisotropy (); }

double 
Soil::dry_bulk_density (size_t i) const
{ return horizon_[i]->dry_bulk_density (); }

double 
Soil::clay (size_t i) const
{ return horizon_[i]->clay (); }

double 
Soil::texture_below (size_t i, double size) const
{ return horizon_[i]->texture_below (size); }

double 
Soil::humus (size_t i) const
{ return horizon_[i]->humus (); }

double 
Soil::humus_C (size_t i) const
{ return horizon_[i]->humus_C (); }

const std::vector<double>& 
Soil::SOM_fractions (size_t i) const
{ return horizon_[i]->SOM_fractions (); }

const std::vector<double>& 
Soil::SOM_C_per_N (size_t i) const
{ return horizon_[i]->SOM_C_per_N (); }

double
Soil::C_per_N (size_t i) const
{ return horizon_[i]->C_per_N (); }

double 
Soil::turnover_factor (size_t i) const
{ return horizon_[i]->turnover_factor (); }

double 
Soil::heat_conductivity (size_t i, double Theta, double Ice) const
{ return horizon_[i]->heat_conductivity (Theta, Ice); }

double 
Soil::heat_capacity (size_t i, double Theta, double Ice) const
{ return horizon_[i]->heat_capacity (Theta, Ice); }

bool
Soil::has_attribute (const std::string& name, Treelog& msg) const
{ return impl.has_attribute (name, msg); }

bool
Soil::has_attribute (const std::string& name) const
{ return impl.has_attribute (name); }

bool 
Soil::has_attribute (size_t i, const std::string& name) const
{ return horizon_[i]->has_attribute (name); }

double 
Soil::get_attribute (size_t i, const std::string& name) const
{ return horizon_[i]->get_attribute (name); }

std::string
Soil::get_dimension (size_t i, const std::string& name) const
{ return horizon_[i]->get_dimension (name); }

void
Soil::output (Log& log) const
{
  static const symbol horizons_symbol ("horizons");
  if (log.check_interior (horizons_symbol))
    {
      Log::Open open (log, horizons_symbol);
      for (size_t i = 0; i < impl.original_layer_size; i++)
	{
	  Log::Unnamed unnamed (log);
	  impl.layers[i]->output (log);
	}
    }
}

void 
Soil::nitrification (const size_t i,
                     const double M, const double C, 
                     const double M_left,
                     const double h, const double T,
                     double& NH4, double& N2O, double& NO3, 
                     const double dt) const
{ horizon_[i]->nitrification (M, C, M_left, h,  T, NH4, N2O, NO3, dt); }

double
Soil::MaxRootingHeight () const
{
  return -impl.MaxRootingDepth;
}

double
Soil::end_of_first_horizon () const
{ 
  daisy_assert (impl.layers.size () > 0);
  return impl.layers[0]->end;
}

bool 
Soil::check (const int som_size, Geometry& geo, Treelog& err) const
{
  bool ok = true;
  if (som_size >= 0)
    {
      Treelog::Open nest (err, "horizons");
      for (size_t i = 0; i < impl.layers.size (); i++)
	{
	  const Horizon& horizon = *impl.layers[i]->horizon;
	  Treelog::Open nest (err, horizon.name);
	  const size_t f_size = horizon.SOM_fractions ().size ();
	  if (f_size > 0 && f_size != som_size)
	    {
	      Treelog::Open nest (err, "SOM_fractions");
	      std::ostringstream tmp;
	      tmp << "Need " << som_size << " fractions, got " << f_size;
	      err.error (tmp.str ());
	      ok = false;
	    }
	  const size_t n_size = horizon.SOM_C_per_N ().size ();
	  if (n_size != som_size)
	    {
	      Treelog::Open nest (err, "SOM_C_per_N");
	      std::ostringstream tmp;
	      tmp << "Need " << som_size << " C/N numbers, got " << n_size;
	      err.error (tmp.str ());
	      ok = false;
	    }
	}
    }

  bool geo_ok = true;
  for (size_t i = 0; i < geo.cell_size (); i++)
    if (horizon_[i] == NULL)
      geo_ok = false;

  if (!geo_ok)
    {
      Treelog::Open nest (err, "horizons");
      err.error ("Some cells have no associated horizon");
      ok = false;
    }

  return ok;
}

bool
Soil::check_z_border (const double value, Treelog& err) const
{
  bool ok = false;

  for (size_t i = 0; i < impl.border.size (); i++)
    if (approximate (value, impl.border[i]))
      ok = true;

  if (!ok)
    {
      std::ostringstream tmp;
      tmp << "No soil border near " << value
          << " [cm], log results may be inexact";
      err.warning (tmp.str ());
    }
  return ok;
}

bool
Soil::check_x_border (const double, Treelog&) const
{ return true; }

bool
Soil::check_y_border (const double, Treelog&) const
{ return true; }

static bool
check_alist (const AttributeList& al, Treelog& err)
{
  bool ok = true;

  const std::vector<AttributeList*>& layers = al.alist_sequence ("horizons");

  if (layers.size () < 1U)
    {
      err.entry ("You need at least one horizon");
      ok = false;
    }
  double last = 0.0;

  for (size_t i = 0; i < layers.size (); i++)
    {
      double end = layers[i]->number ("end");
      if (end >= last)
	{
	  err.entry ("Horizon endpoints must be monotonically decreasing");
	  ok = false;
	  break;
	}
      last = end;
    }
  return ok;
}  

void
Soil::load_syntax (Syntax& syntax, AttributeList& alist)
{ 
  syntax.add_check (check_alist);
  alist.add ("submodel", "Soil");
  alist.add ("description", "\
The soil component provides the numeric and physical properties of the soil.");
  syntax.add_submodule_sequence ("horizons", Syntax::State, "\
Layered description of the soil properties.\n\
Some groundwater models, specifically 'pipe', may cause an extra horizon to\n\
be added below the one specified here if you do not also specify an explicit\n\
geometry.",
				 Implementation::Layer::load_syntax);
  syntax.add ("MaxRootingDepth", "cm", Check::positive (), Syntax::Const,
	      "Depth at the end of the root zone (a positive number).");
  syntax.add ("dispersivity", "cm", Check::positive (), 
	      Syntax::Const, "Dispersion length.");
  alist.add ("dispersivity", 5.0);
  syntax.add ("border", "cm", Check::negative (), 
              Syntax::Const, Syntax::Sequence, "\
List of flux depths where a mass balance should be possible when logging.\n\
This attribute is ignored if the geometry is specified explicitly.");
  syntax.add_check ("border", VCheck::decreasing ());
  std::vector<double> default_borders;
  default_borders.push_back (-100.0);
  alist.add ("border", default_borders);
}
  
Soil::Soil (Block& al)
  : impl (*new Implementation (al))
{ }

double
Soil::initialize_aquitard (const double Z_aquitard, const double K_aquitard,
                           Treelog& msg)
{
  const double old_end = impl.layers[impl.layers.size () - 1]->end;
  const double Z_horizon
    = (Z_aquitard > 5.0) ? floor (Z_aquitard / 3.0)	: (Z_aquitard / 3.0);
  const double new_end = old_end - Z_horizon;

  // Add layer.
  Library& library = Librarian<Horizon>::library ();
  static const symbol aquitard_symbol ("aquitard");
  static const symbol default_symbol ("default");
  if (!library.check (aquitard_symbol))
    {
      // Create aquitard horizon.
      AttributeList& alist 
        = *new AttributeList (library.lookup (default_symbol));
      alist.add ("clay", 50.0);
      alist.add ("silt", 20.0);
      alist.add ("sand", 29.99);
      alist.add ("humus", 0.01);
      alist.add ("dry_bulk_density", 2.0);
      library.add_derived (aquitard_symbol, alist, default_symbol);
    }
  daisy_assert (library.check (aquitard_symbol));
  AttributeList horizon_alist (library.lookup (aquitard_symbol));
  horizon_alist.add ("type", "aquitard");
  AttributeList hydraulic_alist (horizon_alist.alist ("hydraulic"));
  hydraulic_alist.add ("K_sat", K_aquitard);
  horizon_alist.add ("hydraulic", hydraulic_alist);
  daisy_assert (library.syntax (aquitard_symbol).check (horizon_alist,
                                                        msg));
  Syntax layer_syntax;
  AttributeList layer_alist;
  Implementation::Layer::load_syntax (layer_syntax, layer_alist);
  layer_alist.add ("end", new_end);
  layer_alist.add ("horizon", horizon_alist);
  daisy_assert (layer_syntax.check (layer_alist, msg));
  Block block (layer_syntax, layer_alist, msg, "aquitard layer");
  impl.layers.push_back (new Implementation::Layer (block));

  // Return the new value of Z_aquitard.
  return Z_aquitard - Z_horizon;
}

void
Soil::initialize (Geometry& geo,
                  Groundwater& groundwater,
                  const int som_size, Treelog& msg)
{
  Treelog::Open nest (msg, "Soil");

  // Extra aquitard layer.
  if (groundwater.is_pipe ())
    {
      // Find parameters.
      const double Z_aquitard = groundwater.Z_aquitard ();
      const double K_aquitard = groundwater.K_aquitard ();
      const double new_Z_aq 
        = initialize_aquitard (Z_aquitard, K_aquitard, msg);
      groundwater.set_Z_aquitard (new_Z_aq);
    }
  const bool volatile_bottom =
    groundwater.bottom_type () == Groundwater::lysimeter 
    || groundwater.is_pipe (); 

  const std::vector<Implementation::Layer*>::const_iterator begin
    = impl.layers.begin ();
  const std::vector<Implementation::Layer*>::const_iterator end 
    = impl.layers.end ();
  daisy_assert (begin != end);
  std::vector<Implementation::Layer*>::const_iterator layer;

  // Initialize geometry.
  std::vector<double> fixed;
  {
    Treelog::Open nest (msg, "Horizons");
    double last = 0.0;
    size_t next_border = 0;
    for (layer = begin; layer != end; layer++)
      {
        const double current = (*layer)->end;
	daisy_assert (current < last);

	const bool top_soil = (layer == begin);
	(*layer)->horizon->initialize (top_soil, som_size, msg);

        while (next_border < impl.border.size ()
               && current < impl.border[next_border])
          {
            if (last > impl.border[next_border])
              fixed.push_back (impl.border[next_border]);
            next_border++;
          }
      
	last = current;
	fixed.push_back (last);
      }
    if (-last < impl.MaxRootingDepth)
      impl.MaxRootingDepth = -last;
  }
  geo.initialize_zplus (volatile_bottom, fixed, -impl.MaxRootingDepth, 
                        2 * impl.dispersivity, msg);

  // Initialize horizons.
  horizon_.insert (horizon_.end (), geo.cell_size (), NULL);
  daisy_assert (horizon_.size () == geo.cell_size ());
  double last = 0.0;

  for (layer = begin; layer != end; layer++)
    {
      Horizon *const h  = (*layer)->horizon.get ();
      const double next = (*layer)->end;

      for (size_t i = 0; i < geo.cell_size (); i++)
        {
          const double z = geo.z (i);
          if (last > z && z >= next)
            { 
              daisy_assert (horizon_[i] == NULL);
              horizon_[i] = h;
            }
        }
      last = next;
    }
  for (size_t i = 0; i < geo.cell_size (); i++)
    {
      std::ostringstream tmp;
      tmp << "cell[" << i << "] of " << geo.cell_size () << " z = " << geo.z (i) << ", last = " << last;
      Treelog::Open nest (msg, tmp.str ());
      daisy_assert (horizon_[i] != NULL);
    }
}

Soil::~Soil ()
{ delete &impl; }

static Submodel::Register 
soil_submodel ("Soil", Soil::load_syntax);
