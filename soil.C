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

#define BUILD_DLL

#include "soil.h"
#include "horizon.h"
#include "geometry.h"
#include "hydraulic.h"
#include "tortuosity.h"
#include "groundwater.h"
#include "metalib.h"
#include "library.h"
#include "frame_submodel.h"
#include "mathlib.h"
#include "librarian.h"
#include "submodeler.h"
#include "log.h"
#include "check.h"
#include "vcheck.h"
#include "plf.h"
#include "treelog.h"
#include "memutils.h"
#include "mathlib.h"
#include "secondary.h"
#include "zone.h"
#include "water.h"
#include "soil_water.h"
#include "soil_heat.h"
#include "organic.h"
#include <sstream>

struct Soil::Implementation
{
  // Layers.
  struct Layer
  {
    // Content.
    const double end;
    std::unique_ptr<Horizon> horizon;

    // Simulation.
    void output (Log& log) const
    {
      output_derived (horizon, "horizon", log);
    }

    // Create and Destroy.
    static void load_syntax (Frame& frame)
    { 
      frame.declare ("end", "cm", Check::negative (), Attribute::Const,
		  "End point of this layer (a negative number).");
      frame.declare_object ("horizon", Horizon::component, 
                         "Soil properties of this layer.");
      frame.order ("end", "horizon");
    }
    Layer (const Block& al)
      : end (al.number ("end")),
	horizon (Librarian::build_item<Horizon> (al, "horizon"))
    { }
    ~Layer ()
    { }
  };
  auto_vector<const Layer*> layers;

  static std::vector<double> endpoints (const std::vector<const Layer*>& layers)
  {
    std::vector<double> result;
    for (const Layer* layer : layers)
      result.push_back (layer->end);
    return result;
  }

  // Regions.
  struct Region
  {
    // Content.
    std::unique_ptr<Zone> volume;
    std::unique_ptr<Horizon> horizon;

    // Simulation.
    void output (Log& log) const
    { output_derived (horizon, "horizon", log); }

    // Create and Destroy.
    static void load_syntax (Frame& frame)
    { 
      frame.declare_object ("volume", Zone::component, 
                         "Volume covered by this zone.");
      frame.declare_object ("horizon", Horizon::component, 
                         "Soil properties of this zone.");
      frame.order ("volume", "horizon");
    }
    Region (const Block& al)
      : volume (Librarian::build_item<Zone> (al, "volume")),
	horizon (Librarian::build_item<Horizon> (al, "horizon"))
    { }
    ~Region ()
    { }
  };
  const auto_vector<const Region*> zones;
  const auto_vector<Hydraulic*> hyd_cells;

  // Access.
  std::vector<Horizon*> horizon_;
  std::vector<const Hydraulic*> hydraulic_;

  // Volume
  std::map<const Horizon*, double> volume;

  // Parameters
  /* const */ double MaxRootingDepth;
  const double dispersivity;
  const double dispersivity_transversal;
  const std::vector<double> border;
  const double frozen_water_K_factor; // []

  // Cache.
  std::vector<double> anisotropy_edge;
  
  bool has_attribute (const symbol name, Treelog& msg) const
  { 
    bool missing = false;
    for (size_t i = 0; i < layers.size (); i++)
      if (!layers[i]->horizon->has_attribute (name))
        {
          msg.error ("Required attribute '" 
                     + name + "' is missing from the soil horizon '"
                     + layers[i]->horizon->objid + "'");
          missing = true;
        }
    for (size_t i = 0; i < zones.size (); i++)
      if (!zones[i]->horizon->has_attribute (name))
        {
          msg.error ("Required attribute '" 
                     + name + "' is missing from the soil zone '"
                     + zones[i]->horizon->objid + "'");
          missing = true;
        }
    return !missing;
  }
  
  bool has_attribute (const symbol name) const
  { 
    bool missing = false;
    for (size_t i = 0; i < layers.size (); i++)
      if (!layers[i]->horizon->has_attribute (name))
	missing = true;
    for (size_t i = 0; i < zones.size (); i++)
      if (!zones[i]->horizon->has_attribute (name))
	missing = true;
    return !missing;
  }
  
  void tillage (const Geometry& geo, const double from, const double to,
                const double surface_loose, const double RR0, 
                const SoilWater& soil_water, 
                const OrganicMatter& organic_matter)
  {
    // Water content.
    std::map<const Horizon*, double> water;
    for (std::size_t c = 0; c < geo.cell_size (); c++)
      if (geo.fraction_in_z_interval (c, from, to) > 0.01)
        water[horizon_[c]] += soil_water.Theta (c) * geo.cell_volume (c);

    static const double c_fraction_in_humus = 0.587;
    static const double aom_from = 0.0; // [cm]
    static const double aom_to = -15.0; // [cm]
    const double AOM_C = organic_matter.AOM_C (geo, aom_from, aom_to)
      / geo.surface_area ();    // [g C/cm^2]
    const double AOM15 = 10.0 * AOM_C / c_fraction_in_humus;   // [kg/m^2]

    for (std::map<const Horizon*, double>::const_iterator i = water.begin ();
         i != water.end ();
         i++)
      {
        const Horizon *const hor = (*i).first;
        const double total_water = (*i).second;
        const double total_volume = volume[hor];
        daisy_assert (total_water < total_volume);
        const double Theta = total_water / total_volume;
        hor->hydraulic->tillage (surface_loose, RR0, Theta, AOM15);
      }
  }
  void tick (const double dt, const double rain, const Geometry& geo,
             const SoilWater& soil_water, const SoilHeat& soil_heat,
	     Treelog& msg)
  {
    // Ice content.
    std::map<const Horizon*, double> ice;
    for (std::size_t c = 0; c < geo.cell_size (); c++)
      ice[horizon_[c]] += soil_water.X_ice (c) * geo.cell_volume (c);
    
    for (std::map<const Horizon*, double>::const_iterator i = ice.begin ();
         i != ice.end ();
         i++)
      {
        const Horizon *const hor = (*i).first;
        const double total_ice = (*i).second;
        const double total_volume = volume[hor];
        const double Ice = total_ice / total_volume;
        hor->hydraulic->tick (dt, rain, Ice, msg);
      }

    // Hysteresis.
    for (size_t c = 0; c < hyd_cells.size (); c++)
      {
	hyd_cells[c]->hysteresis (dt, soil_water.h_old (c), soil_water.h (c),
				  soil_heat.T (c));
      }
  }

  // Create and Destroy.
  Implementation (const Block& al)
    : layers (map_submodel_const<Layer> (al, "horizons")),
      zones (map_submodel_const<Region> (al, "zones")),
      hyd_cells (al.check ("Hydraulic")
		 ? Librarian::build_vector<Hydraulic> (al, "Hydraulic")
		 : std::vector<Hydraulic*> ()),
      MaxRootingDepth (al.number ("MaxRootingDepth")),
      dispersivity (al.number ("dispersivity")),
      dispersivity_transversal (al.number ("dispersivity_transversal",
					   dispersivity * 0.1)),
      border (al.check ("border")
	      ? al.number_sequence ("border")
	      : endpoints (layers)),
      frozen_water_K_factor (al.number ("frozen_water_K_factor"))
  { }
  ~Implementation ()
  { }
};

static DeclareSubmodel 
soil_layer_submodel (Soil::Implementation::Layer::load_syntax, "SoilLayer", "\
A location and content of a soil layer.\n\
The layers apply to the soil section not covered by the 'zones' parameter.");

static DeclareSubmodel 
soil_zone_submodel (Soil::Implementation::Region::load_syntax, "SoilRegion", "\
A location and content of a soil zone.\n\
If several zones cover the same soil, the first one listed is used.\n\
If no zones cover the soil, the 'horizons' parameter is used.\n\
\n\
With regard to the numeric discretization, the whole cell is assumed to\n\
be of the soil found in the cell center.");

size_t 
Soil::size () const
{ return impl->horizon_.size (); }

const Horizon& 
Soil::horizon (size_t i) const
{ return *impl->horizon_[i]; }

const Hydraulic& 
Soil::hydraulic (size_t i) const
{ return *impl->hydraulic_[i]; }

double 
Soil::K (size_t i, double h, double h_ice, double T) const
{ 
  static struct ViscosityFactor : public PLF
  {
    ViscosityFactor ()
    {
      const double v20 = Water::viscosity (20.0);
      add ( 0.0, v20 / Water::viscosity (0.0));
      add ( 5.0, v20 / Water::viscosity (5.0));
      add (10.0, v20 / Water::viscosity (10.0));
      add (15.0, v20 / Water::viscosity (15.0));
      add (20.0, v20 / Water::viscosity (20.0));
      add (25.0, v20 / Water::viscosity (25.0));
      add (30.0, v20 / Water::viscosity (30.0));
      add (35.0, v20 / Water::viscosity (35.0));
      add (40.0, v20 / Water::viscosity (40.0));
    }
  } viscosity_factor;

  const double T_factor = (T < 0.0)
    ? impl->frozen_water_K_factor
    : viscosity_factor (T);
  const double h_water = std::min (h, h_ice);

  const double K_primary = hydraulic (i).KT (h_water, T); 
  const double K_secondary = horizon (i).secondary_domain ().K (h_water);
  const double K_factor = horizon (i).K_factor ();

  return T_factor * K_factor * std::max (K_primary, K_secondary);
}

double 
Soil::Cw1 (size_t i, double h, double h_ice) const
{ return Theta (i, h, h_ice) - Cw2 (i, h) * h; }

double
Soil::Cw2 (size_t i, double h) const
{ 
  const double answer = hydraulic (i).Cw2 (h); 
  if (answer > 0.0)
    return answer;
  // We divide with this.
  return 1.0e-8;
}

double Soil::Theta (size_t i, double h, double h_ice) const
{ 
  if (h < h_ice)
    return hydraulic (i).Theta (h);
  else
    return hydraulic (i).Theta (h_ice);
}

double 
Soil::Theta_res (size_t i) const
{ return hydraulic (i).Theta_res; }

double 
Soil::Theta_sat (size_t i) const
{ return hydraulic (i).Theta_sat; }

double 
Soil::h (size_t i, double Theta) const
{ return hydraulic (i).h (Theta); }

double 
Soil::M (size_t i, double h) const
{ return hydraulic (i).M (h); }

double
Soil::primary_sorption_fraction (size_t c) const
{ return horizon (c).primary_sorption_fraction (); }

double 
Soil::dispersivity (size_t) const
{ return impl->dispersivity; }

double 
Soil::dispersivity_transversal (size_t c) const 
{ return impl->dispersivity_transversal; } 

void 
Soil::tillage (const Geometry& geo, const double from, const double to,
               const double surface_loose, const double RR0, 
               const SoilWater& soil_water,
               const OrganicMatter& organic_matter)
{ impl->tillage (geo, from, to, surface_loose, RR0, 
                 soil_water, organic_matter); }

void
Soil::tick (const double dt, const double rain, const Geometry& geo,
            const SoilWater& soil_water, const SoilHeat& soil_heat,
	    Treelog& msg)
{ impl->tick (dt, rain, geo, soil_water, soil_heat, msg); }

void
Soil::set_porosity (size_t i, double Theta)
{ horizon (i).hydraulic->set_porosity (Theta); }

double              // Activation pressure for secondary domain. [cm] 
Soil::h_secondary (size_t i) const
{ return horizon (i).secondary_domain ().h_lim (); }

double  // Exchange rate between primary and secondary water.  [h^-1] 
Soil::alpha (size_t i) const
{ return horizon (i).secondary_domain ().alpha (); }

double 
Soil::tortuosity_factor (size_t i, double Theta) const
{ return horizon (i).tortuosity->factor (hydraulic (i), Theta); }

double 
Soil::anisotropy_cell (size_t c) const
{ return horizon (c).anisotropy (); }

double 
Soil::anisotropy_edge (size_t e) const
{ 
  daisy_assert (impl->anisotropy_edge.size () > e);
  return impl->anisotropy_edge[e];
}

double 
Soil::dry_bulk_density (size_t i) const
{ return horizon (i).dry_bulk_density (); }

double 
Soil::clay (size_t i) const
{ return horizon (i).clay (); }

double 
Soil::texture_below (size_t i, double size) const
{ return horizon (i).texture_below (size); }

double 
Soil::humus (size_t i) const
{ return horizon (i).humus (); }

double 
Soil::humus_C (size_t i) const
{ return horizon (i).humus_C (); }

const std::vector<double>& 
Soil::SOM_fractions (size_t i) const
{ return horizon (i).SOM_fractions (); }

const std::vector<double>& 
Soil::SOM_C_per_N (size_t i) const
{ return horizon (i).SOM_C_per_N (); }

double
Soil::C_per_N (size_t i) const
{ return horizon (i).C_per_N (); }

double 
Soil::turnover_factor (size_t i) const
{ return horizon (i).turnover_factor (); }

double
Soil::root_homogeneity (size_t i) const
{ return horizon (i).root_homogeneity (); }

double
Soil::root_retardation (size_t i) const
{ return horizon (i).root_retardation (); }

double 
Soil::heat_conductivity (size_t i, double Theta, double Ice) const
{ return horizon (i).heat_conductivity (Theta, Ice); }

double 
Soil::heat_capacity (size_t i, double Theta, double Ice) const
{ return horizon (i).heat_capacity (Theta, Ice); }

bool
Soil::has_attribute (const symbol name, Treelog& msg) const
{ return impl->has_attribute (name, msg); }

bool
Soil::has_attribute (const symbol name) const
{ return impl->has_attribute (name); }

bool 
Soil::has_attribute (size_t i, const symbol name) const
{ return horizon (i).has_attribute (name); }

double 
Soil::get_attribute (size_t i, const symbol name) const
{ return horizon (i).get_attribute (name); }

symbol
Soil::get_dimension (size_t i, const symbol name) const
{ return horizon (i).get_dimension (name); }

void 
Soil::append_attributes (size_t i, std::set<symbol>& all) const
{ horizon (i).append_attributes (all); }

void
Soil::output (Log& log) const
{
  static const symbol horizons_symbol ("horizons");
  if (log.check_interior (horizons_symbol))
    {
      Log::Open open (log, horizons_symbol);
      for (size_t i = 0; i < impl->layers.size (); i++)
	{
	  Log::Unnamed unnamed (log);
	  impl->layers[i]->output (log);
	}
    }
  static const symbol zones_symbol ("zones");
  if (log.check_interior (zones_symbol))
    {
      Log::Open open (log, zones_symbol);
      for (size_t i = 0; i < impl->zones.size (); i++)
	{
	  Log::Unnamed unnamed (log);
	  impl->zones[i]->output (log);
	}
    }
  output_list (impl->hyd_cells, "Hydraulic", log, Hydraulic::component);
}

void 
Soil::nitrification (const size_t i,
                     const double M, const double C, 
                     const double h, const double T,
                     double& NH4, double& N2O, double& NO3) const
{ horizon (i).nitrification (M, C, h,  T, NH4, N2O, NO3); }

double
Soil::MaxRootingHeight () const
{
  return -impl->MaxRootingDepth;
}

double
Soil::end_of_first_horizon () const
{ 
  daisy_assert (impl->layers.size () > 0);
  return impl->layers[0]->end;
}

bool 
Soil::check (const int som_size, Geometry& geo, Treelog& err) const
{
  bool ok = true;
  if (som_size >= 0)
    {
      {
        Treelog::Open nest (err, "horizons");
        for (size_t i = 0; i < impl->layers.size (); i++)
          {
            const Horizon& horizon = *impl->layers[i]->horizon;
            Treelog::Open nest (err, horizon.objid);
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
      {
        Treelog::Open nest (err, "zones");
        for (size_t i = 0; i < impl->zones.size (); i++)
          {
            const Horizon& horizon = *impl->zones[i]->horizon;
            Treelog::Open nest (err, horizon.objid);
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
    }

  bool geo_ok = true;
  for (size_t i = 0; i < geo.cell_size (); i++)
    if (impl->horizon_[i] == NULL)
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

  for (size_t i = 0; i < impl->border.size (); i++)
    if (approximate (value, impl->border[i]))
      ok = true;

  if (!ok)
    {
      std::ostringstream tmp;
      tmp << "No soil border near " << value
          << " [cm], log results may be inexact.\n\
Use (Soil (border ... " << value << " ... [cm]) ... ) to correct";
      err.error (tmp.str ());
      
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
check_alist (const Metalib&, const Frame& al, Treelog& err)
{
  bool ok = true;

  const std::vector<boost::shared_ptr<const FrameSubmodel>/**/>& layers 
    = al.submodel_sequence ("horizons");

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
Soil::load_syntax (Frame& frame)
{ 
  frame.add_check (check_alist);
  frame.declare_submodule_sequence ("horizons", Attribute::State, "\
Layered description of the soil properties.\n\
The horizons can be overlapped by the 'zones' parameter.\n\
Some groundwater models, specifically 'pipe', may cause an extra horizon to\n\
be added below the one specified here if you do not also specify an explicit\n\
geometry.",
				 Implementation::Layer::load_syntax);
  frame.declare_submodule_sequence ("zones", Attribute::State, "\
Regions with special soil properties.\n\
This overrules the 'horizons' paramter.",
				 Implementation::Region::load_syntax);
  frame.set_empty ("zones");
  frame.declare_object ("Hydraulic", Hydraulic::component, 
			Attribute::OptionalState, Attribute::SoilCells,
			"Hydraulic properties for each cell.\n\
Use when the hydraulic properties are dynamic and cells specific.\n\
For example when implementing hysteresis.");
  frame.declare ("MaxRootingDepth", "cm", Check::positive (), Attribute::Const,
	      "Depth at the end of the root zone (a positive number).");
  frame.declare ("dispersivity", "cm", Check::positive (), 
	      Attribute::Const, "Dispersion length.");
  frame.set ("dispersivity", 5.0);
  frame.declare ("dispersivity_transversal", "cm", Check::non_negative (), 
	      Attribute::OptionalConst, "Transversal dispersion length.\n\
By default, this is 0.1 times the dispersivity.");
  frame.declare ("border", "cm", Check::negative (), 
              Attribute::OptionalConst, Attribute::Variable, "\
List of flux depths where a mass balance should be possible when logging.\n\
By default, this is the endpoint of each horizon.");
  frame.set_check ("border", VCheck::decreasing ());
  frame.declare ("frozen_water_K_factor", Attribute::None (), 
                 Check::positive (), 
                 Attribute::OptionalConst, "\
Hydraulic conductivity for water below 0 dg C compared to 20 dg C.\n\
The default value, 0.561, corresponds to 0 dg C water viscosity.");
  frame.set ("frozen_water_K_factor", 0.561);
}
  
Soil::Soil (const Block& al)
  : impl (new Implementation (al))
{ }

void
Soil::initialize (const Time& time, Geometry& geo,
                  Groundwater& groundwater,
                  const int som_size, Treelog& msg)
{
  Treelog::Open nest (msg, "Soil");

  const bool volatile_bottom =
    groundwater.bottom_type () == Groundwater::lysimeter; 

  const std::vector<const Implementation::Layer*>::const_iterator begin
    = impl->layers.begin ();
  const std::vector<const Implementation::Layer*>::const_iterator end 
    = impl->layers.end ();
  daisy_assert (begin != end);
  std::vector<const Implementation::Layer*>::const_iterator layer;

  // Initialize zone horizons.
  for (int i = 0; i < impl->zones.size (); i++)
    {
      const Zone& zone = *impl->zones[i]->volume;
      const double center_z = zone.center_z ();
      const bool top_soil = center_z > -20; // Center of zone within plow layer
      impl->zones[i]->horizon->initialize (top_soil, som_size, center_z, msg);
    }

  // Initialize geometry and layer horizons.
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
        const double center_z = 0.5 * (current + last); // center of layer.
	(*layer)->horizon->initialize (top_soil, som_size, center_z, msg);

        while (next_border < impl->border.size ()
               && current < impl->border[next_border])
          {
            if (last > impl->border[next_border])
              fixed.push_back (impl->border[next_border]);
            next_border++;
          }
      
	last = current;
	fixed.push_back (last);
      }
    if (-last < impl->MaxRootingDepth)
      impl->MaxRootingDepth = -last;
  }
  geo.initialize_zplus (volatile_bottom, fixed, -impl->MaxRootingDepth, 
                        2 * impl->dispersivity, msg);
  const size_t cell_size = geo.cell_size ();

  // Initialize horizons.
  impl->horizon_.insert (impl->horizon_.end (), cell_size, NULL);
  daisy_assert (impl->horizon_.size () == cell_size);

  // Check zones first.
  for (size_t c = 0; c < cell_size; c++)
    {
      for (size_t i = 0; i < impl->zones.size (); i++)
        if (impl->zones[i]->volume->contain_point (geo.cell_z (c), 
                                                  geo.cell_x (c), geo.cell_y (c)))
          {
            daisy_assert (impl->horizon_[c] == NULL);
            impl->horizon_[c] = impl->zones[i]->horizon.get ();
            break;
          }
    }

  // Fill in missing stuff by layers.
  double last = 0.0;
  for (layer = begin; layer != end; layer++)
    {
      Horizon *const h  = (*layer)->horizon.get ();
      const double next = (*layer)->end;

      for (size_t i = 0; i < cell_size; i++)
        {
          if (impl->horizon_[i] != NULL)
            // Already defined by a zone.
            continue;

          const double z = geo.cell_z (i);
          if (last > z && z >= next)
            { 
              daisy_assert (impl->horizon_[i] == NULL);
              impl->horizon_[i] = h;
            }
        }
      last = next;
    }
  daisy_assert (impl->hydraulic_.size () == 0);
  for (size_t i = 0; i < cell_size; i++)
    {
      // Sanity check.
      std::ostringstream tmp;
      tmp << "cell[" << i << "] of " << cell_size
          << " z = " << geo.cell_z (i) << ", last = " << last;
      Treelog::Open nest (msg, tmp.str ());
      daisy_assert (impl->horizon_[i] != NULL);

      // Count volume for all horizons.
      impl->volume[impl->horizon_[i]] +=geo.cell_volume (i);

      assert (impl->hydraulic_.size () == i);
      if (impl->hyd_cells.size () > i)
	impl->hydraulic_.push_back (impl->hyd_cells[i]);
      else
	impl->hydraulic_.push_back (impl->horizon_[i]->hydraulic.get ());
    }
  daisy_assert (impl->hydraulic_.size () == cell_size);

  for (int i = 0; i < impl->zones.size (); i++)
    {
      const Horizon* h = impl->zones[i]->horizon.get ();
      if (impl->volume[h] > 0.0)
        continue;
      std::ostringstream tmp;
      tmp << "zone '" << h->objid << "' has volume of " << impl->volume[h];
      msg.warning (tmp.str ());
    }
  for (layer = begin; layer != end; layer++)
    {
      const Horizon* h = (*layer)->horizon.get ();
      if (impl->volume[h] > 0.0)
        continue;
      std::ostringstream tmp;
      tmp << "horizon '" << h->objid << "' has volume of " << impl->volume[h];
      msg.warning (tmp.str ());
    }

  // anisotropy_edge.
  const size_t edge_size = geo.edge_size ();
  for (size_t e = 0; e < edge_size; e++)
    {
      const int from = geo.edge_from (e);
      const int to = geo.edge_to (e);
  
      // External edges.
      if (!geo.cell_is_internal (from))
        {
          daisy_assert (geo.cell_is_internal (to));
          impl->anisotropy_edge.push_back (anisotropy_cell (to));
          continue;
        }
      if (!geo.cell_is_internal (to))
        {
          daisy_assert (geo.cell_is_internal (from));
          impl->anisotropy_edge.push_back (anisotropy_cell (from));
          continue;
        }

      // Internal edges.
      const double sin_angle = geo.edge_sin_angle (e);
      const double cos_angle = geo.edge_cos_angle (e);
      const double a_from = anisotropy_cell (from);
      const double a_to = anisotropy_cell (to);

      // Arithmetic average.  Because we don't know.
      const double factor = (a_from + a_to) / 2.0;

      // Geometric average.  Because it is a geometric problem.
      const double aniso = sqrt (sqr (sin_angle) + sqr (factor * cos_angle));
      impl->anisotropy_edge.push_back (aniso);
    }
  daisy_assert (impl->anisotropy_edge.size () == edge_size);

  if (impl->hyd_cells.size () > 0 && impl->hyd_cells.size () != cell_size)
    {
      std::ostringstream tmp;
      tmp << "You have specified explicit hydraulic properties for "
	  << impl->hyd_cells.size () << " out of " << cell_size << " cells";
      msg.warning (tmp.str ());
    }
  
  for (size_t c = 0; c < impl->hyd_cells.size (); c++)
    {
      const Horizon& h = horizon (c);
      const double z = geo.cell_z (c);
      const bool top_soil = z > 30.0;
      impl->hyd_cells[c]->initialize (h.texture (),
				      h.dry_bulk_density (),
				      top_soil,
				      h.CEC (),
				      z,
				      msg);
    }
}

Soil::~Soil ()
{ }

static DeclareSubmodel 
soil_submodel (Soil::load_syntax, "Soil", "\
The soil submodel provides the numeric and physical properties of the soil.");

// soil.C ends here.
