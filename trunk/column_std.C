// column_std.C -- Full Daisy simulation with organic matter and nitrogen.
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
// Copyright 2006 KVL and Per Abrahamsen
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
#include "column.h"
#include "library.h"
#include "surface.h"
#include "soil_heat.h"
#include "macro.h"
#include "movement.h"
#include "groundwater.h"
#include "geometry.h"
#include "soil.h"
#include "soil_water.h"
#include "vegetation.h"
#include "bioclimate.h"
#include "weather.h"
#include "chemistry.h"
#include "chemical.h"
#include "tertiary.h"
#include "organic_matter.h"
#include "im.h"
#include "am.h"
#include "dom.h"
#include "time.h"
#include "units.h"
#include "log.h"
#include "submodeler.h"
#include "memutils.h"
#include "librarian.h"
#include "scope_multi.h"
#include "scopesel.h"

struct ColumnStandard : public Column
{
  static const symbol solute_per_mm_unit;
  static const symbol field_flux_unit;


  const std::auto_ptr<Scopesel> scopesel;
  const Scope* extern_scope;
  std::auto_ptr<Movement> movement;
  std::auto_ptr<Groundwater> groundwater;
  std::auto_ptr<Weather> weather;
  std::auto_ptr<Vegetation> vegetation;
  std::auto_ptr<Bioclimate> bioclimate;
  Surface surface;
  Geometry& geometry;
  std::auto_ptr<Soil> soil;
  std::auto_ptr<SoilWater> soil_water;
  std::auto_ptr<SoilHeat> soil_heat;
  std::auto_ptr<Chemistry> chemistry;
  std::auto_ptr<Tertiary> tertiary;
  std::auto_ptr<OrganicMatter> organic_matter;
  double second_year_utilization_;

  // Log variables.
  double harvest_DM;
  double harvest_N;
  double harvest_C;
  double residuals_DM;
  double residuals_N_top;
  double residuals_C_top;
  std::vector<double> residuals_N_soil;
  std::vector<double> residuals_C_soil;
  double seed_N;
  double seed_C;
  double applied_DM;
  double first_year_utilization;

public:
  const Horizon& horizon_at (double z, double x, double y) const;

  // Actions.
  void sow (Metalib&, const AttributeList&, const double row_width,
            const Time&, const double dt, Treelog&);
  void ridge (const AttributeList& al);
  void irrigate_overhead (double flux, double temp, const IM&, double dt,
			  Treelog& msg);
  void irrigate_surface (double flux, double temp, const IM&, double dt, 
			 Treelog& msg);
  void irrigate_overhead (double flux, const IM&, double dt, Treelog& msg);
  void irrigate_surface (double flux, const IM&, double dt, Treelog& msg);
  void irrigate_subsoil (double flux, const IM& sm, 
                         double from, double to, double dt, Treelog& msg);
  void irrigate_subsoil (double flux, const IM& sm, 
                         const Volume& volume, double dt, Treelog& msg);
  void fertilize (const IM&, double dt, Treelog& msg);
  void fertilize (const AttributeList&, double dt, Treelog& msg);
  void fertilize (const AttributeList&, double from, double to, double dt,
		  Treelog& msg);
  void fertilize (const AttributeList&, const Volume&, double dt,
		  Treelog& msg);
  void clear_second_year_utilization ();
  void emerge (const symbol crop_name, Treelog& msg);
  void harvest (const Time& time, double dt, const symbol crop_name,
                double stub_length, double stem_harvest,
                double leaf_harvest, double sorg_harvest, const bool combine,
                std::vector<const Harvest*>& harvest, Treelog& msg);
  void pluck (const Time& time, double dt, const symbol crop_name,
              const double stem_harvest,
              const double leaf_harvest,
              const double sorg_harvest,
              std::vector<const Harvest*>& harvest, 
              Treelog& msg);

  void add_residuals (std::vector<AM*>& residuals);
  void mix (double from, double to, double penetration, 
            const Time&, double dt, Treelog&);
  void swap (double from, double middle, double to, 
             const Time&, double dt, Treelog&);
  void set_porosity (double at, double Theta);
  void set_heat_source (double at, double value); // [W/m^2]
  void spray (symbol chemical, double amount, double dt, Treelog&); // [g/ha]
  void set_surface_detention_capacity (double height); // [mm]

  // Conditions.
  double daily_air_temperature () const; // [dg C]
  double daily_precipitation () const; // [mm]
  double daily_global_radiation () const; // [W/m^2]
  double soil_temperature (double height) const; // [cm -> dg C]
  double soil_water_potential (double height) const; // [cm -> cm]
  double soil_water_content (double from, double to) const; // [cm]
  double soil_inorganic_nitrogen (double from, double to) const; // [kg N/ha]
  double second_year_utilization () const;// [kg N/ha]
  double crop_ds (symbol crop) const; 
  double crop_dm (symbol crop, double height) const; 
  double crop_sorg_dm (const symbol name) const;
  std::string crop_names () const;
  double bottom () const;

  // Simulation.
  void clear ();
  void tick (const Time&, const double dt, const Weather*,
	     const Scope&, Treelog&);
  bool check (bool require_weather,
              const Time& from, const Time& to, 
              const Scope&, Treelog&) const;
  bool check_am (const AttributeList& am, Treelog&) const;
  bool check_z_border (double, Treelog&) const;
  bool check_x_border (double, Treelog&) const;
  bool check_y_border (double, Treelog&) const;
  void output (Log&) const;

  // Create and Destroy.
  static Movement* build_vertical (Block& al);
  ColumnStandard (Block& al);
  bool initialize (Block&, const Output&, const Time&, const Weather*,
		   const Scope& scope);
  ~ColumnStandard ();
};

const symbol 
ColumnStandard::solute_per_mm_unit ("g/cm^2/mm");

const symbol
ColumnStandard::field_flux_unit ("kg/ha/h");

const Horizon& 
ColumnStandard::horizon_at (const double z, 
                            const double x, const double y) const
{ return soil->horizon (geometry.cell_at (z, x, y)); }

void 
ColumnStandard::sow (Metalib& metalib, const AttributeList& al, 
                     const double row_width, const Time& time,
                     const double dt, Treelog& msg)
{ vegetation->sow (metalib, al, row_width, 
                   geometry, *organic_matter, -soil->MaxRootingHeight (),
                   seed_N, seed_C, time, dt, msg); }


void 
ColumnStandard::ridge (const AttributeList& al)
{ movement->ridge (surface, *soil, *soil_water, al); }

void 
ColumnStandard::irrigate_overhead (const double flux, const double temp,
                                   const IM& sm, const double dt, Treelog& msg)
{
  daisy_assert (flux >= 0.0);
  bioclimate->irrigate_overhead (flux, temp);
  IM im (solute_per_mm_unit, sm);
  im *= Scalar (flux * dt, Units::mm ());
  fertilize (im, dt, msg);
}

void 
ColumnStandard::irrigate_surface (const double flux, const double temp, 
                                  const IM& sm, const double dt, Treelog& msg)
{
  daisy_assert (flux >= 0.0);
  bioclimate->irrigate_surface (flux, temp);
  IM im (solute_per_mm_unit, sm);
  im *= Scalar (flux * dt, Units::mm ());
  fertilize (im, dt, msg);
}

void 
ColumnStandard::irrigate_overhead (const double flux,
                                   const IM& sm, const double dt, Treelog& msg)
{
  daisy_assert (flux >= 0.0);
  bioclimate->irrigate_overhead (flux);
  IM im (solute_per_mm_unit, sm);
  im *= Scalar (flux * dt, Units::mm ());
  fertilize (im, dt, msg);
}

void 
ColumnStandard::irrigate_surface (const double flux, 
                                  const IM& sm, const double dt, Treelog& msg)
{
  daisy_assert (flux >= 0.0);
  bioclimate->irrigate_surface (flux);
  IM im (solute_per_mm_unit, sm);
  im *= Scalar (flux * dt, Units::mm ());
  fertilize (im, dt, msg);
}

void
ColumnStandard::irrigate_subsoil (const double flux, const IM& sm, 
                                  const double from, const double to, 
                                  const double dt, Treelog& msg)
{
  soil_water->incorporate (geometry, flux / 10.0 /* mm -> cm */, from, to);
  bioclimate->irrigate_subsoil (flux);
  IM im (solute_per_mm_unit, sm);
  im *= Scalar (flux * dt, Units::mm ());
  chemistry->incorporate (geometry, im, from, to, dt, msg);
}

void
ColumnStandard::irrigate_subsoil (const double flux, const IM& sm, 
                                  const Volume& volume, 
                                  const double dt, Treelog& msg)
{
  soil_water->incorporate (geometry, flux / 10.0 /* mm -> cm */, volume);
  bioclimate->irrigate_subsoil (flux);
  IM im (solute_per_mm_unit, sm);
  im *= Scalar (flux * dt, Units::mm ());
  chemistry->incorporate (geometry, im, volume, dt, msg);
}

void
ColumnStandard::fertilize (const IM& im, const double dt, Treelog& msg)
{ chemistry->spray (im, dt, msg); }

void
ColumnStandard::fertilize (const AttributeList& al, const double dt,
			   Treelog& msg)
{
  // Utilization log.
  first_year_utilization += AM::utilized_weight (al);
  second_year_utilization_ += AM::second_year_utilization (al);

  // Volatilization.
  const double lost_NH4 = AM::get_volatilization (al);
  chemistry->dissipate (Chemical::NH4 (), lost_NH4, dt, msg);

  // Add inorganic matter.
  fertilize (AM::get_IM (al), dt, msg);

  // Add organic matter, if any.
  if (al.name ("syntax") != "mineral")
    organic_matter->fertilize (al, geometry, dt);
  applied_DM += AM::get_DM (al) / dt;
}

void 
ColumnStandard::fertilize (const AttributeList& al, 
                           const double from, const double to, const double dt,
			   Treelog& msg)
{
  daisy_assert (to < from);

  // Utilization log.
  first_year_utilization += AM::utilized_weight (al);
  second_year_utilization_ += AM::second_year_utilization (al);

  // Volatilization.
  const double lost_NH4 = AM::get_volatilization (al);
  chemistry->dissipate (Chemical::NH4 (), lost_NH4, dt, msg);

  // Add inorganic matter.
  const IM im = AM::get_IM (al);
  chemistry->incorporate (geometry, im, from, to, dt, msg);
  applied_DM += AM::get_DM (al) / dt;

  // Add organic matter, if any.
  if (al.name ("syntax") != "mineral")
    organic_matter->fertilize (al, geometry, from, to, dt);
}

void 
ColumnStandard::fertilize (const AttributeList& al, 
                           const Volume& volume, const double dt,
			   Treelog& msg)
{
  // Utilization log.
  first_year_utilization += AM::utilized_weight (al);
  second_year_utilization_ += AM::second_year_utilization (al);

  // Volatilization.
  const double lost_NH4 = AM::get_volatilization (al);
  chemistry->dissipate (Chemical::NH4 (), lost_NH4, dt, msg);

  // Add inorganic matter.
  const IM im = AM::get_IM (al);
  chemistry->incorporate (geometry, im, volume, dt, msg);
  applied_DM += AM::get_DM (al) / dt;

  // Add organic matter, if any.
  if (al.name ("syntax") != "mineral")
    organic_matter->fertilize (al, geometry, volume, dt);
}

void 
ColumnStandard::clear_second_year_utilization ()
{ second_year_utilization_ = 0.0; }

void
ColumnStandard::emerge (const symbol crop_name, Treelog& msg)
{ vegetation->emerge (crop_name, msg); }

void
ColumnStandard::harvest (const Time& time, const double dt,
                         const symbol crop_name,
                         const double stub_length,
                         const double stem_harvest,
                         const double leaf_harvest, 
                         const double sorg_harvest,
                         const bool combine,
                         std::vector<const Harvest*>& harvest, Treelog& msg)
{ 
  const double old_LAI = vegetation->LAI ();
  std::vector<AM*> residuals;
  double min_height = 100.0;
  vegetation->harvest (name, crop_name, time, geometry,
                       stub_length, 
                       stem_harvest, leaf_harvest, sorg_harvest,
                       harvest, min_height, 
                       harvest_DM, harvest_N, harvest_C, residuals, 
                       residuals_DM, residuals_N_top, residuals_C_top,
                       residuals_N_soil, residuals_C_soil,
                       combine, msg); 
  add_residuals (residuals);
  if (min_height < 0.0)
    mix (0.0, min_height, 0.0, time, dt, msg);

  // Chemicals removed by harvest.  BUG: We assume chemicals are above stub.
  const double new_LAI = vegetation->LAI ();
  if (new_LAI < 1e-5)
    chemistry->harvest (1.0, 1.0 - leaf_harvest, dt);
  else if (new_LAI < old_LAI)
    {
      const double removed_fraction = (old_LAI - new_LAI) / old_LAI;
      chemistry->harvest (removed_fraction, 1.0 - leaf_harvest, dt);
    }
}

void 
ColumnStandard::pluck (const Time& time, double dt, const symbol crop_name,
                       const double stem_harvest,
                       const double leaf_harvest,
                       const double sorg_harvest,
                       std::vector<const Harvest*>& harvest, 
                       Treelog& msg)
{ 
  const double old_LAI = vegetation->LAI ();
  std::vector<AM*> residuals;
  vegetation->pluck (name, crop_name, time, geometry,
                     stem_harvest, leaf_harvest, sorg_harvest,
                     harvest, harvest_DM, harvest_N, harvest_C, 
                     residuals, residuals_DM, residuals_N_top, residuals_C_top,
                     residuals_N_soil, residuals_C_soil,msg); 
  add_residuals (residuals);

  // Chemicals removed by harvest.  BUG: We assume chemicals are above stub.
  const double new_LAI = vegetation->LAI ();
  if (new_LAI < 1e-5)
    chemistry->harvest (1.0, 0.0, dt);
  else if (new_LAI < old_LAI)
    {
      const double removed_fraction = (old_LAI - new_LAI) / old_LAI;
      chemistry->harvest (removed_fraction, 0.0, dt);
    }
}

void
ColumnStandard::add_residuals (std::vector<AM*>& residuals)
{
  // Put the residuals in the soil.
  for (std::vector<AM*>::iterator residual = residuals.begin ();
       residual != residuals.end ();
       residual++)
    organic_matter->add (*(*residual));
}

void 
ColumnStandard::mix (const double from, const double to,
                     const double penetration, 
                     const Time& time, const double dt,
                     Treelog& msg)
{
  std::vector<AM*> residuals;
  vegetation->kill_all (name, time, geometry, residuals, 
                        residuals_DM, residuals_N_top, residuals_C_top, 
                        residuals_N_soil, residuals_C_soil, msg);
  add_residuals (residuals);
  const double energy 
    = soil_heat->energy (geometry, *soil, *soil_water, from, to);
  soil_water->mix (geometry, *soil, *soil_heat, from, to, dt, msg);
  soil_heat->set_energy (geometry, *soil, *soil_water, from, to, energy);
  chemistry->mix (geometry, *soil, *soil_water, from, to, penetration, dt);
  surface.unridge ();
  organic_matter->mix (geometry, *soil, *soil_water, from, to, penetration, 
                       time, dt);
}

void 
ColumnStandard::swap (const double from, const double middle, const double to,
		      const Time& time, const double dt, Treelog& msg)
{
  mix (from, middle, 1.0, time, dt, msg);
  mix (middle, to, 0.0, time, dt, msg);
  soil_water->swap (geometry, *soil, *soil_heat, from, middle, to, dt, msg);
  soil_heat->swap (geometry, from, middle, to);
  chemistry->swap (geometry, *soil, *soil_water, from, middle, to, dt);
  organic_matter->swap (geometry, *soil, *soil_water, from, middle, to, 
                        time, dt);
}

void 
ColumnStandard::set_porosity (double at, double Theta)
{ 
  const size_t cell_size = geometry.cell_size ();
  for (size_t i = 0; i < cell_size; i++)
    if (geometry.contain_z (i, at))
      soil->set_porosity (i, Theta); 
}

void 
ColumnStandard::set_heat_source (double at, double value) // [W/m^2]
{
 const size_t cell_size = geometry.cell_size ();
 for (size_t i = 0; i < cell_size; i++)
   if (geometry.contain_z (i, at))
     {
       const double V = geometry.cell_volume (i);
       value *= 10^3;		// [W/m^2] -> [erg/cm^2/s]
       value *= 3600;		// [erg/cm^2/s] -> [erg/cm^2/h]
       value /= V;              // [erg/cm^2/h] -> [erg/cm^3/h]

       soil_heat->set_source (i, value);
     }
}

void 
ColumnStandard::spray (const symbol chemical, 
                       const double amount /* [g/ha] */,
                       const double dt, Treelog& msg)
{
  chemistry->spray (chemical, amount / (100.0 * 100.0 /* ha->m^2 */),
                    dt, msg); 
}

void 
ColumnStandard::set_surface_detention_capacity (double height) // [mm]
{ surface.set_detention_capacity (height); }

double 
ColumnStandard::daily_air_temperature () const
{ return bioclimate->daily_air_temperature (); }

double 
ColumnStandard::daily_precipitation () const
{ return bioclimate->daily_precipitation (); }

double 
ColumnStandard::daily_global_radiation () const
{ return bioclimate->daily_global_radiation (); }

double 
ColumnStandard::soil_temperature (double height) const
{ return geometry.content_at (static_cast<const SoilHeat&> (*soil_heat),
                               &SoilHeat::T, height); }

double 
ColumnStandard::soil_water_potential (double height) const
{ return geometry.content_at (static_cast<const SoilWater&> (*soil_water), 
                               &SoilWater::h, height); }

double 
ColumnStandard::soil_water_content (double from, double to) const
{
  daisy_assert (to <= from);
  daisy_assert (to <= 0.0);
  daisy_assert (to > geometry.cell_z (soil->size () - 1));
  return soil_water->content_surface (geometry, from, to);
}

double				// [kg N/ha]
ColumnStandard::soil_inorganic_nitrogen (double from, double to) const
{
  double N = 0.0;
  if (chemistry->know (Chemical::NH4 ()))
    N += chemistry->find (Chemical::NH4 ())
      /**/ .total_surface (geometry, from, to);
  if (chemistry->know (Chemical::NO3 ()))
    N += chemistry->find (Chemical::NO3 ())
      /**/ .total_surface (geometry, from, to);
  return N * 1.0e5; // g N/cm^2 -> kg N/ha
}  

double				// [kg N/ha]
ColumnStandard::second_year_utilization () const
{ return second_year_utilization_; }

double  
ColumnStandard::crop_ds (const symbol name) const // {[-1:2], Crop::DSremove}
{ return vegetation->DS_by_name (name); }

double 
ColumnStandard::crop_dm (const symbol name, const double height) const
  //[kg/ha], negative when no crop
{ return vegetation->DM_by_name (name, height); }

double 
ColumnStandard::crop_sorg_dm (const symbol name) const
  //[kg/ha], negative when no crop
{ return vegetation->SOrg_DM_by_name (name); }

std::string
ColumnStandard::crop_names () const
{ return vegetation->crop_names (); }

double
ColumnStandard::bottom () const
{ return geometry.bottom (); }

void
ColumnStandard::clear ()
{ 
  soil_water->clear ();
  chemistry->clear ();

  harvest_DM = 0.0;
  harvest_N = 0.0;
  harvest_C = 0.0;
  residuals_DM = 0.0;
  residuals_N_top = 0.0;
  residuals_C_top = 0.0;
  fill (residuals_N_soil.begin (), residuals_N_soil.end (), 0.0);
  fill (residuals_C_soil.begin (), residuals_C_soil.end (), 0.0);

  organic_matter->clear ();

  seed_N = 0.0;
  seed_C = 0.0;
  applied_DM = 0.0;
  first_year_utilization = 0.0;
}

void
ColumnStandard::tick (const Time& time, const double dt,
                      const Weather* global_weather, const Scope& parent_scope,
		      Treelog& msg)
{
  daisy_assert (extern_scope);
  ScopeMulti scope (*extern_scope, parent_scope);

  // Weather.
  if (weather.get ())
    weather->tick (time, msg);

  const Weather& my_weather = weather.get () ? *weather : *global_weather;

#if 1
  tertiary->tick (geometry, *soil, dt, *soil_water, surface, msg);
#else
  movement->macro_tick (*soil, *soil_water, surface, dt, msg);
#endif

  // Early calculation.
  bioclimate->tick (time, surface, my_weather, 
                    *vegetation, *movement,
                    geometry, *soil, *soil_water, *soil_heat, *chemistry,
                    dt, msg);
  vegetation->tick (time, my_weather.relative_humidity (), my_weather.CO2 (),
                    *bioclimate, geometry, *soil, 
		    *organic_matter,
                    *soil_heat, *soil_water, *chemistry,
                    residuals_DM, residuals_N_top, residuals_C_top, 
                    residuals_N_soil, residuals_C_soil, dt, msg);
  chemistry->tick_top (bioclimate->snow_leak_rate (dt), vegetation->cover (),
                       bioclimate->canopy_leak_rate (dt), 
                       surface.runoff_rate (dt), dt, msg);

  // Turnover.
  organic_matter->tick (geometry, *soil_water, *soil_heat, 
                        *chemistry, dt, msg);
  
  // Transport.
  groundwater->tick (geometry, *soil, *soil_water, 
                     surface.ponding () * 0.1, 
                     *soil_heat, time, scope, msg);
  movement->tick (*soil, *soil_water, *soil_heat,
                  surface, *groundwater, time, my_weather, dt, msg);
  soil_heat->tick (geometry, *soil, *soil_water, *movement, 
		   surface, dt, msg);
  soil_water->tick_after (geometry, *soil, *soil_heat, false, msg);
  soil_heat->tick_after (geometry.cell_size (), *soil, *soil_water, msg);
  chemistry->tick_soil (geometry, 
                        surface.ponding (), surface.mixing_resistance (),
                        *soil, *soil_water, *soil_heat, 
                        *movement, *organic_matter, *chemistry,
			*tertiary, dt, scope, msg);
  organic_matter->transport (*soil, *soil_water, *soil_heat, msg);
  const std::vector<DOM*>& dom = organic_matter->fetch_dom ();
  for (size_t i = 0; i < dom.size (); i++)
    {
      movement->element (*soil, *soil_water, dom[i]->C, 
                         dom[i]->diffusion_coefficient, 
                         dt, msg);
      movement->element (*soil, *soil_water, dom[i]->N, 
                         dom[i]->diffusion_coefficient, 
                         dt, msg);
    }
  
  // Once a month we clean up old AM from organic matter.
  if (time.hour () == 13 && time.mday () == 13)
    organic_matter->monthly (geometry);
}

bool
ColumnStandard::check (bool require_weather,
                       const Time& from, const Time& to,
		       const Scope& parent_scope, Treelog& msg) const
{
  bool ok = true;
  const int n = geometry.cell_size ();

  if (!extern_scope)
    {
      msg.error ("Extern scope not found");
      ok = false;
    }
  ScopeMulti scope (extern_scope ? *extern_scope : Scope::null (), 
		    parent_scope);
  {
    Treelog::Open nest (msg, "Geometry");
    if (!geometry.check (msg))
      ok = false;
  }
  {
    Treelog::Open nest (msg, "SoilHeat");
    if (!soil_heat->check (n, msg))
      ok = false;
  }
  {
    Treelog::Open nest (msg, "Movement: " + movement->name);
    if (!movement->check (msg))
      ok = false;
  }
  {
    Treelog::Open nest (msg, "Groundwater: " + groundwater->name);
    if (!groundwater->check (geometry, scope, msg))
      ok = false;
  }
  {
    if (weather.get ())
      {
        Treelog::Open nest (msg, "Weather: " + weather->name);
	if (!weather->check (from, to, msg))
	  ok = false;
      }

    else if (require_weather)
      {
	msg.entry ("Weather unspecified");
	// The rest is uninitialized, don't check it!
	return false;
      }
  }
  {
    Treelog::Open nest (msg, "Chemistry");
    if (!chemistry->check (geometry, *soil, *soil_water, *soil_heat, *chemistry,
			   scope, msg))
      ok = false;
  }
  {
    Treelog::Open nest (msg, "Tertiary");
    if (!tertiary->check (geometry, msg))
      ok = false;
  }
  {
    Treelog::Open nest (msg, "Vegetation");
    if (!vegetation->check (msg))
      ok = false;
  }
  {
    Treelog::Open nest (msg, "Soil");
    if (!soil->check (organic_matter->som_pools (), geometry, msg))
      ok = false;
  }
  if (!organic_matter->check (*soil, *soil_water, *soil_heat, *chemistry, msg))
    ok = false;
  return ok;
}

bool 
ColumnStandard::check_am (const AttributeList& am, Treelog& msg) const 
{ 
  Treelog::Open nest (msg, name);
  return organic_matter->check_am (am, msg); 
}

bool 
ColumnStandard::check_z_border (const double value, Treelog& msg) const
{ 
  Treelog::Open nest (msg, "column: " + name);

  bool ok = true;
  if (!soil->check_z_border (value, msg))
    ok = false; 
  if (!geometry.check_z_border (value, msg))
    ok = false; 
  return ok;
}

bool 
ColumnStandard::check_x_border (const double value, Treelog& msg) const
{ 
  Treelog::Open nest (msg, "column: " + name);

  bool ok = true;
  if (!soil->check_x_border (value, msg))
    ok = false; 
  if (!geometry.check_x_border (value, msg))
    ok = false; 
  return ok;
}

bool 
ColumnStandard::check_y_border (const double value, Treelog& msg) const
{ 
  Treelog::Open nest (msg, "column: " + name);

  bool ok = true;
  if (!soil->check_y_border (value, msg))
    ok = false; 
  if (!geometry.check_y_border (value, msg))
    ok = false; 
  return ok;
}

void
ColumnStandard::output (Log& log) const
{
  Log::Geo geo (log, geometry, *soil, *vegetation);
  Column::output (log);
  if (weather.get ())
    output_derived (weather, "weather", log);
  output_object (bioclimate, "Bioclimate", log);
  output_submodule (surface, "Surface", log);
  // output_submodule (geometry, "Geometry", log);
  output_submodule (*soil, "Soil", log);
  output_submodule (*soil_water, "SoilWater", log);
  output_submodule (*soil_heat, "SoilHeat", log);
  output_derived (chemistry, "Chemistry", log);
  output_derived (tertiary, "Tertiary", log);
  output_derived (vegetation, "Vegetation", log);
  output_derived (organic_matter, "OrganicMatter", log);
  output_value (second_year_utilization_, "second_year_utilization", log);
  output_variable (seed_N, log);
  output_variable (seed_C, log);
  output_variable (applied_DM, log);
  output_variable (first_year_utilization, log);
  output_value (harvest_DM, "harvest_DM", log);
  output_value (harvest_N, "harvest_N", log);
  output_value (harvest_C, "harvest_C", log);
  output_value (residuals_DM, "residuals_DM", log);
  output_value (residuals_N_top, "residuals_N_top", log);
  output_value (residuals_C_top, "residuals_C_top", log);
  output_value (residuals_N_soil, "residuals_N_soil", log);
  output_value (residuals_C_soil, "residuals_C_soil", log);

  const double m2_per_cm2 = 0.0001;
  const double cm2_per_m2 = 1.0 / m2_per_cm2;
  output_lazy (geometry.total_surface (residuals_N_soil) * cm2_per_m2,
               "residuals_N_root", log);
  output_lazy (geometry.total_surface (residuals_C_soil) * cm2_per_m2,
               "residuals_C_root", log);
  output_lazy (bioclimate->get_intercepted_water ()
               + bioclimate->get_snow_storage ()
               + surface.ponding (),
               "surface_water", log);
  output_derived (movement, "Movement", log);
  output_derived (groundwater, "Groundwater", log);
}

ColumnStandard::ColumnStandard (Block& al)
  : Column (al),
    scopesel (Librarian::build_item<Scopesel> (al, "scope")),
    extern_scope (NULL),
    movement (Librarian::build_item<Movement> (al, "Movement")),
    groundwater (Librarian::build_item<Groundwater> (al, "Groundwater")),
    weather (al.check ("weather") 
	     ? Librarian::build_item<Weather> (al, "weather")
	     : NULL), 
    vegetation (Librarian::build_item<Vegetation> (al, "Vegetation")),
    bioclimate (Librarian::build_item<Bioclimate> (al, "Bioclimate")),
    surface (al.alist ("Surface")),
    geometry (movement->geometry ()),
    soil (submodel<Soil> (al, "Soil")),
    soil_water (submodel<SoilWater> (al, "SoilWater")),
    soil_heat (submodel<SoilHeat> (al, "SoilHeat")),
    chemistry (Librarian::build_item<Chemistry> (al, "Chemistry")),
    tertiary (Librarian::build_item<Tertiary> (al, "Tertiary")),
    organic_matter (Librarian::build_item<OrganicMatter> 
                    (al, "OrganicMatter")),
    second_year_utilization_ (al.number ("second_year_utilization")),
    harvest_DM (0.0),
    harvest_N (0.0),
    harvest_C (0.0),
    residuals_DM (0.0),
    residuals_N_top (0.0),
    residuals_C_top (0.0),
    seed_N (0.0),
    seed_C (0.0),
    applied_DM (0.0),
    first_year_utilization (0.0)
{ }

bool
ColumnStandard::initialize (Block& block, 
                            const Output& output,
                            const Time& time, 
			    const Weather* global_weather,
			    const Scope& parent_scope)
{
  bool ok = true;
  Treelog& msg = block.msg ();
  Treelog::Open nest (msg, name);
  extern_scope = scopesel->lookup (output, msg); 
  ScopeMulti scope (extern_scope ? *extern_scope : Scope::null (),
		    parent_scope);
  soil->initialize (block, geometry, *groundwater,
                    organic_matter->som_pools ());
  residuals_N_soil.insert (residuals_N_soil.begin (), soil->size (), 0.0);
  daisy_assert (residuals_N_soil.size () == soil->size ());
  residuals_C_soil.insert (residuals_C_soil.begin (), soil->size (), 0.0);
  daisy_assert (residuals_C_soil.size () == soil->size ());

  groundwater->initialize (geometry, time, scope, msg);

  // Tertiary transport depends on groundwater and soil.
  const double pipe_position = groundwater->is_pipe ()
    ? groundwater->pipe_height ()
    : 42.42e42;
  if (!tertiary->initialize (geometry, *soil, scope, pipe_position, msg))
    ok = false;
  
  // Movement depends on soil, groundwater, and tertiary.
  movement->initialize (*soil, *groundwater,  tertiary->has_macropores (),
                        msg);

  surface.initialize (geometry);

  // Bioclimate and heat depends on weather.
  if (weather.get () && !weather->initialize (time, msg))
    return false;
  if (!global_weather && !weather.get ())
    {
      msg.error ("No weather specified");
      return false;
    }
  const Weather& my_weather = weather.get () ? *weather : *global_weather;
  bioclimate->initialize (block, my_weather);
  soil_heat->initialize (alist.alist ("SoilHeat"), geometry, 
                         movement->default_heat (*soil, time, my_weather),
                         msg);

  soil_water->initialize (alist.alist ("SoilWater"), 
                          geometry, *soil, *soil_heat, *groundwater, msg);
  
  // Solutes depends on water and heat.
  chemistry->initialize (alist.alist ("Chemistry"),
                         geometry, *soil, *soil_water, *soil_heat, msg);
  
  // Organic matter and vegetation.
  const double T_avg = my_weather.average_temperature ();
  organic_matter->initialize (alist.alist ("OrganicMatter"), 
                              geometry, *soil, *soil_water, T_avg, msg);
  vegetation->initialize (time, geometry, *soil, *organic_matter, msg);
  
  // Soil conductivity and capacity logs.
  soil_heat->tick_after (geometry.cell_size (), *soil, *soil_water, msg);

  return ok;
}

ColumnStandard::~ColumnStandard ()
{ }

static struct ColumnStandardSyntax
{
  static Model& make (Block& al)
  { return *new ColumnStandard (al); }

  static void load_water_and_macro (Syntax& syntax, AttributeList& alist)
  {
    SoilWater::load_syntax (syntax, alist);
    syntax.add_object ("macro", Macro::component,
                       Syntax::OptionalState, Syntax::Singleton,
                       "Preferential flow model.\n\
By default, preferential flow is enabled if and only if the combined\n\
amount of humus and clay in the top horizon is above 5%.");
  }

  ColumnStandardSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    Column::load_syntax (syntax, alist);
    syntax.add ("description", Syntax::String, Syntax::OptionalConst,
		"Description of this column."); 
    alist.add ("description", "\
Hansen et.al. 1990. with generic movement in soil.");

    syntax.add_object ("scope", Scopesel::component, 
		       Syntax::Const, Syntax::Singleton, "\
Scope to evaluate expessions in.");
    alist.add ("scope", Scopesel::default_model ());
    syntax.add_submodule ("Soil", alist, Syntax::State,
                          "The numeric and physical soil properties.",
                          Soil::load_syntax);
    syntax.add_submodule ("SoilWater", alist, Syntax::State,
                          "Soil water content and transportation.",
                          load_water_and_macro);
    syntax.add_submodule ("SoilHeat", alist, Syntax::State,
                          "Soil heat capacity and transportation.",
                          SoilHeat::load_syntax);
    syntax.add_object ("Movement", Movement::component,
                       Syntax::State, Syntax::Singleton, "\
Discretization and movement of water, heat and solutes in the soil.");
    alist.add ("Movement", Movement::default_model ());
    syntax.add_object ("weather", Weather::component,
                       Syntax::OptionalState, Syntax::Singleton,
                       "Weather model for providing climate information during\n\
the simulation.  If unspecified, used global weather.");
    syntax.add_object ("Vegetation", Vegetation::component,
                       Syntax::State, Syntax::Singleton,
                       "The crops on the field.");
    AttributeList vegetation_alist;
    vegetation_alist.add ("type", "crops");
    vegetation_alist.add ("crops", std::vector<const AttributeList*> ());
    vegetation_alist.add ("ForcedLAI", std::vector<const AttributeList*> ());
    vegetation_alist.add ("EpInterchange", 0.6);
    alist.add ("Vegetation", vegetation_alist);

    syntax.add_object ("Bioclimate", Bioclimate::component, 
                       Syntax::State, Syntax::Singleton,
                       "The water and energy distribution among the crops.");
    alist.add ("Bioclimate", Bioclimate::default_model ());
    syntax.add_submodule ("Surface", alist, Syntax::State,
                          "The upper border of the soil.",
                          Surface::load_syntax);
    syntax.add_object ("Groundwater", Groundwater::component,
                       "The groundwater level.");
    syntax.add_object ("Chemistry", Chemistry::component, 
                       Syntax::State, Syntax::Singleton,
                       "Chemical compounds in the system.");
    alist.add ("Chemistry", Chemistry::default_model ());
    syntax.add_object ("Tertiary", Tertiary::component, "\
Tertiary (that is, non-matrix) transport method.");
    alist.add ("Tertiary", Tertiary::none_model ());
    syntax.add ("harvest_DM", "g/m^2/h", Syntax::LogOnly, 
                "Amount of DM removed by harvest this hour.");
    syntax.add ("harvest_N", "g/m^2/h", Syntax::LogOnly, 
                "Amount of nitrogen removed by harvest this hour.");
    syntax.add ("harvest_C", "g/m^2/h", Syntax::LogOnly, 
                "Amount of carbon removed by harvest this hour.");
    syntax.add ("residuals_DM", "g/m^2/h", Syntax::LogOnly, "\
Amount of dry matter removed from crops to surface and soil this hour.\n\
This includes loss as harvest, as well as loss of old leaves and roots.");
    syntax.add ("residuals_N_top", "g/m^2/h", Syntax::LogOnly, 
                "Amount of nitrogen removed from crops to soil this hour.\n\
This includes loss as harvest, as well as loss of old leaves.");
    syntax.add ("residuals_C_top", "g/m^2/h", Syntax::LogOnly, 
                "Amount of carbon removed from crops to surface this hour.\n\
This includes loss as harvest, as well as loss of old leaves.");
    syntax.add ("residuals_N_soil", "g/cm^3/h", Syntax::LogOnly, Syntax::Sequence, 
                "Amount of nitrogen removed from crops in soil this hour.\n\
This includes loss as harvest, as well as loss of old roots.");
    syntax.add ("residuals_C_soil", "g/cm^3/h", Syntax::LogOnly, Syntax::Sequence, 
                "Amount of carbon removed from crops in soil this hour.\n\
This includes loss as harvest, as well as loss of old roots.");
    syntax.add ("residuals_N_root", "g/m^2/h", Syntax::LogOnly, 
                "Amount of nitrogen removed from crops to soil this hour.\n\
This includes loss as harvest, as well as loss of old roots.");
    syntax.add ("residuals_C_root", "g/m^2/h", Syntax::LogOnly, 
                "Amount of carbon removed from crops to surface this hour.\n\
This includes loss as harvest, as well as loss of old roots.");
    syntax.add ("surface_water", "mm", Syntax::LogOnly, 
                "Amount of water in the system above ground.\n\
This include ponded water, intercepted water and the snow pack.");
    syntax.add_object ("OrganicMatter", OrganicMatter::component,
                       Syntax::State, Syntax::Singleton, "\
The organic matter in the soil and on the surface.");
    alist.add ("OrganicMatter", OrganicMatter::default_model ());
    syntax.add ("second_year_utilization", "kg N/ha", Syntax::State,
		"Estimated accumulated second year fertilizer effect.");
    alist.add ("second_year_utilization", 0.0);
    syntax.add ("seed_N", "kg N/ha/h", Syntax::LogOnly,
		"Amount of nitrogen in seed applied this time step.");
    syntax.add ("seed_C", "kg C/ha/h", Syntax::LogOnly,
		"Amount of carbon in seed applied this time step.");
    syntax.add ("applied_DM", "ton DM/ha/h", Syntax::LogOnly,
		"Amount of dry matter applied this time step.\n\
This includes dry matter incorporated directly in the soil.");
    syntax.add ("first_year_utilization", "kg N/ha/h", Syntax::LogOnly,
		"Estimated first year fertilizer effect.");
    
    Librarian::add_type (Column::component, "default", alist, syntax, &make);
  }
} column_syntax;
