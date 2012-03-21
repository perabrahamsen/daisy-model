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
#include "movement.h"
#include "drain.h"
#include "groundwater.h"
#include "geometry.h"
#include "soil.h"
#include "soil_water.h"
#include "vegetation.h"
#include "litter.h"
#include "bioclimate.h"
#include "wsource.h"
#include "chemistry.h"
#include "chemical.h"
#include "organic.h"
#include "im.h"
#include "am.h"
#include "dom.h"
#include "irrigate.h"
#include "time.h"
#include "log.h"
#include "submodeler.h"
#include "memutils.h"
#include "scope_multi.h"
#include "scopesel.h"
#include "units.h"
#include "treelog.h"
#include "librarian.h"
#include "assertion.h"
#include "frame_model.h"
#include "block_model.h"
#include "mathlib.h"
#include <sstream>

struct ColumnStandard : public Column
{
  static const symbol solute_per_mm_unit;
  static const symbol field_flux_unit;
  
  const Units& units;
  const std::auto_ptr<Scopesel> scopesel;
  const Scope* extern_scope;
  std::auto_ptr<Movement> movement;
  std::auto_ptr<Drain> drain;
  std::auto_ptr<Groundwater> groundwater;
  std::auto_ptr<WSource> weather;
  std::auto_ptr<Vegetation> vegetation;
  std::auto_ptr<Litter> litter;
  std::auto_ptr<Bioclimate> bioclimate;
  Surface surface;
  Geometry& geometry;
  std::auto_ptr<Soil> soil;
  std::auto_ptr<SoilWater> soil_water;
  std::auto_ptr<SoilHeat> soil_heat;
  std::auto_ptr<Chemistry> chemistry;
  std::auto_ptr<OrganicMatter> organic_matter;
  double second_year_utilization_;
  std::vector<double> tillage_age;
  std::auto_ptr<Irrigation> irrigation;

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

  // Log interface.
  const Geometry& get_geometry () const 
  { return geometry; }
  const Soil& get_soil () const 
  { return *soil; }
  const Vegetation& get_vegetation () const 
  { return *vegetation; }

public:
  const Horizon& horizon_at (double z, double x, double y) const;

  // Actions.
  void sow (const Metalib&, const FrameModel&, 
            double row_width, double row_pos, double seed,
            const Time&, Treelog&);
  void ridge (const FrameSubmodel& al);
  void irrigate (const double duration, const double flux, 
                 const double temp, Irrigation::target_t target,
                 const IM& sm, const boost::shared_ptr<Volume> volume,
                 const bool silence, Treelog& msg);
  void fertilize (const IM&, Treelog& msg);
  void fertilize (const Metalib&, const FrameModel&,
                  const Time&, Treelog& msg);
  void fertilize (const Metalib&, const FrameModel&, double from, double to, 
                  const Time&, Treelog& msg);
  void fertilize (const Metalib&, const FrameModel&, const Volume&, 
                  const Time&, Treelog& msg);
  void clear_second_year_utilization ();
  void emerge (const symbol crop_name, Treelog& msg);
  void harvest (const Metalib&, const Time& time, const symbol crop_name,
                double stub_length, double stem_harvest,
                double leaf_harvest, double sorg_harvest, const bool combine,
                std::vector<const Harvest*>& harvest, Treelog& msg);
  void pluck (const Metalib&, const Time& time, const symbol crop_name,
              const double stem_harvest,
              const double leaf_harvest,
              const double sorg_harvest,
              std::vector<const Harvest*>& harvest, 
              Treelog& msg);

  void add_residuals (std::vector<AM*>& residuals);
  void mix (const Metalib&, double from, double to, double penetration, 
            const Time&, Treelog&);
  void swap (const Metalib&, double from, double middle, double to, 
             const Time&, Treelog&);
  void set_porosity (double at, double Theta, Treelog& msg);
  void overflow (const double extra, Treelog& msg);
  void set_heat_source (double at, double value); // [W/m^2]
  void spray (symbol chemical, double amount, Treelog&); // [g/ha]
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
  void tick_source (const Scope&, const Time&, Treelog&);
  double suggest_dt () const;
  void tick_move (const Metalib& metalib, 
                  const Time&, double dt, const Weather*, 
                  const Scope&, Treelog&);
  bool check (const Weather* global_weather,
              const Time& from, const Time& to, 
              const Scope&, Treelog&) const;
  bool check_am (const FrameModel& am, Treelog&) const;
  bool check_z_border (double, Treelog&) const;
  bool check_x_border (double, Treelog&) const;
  bool check_y_border (double, Treelog&) const;
  void output (Log&) const;

  // Create and Destroy.
  static Movement* build_vertical (const BlockModel& al);
  ColumnStandard (const BlockModel& al);
  bool initialize (const Block&, const std::vector<const Scope*> scopes,
                   const Time&, const Weather*, const Scope& scope);
  void summarize (Treelog& msg) const;
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
ColumnStandard::sow (const Metalib& metalib, const FrameModel& al, 
                     const double row_width, const double row_pos,
                     const double seed,
                     const Time& time, Treelog& msg)
{ vegetation->sow (metalib, al, row_width, row_pos, seed,
                   geometry, *organic_matter, -soil->MaxRootingHeight (),
                   seed_N, seed_C, time, msg); }


void 
ColumnStandard::ridge (const FrameSubmodel& al)
{ movement->ridge (surface, *soil, *soil_water, al); }

void 
ColumnStandard::irrigate (const double duration, const double flux, 
                          const double temp, Irrigation::target_t target,
                          const IM& sm, const boost::shared_ptr<Volume> volume,
                          const bool silence, 
                          Treelog& msg)
{
  irrigation->add (duration, flux, temp, target, sm, volume, silence, msg);
}

void
ColumnStandard::fertilize (const IM& im, Treelog& msg)
{ chemistry->spray (im, msg); }

void
ColumnStandard::fertilize (const Metalib& metalib, const FrameModel& al, 
                           const Time& now, Treelog& msg)
{
  // Utilization log.
  first_year_utilization += AM::utilized_weight (metalib, al);
  second_year_utilization_ += AM::second_year_utilization (metalib, al);

  // Volatilization.
  const double lost_NH4 = AM::get_volatilization (metalib, al);
  chemistry->dissipate (Chemical::NH4 (), lost_NH4, msg);

  // Add inorganic matter.
  fertilize (AM::get_IM (metalib, units.get_unit (IM::storage_unit ()), al),
             msg);

  // Add organic matter, if any.
  if (!AM::is_mineral (metalib, al))
    organic_matter->fertilize (metalib, al, geometry, now, msg);
  applied_DM += AM::get_DM (metalib, al);
}

void 
ColumnStandard::fertilize (const Metalib& metalib, const FrameModel& al, 
                           const double from, const double to, 
                           const Time& now, Treelog& msg)
{
  daisy_assert (to < from);

  // Utilization log.
  first_year_utilization += AM::utilized_weight (metalib, al);
  second_year_utilization_ += AM::second_year_utilization (metalib, al);

  // Volatilization.
  const double lost_NH4 = AM::get_volatilization (metalib, al);
  chemistry->dissipate (Chemical::NH4 (), lost_NH4, msg);

  // Add inorganic matter.
  const IM im = AM::get_IM (metalib, units.get_unit (IM::storage_unit ()), al);
  chemistry->incorporate (geometry, im, from, to, msg);
  applied_DM += AM::get_DM (metalib, al);

  // Add organic matter, if any.
  if (!AM::is_mineral (metalib, al))
    organic_matter->fertilize (metalib, al, geometry, from, to, now, msg);
}

void 
ColumnStandard::fertilize (const Metalib& metalib, const FrameModel& al, 
                           const Volume& volume, const Time& now, Treelog& msg)
{
  // Utilization log.
  first_year_utilization += AM::utilized_weight (metalib, al);
  second_year_utilization_ += AM::second_year_utilization (metalib, al);

  // Volatilization.
  const double lost_NH4 = AM::get_volatilization (metalib, al);
  chemistry->dissipate (Chemical::NH4 (), lost_NH4, msg);

  // Add inorganic matter.
  const IM im = AM::get_IM (metalib, units.get_unit (IM::storage_unit ()), al);
  chemistry->incorporate (geometry, im, volume, msg);
  applied_DM += AM::get_DM (metalib, al);

  // Add organic matter, if any.
  if (!AM::is_mineral (metalib, al))
    organic_matter->fertilize (metalib, al, geometry, volume, now, msg);
}

void 
ColumnStandard::clear_second_year_utilization ()
{ second_year_utilization_ = 0.0; }

void
ColumnStandard::emerge (const symbol crop_name, Treelog& msg)
{ vegetation->emerge (crop_name, msg); }

void
ColumnStandard::harvest (const Metalib& metalib, const Time& time,
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
  vegetation->harvest (metalib, objid, crop_name, time, geometry,
                       stub_length, 
                       stem_harvest, leaf_harvest, sorg_harvest,
                       harvest, min_height, 
                       harvest_DM, harvest_N, harvest_C, residuals, 
                       residuals_DM, residuals_N_top, residuals_C_top,
                       residuals_N_soil, residuals_C_soil,
                       combine, msg); 
  add_residuals (residuals);
  if (min_height < 0.0)
    mix (metalib, 0.0, min_height, 0.0, time, msg);

  // Chemicals removed by harvest.  BUG: We assume chemicals are above stub.
  const double new_LAI = vegetation->LAI ();
  const double to_surface = 1.0 - leaf_harvest;
  if (new_LAI < 1e-5)
    chemistry->harvest (1.0, to_surface);
  else if (new_LAI < old_LAI)
    {
      const double removed_fraction = (old_LAI - new_LAI) / old_LAI;
      chemistry->harvest (removed_fraction, to_surface);
    }
}

void 
ColumnStandard::pluck (const Metalib& metalib, 
                       const Time& time, const symbol crop_name,
                       const double stem_harvest,
                       const double leaf_harvest,
                       const double sorg_harvest,
                       std::vector<const Harvest*>& harvest, 
                       Treelog& msg)
{ 
  const double old_LAI = vegetation->LAI ();
  std::vector<AM*> residuals;
  vegetation->pluck (metalib, objid, crop_name, time, geometry,
                     stem_harvest, leaf_harvest, sorg_harvest,
                     harvest, harvest_DM, harvest_N, harvest_C, 
                     residuals, residuals_DM, residuals_N_top, residuals_C_top,
                     residuals_N_soil, residuals_C_soil,msg); 
  add_residuals (residuals);

  // Chemicals removed by harvest.  BUG: We assume chemicals are above stub.
  const double new_LAI = vegetation->LAI ();
  const double to_surface = 0.0;
  if (new_LAI < 1e-5)
    chemistry->harvest (1.0, to_surface);
  else if (new_LAI < old_LAI)
    {
      const double removed_fraction = (old_LAI - new_LAI) / old_LAI;
      chemistry->harvest (removed_fraction, to_surface);
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
  litter->update (organic_matter->top_DM ());
}

void 
ColumnStandard::mix (const Metalib& metalib, const double from, const double to,
                     const double penetration, const Time& time, Treelog& msg)
{
  std::vector<AM*> residuals;
  vegetation->kill_all (metalib, objid, time, geometry, residuals, 
                        residuals_DM, residuals_N_top, residuals_C_top, 
                        residuals_N_soil, residuals_C_soil, msg);
  add_residuals (residuals);
  const double energy 
    = soil_heat->energy (geometry, *soil, *soil_water, from, to);
  const double extra 
    = soil_water->mix (geometry, *soil, *soil_heat, from, to, msg);
  overflow (extra, msg);
  soil_heat->set_energy (geometry, *soil, *soil_water, from, to, energy);
  chemistry->mix (geometry, *soil, *soil_water, from, to, penetration);
  surface.unridge ();
  organic_matter->mix (geometry, *soil, *soil_water, from, to, penetration, 
                       time);
  litter->update (organic_matter->top_DM ());
  // Reset tillage age.
  for (size_t i = 0; i < tillage_age.size (); i++)
    {
      const double z = geometry.cell_z (i);
      if (z > to && z < from)
	tillage_age[i] = 0.0;
    }
}

void 
ColumnStandard::swap (const Metalib& metalib, 
                      const double from, const double middle, const double to,
                      const Time& time, Treelog& msg)
{
  mix (metalib, from, middle, 1.0, time, msg);
  mix (metalib, middle, to, 0.0, time, msg);
  const double extra 
    = soil_water->swap (geometry, *soil, *soil_heat, from, middle, to, msg);
  overflow (extra, msg);
  soil_heat->swap (geometry, from, middle, to);
  chemistry->swap (geometry, *soil, *soil_water, from, middle, to);
  organic_matter->swap (geometry, *soil, *soil_water, from, middle, to, 
                        time);
}

void 
ColumnStandard::set_porosity (const double at, const double Theta, Treelog& msg)
{ 
  const size_t cell_size = geometry.cell_size ();
  for (size_t i = 0; i < cell_size; i++)
    if (geometry.contain_z (i, at))
      soil->set_porosity (i, Theta); 
  const double extra = soil_water->overflow (geometry, *soil, *soil_heat, msg);
  overflow (extra, msg);
  chemistry->update_C (*soil, *soil_water);
}

void
ColumnStandard::overflow (const double extra, Treelog& msg)
{
  if (extra > 0.0)
    {
      std::ostringstream tmp;
      tmp << "Adding " << extra << " [mm] overflow to surface";
      msg.message (tmp.str ());
      bioclimate->add_tillage_water (extra);
    }
}

void 
ColumnStandard::set_heat_source (double at, double value) // [W/m^2]
{
  const size_t cell_size = geometry.cell_size ();
  for (size_t i = 0; i < cell_size; i++)
    if (geometry.contain_z (i, at))
      {
        const double V = geometry.cell_volume (i);
        value *= 10^3;          // [W/m^2] -> [erg/cm^2/s]
        value *= 3600;          // [erg/cm^2/s] -> [erg/cm^2/h]
        value /= V;              // [erg/cm^2/h] -> [erg/cm^3/h]

        soil_heat->set_source (i, value);
      }
}

void 
ColumnStandard::spray (const symbol chemical, 
                       const double amount /* [g/ha] */,
                       Treelog& msg)
{
  chemistry->spray (chemical, amount / (100.0 * 100.0 /* ha->m^2 */), msg); 
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
{ return geometry.content_height (*soil_heat, &SoilHeat::T, height); }

double 
ColumnStandard::soil_water_potential (double height) const
{ return geometry.content_height (*soil_water, &SoilWater::h, height); }

double 
ColumnStandard::soil_water_content (double from, double to) const
{
  daisy_assert (to <= from);
  daisy_assert (to <= 0.0);
  daisy_assert (to > geometry.cell_z (soil->size () - 1));
  return soil_water->content_surface (geometry, from, to);
}

double                          // [kg N/ha]
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

double                          // [kg N/ha]
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
  movement->clear ();
  bioclimate->clear ();
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
ColumnStandard::tick_source (const Scope& parent_scope, 
                             const Time& time, Treelog& msg)
{ 
  // Weather.
  if (weather.get ())
    weather->weather_tick (time, msg);

  // Find forward sink.
  drain->tick (geometry, *soil, *soil_heat, surface, *soil_water, msg);
  movement->tick_source (*soil, *soil_heat, *soil_water, msg);

  // Find water based limit.
  soil_water->tick_source (geometry, *soil, msg);

  // Scope.
  daisy_assert (extern_scope);
  ScopeMulti scope (*extern_scope, parent_scope);

  // Find solute based limit.
  chemistry->tick_source (scope, geometry, *soil, *soil_water, *soil_heat, 
                          *organic_matter, *chemistry, msg);
}

double
ColumnStandard::suggest_dt () const
{ 
  double dt = 0.0;
  
  if (weather.get ())
    {
      const double w_dt = weather->suggest_dt ();
      if (std::isnormal (w_dt) && (!std::isnormal (dt) || dt > w_dt))
        dt = w_dt;
    }

  const double sw_dt = soil_water->suggest_dt ();
  
  if (std::isnormal (sw_dt) 
      && (!std::isnormal (dt) || dt > sw_dt))
    dt = sw_dt;

  const double chem_dt = chemistry->suggest_dt ();

  if (std::isnormal (chem_dt) 
      && (!std::isnormal (dt) || dt > chem_dt))
    dt = chem_dt;

  return dt;
}

void
ColumnStandard::tick_move (const Metalib& metalib, const Time& time, 
                           const double dt,
                           const Weather *const global_weather,
                           const Scope& parent_scope,
                           Treelog& msg)
{
  // Scope.
  daisy_assert (extern_scope);
  ScopeMulti scope (*extern_scope, parent_scope);

  const Weather& my_weather = weather.get () ? *weather : *global_weather;
  const double T_bottom = movement->bottom_heat (time, my_weather);

  // Add deposition. 
  chemistry->deposit (my_weather.deposit (), msg);

  // Irrigation is delayed management.
  irrigation->tick (geometry, *soil_water, *chemistry, *bioclimate, dt, msg);

  // Put on timestep on management results for output.
  harvest_DM /= dt;
  harvest_N /= dt;
  harvest_C /= dt;
  residuals_DM /= dt;
  residuals_N_top /= dt;
  residuals_C_top /= dt;
  for (size_t c = 0; c < residuals_N_soil.size (); c++)
    residuals_N_soil[c] /= dt;
  for (size_t c = 0; c < residuals_C_soil.size (); c++)
    residuals_C_soil[c] /= dt;
  seed_N /= dt;
  seed_C /= dt;
  applied_DM /= dt;
  first_year_utilization /= dt;

  // Macropores before everything else.
  movement->tick_tertiary (units, geometry, *soil, *soil_heat, dt,
                           *soil_water, surface, msg);

  // Early calculation.

  const double old_pond 
    = bioclimate->get_snow_storage () + surface.ponding_average ();
  bioclimate->tick (units, time, surface, my_weather,
                    *vegetation, *litter, *movement,
                    geometry, *soil, *soil_water, *soil_heat,
                    dt, msg);
  vegetation->tick (metalib, time, *bioclimate, geometry, *soil, 
                    *soil_heat, *soil_water, *chemistry, *organic_matter, 
                    residuals_DM, residuals_N_top, residuals_C_top, 
                    residuals_N_soil, residuals_C_soil, dt, msg);

  const double tillage_top 
    = geometry.content_hood (tillage_age, Geometry::cell_above);
  
  chemistry->tick_top (units, geometry, *soil, *soil_water, *soil_heat, 
                       tillage_top, surface,
                       bioclimate->snow_leak_rate (dt), vegetation->cover (),
                       bioclimate->canopy_leak_rate (dt),
                       litter->cover (),
                       bioclimate->litter_leak_rate (dt), 
                       surface.runoff_rate (dt),
                       old_pond,
                       my_weather.rain (),
                       bioclimate->direct_rain (), 
                       bioclimate->canopy_leak (),
                       vegetation->height () * 0.01 /* [m] */,
                       *chemistry,
                       dt, msg);
  
  // Tillage time.
  const size_t cell_size = geometry.cell_size ();
  for (size_t i = 0; i < cell_size; i++)
    tillage_age[i] += dt/24.0;

  // Turnover.
  organic_matter->tick (geometry, *soil_water, *soil_heat, tillage_age,
                        *chemistry, dt, msg);
  litter->update (organic_matter->top_DM ());
  // Transport.
  groundwater->tick (units, geometry, *soil, *soil_water, 
                     surface.ponding_average () * 0.1, 
                     *soil_heat, time, scope, msg);
  soil_heat->tick (geometry, *soil, *soil_water, T_bottom, *movement, 
                   surface, dt, msg);
  movement->tick (*soil, *soil_water, *soil_heat,
                  surface, *groundwater, time, my_weather, 
                  dt, msg);
  soil_water->tick_after (geometry, *soil, *soil_heat, false, msg);
  soil_water->mass_balance (geometry, dt, msg);
  soil_heat->tick_after (geometry.cell_size (), *soil, *soil_water, msg);
  chemistry->tick_soil (scope, geometry, 
                        surface.ponding_average (),
                        surface.mixing_resistance (),
                        *soil, *soil_water, *soil_heat, 
                        *movement, *organic_matter, *chemistry, dt, msg);
  organic_matter->transport (units, geometry, 
                             *soil, *soil_water, *soil_heat, msg);
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
    organic_matter->monthly (metalib, geometry, msg);
}

bool
ColumnStandard::check (const Weather* global_weather,
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
    Treelog::Open nest (msg, "Drain");
    if (!drain->check (msg))
      ok = false;
  }
  {
    Treelog::Open nest (msg, "Movement: " + movement->objid);
    if (!movement->check (msg))
      ok = false;
  }
  {
    Treelog::Open nest (msg, "Groundwater: " + groundwater->objid);
    if (!groundwater->check (units, geometry, scope, msg))
      ok = false;
  }
  {
    if (weather.get ())
      {
        Treelog::Open nest (msg, "Weather: " + weather->objid);
        if (!weather->weather_check (from, to, msg))
          // The rest is uninitialized, don't check it!
          return false;
      }

    else if (!global_weather)
      {
        msg.entry ("Weather unspecified");
        // The rest is uninitialized, don't check it!
        return false;
      }
  }
  const Weather& my_weather = weather.get () ? *weather : *global_weather;
  {
    Treelog::Open nest (msg, "Bioclimate");
    if (!bioclimate->check (my_weather, msg))
      ok = false;
  }
  {
    Treelog::Open nest (msg, "Chemistry");
    if (!chemistry->check (scope, geometry, *soil, *soil_water, *soil_heat,
                           *chemistry, msg))
      ok = false;
  }
  {
    Treelog::Open nest (msg, "Vegetation");
    if (!vegetation->check (units, msg))
      ok = false;
  }
  {
    Treelog::Open nest (msg, "Soil");
    if (!soil->check (organic_matter->som_pools (), geometry, msg))
      ok = false;
  }
  if (!organic_matter->check (units, geometry, 
                              *soil, *soil_water, *soil_heat, *chemistry, msg))
    ok = false;
  return ok;
}

bool 
ColumnStandard::check_am (const FrameModel& am, Treelog& msg) const 
{ 
  TREELOG_MODEL (msg);
  return organic_matter->check_am (am, msg); 
}

bool 
ColumnStandard::check_z_border (const double value, Treelog& msg) const
{ 
  TREELOG_MODEL (msg);

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
  TREELOG_MODEL (msg);

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
  TREELOG_MODEL (msg);

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
  Column::output (log);
  if (weather.get ())
    output_derived (weather, "weather", log);
  output_object (bioclimate, "Bioclimate", log);
  output_submodule (surface, "Surface", log);
  // output_submodule (geometry, "Geometry", log);
  output_submodule (*soil, "Soil", log);
  output_submodule (*soil_water, "SoilWater", log);
  output_submodule (*soil_heat, "SoilHeat", log);
  output_object (chemistry, "Chemistry", log);
  output_derived (vegetation, "Vegetation", log);
  output_derived (organic_matter, "OrganicMatter", log);
  output_value (second_year_utilization_, "second_year_utilization", log);
  output_variable (tillage_age, log);
  output_submodule (*irrigation, "Irrigation", log);
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
               + surface.ponding_average (),
               "surface_water", log);
  output_derived (movement, "Movement", log);
  output_derived (drain, "Drain", log);
  output_derived (groundwater, "Groundwater", log);
}

ColumnStandard::ColumnStandard (const BlockModel& al)
  : Column (al),
    units (al.units ()),
    scopesel (Librarian::build_item<Scopesel> (al, "scope")),
    extern_scope (NULL),
    movement (Librarian::build_item<Movement> (al, "Movement")),
    drain (Librarian::build_item<Drain> (al, "Drain")),
    groundwater (Librarian::build_item<Groundwater> (al, "Groundwater")),
    weather (al.check ("weather") 
             ? Librarian::build_item<WSource> (al, "weather")
             : NULL), 
    vegetation (Librarian::build_item<Vegetation> (al, "Vegetation")),
    litter (Librarian::build_item<Litter> (al, "Litter")),
    bioclimate (Librarian::build_item<Bioclimate> (al, "Bioclimate")),
    surface (al.submodel ("Surface")),
    geometry (movement->geometry ()),
    soil (submodel<Soil> (al, "Soil")),
    soil_water (submodel<SoilWater> (al, "SoilWater")),
    soil_heat (submodel<SoilHeat> (al, "SoilHeat")),
    chemistry (Librarian::build_item<Chemistry> (al, "Chemistry")),
    organic_matter (Librarian::build_item<OrganicMatter> 
                    (al, "OrganicMatter")),
    second_year_utilization_ (al.number ("second_year_utilization")),
    irrigation (submodel<Irrigation> (al, "Irrigation")),
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
{ 
  if (al.check ("tillage_age"))
    tillage_age = al.number_sequence ("tillage_age");
}

bool
ColumnStandard::initialize (const Block& block, 
                            const std::vector<const Scope*> scopes,
                            const Time& time, 
                            const Weather* global_weather,
                            const Scope& parent_scope)
{
  bool ok = true;
  Treelog& msg = block.msg ();
  TREELOG_MODEL (msg);
  extern_scope = scopesel->lookup (scopes, msg); 
  ScopeMulti scope (extern_scope ? *extern_scope : Scope::null (),
                    parent_scope);
  soil->initialize (block, geometry, *groundwater,
                    organic_matter->som_pools ());
  const size_t cell_size = geometry.cell_size ();
  residuals_N_soil.insert (residuals_N_soil.begin (), cell_size, 0.0);
  daisy_assert (residuals_N_soil.size () == cell_size);
  residuals_C_soil.insert (residuals_C_soil.begin (), cell_size, 0.0);
  daisy_assert (residuals_C_soil.size () == cell_size);

  // Tillage age.
  if (tillage_age.size () == 0)
    {
      tillage_age.insert (tillage_age.end (), cell_size, 365.2425 * 100);
      for (size_t i = 0; i < cell_size; i++)
        if (geometry.cell_z (i) > -25)
          tillage_age[i] = 100.0;
    }
  else if (tillage_age.size () < cell_size)
    tillage_age.insert (tillage_age.end (), 
                        cell_size - tillage_age.size (), 
                        tillage_age.back ());

  groundwater->initialize (units, geometry, time, scope, msg);
  drain->initialize (geometry, msg);

  // Movement depends on soil and groundwater
  movement->initialize (units, *soil, *groundwater,  scope, msg);

  surface.initialize (geometry);

  // Bioclimate and heat depends on weather.
  if (weather.get ())
    weather->weather_initialize (time, msg);

  if (!global_weather && !weather.get ())
    {
      msg.error ("No weather specified");
      return false;
    }
  const Weather& my_weather = weather.get () ? *weather : *global_weather;
  bioclimate->initialize (my_weather, msg);
  soil_heat->initialize (frame ().submodel ("SoilHeat"), geometry, 
                         movement->default_heat (*soil, time, my_weather),
                         msg);

  soil_water->initialize (frame ().submodel ("SoilWater"), 
                          geometry, *soil, *soil_heat, *groundwater, msg);
  
  // Solutes depends on water and heat.
  chemistry->initialize (scope, geometry, *soil, *soil_water, *soil_heat, 
                         surface, msg);
  
  // Organic matter and vegetation.
  const double T_avg = my_weather.average_temperature ();
  organic_matter->initialize (block.metalib (), 
                              units, frame ().model ("OrganicMatter"), 
                              geometry, *soil, *soil_water, *soil_heat, 
                              T_avg, msg);
  vegetation->initialize (block.metalib (), 
                          units, time, geometry, *soil, *organic_matter, msg);
  litter->initialize (organic_matter->top_DM ());

  // Soil conductivity and capacity logs.
  soil_heat->tick_after (geometry.cell_size (), *soil, *soil_water, msg);

  return ok;
}

void 
ColumnStandard::summarize (Treelog& msg) const
{
  TREELOG_MODEL (msg);
  movement->summarize (msg);
  bioclimate->summarize (msg);
}

ColumnStandard::~ColumnStandard ()
{ }

static struct ColumnStandardSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ColumnStandard (al); }

  void load_frame (Frame& frame) const
  { 
    frame.declare_object ("scope", Scopesel::component, 
                          Attribute::Const, Attribute::Singleton, "\
Scope to evaluate expessions in.");
    frame.set ("scope", "null");
    frame.declare_submodule ("Soil", Attribute::State,
                             "The numeric and physical soil properties.",
                             Soil::load_syntax);
    frame.declare_submodule ("SoilWater", Attribute::State,
                             "Soil water content and transportation.",
                             SoilWater::load_syntax);
    frame.declare_submodule ("SoilHeat", Attribute::State,
                             "Soil heat capacity and transportation.",
                             SoilHeat::load_syntax);
    frame.declare_object ("Movement", Movement::component,
                          Attribute::State, Attribute::Singleton, "\
Discretization and movement of water, heat and solutes in the soil.");
    frame.set ("Movement", "vertical");
    frame.declare_object ("weather", WSource::component,
                          Attribute::OptionalState, Attribute::Singleton, "\
Weather model for providing climate information during\n\
the simulation.  If unspecified, used global weather.");
    frame.declare_object ("Vegetation", Vegetation::component,
                          Attribute::State, Attribute::Singleton,
                          "The crops on the field.");
    frame.set ("Vegetation", "crops");
    frame.declare_object ("Litter", Litter::component,
                          Attribute::State, Attribute::Singleton,
                          "The litter layer below the canopy.");
    frame.set ("Litter", "none");

    frame.declare_object ("Bioclimate", Bioclimate::component, 
                          Attribute::State, Attribute::Singleton,
                          "The water and energy distribution among the crops.");
    frame.set ("Bioclimate", "default");
    frame.declare_submodule ("Surface", Attribute::State,
                             "The upper border of the soil.",
                             Surface::load_syntax);
    frame.declare_object ("Drain", Drain::component, 
                          Attribute::State, Attribute::Singleton, "\
Drainage.");
    frame.set ("Drain", "none");
    frame.declare_object ("Groundwater", Groundwater::component,
                          "The groundwater level.");
    frame.declare_object ("Chemistry", Chemistry::component, 
                          Attribute::State, Attribute::Singleton,
                          "Chemical compounds in the system.");
    frame.set ("Chemistry", "nutrient");
    frame.declare ("harvest_DM", "g/m^2/h", Attribute::LogOnly, 
                   "Amount of DM removed by harvest this hour.");
    frame.declare ("harvest_N", "g/m^2/h", Attribute::LogOnly, 
                   "Amount of nitrogen removed by harvest this hour.");
    frame.declare ("harvest_C", "g/m^2/h", Attribute::LogOnly, 
                   "Amount of carbon removed by harvest this hour.");
    frame.declare ("residuals_DM", "g/m^2/h", Attribute::LogOnly, "\
Amount of dry matter removed from crops to surface and soil this hour.\n\
This includes loss as harvest, as well as loss of old leaves and roots.");
    frame.declare ("residuals_N_top", "g/m^2/h", Attribute::LogOnly, 
                   "Amount of nitrogen removed from crops to soil this hour.\n\
This includes loss as harvest, as well as loss of old leaves.");
    frame.declare ("residuals_C_top", "g/m^2/h", Attribute::LogOnly, 
                   "Amount of carbon removed from crops to surface this hour.\n\
This includes loss as harvest, as well as loss of old leaves.");
    frame.declare ("residuals_N_soil", "g/cm^3/h", Attribute::LogOnly, Attribute::SoilCells, 
                   "Amount of nitrogen removed from crops in soil this hour.\n\
This includes loss as harvest, as well as loss of old roots.");
    frame.declare ("residuals_C_soil", "g/cm^3/h", Attribute::LogOnly, Attribute::SoilCells, 
                   "Amount of carbon removed from crops in soil this hour.\n\
This includes loss as harvest, as well as loss of old roots.");
    frame.declare ("residuals_N_root", "g/m^2/h", Attribute::LogOnly, 
                   "Amount of nitrogen removed from crops to soil this hour.\n\
This includes loss as harvest, as well as loss of old roots.");
    frame.declare ("residuals_C_root", "g/m^2/h", Attribute::LogOnly, 
                   "Amount of carbon removed from crops to surface this hour.\n\
This includes loss as harvest, as well as loss of old roots.");
    frame.declare ("surface_water", "mm", Attribute::LogOnly, 
                   "Amount of water in the system above ground.\n\
This include ponded water, intercepted water and the snow pack.");
    frame.declare_object ("OrganicMatter", OrganicMatter::component,
                          Attribute::State, Attribute::Singleton, "\
The organic matter in the soil and on the surface.");
    frame.set ("OrganicMatter", "default");
    frame.declare ("second_year_utilization", "kg N/ha", Attribute::State,
                   "Estimated accumulated second year fertilizer effect.");
    frame.set ("second_year_utilization", 0.0);
    frame.declare ("tillage_age", "d",
                   Attribute::OptionalState, Attribute::SoilCells, "\
Time since the latest tillage operation was performed.\n\
By default, the top 25 cm will have an initial tillage age of 100 days,\n\
while the soil below that will have an initial tillage age of 100 years.\n\
If you specify fewer values than there are soil cells, the last specified\n\
value will be used for the remaining cells."); 
    frame.declare_submodule ("Irrigation", Attribute::State,
                             "Active irrigation events.",
                             Irrigation::load_syntax);

    frame.declare ("seed_N", "kg N/ha/h", Attribute::LogOnly,
                   "Amount of nitrogen in seed applied this time step.");
    frame.declare ("seed_C", "kg C/ha/h", Attribute::LogOnly,
                   "Amount of carbon in seed applied this time step.");
    frame.declare ("applied_DM", "ton DM/ha/h", Attribute::LogOnly,
                   "Amount of dry matter applied this time step.\n\
This includes dry matter incorporated directly in the soil.");
    frame.declare ("first_year_utilization", "kg N/ha/h", Attribute::LogOnly,
                   "Estimated first year fertilizer effect.");
  }
    
  ColumnStandardSyntax ()
    : DeclareModel (Column::component, "default", "\
Hansen et.al. 1990. with generic movement in soil.")
  { }
} column_syntax;

// column_std.C ends here.
