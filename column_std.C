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

#include "column.h"
#include "library.h"
#include "surface.h"
#include "soil_heat.h"
#include "soil_chemicals.h"
#include "movement.h"
#include "groundwater.h"
#include "geometry.h"
#include "soil.h"
#include "soil_water.h"
#include "vegetation.h"
#include "bioclimate.h"
#include "weather.h"
#include "chemistry.h"
#include "chemicals.h"
#include "soil_NH4.h"
#include "soil_NO3.h"
#include "organic_matter.h"
#include "denitrification.h"
#include "im.h"
#include "am.h"
#include "dom.h"
#include "time.h"
#include "log.h"
#include "submodeler.h"
#include "memutils.h"

struct ColumnStandard : public Column
{
  std::auto_ptr<Movement> movement;
  std::auto_ptr<Groundwater> groundwater;
  Weather* weather;
  std::auto_ptr<Vegetation> vegetation;
  std::auto_ptr<Bioclimate> bioclimate;
  Surface surface;
  Geometry& geometry;
  std::auto_ptr<Soil> soil;
  std::auto_ptr<SoilWater> soil_water;
  std::auto_ptr<SoilHeat> soil_heat;
  SoilChemicals soil_chemicals;
  std::vector<Chemistry*> chemistry;
  SoilNH4 soil_NH4;
  SoilNO3 soil_NO3;
  std::auto_ptr<OrganicMatter> organic_matter;
  Denitrification denitrification;
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
  double fertilized_NO3_total;
  double fertilized_NH4_total;
  double volatilization_total;
  double fertilized_NO3_surface;
  double fertilized_NH4_surface;
  double volatilization_surface;
  double fertilized_DM;
  double first_year_utilization;

public:
  struct NitLog			// Nitrification log variables.
  {
    // Log variables.
    std::vector<double> NH4;
    std::vector<double> NO3;
    std::vector<double> N2O;

    // Simulation.
    void tick (const std::vector<bool>&, 
               const Soil& soil, const SoilWater& soil_water,
               const SoilHeat& soil_heat,
               SoilNO3& soil_NO3, SoilNH4& soil_NH4, double dt);
    void output (Log&) const;
    
    // Create and Destroy.
    void initialize (const size_t size);
    static void load_syntax (Syntax&, AttributeList&);
    NitLog (const AttributeList&)
    { }
    ~NitLog () 
    { }
  } nitrification;

  const Horizon& horizon_at (double z, double x, double y) const;

  // Actions.
  void sow (Treelog&, const AttributeList&);
  void ridge (const AttributeList& al);
  void irrigate_overhead (double flux, double temp, const IM&);
  void irrigate_surface (double flux, double temp, const IM&);
  void irrigate_overhead (double flux, const IM&);
  void irrigate_surface (double flux, const IM&);
  void irrigate_subsoil (double flux, const IM& sm, 
                         double from, double to);
  void fertilize (const IM&);
  void fertilize (const AttributeList&);
  void fertilize (const AttributeList&, double from, double to);
  void clear_second_year_utilization ();
  void emerge (const symbol crop_name, Treelog& msg);
  void harvest (const Time& time, const symbol crop_name,
                double stub_length, double stem_harvest,
                double leaf_harvest, double sorg_harvest, const bool combine,
                std::vector<const Harvest*>& harvest, Treelog& msg);

  void add_residuals (std::vector<AM*>& residuals);
  void mix (Treelog&,
	    const Time&, double from, double to, double penetration = 1.0);
  void swap (Treelog&,
	     const Time&, double from, double middle, double to);
  void set_porosity (double at, double Theta);
  void set_heat_source (double at, double value); // [W/m^2]
  void spray (symbol chemical, double amount); // [g/ha]
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
  std::string crop_names () const;
  double bottom () const;

  // Simulation.
  void clear ();
  void tick (Treelog&, double dt, const Time&, const Weather*);
  bool check (bool require_weather,
              const Time& from, const Time& to, 
              Treelog& err) const;
  bool check_am (const AttributeList& am, Treelog& err) const;
  bool check_z_border (double, Treelog& err) const;
  bool check_x_border (double, Treelog& err) const;
  bool check_y_border (double, Treelog& err) const;
  void output (Log&) const;

  // Create and Destroy.
  static Movement* build_vertical (Block& al);
  ColumnStandard (Block& al);
  Column& clone (symbol name) const
  { 
    AttributeList new_alist (alist);
    // BUG: TODO: Log state of 'this' to new_alist.
    new_alist.add ("type", name.name ());
    Block block (Librarian<Column>::library ().syntax (name), new_alist,
		 Treelog::null (), "clone");
    return *new ColumnStandard (block); 
  }
  void initialize (const Time&, Treelog&, const Weather*);
  ~ColumnStandard ();
};

void 
ColumnStandard::NitLog::tick (const std::vector<bool>& active, 
                              const Soil& soil, const SoilWater& soil_water,
                              const SoilHeat& soil_heat,
                              SoilNO3& soil_NO3, SoilNH4& soil_NH4,
                              const double dt)
{
  const size_t cell_size = soil.size ();
  for (int i = 0; i < cell_size; i++)
    {
      daisy_assert (soil_NO3.M_left (i, dt) >= 0.0);
      daisy_assert (soil_NH4.M_left (i, dt) >= 0.0);
    }

  daisy_assert (NH4.size () == cell_size);
  daisy_assert (N2O.size () == cell_size);
  daisy_assert (NO3.size () == cell_size);

  for (size_t i = 0; i < cell_size; i++)
    {
      if (active[i])
        soil.nitrification (i, 
                            soil_NH4.M (i), soil_NH4.C (i), 
                            soil_NH4.M_left (i, dt),
                            soil_water.h (i), soil_heat.T (i),
                            NH4[i], N2O[i], NO3[i], dt);
      else
        NH4[i] = N2O[i] = NO3[i] = 0.0;        
    }

  soil_NH4.add_to_sink (NH4, dt);
  soil_NO3.add_to_source (NO3, dt);

  for (size_t i = 0; i < cell_size; i++)
    {
      daisy_assert (soil_NO3.M_left (i, dt) >= 0.0);
      daisy_assert (soil_NH4.M_left (i, dt) >= 0.0);
    }
}

void 
ColumnStandard::NitLog::output (Log& log) const
{ 
  output_variable (NH4, log);
  output_variable (NO3, log);
  output_variable (N2O, log);
}
    
void 
ColumnStandard::NitLog::initialize (const size_t size)
{
  NH4 = std::vector<double> (size, 0.0);
  NO3 = std::vector<double> (size, 0.0);
  N2O = std::vector<double> (size, 0.0);
}

void 
ColumnStandard::NitLog::load_syntax (Syntax& syntax, AttributeList&)
{
  syntax.add ("NH4", "g N/cm^3/h", Syntax::LogOnly, Syntax::Sequence, 
              "Amount of ammonium consumed this hour.");
  syntax.add ("NO3", "g N/cm^3/h", Syntax::LogOnly, Syntax::Sequence, 
              "Amount of nitrate generated this hour.");
  syntax.add ("N2O", "g N/cm^3/h", Syntax::LogOnly, Syntax::Sequence, 
              "Amount of nitrous oxide generated this hour.");
}

const Horizon& 
ColumnStandard::horizon_at (const double z, 
                            const double x, const double y) const
{ return soil->horizon (geometry.cell_at (z, x, y)); }

void 
ColumnStandard::sow (Treelog& msg, const AttributeList& al)
{ vegetation->sow (msg, al, geometry, *organic_matter, seed_N, seed_C); }

// We need to convert from mm * mg N / liter to g N/cm^2.
// mm / liter = 1/m^2 = 1/(100^2 cm^2) = 1/10000 1/cm^2 = 1.0e-4 1/cm^2
// mg = 1/1000 g = 1.0e-3 g
// Thus, we need to divide flux * solute with 1.0e-7 to get the surface input.
static const double irrigate_solute_factor = 1.0e-7;

void 
ColumnStandard::ridge (const AttributeList& al)
{ movement->ridge (surface, *soil, *soil_water, al); }

void 
ColumnStandard::irrigate_overhead (double flux, double temp, const IM& sm)
{
  daisy_assert (flux >= 0.0);
  daisy_assert (sm.NH4 >= 0.0);
  daisy_assert (sm.NO3 >= 0.0);
  bioclimate->irrigate_overhead (flux, temp);
  fertilize (sm * (flux * irrigate_solute_factor));
}

void 
ColumnStandard::irrigate_surface (double flux, double temp, const IM& sm)
{
  daisy_assert (flux >= 0.0);
  daisy_assert (sm.NH4 >= 0.0);
  daisy_assert (sm.NO3 >= 0.0);
  bioclimate->irrigate_surface (flux, temp);
  fertilize (sm * (flux * irrigate_solute_factor));
}

void 
ColumnStandard::irrigate_overhead (double flux, const IM& sm)
{
  daisy_assert (flux >= 0.0);
  daisy_assert (sm.NH4 >= 0.0);
  daisy_assert (sm.NO3 >= 0.0);
  bioclimate->irrigate_overhead (flux);
  fertilize (sm * (flux * irrigate_solute_factor));
}

void 
ColumnStandard::irrigate_surface (double flux, const IM& sm)
{
  daisy_assert (flux >= 0.0);
  daisy_assert (sm.NH4 >= 0.0);
  daisy_assert (sm.NO3 >= 0.0);
  bioclimate->irrigate_surface (flux);
  fertilize (sm * (flux * irrigate_solute_factor));
}

void
ColumnStandard::irrigate_subsoil (double flux, const IM& sm, 
                                  double from, double to)
{
  soil_water->incorporate (geometry, flux / 10.0 /* mm -> cm */, from, to);
  bioclimate->irrigate_subsoil (flux);
  const IM im (sm, flux * irrigate_solute_factor);
  soil_NH4.incorporate (geometry, im.NH4, from, to);
  soil_NO3.incorporate (geometry, im.NO3, from, to);
  // kg/ha -> g/cm^2
  const double conv = (1000.0 / ((100.0 * 100.0) * (100.0 * 100.0)));
  fertilized_NO3_total += im.NO3 / conv; 
  fertilized_NH4_total += im.NH4 / conv;
}

void
ColumnStandard::fertilize (const IM& im)
{
  // kg/ha -> g/cm^2
  const double conv = (1000.0 / ((100.0 * 100.0) * (100.0 * 100.0)));

  daisy_assert (im.NH4 >= 0.0);
  daisy_assert (im.NO3 >= 0.0);
  surface.fertilize (im);
  fertilized_NO3_total += im.NO3 / conv; 
  fertilized_NH4_total += im.NH4 / conv;
  fertilized_NO3_surface += im.NO3 / conv; 
  fertilized_NH4_surface += im.NH4 / conv;
}

void
ColumnStandard::fertilize (const AttributeList& al)
{
  // Utilization log.
  first_year_utilization += AM::utilized_weight (al);
  second_year_utilization_ += AM::second_year_utilization (al);

  // Volatilization.
  const double lost_NH4 = AM::get_volatilization (al);
  volatilization_total += lost_NH4;
  volatilization_surface += lost_NH4;

  // Add inorganic matter.
  fertilize (IM (al));
  fertilized_NH4_total += lost_NH4;
  fertilized_NH4_surface += lost_NH4;

  // Add organic matter, if any.
  if (al.name ("syntax") != "mineral")
    organic_matter->fertilize (al, geometry);
  fertilized_DM += AM::get_DM (al);
}

void 
ColumnStandard::fertilize (const AttributeList& al, double from, double to)
{
  daisy_assert (to < from);
  // kg/ha -> g/cm^2
  const double conv = (1000.0 / ((100.0 * 100.0) * (100.0 * 100.0)));

  // Utilization log.
  first_year_utilization += AM::utilized_weight (al);
  second_year_utilization_ += AM::second_year_utilization (al);

  // Volatilization.
  const double lost_NH4 = AM::get_volatilization (al);
  volatilization_total += lost_NH4;

  // Add inorganic matter.
  IM im (al);
  daisy_assert (im.NH4 >= 0.0);
  daisy_assert (im.NO3 >= 0.0);
  soil_NO3.incorporate (geometry, im.NO3, from, to);
  soil_NH4.incorporate (geometry, im.NH4, from, to);
  fertilized_NO3_total += im.NO3 / conv; 
  fertilized_NH4_total += im.NH4 / conv + lost_NH4;
  fertilized_DM += AM::get_DM (al);

  // Add organic matter, if any.
  if (al.name ("syntax") != "mineral")
    organic_matter->fertilize (al, geometry, from, to);
}

void 
ColumnStandard::clear_second_year_utilization ()
{ second_year_utilization_ = 0.0; }

void
ColumnStandard::emerge (const symbol crop_name, Treelog& msg)
{ vegetation->emerge (crop_name, msg); }

void
ColumnStandard::harvest (const Time& time, const symbol crop_name,
                         double stub_length,
                         double stem_harvest,
                         double leaf_harvest, 
                         double sorg_harvest,
                         const bool combine,
                         std::vector<const Harvest*>& harvest, Treelog& msg)
{ 
  std::vector<AM*> residuals;
  double min_height = 100.0;
  vegetation->harvest (name, crop_name, time, geometry, *bioclimate,
                       stub_length, 
                       stem_harvest, leaf_harvest, sorg_harvest,
                       harvest, min_height, 
                       residuals, harvest_DM, harvest_N, harvest_C, 
                       residuals_DM, residuals_N_top, residuals_C_top,
                       residuals_N_soil, residuals_C_soil,
                       combine, msg); 
  add_residuals (residuals);
  if (min_height < 0.0)
    mix (msg, time, 0.0, min_height, 0.0);
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
ColumnStandard::mix (Treelog& msg, const Time& time,
		     double from, double to, double penetration)
{
  std::vector<AM*> residuals;
  vegetation->kill_all (name, time, geometry, *bioclimate, residuals, 
                        residuals_DM, residuals_N_top, residuals_C_top, 
                        residuals_N_soil, residuals_C_soil, msg);
  add_residuals (residuals);
  const double energy 
    = soil_heat->energy (geometry, *soil, *soil_water, from, to);
  soil_water->mix (geometry, *soil, from, to);
  soil_heat->set_energy (geometry, *soil, *soil_water, from, to, energy);
  soil_chemicals.mix (geometry, *soil, *soil_water, from, to);
  surface.unridge ();
  soil_NO3.mix (geometry, *soil, *soil_water, from, to);
  soil_NH4.mix (geometry, *soil, *soil_water, from, to);
  organic_matter->mix (geometry, *soil, *soil_water, from, to, penetration, time);
}

void 
ColumnStandard::swap (Treelog& msg, 
		      const Time& time, double from, double middle, double to)
{
  mix (msg, time, from, middle, 1.0);
  mix (msg, time, middle, to, 0.0);
  soil_water->swap (msg, geometry, *soil, from, middle, to);
  soil_heat->swap (geometry, from, middle, to);
  soil_chemicals.swap (geometry, *soil, *soil_water, from, middle, to);
  soil_NO3.swap (geometry, *soil, *soil_water, from, middle, to);
  soil_NH4.swap (geometry, *soil, *soil_water, from, middle, to);
  organic_matter->swap (geometry, *soil, *soil_water, from, middle, to, time);
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
       const double V = geometry.volume (i);
       value *= 10^3;		// [W/m^2] -> [erg/cm^2/s]
       value *= 3600;		// [erg/cm^2/s] -> [erg/cm^2/h]
       value /= V;              // [erg/cm^2/h] -> [erg/cm^3/h]

       soil_heat->set_source (i, value);
     }
}

void 
ColumnStandard::spray (symbol chemical, double amount) // [g/ha]
{ bioclimate->spray (chemical, amount / (100.0 * 100.0) /* ha->m^2 */); }

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
  daisy_assert (to > geometry.z (soil->size () - 1));
  return soil_water->content_surface (geometry, from, to);
}

double				// [kg N/ha]
ColumnStandard::soil_inorganic_nitrogen (double from, double to) const
{
  return (soil_NH4.total_surface (geometry, from, to) 
	  + soil_NO3.total_surface (geometry, from, to))
    * 1.0e5; // g N/cm^2 -> kg N/ha
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
  soil_chemicals.clear ();

  harvest_DM = 0.0;
  harvest_N = 0.0;
  harvest_C = 0.0;
  residuals_DM = 0.0;
  residuals_N_top = 0.0;
  residuals_C_top = 0.0;
  fill (residuals_N_soil.begin (), residuals_N_soil.end (), 0.0);
  fill (residuals_C_soil.begin (), residuals_C_soil.end (), 0.0);

  soil_NO3.clear ();
  soil_NH4.clear ();
  
  organic_matter->clear ();

  seed_N = 0.0;
  seed_C = 0.0;
  fertilized_NO3_total = 0.0;
  fertilized_NH4_total = 0.0;
  fertilized_NO3_surface = 0.0;
  fertilized_NH4_surface = 0.0;
  fertilized_DM = 0.0;
  first_year_utilization = 0.0;
  volatilization_total = 0.0;
  volatilization_surface = 0.0;
}

void
ColumnStandard::tick (Treelog& msg, const double dt,
		      const Time& time, const Weather* global_weather)
{
  // Chemistry.
  for (std::vector<Chemistry*>::const_iterator i = chemistry.begin ();
       i != chemistry.end ();
       i++)
    (*i)->tick (geometry, *soil, *soil_water, soil_chemicals, dt, msg);

  // Weather.
  if (weather)
    weather->tick (time, msg);

  const Weather& my_weather = *(weather ? weather : global_weather);

  // Early calculation.
  IM soil_top_conc;
  soil_top_conc.NO3 
    = geometry.content_at (static_cast<const Solute&> (soil_NO3),
                            &Solute::C, 0.0)
    / 10.0; // [g/cm^3] -> [g/cm^2/mm]
  soil_top_conc.NH4 
    = geometry.content_at (static_cast<const Solute&> (soil_NH4),
                            &Solute::C, 0.0) 
    / 10.0; // [g/cm^3] -> [g/cm^2/mm]
  surface.mixture (soil_top_conc, dt);
  surface.mixture (geometry, soil_chemicals, dt);
  movement->macro_tick (*soil, *soil_water, surface, dt, msg);
  bioclimate->tick (time, surface, my_weather, 
                    *vegetation, *movement,
                    geometry, *soil, *soil_water, *soil_heat, 
                    dt, msg);

  vegetation->tick (time, my_weather.relative_humidity (),
                    *bioclimate, geometry, *soil, 
		    organic_matter.get (),
                    *soil_heat, *soil_water, &soil_NH4, &soil_NO3, 
                    residuals_DM, residuals_N_top, residuals_C_top, 
                    residuals_N_soil, residuals_C_soil, dt, msg);
  organic_matter->tick (geometry, *soil_water, *soil_heat, 
                        soil_NO3, soil_NH4, dt, msg);
  const std::vector<bool> active = organic_matter-> active (); 
  nitrification.tick (active, 
                      *soil, *soil_water, *soil_heat, soil_NO3, soil_NH4, dt);
  denitrification.tick (active, 
                        geometry, *soil, *soil_water, *soil_heat, soil_NO3, 
			*organic_matter, dt);

  
  // Transport.
  groundwater->tick (geometry, *soil, *soil_water, 
                     surface.ponding () * 0.1, 
                     *soil_heat, time, msg);
  movement->tick (*soil, *soil_water, *soil_heat,
                  surface, *groundwater, time, my_weather, dt, msg);
  soil_water->tick_after (geometry.cell_size (), *soil, *soil_heat, msg);
  soil_heat->tick_after (geometry.cell_size (), *soil, *soil_water, msg);

  soil_chemicals.tick (geometry, *soil, *soil_water, *soil_heat,
                       organic_matter.get (),
		       surface.chemicals_down (), dt, msg);
  const SoilChemicals::SoluteMap& solutes = soil_chemicals.all ();
  for (SoilChemicals::SoluteMap::const_iterator i = solutes.begin ();
       i != solutes.end ();
       i++)
    {
      const symbol name = (*i).first;
      SoilChemical& solute = *(*i).second;
      Treelog::Open nest (msg, name);
      // [g/m^2/h ned -> g/cm^2/h op]
      const double J_in = -surface.chemicals_down ().amount (name) 
        / (100.0 * 100.0);
      movement->solute (*soil, *soil_water, J_in, solute, dt, msg); 
    }

  organic_matter->transport (*soil, *soil_water, msg);
  const std::vector<DOM*>& dom = organic_matter->fetch_dom ();
  for (size_t i = 0; i < dom.size (); i++)
    {
      movement->element (*soil, *soil_water, dom[i]->C, 
                         *dom[i]->adsorption, dom[i]->diffusion_coefficient, 
                         dt, msg);
      movement->element (*soil, *soil_water, dom[i]->N, 
                         *dom[i]->adsorption, dom[i]->diffusion_coefficient, 
                         dt, msg);
    }
  {
    Treelog::Open nest (msg, "soil_NO3");
    movement->solute (*soil, *soil_water, 
                      surface.matter_flux ().NO3, soil_NO3, dt, msg);
  }
  {
    Treelog::Open nest (msg, "soil_NH4");
    movement->solute (*soil, *soil_water,
                      surface.matter_flux ().NH4, soil_NH4, dt, msg);
  }
  
  // Once a month we clean up old AM from organic matter.
  if (time.hour () == 13 && time.mday () == 13)
    organic_matter->monthly (geometry);
}

bool
ColumnStandard::check (bool require_weather,
                       const Time& from, const Time& to, Treelog& err) const
{
  bool ok = true;
  const int n = geometry.cell_size ();
  {
    Treelog::Open nest (err, "Soil");
    if (!geometry.check (err))
      ok = false;
  }
  {
    Treelog::Open nest (err, "SoilHeat");
    if (!soil_heat->check (n, err))
      ok = false;
  }
  {
    Treelog::Open nest (err, "Movement");
    if (!movement->check (err))
      ok = false;
  }
  {
    Treelog::Open nest (err, "Groundwater");
    if (!groundwater->check (err))
      ok = false;
  }
  {
    Treelog::Open nest (err, "Weather");
    if (weather)
      {
	if (!weather->check (from, to, err))
	  ok = false;
      }

    else if (require_weather)
      {
	err.entry ("Weather unspecified");
	// The rest is uninitialized, don't check it!
	return false;
      }
  }
  {
    Treelog::Open nest (err, "SoilChemicals");
    if (!soil_chemicals.check (n, err))
      ok = false;
  }
  {
    Treelog::Open nest (err, "Chemistry");
    for (std::vector<Chemistry*>::const_iterator i = chemistry.begin ();
	 i != chemistry.end ();
	 i++)
      {
	const Chemistry& reaction = **i;
	Treelog::Open nest (err, reaction.name);
	if (!reaction.check (*soil, err))
	  ok = false;
      }
  }
  {
    Treelog::Open nest (err, "Soil");
    if (!soil->check (organic_matter->som_pools (), geometry, err))
      ok = false;
  }
  {
    Treelog::Open nest (err, "SoilNO3");
    if (!soil_NO3.check (n, err))
      ok = false;
  }
  {
    Treelog::Open nest (err, "SoilNH4");
    if (!soil_NH4.check (n, err))
      ok = false;
  }
  if (!organic_matter->check (*soil, err))
    ok = false;
  return ok;
}

bool 
ColumnStandard::check_am (const AttributeList& am, Treelog& err) const 
{ 
  Treelog::Open nest (err, name);
  return organic_matter->check_am (am, err); 
}

bool 
ColumnStandard::check_z_border (const double value, Treelog& err) const
{ 
  Treelog::Open nest (err, "column: " + name);

  bool ok = true;
  if (!soil->check_z_border (value, err))
    ok = false; 
  if (!geometry.check_z_border (value, err))
    ok = false; 
  return ok;
}

bool 
ColumnStandard::check_x_border (const double value, Treelog& err) const
{ 
  Treelog::Open nest (err, "column: " + name);

  bool ok = true;
  if (!soil->check_x_border (value, err))
    ok = false; 
  if (!geometry.check_x_border (value, err))
    ok = false; 
  return ok;
}

bool 
ColumnStandard::check_y_border (const double value, Treelog& err) const
{ 
  Treelog::Open nest (err, "column: " + name);

  bool ok = true;
  if (!soil->check_y_border (value, err))
    ok = false; 
  if (!geometry.check_y_border (value, err))
    ok = false; 
  return ok;
}

void
ColumnStandard::output (Log& log) const
{
  Log::Geo geo (log, geometry, *soil);
  Column::output (log);
  if (weather)
    output_derived (weather, "weather", log);
  output_object (bioclimate, "Bioclimate", log);
  output_submodule (surface, "Surface", log);
  // output_submodule (geometry, "Geometry", log);
  output_submodule (*soil, "Soil", log);
  output_submodule (*soil_water, "SoilWater", log);
  output_submodule (*soil_heat, "SoilHeat", log);
  output_submodule (soil_chemicals, "SoilChemicals", log);
  output_list (chemistry, "Chemistry", log, 
	       Librarian<Chemistry>::library ());
  output_derived (vegetation, "Vegetation", log);
  output_submodule (soil_NH4, "SoilNH4", log);
  output_submodule (soil_NO3, "SoilNO3", log);
  output_derived (organic_matter, "OrganicMatter", log);
  output_submodule (denitrification, "Denitrification", log);
  output_value (second_year_utilization_, "second_year_utilization", log);
  output_variable (seed_N, log);
  output_variable (seed_C, log);
  output_value (fertilized_NO3_total, "fertilized_NO3", log);
  output_value (fertilized_NH4_total, "fertilized_NH4", log);
  output_variable (fertilized_NO3_surface, log);
  output_variable (fertilized_NH4_surface, log);
  output_value (volatilization_total, "volatilization", log);
  output_variable (volatilization_surface, log);
  output_variable (fertilized_DM, log);
  output_variable (first_year_utilization, log);
  output_submodule (nitrification, "Nitrification", log);
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
    movement (Librarian<Movement>::build_item (al, "Movement")),
    groundwater (Librarian<Groundwater>::build_item (al, "Groundwater")),
    weather (al.check ("weather") 
	     ? Librarian<Weather>::build_item (al, "weather")
	     : NULL), 
    vegetation (Librarian<Vegetation>::build_item (al, "Vegetation")),
    bioclimate (Librarian<Bioclimate>::build_item (al, "Bioclimate")),
    surface (al.alist ("Surface")),
    geometry (movement->geometry ()),
    soil (submodel<Soil> (al, "Soil")),
    soil_water (submodel<SoilWater> (al, "SoilWater")),
    soil_heat (submodel<SoilHeat> (al, "SoilHeat")),
    soil_chemicals (al.alist ("SoilChemicals")),
    chemistry (Librarian<Chemistry>::build_vector (al, "Chemistry")),
    soil_NH4 (al.alist ("SoilNH4")),
    soil_NO3 (al.alist ("SoilNO3")),
    organic_matter (Librarian<OrganicMatter>::build_item 
                    (al, "OrganicMatter")),
    denitrification (al.alist ("Denitrification")),
    second_year_utilization_ (al.number ("second_year_utilization")),
    harvest_DM (0.0),
    harvest_N (0.0),
    harvest_C (0.0),
    residuals_DM (0.0),
    residuals_N_top (0.0),
    residuals_C_top (0.0),
    seed_N (0.0),
    seed_C (0.0),
    fertilized_NO3_total (0.0),
    fertilized_NH4_total (0.0),
    volatilization_total (0.0),
    fertilized_NO3_surface (0.0),
    fertilized_NH4_surface (0.0),
    volatilization_surface (0.0),
    fertilized_DM (0.0),
    first_year_utilization (0.0),
    nitrification (al.alist ("Nitrification"))
{ }

void 
ColumnStandard::initialize (const Time& time, Treelog& msg, 
			    const Weather* global_weather)
{
  Treelog::Open nest (msg, name);
  soil->initialize (geometry, *groundwater,
                    organic_matter->som_pools (), msg);
  residuals_N_soil.insert (residuals_N_soil.begin (), soil->size (), 0.0);
  daisy_assert (residuals_N_soil.size () == soil->size ());
  residuals_C_soil.insert (residuals_C_soil.begin (), soil->size (), 0.0);
  daisy_assert (residuals_C_soil.size () == soil->size ());

  groundwater->initialize (geometry, time, msg);
  soil_water->initialize (alist.alist ("SoilWater"), 
                          geometry, *soil, *groundwater, msg);
  if (alist.check ("Movement"))
    {
      AttributeList move_alist (alist.alist ("Movement"));
      const AttributeList& water_alist = alist.alist ("SoilWater");
      if (water_alist.check ("macro")
          && !move_alist.check ("macro"))
        move_alist.add ("macro", water_alist.alist ("macro"));
      movement->initialize (move_alist, *soil, *groundwater, msg);
    }
  else
    {
      AttributeList al;
      movement->initialize (al, *soil, *groundwater, msg);
    }

  // Solutes depends on water.
  soil_chemicals.initialize (alist.alist ("SoilChemicals"),
                             geometry, *soil, *soil_water, msg);
  for (std::vector<Chemistry*>::const_iterator i = chemistry.begin ();
       i != chemistry.end ();
       i++)
    (*i)->initialize (*soil, msg);
  soil_NH4.initialize (alist.alist ("SoilNH4"),
                       geometry, *soil, *soil_water, msg);
  soil_NO3.initialize (alist.alist ("SoilNO3"), 
                       geometry, *soil, *soil_water, msg);
  nitrification.initialize (soil->size ());
  denitrification.initialize (soil->size ());

  // Bioclimate and heat depends on weather.
  if (weather && !weather->initialize (time, msg))
    return;
  if (!global_weather && !weather)
    return;
  const Weather& my_weather = *(weather ? weather : global_weather);
  bioclimate->initialize (my_weather, msg);
  soil_heat->initialize (alist.alist ("SoilHeat"), geometry, 
                         movement->default_heat (*soil, time, my_weather), msg);
  // Organic matter and vegetation.
  const double T_avg = my_weather.average_temperature ();
  organic_matter->initialize (alist.alist ("OrganicMatter"), 
                              geometry, *soil, *soil_water, 
                              T_avg, msg);
  vegetation->initialize (time, geometry, *soil, &*organic_matter, msg);
  
  // Soil conductivity and capacity logs.
  soil_water->tick_after (geometry.cell_size (), *soil, *soil_heat, msg);
  soil_heat->tick_after (geometry.cell_size (), *soil, *soil_water, msg);
}

ColumnStandard::~ColumnStandard ()
{
  if (weather)
    delete weather;
  sequence_delete (chemistry.begin (), chemistry.end ());
}

static struct ColumnStandardSyntax
{
  static Column& make (Block& al)
  { return *new ColumnStandard (al); }

  static void load_water_and_macro (Syntax& syntax, AttributeList& alist)
  {
    SoilWater::load_syntax (syntax, alist);
    syntax.add ("macro", Librarian<Macro>::library (),
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

    syntax.add_submodule ("Soil", alist, Syntax::State,
                          "The numeric and physical soil properties.",
                          Soil::load_syntax);
    syntax.add_submodule ("SoilWater", alist, Syntax::State,
                          "Soil water content and transportation.",
                          load_water_and_macro);
    syntax.add_submodule ("SoilHeat", alist, Syntax::State,
                          "Soil heat capacity and transportation.",
                          SoilHeat::load_syntax);
    syntax.add ("Movement", Librarian<Movement>::library (),
                Syntax::State, Syntax::Singleton, "\
Discretization and movement of water, heat and solutes in the soil.");
    alist.add ("Movement", Movement::default_model ());
    syntax.add ("weather", Librarian<Weather>::library (),
                Syntax::OptionalState, Syntax::Singleton,
                "Weather model for providing climate information during\n\
the simulation.  If unspecified, used global weather.");
    syntax.add ("Vegetation", Librarian<Vegetation>::library (),
                Syntax::State, Syntax::Singleton,
                "The crops on the field.");
    AttributeList vegetation_alist;
    vegetation_alist.add ("type", "crops");
    vegetation_alist.add ("crops", std::vector<AttributeList*> ());
    vegetation_alist.add ("ForcedLAI", std::vector<AttributeList*> ());
    vegetation_alist.add ("EpInterchange", 0.6);
    alist.add ("Vegetation", vegetation_alist);

    syntax.add ("Bioclimate", Librarian<Bioclimate>::library (), 
                Syntax::State, Syntax::Singleton,
                "The water and energy distribution among the crops.");
    alist.add ("Bioclimate", Bioclimate::default_model ());
    syntax.add_submodule ("Surface", alist, Syntax::State,
                          "The upper border of the soil.",
                          Surface::load_syntax);
    syntax.add ("Groundwater", Librarian<Groundwater>::library (),
                "The groundwater level.");
    syntax.add_submodule ("SoilChemicals", alist, Syntax::State,
                          "Chemicals in the soil.",
                          SoilChemicals::load_syntax);
    syntax.add ("Chemistry", Librarian<Chemistry>::library (), 
                Syntax::State, Syntax::Sequence, 
                "Transformations applied to soil chemicals.");
    const std::vector<AttributeList*> empty;
    alist.add ("Chemistry", empty);
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
    syntax.add_submodule ("SoilNH4", alist, Syntax::State,
			  "Ammonium content in soil.",
			  SoilNH4::load_syntax);
    syntax.add_submodule ("SoilNO3", alist, Syntax::State,
			  "Nitrate content in soil.",
			  SoilNO3::load_syntax);
    syntax.add ("OrganicMatter", Librarian<OrganicMatter>::library (),
                Syntax::State, Syntax::Singleton, "\
The organic matter in the soil and on the surface.");
    alist.add ("OrganicMatter", OrganicMatter::default_model ());
    syntax.add_submodule ("Denitrification", alist, Syntax::State, "\
The denitrification process.",
			  Denitrification::load_syntax);
    syntax.add ("second_year_utilization", "kg N/ha", Syntax::State,
		"Estimated accumulated second year fertilizer effect.");
    alist.add ("second_year_utilization", 0.0);
    syntax.add ("seed_N", "kg N/ha/h", Syntax::LogOnly,
		"Amount of nitrogen in seed applied this time step.");
    syntax.add ("seed_C", "kg C/ha/h", Syntax::LogOnly,
		"Amount of carbon in seed applied this time step.");
    syntax.add ("fertilized_NO3", "kg N/ha/h", Syntax::LogOnly,
		"Amount of nitrate applied this time step.\n\
This does include nitrate incorporated in the soil.");
    syntax.add ("fertilized_NH4", "kg N/ha/h", Syntax::LogOnly,
		"Amount of ammonium applied this time step.\n\
This includes both ammonium lost due to volatilization, and ammonium\n\
incorporated in the soil.");
    syntax.add ("fertilized_NO3_surface", "kg N/ha/h", Syntax::LogOnly,
		"Amount of nitrate applied to surface this time step.\n\
This does not include nitrate incorporated in the soil.");
    syntax.add ("fertilized_NH4_surface", "kg N/ha/h", Syntax::LogOnly,
		"Amount of ammonium applied to surface this time step.\n\
This includes ammonium lost due to volatilization, but not ammonium\n\
incorporated in the soil.");
    syntax.add ("fertilized_DM", "ton DM/ha/h", Syntax::LogOnly,
		"Amount of dry matter applied this time step.\n\
This includes dry matter incorporated directly in the soil.");
    syntax.add ("first_year_utilization", "kg N/ha/h", Syntax::LogOnly,
		"Estimated first year fertilizer effect.");
    syntax.add ("volatilization", "kg N/ha/h", Syntax::LogOnly, "\
Amount of NH4 volatilization, also from incorporated fertilizer.");
    syntax.add ("volatilization_surface", "kg N/ha/h", Syntax::LogOnly, "\
Amount of NH4 volatilization, only from surface applied fertilizer.");
    syntax.add_submodule ("Nitrification", alist, Syntax::State, "\
The nitrification log.\n\
Note that the nitrification parameters are found in the horizons.",
			  ColumnStandard::NitLog::load_syntax);
    
    Librarian<Column>::add_type ("default", alist, syntax, &make);
  }
} column_syntax;
