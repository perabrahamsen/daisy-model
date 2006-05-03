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
#include "surface.h"
#include "soil_heat1d.h"
#include "soil_chemicals.h"
#include "movement.h"
#include "geometry1d.h"
#include "soil.h"
#include "soil_water1d.h"
#include "vegetation.h"
#include "bioclimate.h"
#include "weather.h"
#include "groundwater.h"
#include "soltrans1d.h"
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

using namespace std;

static const double m2_per_cm2 = 0.0001;
static const double cm2_per_m2 = 1.0 / m2_per_cm2;

struct ColumnStandard : public Column
{
  Weather* weather;
  std::auto_ptr<Vegetation> vegetation;
  std::auto_ptr<Bioclimate> bioclimate;
  Surface surface;
  std::auto_ptr<Geometry1D> geometry;
  std::auto_ptr<Soil> soil;
  std::auto_ptr<SoilWater1D> soil_water;
  std::auto_ptr<SoilHeat1D> soil_heat;
  std::auto_ptr<Soltrans1D> soltrans;
  SoilChemicals soil_chemicals;
  std::vector<Chemistry*> chemistry;
  std::auto_ptr<Groundwater> groundwater;
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
    vector<double> NH4;
    vector<double> NO3;
    vector<double> N2O;

    // Simulation.
    void tick (const std::vector<bool>&, 
               const Soil& soil, const SoilWater& soil_water,
               const SoilHeat& soil_heat,
               SoilNO3& soil_NO3, SoilNH4& soil_NH4);
    void output (Log&) const;
    
    // Create and Destroy.
    void initialize (const size_t size);
    static void load_syntax (Syntax&, AttributeList&);
    NitLog (const AttributeList&)
    { }
    ~NitLog () 
    { }
  } nitrification;

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
  void harvest (const Time& time, const symbol crop_name,
                double stub_length, double stem_harvest,
                double leaf_harvest, double sorg_harvest, const bool combine,
                vector<const Harvest*>& harvest, Treelog& msg);

  void add_residuals (vector<AM*>& residuals);
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

  // Simulation.
  void clear ();
  void tick (Treelog&, const Time&, const Weather*);
  bool check (bool require_weather,
              const Time& from, const Time& to, 
              Treelog& err) const;
  bool check_am (const AttributeList& am, Treelog& err) const;
  bool check_border (const double border, Treelog& err) const;
  void output (Log&) const;

  // Communication with external model.
  unsigned int count_layers () const; // Number of num. layers.
  double get_dz (unsigned int i) const; // Size of layer 'i'. [cm]
  void put_water_pressure (const std::vector<double>& v); // [cm]
  void get_water_sink (std::vector<double>& v) 
    const; // [cm^3/cm^3/h]
  void put_no3_m (const vector<double>& v); // [g/cm^3]
  void get_no3_m (vector<double>& v) const; // [g/cm^3]
  double get_evap_interception () const; // [mm/h]
  double get_intercepted_water () const; // [mm]
  double get_net_throughfall () const; // [mm/h]
  double get_snow_storage () const; // [mm]
  double get_exfiltration () const; // [mm/h]
  double get_evap_soil_surface () const; // [mm/h]
  void put_ponding (double pond);	// [mm]
  void put_surface_no3 (double no3); // [g/cm^2]
  double get_surface_no3 () const; // [g/cm^2]
  void put_surface_chemical (symbol, double); // [g/cm^2]
  double get_surface_chemical (symbol) const; // [g/cm^2]
  double get_smb_c_at (unsigned int i) const; //[g C/cm³]
  double get_co2_production_at (unsigned int i) const; // [g C/cm³]
  double get_temperature_at (unsigned int i) const; // [°C]
  double get_crop_h2o_uptake_at (unsigned int i) const; //[cm³/cm³]
  double get_water_content_at (unsigned int i) const; // [cm³/cm³]
  double get_water_conductivity_at (unsigned int i) const; // [cm/h]

  // Create and Destroy.
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
                              SoilNO3& soil_NO3, SoilNH4& soil_NH4)
{
  const size_t cell_size = soil.size ();
  for (int i = 0; i < cell_size; i++)
    {
      daisy_assert (soil_NO3.M_left (i) >= 0.0);
      daisy_assert (soil_NH4.M_left (i) >= 0.0);
    }

  daisy_assert (NH4.size () == cell_size);
  daisy_assert (N2O.size () == cell_size);
  daisy_assert (NO3.size () == cell_size);

  for (size_t i = 0; i < cell_size; i++)
    {
      if (active[i])
        soil.nitrification (i, 
                            soil_NH4.M (i), soil_NH4.C (i), 
                            soil_NH4.M_left (i),
                            soil_water.h (i), soil_heat.T (i),
                            NH4[i], N2O[i], NO3[i]);
      else
        NH4[i] = N2O[i] = NO3[i] = 0.0;        
    }

  soil_NH4.add_to_sink (NH4);
  soil_NO3.add_to_source (NO3);

  for (size_t i = 0; i < cell_size; i++)
    {
      daisy_assert (soil_NO3.M_left (i) >= 0.0);
      daisy_assert (soil_NH4.M_left (i) >= 0.0);
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
  NH4 = vector<double> (size, 0.0);
  NO3 = vector<double> (size, 0.0);
  N2O = vector<double> (size, 0.0);
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

void 
ColumnStandard::sow (Treelog& msg, const AttributeList& al)
{ vegetation->sow (msg, al, *geometry, *organic_matter, seed_N, seed_C); }

// We need to convert from mm * mg N / liter to g N/cm^2.
// mm / liter = 1/m^2 = 1/(100^2 cm^2) = 1/10000 1/cm^2 = 1.0e-4 1/cm^2
// mg = 1/1000 g = 1.0e-3 g
// Thus, we need to divide flux * solute with 1.0e-7 to get the surface input.
static const double irrigate_solute_factor = 1.0e-7;

void 
ColumnStandard::ridge (const AttributeList& al)
{ surface.ridge (*geometry, *soil, *soil_water, al); }

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
  soil_water->incorporate (*geometry, flux / 10.0 /* mm -> cm */, from, to);
  bioclimate->irrigate_subsoil (flux);
  const IM im (sm, flux * irrigate_solute_factor);
  soil_NH4.incorporate (*geometry, im.NH4, from, to);
  soil_NO3.incorporate (*geometry, im.NO3, from, to);
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
    organic_matter->fertilize (al, *geometry);
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
  soil_NO3.incorporate (*geometry, im.NO3, from, to);
  soil_NH4.incorporate (*geometry, im.NH4, from, to);
  fertilized_NO3_total += im.NO3 / conv; 
  fertilized_NH4_total += im.NH4 / conv + lost_NH4;
  fertilized_DM += AM::get_DM (al);

  // Add organic matter, if any.
  if (al.name ("syntax") != "mineral")
    organic_matter->fertilize (al, *geometry, from, to);
}

void 
ColumnStandard::clear_second_year_utilization ()
{ second_year_utilization_ = 0.0; }

void
ColumnStandard::harvest (const Time& time, const symbol crop_name,
                         double stub_length,
                         double stem_harvest,
                         double leaf_harvest, 
                         double sorg_harvest,
                         const bool combine,
                         vector<const Harvest*>& harvest, Treelog& msg)
{ 
  vector<AM*> residuals;
  double min_height = 100.0;
  vegetation->harvest (name, crop_name, time, *geometry, *bioclimate,
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
ColumnStandard::add_residuals (vector<AM*>& residuals)
{
  // Put the residuals in the soil.
  for (vector<AM*>::iterator residual = residuals.begin ();
       residual != residuals.end ();
       residual++)
    organic_matter->add (*(*residual));
}

void 
ColumnStandard::mix (Treelog& msg, const Time& time,
		     double from, double to, double penetration)
{
  vector<AM*> residuals;
  vegetation->kill_all (name, time, *geometry, *bioclimate, residuals, 
                        residuals_DM, residuals_N_top, residuals_C_top, 
                        residuals_N_soil, residuals_C_soil, msg);
  add_residuals (residuals);
  const double energy = soil_heat->energy (*geometry, *soil, *soil_water, from, to);
  soil_water->mix (*geometry, *soil, from, to);
  soil_heat->set_energy (*geometry, *soil, *soil_water, from, to, energy);
  soil_chemicals.mix (*geometry, *soil, *soil_water, from, to);
  surface.unridge ();
  soil_NO3.mix (*geometry, *soil, *soil_water, from, to);
  soil_NH4.mix (*geometry, *soil, *soil_water, from, to);
  organic_matter->mix (*geometry, *soil, *soil_water, from, to, penetration, time);
}

void 
ColumnStandard::swap (Treelog& msg, 
		      const Time& time, double from, double middle, double to)
{
  mix (msg, time, from, middle, 1.0);
  mix (msg, time, middle, to, 0.0);
  soil_water->swap (msg, *geometry, *soil, from, middle, to);
  soil_heat->swap (*geometry, from, middle, to);
  soil_chemicals.swap (*geometry, *soil, *soil_water, from, middle, to);
  soil_NO3.swap (*geometry, *soil, *soil_water, from, middle, to);
  soil_NH4.swap (*geometry, *soil, *soil_water, from, middle, to);
  organic_matter->swap (*geometry, *soil, *soil_water, from, middle, to, time);
}

void 
ColumnStandard::set_porosity (double at, double Theta)
{ soil->set_porosity (geometry->interval_plus (at), Theta); }

void 
ColumnStandard::set_heat_source (double at, double value) // [W/m^2]
{
  const unsigned i = geometry->interval_plus (at);
  const double dz = geometry->dz (i);

  value *= 10^3;		// [W/m^2] -> [erg/cm^2/s]
  value *= 3600;		// [erg/cm^2/s] -> [erg/cm^2/h]
  value /= dz;			// [erg/cm^2/h] -> [erg/cm^3/h]
  
  soil_heat->set_source (i, value);
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
{
  daisy_assert (height < 0);
  daisy_assert (height > geometry->z (soil->size () - 1));
  return soil_heat->T (geometry->interval_plus (height));
}

double 
ColumnStandard::soil_water_potential (double height) const
{
  daisy_assert (height < 0);
  daisy_assert (height > geometry->z (soil->size () - 1));
  return soil_water->h (geometry->interval_plus (height));
}

double 
ColumnStandard::soil_water_content (double from, double to) const
{
  daisy_assert (to <= from);
  daisy_assert (to <= 0.0);
  daisy_assert (to > geometry->z (soil->size () - 1));
  return soil_water->content (*geometry, from, to);
}

double				// [kg N/ha]
ColumnStandard::soil_inorganic_nitrogen (double from, double to) const
{
  return (soil_NH4.total (*geometry, from, to) 
	  + soil_NO3.total (*geometry, from, to)) * 1.0e5; // g N/cm^2 -> kg N/ha
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

void
ColumnStandard::clear ()
{ 
  soil_water->clear (*geometry);
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
ColumnStandard::tick (Treelog& msg, 
		      const Time& time, const Weather* global_weather)
{
  // Base log.
  // Chemistry.
  for (vector<Chemistry*>::const_iterator i = chemistry.begin ();
       i != chemistry.end ();
       i++)
    (*i)->tick (*geometry, *soil, *soil_water, soil_chemicals, msg);

  // Weather.
  if (weather)
    weather->tick (time, msg);

  const Weather& my_weather = *(weather ? weather : global_weather);

  // Early calculation.
  IM soil_top_conc;
  soil_top_conc.NO3 
    = geometry->content_at (static_cast<const Solute&> (soil_NO3),
                            &Solute::C, 0.0)
    / 10.0; // [g/cm^3] -> [g/cm^2/mm]
  soil_top_conc.NH4 
    = geometry->content_at (static_cast<const Solute&> (soil_NH4),
                            &Solute::C, 0.0) 
    / 10.0; // [g/cm^3] -> [g/cm^2/mm]
  surface.mixture (soil_top_conc);
  surface.mixture (*geometry, soil_chemicals);
  soil_water->macro_tick (*geometry, *soil, surface, msg);

  bioclimate->tick (time, surface, my_weather, 
                    *vegetation, *geometry, *soil, *soil_water, *soil_heat, msg);
  vegetation->tick (time, *bioclimate, *geometry, *soil, organic_matter.get (), 
                    *soil_heat, *soil_water, &soil_NH4, &soil_NO3, 
                    residuals_DM, residuals_N_top, residuals_C_top, 
                    residuals_N_soil, residuals_C_soil, msg);
  organic_matter->tick (*geometry, *soil_water, *soil_heat, 
                        soil_NO3, soil_NH4, msg);
  const std::vector<bool> active = organic_matter-> active (); 
  nitrification.tick (active, 
                      *soil, *soil_water, *soil_heat, soil_NO3, soil_NH4);
  denitrification.tick (active, 
                        *geometry, *soil, *soil_water, *soil_heat, soil_NO3, 
			*organic_matter);

  
  groundwater->tick (*geometry, *soil,
                     *soil_water, surface.h (), *soil_heat, time, msg);

  // Transport.
  soil_heat->tick (time, *geometry, *soil, *soil_water, surface, my_weather);
  soil_water->tick (*geometry, *soil, *soil_heat, surface, *groundwater, msg);
  soil_chemicals.tick (*geometry, *soil, *soil_water, *soil_heat,
                       organic_matter.get (),
		       surface.chemicals_down (), msg);
  const SoilChemicals::SoluteMap& solutes = soil_chemicals.all ();
  // Transport.
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
      soltrans->solute (*geometry, *soil, *soil_water, J_in, solute, msg); 
    }
  organic_matter->transport (*soil, *soil_water, msg);
  const std::vector<DOM*>& dom = organic_matter->fetch_dom ();
  for (size_t i = 0; i < dom.size (); i++)
    {
      soltrans->element (*geometry, *soil, *soil_water,
                         dom[i]->C, 
                         *dom[i]->adsorption, dom[i]->diffusion_coefficient, 
                         msg);
      soltrans->element (*geometry, *soil, *soil_water,
                         dom[i]->N, 
                         *dom[i]->adsorption, dom[i]->diffusion_coefficient, 
                         msg);
    }
  soltrans->solute (*geometry, *soil, *soil_water, surface.matter_flux ().NO3, 
                    soil_NO3, msg);
  soltrans->solute (*geometry, *soil, *soil_water, surface.matter_flux ().NH4, 
                    soil_NH4, msg);
  
  // Once a month we clean up old AM from organic matter.
  if (time.hour () == 13 && time.mday () == 13)
    organic_matter->monthly (*geometry);
}

bool
ColumnStandard::check (bool require_weather,
                       const Time& from, const Time& to, Treelog& err) const
{
  bool ok = true;
  const int n = soil->size ();
  {
    Treelog::Open nest (err, "Soil");
    if (!geometry->check (err))
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
    Treelog::Open nest (err, "SoilWater");
    if (!soil_water->check (n, err))
      ok = false;
  }
  {
    Treelog::Open nest (err, "SoilHeat");
    if (!soil_heat->check (n, err))
      ok = false;
  }
  {
    Treelog::Open nest (err, "SoilChemicals");
    if (!soil_chemicals.check (n, err))
      ok = false;
  }
  {
    Treelog::Open nest (err, "Chemistry");
    for (vector<Chemistry*>::const_iterator i = chemistry.begin ();
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
    Treelog::Open nest (err, "Groundwater");
    if (!groundwater->check (err))
      ok = false;
  }
  {
    Treelog::Open nest (err, "Soil");
    if (!soil->check (organic_matter->som_pools (), *geometry, err))
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
ColumnStandard::check_border (const double border, Treelog& err) const
{ 
  bool ok = true;
  if (!geometry->check_border (border, err))
    ok = false; 
  if (!soil->check_border (border, err))
    ok = false; 
  return ok;
}

void
ColumnStandard::output (Log& log) const
{
  Log::Geo geo (log, *geometry, *soil);
  Column::output (log);
  if (weather)
    output_derived (weather, "weather", log);
  output_object (bioclimate, "Bioclimate", log);
  output_submodule (surface, "Surface", log);
  // output_submodule (*geometry, "Geometry", log);
  output_submodule (*soil, "Soil", log);
  output_submodule (*soil_water, "SoilWater", log);
  output_submodule (*soil_heat, "SoilHeat", log);
  output_submodule (soil_chemicals, "SoilChemicals", log);
  output_list (chemistry, "Chemistry", log, 
	       Librarian<Chemistry>::library ());
  output_derived (groundwater, "Groundwater", log);
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

  static const symbol N_symbol ("residuals_N_root");
  if (log.check_leaf (N_symbol))
    log.output (N_symbol, geometry->total (residuals_N_soil) * cm2_per_m2);
  static const symbol C_symbol ("residuals_C_root");
  if (log.check_leaf (C_symbol))
    log.output (C_symbol, geometry->total (residuals_C_soil) * cm2_per_m2);
  static const symbol surface_water_symbol ("surface_water");
  if (log.check_leaf (surface_water_symbol))
    log.output (surface_water_symbol, (bioclimate->get_intercepted_water ()
                                       + bioclimate->get_snow_storage ()
                                       + surface.ponding ()));
}

unsigned int 
ColumnStandard::count_layers () const // Number of num. layers.
{ return soil->size (); }

double 
ColumnStandard::get_dz (unsigned int i) const // Size of layer 'i'. [cm]
{ return geometry->dz (i); }

void 
ColumnStandard::put_water_pressure (const vector<double>& v) // [cm]
{ soil_water->put_h (*soil, v); }

void 
ColumnStandard::get_water_sink (vector<double>& v) const // [cm^3/cm^3/h]
{ soil_water->get_sink (v); }

void 
ColumnStandard::put_no3_m (const vector<double>& v) // [g/cm^3]
{ soil_NO3.put_M (*soil, *soil_water, v); }

void 
ColumnStandard::get_no3_m (vector<double>& v) const // [g/cm^3]
{ 
  const unsigned int size = soil->size ();

  v.erase (v.begin (), v.end ());
  for (unsigned int i = 0; i < size; i++)
    v.push_back (soil_NO3.M (i));
}

double 
ColumnStandard::get_evap_interception () const // [mm/h]
{ return bioclimate->get_evap_interception (); }

double 
ColumnStandard::get_intercepted_water () const // [mm]
{ return bioclimate->get_intercepted_water (); }

double 
ColumnStandard::get_net_throughfall () const // [mm/h]
{ return bioclimate->get_net_throughfall (); }

double 
ColumnStandard::get_snow_storage () const // [mm]
{ return bioclimate->get_snow_storage (); }

double 
ColumnStandard::get_exfiltration () const // [mm/h]
{ return surface.exfiltration (); }

double 
ColumnStandard::get_evap_soil_surface () const // [mm/h]
{ return surface.evap_soil_surface (); }

void 
ColumnStandard::put_ponding (double pond)	// [mm]
{ surface.put_ponding (pond); }

void 
ColumnStandard::put_surface_no3 (double no3) // [g/cm^2]
{ surface.put_no3 (no3); }

double 
ColumnStandard::get_surface_no3 () const // [g/cm^2]
{ return surface.get_no3 (); }

void
ColumnStandard::put_surface_chemical (symbol name, double amount) //[g/cm^2]
{ surface.put_chemical (name, amount); }

double 
ColumnStandard::get_surface_chemical (symbol name) const // [g/cm^2]
{ return surface.get_chemical (name); }

double 
ColumnStandard::get_smb_c_at (unsigned int i) const //[g C/cm³]
{ return organic_matter->get_smb_c_at (i); }

double 
ColumnStandard::get_co2_production_at (unsigned int i) const // [g C/cm³]
{ return organic_matter->CO2 (i); }

double
ColumnStandard::get_temperature_at (unsigned int i) const // [°C]
{ return soil_heat->T (i); }

double 
ColumnStandard::get_crop_h2o_uptake_at (unsigned int i) const // [cm³/cm³/h]
{ 
  vector<double> v;
  soil_water->get_sink (v);
  daisy_assert (v.size () > i);
  return v[i];
}

double 
ColumnStandard::get_water_content_at (unsigned int i) const // [cm³/cm³]
{ return soil_water->Theta (i); }

double 
ColumnStandard::get_water_conductivity_at (unsigned int i) const // [cm³/cm³]
{ return soil->K (i, soil_water->h (i), soil_water->h_ice (i), 
                  soil_heat->T (i)); }

static Bioclimate*
get_bioclimate (Block& al)
{
  if (al.check ("Bioclimate"))
    return Librarian<Bioclimate>::build_item (al, "Bioclimate");
  static const symbol default_symbol ("default");
  AttributeList alist (Librarian<Bioclimate>::library ()
		       .lookup (default_symbol));
  alist.add ("type", "default");
  return Librarian<Bioclimate>::build_alist (al, alist, "Bioclimate");
}

ColumnStandard::ColumnStandard (Block& al)
  : Column (al),
    weather (al.check ("weather") 
	     ? Librarian<Weather>::build_item (al, "weather")
	     : NULL), 
    vegetation (Librarian<Vegetation>::build_item (al, "Vegetation")),
    bioclimate (get_bioclimate (al)),
    surface (al.alist ("Surface")),
    geometry (submodel<Geometry1D> (al, "Soil")),
    soil (submodel<Soil> (al, "Soil")),
    soil_water (submodel<SoilWater1D> (al, "SoilWater")),
    soil_heat (submodel<SoilHeat1D> (al, "SoilHeat")),
    soltrans (submodel<Soltrans1D> (al, "Soltrans")),
    soil_chemicals (al.alist ("SoilChemicals")),
    chemistry (Librarian<Chemistry>::build_vector (al, "Chemistry")),
    groundwater (Librarian<Groundwater>::build_item (al, "Groundwater")),
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
ColumnStandard::initialize (const Time& time, Treelog& err, 
			    const Weather* global_weather)
{
  Treelog::Open nest (err, name);
  soil->initialize (*geometry, 
                    *groundwater, organic_matter->som_pools (), err);
  residuals_N_soil.insert (residuals_N_soil.begin (), soil->size (), 0.0);
  daisy_assert (residuals_N_soil.size () == soil->size ());
  residuals_C_soil.insert (residuals_C_soil.begin (), soil->size (), 0.0);
  daisy_assert (residuals_C_soil.size () == soil->size ());
  groundwater->initialize (*geometry, time, err);
  soil_water->initialize (alist.alist ("SoilWater"), 
                          *geometry, *soil, *groundwater, err);
  soil_chemicals.initialize (alist.alist ("SoilChemicals"),
                             *geometry, *soil, *soil_water, err);
  for (vector<Chemistry*>::const_iterator i = chemistry.begin ();
       i != chemistry.end ();
       i++)
    (*i)->initialize (*soil, err);

  // Bioclimate and heat depends on weather.
  if (weather && !weather->initialize (time, err))
    return;
  if (!global_weather && !weather)
    return;
  const Weather& my_weather = *(weather ? weather : global_weather);
  bioclimate->initialize (my_weather, err);
  soil_heat->initialize (alist.alist ("SoilHeat"), 
                         *geometry, *soil, time, my_weather, err);
  soil_NH4.initialize (alist.alist ("SoilNH4"),
                       *geometry, *soil, *soil_water, err);
  soil_NO3.initialize (alist.alist ("SoilNO3"), 
                       *geometry, *soil, *soil_water, err);
  nitrification.initialize (soil->size ());
  denitrification.initialize (soil->size ());
  if (!global_weather && !weather)
    return;
  const double T_avg = weather // Must be after initialize_common.
    ? weather->average_temperature ()
    : global_weather->average_temperature ();
  organic_matter->initialize (alist.alist ("OrganicMatter"), 
                              *geometry, *soil, *soil_water, 
                              T_avg, err);
  vegetation->initialize (time, *geometry, *soil, organic_matter.get (), err);
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

  static void load_soil_and_geometry (Syntax& syntax, AttributeList& alist)
  {
    Geometry1D::load_syntax (syntax, alist);
    Soil::load_syntax (syntax, alist);
  }

  ColumnStandardSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    Column::load_syntax (syntax, alist);
    syntax.add ("description", Syntax::String, Syntax::OptionalConst,
		"Description of this column."); 
    alist.add ("description", "Hansen et.al. 1990.");
    Librarian<Column>::add_type ("default", alist, syntax, &make);

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
    vegetation_alist.add ("crops", vector<AttributeList*> ());
    vegetation_alist.add ("ForcedLAI", vector<AttributeList*> ());
    vegetation_alist.add ("EpInterchange", 0.6);
    alist.add ("Vegetation", vegetation_alist);

    syntax.add ("Bioclimate", Librarian<Bioclimate>::library (), 
                Syntax::OptionalState, Syntax::Singleton,
                "The water and energy distribution among the crops.");
    syntax.add_submodule ("Surface", alist, Syntax::State,
                          "The upper border of the soil.",
                          Surface::load_syntax);
    syntax.add_submodule ("Soil", alist, Syntax::State,
                          "The numeric and physical soil properties.",
                          load_soil_and_geometry);
    syntax.add_submodule ("SoilWater", alist, Syntax::State,
                          "Soil water content and transportation.",
                          SoilWater1D::load_syntax);
    syntax.add_submodule ("SoilHeat", alist, Syntax::State,
                          "Soil heat and flux.",
                          SoilHeat1D::load_syntax);
    syntax.add_submodule ("Soltrans", alist, Syntax::State,
                          "Solute transport in soil.",
                          Soltrans1D::load_syntax);
    syntax.add_submodule ("SoilChemicals", alist, Syntax::State,
                          "Chemicals in the soil.",
                          SoilChemicals::load_syntax);
    syntax.add ("Chemistry", Librarian<Chemistry>::library (), 
                Syntax::State, Syntax::Sequence, 
                "Transformations applied to soil chemicals.");
    const vector<AttributeList*> empty;
    alist.add ("Chemistry", empty);
    syntax.add ("Groundwater", Librarian<Groundwater>::library (),
                "The groundwater level.");
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
  }
} column_syntax;
