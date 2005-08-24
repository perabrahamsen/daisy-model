// column_base.C
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

#include "column_base.h"

using namespace std;

static const double m2_per_cm2 = 0.0001;
static const double cm2_per_m2 = 1.0 / m2_per_cm2;

void 
ColumnBase::ridge (const AttributeList& al)
{ surface.ridge (soil, soil_water, al); }

void 
ColumnBase::irrigate_overhead (double flux, double temp, const IM&)
{
  daisy_assert (flux >= 0.0);
  bioclimate->irrigate_overhead (flux, temp);
}

void 
ColumnBase::irrigate_surface (double flux, double temp, const IM&)
{
  daisy_assert (flux >= 0.0);
  bioclimate->irrigate_surface (flux, temp);
}

void 
ColumnBase::irrigate_overhead (double flux, const IM&)
{
  daisy_assert (flux >= 0.0);
  bioclimate->irrigate_overhead (flux);
}

void 
ColumnBase::irrigate_surface (double flux, const IM&)
{
  daisy_assert (flux >= 0.0);
  bioclimate->irrigate_surface (flux);
}

void
ColumnBase::irrigate_subsoil (double flux, const IM&, 
                              double from, double to)
{
  daisy_assert (flux >= 0.0);
  daisy_assert (from <= 0.0);
  daisy_assert (to <= from); 
  soil_water.incorporate (soil, flux / 10.0 /* mm -> cm */, from, to);
  bioclimate->irrigate_subsoil (flux);
}

void
ColumnBase::harvest (const Time& time, const symbol crop_name,
		     double stub_length,
		     double stem_harvest,
		     double leaf_harvest, 
		     double sorg_harvest,
                     const bool combine,
		     vector<const Harvest*>& harvest, Treelog& msg)
{ 
  vector<AM*> residuals;
  double min_height = 100.0;
  vegetation->harvest (name, crop_name, time, soil, *bioclimate,
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
ColumnBase::mix (Treelog& msg, const Time& time,
		 double from, double to, double)
{
  vector<AM*> residuals;
  vegetation->kill_all (name, time, soil, *bioclimate, residuals, 
                        residuals_DM, residuals_N_top, residuals_C_top, 
                        residuals_N_soil, residuals_C_soil, msg);
  add_residuals (residuals);
  const double energy = soil_heat.energy (soil, soil_water, from, to);
  soil_water.mix (soil, from, to);
  soil_heat.set_energy (soil, soil_water, from, to, energy);
  soil_chemicals.mix (soil, soil_water, from, to);
  surface.unridge ();
}

void 
ColumnBase::swap (Treelog& msg, 
		  const Time& time, double from, double middle, double to)
{
  mix (msg, time, from, middle, 1.0);
  mix (msg, time, middle, to, 0.0);
  soil_water.swap (msg, soil, from, middle, to);
  soil_heat.swap (soil, from, middle, to);
  soil_chemicals.swap (soil, soil_water, from, middle, to);
}

void 
ColumnBase::set_porosity (double at, double Theta)
{ soil.set_porosity (soil.interval_plus (at), Theta); }

void 
ColumnBase::set_heat_source (double at, double value) // [W/m^2]
{
  const unsigned i = soil.interval_plus (at);
  const double dz = soil.dz (i);

  value *= 10^3;		// [W/m^2] -> [erg/cm^2/s]
  value *= 3600;		// [erg/cm^2/s] -> [erg/cm^2/h]
  value /= dz;			// [erg/cm^2/h] -> [erg/cm^3/h]
  
  soil_heat.set_source (i, value);
}

void 
ColumnBase::spray (symbol chemical, double amount) // [g/ha]
{ bioclimate->spray (chemical, amount / (100.0 * 100.0) /* ha->m^2 */); }

void 
ColumnBase::set_surface_detention_capacity (double height) // [mm]
{ surface.set_detention_capacity (height); }

double 
ColumnBase::daily_air_temperature () const
{ return bioclimate->daily_air_temperature (); }

double 
ColumnBase::daily_precipitation () const
{ return bioclimate->daily_precipitation (); }

double 
ColumnBase::daily_global_radiation () const
{ return bioclimate->daily_global_radiation (); }

double 
ColumnBase::soil_temperature (double height) const
{
  daisy_assert (height < 0);
  daisy_assert (height > soil.z (soil.size () - 1));
  return soil_heat.T (soil.interval_plus (height));
}

double 
ColumnBase::soil_water_potential (double height) const
{
  daisy_assert (height < 0);
  daisy_assert (height > soil.z (soil.size () - 1));
  return soil_water.h (soil.interval_plus (height));
}

double 
ColumnBase::soil_water_content (double from, double to) const
{
  daisy_assert (to <= from);
  daisy_assert (to <= 0.0);
  daisy_assert (to > soil.z (soil.size () - 1));
  return soil_water.content (soil, from, to);
}

double  
ColumnBase::crop_ds (const symbol name) const // {[-1:2], Crop::DSremove}
{ return vegetation->DS_by_name (name); }

double 
ColumnBase::crop_dm (const symbol name, const double height) const
  //[kg/ha], negative when no crop
{ return vegetation->DM_by_name (name, height); }

std::string
ColumnBase::crop_names () const
{ return vegetation->crop_names (); }

unsigned int 
ColumnBase::count_layers () const // Number of num. layers.
{ return soil.size (); }

double 
ColumnBase::get_dz (unsigned int i) const // Size of layer 'i'. [cm]
{ return soil.dz (i); }

void 
ColumnBase::put_water_pressure (const vector<double>& v) // [cm]
{ soil_water.put_h (soil, v); }

void 
ColumnBase::get_water_sink (vector<double>& v) const // [cm^3/cm^3/h]
{ soil_water.get_sink (v); }

double 
ColumnBase::get_evap_interception () const // [mm/h]
{ return bioclimate->get_evap_interception (); }

double 
ColumnBase::get_intercepted_water () const // [mm]
{ return bioclimate->get_intercepted_water (); }

double 
ColumnBase::get_net_throughfall () const // [mm/h]
{ return bioclimate->get_net_throughfall (); }

double 
ColumnBase::get_snow_storage () const // [mm]
{ return bioclimate->get_snow_storage (); }

double 
ColumnBase::get_exfiltration () const // [mm/h]
{ return surface.exfiltration (); }

double 
ColumnBase::get_evap_soil_surface () const // [mm/h]
{ return surface.evap_soil_surface (); }

void 
ColumnBase::put_ponding (double pond)	// [mm]
{ surface.put_ponding (pond); }

void 
ColumnBase::put_surface_chemical (symbol name, double amount) //[g/cm^2]
{ surface.put_chemical (name, amount); }

double 
ColumnBase::get_surface_chemical (symbol name) const // [g/cm^2]
{ return surface.get_chemical (name); }

double 
ColumnBase::get_temperature_at (unsigned int i) const // [°C]
{ return soil_heat.T (i); }

double 
ColumnBase::get_crop_h2o_uptake_at (unsigned int i) const // [cm³/cm³/h]
{ 
  vector<double> v;
  soil_water.get_sink (v);
  daisy_assert (v.size () > i);
  return v[i];
}

double 
ColumnBase::get_water_content_at (unsigned int i) const // [cm³/cm³]
{ return soil_water.Theta (i); }

void
ColumnBase::clear ()
{ 
  soil_water.clear (soil);
  soil_chemicals.clear ();

  harvest_DM = 0.0;
  harvest_N = 0.0;
  harvest_C = 0.0;
  residuals_DM = 0.0;
  residuals_N_top = 0.0;
  residuals_C_top = 0.0;
  fill (residuals_N_soil.begin (), residuals_N_soil.end (), 0.0);
  fill (residuals_C_soil.begin (), residuals_C_soil.end (), 0.0);
}

bool
ColumnBase::check (bool require_weather,
		   const Time& from, const Time& to, Treelog& err) const
{
  bool ok = true;
  const int n = soil.size ();
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
    if (!soil_water.check (n, err))
      ok = false;
  }
  {
    Treelog::Open nest (err, "SoilHeat");
    if (!soil_heat.check (n, err))
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
	if (!reaction.check (soil, err))
	  ok = false;
      }
  }
  {
    Treelog::Open nest (err, "Groundwater");
    if (!groundwater->check (err))
      ok = false;
  }
  if (!check_inner (err))
    ok = false;
  return ok;
}

bool 
ColumnBase::check_border (const double border, Treelog& err) const
{ return soil.check_border (border, err); }

void
ColumnBase::tick_base (Treelog& msg)
{
  for (vector<Chemistry*>::const_iterator i = chemistry.begin ();
       i != chemistry.end ();
       i++)
    (*i)->tick (soil, soil_water, soil_chemicals, msg);
}

void
ColumnBase::output (Log& log) const
{
  Log::Geo geo (log, soil);
  Column::output (log);
  if (weather)
    output_derived (weather, "weather", log);
  output_object (bioclimate, "Bioclimate", log);
  output_submodule (surface, "Surface", log);
  output_submodule (soil, "Soil", log);
  output_submodule (soil_water, "SoilWater", log);
  output_submodule (soil_heat, "SoilHeat", log);
  output_submodule (soil_chemicals, "SoilChemicals", log);
  output_list (chemistry, "Chemistry", log, 
	       Librarian<Chemistry>::library ());
  output_derived (groundwater, "Groundwater", log);
  output_derived (vegetation, "Vegetation", log);
  output_inner (log);
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
    log.output (N_symbol, soil.total (residuals_N_soil) * cm2_per_m2);
  static const symbol C_symbol ("residuals_C_root");
  if (log.check_leaf (C_symbol))
    log.output (C_symbol, soil.total (residuals_C_soil) * cm2_per_m2);
  static const symbol surface_water_symbol ("surface_water");
  if (log.check_leaf (surface_water_symbol))
    log.output (surface_water_symbol, (bioclimate->get_intercepted_water ()
                                       + bioclimate->get_snow_storage ()
                                       + surface.ponding ()));
}

void
ColumnBase::output_inner (Log&) const
{ }

static Bioclimate*
get_bioclimate (const AttributeList& al)
{
  if (al.check ("Bioclimate"))
    return Librarian<Bioclimate>::create (al.alist ("Bioclimate"));
  static const symbol default_symbol ("default");
  AttributeList alist (Librarian<Bioclimate>::library ()
		       .lookup (default_symbol));
  alist.add ("type", "default");
  return Librarian<Bioclimate>::create (alist);
}

ColumnBase::ColumnBase (const AttributeList& al)
  : Column (al),
    weather (al.check ("weather") 
	     ? Librarian<Weather>::create (al.alist ("weather"))
	     : NULL), 
    vegetation (Librarian<Vegetation>::create (al.alist ("Vegetation"))),
    bioclimate (get_bioclimate (al)),
    surface (al.alist ("Surface")),
    soil (al.alist ("Soil")),
    soil_water (al.alist ("SoilWater")),
    soil_heat (al.alist ("SoilHeat")),
    soil_chemicals (al.alist ("SoilChemicals")),
    chemistry (map_create<Chemistry> (al.alist_sequence 
					    ("Chemistry"))),
    groundwater (Librarian<Groundwater>::create (al.alist ("Groundwater"))),
    harvest_DM (0.0),
    harvest_N (0.0),
    harvest_C (0.0),
    residuals_DM (0.0),
    residuals_N_top (0.0),
    residuals_C_top (0.0)
{ }

bool
ColumnBase::initialize_common (const Time& time, Treelog& err, 
			       const Weather* global_weather)
{
  residuals_N_soil.insert (residuals_N_soil.begin (), soil.size (), 0.0);
  daisy_assert (residuals_N_soil.size () == soil.size ());
  residuals_C_soil.insert (residuals_C_soil.begin (), soil.size (), 0.0);
  daisy_assert (residuals_C_soil.size () == soil.size ());
  if (weather && !weather->initialize (time, err))
    return false;
  if (!global_weather && !weather)
    return false;
  const Weather& my_weather = *(weather ? weather : global_weather);
  bioclimate->initialize (my_weather, err);
  groundwater->initialize (soil, time, err);
  soil_heat.initialize (alist.alist ("SoilHeat"), soil, time, my_weather, err);
  soil_water.initialize (alist.alist ("SoilWater"), soil, *groundwater, err);
  soil_chemicals.initialize (alist.alist ("SoilChemicals"), soil, soil_water, 
			     err);
  for (vector<Chemistry*>::const_iterator i = chemistry.begin ();
       i != chemistry.end ();
       i++)
    (*i)->initialize (soil, err);
  return true;
}

ColumnBase::~ColumnBase ()
{ 
  if (weather)
    delete weather;
  sequence_delete (chemistry.begin (), chemistry.end ());
}

void
ColumnBase::load_syntax (Syntax& syntax, AttributeList& alist)
{
  Column::load_syntax (syntax, alist);
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
			Soil::load_syntax);
  syntax.add_submodule ("SoilWater", alist, Syntax::State,
			"Soil water content and transportation.",
			SoilWater::load_syntax);
  syntax.add_submodule ("SoilHeat", alist, Syntax::State,
			"Soil heat and flux.",
			SoilHeat::load_syntax);
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
}
