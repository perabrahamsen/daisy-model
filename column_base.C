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

void 
ColumnBase::ridge (const AttributeList& al)
{ surface.ridge (soil, soil_water, al); }

void 
ColumnBase::irrigate_overhead (double flux, double temp, const IM&)
{
  assert (flux >= 0.0);
  bioclimate.irrigate_overhead (flux, temp);
}

void 
ColumnBase::irrigate_surface (double flux, double temp, const IM&)
{
  assert (flux >= 0.0);
  bioclimate.irrigate_surface (flux, temp);
}

void 
ColumnBase::irrigate_overhead (double flux, const IM&)
{
  assert (flux >= 0.0);
  bioclimate.irrigate_overhead (flux);
}

void 
ColumnBase::irrigate_surface (double flux, const IM&)
{
  assert (flux >= 0.0);
  bioclimate.irrigate_surface (flux);
}

void
ColumnBase::set_subsoil_irrigation (double flux, const IM&, 
				    double from, double to)
{
  assert (flux >= 0.0);
  assert (from <= 0.0);
  assert (to < from);
  soil_water.set_external_source (soil, flux * 0.1 /* mm->cm */, from, to);
}

void
ColumnBase::harvest (const Time& time, const string& crop_name,
		     double stub_length,
		     double stem_harvest,
		     double leaf_harvest, 
		     double sorg_harvest,
		     vector<const Harvest*>& harvest)
{ 
  vector<AM*> residuals;
  vegetation.harvest (name, crop_name, time, soil, 
		      bioclimate,
		      stub_length, 
		      stem_harvest, leaf_harvest, sorg_harvest,
		      harvest, residuals, harvest_DM, harvest_N, harvest_C); 
  add_residuals (residuals);
}


void 
ColumnBase::mix (const Time& time,
		 double from, double to, double)
{
  vector<AM*> residuals;
  vegetation.kill_all (name, time, soil, bioclimate, residuals);
  add_residuals (residuals);
  const double energy = soil_heat.energy (soil, soil_water, from, to);
  soil_water.mix (soil, from, to);
  soil_heat.set_energy (soil, soil_water, from, to, energy);
  soil_chemicals.mix (soil, soil_water, from, to);
  surface.unridge ();
}

void 
ColumnBase::swap (const Time& time, double from, double middle, double to)
{
  mix (time, from, middle, 1.0);
  mix (time, middle, to, 0.0);
  soil_water.swap (soil, from, middle, to);
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
ColumnBase::spray (const string& chemical, double amount) // [g/ha]
{ bioclimate.spray (chemical, amount / (100.0 * 100.0) /* ha->m^2 */); }

void 
ColumnBase::set_surface_detention_capacity (double height) // [mm]
{ surface.set_detention_capacity (height); }

double 
ColumnBase::daily_air_temperature () const
{ return bioclimate.daily_air_temperature (); }

double 
ColumnBase::soil_temperature (double height) const
{
  assert (height < 0);
  assert (height > soil.z (soil.size () - 1));
  return soil_heat.T (soil.interval_plus (height));
}

double 
ColumnBase::soil_water_potential (double height) const
{
  assert (height < 0);
  assert (height > soil.z (soil.size () - 1));
  return soil_water.h (soil.interval_plus (height));
}

double 
ColumnBase::soil_water_content (double from, double to) const
{
  assert (to <= from);
  assert (to <= 0.0);
  assert (to > soil.z (soil.size () - 1));
  return soil_water.content (soil, from, to);
}

double  
ColumnBase::crop_ds (const string& name) const // {[-1:2], Crop::DSremove}
{ return vegetation.DS_by_name (name); }

double 
ColumnBase::crop_dm (const string& name) const //[kg/ha], negative when no crop
{ return vegetation.DM_by_name (name); }

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
{ return bioclimate.get_evap_interception (); }

double 
ColumnBase::get_intercepted_water () const // [mm]
{ return bioclimate.get_intercepted_water (); }

double 
ColumnBase::get_net_throughfall () const // [mm/h]
{ return bioclimate.get_net_throughfall (); }

double 
ColumnBase::get_snow_storage () const // [mm]
{ return bioclimate.get_snow_storage (); }

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
ColumnBase::put_surface_chemical (const string& name, double amount) //[g/cm^2]
{ surface.put_chemical (name, amount); }

double 
ColumnBase::get_surface_chemical (const string& name) const // [g/cm^2]
{ return surface.get_chemical (name); }

double 
ColumnBase::get_temperature_at (unsigned int i) const // [°C]
{ return soil_heat.T (i); }

double 
ColumnBase::get_crop_h2o_uptake_at (unsigned int i) const // [cm³/cm³/h]
{ 
  vector<double> v;
  soil_water.get_sink (v);
  assert (v.size () > i);
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
}

bool
ColumnBase::check (bool require_weather,
		   const Time& from, const Time& to, Treelog& err) const
{
  const int n = soil.size ();
  bool ok = true;

  if (require_weather && !weather)
    {
      err.entry ("Weather unspecified");
      ok = false;
    }
  {
    Treelog::Open nest (err, "Weather");
    if (weather && !weather->check (from, to, err))
      ok = false;
  }
  {
    Treelog::Open nest (err, "Soil");
    if (!soil.check (err))
      ok = false;
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
    bool ok = true;
    Treelog::Open nest (err, "Transformations");
    for (vector<Transform*>::const_iterator i = transformations.begin ();
	 i != transformations.end ();
	 i++)
      {
	const Transform& transform = **i;
	Treelog::Open nest (err, transform.name);
	if (!transform.check (soil, err))
	  ok = false;
      }
  }
  if (!check_inner (err))
    ok = false;
  return ok;
}

bool
ColumnBase::check_inner (Treelog&) const
{ return true; }

void
ColumnBase::tick_base ()
{
  for (vector<Transform*>::const_iterator i = transformations.begin ();
       i != transformations.end ();
       i++)
    (*i)->tick (soil, soil_water, soil_chemicals);

  log_harvest_DM = harvest_DM;
  log_harvest_N = harvest_N;
  log_harvest_C = harvest_C;
  harvest_DM = 0.0;
  harvest_N = 0.0;
  harvest_C = 0.0;
}

void
ColumnBase::output (Log& log) const
{
  Column::output (log);
  log.open_geometry (soil);
  if (weather)
    output_derived (*weather, "weather", log);
  output_derived (bioclimate, "Bioclimate", log);
  output_submodule (surface, "Surface", log);
  output_submodule (soil, "Soil", log);
  output_submodule (soil_water, "SoilWater", log);
  output_submodule (soil_heat, "SoilHeat", log);
  output_submodule (soil_chemicals, "SoilChemicals", log);
  output_list (transformations, "Transformations", log, 
	       Librarian<Transform>::library ());
  output_derived (groundwater, "Groundwater", log);
  output_derived (vegetation, "Vegetation", log);
  output_inner (log);
  log.output ("harvest_DM", log_harvest_DM);
  log.output ("harvest_N", log_harvest_N);
  log.output ("harvest_C", log_harvest_C);
  log.close_geometry ();
}

void
ColumnBase::output_inner (Log&) const
{ }

static Bioclimate& 
get_bioclimate (const AttributeList& al)
{
  if (al.check ("Bioclimate"))
    return Librarian<Bioclimate>::create (al.alist ("Bioclimate"));
  AttributeList alist (Librarian<Bioclimate>::library ().lookup ("default"));
  alist.add ("type", "default");
  return Librarian<Bioclimate>::create (alist);
}

static AttributeList		// Needed for checkpoint.
add_bioclimate (const AttributeList& al)
{
  if (al.check ("Bioclimate"))
    return al;
  AttributeList parent (al);
  AttributeList child (Librarian<Bioclimate>::library ().lookup ("default"));
  child.add ("type", "default");
  parent.add ("Bioclimate", child);
  return parent;
}

ColumnBase::ColumnBase (const AttributeList& al)
  : Column (add_bioclimate (al)),
    weather (al.check ("weather") 
	     ? &Librarian<Weather>::create (al.alist ("weather"))
	     : NULL), 
    vegetation (Librarian<Vegetation>::create (al.alist ("Vegetation"))),
    bioclimate (get_bioclimate (al)),
    surface (al.alist ("Surface")),
    soil (al.alist ("Soil")),
    soil_water (al.alist ("SoilWater")),
    soil_heat (al.alist ("SoilHeat")),
    soil_chemicals (al.alist ("SoilChemicals")),
    transformations (map_create<Transform> (al.alist_sequence 
					    ("Transformations"))),
    groundwater (Librarian<Groundwater>::create (al.alist ("Groundwater"))),
    log_harvest_DM (0.0),
    log_harvest_N (0.0),
    log_harvest_C (0.0),
    harvest_DM (0.0),
    harvest_N (0.0),
    harvest_C (0.0)
{ }

void 
ColumnBase::initialize (const Time& time, Treelog& err, 
			const Weather* global_weather)
{
  if (weather)
    weather->initialize (time, err);
  if (!global_weather && !weather)
    return;
  const Weather& my_weather = *(weather ? weather : global_weather);
  groundwater.initialize (time, soil, err);
  soil_heat.initialize (alist.alist ("SoilHeat"), soil, time, my_weather);
  soil_water.initialize (alist.alist ("SoilWater"), soil, groundwater);
  soil_chemicals.initialize (alist.alist ("SoilChemicals"), soil, soil_water);
  for (vector<Transform*>::const_iterator i = transformations.begin ();
       i != transformations.end ();
       i++)
    (*i)->initialize (soil);
}

ColumnBase::~ColumnBase ()
{ 
  if (weather)
    delete weather;
  delete &vegetation;
  delete &bioclimate;
  sequence_delete (transformations.begin (), transformations.end ());
  delete &groundwater;
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
  syntax.add ("Transformations", Librarian<Transform>::library (), 
	      Syntax::Sequence, 
	      "Transformations applied to soil chemicals.");
  const vector<AttributeList*> empty;
  alist.add ("Transformations", empty);
  syntax.add ("Groundwater", Librarian<Groundwater>::library (),
	      "The groundwater level.");
  syntax.add ("harvest_DM", "g/m^2", Syntax::LogOnly, 
	      "Amount of DM removed by harvest this hour.");
  syntax.add ("harvest_N", "g/m^2", Syntax::LogOnly, 
	      "Amount of nitrogen removed by harvest this hour.");
  syntax.add ("harvest_C", "g/m^2", Syntax::LogOnly, 
	      "Amount of carbon removed by harvest this hour.");
  
}
