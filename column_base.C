// column_base.C

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
		      harvest, residuals); 
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
ColumnBase::crop_ds (const string& name) const // {[-1:2], Crop::DSremove}
{ return vegetation.DS_by_name (name); }

double 
ColumnBase::crop_dm (const string& name) const //[kg/ha], negative when no crop
{ return vegetation.DM_by_name (name); }

unsigned int 
ColumnBase::count_layers () const // Number of num. layers.
{ return soil.size (); }

double 
ColumnBase::get_dz (unsigned int i) const // Size of layer `i'. [cm]
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

bool
ColumnBase::check (bool require_weather,
		   const Time& from, const Time& to, ostream& err) const
{
  const int n = soil.size ();
  bool ok = true;

  if (require_weather && !weather)
    {
      err << "Weather unspecified\n";
      ok = false;
    }
  if (weather && !weather->check (from, to, err))
    ok = false;
  if (!soil.check (err))
    ok = false;
  if (!soil_water.check (n, err))
    ok = false;
  if (!soil_heat.check (n, err))
    ok = false;
  if (!soil_chemicals.check (n, err))
    ok = false;
  if (!check_inner (err))
    ok = false;

  if (!ok)
    err << "in column `" << name << "'\n";

  return ok;
}

bool
ColumnBase::check_inner (ostream&) const
{ return true; }

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
  output_derived (groundwater, "Groundwater", log);
  output_derived (vegetation, "Vegetation", log);
  output_inner (log);
  log.close_geometry ();
}

void
ColumnBase::output_inner (Log&) const
{ }

ColumnBase::ColumnBase (const AttributeList& al)
  : Column (al),
    weather (al.check ("weather") 
	     ? &Librarian<Weather>::create (al.alist ("weather"))
	     : NULL), 
    vegetation (Librarian<Vegetation>::create (al.alist ("Vegetation"))),
    bioclimate (Librarian<Bioclimate>::create (al.alist ("Bioclimate"))),
    surface (al.alist ("Surface")),
    soil (al.alist ("Soil")),
    soil_water (al.alist ("SoilWater")),
    soil_heat (al.alist ("SoilHeat")),
    soil_chemicals (al.alist ("SoilChemicals")),
    groundwater (Librarian<Groundwater>::create (al.alist ("Groundwater")))
{ }

void 
ColumnBase::initialize (const Time& time, const Weather* global_weather)
{
  if (!global_weather && !weather)
    return;
  const Weather& my_weather = *(weather ? weather : global_weather);
  groundwater.initialize (time, soil);
  soil_heat.initialize (alist.alist ("SoilHeat"), soil, time, my_weather);
  soil_water.initialize (alist.alist ("SoilWater"), soil, groundwater);
  soil_chemicals.initialize (alist.alist ("SoilChemicals"), soil, soil_water);
}

ColumnBase::~ColumnBase ()
{ 
  if (weather)
    delete weather;
  delete &vegetation;
  delete &bioclimate;
  delete &groundwater;
}

#ifdef BORLAND_TEMPLATES
template class add_submodule<Vegetation>;
template class add_submodule<Surface>;
template class add_submodule<Soil>;
template class add_submodule<SoilWater>;
template class add_submodule<SoilHeat>;
template class add_submodule<SoilChemicals>;
#endif

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
  vegetation_alist.add ("crops", *new vector<AttributeList*>);
  alist.add ("Vegetation", vegetation_alist);

  syntax.add ("Bioclimate", Librarian<Bioclimate>::library (), 
	      "The water and energy distribution among the crops.");
  add_submodule<Surface> ("Surface", syntax, alist, Syntax::State,
			  "The upper border of the soil.");
  add_submodule<Soil> ("Soil", syntax, alist, Syntax::State,
		       "The numeric and physical soil properties.");
  add_submodule<SoilWater> ("SoilWater", syntax, alist, Syntax::State,
			    "Soil water content and transportation.");
  add_submodule<SoilHeat> ("SoilHeat", syntax, alist, Syntax::State,
			   "Soil heat and flux.");
  add_submodule<SoilChemicals> ("SoilChemicals", syntax, alist, 
				Syntax::State,
				"Chemicals in the soil.");
  syntax.add ("Groundwater", Librarian<Groundwater>::library (),
	      "The groundwater level.");
}
