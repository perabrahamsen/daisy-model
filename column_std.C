// column_std.C

#include "column.h"
#include "bioclimate.h"
#include "surface.h"
#include "soil.h"
#include "soil_water.h"
#include "soil_heat.h"
#include "soil_NH4.h"
#include "soil_NO3.h"

#include "soil_chemicals.h"
#include "organic_matter.h"
#include "nitrification.h"
#include "denitrification.h"
#include "groundwater.h"
#include "alist.h"
#include "syntax.h"
#include "library.h"
#include "log.h"
#include "im.h"
#include "am.h"
#include "weather.h"
#include "vegetation.h"
#include "time.h"

class ColumnStandard : public Column
{
  // Content.
private:
  Vegetation vegetation;
  Bioclimate& bioclimate;
  Surface surface;
  Soil soil;
  SoilWater soil_water;
  SoilHeat soil_heat;
  SoilNH4 soil_NH4;
  SoilNO3 soil_NO3;
  SoilChemicals soil_chemicals;
  OrganicMatter organic_matter;
  Nitrification& nitrification;
  Denitrification denitrification;
  Groundwater& groundwater;

  // Actions.
public:
  void sow (const AttributeList& al)
    { vegetation.sow (al, soil, organic_matter); }
  void irrigate_top (double flux, double temp, const IM&);
  void irrigate_surface (double flux, double temp, const IM&);
  void fertilize (const AttributeList&);
  void fertilize (const AttributeList&, double from, double to);
  void fertilize (const IM&);
  void fertilize (const IM&, double from, double to);
  vector<const Harvest*> harvest (const Time& time, const string& crop_name,
				  double stub_length,
				  double stem_harvest,
				  double leaf_harvest, 
				  double sorg_harvest)
    { return vegetation.harvest (name, crop_name, time, soil, organic_matter,
				 bioclimate,
				 stub_length, 
				 stem_harvest, leaf_harvest, sorg_harvest); }
  void mix (const Time&, double from, double to, double penetration = 1.0);
  void swap (const Time&, double from, double middle, double to);
  void spray (const string& chemical, double amount) // [g/ha]
    { bioclimate.spray (chemical, amount / (100.0 * 100.0) /* ha->m^2 */); }

  // Conditions.
  double soil_temperature (double height) const; // [ cm -> dg C]
  double soil_water_potential (double height) const; // [cm -> cm]
  double  crop_ds (const string& name) const // {[-1:2], Crop::DSremove}
    { return vegetation.DS_by_name (name); }
  double crop_dm (const string& name) const // [kg/ha], negative when no crop
    { return vegetation.DM_by_name (name); }

  // Simulation.
public:
  void tick (const Time&, const Weather&);

  bool check () const;
  bool check_am (const AttributeList& am) const 
  { return organic_matter.check_am (am); }
  void output (Log&) const;

  // Communication with external model.
  unsigned int count_layers () const // Number of num. layers.
    { return soil.size (); }
  double get_dz (unsigned int i) const // Size of layer `i'. [cm]
    { return soil.dz (i); }
  void put_water_pressure (const vector<double>& v) // [cm]
    { soil_water.put_h (soil, v); }
  void get_water_sink (vector<double>& v) const // [cm^3/cm^3/h]
    { soil_water.get_sink (v); }
  void put_no3_m (const vector<double>& v) // [g/cm^3]
    { soil_NO3.put_M (soil, soil_water, v); }
  void get_no3_m (vector<double>& v) const // [g/cm^3]
    { 
      const unsigned int size = soil.size ();

      v.erase (v.begin (), v.end ());
      for (unsigned int i = 0; i < size; i++)
	v.push_back (soil_NO3.M (i));
    }
  double get_evap_interception () const // [mm/h]
    { return bioclimate.get_evap_interception (); }
  double get_intercepted_water () const // [mm]
    { return bioclimate.get_intercepted_water (); }
  double get_net_throughfall () const // [mm/h]
    { return bioclimate.get_net_throughfall (); }
  double get_snow_storage () const // [mm]
    { return bioclimate.get_snow_storage (); }
  double get_exfiltration () const // [mm/h]
    { return surface.exfiltration (); }
  double get_evap_soil_surface () const // [mm/h]
    { return surface.evap_soil_surface (); }
  void put_ponding (double pond)	// [mm]
    { surface.put_ponding (pond); }
  void put_surface_no3 (double no3) // [g/cm^2]
    { surface.put_no3 (no3); }
  double get_surface_no3 () const // [g/cm^2]
    { return surface.get_no3 (); }
  void put_surface_chemical (const string& name, double amount) // [g/cm^2]
    { bioclimate.put_surface_chemical (name, amount); }
  double get_surface_chemical (const string& name) const // [g/cm^2]
    { return bioclimate.get_surface_chemical (name); }
  double get_smb_c_at (unsigned int i) const //[g C/cm³]
    { return organic_matter.get_smb_c_at (i); }
  double get_co2_production_at (unsigned int i) const // [g C/cm³]
    { return organic_matter.CO2 (i); }
  double get_temperature_at (unsigned int i) const // [°C]
    { return soil_heat.T (i); }
  double get_crop_h2o_uptake_at (unsigned int i) const // [cm³/cm³/h]
    { 
      vector<double> v;
      soil_water.get_sink (v);
      assert (v.size () > i);
      return v[i];
    }
  double get_water_content_at (unsigned int i) const // [cm³/cm³]
    { return soil_water.Theta (i); }

  // Create and Destroy.
public:
  Column& clone (const string& name) const
    { 
      AttributeList new_alist (alist);
      // BUG: TODO: Log state of `this' to new_alist.
      new_alist.add ("type", name);
      return *new ColumnStandard (new_alist); 
    }
  ColumnStandard (const AttributeList&);
  void initialize (const Time& time, const Weather& weather);
  ~ColumnStandard ();
};

void 
ColumnStandard::irrigate_top (double flux, double temp, const IM& sm)
{
  assert (flux >= 0.0);
  assert (sm.NH4 >= 0.0);
  assert (sm.NO3 >= 0.0);
  surface.fertilize (sm * (flux / 10.0)); // [mm to cm]
  bioclimate.irrigate_top (flux, temp);
}

void 
ColumnStandard::irrigate_surface (double flux, double temp, const IM& sm)
{
  assert (flux >= 0.0);
  assert (sm.NH4 >= 0.0);
  assert (sm.NO3 >= 0.0);
  surface.fertilize (sm * (flux / 10.0)); // [mm to cm]
  bioclimate.irrigate_surface (flux, temp);
}

void
ColumnStandard::fertilize (const AttributeList& al)
{
  AM& am = AM::create (al, soil);
  organic_matter.add (am);
}

void 
ColumnStandard::fertilize (const AttributeList& al,
			   double from, double to)
{
  assert (to < from);
  AM& am = AM::create (al, soil);
  am.mix (soil, from, to);
  organic_matter.add (am);
}

void 
ColumnStandard::fertilize (const IM& im)
{
  assert (im.NH4 >= 0.0);
  assert (im.NO3 >= 0.0);
  surface.fertilize (im);
}

void 
ColumnStandard::fertilize (const IM& im, 
			   double from, double to)
{
  assert (im.NH4 >= 0.0);
  assert (im.NO3 >= 0.0);
  assert (to < from);
  soil_NO3.add (soil, soil_water, im.NO3, from, to);
  soil_NH4.add (soil, soil_water, im.NH4, from, to);
}

void 
ColumnStandard::mix (const Time& time,
		     double from, double to, double penetration)
{
  vegetation.kill_all (name, time, soil, organic_matter, bioclimate);
  const double energy = soil_heat.energy (soil, soil_water, from, to);
  soil_water.mix (soil, from, to);
  soil_heat.set_energy (soil, soil_water, from, to, energy);
  soil_chemicals.mix (soil, soil_water, from, to);
  soil_NO3.mix (soil, soil_water, from, to);
  soil_NH4.mix (soil, soil_water, from, to);
  organic_matter.mix (soil, from, to, penetration);
}

void 
ColumnStandard::swap (const Time& time, double from, double middle, double to)
{
  mix (time, from, middle, 1.0);
  mix (time, middle, to, 0.0);
  soil_water.swap (soil, from, middle, to);
  soil_heat.swap (soil, from, middle, to);
  soil_chemicals.swap (soil, soil_water, from, middle, to);
  soil_NO3.swap (soil, soil_water, from, middle, to);
  soil_NH4.swap (soil, soil_water, from, middle, to);
  organic_matter.swap (soil, from, middle, to);
}

double 
ColumnStandard::soil_temperature (double height) const
{
  assert (height < 0);
  assert (height > soil.z (soil.size () - 1));
  return soil_heat.T (soil.interval (height));
  }

double 
ColumnStandard::soil_water_potential (double height) const
{
  assert (height < 0);
  assert (height > soil.z (soil.size () - 1));
  return soil_water.h (soil.interval (height));
}

bool
ColumnStandard::check () const
{
  int n = soil.size ();
  bool ok = true;

  if (!soil.check ())
    ok = false;
  if (!soil_heat.check (n))
    ok = false;
  if (!soil_chemicals.check (n))
    ok = false;
  if (!soil_NO3.check (n))
    ok = false;
  if (!soil_NH4.check (n))
    ok = false;
  if (!organic_matter.check ())
    ok = false;

  if (!ok)
    CERR << "in column `" << name << "'\n";

  return ok;
}

void
ColumnStandard::tick (const Time& time, const Weather& weather)
{
  // Remove old source sink terms. 
  soil_water.clear (soil);
  soil_chemicals.clear ();
  soil_NO3.clear ();
  soil_NH4.clear ();

  // Early calculation.
  IM soil_top_conc;
  soil_top_conc.NO3 = soil_NO3.C (0) / 10.0; // [g/cm^3] -> [g/cm^2/mm]
  soil_top_conc.NH4 = soil_NH4.C (0) / 10.0; // [g/cm^3] -> [g/cm^2/mm]
  surface.mixture (soil_top_conc);
  soil_water.macro_tick (soil, surface);

  bioclimate.tick (surface, weather, 
		   vegetation, soil, soil_water, soil_heat);
  vegetation.tick (time, bioclimate, soil, organic_matter, 
		   soil_heat, soil_water, soil_NH4, soil_NO3);
  organic_matter.tick (soil, soil_water, soil_heat, 
		       soil_NO3, soil_NH4);
  nitrification.tick (soil, soil_water, soil_heat, soil_NO3, soil_NH4);
  denitrification.tick (soil, soil_water, soil_heat, soil_NO3, 
			organic_matter);
  groundwater.tick (time);

  // Transport.
  soil_heat.tick (time, soil, soil_water, surface, weather);
  soil_water.tick (soil, surface, groundwater);
  soil_chemicals.tick (soil, soil_water, soil_heat, organic_matter,
		       bioclimate.chemicals_down ());
  soil_NO3.tick (soil, soil_water, surface.matter_flux ().NO3);
  soil_NH4.tick (soil, soil_water, surface.matter_flux ().NH4);

  // Once a month we clean up old AM from organic matter.
  if (time.hour () == 13 && time.mday () == 13)
    organic_matter.monthly (soil);
}

void
ColumnStandard::output (Log& log) const
{
  Column::output (log);
  log.open_geometry (soil);
  output_derived (bioclimate, "Bioclimate", log);
  output_submodule (surface, "Surface", log);
#if 0
  output_submodule (soil, "Soil", log);
#endif
  output_submodule (soil_water, "SoilWater", log);
  output_submodule (soil_heat, "SoilHeat", log);
  output_submodule (soil_NH4, "SoilNH4", log);
  output_submodule (soil_NO3, "SoilNO3", log);
  output_submodule (soil_chemicals, "SoilChemicals", log);
  if (log.check ("OrganicMatter"))
    {
      log.open ("OrganicMatter");
      organic_matter.output (log, soil);
      log.close ();
    }
  output_derived (nitrification, "Nitrification", log);
  output_submodule (denitrification, "Denitrification", log);
  output_derived (groundwater, "Groundwater", log);
  output_submodule (vegetation, "Vegetation", log);
  log.close_geometry ();
}

ColumnStandard::ColumnStandard (const AttributeList& al)
  : Column (al),
    vegetation (al.alist ("Vegetation")),
    bioclimate (Librarian<Bioclimate>::create (al.alist ("Bioclimate"))),
    surface (al.alist ("Surface")),
    soil (al.alist ("Soil")),
    soil_water (al.alist ("SoilWater")),
    soil_heat (al.alist ("SoilHeat")),
    soil_NH4 (al.alist ("SoilNH4")),
    soil_NO3 (al.alist ("SoilNO3")),
    soil_chemicals (al.alist ("SoilChemicals")),
    organic_matter (al.alist ("OrganicMatter")),
    nitrification (Librarian<Nitrification>::create 
		   (al.alist ("Nitrification"))),
    denitrification (al.alist ("Denitrification")),
    groundwater (Librarian<Groundwater>::create (al.alist ("Groundwater")))
{ }

void ColumnStandard::initialize (const Time& time, const Weather& weather)
{
  groundwater.initialize (time, soil);
  soil_heat.initialize (alist.alist ("SoilHeat"), soil, time, weather);
  soil_water.initialize (alist.alist ("SoilWater"), soil, groundwater);
  soil_NH4.initialize (alist.alist ("SoilNH4"), soil, soil_water);
  soil_NO3.initialize (alist.alist ("SoilNO3"), soil, soil_water);
  soil_chemicals.initialize (alist.alist ("SoilChemicals"), soil, soil_water);
  organic_matter.initialize (alist.alist ("OrganicMatter"), soil);
  vegetation.initialize (soil, organic_matter);
}

ColumnStandard::~ColumnStandard ()
{ 
  delete &nitrification;
}

#ifdef BORLAND_TEMPLATES
template class add_submodule<Vegetation>;
template class add_submodule<Surface>;
template class add_submodule<Soil>;
template class add_submodule<SoilWater>;
template class add_submodule<SoilHeat>;
template class add_submodule<SoilNH4>;
template class add_submodule<SoilNO3>;
template class add_submodule<SoilChemicals>;
template class add_submodule<OrganicMatter>;
template class add_submodule<Denitrification>;
#endif

static struct ColumnStandardSyntax
{
  static Column& make (const AttributeList& al)
  {
    return *new ColumnStandard (al);
  }

  ColumnStandardSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    Column::load_syntax (syntax, alist);
    syntax.add ("description", Syntax::String, Syntax::OptionalConst,
		"Description of this column."); 
    alist.add ("description", "Hansen et.al. 1990.");
    add_submodule<Vegetation> ("Vegetation", syntax, alist, Syntax::State,
			       "The crops on the field.");
    syntax.add ("Bioclimate", Librarian<Bioclimate>::library (), 
		"The water and energy distribution among the crops.");
    add_submodule<Surface> ("Surface", syntax, alist, Syntax::State,
			    "The upper border of the soil.");
    add_submodule<Soil> ("Soil", syntax, alist, Syntax::Const, 
			 "The numeric and physical soil properties.");
    add_submodule<SoilWater> ("SoilWater", syntax, alist, Syntax::State,
			      "Soil water content and transportation.");
    add_submodule<SoilHeat> ("SoilHeat", syntax, alist, Syntax::State,
			     "Soil heat and flux.");
    add_submodule<SoilNH4> ("SoilNH4", syntax, alist, Syntax::State,
			    "Ammonium transport and adsorption in soil.");
    add_submodule<SoilNO3> ("SoilNO3", syntax, alist, Syntax::State,
			    "Nitrate transport in soil.");
    add_submodule<SoilChemicals> ("SoilChemicals", syntax, alist, 
				  Syntax::State,
				  "Chemicals in the soil.");
    add_submodule<OrganicMatter> ("OrganicMatter", syntax, alist,
				  Syntax::State, "\
The organic matter in the soil and on the surface.");
    syntax.add ("Nitrification", Librarian<Nitrification>::library (),
		"The soil nitrification process.");
    AttributeList nitrification_alist;
    nitrification_alist.add ("type", "soil");
    nitrification_alist.add ("k_10", 2.08333333333e-7); // 5e-5/24 [1/h]
    nitrification_alist.add ("k", 5.0e-5); // [gN/cm³]
    nitrification_alist.add ("active_underground", false);
    nitrification_alist.add ("active_groundwater", false);
    alist.add ("Nitrification", nitrification_alist);
    add_submodule<Denitrification> ("Denitrification", syntax, alist,
				    Syntax::State, "\
The denitrification process.");
    syntax.add ("Groundwater", Librarian<Groundwater>::library (),
		"The groundwater level.");
    
    Librarian<Column>::add_type ("default", alist, syntax, &make);
  }
} column_syntax;
