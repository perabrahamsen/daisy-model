// column_std.C

#include "column.h"
#include "crop.h"
#include "bioclimate.h"
#include "surface.h"
#include "soil.h"
#include "soil_water.h"
#include "soil_heat.h"
#include "soil_NH4.h"
#include "soil_NO3.h"
#include "organic_matter.h"
#include "nitrification.h"
#include "denitrification.h"
#include "groundwater.h"
#include "alist.h"
#include "syntax.h"
#include "library.h"
#include "log.h"
#include "filter.h"
#include "crop.h"
#include "im.h"
#include "am.h"

class Groundwater;

class ColumnStandard : public Column
{
  // Content.
private:
  CropList crops;
  Bioclimate& bioclimate;
  Surface surface;
  Soil soil;
  SoilWater soil_water;
  SoilHeat soil_heat;
  SoilNH4 soil_NH4;
  SoilNO3 soil_NO3;
  OrganicMatter organic_matter;
  Nitrification& nitrification;
  Denitrification denitrification;
  Groundwater& groundwater;

  // Actions.
public:
  void sow (const AttributeList& crop);
  void irrigate (double flux, double temp, 
		 const IM&, irrigation_from);
  void fertilize (const AttributeList&, const Time&);
  void fertilize (const AttributeList&, const Time&, double from, double to);
  void fertilize (const IM&);
  void fertilize (const IM&, double from, double to);
  vector<const Harvest*> harvest (const Time&, const string& name,
				  double stub_length,
				  double stem_harvest,
				  double leaf_harvest, 
				  double sorg_harvest);
  void mix (const Time&, double from, double to, double penetration = 1.0);
  void swap (const Time&, double from, double middle, double to);

  // Conditions.
  double soil_temperature (double height) const; // [ cm -> dg C]
  double soil_water_potential (double height) const; // [cm -> cm]
  double crop_ds (const string& crop) const; // {[-1:2], Crop::DSremove}
  double crop_dm (const string& crop) const; // [kg/ha], negative when no crop

  // Simulation.
public:
  void tick (const Time&, const Weather&);

  bool check () const;
  static bool check (const AttributeList&);
  bool check_am (const AttributeList& am) const 
  { return organic_matter.check_am (am); }
  void output (Log&, Filter&) const;

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
    { return surface.get_exfiltration (); }
  double get_evap_soil_surface () const // [mm/h]
    { return surface.get_evap_soil_surface (); }
  void put_ponding (double pond)	// [mm]
    { surface.put_ponding (pond); }
  void put_surface_no3 (double no3) // [g/cm^2]
    { surface.put_no3 (no3); }
  double get_surface_no3 () const // [g/cm^2]
    { return surface.get_no3 (); }

  // Create and Destroy.
public:
  Column& clone (const string& name) const
    { return *new ColumnStandard (*this, name); }

  ColumnStandard (const AttributeList&);
  ColumnStandard (const ColumnStandard& column, const string& name);
  void initialize (const AttributeList& al, const Time& time, 
		   const Weather& weather);
  ~ColumnStandard ();
};

void
ColumnStandard::sow (const AttributeList& al)
{
  Crop& crop = Librarian<Crop>::create (al);
  crop.initialize (soil);
  crops.push_back (&crop);
}

void 
ColumnStandard::irrigate (double flux, double temp, 
		 const IM& sm, irrigation_from from)
{
  surface.fertilize (sm * (flux / 10.0)); // [mm to cm]
  bioclimate.irrigate (flux, temp, from);
}

void
ColumnStandard::fertilize (const AttributeList& al, const Time& time)
{
  AM& am = AM::create (al, soil, time);
  organic_matter.add (am);
}

void 
ColumnStandard::fertilize (const AttributeList& al, const Time& time,
			   double from, double to)
{
  assert (to < from);
  AM& am = AM::create (al, soil, time);
  am.mix (soil, from, to);
  organic_matter.add (am);
}

void 
ColumnStandard::fertilize (const IM& im)
{
  surface.fertilize (im);
}

void 
ColumnStandard::fertilize (const IM& im, 
			   double from, double to)
{
  assert (to < from);
  soil_NO3.add (soil, soil_water, im.NO3, from, to);
  soil_NH4.add (soil, soil_water, im.NH4, from, to);
}

vector<const Harvest*>
ColumnStandard::harvest (const Time& time, const string& crop_name,
			 double stub_length,
			 double stem_harvest, double leaf_harvest, 
			 double sorg_harvest)
{
  vector<const Harvest*> harvest;
  
  // Harvest all crops of this type.
  for (CropList::iterator crop = crops.begin(); crop != crops.end(); crop++)
    if ((*crop)->name == crop_name)
      harvest.push_back (&(*crop)->harvest (name, time, 
					    soil, organic_matter,
					    stub_length, stem_harvest,
					    leaf_harvest, sorg_harvest, 
					    false));

  // Remove all dead crops.  There has to be a better way.
  bool removed;
  do
    {
      removed = false;
      for (CropList::iterator crop = crops.begin();
	   crop != crops.end();
	   crop++)
	if ((*crop)->name == crop_name)
	  {
	    if (Crop::ds_remove (*crop))
	      {
		delete *crop;
		crops.erase (crop); // This invalidates the iterator.
		// Restart the loop.
		removed = true;
		break;
	      }
	  }
    }
  while (removed);
  
  // Return the result.
  return harvest;
}

void 
ColumnStandard::mix (const Time& time,
		     double from, double to, double penetration)
{
  for (CropList::iterator crop = crops.begin(); crop != crops.end(); crop++)
    {
      (*crop)->kill (name, time, soil, organic_matter);
      delete *crop;
    }
  crops.erase (crops.begin (), crops.end ());
  assert (crops.size () == 0);
  const double energy = soil_heat.energy (soil, soil_water, from, to);
  soil_water.mix (soil, from, to);
  soil_heat.set_energy (soil, soil_water, from, to, energy);
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

double 
ColumnStandard::crop_ds (const string& name) const
{
  for (CropList::const_iterator crop = crops.begin();
       crop != crops.end();
       crop++)
    if ((*crop)->name == name)
      return (*crop)->DS ();
  return Crop::DSremove;
}

double 
ColumnStandard::crop_dm (const string& name) const
{
  for (CropList::const_iterator crop = crops.begin();
       crop != crops.end();
       crop++)
    if ((*crop)->name == name)
      return (*crop)->DM ();
  return -42.42e42;
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
  if (!soil_NO3.check (n))
    ok = false;
  if (!soil_NH4.check (n))
    ok = false;
  if (!organic_matter.check ())
    ok = false;

  if (!ok)
    cerr << "in column `" << name << "'\n";

  return ok;
}

bool
ColumnStandard::check (const AttributeList& al)
{
  bool ok = true;

  const AttributeList& om = al.alist ("OrganicMatter");
  if (!OrganicMatter::check (om))
    ok = false;

  if (!ok)
    cerr << "in column[" << al.name ("type") << "]\n";
  
  return ok;
}

void
ColumnStandard::tick (const Time& time, const Weather& weather)
{
  // Remove old source sink terms. 
  soil_water.clear (soil);
  soil_NO3.clear ();
  soil_NH4.clear ();
  surface.clear ();
  
  bioclimate.tick (surface, weather, time, crops, soil, soil_water, soil_heat);

  // Uptake and convertion of matter.
  for (CropList::iterator crop = crops.begin(); crop != crops.end(); crop++)
    (*crop)->tick (time, bioclimate, soil, organic_matter, 
		   soil_heat, soil_water, soil_NH4, soil_NO3);
  if (crops.size () > 1U)
    {
      // Make sure the crop which took first this time will be last next.
      crops.push_back (crops.front ());
      crops.pop_front ();
    }

  organic_matter.tick (soil, soil_water, soil_heat, groundwater, 
		       soil_NO3, soil_NH4);
  nitrification.tick (soil, soil_water, soil_heat, soil_NO3, soil_NH4,
		      groundwater);
  denitrification.tick (soil, soil_water, soil_heat, soil_NO3, 
			organic_matter, groundwater);
  groundwater.tick (time);

  // Transport.
  soil_water.tick (surface, groundwater, soil);
  soil_heat.tick (time, soil, soil_water, surface, groundwater, weather);
  soil_NO3.tick (soil, soil_water, surface.matter_flux ().NO3);
  soil_NH4.tick (soil, soil_water, surface.matter_flux ().NH4);

  // Once a month we clean up old AM from organic matter.
  if (time.hour () == 13 && time.mday () == 13)
    organic_matter.monthly (soil);
}

void
ColumnStandard::output (Log& log, Filter& filter) const
{
  log.open_geometry (soil);
  output_derived (bioclimate, "Bioclimate", log, filter);
  output_submodule (surface, "Surface", log, filter);
#if 0
  output_submodule (soil, "Soil", log, filter);
#endif
  output_submodule (soil_water, "SoilWater", log, filter);
  output_submodule (soil_heat, "SoilHeat", log, filter);
  output_submodule (soil_NH4, "SoilNH4", log, filter);
  output_submodule (soil_NO3, "SoilNO3", log, filter);
  if (filter.check ("OrganicMatter"))
    {
      log.open ("OrganicMatter");
      organic_matter.output (log, filter.lookup ("OrganicMatter"), soil);
      log.close ();
    }
  output_derived (nitrification, "Nitrification", log, filter);
  output_submodule (denitrification, "Denitrification", log, filter);
  output_derived (groundwater, "Groundwater", log, filter);
  output_list (crops, "crops", log, filter, Librarian<Crop>::library ());
  log.close_geometry ();
}

ColumnStandard::ColumnStandard (const ColumnStandard& column, 
				const string& name)
  : Column (name),
    // BUG: None of the constructors below have been implemented.
    crops (column.crops),
    bioclimate (column.bioclimate),
    surface (column.surface),
    soil (column.soil),
    soil_water (column.soil_water),
    soil_heat (column.soil_heat),
    soil_NH4 (column.soil_NH4),
    soil_NO3 (column.soil_NO3),
    organic_matter (column.organic_matter),
    nitrification (column.nitrification),
    denitrification (column.denitrification),
    groundwater (column.groundwater)
{ }
    

ColumnStandard::ColumnStandard (const AttributeList& al)
  : Column (al.name ("type")),
    crops (al.alist_sequence ("crops")),
    bioclimate (Librarian<Bioclimate>::create (al.alist ("Bioclimate"))),
    surface (al.alist ("Surface")),
    soil (al.alist ("Soil")),
    soil_water (al.alist ("SoilWater")),
    soil_heat (al.alist ("SoilHeat")),
    soil_NH4 (al.alist ("SoilNH4")),
    soil_NO3 (al.alist ("SoilNO3")),
    organic_matter (soil, al.alist ("OrganicMatter")),
    nitrification (Librarian<Nitrification>::create 
		   (al.alist ("Nitrification"))),
    denitrification (al.alist ("Denitrification")),
    groundwater (Librarian<Groundwater>::create (al.alist ("Groundwater")))
{ }

void ColumnStandard::initialize (const AttributeList& al, const Time& time,
				 const Weather& weather)
{
  soil_heat.initialize (al.alist ("SoilHeat"), soil, time, weather);
  soil_water.initialize (al.alist ("SoilWater"), soil, groundwater);
  soil_NH4.initialize (al.alist ("SoilNH4"), soil, soil_water);
  soil_NO3.initialize (al.alist ("SoilNO3"), soil, soil_water);
}

ColumnStandard::~ColumnStandard ()
{ 
  delete &nitrification;
}

#ifdef BORLAND_TEMPLATES
template class add_submodule<Surface>;
template class add_submodule<Soil>;
template class add_submodule<SoilWater>;
template class add_submodule<SoilHeat>;
template class add_submodule<SoilNH4>;
template class add_submodule<SoilNO3>;
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

    syntax.add ("description", Syntax::String, Syntax::Optional); 
    syntax.add ("crops", Librarian<Crop>::library (), 
		Syntax::State, Syntax::Sequence);
    alist.add ("crops", *new vector<AttributeList*>);
    syntax.add ("Bioclimate", Librarian<Bioclimate>::library (), 
		Syntax::State);
    add_submodule<Surface> ("Surface", syntax, alist);
    add_submodule<Soil> ("Soil", syntax, alist);
    add_submodule<SoilWater> ("SoilWater", syntax, alist);
    add_submodule<SoilHeat> ("SoilHeat", syntax, alist);
    add_submodule<SoilNH4> ("SoilNH4", syntax, alist);
    add_submodule<SoilNO3> ("SoilNO3", syntax, alist);
    add_submodule<OrganicMatter> ("OrganicMatter", syntax, alist);
    syntax.add ("Nitrification", Librarian<Nitrification>::library (),
		Syntax::State);
    add_submodule<Denitrification> ("Denitrification", syntax, alist);
    syntax.add ("Groundwater", Librarian<Groundwater>::library (),
		Syntax::State);
    
    Librarian<Column>::add_type ("default", alist, syntax, &make);
  }
} column_syntax;
