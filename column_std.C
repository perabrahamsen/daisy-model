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
#include "alist.h"
#include "syntax.h"
#include "library.h"
#include "log.h"
#include "filter.h"
#include "crop.h"
#include "im.h"
#include "am.h"
#include <algo.h>

class Groundwater;

class ColumnStandard : public Column
{
  // Content.
private:
  CropList crops;
  Bioclimate bioclimate;
  Surface surface;
  Soil soil;
  SoilWater soil_water;
  SoilHeat soil_heat;
  SoilNH4 soil_NH4;
  SoilNO3 soil_NO3;
  OrganicMatter organic_matter;
  Nitrification& nitrification;
  Denitrification denitrification;

  // Actions.
public:
  void sow (const AttributeList& crop);
  void irrigate (double flux, double temp, 
		 const IM&, irrigation_from);
  void fertilize (const AttributeList&, const Time&);
  void fertilize (const AttributeList&, const Time&, double from, double to);
  void fertilize (const IM&);
  void fertilize (const IM&, double from, double to);
  void fertilize (const Time&, const vector<const AttributeList*>&,
		  string name, string part, 
		  double C, double N);
  void fertilize (const Time&, const vector<const AttributeList*>&,
		  string name, const vector<double>& density, 
		  double C, double N);
  vector<const Harvest*> harvest (const Time&, const string name,
				  double stub_length,
				  double stem_harvest,
				  double leaf_harvest, 
				  double sorg_harvest,
				  double dead_harvest);
  void mix (const Time&, double from, double to, double penetration = 1.0);
  void swap (const Time&, double from, double middle, double to);

  // Simulation.
public:
  void tick (const Time&, const Weather&, Groundwater&);

  bool check () const;
  static bool check (const AttributeList&);
  bool check_am (const AttributeList& am) const 
  { return organic_matter.check_am (am); }
  void output (Log&, const Filter&) const;

  // Create and Destroy.
private:
  friend class ColumnStandardSyntax;
  static Column* make (const AttributeList&);
  ColumnStandard (const AttributeList&);
public:
  ~ColumnStandard ();
};

void
ColumnStandard::sow (const AttributeList& crop)
{
  string name = crop.name ("type");
  if (!Crop::library ().check (name))
    cerr << "Cannot sow unknown crop `" << name << "'\n";
  else if (!Crop::library ().syntax (name).check (crop, name))
    cerr << "Cannot sow incomplete crop `" << name << "'\n";
  else
    {
      assert (crops.size () == 0); // Multiple crops not implemented.
      crops.push_back (Crop::create (crop, soil.size ()));
      assert (crops.size () == 1); // Multiple crops not implemented.
    }
}

void 
ColumnStandard::irrigate (double flux, double temp, 
		 const IM& sm, irrigation_from from)
{
  surface.fertilize (sm * (flux / 10.0)); // [mm to cm]
  bioclimate.irrigate (flux, temp, from);
}

void 
ColumnStandard::fertilize (const Time& time, 
			   const vector<const AttributeList*>& om, 
			   string name, string part, 
			   double C, double N)
{
  vector<double> content;
  organic_matter.add (AM::create (soil, time, om, name, part,
				  // g/m² -> g/cm²
				  C * 1e-4, N * 1e-4, content));
}

void 
ColumnStandard::fertilize (const Time& time,
			   const vector<const AttributeList*>& om, 
			   string name, const vector<double>& density, 
			   double C, double N)
{
  vector<double> content;
  for (unsigned int i = 0; i < density.size (); i++)
    content.push_back (density[i] * soil.dz (i));
		       
  organic_matter.add (AM::create (soil, time, om, name, "root",
				  // g/m² -> g/cm²
				  C * 1e-4, N * 1e-4, content));
}

void 
ColumnStandard::fertilize (const AttributeList& al, const Time& time)
{
  organic_matter.add (AM::create (al, soil, time));
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
ColumnStandard::harvest (const Time& time, const string name,
			 double stub_length,
			 double stem_harvest, double leaf_harvest, 
			 double sorg_harvest, double dead_harvest)
{
  vector<const Harvest*> harvest;
  for (CropList::iterator crop = crops.begin(); crop != crops.end(); crop++)
    if ((*crop)->name == name)
      {
	harvest.push_back (&(*crop)->harvest (time, *this, 
					      stub_length, 
					      stem_harvest, leaf_harvest,
					      sorg_harvest, dead_harvest,
					      false));
	if (Crop::ds_remove (*crop))
	  {
	    // BUG? I hope it is allowed to delete members of a list
	    // while iterating over it.
	    delete *crop;
	    crops.erase (crop);
	  }
      }
  return harvest;
}

void 
ColumnStandard::mix (const Time& time,
		     double from, double to, double penetration)
{
  for (CropList::iterator crop = crops.begin(); crop != crops.end(); crop++)
    {
      (*crop)->kill (time, *this);
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

  const AttributeList& om = al.list ("OrganicMatter");
  if (!OrganicMatter::check (om))
    ok = false;

  if (!ok)
    cerr << "in column[" << al.name ("type") << "]\n";
  
  return ok;
}

void
ColumnStandard::tick (const Time& time, 
		      const Weather& weather, Groundwater& groundwater)
{
  // Remove old source sink terms. 
  soil_water.clear ();
  soil_NO3.clear (soil, soil_water);
  soil_NH4.clear ();
  surface.clear ();
  
  bioclimate.tick (surface, weather, time, crops, soil, soil_water, soil_heat);

  // Uptake and convertion of matter.
  for (CropList::iterator crop = crops.begin(); crop != crops.end(); crop++)
    (*crop)->tick (time, bioclimate, soil, soil_heat, soil_water, 
		   soil_NH4, soil_NO3);
  organic_matter.tick (soil, soil_water, soil_heat, soil_NO3, soil_NH4);
  nitrification.tick (soil, soil_water, soil_heat, soil_NO3, soil_NH4);
  denitrification.tick (soil, soil_water, soil_heat, soil_NO3, organic_matter);

  // Transport.
  soil_water.tick (surface, groundwater, soil);
  soil_heat.tick (time, soil, soil_water, surface, groundwater);
  soil_NO3.tick (soil, soil_water, surface.matter_flux ().NO3);
  soil_NH4.tick (soil, soil_water, surface.matter_flux ().NH4);

  // Once a month we clean up old AM from organic matter.
  if (time.hour () == 13 && time.mday () == 13)
    organic_matter.monthly (soil);
}

void
ColumnStandard::output (Log& log, const Filter& filter) const
{
  output_submodule (bioclimate, "Bioclimate", log, filter);
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
  output_list (crops, "crops", log, filter);
}

ColumnStandard::ColumnStandard (const AttributeList& al)
  : Column (al.name ("type")),
    crops (al.list_sequence ("crops")),
    bioclimate (al.list ("Bioclimate")),
    surface (al.list ("Surface")),
    soil (al.list ("Soil")),
    soil_water (soil, al.list ("SoilWater")),
    soil_heat (soil, soil_water, al.list ("SoilHeat")),
    soil_NH4 (soil, soil_water, al.list ("SoilNH4")),
    soil_NO3 (soil, soil_water, al.list ("SoilNO3")),
    organic_matter (soil, al.list ("OrganicMatter")),
    nitrification (Librarian<Nitrification>::create 
		   (al.list ("Nitrification"))),
    denitrification (al.list ("Denitrification"))
{ }

ColumnStandard::~ColumnStandard ()
{ 
  delete &nitrification;
}

// Add the Column syntax to the syntax table.
Column*
ColumnStandard::make (const AttributeList& al)
{
  if (check (al))
    return new ColumnStandard (al);
  else
    return NULL;
}

static struct ColumnStandardSyntax
{
  ColumnStandardSyntax ();
} column_syntax;

ColumnStandardSyntax::ColumnStandardSyntax ()
{ 
  Syntax& syntax = *new Syntax ();
  AttributeList& alist = *new AttributeList ();

  syntax.add ("crops", Crop::library (), Syntax::State, Syntax::Sequence);
  alist.add ("crops", *new vector<const AttributeList*>);

  add_submodule<Bioclimate> ("Bioclimate", syntax, alist);
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

  Column::add_type ("default", alist, syntax, &ColumnStandard::make);
}
