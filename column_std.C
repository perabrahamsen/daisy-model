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
  Nitrification nitrification;
  Denitrification denitrification;

  // Actions.
public:
  void sow (const AttributeList& crop);
  void irrigate (double flux, double temp);

  // Simulation.
public:
  void tick (const Time&, const Weather&, Groundwater&);

  bool check () const;
  void output (Log&, const Filter&) const;
private:
  void output_crops (Log& log, const Filter& filter) const;

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
    crops.push_back (Crop::create (crop, soil.size ()));
}

void
ColumnStandard::irrigate (const double flux, const double temp)
{
  bioclimate.Irrigate (flux, temp);
}

bool
ColumnStandard::check () const
{
  int n = soil.size ();
  bool ok = (soil.check ()
	     && soil_heat.check (n)
	     && soil_NO3.check (n));
  return ok;
}

void
ColumnStandard::tick (const Time& time, 
		      const Weather& weather, Groundwater& groundwater)
{
  soil_water.clear ();
  soil_NO3.clear ();
  bioclimate.tick (surface, weather, crops, soil, soil_water);
  soil_heat.tick (surface, bioclimate);
  soil_water.tick (surface, groundwater, soil);
  for (CropList::iterator crop = crops.begin(); crop != crops.end(); crop++)
    (*crop)->tick (time, bioclimate, soil, soil_heat);
  soil_NO3.tick (soil, soil_water, 0.0);
}

void
ColumnStandard::output (Log& log, const Filter& filter) const
{
  log.open (name);
  output_submodule (bioclimate, "Bioclimate", log, filter);
  output_submodule (surface, "Surface", log, filter);
#if 0
  output_submodule (soil, "Soil", log, filter);
#endif
  output_submodule (soil_water, "SoilWater", log, filter);
#if 0
  output_submodule (soil_heat, "SoilHeat", log, filter);
  output_submodule (soil_NH4, "SoilNH4", log, filter);
#endif
  output_submodule (soil_NO3, "SoilNO3", log, filter);
#if 0
  output_submodule (organic_matter, "OrganicMatter", log, filter);
  output_submodule (nitrification, "Nitrification", log, filter);
  output_submodule (denitrification, "Denitrification", log, filter);
#endif
  if (filter.check ("crops"))
    output_crops (log, filter.lookup ("crops"));
  log.close ();
}

void
ColumnStandard::output_crops (Log& log, const Filter& filter) const
{
  log.open ("crops");
  for (CropList::const_iterator crop = crops.begin(); 
       crop != crops.end();
       crop++)
    {
      if (filter.check ((*crop)->name))
	(*crop)->output (log, filter.lookup ((*crop)->name));
    }
  log.close ();
}

ColumnStandard::ColumnStandard (const AttributeList& al)
  : Column (al.name ("type")),
    crops (al.list_sequence ("crops")),
    bioclimate (al.list ("Bioclimate")),
    surface (al.list ("Surface")),
    soil (al.list ("Soil")),
    soil_water (soil, al.list ("SoilWater")),
    soil_heat (al.list ("SoilHeat")),
    soil_NH4 (soil, soil_water, al.list ("SoilNH4")),
    soil_NO3 (soil, soil_water, al.list ("SoilNO3")),
    organic_matter (al.list ("OrganicMatter")),
    nitrification (al.list ("Nitrification")),
    denitrification (al.list ("Denitrification"))
{ }

ColumnStandard::~ColumnStandard ()
{ }

// Add the Column syntax to the syntax table.
Column*
ColumnStandard::make (const AttributeList& al)
{
  return new ColumnStandard (al);
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
  
  add_submodule<Bioclimate> ("Bioclimate", syntax, alist);
  add_submodule<Surface> ("Surface", syntax, alist);
  add_submodule<Soil> ("Soil", syntax, alist);
  add_submodule<SoilWater> ("SoilWater", syntax, alist);
  add_submodule<SoilHeat> ("SoilHeat", syntax, alist);
  add_submodule<SoilNH4> ("SoilNH4", syntax, alist);
  add_submodule<SoilNO3> ("SoilNO3", syntax, alist);
  add_submodule<OrganicMatter> ("OrganicMatter", syntax, alist);
  add_submodule<Nitrification> ("Nitrification", syntax, alist);
  add_submodule<Denitrification> ("Denitrification", syntax, alist);

  Column::add_type ("column", alist, syntax, &ColumnStandard::make);
}
