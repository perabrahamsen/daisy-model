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
  void sow (const AttributeList& crop, Log&);

  bool check (Log&) const;
  void output (Log&, const Filter*) const;
private:
  void output_crops (Log& log, const Filter* filter) const;

  // Simulation.
public:
  void tick (const Time&, const Weather&, Groundwater&);

  // Communication with crops.
public:
  double SoilTemperature (double depth) const;
  double MaxRootingDepth () const;

  // Create and Destroy.
private:
  friend class ColumnStandardSyntax;
  static Column* make (const AttributeList&);
  ColumnStandard (const AttributeList&);
public:
  ~ColumnStandard ();
};

void
ColumnStandard::sow (const AttributeList& crop, Log& log)
{
  string name = crop.name ("type");
  if (!Crop::library ().check (name))
    log.err () << "Cannot sow unknown crop `" << name << "'\n";
  else if (!Crop::library ().syntax (name).check (name, crop, log))
    log.err () << "Cannot sow incomplete crop `" << name << "'\n";
  else
    crops.push_back (Crop::create (crop));
}

bool
ColumnStandard::check (Log& log) const
{
  int n = soil.size ();
  bool ok = (   soil.check (log)
	     && soil_heat.check (log, n));
  return ok;
}

void
ColumnStandard::tick (const Time& time, 
		      const Weather& weather, Groundwater& groundwater)
{
  cout << "Column `" << name << "' tick\n"; 
  
  bioclimate.tick (surface, weather, crops, soil, soil_water);
  soil_heat.tick (surface, bioclimate);
  soil_water.tick (surface, groundwater, soil);
  for (CropList::iterator crop = crops.begin(); crop != crops.end(); crop++)
    (*crop)->tick (time, *this, bioclimate);
}

void
ColumnStandard::output (Log& log, const Filter* filter) const
{
  log.open (name);
#if 0
  if (filter->check ("Bioclimate"))
    bioclimate.output (log, filter->lookup ("Bioclimate"));
#endif
  if (filter->check ("Surface"))
    surface.output (log, filter->lookup ("Surface"));
#if 0
  if (filter->check ("Soil"))
    soil.output (log, filter->lookup ("Soil"));
#endif
  if (filter->check ("SoilWater"))
    soil_water.output (log, filter->lookup ("SoilWater"));
#if 0
  if (filter->check ("SoilHeat"))
    soil_heat.output (log, filter->lookup ("SoilHeat"));
  if (filter->check ("SoilNH4"))
    soil_NH4.output (log, filter->lookup ("SoilNH4"));
  if (filter->check ("SoilNO3"))
    soil_NO3.output (log, filter->lookup ("SoilNO3"));
  if (filter->check ("OrganicMatter"))
    organic_matter.output (log, filter->lookup ("OrganicMatter"));
  if (filter->check ("Nitrification"))
    nitrification.output (log, filter->lookup ("Nitrification"));
  if (filter->check ("Denitrification"))
    denitrification.output (log, filter->lookup ("Denitrification"));
#endif
  if (filter->check ("crops"))
    output_crops (log, filter->lookup ("crops"));
  log.close ();
}

void
ColumnStandard::output_crops (Log& log, const Filter* filter) const
{
  log.open ("crops");
  for (CropList::const_iterator crop = crops.begin(); 
       crop != crops.end();
       crop++)
    {
      if (filter->check ((*crop)->name))
	(*crop)->output (log, filter->lookup ((*crop)->name));
    }
  log.close ();
}

double 
ColumnStandard::SoilTemperature (double depth) const
{
  return soil_heat.temperature (soil.interval_plus (depth));
}

double 
ColumnStandard::MaxRootingDepth () const
{
  return soil.MaxRootingDepth ();
}

ColumnStandard::ColumnStandard (const AttributeList& al)
  : Column (al.name ("type")),
    crops (al.sequence ("crops")),
    bioclimate (al.list ("Bioclimate")),
    surface (al.list ("Surface")),
    soil (al.list ("Soil")),
    soil_water (soil, al.list ("SoilWater")),
    soil_heat (al.list ("SoilHeat")),
    soil_NH4 (al.list ("SoilNH4")),
    soil_NO3 (al.list ("SoilNO3")),
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

  syntax.add_sequence ("crops", Crop::library ());
  
  ADD_SUBMODULE (syntax, alist, Bioclimate);
  ADD_SUBMODULE (syntax, alist, Surface);
  ADD_SUBMODULE (syntax, alist, Soil);
  ADD_SUBMODULE (syntax, alist, SoilWater);
  ADD_SUBMODULE (syntax, alist, SoilHeat);
  ADD_SUBMODULE (syntax, alist, SoilNH4);
  ADD_SUBMODULE (syntax, alist, SoilNO3);
  ADD_SUBMODULE (syntax, alist, OrganicMatter);
  ADD_SUBMODULE (syntax, alist, Nitrification);
  ADD_SUBMODULE (syntax, alist, Denitrification);

  Column::add_type ("column", alist, syntax, &ColumnStandard::make);
}
