// column_std.C

#include "column_std.h"
#include "alist.h"
#include "syntax.h"
#include "library.h"
#include "log.h"
#include "filter.h"
#include "crop.h"

void
ColumnStandard::sow (const Library& croplib, string crop, Log& log)
{
  if (croplib.check (crop))
    {
      const AttributeList& values = croplib.lookup (crop);
	
      if (syntax_table->syntax ("crop")->check (crop, values, log))
	crops.push_back (new Crop (crop, values));
    }
  else
    cerr << "Cannot sow unknow crop `" << crop << "'\n";
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
		      const Weather& weather, const Groundwater& groundwater)
{
  cout << "Column `" << name << "' tick\n"; 
  
  bioclimate.tick (surface, weather, crops);
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
  if (filter->check ("Surface"))
    surface.output (log, filter->lookup ("Surface"));
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

ColumnStandard::ColumnStandard (string n,
				const AttributeList& par, 
				const AttributeList& var)
  : Column (n),
    bioclimate (par.list ("Bioclimate"), var.list ("Bioclimate")),
    surface (par.list ("Surface"), var.list ("Surface")),
    soil (par.list ("Soil")),
    soil_water (soil, par.list ("SoilWater"), var.list ("SoilWater")),
    soil_heat (par.list ("SoilHeat"), var.list ("SoilHeat")),
    soil_NH4 (par.list ("SoilNH4"), var.list ("SoilNH4")),
    soil_NO3 (par.list ("SoilNO3"), var.list ("SoilNO3")),
    organic_matter (par.list ("OrganicMatter"), var.list ("OrganicMatter")),
    nitrification (par.list ("Nitrification"), var.list ("Nitrification")),
    denitrification (par.list ("Denitrification"), 
		     var.list ("Denitrification"))
{ 
  // Crops.
}

ColumnStandard::~ColumnStandard ()
{ }

// Add the Column syntax to the syntax table.
static Column*
ColumnStandard_constructor (string name, 
			    const AttributeList& par, 
			    const AttributeList& var)
{
  return new ColumnStandard (name, par, var);
}

static struct ColumnSyntax
{
  const Syntax* parameters ();
  const Syntax* variables ();
  ColumnSyntax ();
} column_syntax;

ColumnSyntax::ColumnSyntax ()
{ 
  Column::add_type ("column",
		    AttributeList::empty, parameters (),
		    AttributeList::empty, variables (),
		    &ColumnStandard_constructor);
}

const Syntax*
ColumnSyntax::parameters ()
{ 
  Syntax* par = new Syntax ();

  Syntax* bioclimate = new Syntax ();
  bioclimate->add ("NoOfIntervals", Syntax::Integer);
  par->add ("Bioclimate", bioclimate);
  Syntax* surface = new Syntax ();
  par->add ("Surface", surface);
  Syntax* soil = new Syntax ();
  soil->add ("horizons", Syntax::Soil);
  soil->add ("zplus", Syntax::Array);
  par->add ("Soil", soil);
  par->add ("SoilWater", SoilWater::parameter_syntax ());
  Syntax* soil_heat = new Syntax ();
  par->add ("SoilHeat", soil_heat);
  Syntax* soil_NH4 = new Syntax ();
  par->add ("SoilNH4", soil_NH4);
  Syntax* soil_NO3 = new Syntax ();
  par->add ("SoilNO3", soil_NO3);
  Syntax* organic_matter = new Syntax ();
  par->add ("OrganicMatter", organic_matter);
  Syntax* nitrification = new Syntax ();
  par->add ("Nitrification", nitrification);
  Syntax* denitrification = new Syntax ();
  par->add ("Denitrification", denitrification);

  return par;
}

const Syntax*
ColumnSyntax::variables ()
{ 
  Syntax* var = new Syntax ();

  Syntax* bioclimate = new Syntax ();
  var->add ("Bioclimate", bioclimate);
  Syntax* surface = new Syntax ();
  var->add ("Surface", surface);
  Syntax* soil = new Syntax ();
  var->add ("Soil", soil);
  var->add ("SoilWater", SoilWater::variable_syntax ());
  Syntax* soil_heat = new Syntax ();
  soil_heat->add ("T", Syntax::Array);
  var->add ("SoilHeat", soil_heat);
  Syntax* soil_NH4 = new Syntax ();
  var->add ("SoilNH4", soil_NH4);
  Syntax* soil_NO3 = new Syntax ();
  var->add ("SoilNO3", soil_NO3);
  Syntax* organic_matter = new Syntax ();
  var->add ("OrganicMatter", organic_matter);
  Syntax* nitrification = new Syntax ();
  var->add ("Nitrification", nitrification);
  Syntax* denitrification = new Syntax ();
  var->add ("Denitrification", denitrification);
  var->add ("crops", Syntax::Crops);

  return var;
}
