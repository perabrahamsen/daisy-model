// column_std.h

#ifndef COLUMN_STD_H
#define COLUMN_STD_H

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
  void tick (const Time&, const Weather&, const Groundwater&);

  // Communication with crops.
public:
  double SoilTemperature (double depth) const;
  double MaxRootingDepth () const;

  // Create and Destroy.
private:
  friend class ColumnStandardSyntax;
  static Column* make (string, const AttributeList&, const AttributeList&);
  ColumnStandard (string name, 
		  const AttributeList& par, const AttributeList& var);
public:
  ~ColumnStandard ();
};

#endif COLUMN_STD_H
