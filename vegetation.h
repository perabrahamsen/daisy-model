// vegetation.h

#ifndef VEGETATION_H
#define VEGETATION_H

#include "common.h"
#include <vector>

class Crop;
class Time;
class Bioclimate;
class Geometry;
class Soil;
class OrganicMatter;
class SoilHeat;
class SoilWater;
class SoilNH4;
class SoilNO3;
class AttributeList;
class CSMP;
class Harvest;
class Log;
class Filter;
class Syntax;
class AttributeList;

class Vegetation
{ 
  // Content.
  struct Implementation;	// Top secret internal state.
  Implementation& impl;

  // Canopy queries.
public:
  double LAI () const;		// Total LAI of all crops on this column [0-]
  double height () const;	// Max crop height in canopy [cm]
  double cover () const;	// Fraction of soil covered by crops [0-1]
  const CSMP& LAIvsH () const;	// LAI below given height [f: cm -> R]
  const CSMP& HvsLAI () const;	// Height with LAI below [f: R -> cm]
  double ACExt () const;	// Canopy extinction coefficient
  double ACRef () const;	// Canopy reflection coefficient 
  double ARExt () const;	// Radiation Extinction coefficient
  double EpFactor () const;	// Reference to potential evapotranspiration.
  double albedo () const;	// Another reflection factor.
  double interception_capacity () const;// Canopy water storage capacity [mm]

  // Individual crop queries.
public:
  double DS_by_name (const string& name) const;// Dev stage [-1:2] or DSremove.
  double DM_by_name (const string& name) const;// Shoot dry matter, [kg/ha].

  // Simulation
public:
  void tick (const Time&, const Bioclimate&, const Soil&,
	     OrganicMatter&, const SoilHeat&, const SoilWater&,
	     SoilNH4&, SoilNO3&); // Allow plants to grow (hourly).
  double transpiration (double potential_transpiration,	// Actual trans. [mm/h]
			double canopy_evaporation,
			const Soil& soil, SoilWater& soil_water);
  void kill_all (const string&, const Time&, const Geometry&, OrganicMatter&, 
		 Bioclimate&);
  vector<const Harvest*> harvest (const string& column_name,
				  const string& crop_name,
				  const Time&, const Geometry&, 
				  OrganicMatter&,
				  Bioclimate& bioclimate,
				  double stub_length,
				  double stem_harvest,
				  double leaf_harvest, 
				  double sorg_harvest);
  void sow (const AttributeList& al, const Geometry&);
  void output (Log&, Filter&) const;

  // Create and Destroy.
public:
  void initialize (const Geometry& geometry);
  static void load_syntax (Syntax&, AttributeList&);
  Vegetation (const AttributeList&);
  ~Vegetation ();
};

#endif VEGETATION_H
