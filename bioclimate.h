// bioclimate.h

#ifndef BIOCLIMATE_H
#define BIOCLIMATE_H

#include "librarian.h"
#include "column.h"

struct Surface;
struct Weather;
struct AttributeList;
struct CropList;
struct Soil;
struct Syntax;
struct SoilWater;
struct SoilHeat;
struct Log;
struct Filter;

class Bioclimate
{ 
  // Content.
public:
  const string name;

  // Simulation.
public:
  virtual void tick (Surface&, const Weather&, const Time&, 
		     const CropList&, 
		     const Soil&, SoilWater&, const SoilHeat&) = 0;
  virtual void output (Log&, Filter&) const = 0;

  // Canopy.
public:
  virtual int NumberOfIntervals () const = 0;
  virtual double height (int) const = 0;
  virtual double PAR (int) const = 0;
  virtual double LAI () const = 0;
  virtual double daily_air_temperature () const = 0;
  virtual double day_length () const = 0;
  virtual double daily_global_radiation () const = 0;

  // Manager.
public:
  virtual void irrigate (double flux, double temp, 
			 Column::irrigation_from from) = 0;

  // Communication with external model.
  virtual double get_evap_interception () const; // [mm/h]
  virtual double get_intercepted_water () const; // [mm]
  virtual double get_net_precipitation () const; // [mm/h]
  virtual double get_snow_storage () const; // [mm]

  // Create.
protected:
  Bioclimate (const string& name);
public:
  virtual ~Bioclimate ();
};

static Librarian<Bioclimate> Bioclimate_init ("bioclimate");

#endif BIOCLIMATE_H
