// bioclimate.h

#ifndef BIOCLIMATE_H
#define BIOCLIMATE_H

#include "librarian.h"
#include "column.h"

struct Surface;
struct Weather;
struct AttributeList;
struct Vegetation;
struct Soil;
struct Syntax;
struct SoilWater;
struct SoilHeat;
struct Log;
struct Filter;
struct Chemicals;

class Bioclimate
{ 
  // Content.
public:
  const string name;

  // FAO utility functions.
  static double CanopyResistance (double LAI /* [m^2/m^2] */); // [s/m]
  static double RefCanopyResistance (void); // [s/m]
  static double ZeroPlaneDisplacement (double CropHeight /* [m] */); // [m]
  static double RoughnessHeight_Momentum (double CropHeight /* [m] */); // [m]
  static double RoughnessHeight_Heat (double CropHeight /* [m] */); // [m]
  static double AerodynamicResistance (double CropHeight /* [m] */,
				       double ScreenHeight /* [m] */,
				       double U /* [m/s] */); // [s/m]
  static double RefAerodynamicResistance (double U2 /* [m/s] */); // [s/m]

  // Simulation.
public:
  virtual void tick (Surface&, const Weather&, const Time&, Vegetation&, 
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
  virtual void irrigate_top (double flux, double temp) = 0;
  virtual void irrigate_surface (double flux, double temp) = 0;
  virtual void spray (const string& chemical, double amount) = 0; // [g/m^2]
  virtual void harvest_chemicals (Chemicals& chemicals, double LAI) = 0;
		       
  // Communication with external model.
  virtual double get_evap_interception () const; // [mm/h]
  virtual double get_intercepted_water () const; // [mm]
  virtual double get_net_throughfall () const; // [mm/h]
  virtual double get_snow_storage () const; // [mm]

  // Create.
protected:
  Bioclimate (const string& name);
public:
  virtual ~Bioclimate ();
};

static Librarian<Bioclimate> Bioclimate_init ("bioclimate");

#endif BIOCLIMATE_H
