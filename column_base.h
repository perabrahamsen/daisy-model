// column_base.h

#include "column.h"
#include "bioclimate.h"
#include "surface.h"
#include "soil.h"
#include "soil_water.h"
#include "soil_heat.h"
#include "soil_chemicals.h"
#include "groundwater.h"
#include "alist.h"
#include "syntax.h"
#include "library.h"
#include "log.h"
#include "weather.h"
#include "vegetation.h"
#include "time.h"

struct IM;

class ColumnBase : public Column
{
  // Content.
protected:
  Weather* weather;
  Vegetation& vegetation;
  Bioclimate& bioclimate;
  Surface surface;
  Soil soil;
  SoilWater soil_water;
  SoilHeat soil_heat;
  SoilChemicals soil_chemicals;
  Groundwater& groundwater;

  // Actions.
public:
  void ridge (const AttributeList& al);
  void irrigate_top (double flux, double temp, const IM&);
  void irrigate_surface (double flux, double temp, const IM&);
  void irrigate_top (double flux, const IM&);
  void irrigate_surface (double flux, const IM&);
  void mix (const Time&, double from, double to, double penetration = 1.0);
  virtual void kill_all (const Time& time) = 0;
  void swap (const Time&, double from, double middle, double to);
  void set_porosity (double at, double Theta);
  void set_heat_source (double at, double value); // [W/m^2]
  void spray (const string& chemical, double amount); // [g/ha]
  void set_surface_detention_capacity (double height); // [mm]

  // Conditions.
  double soil_temperature (double height) const; // [ cm -> dg C]
  double soil_water_potential (double height) const; // [cm -> cm]
  double  crop_ds (const string& name) const ;// {[-1:2], Crop::DSremove}
  double crop_dm (const string& name) const; // [kg/ha], negative when no crop

  // Communication with external model.
public:
  unsigned int count_layers () const; // Number of num. layers.
  double get_dz (unsigned int i) const; // Size of layer `i'. [cm]
  void put_water_pressure (const vector<double>& v); // [cm]
  void get_water_sink (vector<double>& v) const; // [cm^3/cm^3/h]
  double get_evap_interception () const; // [mm/h]
  double get_intercepted_water () const; // [mm]
  double get_net_throughfall () const; // [mm/h]
  double get_snow_storage () const; // [mm]
  double get_exfiltration () const; // [mm/h]
  double get_evap_soil_surface () const; // [mm/h]
  void put_ponding (double pond); // [mm]
  void put_surface_chemical (const string& name, double amount); // [g/cm^2]
  double get_surface_chemical (const string& name) const; // [g/cm^2]
  double get_temperature_at (unsigned int i) const; // [°C]
  double get_crop_h2o_uptake_at (unsigned int i) const; // [cm³/cm³/h]
  double get_water_content_at (unsigned int i) const; // [cm³/cm³]

  // Simulation.
public:
  void output (Log&) const;
  virtual void output_inner (Log&) const;
  bool check (bool require_weather, const Time& from, const Time& to) const;
  virtual bool check_inner (int n) const;

  // Create and Destroy.
public:
  ColumnBase (const AttributeList&);
  void initialize (const Time&, const Weather*);
  ~ColumnBase ();
  static void load_syntax (Syntax&, AttributeList&);
};
