// column.h

#ifndef COLUMN_H
#define COLUMN_H

#include "librarian.h"

class Time;
class Weather;
class OrganicMatter;
class IM;
class Crop;
class Harvest;

class Column
{
public:
  const AttributeList alist;	// Remember attributes for checkpoint.
  string name;
  double size;
  static const char *const description;

  // Actions.
public:
  virtual void sow (const AttributeList& crop) = 0;
  virtual void irrigate_top (double flux, double temp, const IM&) = 0;
  virtual void irrigate_surface (double flux, double temp, const IM&) = 0;
  virtual void fertilize (const AttributeList&, double from, double to) = 0;
  virtual void fertilize (const AttributeList&) = 0;
  virtual void fertilize (const IM&, double from, double to) = 0; // Mineral.
  virtual void fertilize (const IM&) = 0;
  virtual vector<const Harvest*> harvest (const Time&, const string& name,
					  double stub_length, 
					  double stem_harvest, 
					  double leaf_harvest, 
					  double sorg_harvest) = 0;
  virtual void mix (const Time&,
		    double from, double to, double penetration = 1.0) = 0;
  virtual void swap (const Time&, double from, double middle, double to) = 0;
  virtual void spray (const string& chemical, double amount) = 0; // [g/ha]

  // Conditions.
public:
  virtual double soil_temperature (double height) const = 0; // [ cm -> dg C]
  virtual double soil_water_potential (double height) const = 0; // [cm -> cm]
  // Current development stage for the crop named "crop", or
  // Crop::DSremove if no such crop is present.
  virtual double crop_ds (const string& crop) const = 0; 
  // Drymatter in shoot [kg/ha], or negative if no such crop is present
  virtual double crop_dm (const string& crop) const = 0; 
  
  // Simulation.
  virtual void tick (const Time&, const Weather&) = 0;

  virtual bool check () const = 0;
  virtual bool check_am (const AttributeList& am) const = 0;
  virtual void output (Log&, Filter&) const;

  // Communication with external model.
  virtual unsigned int count_layers () const = 0; // Number of num. layers.
  virtual double get_dz (unsigned int i) const = 0; // Size of layer `i'. [cm]
  virtual void put_water_pressure (const vector<double>& v) = 0; // [cm]
  virtual void get_water_sink (vector<double>& v) const = 0; // [cm^3/cm^3/h]
  virtual void put_no3_m (const vector<double>& v) = 0; // [g/cm^3]
  virtual void get_no3_m (vector<double>& v) const = 0; // [g/cm^3]
  virtual double get_evap_interception () const = 0; // [mm/h]
  virtual double get_intercepted_water () const = 0; // [mm]
  virtual double get_net_throughfall () const = 0; // [mm/h]
  virtual double get_snow_storage () const = 0; // [mm]
  virtual double get_exfiltration () const = 0; // [mm/h]
  virtual double get_evap_soil_surface () const = 0; // [mm/h]
  virtual void put_ponding (double pond) = 0;	// [mm]
  virtual void put_surface_no3 (double no3) = 0; // [g/cm^2]
  virtual double get_surface_no3 () const = 0; // [g/cm^2]
  virtual double get_smb_c_at (unsigned int i) const = 0; // [g C/cm³]
  virtual double get_co2_production_at (unsigned int i) const = 0; // [g C/cm³]
  virtual double get_temperature_at (unsigned int i) const = 0; // [°C]
  virtual double get_crop_h2o_uptake_at (unsigned int i) const = 0; //[cm³/cm³]
  virtual double get_water_content_at (unsigned int i) const = 0; // [cm³/cm³]

  // Create and Destroy.
protected:
  Column (const AttributeList&);
public:
  static void load_syntax (Syntax&, AttributeList&);
  virtual Column& clone (const string& name) const = 0;
  virtual void initialize (const Time&, const Weather&) = 0;

  virtual ~Column ();
};

static Librarian<Column> Column_init ("column");

#endif COLUMN_H
