// field.h

#ifndef FIELD_H
#define FIELD_H

#include "common.h"
#include <vector>

struct Column;
struct AttributeList;
struct Time;
struct IM;
struct Harvest;
struct Weather;
struct Log;

class Field
{ 
  // Top secret internal state.
  struct Implementation;
  Implementation& impl;

public:
  // Restrict operations to selected columns.
  class Restrict 
  {
  private:
    Field& field;
  public:
    Restrict (Field&, const string& column);
    ~Restrict ();
  };
  friend class Restrict;

  // Actions.
public:
  void sow (const AttributeList& crop);
  void irrigate_top (double flux, double temp, const IM&);
  void irrigate_surface (double flux, double temp, const IM&);
  void fertilize (const AttributeList&, double from, double to); // Organic.
  void fertilize (const AttributeList&);
  void fertilize (const IM&, double from, double to); // Mineral.
  void fertilize (const IM&);
  vector<const Harvest*> harvest (const Time&, const string& name,
					  double stub_length, 
					  double stem_harvest, 
					  double leaf_harvest, 
					  double sorg_harvest);
  void mix (const Time&,
		    double from, double to, double penetration = 1.0);
  void swap (const Time&, double from, double middle, double to);
  void set_porosity (double at, double Theta);
  void set_heat_source (double at, double value); // [W/m^2]
  void spray (const string& chemical, double amount); // [g/ha]
  void set_surface_detention_capacity (double height); // [mm]

  // Conditions.
public:
  double soil_temperature (double height) const; // [ cm -> dg C]
  double soil_water_potential (double height) const; // [cm -> cm]
  // Current development stage for the crop named "crop", or
  // Crop::DSremove if no such crop is present.
  double crop_ds (const string& crop) const; 
  // Drymatter in shoot [kg/ha], or negative if no such crop is present
  double crop_dm (const string& crop) const; 

  // Simulation.
  void tick (const Time&, const Weather&);
  void output (Log&) const;

  // Find a specific column.
  const Column* find (const string& name) const;

  // Communication with external model.
  Column* find (unsigned int pos) const;
  unsigned int size () const;

  // Create and destroy.
  void divide (const string& original, const string& copy, double copy_size,
	       const Time&, const Weather&);
  void merge (const string& combine, const string& remove);
  bool check () const;
  bool check_am (const AttributeList& am) const;
  void initialize (const Time&, const Weather&);
  Field (const vector<AttributeList*>&);
  ~Field ();
};

#endif FIELD_H
