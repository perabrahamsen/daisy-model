// field.h
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
//
// This file is part of Daisy.
// 
// Daisy is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser Public License as published by
// the Free Software Foundation; either version 2.1 of the License, or
// (at your option) any later version.
// 
// Daisy is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser Public License for more details.
// 
// You should have received a copy of the GNU Lesser Public License
// along with Daisy; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA


#ifndef FIELD_H
#define FIELD_H

#include "symbol.h"
#include <vector>
using namespace std;

class Column;
class AttributeList;
class Time;
class IM;
class Harvest;
class Weather;
class Log;
class Treelog;

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
    Restrict (Field&, symbol column);
    ~Restrict ();
  };
  friend class Restrict;

  // Actions.
public:
  void sow (Treelog&, const AttributeList& crop);
  void ridge (const AttributeList& ridge);
  void irrigate_overhead (double flux, double temp, const IM&);
  void irrigate_surface (double flux, double temp, const IM&);
  void irrigate_overhead (double flux, const IM&);
  void irrigate_surface (double flux, const IM&);
  void irrigate_subsoil (double flux, const IM&, double from, double to);
  void set_subsoil_irrigation (double flux, const IM& sm, 
			       double from, double to);
  void fertilize (const AttributeList&, double from, double to); // Organic.
  void fertilize (const AttributeList&);
  void clear_second_year_utilization ();
  void harvest (const Time&, symbol name,
		double stub_length, 
		double stem_harvest, 
		double leaf_harvest, 
		double sorg_harvest,
		vector<const Harvest*>&, Treelog&);
  void mix (Treelog&, const Time&,
	    double from, double to, double penetration = 1.0);
  void swap (Treelog&, const Time&, double from, double middle, double to);
  void set_porosity (double at, double Theta);
  void set_heat_source (double at, double value); // [W/m^2]
  void spray (symbol chemical, double amount); // [g/ha]
  void set_surface_detention_capacity (double height); // [mm]

  // Conditions.
public:
  double daily_air_temperature () const; // [ dg C]
  double soil_temperature (double height) const; // [ cm -> dg C]
  double soil_water_potential (double height) const; // [cm -> cm]
  double soil_water_content (double from, double to) const; // [cm, cm -> cm]
  double soil_inorganic_nitrogen (double from, double to) const; // [kg N/ha]
  double second_year_utilization () const;// [kg N/ha]
  // Current development stage for the crop named "crop", or
  // Crop::DSremove if no such crop is present.
  double crop_ds (symbol crop) const; 
  // Drymatter in shoot [kg/ha], or negative if no such crop is present
  double crop_dm (symbol crop) const; 
  // Simulation.
  void clear ();
  void tick (Treelog&, const Time&, const Weather*);
  void output (Log&) const;

  // Find a specific column.
  const Column* find (symbol name) const;

  // Communication with external model.
  Column* find (unsigned int pos) const;
  unsigned int size () const;

  // Create and destroy.
  void divide (symbol original, symbol copy, double copy_size,
	       const Time&, const Weather*);
  void merge (symbol combine, symbol remove);
  bool check (bool require_weather, const Time& from, const Time& to, 
	      Treelog& err) const;
  bool check_am (const AttributeList& am, Treelog& err) const;
  void initialize (const Time&, Treelog& err, const Weather*);
  Field (const vector<AttributeList*>&);
  ~Field ();
};

#endif // FIELD_H
