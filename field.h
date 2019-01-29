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

#include "irrigate.h"
#include "border.h"
#include "symbol.h"
#include <vector>
#include <string>
#include <memory>

class Column;
class FrameSubmodel;
class FrameModel;
class Frame;
class Time;
class IM;
class Harvest;
class Weather;
class Output;
class Treelog;
class Log;
class Select;
class Block;
class Metalib;
class Scope;
class Volume;
class Crop;

class Field : public Border
{ 
  // Top secret internal state.
  struct Implementation;
  std::unique_ptr<Implementation> impl;

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
  void sorption_table (const size_t cell, 
                       const double Theta, const double start,
                       const double factor, const int intervals,
                       Treelog& msg) const;
  void sow (const Metalib&, const FrameModel& crop, 
            double row_width, double row_pos, double seed,
            const Time&, Treelog&);
  void sow (const Metalib&, Crop& crop, 
            double row_width, double row_pos, double seed,
            const Time&, Treelog&);
  void irrigate (const double duration, const double flux, 
                 const double temp, Irrigation::target_t target,
                 const IM& sm, const boost::shared_ptr<Volume> volume,
                 const bool silence, Treelog& msg);
  void fertilize (const Metalib&, const FrameModel&,
                  double from, double to, 
                  const Time&, Treelog& msg); // Organic.
  void fertilize (const Metalib&, const FrameModel&, const Volume&,
                  const Time&, Treelog&); // Organic.
  void fertilize (const Metalib&, const FrameModel&,
                  const Time&, Treelog& msg);
  void clear_second_year_utilization ();
  void emerge (symbol crop, Treelog&);
  void harvest (const Time&, symbol name,
		double stub_length, 
		double stem_harvest, double leaf_harvest, double sorg_harvest,
                bool combine,
		std::vector<const Harvest*>&, Treelog&);
  void pluck (const Time&, symbol name,
              double stem_harvest, double leaf_harvest, double sorg_harvest,
              std::vector<const Harvest*>&, Treelog&);
  void mix (double from, double to, double penetration, double surface_loose,
            double RR0, const Time&, Treelog&);
  void swap (double from, double middle, double to, double RR0,
             const Time&, Treelog&);
  void store_SOM (Treelog& msg);
  void restore_SOM (Treelog& msg);
  void set_porosity (double at, double Theta, Treelog& msg);
  void spray_overhead (symbol chemical, double amount, Treelog&); // [g/ha]
  void spray_surface (symbol chemical, double amount, Treelog&); // [g/ha]
  void set_surface_detention_capacity (double height); // [mm]
  void remove_solute (symbol chemical);
  double total_solute (symbol chemical); // [g/ha]

  // Conditions.
public:
  double daily_air_temperature () const; // [dg C]
  double daily_precipitation () const; // [mm]
  double daily_global_radiation () const; // [W/m^2]
  double soil_temperature (double height) const; // [cm -> dg C]
  double soil_water_potential (double height) const; // [cm -> cm]
  double soil_water_content (double from, double to) const; // [cm, cm -> cm]
  double soil_inorganic_nitrogen (double from, double to) const; // [kg N/ha]
  double second_year_utilization () const;// [kg N/ha]
  // Current development stage for the crop named "crop", or
  // Crop::DSremove if no such crop is present.
  double crop_ds (symbol crop) const; 
  // Phenological stage (could be BBCH, by default DS)
  double crop_stage (symbol crop) const; 
  // Drymatter in shoot [kg/ha], or negative if no such crop is present
  double crop_dm (symbol crop, double height) const; 
  double crop_sorg_dm (symbol crop) const; 
  // All names of all crops on selected columns.
  std::string crop_names () const;
  // Simulation.
  void clear ();
  void tick_source (const Scope&, const Time&, Treelog&);
  double suggest_dt (double weather_dt, double T_air) const;
  void tick_move (const Metalib& metalib, 
                  const Time&, const Time&, double dt, const Weather*, 
                  const Scope&, Treelog&);
  void output (Log&) const;

  // Find a specific column.
  const Column* find (symbol name) const;

  // Communication with external model.
  Column* find (unsigned int pos) const;
  unsigned int size () const;

  // Create and destroy.
  bool check (const Weather* global_weather, const Time& from, const Time& to, 
	      const Scope&, Treelog&) const;
  bool check_am (const FrameModel& am, Treelog&) const;
  bool check_z_border (double, Treelog&) const;
  bool check_x_border (double, Treelog&) const;
  bool check_y_border (double, Treelog&) const;

  bool initialize (const Block&, const std::vector<const Scope*> scopes,
                   const Time&, const Weather*,
		   const Scope&);
  Field (const Block&, const std::string& key);
  void summarize (Treelog& msg) const;
  ~Field ();
};

#endif // FIELD_H
