// column.h
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


#ifndef COLUMN_H
#define COLUMN_H

#include "model_framed.h"
#include "irrigate.h"
#include <vector>

class Frame;
class FrameModel;
class FrameSubmodel;
class Log;
class Time;
class Weather;
class OrganicMatter;
class Horizon;
class IM;
class Crop;
class Harvest;
class Volume;
class Output;
class Treelog;
class Metalib;
class Block;
class BlockModel;
class Scope;
class Geometry;
class Vegetation;
class Soil;
class XYPoint;

class Column : public ModelFramed
{
public:
  double area;
  static const char *const component;
  symbol library_id () const;

  // Location.
private:
  const std::vector<const XYPoint*> location_;
public:
  const std::vector<const XYPoint*>& location () const;

  virtual const Horizon& horizon_at (double z, double x, double y) const = 0;

  // Log.
public:
  virtual const Geometry& get_geometry () const = 0;
  virtual const Soil& get_soil () const = 0;
  virtual const Vegetation& get_vegetation () const = 0;

  // Actions.
public:
  virtual void sorption_table (const size_t cell, 
                               const double Theta, const double start,
                               const double factor, const int intervals,
                               Treelog& msg) const = 0;
  virtual void sow (const Scope&, const FrameModel& crop, 
                    double row_width, double row_pos, double seed,
                    const Time&, Treelog&) = 0;
  virtual void sow (const Scope&, Crop& crop, 
                    double row_width, double row_pos, double seed,
                    const Time&, Treelog&) = 0;
  virtual void irrigate (const double duration, const double flux, 
                         const double temp, Irrigation::target_t target,
                         const IM& sm, const boost::shared_ptr<Volume> volume,
                         const bool silence, Treelog& msg) = 0;
  virtual void fertilize (const Metalib&, const FrameModel&,
                          double from, double to, 
                          const Time&, Treelog& msg) = 0;
  virtual void fertilize (const Metalib&, const FrameModel&, const Volume&, 
                          const Time&, Treelog& msg) = 0;
  virtual void fertilize (const Metalib&, const FrameModel&, 
                          const Time&, Treelog& msg) = 0;
  virtual void clear_second_year_utilization () = 0;
  virtual void emerge (symbol crop, Treelog&) = 0;
  virtual void harvest (const Time&, symbol name,
			double stub_length, 
			double stem_harvest, 
			double leaf_harvest, 
			double sorg_harvest, 
                        const bool combine,
			std::vector<const Harvest*>& harvest, Treelog&) = 0;
  virtual void pluck (const Time& time, const symbol crop_name,
                      const double stem_harvest,
                      const double leaf_harvest,
                      const double sorg_harvest,
                      std::vector<const Harvest*>& harvest, 
                      Treelog& msg) = 0;
  virtual void mix (double from, double to, double penetration, 
                    double surface_loose, double RR0, 
                    const Time&, Treelog&) = 0;
  virtual void swap (double from, double middle, double to, 
                     double RR0, const Time&, Treelog&) = 0;
  virtual void store_SOM (Treelog& msg) = 0;
  virtual void restore_SOM (Treelog& msg) = 0;
  virtual void set_porosity (double at, double Theta, Treelog&) = 0;
  virtual void spray_overhead (symbol chemical, double amount, Treelog&) = 0; // [g/ha]
  virtual void spray_surface (symbol chemical, double amount, Treelog&) = 0; // [g/ha]
  virtual void set_surface_detention_capacity (double height) = 0; // [mm]
  virtual void remove_solute (symbol chemical) = 0;
  virtual double total_solute (const symbol chem) const = 0; //[g/ha]

  // Conditions.
public:
  virtual double daily_air_temperature () const = 0; // [dg C]
  virtual double daily_precipitation () const = 0; // [mm]
  virtual double daily_global_radiation () const = 0; // [W/m^2]
  virtual double soil_temperature (double height) const = 0; // [cm -> dg C]
  virtual double soil_water_potential (double height) const = 0; // [cm -> cm]
  virtual double soil_water_content (double from, double to) const = 0; // [cm]
  virtual double soil_inorganic_nitrogen (double from, // [kg N/ha]
					  double to) const = 0; 
  virtual double second_year_utilization () const = 0;
  // Current development stage for the crop named "crop", or
  // Crop::DSremove if no such crop is present.
  virtual double crop_ds (symbol crop) const = 0; 
  // Phenological stage (could be BBCH, by default DS)
  virtual double crop_stage (symbol crop) const = 0; 
  // Drymatter in shoot [kg/ha], or negative if no such crop is present
  virtual double crop_dm (symbol crop, double height) const = 0; 
  // Drymatter in storage organ [kg/ha], or negative if no such crop is present
  virtual double crop_sorg_dm (symbol crop) const = 0; 
  // All names of all crops on the column.
  virtual std::string crop_names () const = 0;
  // Lower limit of soil description.
  virtual double bottom () const = 0;

  // Simulation.
  virtual void clear () = 0;
  virtual void tick_source (const Scope&, const Time&, Treelog&) = 0;
  virtual double suggest_dt (double weather_dt, double T_air) const = 0;
  virtual void tick_move (const Metalib& metalib, 
                          const Time&, const Time&, double dt, const Weather*, 
                          const Scope&, Treelog&) = 0;
  virtual bool check (const Weather* global_weather,
		      const Time& from, const Time& to, 
		      const Scope&, Treelog&) const = 0;
  virtual bool check_am (const FrameModel& am, Treelog&) const = 0;
  virtual bool check_z_border (double, Treelog&) const = 0;
  virtual bool check_x_border (double, Treelog&) const = 0;
  virtual bool check_y_border (double, Treelog&) const = 0;
  void output (Log&) const;

  // Create and Destroy.
protected:
  explicit Column (const BlockModel&);
public:
  virtual void initialize (const Block& al) = 0;
  virtual bool initialize (const Metalib&,
                           const std::vector<const Scope*>& scopes,
                           const Time&, const Weather*, const Scope&,
                           Treelog&) = 0;
  virtual void summarize (Treelog& msg) const = 0;
  ~Column ();
};

#endif // COLUMN_H
