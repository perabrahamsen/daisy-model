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

#include "librarian.h"

class Time;
class Weather;
class OrganicMatter;
class Horizon;
class IM;
class Crop;
class Harvest;
class Treelog;

class Column
{
public:
  const AttributeList alist;	// Remember attributes for checkpoint.
  const symbol name;
  double size;
  static const char *const description;

  virtual const Horizon& horizon_at (double z, double x, double y) const = 0;

  // Actions.
public:
  virtual void sow (Treelog&, const AttributeList& crop) = 0;
  virtual void ridge (const AttributeList& ridge) = 0;
  virtual void irrigate_overhead (double flux, double temp, const IM&) = 0;
  virtual void irrigate_surface (double flux, double temp, const IM&) = 0;
  virtual void irrigate_overhead (double flux, const IM&) = 0;
  virtual void irrigate_surface (double flux, const IM&) = 0;
  virtual void irrigate_subsoil (double flux, const IM&, 
                                 double from, double to) = 0;
  virtual void fertilize (const AttributeList&, double from, double to) = 0;
  virtual void fertilize (const AttributeList&) = 0;
  virtual void clear_second_year_utilization () = 0;
  virtual void emerge (symbol crop, Treelog&) = 0;
  virtual void harvest (const Time&, symbol name,
			double stub_length, 
			double stem_harvest, 
			double leaf_harvest, 
			double sorg_harvest, 
                        const bool combine,
			std::vector<const Harvest*>& harvest, Treelog&) = 0;
  virtual void mix (Treelog&, const Time&,
		    double from, double to, double penetration = 1.0) = 0;
  virtual void swap (Treelog&, 
		     const Time&, double from, double middle, double to) = 0;
  virtual void set_porosity (double at, double Theta) = 0;
  virtual void set_heat_source (double at, double value) = 0; // [W/m^2]
  virtual void spray (symbol chemical, double amount) = 0; // [g/ha]
  virtual void set_surface_detention_capacity (double height) = 0; // [mm]

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
  // Drymatter in shoot [kg/ha], or negative if no such crop is present
  virtual double crop_dm (symbol crop, double height) const = 0; 
  // All names of all crops on the column.
  virtual std::string crop_names () const = 0;
  // Lower limit of soil description.
  virtual double bottom () const = 0;

  // Simulation.
  virtual void clear () = 0;
  virtual void tick (Treelog&, const Time&, const Weather*) = 0;

  virtual bool check (bool require_weather,
		      const Time& from, const Time& to, 
		      Treelog& err) const = 0;
  virtual bool check_am (const AttributeList& am, Treelog& err) const = 0;
  virtual bool check_border (const double border, Treelog& err) const = 0;
  virtual void output (Log&) const;

  // Create and Destroy.
protected:
  explicit Column (Block&);
public:
  static void load_syntax (Syntax&, AttributeList&);
  virtual Column& clone (symbol) const = 0;
  virtual void initialize (const Time&, Treelog& err, const Weather*) = 0;

  virtual ~Column ();
};

#ifdef FORWARD_TEMPLATES
template<>
Librarian<Column>::Content* Librarian<Column>::content;
#endif

static Librarian<Column> Column_init ("column");

#endif // COLUMN_H
