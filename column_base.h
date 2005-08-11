// column_base.h
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


#include "column.h"
#include "bioclimate.h"
#include "surface.h"
#include "soil.h"
#include "soil_water.h"
#include "soil_heat.h"
#include "soil_chemicals.h"
#include "chemistry.h"
#include "groundwater.h"
#include "alist.h"
#include "syntax.h"
#include "library.h"
#include "log.h"
#include "weather.h"
#include "vegetation.h"
#include "time.h"
#include <memory>

struct IM;
struct AM;

class ColumnBase : public Column
{
  // Content.
protected:
  Weather* weather;
  std::auto_ptr<Vegetation> vegetation;
  std::auto_ptr<Bioclimate> bioclimate;
  Surface surface;
  Soil soil;
  SoilWater soil_water;
  SoilHeat soil_heat;
  SoilChemicals soil_chemicals;
  std::vector<Chemistry*> chemistry;
  std::auto_ptr<Groundwater> groundwater;

  // Log variables.
private:
  double harvest_DM;
  double harvest_N;
  double harvest_C;
protected:
  double residuals_DM;
  double residuals_N_top;
  double residuals_C_top;
  std::vector<double> residuals_N_soil;
  std::vector<double> residuals_C_soil;

  // Actions.
public:
  void ridge (const AttributeList& al);
  void irrigate_overhead (double flux, double temp, const IM&);
  void irrigate_surface (double flux, double temp, const IM&);
  void irrigate_overhead (double flux, const IM&);
  void irrigate_surface (double flux, const IM&);
  void irrigate_subsoil (double flux, const IM& im, 
                         double from, double to);
  void set_subsoil_irrigation (double flux, const IM& im, 
			       double from, double to);
  void harvest (const Time& time, symbol crop_name,
		double stub_length,
		double stem_harvest,
		double leaf_harvest, 
		double sorg_harvest,
		std::vector<const Harvest*>& harvest, Treelog&);
  virtual void add_residuals (std::vector<AM*>&) = 0;
  void mix (Treelog&, const Time&,
	    double from, double to, double penetration = 1.0);
  void swap (Treelog&, const Time&,
	     double from, double middle, double to);
  void set_porosity (double at, double Theta);
  void set_heat_source (double at, double value); // [W/m^2]
  void spray (symbol chemical, double amount); // [g/ha]
  void set_surface_detention_capacity (double height); // [mm]

  // Conditions.
  double daily_air_temperature () const; // [ dg C]
  double daily_precipitation () const; // [mm]
  double daily_global_radiation () const; // [W/m^2]
  double soil_temperature (double height) const; // [ cm -> dg C]
  double soil_water_potential (double height) const; // [cm -> cm]
  double soil_water_content (double from, double to) const; // [cm]
  double crop_ds (symbol name) const ;// {[-1:2], Crop::DSremove}
  double crop_dm (symbol name, double height) const; // [kg/ha], negative when no crop
  std::string crop_names () const;   // All names of all crops on the column.


  // Communication with external model.
public:
  unsigned int count_layers () const; // Number of num. layers.
  double get_dz (unsigned int i) const; // Size of layer 'i'. [cm]
  void put_water_pressure (const std::vector<double>& v); // [cm]
  void get_water_sink (std::vector<double>& v) const; // [cm^3/cm^3/h]
  double get_evap_interception () const; // [mm/h]
  double get_intercepted_water () const; // [mm]
  double get_net_throughfall () const; // [mm/h]
  double get_snow_storage () const; // [mm]
  double get_exfiltration () const; // [mm/h]
  double get_evap_soil_surface () const; // [mm/h]
  void put_ponding (double pond); // [mm]
  void put_surface_chemical (symbol name, double amount); // [g/cm^2]
  double get_surface_chemical (symbol name) const; // [g/cm^2]
  double get_temperature_at (unsigned int i) const; // [°C]
  double get_crop_h2o_uptake_at (unsigned int i) const; // [cm³/cm³/h]
  double get_water_content_at (unsigned int i) const; // [cm³/cm³]

  // Simulation.
public:
  void tick_base (Treelog&);
  void output (Log&) const;
  virtual void output_inner (Log&) const;
  void clear ();
  bool check (bool require_weather, const Time& from, const Time& to, 
	      Treelog& err) const;
  virtual bool check_inner (Treelog& err) const = 0;
  bool check_border (const double border, Treelog& err) const;

  // Create and Destroy.
public:
  ColumnBase (const AttributeList&);
  bool initialize_common (const Time&, Treelog&, const Weather*);
  ~ColumnBase ();
  static void load_syntax (Syntax&, AttributeList&);
};
