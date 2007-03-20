// bioclimate.h
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


#ifndef BIOCLIMATE_H
#define BIOCLIMATE_H

#include "librarian.h"
#include "alist.h"
#include "symbol.h"

class Surface;
class Weather;
class AttributeList;
class Vegetation;
class Movement; 
class Geometry;
class Soil;
class Syntax;
class SoilWater;
class SoilHeat;
class Log;
class Chemicals;
class Time;

class Bioclimate : public Model
{ 
  // Content.
public:
  const symbol name;
  static const char *const description;
  const AttributeList alist;	// Remember attributes for checkpoint.

  // Simulation.
public:
  virtual void tick (const Time&, Surface&, const Weather&, Vegetation&, 
                     const Movement&, const Geometry&,
		     const Soil&, SoilWater&, const SoilHeat&, 
                     double dt, Treelog&) = 0;
  virtual void output (Log&) const = 0;
  virtual double get_intercepted_water () const = 0; // [mm]
  virtual double get_snow_storage () const = 0; // [mm]

  // Canopy.
public:
  virtual const std::vector<double>& height () const = 0;
  virtual const std::vector<double>& PAR () const = 0;
  virtual const std::vector<double>& sun_PAR () const = 0;
  virtual const std::vector<double>& sun_LAI_fraction () const = 0;
  virtual double LAI () const = 0;
  virtual double shared_light_fraction () const = 0;

  // Weather.
  virtual double daily_air_temperature () const = 0;
  virtual double hourly_leaf_temperature () const =0;
  virtual double daily_precipitation () const = 0;
  virtual double day_length () const = 0;
  virtual double daily_global_radiation () const = 0;
  virtual double hourly_global_radiation () const = 0;
  double day_fraction () const;   

  // Manager.
public:
  virtual void irrigate_overhead (double flux, double temp) = 0;
  virtual void irrigate_surface (double flux, double temp) = 0;
  virtual void irrigate_overhead (double flux) = 0;
  virtual void irrigate_surface (double flux) = 0;
  virtual void irrigate_subsoil (double flux) = 0;
  virtual void set_subsoil_irrigation (double flux) = 0;
  virtual void spray (symbol chemical, double amount) = 0; // [g/m^2]
  virtual void harvest_chemicals (Chemicals& chemicals, double LAI) = 0;
		       
  // Communication with external model.
  virtual double get_evap_interception () const = 0; // [mm/h]
  virtual double get_net_throughfall () const = 0; // [mm/h]

  // Utilities.
public:
  static void radiation_distribution (const int No, const double LAI,
                                      const double Ref,
                                      const double Si,
                                      const double Ext,
                                      std::vector <double>& Rad);
private:
  static void intensity_distribution (int No, double LAI,
                                      double Rad0, double Ext, 
                                      std::vector <double>& Rad);

  // Create.
public:
  virtual void initialize (const Weather&, Treelog&) = 0;
  static const AttributeList& default_model ();
protected:
  explicit Bioclimate (Block&);
public:
  ~Bioclimate ();
};

#ifdef FORWARD_TEMPLATES
template<>
Librarian<Bioclimate>::Content* Librarian<Bioclimate>::content;
#endif

static Librarian<Bioclimate> Bioclimate_init ("bioclimate");

#endif // BIOCLIMATE_H
