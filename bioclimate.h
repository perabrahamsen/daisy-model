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

struct Surface;
struct Weather;
struct AttributeList;
struct Vegetation;
struct Soil;
struct Syntax;
struct SoilWater;
struct SoilHeat;
struct Log;
struct Chemicals;

class Bioclimate
{ 
  // Content.
public:
  const string name;
  static const char *const description;

  // Simulation.
public:
  virtual void tick (Surface&, const Weather&, Vegetation&, 
		     const Soil&, SoilWater&, const SoilHeat&, Treelog&) = 0;
  virtual void output (Log&) const = 0;

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
  virtual void irrigate_overhead (double flux, double temp) = 0;
  virtual void irrigate_surface (double flux, double temp) = 0;
  virtual void irrigate_overhead (double flux) = 0;
  virtual void irrigate_surface (double flux) = 0;
  virtual void set_subsoil_irrigation (double flux) = 0;
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

#if !defined (__BORLANDC__)
EMPTY_TEMPLATE
Librarian<Bioclimate>::Content* Librarian<Bioclimate>::content;
#endif

static Librarian<Bioclimate> Bioclimate_init ("bioclimate");

#endif // BIOCLIMATE_H
