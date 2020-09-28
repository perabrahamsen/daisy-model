// organic.h
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
// Copyright 2006 Per Abrahamsen and KVL.
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


#ifndef ORGANIC_H
#define ORGANIC_H

#include "model_derived.h"
#include <vector>

class FrameModel;
class Log;
class AM;
class DOM;
class SMB;
class Geometry;
class Soil;
class SoilpH;
class SoilWater;
class SoilHeat;
class Chemistry;
class Time;
class Volume;
class Treelog;
class BlockModel;
class Units;
class Metalib;

class OrganicMatter : public ModelDerived
{
  // Content.
public:
  static const char *const component;
  symbol library_id () const;

  // Simulation.
public:
  virtual void clear () = 0;
  virtual void monthly (const Metalib&, const Geometry&, Treelog&) = 0;
  virtual const std::vector<bool>& active () const = 0;
  virtual void tick (const Geometry& geo, const Soil&, const SoilpH&, 
		     const SoilWater&, const SoilHeat&, 
                     const std::vector<double>& tillage_age,
		     Chemistry&, double dt, Treelog& msg) = 0;
  virtual void transport (const Units&, const Geometry&, 
                          const Soil&, const SoilWater&, const SoilHeat&,
			  Treelog&) = 0;
  virtual const std::vector<DOM*>& fetch_dom () const = 0;
  virtual void output (Log&) const = 0;
  virtual double top_DM () const = 0;	// [kg DM/m^2]
  virtual double CO2 (size_t i) const = 0;	// [g C/cm^3/h]
  virtual double CO2_fast (size_t i) const = 0;	// [g C/cm^3/h]
  virtual void mix (const Geometry&, const Soil&, const SoilWater&,
                    double from, double to, double penetration) = 0;
  virtual void swap (const Geometry&, const Soil&, const SoilWater&, 
                     double from, double middle, double to) = 0;
  virtual double AOM_C (const Geometry&, double from, double to) const = 0;
  virtual void store_SOM () = 0;
  virtual void restore_SOM () = 0;
  virtual const std::vector <AM*> get_am () const = 0;
  virtual const std::vector <SMB*> get_smb () const = 0;
  virtual void add_stationary (const std::vector<double>& C,
			       const std::vector<double>& N,
			       const int where,
			       const double dt) = 0;

  // Communication with external model.
  virtual double get_smb_c_at (size_t i) const = 0; // [g C/cm³]
  virtual void add_to_buffer (const Geometry& geo,
			      const double from /* [cm] */,
			      const double to /* [cm] */,
			      const double C /* [g C/cm^2] */,
			      const double N /* [g N/cm^2] */) = 0;

  // Create and Destroy.
  virtual int som_pools () const = 0;
  virtual bool check (const Units&, const Geometry&,
                      const Soil&, const SoilWater&, const SoilHeat&,
		      const Chemistry&, Treelog&) const = 0;
  virtual bool check_am (const FrameModel& am, Treelog& err) const = 0;
  virtual void add (AM&) = 0;
  virtual void fertilize (const Metalib&, const FrameModel&, 
                          const Geometry&, const Time&, Treelog&) = 0;
  virtual void fertilize (const Metalib&, const FrameModel&, const Geometry&,
                          double from, double to, const Time&, Treelog&) = 0;
  virtual void fertilize (const Metalib&, const FrameModel&, const Geometry&,
                          const Volume&, const Time&, Treelog&) = 0;
  virtual AM* find_am (symbol sort, symbol part) const = 0;
public:
  virtual void initialize (const Metalib&, const Units&, 
                           const Frame&, const Geometry& geo,
                           const Soil&, const SoilpH&, 
                           const SoilWater&, const SoilHeat&,
                           double T_avg, Treelog&) = 0;
private:
  OrganicMatter ();
public:
  explicit OrganicMatter (const BlockModel&);
  ~OrganicMatter ();
};

#endif // ORGANIC_H
