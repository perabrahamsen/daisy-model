// organic_matter.h
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


#ifndef ORGANIC_MATTER_H
#define ORGANIC_MATTER_H

#include "model.h"
#include "symbol.h"
#include <vector>

class AttributeList;
class Syntax;
class Log;
class AM;
class DOM;
class Geometry;
class Soil;
class SoilWater;
class SoilHeat;
class Chemistry;
class Time;
class Treelog;
class Block;

class OrganicMatter : public Model
{
  // Content.
public:
  const symbol name;
  static const char *const component;

  // Simulation.
public:
  virtual void clear () = 0;
  virtual void monthly (const Geometry&) = 0;
  virtual const std::vector<bool>& active () const = 0;
  virtual void tick (const Geometry& geo,
		     const SoilWater&, const SoilHeat&, 
		     Chemistry&, double dt, Treelog& msg) = 0;
  virtual void transport (const Soil&, const SoilWater&, const SoilHeat&,
			  Treelog&) = 0;
  virtual const std::vector<DOM*>& fetch_dom () const = 0;
  virtual void output (Log&) const = 0;
  virtual double CO2 (size_t i) const = 0;	// [g C/cm³]
  virtual double CO2_fast (size_t i) const = 0;	// [g C/cm³]
  virtual void mix (const Geometry&, const Soil&, const SoilWater&,
                    double from, double to, double penetration,
                    const Time& time, double dt) = 0;
  virtual void swap (const Geometry&, const Soil&, const SoilWater&, 
                     double from, double middle, double to,
                     const Time& time, double dt) = 0;

  // Communication with external model.
  virtual double get_smb_c_at (size_t i) const = 0; // [g C/cm³]

  // Create and Destroy.
  virtual int som_pools () const = 0;
  virtual bool check (const Soil&, const SoilWater&, const SoilHeat&,
		      const Chemistry&, Treelog&) const = 0;
  virtual bool check_am (const AttributeList& am, Treelog& err) const = 0;
  virtual void add (AM&) = 0;
  virtual void fertilize (const AttributeList&, const Geometry&, 
                          double dt) = 0;
  virtual void fertilize (const AttributeList&, const Geometry&,
                          double from, double to, double dt) = 0;
  virtual AM* find_am (symbol sort, symbol part) const = 0;
public:
  virtual void initialize (const AttributeList&, const Geometry& geo,
                           const Soil&, const SoilWater&, 
                           double T_avg, Treelog&) = 0;
  static const AttributeList& default_model ();
private:
  OrganicMatter ();
  OrganicMatter (const OrganicMatter&);
  OrganicMatter& operator= (const OrganicMatter&);
public:
  explicit OrganicMatter (Block&);
  ~OrganicMatter ();
};

#endif // ORGANIC_MATTER_H
