// chemistry.h --- Pesticides and other chemicals.
// 
// Copyright 2007 Per Abrahamsen and KVL.
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


#ifndef CHEMISTRY_H
#define CHEMISTRY_H

#include "model_framed.h"
#include "symbol.h"
#include <vector>

class Litter;
class Log;
class Geometry;
class Soil;
class SoilWater;
class SoilHeat;
class OrganicMatter;
class Surface;
class Movement;
class Chemical;
class Volume;
class Treelog;
class BlockModel;
class Scope;
class IM;
class Units;
class Vegetation;
class Bioclimate;

class Chemistry : public ModelFramed
{
  // Content.
public:
  static const char *const component;
  symbol library_id () const;
protected:
  const Units& units;

  // Query.
public:
  virtual bool know (symbol chem) const = 0;
  virtual bool ignored (symbol chem) const = 0;
  virtual Chemical& find (symbol chem) = 0;
  virtual const Chemical& find (symbol chem) const = 0;
  virtual const std::vector<Chemical*>& all () const = 0;

  // Table.
public:
  virtual void sorption_table (const Soil& soil, const size_t cell, 
                               const double Theta, const double start,
                               const double factor, const int intervals,
                               Treelog& msg) const = 0;

public:
  virtual void update_C (const Soil&, const SoilWater&) = 0;
  virtual void mass_balance (const Geometry&, const SoilWater&) const = 0;
  void deposit (const IM& im, Treelog&);
  virtual void deposit (symbol chem, double flux /* [g/m^2/h] */,
			Treelog&) = 0;
  void spray_overhead (const IM& im, Treelog&);
  void spray_surface (const IM& im, Treelog&);
  virtual void spray_overhead (symbol chem, double amount /* [g/m^2] */,
                               Treelog&) = 0;
  virtual void spray_surface (symbol chem, double amount /* [g/m^2] */,
                              Treelog&) = 0;
  virtual void dissipate_surface (symbol chem, double amount /* [g/m^2] */, 
                                  Treelog&) = 0;
  virtual void harvest (double removed, double surface) = 0;
  virtual void mix (const Geometry&, const Soil&, const SoilWater&, 
                    double from, double to, double penetration) = 0;
  virtual void swap (const Geometry&, const Soil&, const SoilWater&,
                     double from, double middle, double to) = 0;
  void incorporate (const Geometry& geo, const IM& im, 
		    const double from, const double to, Treelog& msg);
  void incorporate (const Geometry& geo, const IM& im, const Volume&, 
                    Treelog& msg);
  virtual void incorporate (const Geometry& geo,
			    const symbol chem, const double amount,
			    const double from, const double to, 
			    Treelog& msg) = 0;
  virtual void incorporate (const Geometry& geo,
			    const symbol chem, const double amount,
			    const Volume&, Treelog& msg) = 0;
  virtual void remove_solute (const symbol chem) = 0;
  virtual double total_content (const Geometry&,
				const symbol chem) const = 0; // [g/m^2]


  // Simulation.
public:
  virtual void tick_source (const Scope&, 
                            const Geometry&, const Soil&, const SoilWater&, 
                            const SoilHeat&, const OrganicMatter&, 
                            const Chemistry&, Treelog&) = 0;
  virtual double find_dt (double S, double C, 
                          double M_secondary, double M_solute, 
                          double M_total) const = 0;
  virtual double suggest_dt () const = 0;
  virtual void tick_top (const Geometry&, const Soil&, 
                         const SoilWater&, const SoilHeat&, 
                         const double tillage_age /* [d] */,
                         const Surface&,
			 const Vegetation& vegetation,
			 const Bioclimate& bioclimate,
			 const Litter& litter,
			 const double surface_runoff_rate, // [h^-1]
			 const double surface_water /* [mm] */,
			 const double total_rain /* [mm/h] */,
                         OrganicMatter&, Chemistry& chemistry, 
                         double dt /* [h] */,
			 Treelog&) = 0;
  virtual void tick_soil (const Scope&, 
                          const Geometry& geo, double ponding /* [mm] */,
                          double R_mixing /* [h/mm] */,
                          const Soil&, const SoilWater&, const SoilHeat&, 
			  Movement&, OrganicMatter&, Chemistry&, 
			  double dt, Treelog&) = 0;
  virtual void clear () = 0;
  virtual void output (Log&) const;

  // Create & Destroy.
public:
  virtual void initialize (const Scope&, const Geometry& geo,
                           const Soil&, const SoilWater&, const SoilHeat&,
			   const OrganicMatter&, const Surface&, Treelog&) = 0;
  virtual bool check (const Scope&, const Geometry&,
		      const Soil&, const SoilWater&, const SoilHeat&,
		      const OrganicMatter&, const Chemistry&,
		      Treelog&) const = 0;
protected:
  explicit Chemistry (const BlockModel& al);
public:
  ~Chemistry ();
};

#endif // CHEMISTRY_H
