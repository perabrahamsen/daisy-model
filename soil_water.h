// soil_water.h
// 
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


#ifndef SOIL_WATER_H
#define SOIL_WATER_H

#include <vector>

class AttributeList;
class Surface;
class Groundwater;
class Log;
class Soil;
class Syntax;
class Geometry;
class SoilHeat;
class Treelog;
class Block;

class SoilWater
{
  // Sink.
public:
  virtual void clear (const Geometry&) = 0;
  virtual void root_uptake (const std::vector<double>&) = 0;
  virtual void drain (const std::vector<double>&) = 0;
  virtual void freeze (const Soil&, const std::vector<double>&) = 0;
  
  // Queries
public:
  virtual double h (size_t i) const = 0;
  virtual double pF (size_t i) const = 0;
  virtual double Theta (size_t i) const = 0;
  virtual double Theta_left (size_t i) const = 0;
  virtual double Theta_old (size_t i) const = 0;
  virtual double content (const Geometry&,
                          double from, double to) const = 0; // [cm]
#ifndef NEWMOVE
  virtual double q (size_t i) const = 0;
  virtual double q_p (size_t i) const = 0;
#endif // OLDMOVE
  virtual double S_sum (size_t i) const = 0;
  virtual double S_root (size_t i) const = 0;
  virtual double S_drain (size_t i) const = 0;
  virtual double S_ice (size_t i) const = 0;
  virtual double S_p (size_t i) const = 0;
  virtual double h_ice (size_t i) const = 0;
  virtual double X_ice (size_t i) const = 0;
  virtual double X_ice_total (size_t i) const = 0;

  virtual size_t first_groundwater_cell () const = 0;
    
  // Ice modified lookups.
  virtual double Theta (const Soil&, size_t i, double h) const = 0;
 
  // Simulation.
public:
  virtual void set_external_source (const Geometry&, 
			    double amount, double from, double to) = 0;
  virtual void incorporate (const Geometry&, 
                            double amount, double from, double to) = 0;
  virtual void mix (const Geometry& geo,
            const Soil&, double from, double to) = 0;
  virtual void swap (Treelog&, const Geometry& geo,
             const Soil&, double from, double middle, double to) = 0;
  virtual void set_Theta (const Soil& soil, 
		  size_t from, size_t to, double Theta) = 0;
  virtual bool check (size_t n, Treelog& err) const = 0;
  virtual void output (Log&) const = 0;


  // Communication with surface.
  virtual double MaxExfiltration (const Geometry& geo,
                                  const Soil&, double T) const = 0;
  
  // Communication with external model.
  virtual void put_h (const Soil& soil, 
                      const std::vector<double>& v) = 0; // [cm]
  virtual void get_sink (std::vector<double>& v) const = 0; // [cm^3/cm^3/h]

  // Creation.
  static void load_base (Syntax&, AttributeList&);
  SoilWater (Block&);
  virtual ~SoilWater ();
};

#endif // SOIL_WATER_H
