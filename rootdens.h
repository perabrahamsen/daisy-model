// rootdens.h -- Root Density component.
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


#ifndef ROOTDENS_H
#define ROOTDENS_H

#include "model_framed.h"
#include <vector>
#include <memory>

class Log;
class Geometry;
class BlockModel;
class Treelog;
class Metalib;
class Geometry;
class SoilHeat;
class SoilWater;

class Rootdens : public ModelFramed
{
  // Content.
public:
  static const char *const component;
  symbol library_id () const;
protected:
  const double SpRtLength;	// Specific root length [m/g]

  // Simulation.
public:
  virtual void set_density (const Geometry& geometry, 
			    double SoilDepth /* [cm] */,
			    double CropDepth /* [cm] */,
			    double CropWidth /* [cm] */,
			    double WRoot /* [g DM/m^2] */, double DS,
			    std::vector<double>& Density /* [cm/cm^3] */,
			    Treelog&) = 0;
  virtual void tick (const Geometry& geo,
		     const SoilHeat& soil_heat, const SoilWater& soil_water,
		     std::vector<double>& L, const double dt,
		     Treelog& msg);
  virtual const std::vector<double>& dynamic_root_death () const // [cm/cm^3/h]
  { static std::vector<double> empty; return empty; }
  virtual double dynamic_root_death_DM () const // [g DM/h]
  { return 0; }

  // Create and Destroy.
public:
  static std::unique_ptr<Rootdens> create_row (const Metalib&, Treelog&,
                                             double row_width, 
                                             double row_position,
                                             bool debug = false);
  static std::unique_ptr<Rootdens> create_uniform (const Metalib&, Treelog&);
public:
  virtual void initialize (const Geometry& geo, 
                           double row_width, double row_position, 
                           Treelog& msg) = 0;
protected:
  explicit Rootdens (const BlockModel&);
public:
  ~Rootdens ();
};

#endif // ROOTDENS_H
