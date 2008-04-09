// msoltranrect.h -- Solute transport in primary domain.
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


#ifndef MSOLTRANRECT_H
#define MSOLTRANRECT_H

#include "model.h"
#include <vector>

class Log;
class Geometry;
class Soil;
class SoilWater;
class DOE;
class Chemical;
class Adsorption;
class Surface;
class Groundwater;
class Weather;
class Time;
class Treelog;
class Block;
class Scope;

class Msoltranrect : public Model
{
  // Content.
public:
  static const char *const component;
  symbol library_id () const;

  // Simulation.
public:
  void element (const Geometry&, const Soil&, const SoilWater&,
                DOE&, const double diffusion_coefficient, double dt, Treelog&);
  virtual void flow (const Geometry& geo, 
                     const Soil& soil, 
                     const std::vector<double>& Theta_old,
                     const std::vector<double>& Theta_new,
                     const std::vector<double>& q,
                     symbol name,
                     std::vector<double>& C, 
                     const std::vector<double>& S, 
                     std::vector<double>& J, 
		     const double C_below,
		     const bool flux_below,
                     double diffusion_coefficient, double dt,
                     Treelog& msg) const = 0;

  // Create and Destroy.
public:
  virtual bool check (const Geometry&, Treelog&);
  static const AttributeList& none_model ();
  static const AttributeList& reserve_model ();
  static const AttributeList& default_model ();
protected:
  Msoltranrect (Block&);
public:
  ~Msoltranrect ();
};

#endif // MSOLTRANRECT_H
