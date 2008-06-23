// tertiary.h --- Transport of water and solutes outside the matrix.
// 
// Copyright 2008 Per Abrahamsen and KU.
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

#ifndef TERTIARY_H
#define TERTIARY_H

#include "model.h"
#include <map>

class Block;
class Geometry;
class Soil;
class SoilWater;
class SoilHeat;
class Surface;
class Chemical;
class Scope;
class Treelog;
class Log;
class Anystate;

class Tertiary : public ModelAListed
{
  // Identity.
public:
  static const char *const component;
  symbol library_id () const;
  virtual bool has_macropores () = 0;

  virtual Anystate get_state () const;
  virtual void set_state (const Anystate&);
  virtual bool converge (const Anystate& old); // Are current, old state close?

  // Simulation.
public:
  // - For use by Column.
  void tick (const Geometry&, const Soil&, const double dt, 
             SoilWater&, Surface&, Treelog&);
private:
  virtual void tick_water (const Geometry&, const Soil&, const SoilWater&,
                           const double dt,
                           Surface& surface,
                           std::vector<double>& S_drain,
                           std::vector<double>& S_matrix, 
                           std::vector<double>& q_tertiary,
                           Treelog& msg) = 0;

public:
  // - For use inside Richard's Equation.
  virtual void matrix_sink (const Geometry& geo, const Soil& soil,  
                            const SoilHeat& soil_heat, 
                            const std::vector<double>& h,
                            std::vector<double>& S_matrix,
                            std::vector<double>& S_drain) const;
  
  virtual void update_biopores (const Geometry& geo, 
                                const Soil& soil,  
                                const SoilHeat& soil_heat, 
                                const std::vector<double>& h,
                                const double dt);

  virtual void update_water (const Geometry&, const Soil&, 
                             const std::vector<double>& h_matrix,
                             const double dt,
                             std::vector<double>& S_drain,
                             std::vector<double>& S_matrix, 
                             std::vector<double>& q_tertiary, 
                             Treelog& msg) = 0;
  virtual void update_active (const std::vector<double>& h_matrix)
  { }

  // - For use in Movement::solute
  virtual void solute (const Geometry&, const SoilWater&, 
                       const std::map<size_t, double>& J_tertiary,
                       const double dt,
                       Chemical&, Treelog&) = 0;

  // - Output
  virtual void output (Log&) const = 0;

  // Create and Destroy.
public:
  static const AttributeList& none_model ();
  static Tertiary& none ();
  virtual bool initialize (const Geometry&, const Soil&, const Scope&, 
                           const double pipe_position, Treelog&) = 0;
  virtual bool check (const Geometry&, Treelog&) const = 0;
protected:
  explicit Tertiary (Block& al);
  explicit Tertiary (const symbol name);
public:
  virtual ~Tertiary ();
};

#endif // TERTIARY_H
