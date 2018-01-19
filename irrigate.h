// irrigate.h --- Manage irrigation events.
// 
// Copyright 2010 KU.
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


#ifndef IRRIGATE_H
#define IRRIGATE_H

#include "symbol.h"
#include "memutils.h"
#include <boost/noncopyable.hpp>
#include <boost/shared_ptr.hpp>

class Geometry;
class SoilWater;
class Chemistry;
class Bioclimate;
class IM;
class Volume;
class BlockSubmodel;
class Frame;
class Treelog;
class Unit;
class Log;

class Irrigation : private boost::noncopyable
{
  // Units.
  const Unit& u_mm;           // [mm]
  const Unit& u_storage;      // [g/cm^2]
  static const symbol solute_per_mm; // [g/cm^2/mm]
  const Unit& u_solute_per_mm;  // [g/cm^2/mm]
  static const symbol conc_flux; // [kg/ha/mm]
  const Unit& u_conc_flux;  // [kg/ha/mm]

  // Content.
public:
  static const double at_air_temperature;
  enum target_t { overhead, surface, subsoil };
private:
  class Event;
  auto_vector<Event*> event;

  // Use.
public:
  void add (double duration /* [h] */,
            double flux /* [mm/h] */,
            double temperature /* dg C */,
            target_t target,
            const IM& solute /* [M/L^3] */,
            boost::shared_ptr<Volume> volume, const bool silence, Treelog&);
  double suggest_dt () const; 	// [h]
  void tick (const Geometry&, SoilWater&, Chemistry&, Bioclimate&, 
             const double dt, Treelog&);
  void output (Log&) const;
private:
  void cleanup (Treelog&);

  // Create and Destroy.
public:
  static void load_syntax (Frame&);
  Irrigation (const BlockSubmodel&);
  ~Irrigation ();
};

#endif // IRRIGATE_H
