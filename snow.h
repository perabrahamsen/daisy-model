// snow.h
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


#ifndef SNOW_H
#define SNOW_H

class Weather;
class Bioclimate;
class Frame;
class FrameSubmodel;
class Log;
class Movement;
class Soil;
class SoilWater;
class SoilHeat;
class Treelog;

class Snow
{ 
  struct Implementation;
  Implementation& impl;

  // Simulation.
public:
  void tick (Treelog&, const Movement&,
             const Soil&, const SoilWater&, const SoilHeat&,
	     double Si, double q_h, double Prain,
	     double Psnow, double Pond, double T, double Epot, double dt);
  void output (Log&) const;

  // Queries.
public:
  double percolation () const;
  double temperature () const;
  double evaporation () const;
  double storage () const;

  // Create & Destroy.
public:
  static void load_syntax (Frame&);
  Snow (const FrameSubmodel& al);
  ~Snow ();
};

#endif // SNOW_H
