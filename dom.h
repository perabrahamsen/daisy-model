// dom.h --- A single dissolved organic matter pool.
// 
// Copyright 2002 Per Abrahamsen and KVL.
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


#ifndef DOM_H
#define DOM_H

// These must be included in the header file, for 'load_syntax' to work.
#include "adsorption.h"
#include "transport.h"
#include "mactrans.h"
#include "plf.h"
#include "smb.h"

struct Log;
struct Syntax;
struct AttributeList;
struct Geometry;
struct Soil;
struct SoilWater;

class DOM
{ 
  // Content.
  class Element;
  friend class Element;
  Element& C;
  Element& N;

  // Transport.
private:
  std::auto_ptr<Transport> trans; // Solute transport model in matrix.
  std::auto_ptr<Transport> reserve; // Reserve solute transport model in matr.
  std::auto_ptr<Transport> last_resort; // Last resort solute transport model.
  std::auto_ptr<Mactrans> mactrans; // Solute transport model in macropores.
  std::auto_ptr<Adsorption> adsorption;	// Solute adsorption.
  const double diffusion_coefficient;

  // Turnover.
public:
  /* const */ PLF heat_factor;
  /* const */ PLF water_factor;
  const double turnover_rate;	// How fast this is it turned over?
  const std::vector<double> efficiency;	// How digestible is this?
  const std::vector<double> fractions;	// Where does it end up?

  // Simulation.
public:
  void output (Log&) const;
  void mix (const Soil&, const SoilWater&, double from, double to);
  void swap (const Soil&, const SoilWater& soil_water,
	     double from, double middle, double to);
  void add_to_source (unsigned int at, double C, double N);
  double soil_C (const Geometry& geometry) const;
  double soil_N (const Geometry& geometry) const;
  double soil_C (const Geometry& geometry, double from, double to) const;
  double soil_N (const Geometry& geometry, double from, double to) const;
  double C_source (const Geometry& geometry) const;
  double N_source (const Geometry& geometry) const;
  double C_at (unsigned int at) const;
  double N_at (unsigned int at) const;
public:
  void clear ();
  void turnover (unsigned int size, const double* turnover_factor, 
		 const double* N_soil, double* N_used,
		 double* CO2, const std::vector<SMB*>& smb);
private:
  void tock (unsigned int end,
	     const double* factor, double fraction, double efficiency,
	     const double* N_soil, double* N_used, double* CO2, OM& om);
public:
  void transport (const Soil&, const SoilWater&, Treelog&);

  // Create & Destroy.
public:
  static void load_syntax (Syntax&, AttributeList&);
  void initialize (const Soil&, const SoilWater&, Treelog&);
  DOM (const AttributeList& al);
  ~DOM ();
};

#endif // DOM_H
