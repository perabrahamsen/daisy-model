// solute.h
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


#ifndef SOLUTE_H
#define SOLUTE_H

// These must be included in the header file, for 'load_syntax' to work.
#include "adsorption.h"
#include <string>
#include <memory>

struct Log;
struct Syntax;
struct AttributeList;
struct Geometry;
struct Soil;
struct SoilWater;

class Solute
{
  const std::string submodel;	// Derived submodel.

  friend class Movement1D;
  friend class MovementRect;
  friend class Msoltranrect2x1;

  // State variables.
protected:
  std::vector<double> M_;		// Concentration in soil [g / cm³]
  std::vector<double> C_;		// Concentration in soil solution [g / cm³]

  // Flux variables.
protected:
  std::vector<double> S;		// Combined source term.
  std::vector<double> S_p;		// Source term for macropores only.
  std::vector<double> S_drain;	// Source term for soil drainage only.
  std::vector<double> S_external;	// External source term, e.g. incorp. fert.
  std::vector<double> S_permanent;	// Permanent external source term.
  std::vector<double> S_root;	// Root uptake source term (negative).
  std::vector<double> J;		// Solute transport log in matrix.
  std::vector<double> J_p;		// Solute transport log in macropores.
  std::auto_ptr<Adsorption> adsorption;	// Solute adsorption.
private:
  std::vector<double> tillage;       // Changes during tillage.

public:
  virtual double diffusion_coefficient () const = 0; // in free solute. 
  double C_to_M (const Soil& soil, double Theta, size_t i, double C) const
  { return adsorption->C_to_M (soil, Theta, i, C); }
  double M_to_C (const Soil& soil, double Theta, size_t i, double M) const
  { return adsorption->M_to_C (soil, Theta, i, M); }

public:
  const std::vector<double>& M () const
  { return M_; }
  double M (size_t i) const
  { return M_[i]; }
  double C (size_t i) const
  { return C_[i]; }
  double M_left (size_t i, double dt) const
  { return M_[i] + S[i] * dt; }
  double total_surface (const Geometry&, double from, double to) 
    const; // [g/cm^2]

  // Sink.
public:
  void clear ();
  void add_to_source (const std::vector<double>&, double dt);
  void add_to_sink (const std::vector<double>&, double dt);
  void add_to_root_sink (const std::vector<double>&, double dt);

  // Simulation.
  void tick (const size_t cell_size, const SoilWater&, double dt);
public:
  bool check (size_t n, Treelog& err) const;
  virtual void output (Log&) const;
  void incorporate (const Geometry&, double amount, double from, double to);
  void set_external_source (const Geometry&, 
			    double amount, double from, double to);
  void mix (const Geometry& geo,
            const Soil&, const SoilWater&, double from, double to);
  void swap (const Geometry& geo,
             const Soil&, const SoilWater&,
             double from, double middle, double to);

  // Communication with external model.
  void put_M (const Soil& soil, const SoilWater& soil_water,
	      const std::vector<double>& v);

  // Create and destroy.
protected:
  static void load_syntax (Syntax&, AttributeList&);
  Solute (const AttributeList& al);
private:
  virtual void default_initialize (const Soil& soil, const SoilWater&);
public:
  virtual void initialize (const AttributeList&,
			   const Geometry& geo,
                           const Soil&, const SoilWater&, Treelog&);
public:
  virtual ~Solute ();
};

#endif // SOLUTE_H
