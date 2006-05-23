// soltrans1d.h -- 1D transport of solutes in soil water.
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


#ifndef SOLTRANS1D_H
#define SOLTRANS1D_H

// These must be included in the header file, for 'load_syntax' to work.
#include "transport.h"
#include "mactrans.h"
#include <memory>

struct Syntax;
struct AttributeList;
struct Block;
struct Geometry1D;
struct Soil;
struct SoilWater;
struct Solute;
struct Element;
struct Adsorption;

class Soltrans1D
{
  std::auto_ptr<Transport> transport; // Solute transport model in matrix.
  std::auto_ptr<Transport> reserve; // Reserve solute transport model in matr.
  std::auto_ptr<Transport> last_resort; // Last resort solute transport model.
  std::auto_ptr<Transport> transport_solid; // Pseudo transport for non-solutes
  std::auto_ptr<Mactrans> mactrans; // Solute transport model in macropores.

  // Simulation.
public:
  void solute (const Geometry1D&, const Soil&, const SoilWater&, 
               const double J_in, Solute&, Treelog& msg);
  void element (const Geometry1D& geo, const Soil& soil, 
                const SoilWater& soil_water, Element& element,
                Adsorption& adsorption, double diffusion_coefficient,
                Treelog& msg);
private:
  void flow (const Geometry1D&, const Soil&, const SoilWater&, 
             const std::string& name,
             std::vector<double>& M, std::vector<double>& C, 
             std::vector<double>& S, std::vector<double>& S_p, 
             std::vector<double>& J, std::vector<double>& J_p, 
             Adsorption& adsorption, double diffusion_coefficient,
             Treelog& msg);


  // Creation.
public:
  static void load_syntax (Syntax&, AttributeList&);
private:
  Soltrans1D (const Soltrans1D&);
public:
  explicit Soltrans1D (Block&);
  ~Soltrans1D ();
};

#endif // SOLTRANS1D_H
