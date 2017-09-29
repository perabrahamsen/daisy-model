// raddist.h -- Radiation distribution
// 
// Copyright 2006 Birgitte Gjettermann, Per Abrahamsen and KVL
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


#ifndef RADDIST_H
#define RADDIST_H

#include "model_derived.h"
#include <vector>

class Vegetation;
class Treelog;
class BlockModel;

class Raddist : public ModelDerived
{
  // Content.
public:
  static const char *const component;
  symbol library_id () const;

  // Fraction of Photosynthetically Active Radiation (PAR) in Shortware
  // incoming radiation. 
  static const double PARinSi;  // []

  // Fraction of NIR in Shortware incoming radiation. 
  static const double NIRinSi;  // []

  // Simulation.
public:
  virtual void tick (std::vector <double>& fraction_sun_LAI, 
                     std::vector <double>& sun_PAR, 
		     std::vector <double>& total_PAR, 
                     std::vector <double>& sun_NIR, 
		     std::vector <double>& total_NIR,
                     double global_radiation, double diffuse_radiation, 
                     double min_sin_beta, double sin_beta,
                     const Vegetation&, const double pF, 
		     Treelog&) = 0;


  // Utilities.
protected:
  static void radiation_distribution (const int No, const double LAI,
                                      const double Ref,
                                      const double Si,
                                      const double Ext_PAR,
                                      std::vector <double>& Rad,
                                      const double RadinSi);
private:
  static void intensity_distribution (int No, double LAI,
                                      double Rad0, double Ext_PAR, 
                                      std::vector <double>& Rad);

  // Create and Destroy.
protected:
  Raddist (const BlockModel&);

public:
  ~Raddist ();
};

#endif // RADDIST_H
