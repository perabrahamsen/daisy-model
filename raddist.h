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

#include "librarian.h"
#include "alist.h"
#include <vector>

class Vegetation;

class Raddist : public Model
{
  // Content.
public:
  const symbol name;
  static const char *const description;
  const AttributeList alist;	// Remember attributes for checkpoint.

  // Simulation.
public:
  virtual void tick (std::vector <double>& fraction_sun_LAI, 
                     std::vector <double>& sun_PAR, 
		     std::vector <double>& total_PAR,
                     double global_radiation, 
                     double diffuse_radiation, double sin_beta,
                     const Vegetation&, 
		     Treelog&) = 0;
  virtual void output (Log&) const;


  // Utilities.
protected:
  static void radiation_distribution (const int No, const double LAI,
                                      const double Ref,
                                      const double Si,
                                      const double Ext,
                                      std::vector <double>& Rad);
private:
  static void intensity_distribution (int No, double LAI,
                                      double Rad0, double Ext, 
                                      std::vector <double>& Rad);

  // Create and Destroy.
protected:
  Raddist (Block&);

public:
  static const AttributeList& default_model ();
  static void load_syntax (Syntax&, AttributeList&);
  ~Raddist ();
};

#ifdef FORWARD_TEMPLATES
template<>
BuildBase* Librarian<Raddist>::content;
#endif

static Librarian<Raddist> Raddist_init ("raddist");

#endif // RADDIST_H
