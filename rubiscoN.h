// rubiscoN.h -- Rubisco N in leaves.
// 
// Copyright 2006 Birgitte Gjettermann, Per Abrahamsen and KVL
// Copyright 2017 KU.
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


#ifndef RUBISCON_H
#define RUBISCON_H

#include "model.h"
#include "symbol.h"

class RubiscoN : public Model
{
  // Content.
public:
  static const char *const component;
  symbol library_id () const;

  // Simulation.
public:
  virtual double value (double LAI, double Act, double Nf, double Cr, 
		      double Pt) = 0;

  // Create and Destroy.
protected:
  RubiscoN ();

public:
  ~RubiscoN ();
};

#endif // RUBISCON_H
