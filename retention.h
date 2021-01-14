// retention.h --- Retention curves.
// 
// Copyright 2020 KU.
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


#ifndef RETENTION_H
#define RETENTION_H

#include "model.h"
#include "symbol.h"

#include <memory>

class BlockModel;
class Treelog;

class Retention : public Model
{
  // Content.
public:
  static const char *const component;
  symbol library_id () const;

  // Simulation.
public:
  virtual double h (double Theta) const = 0;

  // Create and Destroy.
public:
  virtual void initialize (const double Theta_res, const double h_res,
			   const double Theta_sat, Treelog&) = 0;
protected:
  Retention ();
public:
  ~Retention ();
};

#endif // RETENTION_H
