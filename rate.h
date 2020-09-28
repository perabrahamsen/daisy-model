// rate.h --- Specify a rate [h^-1].
// 
// Copyright 2018 KU.
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


#ifndef RATE_H
#define RATE_H

#include "model.h"
#include "symbol.h"

#include <memory>

class BlockModel;

class Rate : public Model
{
  // Content.
public:
  static const char *const component;
  symbol library_id () const;

  // Simulation.
private:
  virtual double find_rate () const = 0;

  // Public interface.
public:
  static void declare (Frame&, const symbol name, const symbol description);
  static void set_rate (Frame&, const symbol name, const double value);
  static void set_halftime (Frame&, const symbol name, const double value);
  static double value (const BlockModel&, const symbol name);
  
  // Create and Destroy.
protected:
  Rate ();
public:
  ~Rate ();
};

#endif // RATE_H
