// secondary.h --- Specify secondary domain solute parameters.
// 
// Copyright 2008 Per Abrahamsen, Mikkel Mollerup and KU.
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


#ifndef SECONDARY_H
#define SECONDARY_H

#include "model.h"
#include "symbol.h"
#include <memory>

class BlockModel;
class Treelog;

class Secondary : public Model
{
  // Identity.
public:
  const symbol objid;
  static const char *const component;
  symbol library_id () const;
 
  // Content.
public:
  // Pressure thresshold for sec. domain. [cm]
  virtual double h_lim () const = 0; 
  // Conductivity of water in sec. dom. [cm/h]
  virtual double K (double h) const = 0; 
  virtual double alpha () const = 0; // Solute exchange between 1 & 2 domain.

  // Create and Destroy.
public:
  virtual void initialize (Treelog& msg) = 0;
protected:
  explicit Secondary (const BlockModel& al);
  explicit Secondary (const symbol name);
public:
  static std::unique_ptr<Secondary> create_none ();
  ~Secondary ();
};

#endif // SECONDARY_H
