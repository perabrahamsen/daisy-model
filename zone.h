// zone.h -- Specify part of the soil.
// 
// Copyright 2012 KU
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


#ifndef ZONE_H
#define ZONE_H

#include "model.h"
#include "symbol.h"

class BlockModel;

class Zone : public Model
{
  // Content.
public:
  static const char *const component;
  symbol library_id () const;

  // Use.
public:
  virtual bool contain_point (double z, double x, double y) const = 0;
  virtual bool overlap_interval (double from, double to) const = 0;
  virtual double center_z () const = 0;

  // Create and Destroy.
public:
  Zone (const BlockModel&);
  ~Zone ();
};

#endif // ZONE_H
