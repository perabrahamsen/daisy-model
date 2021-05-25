// cloudiness.h -- Cloudiness
// 
// Copyright 2020 KU
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


#ifndef CLOUDINESS_H
#define CLOUDINESS_H

#include "model_derived.h"

class Log;
class Weather;
class Time;
class Treelog;
class BlockModel;

class Cloudiness : public ModelDerived
{
  // Content.
public:
  static const char *const component;
  symbol library_id () const;

  // Simulation.
public:
  virtual void tick (const Weather&, Treelog&) = 0;
  virtual double index () const = 0;		     // []
  virtual void output (Log&) const;

  // Create and Destroy.
public:
  virtual bool check (const Weather&, Treelog&) const = 0;
protected:
  Cloudiness (const BlockModel&);
public:
  ~Cloudiness ();
};

#endif // CLOUDINESS_H
